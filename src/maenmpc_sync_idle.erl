-module(maenmpc_sync_idle).
-behavior(gen_statem).
-export([init/1, callback_mode/0, is_online/1, run_transaction/2,
	interrupt_no_tx/1, handle_event/4, terminate/3, code_change/4]).

% states:       init, offline, idle, tx_pre, tx_processing, interrupted
% in messages:  mpd_idle, mpd_assign, mpd_assign_error, is_online,
%               interrupt_no_tx, tx_begin, tx_end
% out messages: mpd_idle, mpd_idle_enter, mpd_assign_error, (noidle),
%               reply_to_procid

-record(syncidle, {up, conn, tx}).

init([Notify]) ->
	{ok, init, #syncidle{up=Notify, conn=not_assigned, tx=none}}.

callback_mode() ->
	handle_event_function.

% -- user API --

is_online(Name) ->
	gen_statem:call(Name, is_online).

run_transaction(Name, Callback) ->
	{ok, Connection, NotifyCompletionTo} = gen_statem:call(Name, tx_begin),
	Ret = Callback(Connection),
	ok = gen_statem:call(Name, {tx_end, NotifyCompletionTo}),
	Ret.

interrupt_no_tx(Name) ->
	gen_statem:call(Name, interrupt_no_tx).

% -- begin state machine --

handle_event(cast, {mpd_assign, _Name, Conn}, init, Ctx) ->
	{next_state, idle, Ctx#syncidle{conn=Conn}};
handle_event(cast, Msg={mpd_assign_error, _Name, _Reason}, init,
						Ctx = #syncidle{up=Notify}) ->
	gen_server:cast(Notify, Msg),
	{next_state, offline, Ctx};

handle_event(cast, {mpd_idle, Name, Result, FromMPD}, idle,
				Ctx = #syncidle{up=Notify, conn=Conn}) ->
	ok = gen_server:call(Notify, {mpd_idle, Name, Result, Conn}),
	ok = gen_server:call(FromMPD, mpd_idle_enter),
	{keep_state, Ctx};
handle_event(cast, {mpd_idle, Name, Result, FromMPD}, interrupted,
				Ctx = #syncidle{up=Notify, conn=Conn}) ->
	ok = gen_server:call(Notify, {mpd_idle, Name, Result, Conn}),
	ok = gen_server:call(FromMPD, mpd_idle_enter),
	{next_state, idle, Ctx};
handle_event(cast, {mpd_idle, _Name, _Result, FromMPD}, tx_pre,
					Ctx = #syncidle{conn=Conn, tx=TX}) ->
	% complete reply delayed from tx_begin
	ok = gen_statem:reply(TX, {ok, Conn, FromMPD}),
	{next_state, tx_processing, Ctx#syncidle{tx=none}};

% TODO x erlmpd:noidle small problem: It still seems we cannot avoid the
%        ealready, because just before sending this the idle may have already
%        returned. In fact, this is documented behavior to occur when an action
%        was performed that changes the outcome of idle() in the meantime.
%        It might make sense to store info about ealready return values in a
%        counter inside the context and then observe whether we might be able
%        to join some TX to reduce the numbers...
handle_event({call, FromUp}, interrupt_no_tx, idle,
					Ctx = #syncidle{conn=Conn}) ->
	erlmpd:noidle(Conn),
	{next_state, interrupted, Ctx, [{reply, FromUp, ok}]};

handle_event({call, FromTX}, tx_begin, idle, Ctx = #syncidle{conn=Conn}) ->
	erlmpd:noidle(Conn),
	% delay reply!
	{next_state, tx_pre, Ctx#syncidle{tx=FromTX}, []};

% TODO UNDER EVALUATION. IF IT DOES NOT WORK THEN CONSIDER MAKING INTERRUPT_NO_TX() WAIT UNTIL BACK TO IDLE STATE!
handle_event({call, FromTX}, tx_begin, interrupted, Ctx) ->
	% noidle already set upon interrupting, can rely on the idle to return
	% within the next messages.
	{next_state, tx_pre, Ctx#syncidle{tx=FromTX}, []};

handle_event({call, FromTX}, {tx_end, NotifyCompletionTo}, tx_processing,
								Ctx) ->
	ok = gen_server:call(NotifyCompletionTo, mpd_idle_enter),
	{next_state, idle, Ctx, [{reply, FromTX, ok}]};

handle_event({call, From}, is_online, InState, Ctx) when
				InState =:= init orelse InState =:= offline ->
	{keep_state, Ctx, [{reply, From, false}]};
handle_event({call, From}, is_online, _InState, Ctx) ->
	{keep_state, Ctx, [{reply, From, true}]};

% catchall
handle_event({call, From}, Event, OldState, Ctx) ->
	% report unsolicited calls because they can hide race conditions...
	error_logger:info_msg("maenmpc_sync_idle wrong state from=" ++
		"~p oldstate=~p event=~p ctx=~p", [From, OldState, Event, Ctx]),
	{keep_state, Ctx, [{reply, From, ok}]};
handle_event(cast, _Event, _OldState, Ctx) ->
	{keep_state, Ctx};
handle_event({_Other, _From}, _Event, _OldState, Ctx) ->
	{keep_state, Ctx}.

% -- end state machine --

terminate(_Reason, _State, #syncidle{conn=Conn}) ->
	erlmpd:close(Conn),
	void.

code_change(_Vsn, State, Ctx, _Extra) -> {ok, State, Ctx}.
