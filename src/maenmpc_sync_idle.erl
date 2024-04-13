-module(maenmpc_sync_idle).
-behavior(gen_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, code_change/3,
	is_online/1, run_transaction/2, interrupt_no_tx/1]).

% conn      = not_assigned | offline    | <Connection>
% tx_active = none         | processing | <TXActive>
-record(syncidle, {conn, notify, tx_active}).

init([Notify]) ->
	{ok, #syncidle{conn=not_assigned, notify=Notify, tx_active=none}}.

handle_call(is_online, _From, Ctx) ->
	{reply, Ctx#syncidle.conn =/= not_assigned andalso
					Ctx#syncidle.conn =/= offline, Ctx};
handle_call(interrupt_no_tx, _From, Ctx)
			when Ctx#syncidle.tx_active =:= none andalso
				Ctx#syncidle.conn =/= not_assigned andalso
				Ctx#syncidle.conn =/= offline ->
	maenmpc_mpd:interrupt(Ctx#syncidle.conn),
	{reply, ok, Ctx};
handle_call(tx_begin, From, Ctx)
			when Ctx#syncidle.tx_active =:= none andalso
				Ctx#syncidle.conn =/= not_assigned andalso
				Ctx#syncidle.conn =/= offline ->
	maenmpc_mpd:interrupt(Ctx#syncidle.conn),
	{noreply, Ctx#syncidle{tx_active=From}};
handle_call({mpd_idle, Name, List}, _From, Ctx)
			when Ctx#syncidle.tx_active =:= none andalso
				Ctx#syncidle.conn =/= not_assigned andalso
				Ctx#syncidle.conn =/= offline ->
	% Outside running TX notify DB
	ok = gen_server:call(Ctx#syncidle.notify,
			{mpd_idle, Name, List, Ctx#syncidle.conn}),
	{reply, ok, Ctx};
handle_call({mpd_idle, _Name, _List}, From, Ctx)
			when Ctx#syncidle.tx_active =/= none andalso
			     Ctx#syncidle.tx_active =/= processing ->
	% Inside running TX means we have completed the begin part
	% tx_begin returns with OK. Tell `From` about completion of idle
	% afterwards...
	ok = gen_server:reply(Ctx#syncidle.tx_active, {ok,
						Ctx#syncidle.conn, From}),
	{noreply, Ctx#syncidle{tx_active=processing}};
handle_call({tx_end, NotifyIdleCompletionTo}, _From, Ctx)
			when Ctx#syncidle.tx_active =:= processing ->
	ok = gen_server:reply(NotifyIdleCompletionTo, ok),
	{reply, ok, Ctx#syncidle{tx_active=none}};
% TODO x MAY BE WORTH AN ERROR HERE
handle_call(_Call, _From, Ctx) ->
	{reply, ok, Ctx}.

handle_cast({mpd_assign, _MPDName, Conn}, Ctx) ->
	{noreply, Ctx#syncidle{conn=Conn}};
handle_cast(Msg={mpd_assign_error, _MPDName, _Reason}, Ctx) ->
	gen_server:cast(Ctx#syncidle.notify, Msg),
	{noreply, Ctx#syncidle{conn=offline}};
handle_cast(_Cast, Ctx) ->
	{noreply, Ctx}.

is_online(Name) ->
	gen_server:call(Name, is_online).

run_transaction(Name, Callback) ->
	{ok, Connection, NotifyIdleCompletionTo} = gen_server:call(Name,
								tx_begin),
	Ret = Callback(Connection),
	ok = gen_server:call(Name, {tx_end, NotifyIdleCompletionTo}),
	Ret.

interrupt_no_tx(Name) ->
	gen_server:call(Name, interrupt_no_tx).

handle_info(_Message,    Ctx)         -> {noreply, Ctx}.
code_change(_OldVersion, Ctx, _Extra) -> {ok,      Ctx}.
