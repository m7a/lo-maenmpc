-module(maenmpc_sync_idle).
-behavior(gen_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, code_change/3,
	run_transaction/2]).

% connection = not_assigned | offline | <Connection>
% notify_db  = NotifyDB
% tx_active  = none | processing | <TXActive>
-record(syncidle, {connection, notify_db, tx_active}).

init([NotifyToDB]) ->
	{ok, #syncidle{connection=not_assigned, notify_db=NotifyToDB,
							tx_active=none}}.

handle_call(get_status, _From, Ctx) ->
	{reply, is_online(Ctx), Ctx};
handle_call(tx_begin, From, Ctx)
		when Ctx#syncidle.tx_active =:= none andalso
			Ctx#syncidle.connection =/= not_assigned andalso
			Ctx#syncidle.connection =/= offline ->
	maenmpc_mpd:interrupt(Ctx#syncidle.connection),
	{noreply, ok, Ctx#syncidle{tx_active=From}};
handle_call({mpd_idle, Name, List}, _From, Ctx)
		when Ctx#syncidle.tx_active =:= none andalso
			Ctx#syncidle.connection =/= not_assigned andalso
			Ctx#syncidle.connection =/= offline ->
	ok = gen_server:call(Ctx#syncidle.notify_db,
			{mpd_idle, Name, List, Ctx#syncidle.connection}),
	{reply, ok, Ctx};
handle_call({mpd_idle, _Name, _List}, From, Ctx)
				when Ctx#syncidle.tx_active =/= none andalso
				     Ctx#syncidle.tx_active =/= processing ->
	% Inside running TX means we have completed the begin part
	% tx_begin returns with OK. Tell `From` about completion of idle
	% afterwards...
	ok = gen_server:reply(Ctx#syncidle.tx_active, {ok,
						Ctx#syncidle.connection, From}),
	{noreply, ok, Ctx#syncidle{tx_active=processing}};
handle_call({tx_end, NotifyIdleCompletionTo}, _From, Ctx)
				when Ctx#syncidle.tx_active =:= processing ->
	ok = gen_server:reply(NotifyIdleCompletionTo, ok),
	{reply, ok, Ctx#syncidle{tx_active=none}};
% TODO MAY BE WORTH AN ERROR HERE
handle_call(_Call, _From, Ctx) ->
	{reply, ok, Ctx}.

is_online(Ctx) ->
	Ctx#syncidle.connection =/= not_assigned andalso
	Ctx#syncidle.connection =/= offline.

handle_cast({mpd_assign, _MPDName, Conn}, Ctx) ->
	{noreply, Ctx#syncidle{connection=Conn}};
handle_cast(Msg={mpd_assign_error, _MPDName, _Reason}, Ctx) ->
	gen_server:cast(Ctx#syncidle.notify_db, Msg),
	{noreply, Ctx#syncidle{connection=offline}};
handle_cast(_Cast, Ctx) ->
	{noreply, Ctx}.

% Requires status = online!
run_transaction(Name, Callback) ->
	{ok, Connection, NotifyIdleCompletionTo} = gen_server:call(Name,
								tx_begin),
	Ret = Callback(Connection),
	ok = gen_server:call(Name, {tx_end, NotifyIdleCompletionTo}),
	Ret.

handle_info(_Message,    Ctx)         -> {noreply, Ctx}.
code_change(_OldVersion, Ctx, _Extra) -> {ok,      Ctx}.
