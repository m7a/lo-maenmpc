-module(maenmpc_mpd).
-behavior(gen_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, code_change/3]).

% async idle

% in messages:  call mpd_idle_enter
% out messages: cast mpd_assign, mpd_assign_error, mpd_idle

init([Parent, Name, Config]) ->
	S0 = list_to_atom("maenmpc_conn_" ++ atom_to_list(Name)),
	case maenmpc_erlmpd:connect(Config) of
	{ok, Conn} ->
		SO = idle_enter({Parent, Name, Conn, S0}),
		gen_server:cast(Parent, {mpd_assign, Name, Conn}),
		{ok, SO};
	{error, Reason} ->
		gen_server:cast(Parent, {mpd_assign_error, Name, Reason}),
		{stop, ignore}
	end.

idle_enter(State={_Parent, _Name, Conn, S0}) ->
	ok = erlmpd:idle_send(Conn, [database, playlist, player, mixer, output,
							options, sticker]),
	gen_server:cast(S0, mpd_idle),
	State.

handle_call(mpd_idle_enter, _From, State) ->
	{reply, ok, idle_enter(State)};
handle_call(_Other, _From, State) ->
	{reply, ok, State}.

handle_cast(mpd_idle, State={Parent, Name, Conn, S0}) ->
	IdleResult = erlmpd:idle_receive(Conn),
	ok = gen_server:cast(Parent, {mpd_idle, Name, IdleResult, S0}),
	{noreply, State}.

handle_info(_Message,    State)         -> {noreply, State}.
code_change(_OldVersion, State, _Extra) -> {ok,      State}.
