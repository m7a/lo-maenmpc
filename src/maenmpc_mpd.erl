-module(maenmpc_mpd).
-export([start/3, enter/3, interrupt/1]).

start(Parent, MPDName, Config) ->
	{ok, spawn_link(?MODULE, enter, [Parent, MPDName, Config])}.

enter(Parent, MPDName, Config) ->
	{Host, Port} = proplists:get_value(ip, Config),
	case erlmpd:connect(Host, Port) of
	{ok, Conn} ->
		gen_server:cast(Parent, {mpd_assign, MPDName, Conn}),
		run(Parent, MPDName, Conn);
	{error, Reason} ->
		gen_server:cast(Parent, {mpd_assign_error, MPDName, Reason})
	end.

run(Parent, Name, Conn) ->
	ok = gen_server:call(Parent, {mpd_idle, Name, erlmpd:idle(Conn,
					[database, playlist, player, mixer,
					output, options, sticker])}),
	run(Parent, Name, Conn).

interrupt(Conn) ->
	erlmpd:command(Conn, "noidle").
