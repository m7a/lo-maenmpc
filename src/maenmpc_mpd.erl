-module(maenmpc_mpd).
-export([start/3, enter/3, interrupt/1]).

% async idle

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
	% TODO THERE IS STILL A RACE CONDITION WRT/ NOIDLE: WHAT IF NOIDLE HAPPENS JUST BETWEEN THE RETURN HERE AND THE NEXT RUN? EFFECTIVELY WE WOULD HAVE TO WAIT (UPON INTERRUPT) TO OBSERVE THAT SOMEONE IS LISTENING ON TCP RECV AS TO BE USER THAT THE IDLE COMMAND WAS SENT? / COULD ADD AN IDLE() WITH NOTIFY ONCE SENT CALLBACK THAT WE CAN THEN USE TO UNLOCK THE INTERRUPT FEATURE?
	ok = gen_server:call(Parent, {mpd_idle, Name, erlmpd:idle(Conn,
					[database, playlist, player, mixer,
					output, options, sticker])}),
	run(Parent, Name, Conn).

interrupt(Conn) ->
	erlmpd:command(Conn, "noidle").
