-module(maenmpc_input).
-export([start/1, run/1]).

start(NotifyUI) ->
	{ok, spawn_link(?MODULE, run, [NotifyUI])}.

run(NotifyUI) ->
	gen_server:cast(NotifyUI, {getch, cecho:getch()}),
	run(NotifyUI).
