-module(maenmpc_sup).
-behaviour(supervisor).
-export([start_link/1, init/1]).
-define(SERVER, ?MODULE).

start_link(CLIParams) ->
	supervisor:start_link({local, ?SERVER}, ?MODULE, [CLIParams]).

init([_CLIParams]) ->
	application:start(cecho),
	{ok, {#{strategy => one_for_all, intensity => 0, period => 1}, [
		#{id => maenmpc_ui, start => {gen_server, start_link,
				[{local, maenmpc_ui}, maenmpc_ui, [], []]}},
		#{id => maenmpc_input, start => {maenmpc_input, start,
				[maenmpc_ui]}},
		% TODO CHILD SPECS GO HERE
		#{id => maenmpc_db, start => {gen_server, start_link,
				[{local, maenmpc_db}, maenmpc_db, [], []]}}
	]}}.
