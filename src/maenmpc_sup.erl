-module(maenmpc_sup).
-behaviour(supervisor).
-export([start_link/1, init/1]).
-define(SERVER, ?MODULE).

start_link(CLIParams) ->
	supervisor:start_link({local, ?SERVER}, ?MODULE, [CLIParams]).

init([_CLIParams]) ->
	{ok, {#{strategy => one_for_all, intensity => 0, period => 1}, [
		% TODO CHILD SPECS GO HERE
		#{id => maenmpc_db, start => {gen_server, start_link,
				[{local, maenmpc_db}, maenmpc_db, [], []]}}
	]}}.
