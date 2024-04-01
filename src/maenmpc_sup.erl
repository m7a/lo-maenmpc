-module(maenmpc_sup).
-behaviour(supervisor).
-export([start_link/1, init/1]).
-define(SERVER, ?MODULE).

start_link(CLIParams) ->
	supervisor:start_link({local, ?SERVER}, ?MODULE, [CLIParams]).

init([_CLIParams]) ->
	{ok, MPDList} = application:get_env(maenmpc, mpd),
	application:start(cecho),
	{ok, {#{strategy => one_for_all, intensity => 0, period => 1}, [
		% TODO CHILD SPECS GO HERE
		#{id => maenmpc_ui, start => {gen_server, start_link,
				[{local, maenmpc_ui}, maenmpc_ui, [], []]}},
		#{id => maenmpc_input, start => {maenmpc_input, start,
				[maenmpc_ui]}},
		#{id => maenmpc_db, start => {gen_server, start_link,
				[{local, maenmpc_db}, maenmpc_db,
				[maenmpc_ui], []]}}
	] ++ [#{id => list_to_atom("maenmpc_conn_" ++ atom_to_list(Name)),
		restart => transient,
		start => {maenmpc_mpd, start, [maenmpc_db, Name, Config]}} ||
		{Name, Config} <- MPDList]
	}}.
