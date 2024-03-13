-module(maenmpc_sup_dummy).
-behaviour(supervisor).
-export([start_link/0, init/1]).
-define(SERVER, ?MODULE).

start_link() ->
	supervisor:start_link({local, ?SERVER}, ?MODULE, []).

init([]) ->
	{ok, {#{strategy => one_for_all, intensity => 0, period => 1}, []}}.
