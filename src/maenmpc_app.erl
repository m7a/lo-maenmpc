-module(maenmpc_app).
-behaviour(application).
-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
	{ok, MPDList}        = application:get_env(maenmpc, mpd),
	{ok, PrimaryRatings} = application:get_env(maenmpc, primary_ratings),
	{ok, Maloja}         = application:get_env(maenmpc, maloja),
	cecho:endwin(), % TODO ALT NOT LOAD CECHO BY DEFAULT (MAY BE RC?)
	case maenmpc_cli:run(MPDList, PrimaryRatings, Maloja) of
	ok ->
		init:stop(0),
		maenmpc_sup_dummy:start_link();
	{next, _Params} ->
		% TODO x pass params or such
		maenmpc_sup:start_link();
	Other ->
		Other
	end.

stop(_State) ->
	ok.
