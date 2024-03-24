-module(maenmpc_app).
-behaviour(application).
-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
	{ok, MPDList}        = application:get_env(maenmpc, mpd),
	{ok, PrimaryRatings} = application:get_env(maenmpc, primary_ratings),
	{ok, Maloja}         = application:get_env(maenmpc, maloja),
	{ok, RadioConf}      = application:get_env(maenmpc, radio),
	case maenmpc_cli:run(MPDList, PrimaryRatings, Maloja, RadioConf) of
	ok ->
		init:stop(0),
		maenmpc_sup_dummy:start_link();
	{next, Params} ->
		maenmpc_sup:start_link(Params);
	Other ->
		Other
	end.

stop(_State) ->
	ok.
