-module(maenmpc_db).
-behavior(gen_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, code_change/3]).

% TODO STUB.

init([]) ->
	{ok, MPDList}        = application:get_env(maenmpc, mpd),
	{ok, PrimaryRatings} = application:get_env(maenmpc, primary_ratings),
	{ok, Maloja}         = application:get_env(maenmpc, maloja),
	{ok, my_context}.

handle_call(_Call, _From, Context) ->
	{reply, ok, Context}.

handle_cast(_Cast, Context) ->
	{noreply, Context}.

handle_info(_Message, Context) -> {noreply, Context}.
code_change(_OldVersion, Context, _Extra) -> {ok, Context}.
