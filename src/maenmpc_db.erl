-module(maenmpc_db).
-behavior(gen_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, code_change/3]).

% TODO STUB. MAKE VALUES OF MPDMAP A STRUCT WHICH CONTAINS GUI-RELEVANT DATA
%      THEN MAINTAIN AN ACTIVE MPD AND SEND GUI THE INFO ABOUT IT...?
-record(db, {mpdmap}).

init([]) ->
	{ok, MPDList}        = application:get_env(maenmpc, mpd),
	{ok, PrimaryRatings} = application:get_env(maenmpc, primary_ratings),

	MPDMap = lists:foldl(fun({Name, _ConnInfo}, Map) ->
			maps:put(Name, offline, Map)
		end, maps:new(), MPDList),

	{ok, #db{mpdmap=MPDMap}}.

handle_call({mpd_idle, Name, Subsystems}, _From, Context) ->
	io:fwrite("HELLO IDLE <~p> on <~p> @ctx=<~p>~n",
						[Name, Subsystems, Context]),
	% TODO ASTAT WORKS NOW DO SOMETHING NICE WITH IT!
	{reply, ok, Context};
handle_call(_Call, _From, Context) ->
	{reply, ok, Context}.

handle_cast({mpd_assign, Name, Conn}, Context) ->
	{noreply, Context#db{mpdmap=maps:put(Name, Conn, Context#db.mpdmap)}};
handle_cast(_Cast, Context) ->
	{noreply, Context}.

handle_info(_Message, Context) -> {noreply, Context}.
code_change(_OldVersion, Context, _Extra) -> {ok, Context}.
