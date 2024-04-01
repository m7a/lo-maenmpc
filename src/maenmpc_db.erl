-module(maenmpc_db).
-behavior(gen_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, code_change/3]).

-record(db, {ui, mpd_map, mpd_active, mpd_ratings}).

init([NotifyToUI]) ->
	{ok, PrimaryRatings} = application:get_env(maenmpc, primary_ratings),
	{ok, MPDList}        = application:get_env(maenmpc, mpd),
	% TODO DEBUG ONLY
	MPDFirst = m16,
	%[MPDFirst|_Others] = MPDList,
	% TODO MAY MAKE SENSE TO ALLOW CONFIGURING THIS ETC.
	timer:send_interval(5000, interrupt_idle),
	{ok, #db{
		ui=NotifyToUI,
		mpd_map=lists:foldl(fun({Name, _ConnInfo}, Map) ->
			maps:put(Name, offline, Map)
		end, maps:new(), MPDList),
		% TODO PERIODICALLY INTERRUPT MPD ACTIVE!
		mpd_active=MPDFirst,
		mpd_ratings=PrimaryRatings
	}}.

handle_call({mpd_idle, Name, Subsystems}, _From, Context) ->
	{reply, ok, case Name =:= Context#db.mpd_active of
		true -> case Subsystems =:= [] orelse lists:any(
				fun is_status_subsystem/1, Subsystems) of
			true  -> update_playing_info(Context);
			% TODO DON'T CARE FOR NOW / HANDLE OTHERS HERE
			false -> Context
			end;
		% TODO IS IT ALWAYS IRRELEVANT WHEN STATUS OF INACTIVE UPDATES? MAYBE NOT E.G. FOR DB CHANGES ETC
		false -> Context
		end};
handle_call(_Call, _From, Context) ->
	{reply, ok, Context}.

is_status_subsystem(player)  -> true;  % start stop seek new song, tags changed
is_status_subsystem(mixer)   -> true;  % the volume has been changed
is_status_subsystem(options) -> true;  % repeat, random, crossfade, replay gain
is_status_subsystem(_Other)  -> false.

update_playing_info(Context) ->
	Conn = get_active_connection(Context),
	% file, Artist, Date, Album, Track, Title, Pos, Time,
	CurrentSong = erlmpd:currentsong(Conn),
	% state, audio, volume, repeat, random, single, consume, xfade,
	% updating_db, time
	Status = erlmpd:status(Conn),
	gen_server:cast(Context#db.ui, {db_playing, CurrentSong ++ Status}),
	Context.

get_active_connection(Context) ->
	maps:get(Context#db.mpd_active, Context#db.mpd_map).

%handle_idle(database, Context, _Name) ->
%	% the song database has been modified after update
%	Context; % TODO
%handle_idle(playlist, Context, _Name) ->
%	% the queue (i.e. the current playlist) has been modified
%	Context; % TODO
%handle_idle(output, Context) ->
%	% an audio output has been added, removed or modified
%	% (e.g. renamed, enabled or disabled)
%	Context; % TODO
%handle_idle(sticker, Context) ->
%	% the sticker database has been modified.
%	Context; % TODO

handle_cast({mpd_assign, Name, Conn}, Context) ->
	{noreply, Context#db{mpd_map=maps:put(Name, Conn, Context#db.mpd_map)}};
handle_cast(_Cast, Context) ->
	{noreply, Context}.

handle_info(interrupt_idle, Context) ->
	maenmpc_mpd:interrupt(get_active_connection(Context)),
	{noreply, Context};
	%NotifyTo = list_to_atom("maenmpc_conn_" ++
	%				atom_to_list(Context#db.mpd_active)),
handle_info(_Message, Context) ->
	{noreply, Context}.

code_change(_OldVersion, Context, _Extra) -> {ok, Context}.
