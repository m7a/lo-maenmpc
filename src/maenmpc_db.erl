-module(maenmpc_db).
-behavior(gen_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, code_change/3]).
-include_lib("maenmpc_db.hrl").

-record(db, {ui, alsa, mpd_list, mpd_map, mpd_active, mpd_ratings,
		current_song}).

init([NotifyToUI]) ->
	{ok, PrimaryRatings} = application:get_env(maenmpc, primary_ratings),
	{ok, MPDList}        = application:get_env(maenmpc, mpd),
	{ok, ALSAHWInfo}     = application:get_env(maenmpc, alsa),
	% TODO DEBUG ONLY
	MPDFirst = local,
	%[MPDFirst|_Others] = MPDList,
	% TODO MAY MAKE SENSE TO ALLOW CONFIGURING THIS!
	timer:send_interval(5000, interrupt_idle),
	MPDListIdx = [{Name, Idx} || {{Name, _ConnInfo}, Idx} <-
			lists:zip(MPDList, lists:seq(1, length(MPDList)))],
	gen_server:cast(NotifyToUI, {db_cidx,
				proplists:get_value(MPDFirst, MPDListIdx)}),
	{ok, #db{
		ui=NotifyToUI,
		alsa=ALSAHWInfo,
		% list of {name, idx} tuples / can use as proplist map...
		mpd_list=MPDListIdx,
		% name -> connection
		mpd_map=lists:foldl(fun({Name, _ConnInfo}, Map) ->
			maps:put(Name, offline, Map)
		end, maps:new(), MPDList),
		mpd_active=MPDFirst,
		mpd_ratings=PrimaryRatings,
		current_song=#dbsong{key={<<>>, <<>>, <<>>}}
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
	% file [uri], Artist, Date, Album, Track, Title, Time [duration],
	CurrentSong = erlmpd:currentsong(Conn),
	% state, audio, volume, repeat, random, single, consume, xfade,
	% updating_db, time
	Status = erlmpd:status(Conn),
	% populate from DB
	DBCMP = erlmpd_to_dbsong(Context, CurrentSong),
	RCtx = case DBCMP#dbsong.key =:= Context#db.current_song#dbsong.key of
		true  -> Context;
		false -> Context#db{current_song=query_complete_song_info(
							Context, DBCMP)}
		end,
	gen_server:cast(RCtx#db.ui, {db_playing, [
			{x_maenmpc, RCtx#db.current_song}|[
			{x_maenmpc_alsa, query_alsa(RCtx#db.alsa)}|Status]]}),
	RCtx.

get_active_connection(Context) ->
	maps:get(Context#db.mpd_active, Context#db.mpd_map).

erlmpd_to_dbsong(Context, Entry) ->
	#dbsong{key          = erlmpd_to_key(Entry),
		% TODO TMP / LATER REQUIRE PROVIDING NAME EXTERNALLY OR SUCH?
		uris         = list_to_tuple([case Name =:=
							Context#db.mpd_active of
				true  -> erlmpd_get_file(Entry);
				false -> <<>>
				end || {Name, _Idx} <- Context#db.mpd_list]),
		playcount    = 0,
		rating       = ?RATING_UNRATED,
		duration     = proplists:get_value('Time',  Entry, 1),
		year         = proplists:get_value('Date',  Entry, <<>>),
		trackno      = proplists:get_value('Track', Entry, 0),
		% TODO X LOOKS REDUNDANT TO ABOVE BUT MAY CHANGE ANYWAYS...
		audios       = list_to_tuple([case Name =:=
							Context#db.mpd_active of
				true  -> erlmpd_get_audio(Entry);
				false -> <<>>
				end || {Name, _Idx} <- Context#db.mpd_list])
	}.

erlmpd_to_key(Entry) ->
	{normalize_key(proplists:get_value('Artist',   Entry, <<>>)),
	 normalize_strong(proplists:get_value('Album', Entry, <<>>)),
	 normalize_key(proplists:get_value('Title',    Entry, <<>>))}.

% Expensive normalization option required due to the fact that scrobbling or
% Maloja seem to mess with the supplied metadata.
normalize_key(Value) ->
	normalize_always(normalize_safe(Value)).

normalize_safe(Value) ->
	re:replace(string:replace(string:replace(
				lists:join(<<" ">>, string:lexemes(Value, " ")),
			"[", "("), "]", ")"),
		" \\(?feat\\.? .*$", "").

normalize_always(Value) ->
	unicode:characters_to_nfc_binary(Value).

normalize_strong(Value) ->
	normalize_always(re:replace(normalize_safe(Value), " \\(.*\\)$", "")).

erlmpd_get_file(Entry) ->
	normalize_always(proplists:get_value(file, Entry)).

erlmpd_get_audio(Entry) ->
	proplists:get_value('Format', Entry, <<>>).

query_complete_song_info(Context, DBS) ->
	% Assoc Status, URIs, Audios
	{URIs, Audios} = lists:unzip([case element(Idx, DBS#dbsong.uris) of
		<<>> ->
			% unassigned, must do something
			% TODO THIS ROUTINE HERE IS SPECIFIC TO THE GENERAL SONG INFO CASE. OTHER CASES (LIST PROCESSING) MAY REQUIRE ENTIRELY DIFFERENT HANDLING!
			Conn = case Name =:= Context#db.mpd_active of
			true ->
				% missing info about the active one, can query
				% right away since we are running in "active"
				% callback
				% TODO THIS MAY NOT BE THE CORRECT INFO IN RADIO CASE!
				get_active_connection(Context);
			false ->
				% missing info about an other one, must
				% interrupt the ongoing idle. note that
				% afterwards, a callback message is issued to
				% maenmpc_db, but we do not use this and rather
				% make use of the fact that the queue is not
				% processed while we are inside this function
				% here and hence idle is suspended here already!
				% TODO IT IS NOT CLEAR IF THIS REALLY WORKS
				%      RELIABLY. POSSIBLY SWITCH TO DESIGN WITH
				%      IDLE CALLBACK ALTHOUGH IT MAY BE MUCH
				%      MORE DIFFICULT IN PRACTICE...
				ConnI = maps:get(Name, Context#db.mpd_map),
				% TODO ASTAT EITHER RECEIVE THE REPLY HERE OR SUSPEND THE ENTIRE QUERY OPERATION WITH SOME STATE INFO IN THE CONTEXT AND RESUME IT UPON RECEIVING THE INCOMING IDLES OF THE ITNERRUPTED CONNECTION/S. WE WOULD THEN REDESIGN THIS TO SEND INTERRUPTS AS NECESSARY, SUSPEND UNTIL THE REPLY FROM INTERRUPT COMES AND THEN CONTINUE FINALLY PERFORMING THE ACTUAL CAST OPERATION. WOULD NEED TO REDESIGN THIS THING QUITE A BIT BUT THIS WOULD BE A RELIABLE WAY TO GO ABOUT IT. ALTERNATIVELY PLACE A REAL RECEIVE IN HERE
				% BTW WE NEED TO FOLLOW THE SUSPEND DESIGN BECAUSE OTHERWISE WE MIGHT RECEIVE SOME MESSAGES THAT WE DIDN'T EXPECT WITH NO REAL WAY TO PROCESS THEM FROM IN HERE EXCEPT BY REPLICATING THE SUSPEND DESIGN JUST THE OTHER WAY AROUND!!!
				case maps:get(Name, Context#db.mpd_map) of
				offline ->
					offline;
				ConnI ->
					maenmpc_mpd:interrupt(ConnI),
					timer:sleep(15), % TODO BAD HACK!
					ConnI
				end
			end,
			case Conn of
			offline ->
				{<<>>, element(Idx, DBS#dbsong.audios)};
			_RealConn ->
				% TODO MISSING THE ESCAPE HANDLING!!!
				%      GOOD CANDIDATE TO INCORPORATE INTO ERLMPD?
				case erlmpd:find(Conn, io_lib:format("((artist == '~s"
						++ "') AND (album == '~s"
						++ "') AND (title == '~s'))",
						[element(1, DBS#dbsong.key),
						element(2, DBS#dbsong.key),
						element(3, DBS#dbsong.key)])) of
				% nothing assigned
				[] ->
					{<<>>, element(Idx, DBS#dbsong.audios)};
				% Single element found -- assign it!
				[Element|[]] ->
					{erlmpd_get_file(Element),
							erlmpd_get_audio(Element)};
				% result not unique - cannot safely assign
				[_Element|_Others] -> 
					{<<>>, element(Idx, DBS#dbsong.audios)}
				% else error is fatal because the connection state may
				% be disrupted.
				end
			end;
		AnyURI ->
			% OK, no need to do anything
			{AnyURI, element(Idx, DBS#dbsong.audios)}
		end || {Name, Idx} <- Context#db.mpd_list]),
	URIsConv = list_to_tuple(URIs),
	RatingIdx = proplists:get_value(Context#db.mpd_ratings,
							Context#db.mpd_list),
	Rating = case element(RatingIdx, URIsConv) of
		<<>>      -> ?RATING_ERROR;
		RatingURI ->
			case erlmpd:sticker(maps:get(Context#db.mpd_ratings,
					Context#db.mpd_map), get, "song",
					binary_to_list(RatingURI), "rating") of
			% Typically error just means not found here (OK)
			{error, _Any} -> ?RATING_UNRATED;
			ProperRating  -> sticker_line_to_rating(ProperRating)
			end
		end,
	DBS#dbsong{uris=URIsConv, rating=Rating, audios=list_to_tuple(Audios)}.

% TODO USE FOR RADIO MODE, TOO!
sticker_line_to_rating(Sticker) ->
	[_ConstSticker|[Stickers|[]]] = string:split(Sticker, ": "),
	lists:foldl(fun(KV, Acc) ->
		[Key|[Value|[]]] = string:split(KV, "="),
		case Key == "rating" of
		% Found rating, compute * 10 with 10 map to 0.
		true -> case list_to_integer(Value) of
			1      -> 0;
			NotOne -> NotOne * 10
			end;
		% Skip this sticker
		false -> Acc
		end
	end, ?RATING_UNRATED, string:split(Stickers, " ")).

% $ cat /proc/asound/card0/pcm0p/sub0/hw_params
% closed
% $ cat /proc/asound/card0/pcm0p/sub0/hw_params
% format: S32_LE                <- could use for format?
% channels: 2                   <- channels
% rate: 44100 (44100/1)         <- rate
% ...
query_alsa(ALSA) ->
	case file:read_file(ALSA) of
	{error, Reason} ->
		atom_to_binary(Reason);
	{ok, <<"closed\n">>} ->
		<<"closed">>;
	{ok, ALSALinesRaw} ->
		Lines = binary:split(ALSALinesRaw, <<"\n">>, [global, trim]),
		LoL = [binary:split(Line, <<" ">>, [global, trim]) ||
								Line <- Lines],
		Proplist = [{RawKey, RawValue} ||
					[RawKey|[RawValue|_Other]] <- LoL],
		Chan = proplists:get_value(<<"channels:">>, Proplist),
		Rate = proplists:get_value(<<"rate:">>, Proplist),
		[Rate, <<":__:">>, Chan]
	end.

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
handle_cast({mpd_assign_error, Name, Reason}, Context) ->
	gen_server:cast(Context#db.ui, {db_error, {offline, Name, Reason}}),
	{noreply, Context};
handle_cast(_Cast, Context) ->
	{noreply, Context}.

handle_info(interrupt_idle, Context) ->
	maenmpc_mpd:interrupt(get_active_connection(Context)),
	{noreply, Context};
handle_info(_Message, Context) ->
	{noreply, Context}.

code_change(_OldVersion, Context, _Extra) -> {ok, Context}.
