-module(maenmpc_db).
-behavior(gen_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, code_change/3]).
-include_lib("maenmpc_db.hrl").

% tx_type:  none | songinfo | [list] | [radio]
% tx_await: list of connections to await replys from
% tx_val:   value (record) under construction
-record(db, {ui, alsa, mpd_list, mpd_map, mpd_active, mpd_ratings,
		mpd_volume, current_song, tx_type, tx_val, tx_await}).

init([NotifyToUI]) ->
	{ok, PrimaryRatings} = application:get_env(maenmpc, primary_ratings),
	{ok, MPDList}        = application:get_env(maenmpc, mpd),
	{ok, ALSAHWInfo}     = application:get_env(maenmpc, alsa),
	% TODO DEBUG ONLY
	MPDFirst = m16,
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
		mpd_volume=-1,
		current_song=#dbsong{key={<<>>, <<>>, <<>>}},
		tx_type=none,
		tx_val=undefined,
		tx_await=[]
	}}.

handle_call({mpd_idle, Name, Subsystems}, _From, Context) ->
	{reply, ok, case Context#db.tx_type =/= none andalso
				lists:member(Name, Context#db.tx_await) of
		true ->
			progress_tx(Context, Name);
			% TODO TX WHAT IF WE MISS SOME EVENT THIS WAY? WOULD IT MAKE SENSE TO PUSH THE INFO ABOUT THE CHANGES TO SOME STACK DURING TX PROC?
		false ->
			% TODO TX CAN WE EVEN PROCESS HERE OR MUST WE REJECT IT UNTIL TX COMPLETE (MUST NOT INTERLEAVE MULTIPLE TX SINCE THERE IS NO STACK???)
			case Name =:= Context#db.mpd_active of
			true -> case Subsystems =:= [] orelse lists:any(
					fun is_status_subsystem/1, Subsystems) of
				true  -> update_playing_info(Context);
				% TODO DON'T CARE FOR NOW / HANDLE OTHERS HERE
				false -> Context
				end;
			false -> Context
			end
		end};
handle_call(_Call, _From, Context) ->
	{reply, ok, Context}.

progress_tx(Context, Name) ->
	case Context#db.tx_type of
	songinfo ->
		{DBPRE, Status} = Context#db.tx_val,
		DBPOST = populate_with_conn(Context, Name, DBPRE),
		case Context#db.tx_await -- [Name] of
		% finish
		[] -> send_playing_info(Context#db{current_song=DBPOST,
			tx_type=none, tx_await=[], tx_val=undefined}, Status);
		% continue
		ListRem -> Context#db{tx_await=ListRem, tx_val={DBPOST, Status}}
		end;
	ui_volume_change ->
		Conn = maps:get(Name, Context#db.mpd_map),
		NewVal = Context#db.mpd_volume + Context#db.tx_val,
		case Context#db.mpd_volume /= -1 andalso
					 NewVal >= 0 andalso NewVal =< 100 of
		true  -> erlmpd:setvol(Conn, NewVal);
		false -> ok_pass
		end,
		Context#db{tx_type=none, tx_await=[], tx_val=undefined}
	% TODO OTHERS ARE ERROR FOR NOW
	end.

populate_with_conn(Context, Name, DBS) ->
	case erlmpd:find(maps:get(Name, Context#db.mpd_map), {land, [
			{tagop, artist, eq, element(1, DBS#dbsong.key)},
			{tagop, album,  eq, element(2, DBS#dbsong.key)},
			{tagop, title,  eq, element(3, DBS#dbsong.key)}]}) of
	% nothing assigned
	[] ->
		DBS;
	% Single element found -- assign it!
	[Element|[]] ->
		Idx = proplists:get_value(Name, Context#db.mpd_list),
		rate_if_match(Context, Name, DBS#dbsong{
			uris  =setelement(Idx, DBS#dbsong.uris,
						erlmpd_get_file(Element)),
			audios=setelement(Idx, DBS#dbsong.audios,
						erlmpd_get_audio(Element))
		});
	% result not unique - cannot safely assign
	[_Element|_Others] ->
		DBS
	% else error is fatal because the connection state may
	% be disrupted.
	end.

rate_if_match(Context, Name, DBCMP) ->
	case Name =:= Context#db.mpd_ratings of
	true  -> DBCMP#dbsong{rating=rating_for_uri(Context,
			element(proplists:get_value(Context#db.mpd_ratings,
				Context#db.mpd_list), DBCMP#dbsong.uris))};
	false -> DBCMP
	end.

rating_for_uri(Context, RatingURI) ->
	case erlmpd:sticker_get(maps:get(Context#db.mpd_ratings,
				Context#db.mpd_map), "song",
				binary_to_list(RatingURI), "rating") of
	% Typically error just means not found here (OK)
	{error, _Any} -> ?RATING_UNRATED;
	ProperRating  -> case list_to_integer(ProperRating) of
			 1      -> 0;
			 NotOne -> NotOne * 10
			 end
	end.

is_status_subsystem(player)  -> true;  % start stop seek new song, tags changed
is_status_subsystem(mixer)   -> true;  % the volume has been changed
is_status_subsystem(options) -> true;  % repeat, random, crossfade, replay gain
is_status_subsystem(_Other)  -> false.

update_playing_info(Context) ->
	Conn = get_active_connection(Context),
	% state, audio, volume, repeat, random, single, consume, xfade,
	% updating_db, time
	Status = erlmpd:status(Conn),
	% file [uri], Artist, Date, Album, Track, Title, Time [duration],
	CurrentSong = erlmpd:currentsong(Conn),
	case proplists:get_value(file, CurrentSong) of
	% Playback has stopped!
	undefined -> send_playing_info(Context#db{
				current_song=epsilon_song(Context)}, Status);
	% populate from DB
	_ValidValue ->
		DBCMP = erlmpd_to_dbsong(Context, CurrentSong),
		case DBCMP#dbsong.key =:= Context#db.current_song#dbsong.key of
		true ->
			send_playing_info(Context, Status);
		false ->
			DBINS = rate_if_match(Context, Context#db.mpd_active,
									DBCMP),
			TXAwait = lists:filter(fun(Name) ->
					Name =/= Context#db.mpd_active andalso
					maps:get(Name, Context#db.mpd_map) =/=
									offline
				end, maps:keys(Context#db.mpd_map)),
			case TXAwait of
			[] ->
				send_playing_info(
					Context#db{current_song=DBINS}, Status);
			_NonEmpty ->
				lists:foreach(fun(Name) ->
					maenmpc_mpd:interrupt(maps:get(Name,
					Context#db.mpd_map)) end, TXAwait),
				Context#db{tx_type=songinfo, tx_val={DBINS,
					Status}, tx_await=TXAwait}
			end
		end
	end.

epsilon_song(Context) ->
	EpsTPL = list_to_tuple(lists:duplicate(length(Context#db.mpd_list),
									<<>>)),
	#dbsong{key={<<>>, <<>>, <<>>}, uris=EpsTPL, playcount=0, rating=0,
			duration=1, year = <<>>, trackno=0, audios=EpsTPL}.

send_playing_info(RCtx, Status) ->
	gen_server:cast(RCtx#db.ui, {db_playing, [
			{x_maenmpc, RCtx#db.current_song}|[
			{x_maenmpc_alsa, query_alsa(RCtx#db.alsa)}|Status]]}),
	RCtx#db{mpd_volume = proplists:get_value(volume, Status, -1)}.

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

handle_cast({ui_volume_change, Delta}, Context) ->
	% TODO TX PROBLEM IF TX ALREADY ONGOING THEN IT CRASHES WITH EALREADY!!
	maenmpc_mpd:interrupt(get_active_connection(Context)),
	% TODO TX SMALL PROBLEM WHAT IF THERE IS AN ONGOING TX? -> NEED TO DESIGN THAT IN, IN THE MEANTIME TEST USING THIS CHAOTIC VARIANT HERE...
	{noreply, Context#db{tx_type=ui_volume_change, tx_val=Delta,
					tx_await=[Context#db.mpd_active]}};
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
