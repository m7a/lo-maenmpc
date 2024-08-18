-module(maenmpc_singleplayer).
-behavior(gen_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, code_change/3]).
-include_lib("maenmpc_db.hrl").

-record(spl, {
	db, syncidle, idx, len, is_rating, current_song,
	% keep track of MPD states as to allow toggle commands to be sent etc.
	mpd_volume, mpd_state, mpd_plength, mpd_repeat, mpd_random, mpd_single,
	mpd_consume, mpd_xfade, mpd_partition
}).

init(Properties) ->
	InitialState = #spl{
		db            = proplists:get_value(db,      Properties),
		syncidle      = proplists:get_value(syncidle,Properties),
		idx           = proplists:get_value(idx,     Properties),
		len           = proplists:get_value(len,     Properties),
		is_rating     = proplists:get_value(rating,  Properties, false),
		mpd_volume    = -1,
		mpd_state     = undefined,
		mpd_plength   = -1,
		mpd_repeat    = false,
		mpd_random    = false,
		mpd_single    = false,
		mpd_consume   = false,
		mpd_xfade     = 0,
		mpd_partition = <<"default">>
	},
	{ok, InitialState#spl{current_song=maenmpc_erlmpd:epsilon_song(
							InitialState#spl.len)}}.

handle_call(is_online, _From, Ctx) ->
	{reply, maenmpc_sync_idle:is_online(Ctx#spl.syncidle), Ctx};
handle_call(request_update, _From, Ctx) ->
	maenmpc_sync_idle:interrupt_no_tx(Ctx#spl.syncidle),
	{reply, ok, Ctx};
handle_call({query_by_keys, Keys}, _From, Ctx) ->
	% replaces populate with conn
	% (slightly less efficient, but more regular approach)
	{reply, maenmpc_sync_idle:run_transaction(Ctx#spl.syncidle, fun(Conn) ->
		[case erlmpd:find(Conn,
				{land, [{tagop, artist, eq, element(1, Key)},
					{tagop, album,  eq, element(2, Key)},
					{tagop, title,  eq, element(3, Key)}]})
		of
		% nothing assigned
		[] -> maenmpc_erlmpd:epsilon_song(Ctx#spl.len);
		% Single element found -- assign it!
		[Element|[]] -> query_rating(parse_metadata(Element, Ctx),
								Conn, Ctx);
		% result not unique - cannot safely assign
		[_Element|_Others] -> maenmpc_erlmpd:epsilon_song(Ctx#spl.len)
		% else error is fatal because the connection state may
		% be disrupted.
		end || Key <- Keys]
	end), Ctx};
handle_call({ui_simple, volume_change, Delta}, _From, Ctx) ->
	NewVal = Ctx#spl.mpd_volume + Delta,
	{reply, case Ctx#spl.mpd_volume /= -1 andalso
					 NewVal >= 0 andalso NewVal =< 100 of
		true -> case maenmpc_sync_idle:run_transaction(Ctx#spl.syncidle,
				fun(Conn) -> erlmpd:setvol(Conn, NewVal) end) of
			ok           -> ok;
			{error, Err} -> gen_server:cast(Ctx#spl.db,
					{mpd_assign_error, unknown, Err}), ok
			end;
		false -> ok
	end, Ctx};
handle_call({ui_simple, Action}, _From, Ctx) ->
	{reply, maenmpc_sync_idle:run_transaction(Ctx#spl.syncidle, fun(Conn) ->
		ui_simple_tx(Action, Conn, Ctx)
	end), Ctx};
handle_call({query_queue, ItemsRequested, CurrentQ}, _From, Ctx) ->
	{NewQ, Ctx2} = maenmpc_sync_idle:run_transaction(
						Ctx#spl.syncidle, fun(Conn) ->
		% We must only correct the qoffset in event that the playlist
		% length has changed. Otherwise don't allow changing the qoffset
		% as to not have list_append operations concatenate wrong lists!
		{Ctx1, Q0A} = case Ctx#spl.mpd_plength =< 0 of
			true -> Ctx1P = update_status(erlmpd:status(Conn), Ctx),
				{Ctx1P, max(0, min(CurrentQ#dbscroll.qoffset,
				Ctx1P#spl.mpd_plength - ItemsRequested))};
			false -> {Ctx, CurrentQ#dbscroll.qoffset}
			end,
		Q1A = max(Q0A + 1, min(Q0A + ItemsRequested,
							Ctx1#spl.mpd_plength)),
		{CurrentQ#dbscroll{
			cnt=[query_rating(parse_metadata(El, Ctx1), Conn, Ctx1)
				|| El <- erlmpd:playlistinfo(Conn, {Q0A, Q1A})],
			total=Ctx1#spl.mpd_plength,
			qoffset=Q0A
		}, Ctx1}
	end),
	{reply, NewQ, Ctx2};
handle_call({query_artists_count, Filter}, _From, Ctx) ->
	{reply, maenmpc_sync_idle:run_transaction(Ctx#spl.syncidle, fun(Conn) ->
		erlmpd:count_group(Conn, artist, Filter)
	end), Ctx};
handle_call({query_artists, QList, Filter}, _From, Ctx) ->
	{reply, maenmpc_sync_idle:run_transaction(Ctx#spl.syncidle, fun(Conn) ->
		[[query_rating(parse_metadata(El, Ctx), Conn, Ctx)
			|| El <- erlmpd:find(Conn, {land,
					[{tagop, artist, eq, Artist}, Filter]})]
		|| Artist <- QList]
	end), Ctx};
handle_call(query_output, _From, Ctx) ->
	{reply, maenmpc_sync_idle:run_transaction(Ctx#spl.syncidle, fun(Conn) ->
		% In order to query all outputs completely we temporarily switch
		% partitions. This should not be an issue since the player is
		% not going to do anything with the connection during the
		% running transaction here!
		Partitions = [proplists:get_value(partition, PL) ||
					PL <- erlmpd:listpartitions(Conn)],
		{Outputs, ActiveS} = lists:foldl(fun(Partition, StateIn) ->
			ok = erlmpd:partition(Conn, Partition),
			lists:foldl(fun(Out, {OList, ASet}) ->
				OutputID = proplists:get_value(outputid, Out),
				{[#dboutput{
					player_idx     = Ctx#spl.idx,
					partition_name = Partition,
					output_id      = OutputID,
					output_name    = proplists:get_value(
							outputname, Out)
				}|OList],
				case proplists:get_value(outputenabled, Out) of
					true -> sets:add_element({Ctx#spl.idx,
						Partition, OutputID}, ASet);
					false -> ASet
				end}
			end, StateIn, erlmpd:outputs(Conn))
		end, {[], sets:new()}, Partitions),
		ok = erlmpd:partition(Conn, Ctx#spl.mpd_partition),
		CandidateAssigned = sets:filter(fun({_Idx, Partition, _Output})
			-> Partition =:= Ctx#spl.mpd_partition end, ActiveS),
		#dboutputs{
			outputs    = Outputs,
			partitions = Partitions,
			active_set = ActiveS,
			assigned   = case sets:to_list(CandidateAssigned) of
					[] -> {none, Ctx#spl.mpd_partition};
					[{Pl, Par, _Out}|_Any] -> {Pl, Par}
					end
			% no claim on cursor possible
		}
	end), Ctx};
handle_call(query_search_limits, _From, Ctx) ->
	{reply, maenmpc_sync_idle:run_transaction(Ctx#spl.syncidle, fun(Conn) ->
		% [{1979,[]}, {1981,[]}, {1983,[]}, {1984,[]}, {1986,[]},
		AllYears = [string:to_integer(string:substr([Item], 1, 4)) ||
					Item <- erlmpd:list(Conn, date),
						string:length(Item) >= 4],
		% TODO Query ratings as follows: Binary search 0 upwards up to max 100 and 100 downawards down to min. 0 / {ok, Conn2} = erlmpd:connect("172.17.0.1", 6600), erlmpd:command(Conn2, "list date").
		#dbsearchlim{year=year_min_max(AllYears, 9999, 0),
								rating={0, 100}}
	end), Ctx};
% returns Item or `false` if no matching value found here
handle_call({search_by_artists, Direction, Artists, Query, Filter}, _Fr, Ctx) ->
	{reply, maenmpc_sync_idle:run_transaction(Ctx#spl.syncidle, fun(Conn) ->
		% entry match Artist and (title contains Q || album contains Q)
		Results = [maenmpc_erlmpd:to_dbsong(Ent, Ctx#spl.idx,
					Ctx#spl.len) || Ent <- erlmpd:search(
			Conn, {land, [
				Filter,
				case Artists of
				[Artist] ->
					{tagop, artist, eq, Artist};
				_MultipleArtists ->
					{lnot, {land,
						[{lnot, {tagop, artist, eq,
						Artist}} || Artist <- Artists]
					}}
				end,
				{lnot, {land, [
					% TODO x search: This is a hack to support searching by artist which does not result in the first matching albumb but rather first matching track becoming selected. it may be OK (although inefficient) this way. To optimize it, check via string matching if an artist may match and if yes bypass searching for their tracks and instead only get a virtual first track result that indicates we want to match an album? (Problem is that we cannot easily process the individual albums here without again sacrificing a lot of performance...
					{lnot, {tagop, artist, contains, Query}},
					{lnot, {tagop, title, contains, Query}},
					{lnot, {tagop, album, contains, Query}}
				]}}
			]})],
		case Results of
		[]                           -> false;
		[H|_T] when Direction =:=  1 -> H;
		List   when Direction =:= -1 -> lists:last(List)
		end
	end), Ctx};
handle_call({set_output, #dboutput{partition_name=Partition,
				output_name=OutputName, output_id=OutputID}},
				_From, Ctx) ->
	{reply, maenmpc_sync_idle:run_transaction(Ctx#spl.syncidle, fun(Conn) ->
		if
		Partition =/= Ctx#spl.mpd_partition ->
			ok = erlmpd:partition(Conn, Partition);
		true ->
			true
		end,
		% ignore RC and always execute moveoutput.
		% alternatively check whether it is already on the correct
		% output and only run this command if necessary
		erlmpd:moveoutput(Conn, OutputName),
		erlmpd:toggleoutput(Conn, OutputID)
	end), Ctx#spl{mpd_partition = Partition}};
handle_call({enqueue_end, Songs}, _From, Ctx) ->
	{reply, maenmpc_sync_idle:run_transaction(Ctx#spl.syncidle, fun(Conn) ->
		lists:foreach(fun(Song) ->
			ok = erlmpd:add(Conn, element(Ctx#spl.idx,
							Song#dbsong.uris))
		end, Songs)
	end), Ctx};
handle_call({enqueue_current, Songs}, _From, Ctx) ->
	{reply, maenmpc_sync_idle:run_transaction(Ctx#spl.syncidle, fun(Conn) ->
		enqueue_after_current(Songs, Conn, Ctx)
	end), Ctx};
handle_call({queue_delete, Songs}, _From, Ctx) ->
	{reply, maenmpc_sync_idle:run_transaction(Ctx#spl.syncidle, fun(Conn) ->
		erlmpd:deleteids(Conn, [Song#dbsong.playlist_id ||
								Song <- Songs])
	end), Ctx};
% TODO x when we were to support albums here, would have to go +1 or something
handle_call({play_from_playlist, [SelIt|_Others]}, _From, Ctx) ->
	{reply, maenmpc_sync_idle:run_transaction(Ctx#spl.syncidle, fun(Conn) ->
		erlmpd:playid(Conn, SelIt#dbsong.playlist_id)
	end), Ctx};
handle_call({play, Songs}, _From, Ctx) ->
	{reply, maenmpc_sync_idle:run_transaction(Ctx#spl.syncidle, fun(Conn) ->
		% This behaviour is a little “unconventional” because
		% ENTER is usually not really supposed to add after
		% the currently playing song. However, with my typical
		% usage it may even make more sense to do this way.
		% It might make sense to document the rationale behind,
		% this, though...
		FirstID = enqueue_after_current(Songs, Conn, Ctx),
		ok = erlmpd:playid(Conn, FirstID)
	end), Ctx};
handle_call({rating, Direction, Song}, _From, Ctx) when Ctx#spl.is_rating ->
	{reply, maenmpc_sync_idle:run_transaction(Ctx#spl.syncidle, fun(Conn) ->
		URI  = binary_to_list(element(Ctx#spl.idx, Song#dbsong.uris)),
		NewR = compute_and_transform_rating(Ctx, Direction,
							Song#dbsong.rating),
		if
		NewR =:= -1 ->
			erlmpd:sticker_delete(Conn, "song", URI, "rating"),
			?RATING_UNRATED;
		true ->
			erlmpd:sticker_set(Conn, "song", URI, "rating",
							integer_to_list(NewR)),
			maenmpc_erlmpd:convert_rating(NewR)
		end
	end), Ctx};
handle_call(_Call, _From, Ctx) ->
	{reply, ok, Ctx}.

parse_metadata({error, Descr}, Ctx) ->
	gen_server:cast(Ctx#spl.db, {mpd_assign_error, unknown, Descr}),
	maenmpc_erlmpd:epsilon_song(Ctx#spl.len);
parse_metadata(CurrentSong, Ctx) ->
	case proplists:get_value(file, CurrentSong) of
	undefined -> maenmpc_erlmpd:epsilon_song(Ctx#spl.len);
	_ValidVal -> maenmpc_erlmpd:to_dbsong(CurrentSong,
						Ctx#spl.idx, Ctx#spl.len)
	end.

query_rating(DBCMP, Conn, Ctx) ->
	case DBCMP#dbsong.key =:= {<<>>, <<>>, <<>>} orelse
							not Ctx#spl.is_rating of
	true  -> DBCMP;
	false -> DBCMP#dbsong{rating=rating_for_uri(element(Ctx#spl.idx,
						DBCMP#dbsong.uris), Conn)}
	end.

send_playing_info(Name, Status, Ctx) ->
	ok = gen_server:cast(Ctx#spl.db, {db_playing,
			[{x_maenmpc, Ctx#spl.current_song}|
			[{x_maenmpc_name, Name}|Status]]}),
	update_status(Status, Ctx).

update_status(Status, Ctx) ->
	Ctx#spl{mpd_volume  = proplists:get_value(volume,  Status, -1),
		mpd_plength = proplists:get_value(playlistlength, Status, -1),
		mpd_state   = proplists:get_value(state,   Status, undefined),
		mpd_repeat  = proplists:get_value(repeat,  Status, false),
		mpd_random  = proplists:get_value(random,  Status, false),
		mpd_single  = proplists:get_value(single,  Status, false),
		mpd_consume = proplists:get_value(consume, Status, false),
		mpd_xfade   = proplists:get_value(xfade,   Status, 0)}.

rating_for_uri(RatingURI, Conn) ->
	case erlmpd:sticker_get(Conn, "song", binary_to_list(RatingURI),
								"rating") of
	% Typically error just means not found here (OK)
	{error, _Any} -> ?RATING_UNRATED;
	ProperRating  -> maenmpc_erlmpd:convert_rating(
						list_to_integer(ProperRating))
	end.

ui_simple_tx(stop, Conn, _Ctx) ->
	erlmpd:stop(Conn);
ui_simple_tx(toggle_pause, Conn, Ctx) ->
	case Ctx#spl.mpd_state of
	undefined -> ok; % Cannot do anyhting in undefined state
	play      -> erlmpd:pause(Conn, true);
	pause     -> erlmpd:pause(Conn, false);
	stop      -> erlmpd:play(Conn)
	end;
ui_simple_tx(toggle_repeat, Conn, Ctx) ->
	erlmpd:repeat(Conn, not Ctx#spl.mpd_repeat);
ui_simple_tx(toggle_random, Conn, Ctx) ->
	erlmpd:random(Conn, not Ctx#spl.mpd_random);
ui_simple_tx(toggle_single, Conn, Ctx) ->
	erlmpd:single(Conn, not Ctx#spl.mpd_single);
ui_simple_tx(toggle_consume, Conn, Ctx) ->
	erlmpd:consume(Conn, not Ctx#spl.mpd_consume);
ui_simple_tx(toggle_xfade, Conn, Ctx) ->
	erlmpd:crossfade(Conn, max(0, 5 - Ctx#spl.mpd_xfade));
ui_simple_tx(song_previous, Conn, _Ctx) ->
	erlmpd:previous(Conn);
ui_simple_tx(song_next, Conn, _Ctx) ->
	erlmpd:next(Conn).

enqueue_after_current(Songs, Conn, Ctx) ->
	lists:foldl(fun({Song, Offset}, Acc) ->
		URI = element(Ctx#spl.idx, Song#dbsong.uris),
		ID  = case erlmpd:addid_relative(Conn, URI, Offset) of
			{error, {mpd_error, "55", _EPos, "addid", _NoCurSon}} ->
				% Special case no current song need to insert
				% using absolute offset...
				erlmpd:addid(Conn, URI, Ctx#spl.mpd_plength +
									Offset);
			IDOK -> IDOK
			end,
		case Acc of
		-1   -> ID;
		_Any -> Acc
		end
	end, -1, lists:zip(Songs, lists:seq(0, length(Songs) - 1))).

compute_and_transform_rating(_Ctx, Direction, OldRating) ->
	Delta = Direction * 20,
	if
	% TODO HARDCODED DEFAULT -> NEED TO MOVE OUT OF RADIO SECTION AND MAKE AVAILABLE THROUGH CTX
	OldRating =:= -1 -> (60 + Delta) div 10;
	% out of frange symbolizes “delete rating”
	OldRating + Delta < 0 orelse OldRating + Delta > 100 -> -1;
	true -> (OldRating + Delta) div 10
	end.

year_min_max([], Min, Max) ->
	{Min, Max};
year_min_max([{Year, _Suffix}|T], Min, Max) ->
	year_min_max(T, min(Year, Min), max(Year, Max)).

handle_cast(Msg={mpd_assign_error, _MPDName, _Reason}, Ctx) ->
	% bubble-up error
	gen_server:cast(Ctx#spl.db, Msg),
	{noreply, Ctx};
handle_cast({mpd_idle, Name, Subsystems}, Ctx) ->
	{noreply, maenmpc_sync_idle:run_transaction(
						Ctx#spl.syncidle, fun(Conn) ->
		Ctx1 = update_playing_info(Name, Conn, Ctx),
		case lists:member(playlist, Subsystems) of
			true   -> gen_server:cast(Ctx1#spl.db,
						{db_playlist_changed, Name});
			_Other -> ok
		end,
		Ctx1
	end)};
handle_cast(_Cast, Ctx) ->
	{noreply, Ctx}.

update_playing_info(Name, Conn, Ctx) ->
	% state, audio, volume, repeat, random, single, consume, xfade,
	% updating_db, time
	Status = erlmpd:status(Conn),
	% file [uri], Artist, Date, Album, Track, Title, Time [duration],
	CurrentSong = parse_metadata(erlmpd:currentsong(Conn), Ctx),
	send_playing_info(Name, Status, case CurrentSong#dbsong.key =:=
					Ctx#spl.current_song#dbsong.key of
			true  -> Ctx;
			false -> Ctx#spl{current_song=
					query_rating(CurrentSong, Conn, Ctx)}
			end).


handle_info(_Message,    Ctx)         -> {noreply, Ctx}.
code_change(_OldVersion, Ctx, _Extra) -> {ok,      Ctx}.
