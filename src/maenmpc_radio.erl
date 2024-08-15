-module(maenmpc_radio).
-behavior(gen_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, code_change/3]).
-include_lib("stdlib/include/ms_transform.hrl").
-include_lib("maenmpc_db.hrl").

% radio record
-record(rr, {
	% -- static parts --
	% tbl: radio hash table
	primary_ratings, mpd_list, maloja, tbl, talk_to,
	% -- dynamic parts --
	log_id, active, schedule
}).

init([TalkTo]) ->
	{ok, PrimaryRatings} = application:get_env(maenmpc, primary_ratings),
	{ok, MPDList}        = application:get_env(maenmpc, mpd),
	{ok, Maloja}         = application:get_env(maenmpc, maloja),
	{ok, Radio}          = application:get_env(maenmpc, radio),
	{ok, #rr{
		primary_ratings = PrimaryRatings,
		mpd_list        = MPDList,
		maloja          = maenmpc_maloja:conn(Maloja),
		tbl             = Radio,
		talk_to         = TalkTo,
		log_id          = 0,
		active          = false,
		schedule        = []
	}}.

handle_call(_Call, _From, Ctx) ->
	{ok, Ctx}.

handle_cast({radio_start, MPDName}, Ctx) ->
	{noreply, radio_start(MPDName, Ctx)};
handle_cast(radio_stop, Ctx) ->
	{noreply, radio_stop(Ctx)};
handle_cast({db_playing, _SongProp}, Ctx=#rr{active=false}) ->
	{noreply, Ctx};
handle_cast({db_playing, SongProp}, Ctx=#rr{schedule=[{Key, _ID}|Tail]}) ->
	Song = proplists:get_value(x_maenmpc, SongProp),
	{noreply, case Song#dbsong.key =:= Key of
	true ->
		% matches, advance in playlist, enqueue next
		radio_enqueue(Ctx#rr{schedule=Tail});
	false ->
		% may come up later...
		Ctx
	end};
handle_cast(_Other, Ctx) ->
	{noreply, Ctx}.

log(Info, Ctx=#rr{log_id=ID}) ->
	UseID = ID + 1,
	ok = gen_server:cast(Ctx#rr.talk_to, {radio_log, UseID, Info}),
	Ctx#rr{log_id=UseID}.

radio_start(MPDName, Ctx0) when Ctx0#rr.active =:= false ->
	Ctx1 = log("start radio", Ctx0#rr{active=MPDName}),
	ets:new(plsongs, [set, named_table, {keypos, #dbsong.key}]),

	Len        = length(Ctx1#rr.mpd_list),
	Key2Idx    = lists:zip([Name || {Name, _ConnInfo} <- Ctx1#rr.mpd_list],
							lists:seq(1, Len)),
	RatingsIdx = proplists:get_value(Ctx1#rr.primary_ratings, Key2Idx),
	UseIdx     = proplists:get_value(Ctx1#rr.active,          Key2Idx),

	Ctx2 = log("query primary", Ctx1),
	Ctx3 = foldl_all(fun(PLS, CtxI) ->
			case ets:insert_new(plsongs, PLS) of
			true  -> CtxI;
			false -> log(io_lib:format("duplicate ~s/~s/~s",
					[element(1, PLS#dbsong.key),
					element(2, PLS#dbsong.key),
					element(3, PLS#dbsong.key)]), CtxI)
			end
		end, Ctx2, Ctx2#rr.primary_ratings, RatingsIdx, Ctx2),

	% Ctx4 reserved

	Ctx5 = log("query use", Ctx3),
	Ctx6 = foldl_all(fun(PLS, CtxI) ->
			case ets:lookup(plsongs, PLS#dbsong.key) of
			[] -> % Not found
				ets:insert(plsongs, PLS),
				log(io_lib:format("not found locally: ~s/~s/~s",
					[element(1, PLS#dbsong.key),
					element(2, PLS#dbsong.key),
					element(3, PLS#dbsong.key)]), CtxI);
			[PLIDB] ->
				NewURIs = maenmpc_erlmpd:merge_tuple(
					PLS#dbsong.uris, PLIDB#dbsong.uris),
				case NewURIs =/= PLS#dbsong.uris of
				true -> true = ets:update_element(plsongs,
						PLS#dbsong.key,
						[{#dbsong.uris, NewURIs}]);
				false -> true
				end,
				CtxI
			end
		end, Ctx5, Ctx5#rr.active, UseIdx, Ctx5),

	Ctx7 = log("associate playcounts", Ctx6),
	{Ctx8, Skip} = maenmpc_maloja:foldl_scrobbles(
		fun scrobble_to_playcount/2, {Ctx7, 0}, Ctx7#rr.maloja),
	Ctx8a = log(io_lib:format("~w playcounts skipped", [Skip]), Ctx8),

	Ctx9 = log("associate ratings", Ctx8a),
	{ok, ConnRatings} = maenmpc_erlmpd:connect(proplists:get_value(
				Ctx9#rr.primary_ratings, Ctx9#rr.mpd_list)),
	RatingsRaw = erlmpd:sticker_find(ConnRatings, "song", "", "rating"),
	erlmpd:disconnect(ConnRatings),
	Ctx10 = lists:foldl(fun(RawRating, CtxI) ->
				assign_rating(RawRating, RatingsIdx, CtxI)
			end, Ctx9, RatingsRaw),

	Ctx11 = log("drop primary only", Ctx10),
	RM = ets:select(plsongs, ets:fun2ms(fun(X) when
		element(UseIdx, X#dbsong.uris) =:= <<>> -> X#dbsong.key end)),
	lists:foreach(fun(K) -> ets:delete(plsongs, K) end, RM),
	Ctx12 = log(io_lib:format("~w items dropped", [length(RM)]), Ctx11),

	% a bit of a specific hack, but better than hardcoding the path which
	% was what we did before...
	Ctx13 = log("filter non-radio songs...", Ctx12),
	CMP   = maps:get(discard_prefix, Ctx13#rr.tbl),
	CMPL  = byte_size(CMP),
	RM2 = ets:select(plsongs, ets:fun2ms(fun(X) when binary_part(element(
				RatingsIdx, X#dbsong.uris), {0, CMPL}) =:= CMP
				-> X#dbsong.key end)),
	lists:foreach(fun(K) -> ets:delete(plsongs, K) end, RM2),
	Ctx14 = log(io_lib:format("~w items filtered", [length(RM2)]), Ctx13),

	radio_play(Ctx14);
radio_start(MPDName, Ctx) when Ctx#rr.active =/= false ->
	radio_start(MPDName, radio_stop(Ctx)).

% Function to traverse the entire DB. I know MPD recommends not to do this but
% in the use case it is required to do a full table scan (or come up with some
% totally overengineered solution with “sampling” and ”minimum expected
% playcount...”) and only a matter of how to do that most efficiently. The
% variant here is _not_ efficient yet, because it copies the whole DB to a list
% and then goes on to store it into ETS. It would be much more large-data size
% resilient to query a window range and insert in chunks of 1000 or 10000
% entries instead of doing the naive full table scan. We can improve the
% implementation if performance becomes too bad here...
foldl_all(Func, Acc, MPD, Idx, Ctx1) ->
	DefaultRating = maps:get(default_rating, Ctx1#rr.tbl),
	Len           = length(Ctx1#rr.mpd_list),
	{ok, Conn}    = maenmpc_erlmpd:connect(proplists:get_value(MPD,
							Ctx1#rr.mpd_list)),
	RV = lists:foldl(
		fun(RawEntry, Acc2) ->
			PLS = maenmpc_erlmpd:to_dbsong(RawEntry, Idx, Len),
			Func(PLS#dbsong{rating=DefaultRating, playcount=0},
									Acc2)
		end,
		Acc,
		erlmpd:find(Conn, {lnot, {land, [
			{tagop, artist, eq, ""},
			{tagop, album,  eq, ""},
			{tagop, title,  eq, ""}
		]}})
	),
	erlmpd:disconnect(Conn),
	RV.

scrobble_to_playcount(Scrobble, {Ctx, Ctr0}) ->
	ScrobbleTrack = maps:get(<<"track">>, Scrobble),
	ScrobbleAlbum = maps:get(<<"album">>, ScrobbleTrack),
	TitleRaw      = maps:get(<<"title">>, ScrobbleTrack),
	Titles        = [maenmpc_erlmpd:normalize_key(TitleRaw),
				maenmpc_erlmpd:normalize_strong(TitleRaw)],
	Artists = case maps:get(<<"artists">>, ScrobbleTrack) of
		[A1|[A2|[]]] ->
			[maenmpc_erlmpd:normalize_key([A1, <<" & ">>, A2])|
			[maenmpc_erlmpd:normalize_key([A2, <<" & ">>, A1])|
			[A1|[A2|[]]]]];
		OtherArtists ->
			OtherArtists
		end,
	{Result, CtxR} = lists:foldl(fun({Title, ArtistRaw}, {Acc, CtxI}) ->
		case Acc of
		1 -> {1, CtxI};
		0 ->
			Artist = maenmpc_erlmpd:normalize_key(ArtistRaw),
			case ScrobbleAlbum of
			null ->
				case ets:select(plsongs, ets:fun2ms(fun(X) when
					element(1, X#dbsong.key) == Artist
					andalso
					element(3, X#dbsong.key) == Title
					-> element(2, X#dbsong.key) end))
				of
				[] ->
					{0, CtxI};
				[AlbumI] ->
					{plsongs_inc({Artist, AlbumI, Title}),
						CtxI};
				[AlbumI|_OtherAlbums] ->
					{plsongs_inc({Artist, AlbumI, Title}),
					log(io_lib:format(
						"not unique: ~s/~s -> ~s",
						[Artist, Title, AlbumI]), CtxI)}
				end;
			_Other ->
				AlbumTitle = maenmpc_erlmpd:normalize_strong(
						maps:get(<<"albumtitle">>,
						ScrobbleAlbum)),
				CheckKey = {Artist, AlbumTitle, Title},
				case ets:lookup(plsongs, CheckKey) of
				[]      -> {0, CtxI};
				[_Item] -> {plsongs_inc(CheckKey), CtxI}
				% other case prevented by set property of table!
				end
			end
		end
	end, {0, Ctx}, [{Title, Artist} || Title <- Titles, Artist <- Artists]),
	{CtxR, Ctr0 + (1 - Result)}.

plsongs_inc(Key) ->
	ets:update_counter(plsongs, Key, {#dbsong.playcount, 1}),
	1.

assign_rating(Entry, RatingsIdx, Ctx) ->
	Path = maenmpc_erlmpd:normalize_always(proplists:get_value(file,
									Entry)),
	case ets:select(plsongs, ets:fun2ms(
			fun(X) when element(RatingsIdx, X#dbsong.uris) =:= Path
			-> X#dbsong.key end)) of
	[] ->
		log(io_lib:format("skip not in db: ~s", [Path]), Ctx);
	[UniqueKey] ->
		case proplists:get_value(rating, Entry, nothing) of
		nothing ->
			Ctx;
		BinVal ->
			Rating = maenmpc_erlmpd:convert_rating(
						binary_to_integer(BinVal)),
			true = ets:update_element(plsongs, UniqueKey,
						{#dbsong.rating, Rating}),
			Ctx
		end;
	MultipleResults ->
		log(io_lib:format("skip rating not unique: ~s: ~w",
						[Path, MultipleResults]), Ctx)
	end.

radio_play(Ctx) ->
	radio_enqueue(schedule_compute(log("compute schedule...", Ctx))).

% dequeue on recognize!
radio_enqueue(Ctx=#rr{talk_to=Dest, schedule=[{SongKey, ID}|T]}) ->
	[Value] = ets:lookup(plsongs, SongKey),
	gen_server:cast(Dest, {radio_enqueue, Value#dbsong{playlist_id=ID}}),
	case T of
	[] ->
		Ctx2 = schedule_compute(Ctx),
		% ensure that next time we recognize to input the next song!
		Ctx2#rr{schedule=[{SongKey, ID}|Ctx2#rr.schedule]};
	_More ->
		Ctx
	end.

% Compute a Music Schedule according to the following algorithm:
% Partition songs by rating, drop all 1-star rated songs, shuffle the per-rating
% lists, sort them by play count ASC, ensure that 4+5 stars are at least 30%
% (if not, repeat them as necessary) and then interleave the lists as to produce
% a fair playlist with enough good songs. Ordering by play count ensures that
% repeated execution of the same algorithm always yields diverse playlists.
schedule_compute(Ctx0=#rr{tbl=Conf}) ->
	MinGoodPerc   = maps:get(min_good_perc,  Conf),
	ChaosFactor   = maps:get(chaos_factor,   Conf),
	InitialFactor = maps:get(initial_factor, Conf),
	ScheduleLen   = maps:get(schedule_len,   Conf),
	% unclear why select_count did not return the intended output here?
	Count1 = length(ets:select(plsongs, schedule_construct_match(0))),
	Group2 = ets:select(plsongs, schedule_construct_match(20)),
	Group3 = ets:select(plsongs, schedule_construct_match(40)),
	Group4 = ets:select(plsongs, schedule_construct_match(60)),
	Group5 = ets:select(plsongs, schedule_construct_match(80)),
	Count2 = length(Group2),
	Count3 = length(Group3),
	Count4 = length(Group4),
	Count5 = length(Group5),
	CountT = Count2 + Count3 + Count4 + Count5,
	Perce2 = Count2 * 100 / CountT,
	Perce3 = Count3 * 100 / CountT,
	Perce4 = Count4 * 100 / CountT,
	Perce5 = Count5 * 100 / CountT,
	Ctx1 = log("distribution of stars", Ctx0),
	Ctx2 = log(io_lib:format(" 1 star  ~5w (ignore)", [Count1]), Ctx1),
	Ctx3 = log(io_lib:format(" 2 stars ~5w (~5.2f%)", [Count2, Perce2]),
									Ctx2),
	Ctx4 = log(io_lib:format(" 3 stars ~5w (~5.2f%)", [Count3, Perce3]),
									Ctx3),
	Ctx5 = log(io_lib:format(" 4 stars ~5w (~5.2f%)", [Count4, Perce4]),
									Ctx4),
	Ctx6 = log(io_lib:format(" 5 stars ~5w (~5.2f%)", [Count5, Perce5]),
									Ctx5),
	Duplicate = case Perce4 + Perce5 < MinGoodPerc of
			true  -> trunc(MinGoodPerc / (Perce4 + Perce5));
			false -> 1
			end,
	Ctx7 = log(io_lib:format("duplicate = ~w", [Duplicate]), Ctx6),
	Schedule = schedule_merge([
		schedule_shuffle(ChaosFactor, Group5, Duplicate),
		schedule_shuffle(ChaosFactor, Group4, Duplicate),
		schedule_shuffle(ChaosFactor, Group3, 1),
		schedule_shuffle(ChaosFactor, Group2, 1)
	], ScheduleLen, InitialFactor),

	Ctx8 = log(io_lib:format("schedule of ~w songs", [length(Schedule)]),
									Ctx7),
	lists:foldl(fun(ID, CtxI) ->
		[Entry] = ets:lookup(plsongs, ID),
		CtxI1 = log(io_lib:format("~s ~4w ~s - ~s",
			[maenmpc_erlmpd:format_rating(Entry#dbsong.rating),
			Entry#dbsong.playcount, element(1, Entry#dbsong.key),
			element(3, Entry#dbsong.key)]), CtxI),
		% Yes, its inefficient here but other ways would be much more
		% convoluted! Need to assign log ID to entry and keep it all
		% in order because log has a side-effect!
		CtxI1#rr{schedule=CtxI1#rr.schedule ++ [{ID, CtxI1#rr.log_id}]}
	end, Ctx8#rr{schedule=[]}, Schedule).

schedule_construct_match(Rating) ->
	ets:fun2ms(fun(X) when X#dbsong.rating > Rating andalso
				X#dbsong.rating =< (Rating + 20) -> X end).

% https://stackoverflow.com/questions/8817171/shuffling-elements-in-a-list-
schedule_shuffle(ChaosFactor, Group, Duplicate) ->
	lists:flatten(lists:map(fun(_Ctr) ->
		[Y || {_, Y} <- lists:sort([{rand:uniform() * ChaosFactor +
			S#dbsong.playcount, S#dbsong.key} || S <- Group])]
	end, lists:seq(1, Duplicate))).

schedule_merge(Groups, Limit, InitialFactor) ->
	NonEmptyGroups = lists:filter(fun (X) -> X /= [] end, Groups),
	schedule_merge_annotated([], Limit, lists:zipwith(fun(Group, ID) ->
			LGroup = length(Group),
			% Perc (“0.0”),         ID, Num, Of,     Group
			{ InitialFactor/LGroup, ID, 0,   LGroup, Group }
		end, NonEmptyGroups, lists:seq(1, length(NonEmptyGroups)))).

schedule_merge_annotated(Schedule, _Limit, []) ->
	lists:reverse(Schedule);
schedule_merge_annotated(Schedule, Limit, _AnnotatedGroups)
					when length(Schedule) >= Limit ->
	lists:reverse(Schedule);
schedule_merge_annotated(Schedule, Limit, AnnotatedGroups) ->
	{_Perc, SelID, Num, Of, [SelItem|SelRem]} = lists:min(AnnotatedGroups),
	Others = lists:filter(fun({_Perc2, ID, _Num, _Of, _Group}) ->
					ID /= SelID end, AnnotatedGroups),
	NewNum = Num + 1,
	NewPerc = NewNum / Of,
	case NewNum < Of of
	true  -> schedule_merge_annotated([SelItem|Schedule], Limit,
			[{NewPerc, SelID, NewNum, Of, SelRem}|Others]);
	false -> schedule_merge_annotated([SelItem|Schedule], Limit, Others)
	end.

radio_stop(Ctx) when Ctx#rr.active =:= false ->
	Ctx;
radio_stop(Ctx) ->
	ets:delete(plsongs),
	Ctx#rr{active=false}.

handle_info(_Message, Ctx)            -> {noreply, Ctx}.
code_change(_OldVersion, Ctx, _Extra) -> {ok, Ctx}.
