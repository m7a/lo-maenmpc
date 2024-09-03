-module(maenmpc_radio).
-behavior(gen_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, code_change/3]).
-include_lib("stdlib/include/ms_transform.hrl").
-include_lib("maenmpc_db.hrl").

% radio record
-record(rr, {
	% -- static parts --
	% tbl: radio hash table (config), talk_to: Log server (multiplayer)
	primary_ratings, mpd_list, maloja, tbl, talk_to,
	% -- dynamic parts --
	active, schedule
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
		active          = false,
		schedule        = []
	}}.

handle_call(_Call, _From, Ctx) ->
	{ok, Ctx}.

handle_cast({service_start, MPDName}, Ctx) ->
	{noreply, radio_start(MPDName, Ctx)};
handle_cast(service_stop, Ctx) ->
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

log(Msg, Ctx) ->
	maenmpc_svc:log(Ctx#rr.talk_to, #dblog{msg=Msg, origin=radio}),
	Ctx.

radio_start(MPDName, Ctx0) when Ctx0#rr.active =:= false ->
	Ctx1 = Ctx0#rr{active=MPDName},
	log("start radio", Ctx1),
	ets:new(plsongs, [set, named_table, {keypos, #dbsong.key}]),

	Len        = length(Ctx1#rr.mpd_list),
	Key2Idx    = lists:zip([Name || {Name, _ConnInfo} <- Ctx1#rr.mpd_list],
							lists:seq(1, Len)),
	RatingsIdx = proplists:get_value(Ctx1#rr.primary_ratings, Key2Idx),
	UseIdx     = proplists:get_value(Ctx1#rr.active,          Key2Idx),

	log("query primary", Ctx1),
	foreach_all(fun(PLS) ->
		case ets:insert_new(plsongs, PLS) of
		true  -> ok;
		false -> log(io_lib:format("duplicate ~s/~s/~s",
				[element(1, PLS#dbsong.key),
				element(2, PLS#dbsong.key),
				element(3, PLS#dbsong.key)]), Ctx1)
		end
	end, Ctx1#rr.primary_ratings, RatingsIdx, Ctx1),

	log("query use", Ctx1),
	foreach_all(fun(PLS) ->
		case ets:lookup(plsongs, PLS#dbsong.key) of
		[] -> % Not found
			ets:insert(plsongs, PLS),
			log(io_lib:format("not found locally: ~s/~s/~s",
				[element(1, PLS#dbsong.key),
				element(2, PLS#dbsong.key),
				element(3, PLS#dbsong.key)]), Ctx1);
		[PLIDB] ->
			NewURIs = maenmpc_erlmpd:merge_tuple(
				PLS#dbsong.uris, PLIDB#dbsong.uris),
			case NewURIs =/= PLS#dbsong.uris of
			true  -> true = ets:update_element(plsongs,
					PLS#dbsong.key,
					[{#dbsong.uris, NewURIs}]);
			false -> true
			end
		end
	end, Ctx1#rr.active, UseIdx, Ctx1),

	log("associate playcounts", Ctx1),
	{Ctx2, Skip} = maenmpc_maloja:foldl_scrobbles(
		fun scrobble_to_playcount/2, {Ctx1, 0}, Ctx1#rr.maloja),
	log(io_lib:format("~w playcounts skipped", [Skip]), Ctx2),

	log("associate ratings", Ctx2),
	{ok, ConnRatings} = maenmpc_erlmpd:connect(proplists:get_value(
				Ctx2#rr.primary_ratings, Ctx2#rr.mpd_list)),
	RatingsRaw = erlmpd:sticker_find(ConnRatings, "song", "", "rating"),
	erlmpd:disconnect(ConnRatings),
	lists:foreach(fun(RawRating) ->
		assign_rating(RawRating, RatingsIdx, Ctx2)
	end, RatingsRaw),

	log("drop primary only", Ctx2),
	RM = ets:select(plsongs, ets:fun2ms(fun(X) when
		element(UseIdx, X#dbsong.uris) =:= <<>> -> X#dbsong.key end)),
	lists:foreach(fun(K) -> ets:delete(plsongs, K) end, RM),
	log(io_lib:format("~w items dropped", [length(RM)]), Ctx2),

	% a bit of a specific hack, but better than hardcoding the path which
	% was what we did before...
	log("filter non-radio songs...", Ctx2),
	CMP   = maps:get(discard_prefix, Ctx2#rr.tbl),
	CMPL  = byte_size(CMP),
	RM2 = ets:select(plsongs, ets:fun2ms(fun(X) when binary_part(element(
				RatingsIdx, X#dbsong.uris), {0, CMPL}) =:= CMP
				-> X#dbsong.key end)),
	lists:foreach(fun(K) -> ets:delete(plsongs, K) end, RM2),
	log(io_lib:format("~w items filtered", [length(RM2)]), Ctx2),

	radio_play(Ctx2);
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
foreach_all(Func, MPD, Idx, Ctx) ->
	DefaultRating = maps:get(default_rating, Ctx#rr.tbl),
	Len           = length(Ctx#rr.mpd_list),
	{ok, Conn}    = maenmpc_erlmpd:connect(proplists:get_value(MPD,
							Ctx#rr.mpd_list)),
	lists:foreach(
		fun(RawEntry) ->
			PLS = maenmpc_erlmpd:to_dbsong(RawEntry, Idx, Len),
			Func(PLS#dbsong{rating=DefaultRating, playcount=0})
		end,
		erlmpd:find(Conn, {lnot, {land, [
			{tagop, artist, eq, ""},
			{tagop, album,  eq, ""},
			{tagop, title,  eq, ""}
		]}})
	),
	erlmpd:disconnect(Conn).

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
	Result = lists:foldl(fun({Title, ArtistRaw}, Acc) ->
		case Acc of
		1 -> 1;
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
					0;
				[AlbumI] ->
					plsongs_inc({Artist, AlbumI, Title});
				[AlbumI|_OtherAlbums] ->
					log(io_lib:format(
						"not unique: ~s/~s -> ~s",
						[Artist, Title, AlbumI]), Ctx),
					plsongs_inc({Artist, AlbumI, Title})
				end;
			_Other ->
				AlbumTitle = maenmpc_erlmpd:normalize_strong(
						maps:get(<<"albumtitle">>,
						ScrobbleAlbum)),
				CheckKey = {Artist, AlbumTitle, Title},
				case ets:lookup(plsongs, CheckKey) of
				[]      -> 0;
				[_Item] -> plsongs_inc(CheckKey)
				% other case prevented by set property of table!
				end
			end
		end
	end, 0, [{Title, Artist} || Title <- Titles, Artist <- Artists]),
	{Ctx, Ctr0 + (1 - Result)}.

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
			true;
		BinVal ->
			Rating = maenmpc_erlmpd:convert_rating(
						binary_to_integer(BinVal)),
			true = ets:update_element(plsongs, UniqueKey,
						{#dbsong.rating, Rating})
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
schedule_compute(CtxI=#rr{tbl=Conf}) ->
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
	log("distribution of stars", CtxI),
	log(io_lib:format(" 1 star  ~5w (ignore)", [Count1]), CtxI),
	log(io_lib:format(" 2 stars ~5w (~5.2f%)", [Count2, Perce2]), CtxI),
	log(io_lib:format(" 3 stars ~5w (~5.2f%)", [Count3, Perce3]), CtxI),
	log(io_lib:format(" 4 stars ~5w (~5.2f%)", [Count4, Perce4]), CtxI),
	log(io_lib:format(" 5 stars ~5w (~5.2f%)", [Count5, Perce5]), CtxI),
	Duplicate = case Perce4 + Perce5 < MinGoodPerc of
			true  -> trunc(MinGoodPerc / (Perce4 + Perce5));
			false -> 1
			end,
	log(io_lib:format("duplicate = ~w", [Duplicate]), CtxI),
	Schedule = schedule_merge([
		schedule_shuffle(ChaosFactor, Group5, Duplicate),
		schedule_shuffle(ChaosFactor, Group4, Duplicate),
		schedule_shuffle(ChaosFactor, Group3, 1),
		schedule_shuffle(ChaosFactor, Group2, 1)
	], ScheduleLen, InitialFactor),

	log(io_lib:format("schedule of ~w songs", [length(Schedule)]), CtxI),
	lists:foldl(fun(ID, CtxII) ->
		[Entry] = ets:lookup(plsongs, ID),
		log(io_lib:format("~s ~4w ~s - ~s",
			[maenmpc_erlmpd:format_rating(Entry#dbsong.rating),
			Entry#dbsong.playcount, element(1, Entry#dbsong.key),
			element(3, Entry#dbsong.key)]), CtxII),
		% Yes, its inefficient here but other ways would be much more
		% convoluted! Need to assign log ID to entry and keep it all
		% in order because log has a side-effect!
		% TODO x NEWLY IT MAY NO LONGER BE NECESSARY - COULD SPLIT OUT LIST GENERATION AND PRINTING?
		CtxII#rr{schedule=CtxII#rr.schedule ++ [{ID, 0}]}
	end, CtxI#rr{schedule=[]}, Schedule).

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
