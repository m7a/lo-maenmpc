-module(maenmpc_multiplayer).
-behavior(gen_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, code_change/3]).
-include_lib("maenmpc_db.hrl").

-record(mpl, {
	ui, alsa, mpd_list, mpd_active,
	%mpd_ratings, % TODO UNUSED MAY NEED IT TO EDIT RATING!
	maloja_url, maloja_key,
	current_song, current_queue, current_list, current_filter
}).

init([NotifyToUI]) ->
	%{ok, PrimaryRatings} = application:get_env(maenmpc, primary_ratings),
	{ok, MPDList}        = application:get_env(maenmpc, mpd),
	{ok, ALSAHWInfo}     = application:get_env(maenmpc, alsa),
	{ok, Maloja}         = application:get_env(maenmpc, maloja),
	MPDFirst = m16, % TODO DEBUG ONLY
	%[MPDFirst|_Others] = MPDList,
	timer:send_interval(5000, interrupt_idle),
	MPDListIdx = [{Name, Idx} || {{Name, _ConnInfo}, Idx} <-
			lists:zip(MPDList, lists:seq(1, length(MPDList)))],
	gen_server:cast(NotifyToUI, {db_cidx,
				proplists:get_value(MPDFirst, MPDListIdx)}),
	{ok, #mpl{
		ui             = NotifyToUI,
		alsa           = ALSAHWInfo,
		mpd_list       = MPDListIdx, % [{name, idx}]
		mpd_active     = MPDFirst,
		%mpd_ratings   = PrimaryRatings,
		maloja_url     = proplists:get_value(url, Maloja, none),
		maloja_key     = proplists:get_value(key, Maloja, none),
		current_song   = #dbsong{key={<<>>, <<>>, <<>>}},
		current_queue  = #queue{cnt=[], total=-1, qoffset=0, doffset=0,
					dsel=0, last_query_len=0},
		current_list   = #slist{cnt=[], artists=[],
					dsong={<<>>,<<>>,<<>>},
					ssong={<<>>,<<>>,<<>>},
					last_query_len=0},
		current_filter = {lnot, {land, [{tagop, artist, eq, ""},
						{tagop, album,  eq, ""},
						{tagop, title,  eq, ""}]}}
	}}.

handle_call(_Call, _From, Ctx) ->
	{reply, ok, Ctx}.

handle_cast({db_playing, Info}, Ctx) ->
	{noreply, case Ctx#mpl.mpd_active =:=
				proplists:get_value(x_maenmpc_name, Info) of
	true ->
		SongInfo = proplists:get_value(x_maenmpc, Info),
		{NewCtx, SendToUI} = case SongInfo#dbsong.key =:=
					Ctx#mpl.current_song#dbsong.key of
		true ->
			{Ctx, replace_song_info(Info, Ctx#mpl.current_song)};
		false ->
			NewSong = lists:foldl(fun({Name, _Idx}, S) ->
						complete_song_info(Name, S, Ctx)
					end, SongInfo, Ctx#mpl.mpd_list),
			NCtx = Ctx#mpl{current_song=NewSong},
			gen_server:cast(NCtx#mpl.ui, {db_queue,
						NCtx#mpl.current_queue,
						NewSong#dbsong.playlist_id}),
			{NCtx, replace_song_info(Info, NewSong)}
		end,
		gen_server:cast(Ctx#mpl.ui, {db_playing,
			[{x_maenmpc_alsa, query_alsa(Ctx#mpl.alsa)}|SendToUI]}),
		NewCtx;
	false ->
		% Don't care what other players are playing...
		Ctx
	end};
handle_cast({db_playlist_changed, Name}, Ctx) ->
	{noreply, case Ctx#mpl.mpd_active =:= Name of
	% Updated playlist is equal to out of range, because we cannot know if
	% we are out of range or not...
	true  -> proc_range_result(out_of_range, Ctx);
	false -> Ctx
	end};
handle_cast(R={ui_simple, _A, _B}, Ctx) ->
	call_singleplayer(Ctx#mpl.mpd_active, R),
	{noreply, Ctx};
handle_cast(R={ui_simple, _A}, Ctx) ->
	call_singleplayer(Ctx#mpl.mpd_active, R),
	{noreply, Ctx};
handle_cast({ui_queue, ItemsRequested}, Ctx) ->
	{noreply, check_range_and_proc(Ctx#mpl{current_queue=
		Ctx#mpl.current_queue#queue{last_query_len=ItemsRequested}})};
handle_cast({ui_queue_scroll, Offset, ItemsRequested}, Ctx) ->
	DOffset = Ctx#mpl.current_queue#queue.doffset,
	NewDSEL = max(0, min(Ctx#mpl.current_queue#queue.total - 1,
				Ctx#mpl.current_queue#queue.dsel + Offset)),
	NewDOffset = case NewDSEL < DOffset orelse NewDSEL >= DOffset +
							ItemsRequested of
		     true  -> max(0, min(Ctx#mpl.current_queue#queue.total -
					ItemsRequested, DOffset + Offset));
		     false -> DOffset
		     end,
	{noreply, check_range_and_proc(Ctx#mpl{current_queue=
		Ctx#mpl.current_queue#queue{
			dsel           = NewDSEL,
			doffset        = NewDOffset,
			last_query_len = ItemsRequested
		}}
	)};
handle_cast({ui_list, ItemsRequested}, Ctx) ->
	CtxN = Ctx#mpl{current_list=Ctx#mpl.current_list#slist{
					last_query_len=ItemsRequested}},
	{noreply, case CtxN#mpl.current_list#slist.artists == [] of
		true  -> query_list_new(CtxN);
		false -> query_list_inc(CtxN)
	end};
handle_cast({ui_list_scroll, Offset, ItemsRequested}, Ctx) ->
	{noreply, ui_list_scroll(Offset, ItemsRequested, Ctx)};
handle_cast({mpd_assign_error, Name, Reason}, Ctx) ->
	gen_server:cast(Ctx#mpl.ui, {db_error, {offline, Name, Reason}}),
	{noreply, Ctx};
handle_cast(_Cast, Ctx) ->
	{noreply, Ctx}.

replace_song_info(InfoIn, NewVal) ->
	[case IE of
		{x_maenmpc, _OldVal} -> {x_maenmpc, NewVal};
		OtherEntry           -> OtherEntry
	end || IE <- InfoIn].

complete_song_info(Name, SongInfo, Ctx) ->
	case Name =:= Ctx#mpl.mpd_active of
	true  -> SongInfo;
	% Race condition when player dies after this test, but there does not
	% seem to be a real way around this issue in general.
	false -> case call_singleplayer(Name, is_online) of
		true ->
			[H|_T] = complete_song_info_other(Name, [SongInfo]),
			H;
		false ->
			SongInfo
		end
	end.

complete_song_info_other(Name, SongInfos) ->
	OtherInfos = call_singleplayer(Name, {query_by_keys,
			[SongInfo#dbsong.key || SongInfo <- SongInfos]}),
	[case OtherInfo#dbsong.key =:= SongInfo#dbsong.key of
		true  -> merge_song_info(SongInfo, OtherInfo);
		false -> SongInfo
	end || {SongInfo, OtherInfo} <- lists:zip(SongInfos, OtherInfos)].

merge_song_info(Song, Other) ->
	Song#dbsong{
		uris   = merge_tuple(Song#dbsong.uris, Other#dbsong.uris),
		audios = merge_tuple(Song#dbsong.audios, Other#dbsong.audios),
		rating = case Song#dbsong.rating of
				-1   -> Other#dbsong.rating;
				_Any -> Song#dbsong.rating
			end
	}.

% TODO x MAY BE NEEDLESSLY SLOW MIGHT BE FASTER TO USE ITERATION INDEX BASED APPROACH AND TUPLE SET VALUE?
merge_tuple(T1, T2) ->
	list_to_tuple([case TE of {<<>>, TE2} -> TE2; {TE1, _Any} -> TE1 end ||
			TE <- lists:zip(tuple_to_list(T1), tuple_to_list(T2))]).

call_singleplayer(Name, Query) ->
	gen_server:call(list_to_atom("maenmpc_singleplayer_" ++
						atom_to_list(Name)), Query).

% $ cat /proc/asound/card0/pcm0p/sub0/hw_params
% closed
% $ cat /proc/asound/card0/pcm0p/sub0/hw_params
% format: S32_LE                <- could use for format? TODO x
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

check_range_and_proc(Ctx) ->
	proc_range_result(check_in_range(Ctx), Ctx).

check_in_range(Ctx) ->
	R       = Ctx#mpl.current_queue#queue.last_query_len,
	Len     = length(Ctx#mpl.current_queue#queue.cnt),
	DOffset = Ctx#mpl.current_queue#queue.doffset,
	QOffset = Ctx#mpl.current_queue#queue.qoffset,
	Total   = Ctx#mpl.current_queue#queue.total,
	if
	(DOffset - QOffset   >= R orelse QOffset       == 0) andalso
	(Len - (DOffset + R) >= R orelse QOffset + Len == Total) ->
		{in_range, ok};
	DOffset - QOffset >= 0 andalso (QOffset + Len) - (DOffset + R) >= 0 ->
		case DOffset - QOffset < R of
		true  -> {in_range, query_before};
		false -> {in_range, query_after}
		end;
	true ->
		out_of_range
	end.

proc_range_result(RangeResult, Ctx) ->
	ItemsRequested = Ctx#mpl.current_queue#queue.last_query_len,
	PreCtx = query_playcount(case RangeResult of
		out_of_range ->
			DOffset = Ctx#mpl.current_queue#queue.doffset,
			{QOffset, NumQuery} = case DOffset < 2 *
							ItemsRequested of
				true  -> {0, DOffset + 3 * ItemsRequested};
				false -> {DOffset - 2 * ItemsRequested,
							5 * ItemsRequested}
			end,
			Ctx#mpl{current_queue=query_queue(NumQuery,
				Ctx#mpl.current_queue#queue{qoffset=QOffset},
				Ctx)};
		_Other1 -> Ctx
		end),
	gen_server:cast(PreCtx#mpl.ui, {db_queue, PreCtx#mpl.current_queue,
				PreCtx#mpl.current_song#dbsong.playlist_id}),
	case RangeResult of
	{in_range, ok} ->
		PreCtx;
	{in_range, query_before} when
				PreCtx#mpl.current_queue#queue.qoffset > 0 ->
		QOffset2 = max(0, PreCtx#mpl.current_queue#queue.qoffset
							- ItemsRequested),
		QReq = min(PreCtx#mpl.current_queue#queue.qoffset,
							ItemsRequested),
		append_queue(query_queue(QReq, #queue{qoffset=QOffset2},
							PreCtx), PreCtx);
	{in_range, query_after} ->
		QOffset2 = PreCtx#mpl.current_queue#queue.qoffset +
				length(PreCtx#mpl.current_queue#queue.cnt),
		QReq = min(ItemsRequested, PreCtx#mpl.current_queue#queue.total
				- PreCtx#mpl.current_queue#queue.qoffset),
		case QReq =< 0 of
		true  -> PreCtx;
		false -> append_queue(query_queue(QReq,
				#queue{qoffset=QOffset2}, PreCtx), PreCtx)
		end;
	_Other2 ->
		PreCtx
	end.

query_playcount(Ctx) when Ctx#mpl.maloja_url =:= none orelse
				Ctx#mpl.current_queue#queue.cnt == [] ->
	Ctx;
query_playcount(Ctx) ->
	DOffset = Ctx#mpl.current_queue#queue.dsel -
					Ctx#mpl.current_queue#queue.qoffset,
	Prefix = lists:sublist(Ctx#mpl.current_queue#queue.cnt, 1, DOffset),
	[Sel|Suffix] = lists:nthtail(DOffset, Ctx#mpl.current_queue#queue.cnt),
	case Sel#dbsong.playcount < 0 andalso
					Sel#dbsong.key =/= {<<>>, <<>>, <<>>} of
	true ->
		Query = binary_to_list(iolist_to_binary(io_lib:format(
			"~s/trackinfo?key=~s&trackartist=~s&title=~s", [
				Ctx#mpl.maloja_url,
				uri_string:quote(Ctx#mpl.maloja_key),
				uri_string:quote(element(1, Sel#dbsong.key)),
				uri_string:quote(element(3, Sel#dbsong.key))
			]))),
		{ok, {_Status, _Headers, InfoRaw}} = httpc:request(Query),
		Map = jiffy:decode(InfoRaw, [return_maps]),
		NewPC = case maps:get(<<"scrobbles">>, Map, -1) of
		-1 ->
			EDsc = maps:get(<<"error">>, Map, unknown),
			case is_map(EDsc) andalso
					maps:get(<<"type">>, EDsc, unknown) =:=
					<<"entity_does_not_exist">> of
			true  -> 0;
			false -> gen_server:cast(Ctx#mpl.ui,
						{db_error, {maloja, EDsc}}), -1
			end;
		Value1 -> Value1
		end,
		case NewPC of
		-1    -> Ctx;
		Value -> Ctx#mpl{current_queue=Ctx#mpl.current_queue#queue{cnt=
				Prefix ++ [Sel#dbsong{playcount=Value}|Suffix]}}
		end;
	false ->
		Ctx
	end.

query_queue(ItemsRequested, QIn, Ctx) ->
	InstancesToQuery = lists:filtermap(fun({Name, _Idx}) ->
			case Name /= Ctx#mpl.mpd_active andalso
					call_singleplayer(Name, is_online) of
			true  -> {true, Name};
			false -> false
			end
		end, Ctx#mpl.mpd_list),
	PreliminaryQueue = call_singleplayer(Ctx#mpl.mpd_active, {query_queue,
							ItemsRequested, QIn}),
	PreliminaryQueue#queue{cnt=lists:foldl(fun complete_song_info_other/2,
				PreliminaryQueue#queue.cnt, InstancesToQuery)}.

append_queue(NewQueue, Ctx) ->
	OldQueue = Ctx#mpl.current_queue,
	Ctx#mpl{current_queue=case NewQueue#queue.qoffset <
						OldQueue#queue.qoffset of
	true -> OldQueue#queue{
			cnt     = NewQueue#queue.cnt ++ OldQueue#queue.cnt,
			qoffset = NewQueue#queue.qoffset,
			total   = NewQueue#queue.total
		};
	false -> OldQueue#queue{
			cnt     = OldQueue#queue.cnt ++ NewQueue#queue.cnt,
			total   = NewQueue#queue.total
		}
	end}.

query_list_new(Ctx) ->
	Artists = merge_artists([call_singleplayer(Name, {query_artists_count,
							Ctx#mpl.current_filter})
					|| {Name, _Idx} <- Ctx#mpl.mpd_list]),
	List0 = Ctx#mpl.current_list#slist{cnt=[], artists=Artists},
	query_list_inc(Ctx#mpl{current_list=List0}).

merge_artists(ArtistsRaw) ->
	merge_by_criterion(ArtistsRaw,
			fun(Propl) -> proplists:get_value('Artist', Propl) end,
			0,
			fun(Propl) -> proplists:get_value(songs, Propl) end,
			fun(Sel, Vals) -> #sartist{
				name=Sel, results=list_to_tuple(Vals),
				minsz=lists:max(Vals), knownsz=-1
			} end).

merge_by_criterion(ListRaw, GetKeyCB, Epsilon, ExtractCB, FinalizeCB) ->
	Heads = [case X of [] -> empty; [H|_T] -> H end || X <- ListRaw],
	NonEmptyHeads = lists:filter(fun(X) -> X =/= empty end, Heads),
	case NonEmptyHeads =:= [] of
	true -> []; % Terminate on empty
	false ->
		Keys = [GetKeyCB(List) || List <- NonEmptyHeads],
		[Sel|_Other] = lists:sort(Keys),
		{Vals, Tails} = lists:unzip([case X of
				[] -> {Epsilon, X};
				[H|T] ->
					case GetKeyCB(H) =:= Sel of
					true  -> {ExtractCB(H), T};
					false -> {Epsilon, X}
					end
				end || X <- ListRaw]),
		[FinalizeCB(Sel, Vals)|merge_by_criterion(
			Tails, GetKeyCB, Epsilon, ExtractCB, FinalizeCB)]
	end.

query_list_inc(Ctx) ->
	query_list_inc(Ctx, element(1, Ctx#mpl.current_list#slist.dsong)).

query_list_inc(Ctx, DArtist0) ->
	List0 = Ctx#mpl.current_list,
	CtxRV = case List0#slist.artists of
	% When nothing is found at all there is nothing to do...
	[] ->
		Ctx;
	[H|_T] ->
		{List1, DArtist1} =
			case lists:any(fun(X) -> DArtist0 =:= X#sartist.name
						end, List0#slist.artists) of
			true  -> {List0, DArtist0};
			false -> {List0#slist{dsong={<<>>,<<>>,<<>>}},
								H#sartist.name}
			end,
		QList = dartist_to_qlist(List1, DArtist1,
						List0#slist.last_query_len),
		query_list_artists(QList, Ctx#mpl{current_list=List1})
	end,
	gen_server:cast(CtxRV#mpl.ui, {db_list, CtxRV#mpl.current_list}),
	CtxRV.

% TODO SPLIT AND assemble_artists BASED APPROACH MAY BE BETTER REALLY...
dartist_to_qlist(List, DArtist, NumQuery) ->
	{Prefix, Suffix} = lists:splitwith(fun(LE) ->
			LE#sartist.name =< DArtist end, List#slist.artists),
	{NumPre, SelPre} = lists:foldr(fun(EL, ValList) ->
				count_lim(NumQuery, EL, ValList)
			end, {0, []}, Prefix),
	NumREM = NumQuery * 5 - min(NumPre, NumQuery),
	{_NumPost, SelPost} = lists:foldl(fun(EL, ValList) ->
				count_lim(NumREM, EL, ValList)
			end, {0, []}, Suffix),
	SelPre ++ lists:reverse(SelPost).

count_lim(Lim, EL, {Val, List}) ->
	case Val > Lim of
	true  -> {Val,                    List};
	false -> {Val + EL#sartist.minsz, [EL|List]}
	end.

query_list_artists(QList, Ctx) ->
	NewCnt = query_list_artists_songs(QList, Ctx),
	OldList = Ctx#mpl.current_list,
	NewSong = case OldList#slist.dsong of
			{<<>>,<<>>,<<>>} when length(NewCnt) > 0 ->
				[H|_T2] = NewCnt,
				H#dbsong.key;
			Other -> Other
		end,
	Ctx#mpl{current_list=OldList#slist{cnt = NewCnt, dsong = NewSong,
							ssong = NewSong}}.

query_list_artists_songs(QList, Ctx) ->
	merge_songs(lists:append([call_singleplayer(Name, {query_artists,
					[EL#sartist.name || EL <- QList],
					Ctx#mpl.current_filter})
				|| {Name, _Idx} <- Ctx#mpl.mpd_list])).

merge_songs(SongsRaw) ->
	merge_by_criterion(SongsRaw,
		fun(Song) -> Song#dbsong.key end,
		skip,
		fun(H) -> H end,
		fun(_Key, Vals) ->
			[VH|VT] = lists:filter(fun(X) -> X =/= skip end, Vals),
			lists:foldl(fun merge_song_info/2, VH, VT)
		end).

% TODO X ALL SCROLL RELATED ROUTINES SHOULD BE REVISED, ENHANCED AND UNIFIED AFTER INTRODUCTION OF DUMMY/ALBUM SONG CONCEPT...
ui_list_scroll(Offset, ItemsRequested, Ctx0) ->
	Ctx1 = Ctx0#mpl{current_list=Ctx0#mpl.current_list#slist{
						last_query_len=ItemsRequested}},
	SearchCnt = case Offset < 0 of
		true  -> lists:reverse(Ctx1#mpl.current_list#slist.cnt);
		false -> Ctx1#mpl.current_list#slist.cnt
		end,
	{Result, _Before, After} =
		% TODO USE SSONG RATHER THAN DSONG?
		find_offset_offset_song(abs(Offset),
			Ctx1#mpl.current_list#slist.dsong, SearchCnt, 0),
	case Result of
	out_of_range_detached ->
		% TODO EXPERIMENTAL - IF WE JUMPED JUST JUMP TO THE ARTIST AND IGNORE THE SCROLL REQUEST FOR NOW
		query_list_inc(Ctx1);
	out_of_range_adjacent ->
		NumToGo = ItemsRequested - After,
		case Offset < 0 of
		true ->
			% Query before (TODO WHAT IF CNT IS EMPTY? - NEED TO HANDLE CASE OR PROVE THAT IT CANNOT OCCUR)
			[BeforeIt|_T] = Ctx1#mpl.current_list#slist.cnt,
			BeforeArtist = element(1, BeforeIt#dbsong.key),
			{BeforeArtists, _InAfter} = divide_list_by_pred(
				fun(Artist) ->
					Artist#sartist.name =:= BeforeArtist
				end, Ctx1#mpl.current_list#slist.artists),
			% TODO REVERSE OF REVERSE MADNESS, MERGE THEM FOR EFFICIENCY AND BETTER UNDERSTANDING
			ToQuery = lists:reverse(assemble_artists(
					lists:reverse(BeforeArtists), NumToGo +
					2 * ItemsRequested, [])),
			Songs = query_list_artists_songs(ToQuery, Ctx1),
			NewSong = case length(Songs) > NumToGo of
			true ->
				% Not at the edge, reverse lookup in songs the
				% missing number
				lists:nth(length(Songs) - NumToGo, Songs);
			false when Songs =:= [] ->
				[SH|_ST] = Ctx1#mpl.current_list#slist.cnt,
				SH;
			false ->
				% At the edge we select the “last” song which
				% is effectively the first one in the result.
				[SH|_ST] = Songs,
				SH
			end,
			List1 = Ctx1#mpl.current_list#slist{
				dsong = NewSong#dbsong.key,
				ssong = NewSong#dbsong.key,
				cnt   = Songs ++ Ctx1#mpl.current_list#slist.cnt
			},
			gen_server:cast(Ctx1#mpl.ui, {db_list, List1}),
			Ctx1#mpl{current_list=List1};
		false ->
			% Query after
			SAE = lists:last(Ctx1#mpl.current_list#slist.cnt),
			AfterArtist = element(1, SAE#dbsong.key),
			% TODO WHAT IF EMTPY ETC, does below case catch all the
			% relevant results?
			{_BeforeIt, [_Incl|AfterArtists]} =
				divide_list_by_pred(fun(Artist) ->
					Artist#sartist.name =:= AfterArtist
				end, Ctx1#mpl.current_list#slist.artists),
			ToQuery = assemble_artists(AfterArtists, NumToGo +
							3 * ItemsRequested, []),
			case AfterArtists =:= [] orelse ToQuery =:= [] of
			true ->
				% At the edge select the last in CNT
				Ctx1#mpl{current_list=
					Ctx1#mpl.current_list#slist{
						dsong=SAE#dbsong.key,
						ssong=SAE#dbsong.key}};
			false ->
				Songs = query_list_artists_songs(ToQuery, Ctx1),
				NewSong = case length(Songs) > NumToGo of
				true  -> lists:nth(NumToGo, Songs);
				% edge = select last
				false -> lists:last(Songs)
				end,
				NewCnt = Ctx1#mpl.current_list#slist.cnt ++
									Songs,
				% TODO INACCURATE W/O DUMMY ENTRIES
				DSong = lists:nth(max(1, length(NewCnt) -
						ItemsRequested), NewCnt),
				List1 = Ctx1#mpl.current_list#slist{
					dsong = DSong#dbsong.key,
					ssong = NewSong#dbsong.key,
					cnt   = NewCnt
				},
				gen_server:cast(Ctx1#mpl.ui, {db_list, List1}),
				Ctx1#mpl{current_list=List1}
			end
		end;
	_ResultFound ->
		% TODO CSTAT WE ARE AT A LIMIT HERE. TO SCROLL CORRECTLY, MUST CHANGE DSONG AND SSONG. THIS IS ONLY POSSIBLE IF THE DISTANCE BETWEEN THEM TWO IS WELL KNOWN AND WELL-COMPUTABLE, THOUGH. EITHER MODEL THE “ARTIST SEPARATION” IN THE COUNTING “find_offset_offset_song” OR GENERATE DUMMY SONGS AS ALBUM SEPARATORS (COULD ALSO SIMPLIFY THE DRAWING ROUTINE!)
		List1 = Ctx1#mpl.current_list#slist{dsong=Result#dbsong.key},
		gen_server:cast(Ctx1#mpl.ui, {db_list, List1}),
		Ctx2 = Ctx1#mpl{current_list=List1},
		case After > ItemsRequested of
		true ->
			% Enough result headroom, no need to adjust anything
			Ctx2;
		false ->
			% TODO Not enough result headroom, query artists and append/prepend to CNT until at least + ItemsRequested additional items have been queried!
			Ctx2
		end
	end.

find_offset_offset_song(_ItemsRequested, _CheckFor, [], Before) ->
	{out_of_range_detached, Before, 0};
find_offset_offset_song(ItemsRequested, CheckFor, [CntH|CntT], Before) ->
	case CntH#dbsong.key =:= CheckFor of
	true ->
		case length(CntT) >= ItemsRequested of
		true -> {lists:last(lists:sublist(CntT, ItemsRequested)),
					Before, length(CntT) - ItemsRequested};
		false -> {out_of_range_adjacent, Before, length(CntT)}
		end;
	false ->
		find_offset_offset_song(ItemsRequested, CheckFor, CntT,
								Before + 1)
	end.

% Split list by predicate and return a tuple with exactly two lists
% First list contains all items before the predicate matches (excl)
% Second list contains all items after the predicate matches (incl)
divide_list_by_pred(Predicate, List) ->
	divide_list_by_pred(Predicate, List, {[], []}).
divide_list_by_pred(_Predicate, [], {PreAcc, PostAcc}) ->
	{lists:reverse(PreAcc), lists:reverse(PostAcc)};
divide_list_by_pred(Predicate, [H|T], {PreAcc, PostAcc}) ->
	case PostAcc =/= [] orelse Predicate(H) of
	true  -> divide_list_by_pred(Predicate, T, {PreAcc, [H|PostAcc]});
	false -> divide_list_by_pred(Predicate, T, {[H|PreAcc], PostAcc})
	end.

assemble_artists(Artists, NReq, Acc) when NReq =< 0 orelse Artists =:= [] ->
	lists:reverse(Acc);
assemble_artists([Artist|Others], NReq, Acc) ->
	assemble_artists(Others, NReq - Artist#sartist.minsz, [Artist|Acc]).

% alternative implementation using foldl...
%divide_list_by_pred(Predicate, List) ->
%	{BeforeRev, AfterIncRev} = lists:foldl(fun(Item, {PreL, PostL}) ->
%		case PostL =/= orelse Predicate(Item) of
%			true ->  {PreL, [Item|PostL]};
%			false -> {[Item|PreL], PostL}
%		end
%	end, {[], []}, List),
%	{lists:reverse(BeforeRev), lists:reverse(AfterIncRev)}.

handle_info(interrupt_idle, Ctx) ->
	call_singleplayer(Ctx#mpl.mpd_active, request_update),
	{noreply, Ctx};
handle_info(_Message, Ctx) ->
	{noreply, Ctx}.

code_change(_OldVersion, Ctx, _Extra) -> {ok,      Ctx}.
