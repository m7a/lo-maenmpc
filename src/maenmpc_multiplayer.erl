-module(maenmpc_multiplayer).
-behavior(gen_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, code_change/3]).
-include_lib("maenmpc_db.hrl").

-record(mpl, {
	ui, radio, alsa, mpd_list, mpd_active,
	%mpd_ratings, % TODO UNUSED MAY NEED IT TO EDIT RATING!
	maloja,
	current_song, current_queue, current_list, current_radio,
	current_filter
}).

init([NotifyToUI, NotifyToRadio]) ->
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
		radio          = NotifyToRadio,
		alsa           = ALSAHWInfo,
		mpd_list       = MPDListIdx, % [{name, idx}]
		mpd_active     = MPDFirst,
		%mpd_ratings   = PrimaryRatings,
		maloja         = maenmpc_maloja:conn(Maloja),
		current_song   = maenmpc_erlmpd:epsilon_song(length(MPDList)),
		current_queue  = #dbscroll{type=queue, cnt=[], coffset=0,
					csel=0, total=-1, qoffset=0,
					last_query_len=0, user_data=none},
		current_list   = #dbscroll{type=list, cnt=[], coffset=0,
					csel=0, total=-1, last_query_len=0,
					qoffset=0, user_data={[], [], []}},
		current_radio  = #dbscroll{type=radio, cnt=[], coffset=0,
					csel=0, total=0, last_query_len=0,
					qoffset=0, user_data=-1},
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
			transform_and_send_to_ui(NCtx, NCtx#mpl.current_queue),
			{NCtx, replace_song_info(Info, NewSong)}
		end,
		gen_server:cast(Ctx#mpl.radio, {db_playing, SendToUI}),
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
	true -> Ctx#mpl{current_queue=list_replace(Ctx, Ctx#mpl.current_queue)};
	false -> Ctx
	end};
handle_cast(R={ui_simple, _A, _B}, Ctx) ->
	call_singleplayer(Ctx#mpl.mpd_active, R),
	{noreply, Ctx};
handle_cast(R={ui_simple, _A}, Ctx) ->
	call_singleplayer(Ctx#mpl.mpd_active, R),
	{noreply, Ctx};
handle_cast({ui_query, Action, ItemsRequested}, Ctx) ->
	{noreply, ui_query(Ctx,
			ui_items_requested(Ctx, Action, ItemsRequested))};
handle_cast({ui_scroll, Action, Offset, ItemsRequested}, Ctx) ->
	{noreply, ui_scroll(Ctx, Offset,
			ui_items_requested(Ctx, Action, ItemsRequested))};
handle_cast(ui_radio_start, Ctx) ->
	ok = gen_server:cast(maenmpc_radio, {radio_start, Ctx#mpl.mpd_active}),
	{noreply, Ctx};
handle_cast(ui_radio_stop, Ctx) ->
	ok = gen_server:cast(maenmpc_radio, {radio_stop, Ctx#mpl.mpd_active}),
	{noreply, Ctx};
% TODO IMPLEMENT UI SELECTED ENDPOINT HERE!
handle_cast({mpd_assign_error, Name, Reason}, Ctx) ->
	gen_server:cast(Ctx#mpl.ui, {db_error, {offline, Name, Reason}}),
	{noreply, Ctx};
handle_cast({radio_enqueue, DBSong}, Ctx=#mpl{current_radio=Radio}) ->
	call_singleplayer(Ctx#mpl.mpd_active, {enqueue, DBSong}),
	NewRadio = Radio#dbscroll{user_data=DBSong#dbsong.playlist_id},
	transform_and_send_to_ui(Ctx, NewRadio, NewRadio#dbscroll.user_data),
	{noreply, Ctx#mpl{current_radio=NewRadio}};
handle_cast({radio_log, ID, Info}, Ctx=#mpl{current_radio=Radio}) ->
	NewRadio = Radio#dbscroll{total=Radio#dbscroll.total + 1,
				cnt=Radio#dbscroll.cnt ++ [{ID, Info}]},
	transform_and_send_to_ui(Ctx, NewRadio, NewRadio#dbscroll.user_data),
	{noreply, Ctx#mpl{current_radio=NewRadio}};
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

call_singleplayer(Name, Query) ->
	gen_server:call(list_to_atom("maenmpc_singleplayer_" ++
						atom_to_list(Name)), Query).

merge_song_info(Song, Other) ->
	Song#dbsong{
		uris   = maenmpc_erlmpd:merge_tuple(Song#dbsong.uris,
							Other#dbsong.uris),
		audios = maenmpc_erlmpd:merge_tuple(Song#dbsong.audios,
							Other#dbsong.audios),
		rating = case Song#dbsong.rating of
				-1   -> Other#dbsong.rating;
				_Any -> Song#dbsong.rating
			end
	}.

transform_and_send_to_ui(Ctx, List) ->
	transform_and_send_to_ui(Ctx, List,
				Ctx#mpl.current_song#dbsong.playlist_id).

transform_and_send_to_ui(Ctx, List2=#dbscroll{cnt=Cnt, coffset=COffset,
				csel=CSel, last_query_len=LLen,
				qoffset=QOffset0}, CurrentSongID) ->
	gen_server:cast(Ctx#mpl.ui, {db_results, List2#dbscroll{
		cnt       = lists:sublist(Cnt, COffset + 1, LLen),
		coffset   = 0,
		csel      = CSel - COffset,
		user_data = {CurrentSongID, QOffset0 + COffset}
	}}).

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

ui_items_requested(Ctx, Action, ItemsRequested) ->
	case Action of
	queue -> Ctx#mpl.current_queue#dbscroll{last_query_len=ItemsRequested};
	list  -> Ctx#mpl.current_list #dbscroll{last_query_len=ItemsRequested};
	radio -> Ctx#mpl.current_radio#dbscroll{last_query_len=ItemsRequested}
	end.

ui_query(Ctx, List) ->
	proc_range_result(Ctx, check_in_range(List), List).

check_in_range(#dbscroll{cnt=Cnt, coffset=COffset, total=Total,
					last_query_len=R, qoffset=QOffset}) ->
	Len = length(Cnt),
	if
	COffset < 0 orelse Len - COffset < R ->
		out_of_range;
	COffset =< R andalso QOffset /= 0 ->
		{in_range, query_before};
	Len - COffset =< (2 * R) andalso QOffset + Len < Total ->
		{in_range, query_after};
	true ->
		{in_range, ok}
	end.

% TODO SMALL PROBLEM: WHAT IF WE DONT HAVE ANY ENTRIES AT ALL IN THIS LIST. NEGATIVE OFFSETS WERE OBSERVED ETC. IT CRASHES! / EVEN CRASHES OUTSIDE OF THAT / REALLY NEED TO ENFORCE THE LIMITS FOR TYPE RADIO!!!
proc_range_result(Ctx, _AnyRangeResult, List=#dbscroll{type=radio,
							user_data=Idx}) ->
	transform_and_send_to_ui(Ctx, List, Idx),
	Ctx#mpl{current_radio=List};
proc_range_result(Ctx, RangeResult,
			List=#dbscroll{last_query_len=ItemsRequested}) ->
	List2 = case RangeResult of
		out_of_range   -> query_playcount(Ctx, list_replace(Ctx, List));
		{in_range, ok} -> query_playcount(Ctx, List);
		_Other1        -> List
		end,
	transform_and_send_to_ui(Ctx, List2),
	List3 = case RangeResult of
		{in_range, ok} ->
			List2;
		{in_range, query_before} when List2#dbscroll.qoffset > 0 ->
			list_prepend(Ctx, List2, min(List2#dbscroll.qoffset,
						ItemsRequested));
		{in_range, query_after} ->
			list_append(Ctx, List2, min(ItemsRequested,
						List2#dbscroll.total -
						length(List2#dbscroll.cnt)));
		_Other2 ->
			List2
		end,
	case List3#dbscroll.type of
	queue -> Ctx#mpl{current_queue=List3};
	list  -> Ctx#mpl{current_list =List3}
	end.

list_replace(Ctx, List=#dbscroll{type=list, cnt=OldCnt,
					csel=CSel, last_query_len=NReq}) ->
	{ArtistBefore, PrefixLRev} =
		case CSel > 0 andalso CSel < length(OldCnt) of
		true  -> SelBefore = lists:nth(CSel, OldCnt),
			 {element(1, SelBefore#dbsong.key),
				lists:reverse(lists:sublist(OldCnt, CSel))};
		false -> {<<>>, []}
		end,
	% OIAB := Offset In Artist Before
	OIABefore = count_while(ArtistBefore, PrefixLRev, 0),
	Artists = query_all_artists(Ctx),
	{BeforeRev1, IncAfterArtists} =
		% When there is a degenerate result with no match, everything
		% comes out in the first part. For the remainder of the routine
		% to process it correctly, we then have to swap it and pass it
		% off as a “second part”.
		case divide_list_by_pred_r1(fun({Name, _Songs}) ->
					Name =:= ArtistBefore end, Artists) of
		{FirstRevOnly, []} -> {[], lists:reverse(FirstRevOnly)};
		Other              -> Other
		end,
	{BeforeRev2, Selected, Remaining, OIAB2} =
		case assemble_artists(IncAfterArtists, NReq * 3, []) of
		{S1aR, R1, M} when M =< 0 ->
			{BeforeRev1, lists:reverse(S1aR), R1, OIABefore};
		{S1bR, [], N} ->
			{B2, RR, ReqRem2} = assemble_artists(
							BeforeRev1, N, []),
			{RR, B2 ++ lists:reverse(S1bR), [],
							OIABefore + N - ReqRem2}
		end,
	NewCnt = query_list_artists_songs(Selected, Ctx),
	HaveCnt = length(NewCnt),
	update_total(List#dbscroll{
		cnt     = NewCnt,
		coffset = max(0, min(HaveCnt - NReq * 3,     OIAB2)),
		csel    = max(0, min(HaveCnt - NReq * 2 + 1, OIAB2)),
		qoffset = sum_artists(BeforeRev2),
		user_data = {Artists, BeforeRev2, Remaining}
	});
list_replace(Ctx, List=#dbscroll{type=queue, coffset=COffset, csel=CSel,
				qoffset=QOffset0, last_query_len=NReq}) ->
	AOffset = max(0, CSel + QOffset0),
	{QOffset1, NumQuery} = case AOffset < 2 * NReq of
		true  -> {0,                  AOffset + 3 * NReq};
		false -> {AOffset - 2 * NReq, 5 * NReq}
	end,
	% reset total to -1 to force update query number of items from DB
	NewQ = query_queue(Ctx, NumQuery,
				List#dbscroll{qoffset=QOffset1, total=-1}),
	% selected item must be within the query result range
	% (0 .. length(cnt) - 1) and is preferably the old index
	% converted to new qoffset (old absolute offset - new q offst)
	NewSel = max(0, min(length(NewQ#dbscroll.cnt) - 1,
					AOffset - NewQ#dbscroll.qoffset)),
	% we assert that COffset <= NewSel in order for the selected item to
	% be within the visible range, hence NewSel marks the max permitted
	% value and as a result the following part: max(0, min(NewSel(...
	% Assume this limit is not reached then we can either try to display
	% the selected item at the end of the screen (coffset=NewSel-NReq+1)
	% or just at the old position (COffset + QOffset0 - QOffset1)
	NewQ#dbscroll{coffset = max(0, min(NewSel, max(
				NewSel - NReq + 1,
				COffset + QOffset0 - NewQ#dbscroll.qoffset))),
		      csel = NewSel}.

query_queue(Ctx, NumQuery, List) ->
	InstancesToQuery = lists:filtermap(fun({Name, _Idx}) ->
			case Name /= Ctx#mpl.mpd_active andalso
					call_singleplayer(Name, is_online) of
			true  -> {true, Name};
			false -> false
			end
		end, Ctx#mpl.mpd_list),
	% delete content to avoid large transfer
	Prelim = call_singleplayer(Ctx#mpl.mpd_active, {query_queue,
					NumQuery, List#dbscroll{cnt=[]}}),
	Prelim#dbscroll{cnt = lists:foldl(fun complete_song_info_other/2,
					Prelim#dbscroll.cnt, InstancesToQuery)}.

get_active_players(Ctx) ->
	lists:filtermap(fun({Name, _Idx}) ->
		case Name =:= Ctx#mpl.mpd_active orelse
					call_singleplayer(Name, is_online) of
		true  -> {true, Name};
		false -> false
		end
	end, Ctx#mpl.mpd_list).

query_all_artists(Ctx) ->
	merge_artists([call_singleplayer(Name, {query_artists_count,
		Ctx#mpl.current_filter}) || Name <- get_active_players(Ctx)]).

merge_artists(ArtistsRaw) ->
	merge_by_criterion(ArtistsRaw,
			fun(Propl) -> proplists:get_value('Artist', Propl) end,
			0,
			fun(Propl) -> proplists:get_value(songs, Propl) end,
			fun(Sel, Vals) -> {Sel, lists:max(Vals)} end).

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

count_while(_Artist, [], Acc) -> Acc;
count_while(Artist, [H|_T], Acc) when element(1, H#dbsong.key) =/= Artist -> Acc;
count_while(Artist, [_H|T], Acc) -> count_while(Artist, T, Acc + 1).

% Split list by predicate and return a tuple with exactly two lists
% First list contains all items before the predicate matches (excl) in reverse
% oder! Second list contains all items after the predicate matches (incl)
divide_list_by_pred_r1(Predicate, List) ->
	divide_list_by_pred_r1(Predicate, List, {[], []}).
divide_list_by_pred_r1(_Predicate, [], {PreAcc, PostAcc}) ->
	{PreAcc, lists:reverse(PostAcc)};
divide_list_by_pred_r1(Predicate, [H|T], {PreAcc, PostAcc}) ->
	case PostAcc =/= [] orelse Predicate(H) of
	true  -> divide_list_by_pred_r1(Predicate, T, {PreAcc, [H|PostAcc]});
	false -> divide_list_by_pred_r1(Predicate, T, {[H|PreAcc], PostAcc})
	end.

% Result:
% {Reversed list of selected artists,
%  Remaining list of artists in natural order,
%  Number of Elements pending to fulfil requested number of items
%  (can be negative)}
assemble_artists(Artists, NReq, Acc) when NReq =< 0 orelse Artists =:= [] ->
	{Acc, Artists, NReq};
assemble_artists([Artist={_Name, Songs}|Others], NReq, Acc) ->
	assemble_artists(Others, NReq - Songs, [Artist|Acc]).

query_list_artists_songs(QList, Ctx) ->
	generate_album_dummies(merge_songs(lists:append(
				[call_singleplayer(Name, {query_artists,
					[AName || {AName, _Count} <- QList],
					Ctx#mpl.current_filter})
				|| Name <- get_active_players(Ctx)])),
				{<<>>, <<>>}).

merge_songs(SongsRaw) ->
	merge_by_criterion(SongsRaw,
		fun(Song) -> Song#dbsong.key end,
		skip,
		fun(H) -> H end,
		fun(_Key, Vals) ->
			[VH|VT] = lists:filter(fun(X) -> X =/= skip end, Vals),
			lists:foldl(fun merge_song_info/2, VH, VT)
		end).

% TODO x might be slow due to missing tail recursion...
generate_album_dummies([], _CAA) -> [];
generate_album_dummies([H|T], {CAR, CAL}) ->
	{SAR, SAL, _SAT} = H#dbsong.key,
	case CAR =:= SAR andalso CAL =:= SAL of
	true  -> [H|generate_album_dummies(T, {CAR, CAL})];
	false -> [H#dbsong{
			% Not really possible to do anything with the URLs here
			% without knowing what kind of file structure the user
			% has established.
			key       = {SAR, SAL, album},
			playcount = -1,
			rating    = -1,
			duration  = 1,
			trackno   = 0
		 }|[H|generate_album_dummies(T, {SAR, SAL})]]
	end.

update_total(List=#dbscroll{type=list, cnt=Cnt,
				user_data={_Artists, BeforeRev, After}}) ->
	List#dbscroll{total = length(Cnt) + sum_artists(BeforeRev) +
							sum_artists(After)}.

sum_artists(Artists) ->
	lists:foldl(fun({_Artist, Songs}, Acc) -> Songs + Acc end, 0, Artists).

query_playcount(Ctx, List) when Ctx#mpl.maloja =:= {none, none} orelse
			length(List#dbscroll.cnt) < 0 orelse
			length(List#dbscroll.cnt) < List#dbscroll.csel ->
	List;
query_playcount(Ctx, List=#dbscroll{cnt=Cnt, csel=CSel}) ->
	Prefix = lists:sublist(Cnt, 1, CSel),
	[Sel|Suffix] = lists:nthtail(CSel, Cnt),
	case Sel#dbsong.playcount < 0 andalso
				Sel#dbsong.key =/= {<<>>, <<>>, <<>>} andalso
				% For now don't query album playcounts...
				element(3, Sel#dbsong.key) =/= album of
	true ->
		case maenmpc_maloja:query_playcount(element(1, Sel#dbsong.key),
				element(3, Sel#dbsong.key), Ctx#mpl.maloja) of
		{ok, Count} ->
			List#dbscroll{cnt=Prefix ++ [
					Sel#dbsong{playcount=Count}|Suffix]};
		{error, EDsc} ->
			gen_server:cast(Ctx#mpl.ui, {db_error, {maloja, EDsc}}),
			List
		end;
	false ->
		List
	end.

list_prepend(Ctx, List=#dbscroll{type=list, cnt=Cnt, coffset=COffset, csel=CSel,
			user_data={Artists, BeforeRev, After}}, NumRequested) ->
	{NewArtistsOrd, BeforeRemRev, _ReqRem} =
			assemble_artists(BeforeRev, NumRequested * 2, []),
	Prepend   = query_list_artists_songs(NewArtistsOrd, Ctx),
	NewOffset = sum_artists(BeforeRemRev),
	NewCnt    = Prepend ++ Cnt,
	NumPrep   = length(Prepend),
	List#dbscroll{
		cnt       = NewCnt,
		coffset   = COffset + NumPrep,
		csel      = CSel    + NumPrep,
		total     = NewOffset + length(NewCnt) + sum_artists(After),
		qoffset   = NewOffset,
		user_data = {Artists, BeforeRemRev, After}
	};
list_prepend(Ctx, List=#dbscroll{type=queue, qoffset=QOffset0}, NumRequested) ->
	NewL = query_queue(Ctx, NumRequested, List#dbscroll{
					qoffset=QOffset0 - NumRequested}),
	NewL#dbscroll{cnt = NewL#dbscroll.cnt ++
					Ctx#mpl.current_queue#dbscroll.cnt}.

list_append(Ctx, List=#dbscroll{type=list, cnt=Cnt,
			user_data={Artists, BeforeRev, After}}, NumRequested) ->
	{NewArtistsRev, AfterRem, _ReqRem} = assemble_artists(After,
							NumRequested * 2, []),
	update_total(List#dbscroll{
		cnt       = Cnt ++ query_list_artists_songs(
					lists:reverse(NewArtistsRev), Ctx),
		user_data = {Artists, BeforeRev, AfterRem}
	});
list_append(Ctx, List=#dbscroll{type=queue, cnt=Cnt, qoffset=QOffset0},
								NumRequested) ->
	QOffset2 = QOffset0 + length(Cnt),
	NewQ = query_queue(Ctx, NumRequested, List#dbscroll{qoffset=QOffset2}),
	NewQ#dbscroll{cnt=Cnt ++ NewQ#dbscroll.cnt, qoffset=QOffset0}.

ui_scroll(Ctx, top, List) ->
	proc_range_result(Ctx, case List#dbscroll.qoffset =:= 0 of
		true  -> {in_range, ok};
		false -> out_of_range
	end, List#dbscroll{coffset=0, csel=0, qoffset=0});
ui_scroll(Ctx, bottom, List=#dbscroll{type=list, last_query_len=ItemsRequested,
				user_data={_A, _BR2, Remaining}})
				when length(Remaining) > 0 ->
	Artists = query_all_artists(Ctx),
	{ASel, ABefRev, _NRem} = assemble_artists(lists:reverse(Artists),
							ItemsRequested * 3, []),
	NewCnt = query_list_artists_songs(ASel, Ctx),
	HaveCnt = length(NewCnt),
	QOffset = sum_artists(ABefRev),
	NewList = query_playcount(Ctx, List#dbscroll{
		cnt       = NewCnt,
		coffset   = max(0, HaveCnt - ItemsRequested),
		csel      = max(0, HaveCnt - 1),
		qoffset   = QOffset,
		total     = QOffset + HaveCnt,
		user_data = {Artists, ABefRev, []}
	}),
	transform_and_send_to_ui(Ctx, NewList),
	Ctx#mpl{current_list=NewList};
ui_scroll(Ctx, bottom, List=#dbscroll{qoffset=QOffset, total=Total}) ->
	ui_scroll(Ctx, Total - QOffset, List);
ui_scroll(Ctx, Offset, List=#dbscroll{coffset=COffset, qoffset=QOffset,
		csel=CSel, total=Total, last_query_len=ItemsRequested}) ->
	NewCSel = min(Total - QOffset - 1, CSel + Offset),
	NewCOffset = case NewCSel < COffset orelse
					NewCSel >= COffset + ItemsRequested of
		true -> min(Total - QOffset - ItemsRequested, COffset + Offset);
		false -> COffset
		end,
	error_logger:info_msg("ui scroll coffset=~w csel=~w -> coffset=~w csel=~w", [COffset, CSel, NewCOffset, NewCSel]),
	ui_query(Ctx, List#dbscroll{csel=NewCSel, coffset=NewCOffset}).

handle_info(interrupt_idle, Ctx) ->
	call_singleplayer(Ctx#mpl.mpd_active, request_update),
	{noreply, Ctx};
handle_info(_Message, Ctx) ->
	{noreply, Ctx}.

code_change(_OldVersion, Ctx, _Extra) -> {ok, Ctx}.
