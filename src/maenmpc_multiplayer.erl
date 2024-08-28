-module(maenmpc_multiplayer).
-behavior(gen_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, code_change/3]).
-include_lib("maenmpc_db.hrl").
-define(ARTIST_QUERY_CHUNK_SIZE, 20).

% TODO x recommended refactor: split into two up to three components: One to purely track playback state and service states (useful also in CLI scenario), one for scrolling and optionally one for the rest?

-record(mpl, {
	ui, radio, scrobble, alsa, mpd_list, mpd_active, mpd_ratings,
	maloja, outputs, services,
	search_page, search_term,
	current_song, current_queue, current_list, current_radio,
	current_filter, current_frating
}).

init([NotifyToUI, NotifyToRadio, NotifyToScrobble]) ->
	{ok, PrimaryRatings} = application:get_env(maenmpc, primary_ratings),
	{ok, MPDList}        = application:get_env(maenmpc, mpd),
	{ok, ALSAHWInfo}     = application:get_env(maenmpc, alsa),
	{ok, Maloja}         = application:get_env(maenmpc, maloja),
	[{MPDFirst, _ConnInfo}|_Others] = MPDList,
	timer:send_interval(5000, interrupt_idle),
	MPDListIdx = [{Name, Idx} || {{Name, _ConnInfoI}, Idx} <-
			lists:zip(MPDList, lists:seq(1, length(MPDList)))],
	gen_server:cast(NotifyToUI, {db_cidx,
				proplists:get_value(MPDFirst, MPDListIdx)}),
	% TODO x maybe query is a ctive? Then the scrobble service could also print some logs...
	ScrobbleActive = proplists:get_value(scrobble_send, Maloja, false),
	InitialSVC = [ #dbservice{key=radio, is_online=false,
				label="Radio", node=maenmpc_radio},
			#dbservice{key=podcast, is_online=false,
				label="Podcast", node=maenmpc_podcast},
			#dbservice{key=scrobble, is_online=ScrobbleActive,
				label="Scrobble", node=maenmpc_scrobble} ],
	gen_server:cast(NotifyToUI, {db_services, InitialSVC}),
	{ok, reset_views(#mpl{
		ui              = NotifyToUI,
		radio           = NotifyToRadio,
		scrobble        = NotifyToScrobble,
		alsa            = ALSAHWInfo,
		mpd_list        = MPDListIdx, % [{name, idx}]
		mpd_active      = MPDFirst,
		mpd_ratings     = PrimaryRatings,
		maloja          = maenmpc_maloja:conn(Maloja),
		outputs         = none,
		services        = InitialSVC,
		search_term     = "",
		current_song    = maenmpc_erlmpd:epsilon_song(length(MPDList)),
		current_filter  = filter_reqne_const(),
		current_frating = {any, any} % filter for ratings
	})}.

reset_views(Ctx) ->
	reset_list(Ctx#mpl{
		current_song   = maenmpc_erlmpd:epsilon_song(length(
							Ctx#mpl.mpd_list)),
		current_queue  = #dbscroll{type=queue, cnt=[], coffset=0,
					csel=0, total=-1, qoffset=0,
					last_query_len=0, user_data=none},
		current_radio  = #dbscroll{type=radio, cnt=[], coffset=0,
					csel=0, total=0, last_query_len=0,
					qoffset=0, user_data=-1}}).

reset_list(Ctx) ->
	Ctx#mpl{current_list = #dbscroll{type=list, cnt=[], coffset=0,
					csel=0, total=-1, last_query_len=0,
					qoffset=0, user_data={[], [], []}}}.

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
		gen_server:cast(Ctx#mpl.scrobble, {db_playing, SendToUI}),
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
handle_cast({ui_query, output}, Ctx) ->
	{noreply, ui_query_outputs(Ctx)};
handle_cast({ui_query, search_limits}, Ctx) ->
	{noreply, ui_query_search_limits(Ctx)};
handle_cast({ui_query, Action, ItemsRequested}, Ctx=#mpl{services=ServicesNow,
								ui=UI}) ->
	case Action of
	radio -> ok = gen_server:cast(UI, {db_services, ServicesNow});
	_Any  -> ok
	end,
	{noreply, ui_query(Ctx,
			ui_items_requested(Ctx, Action, ItemsRequested))};
handle_cast({ui_scroll, Action, Offset, ItemsRequested}, Ctx) ->
	{noreply, ui_scroll(Ctx, Offset,
			ui_items_requested(Ctx, Action, ItemsRequested))};
% We really only route it through here as the multiplayer knows about the
% active MPD which is an important information for radio operation. Also,
% we keep track of the state (on/off)
handle_cast({ui_service, status}, Ctx=#mpl{services=ServicesNow,ui=UI}) ->
	ok = gen_server:cast(UI, {db_services, ServicesNow}),
	{noreply, Ctx};
handle_cast({ui_service, start, Target}, Ctx) ->
	{noreply, edit_service(fun(Service=#dbservice{node=Node}, CtxI) ->
		ok = gen_server:cast(Node, {service_start,
							CtxI#mpl.mpd_active}),
		{Service#dbservice{is_online=true}, CtxI}
	end, Target, Ctx)};
handle_cast({ui_service, stop, Target}, Ctx) ->
	{noreply, edit_service(fun(Service=#dbservice{node=Node}, CtxI) ->
		ok = gen_server:cast(Node, service_stop),
		{Service#dbservice{is_online=false}, CtxI}
	end, Target, Ctx)};
handle_cast({ui_service, toggle, Target}, Ctx) ->
	{noreply, edit_service(fun(Service=#dbservice{is_online=SO, node=Node},
									CtxI) ->
		ok = gen_server:cast(Node, case SO of
				true  -> service_stop;
				false -> {service_start, CtxI#mpl.mpd_active}
				end),
		{Service#dbservice{is_online=not SO}, CtxI}
	end, Target, Ctx)};
handle_cast(ui_radio_stop, Ctx) ->
	ok = gen_server:cast(maenmpc_radio, service_stop),
	{noreply, Ctx};
handle_cast({ui_selected, Screen, Action}, Ctx) ->
	{noreply, ui_selected_action(Screen, Action, Ctx)};
handle_cast({ui_horizontal, Page, Delta}, Ctx) ->
	{noreply, ui_horizontal_nav(Page, Delta, Ctx)};
handle_cast({mpd_assign_error, Name, Reason}, Ctx) ->
	gen_server:cast(Ctx#mpl.ui, {db_error, {offline, Name, Reason}}),
	{noreply, Ctx};
handle_cast({radio_enqueue, DBSong}, Ctx=#mpl{current_radio=Radio}) ->
	call_singleplayer(Ctx#mpl.mpd_active, {enqueue_end, [DBSong]}),
	NewRadio = Radio#dbscroll{user_data=DBSong#dbsong.playlist_id},
	transform_and_send_to_ui(Ctx, NewRadio, NewRadio#dbscroll.user_data),
	{noreply, Ctx#mpl{current_radio=NewRadio}};
handle_cast(Log=#dblog{}, Ctx=#mpl{current_radio=Radio}) ->
	NewID    = Radio#dbscroll.total + 1,
	NewRadio = Radio#dbscroll{total=NewID, cnt=Radio#dbscroll.cnt ++
							[Log#dblog{id=NewID}]},
	transform_and_send_to_ui(Ctx, NewRadio, NewRadio#dbscroll.user_data),
	{noreply, Ctx#mpl{current_radio=NewRadio}};
handle_cast({ui_search, Direction, Page, String}, Ctx) ->
	{noreply, search(Direction, Ctx#mpl{search_page=Page,
					search_term=string:lowercase(String)})};
% TODO x search: Backward search from end of list with an item that is maybe off the scren causes something to be selected outside of the visible area (if at all).
handle_cast({ui_search_continue, Direction, Page}, Ctx) ->
	{noreply, search(Direction, Ctx#mpl{search_page=Page})};
handle_cast({ui_set_filter, FilterValues}, Ctx) ->
	{noreply, set_filter(FilterValues, Ctx)};
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
		true  -> [H|_T] = complete_song_info_other(Name, [SongInfo]), H;
		false -> SongInfo
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
	List = list_for_page(Ctx, Action),
	List#dbscroll{last_query_len=ItemsRequested}.

list_for_page(Ctx, queue)  -> Ctx#mpl.current_queue;
list_for_page(Ctx, list)   -> Ctx#mpl.current_list;
list_for_page(Ctx, radio)  -> Ctx#mpl.current_radio.

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

proc_range_result(Ctx, _AnyRangeResult, List=#dbscroll{type=radio, cnt=Cnt,
					last_query_len=NReq, user_data=Idx,
					csel=CSel, coffset=COffset}) ->
	NewSel = max(0, min(CSel, length(Cnt) - 1)),
	ListSafe = List#dbscroll{csel = NewSel,
			coffset = max(0, max(NewSel - NReq + 1, COffset))},
	transform_and_send_to_ui(Ctx, ListSafe, Idx),
	Ctx#mpl{current_radio=ListSafe};
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
		Ctx#mpl.current_filter,
		Ctx#mpl.current_frating}) || Name <- get_active_players(Ctx)]).

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
					Ctx#mpl.current_filter,
					Ctx#mpl.current_frating})
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
			length(List#dbscroll.cnt) < List#dbscroll.csel orelse
			length(List#dbscroll.cnt) == 0 ->
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
list_prepend(Ctx, List=#dbscroll{type=queue, coffset=COffset0, csel=CSel0,
					qoffset=QOffset0}, NumRequested) ->
	NewL = query_queue(Ctx, NumRequested, List#dbscroll{
					qoffset=QOffset0 - NumRequested}),
	NumPrep = length(NewL#dbscroll.cnt),
	NewL#dbscroll{
		%qoffset = QOffset0 - NumPrep,
		cnt = NewL#dbscroll.cnt ++ Ctx#mpl.current_queue#dbscroll.cnt,
		coffset = COffset0 + NumPrep,
		csel = CSel0 + NumPrep
	}.

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

ui_query_outputs(Ctx) ->
	OutputsRaw = lists:foldl(
		fun({Name, _Idx}, DBOI) ->
			case call_singleplayer(Name, is_online) of
			true  -> merge_outputs(DBOI,
					call_singleplayer(Name, query_output),
					Name =:= Ctx#mpl.mpd_active);
			false -> DBOI
			end
		end,
		#dboutputs{outputs=[], partitions=[], active_set=sets:new(),
							assigned={none, none}},
		Ctx#mpl.mpd_list
	),
	% TODO z Optionally return cursor from before if valid...
	Outputs = OutputsRaw#dboutputs{cursor=cursor_to_beginning(OutputsRaw)},
	gen_server:cast(Ctx#mpl.ui, {db_outputs, Outputs}),
	Ctx#mpl{outputs=Outputs}.

cursor_to_beginning(#dboutputs{partitions=[[PF|_PIT]|_PT], outputs=[OF|_OT]}) ->
	{1, PF, OF#dboutput.output_id}.

merge_outputs(DBOI = #dboutputs{outputs=IO, partitions=IP, active_set=AI,
						assigned=ASI},
			#dboutputs{outputs=SO, partitions=SP, active_set=AS,
						assigned=ASGN}, IsActive) ->
	DBOI#dboutputs{
		outputs    = IO ++ SO,
		partitions = IP ++ [SP],
		active_set = sets:union(AI, AS),
		assigned   = case IsActive of true -> ASGN; false -> ASI end
	}.

ui_query_search_limits(Ctx) ->
	gen_server:cast(Ctx#mpl.ui, {db_search_limits, lists:foldl(
		fun({Name, _Idx}, Lim) ->
			case call_singleplayer(Name, is_online) of
			true  -> merge_searchlim(Lim, call_singleplayer(Name,
							query_search_limits));
			false -> Lim
			end
		end,
		#dbsearchlim{year={9999, 0}, rating={100, 0}},
		Ctx#mpl.mpd_list)}),
	Ctx.

merge_searchlim(#dbsearchlim{year=YA, rating=RA},
		#dbsearchlim{year=YB, rating=RB}) ->
	#dbsearchlim{year=merge_limits(YA, YB), rating=merge_limits(RA, RB)}.

merge_limits({MinA, MaxA}, {MinB, MaxB}) ->
	{min(MinA, MinB), max(MaxA, MaxB)}.

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
ui_scroll(Ctx, bottom, List=#dbscroll{qoffset=QOffset, total=Total,
								csel=CSel}) ->
	ui_scroll(Ctx, Total - QOffset - CSel, List);
ui_scroll(Ctx, Offset, List=#dbscroll{coffset=COffset, qoffset=QOffset,
		csel=CSel, total=Total, last_query_len=ItemsRequested}) ->
	NewCSel = min(Total - QOffset - 1, max(- QOffset, CSel + Offset)),
	NewCOffset = case NewCSel < COffset orelse
					NewCSel >= COffset + ItemsRequested of
		true -> min(Total - QOffset - ItemsRequested, COffset + Offset);
		false -> COffset
		end,
	ui_query(Ctx, List#dbscroll{csel=NewCSel, coffset=NewCOffset}).

edit_service(Callback, Service, Ctx0) ->
	ServicesOld = Ctx0#mpl.services,
	OldRec = lists:keyfind(Service, #dbservice.key, ServicesOld),
	{NewRec, Ctx1} = Callback(OldRec, Ctx0),
	ServicesNew = lists:keyreplace(Service, #dbservice.key, ServicesOld,
									NewRec),
	Ctx2 = Ctx1#mpl{services=ServicesNew},
	ok = gen_server:cast(Ctx2#mpl.ui, {db_services, ServicesNew}),
	Ctx2.

ui_selected_action(output, play, Ctx) when Ctx#mpl.outputs =/= none ->
	CtxPre = apply_player_change(Ctx),
	Cursor = CtxPre#mpl.outputs#dboutputs.cursor,
	[OutputObj] = lists:filter(fun(#dboutput{player_idx=PIDX,
				partition_name=PName, output_id=POID}) ->
			PIDX  =:= element(1, Cursor) andalso
			PName =:= element(2, Cursor) andalso
			POID  =:= element(3, Cursor)
		end, CtxPre#mpl.outputs#dboutputs.outputs),
	ok = call_singleplayer(CtxPre#mpl.mpd_active, {set_output, OutputObj}),
	ui_query_outputs(CtxPre#mpl{outputs=Ctx#mpl.outputs#dboutputs{
							active_set=Cursor}});
ui_selected_action(Page, _AnyAction, Ctx)
				when Page =/= queue andalso Page =/= list ->
	% no operation when not on a music playback page...
	Ctx;
ui_selected_action(Page, Action, Ctx) ->
	List = list_for_page(Ctx, Page),
	Cnt  = List#dbscroll.cnt,
	CSel = List#dbscroll.csel,
	case CSel < 0 orelse CSel >= length(Cnt) of
	true ->
		% cancel if out of bounds / not exists
		Ctx;
	false ->
		Item = lists:nth(CSel + 1, Cnt),
		{Artist, Album, Title} = Item#dbsong.key,
		UseItems = case Page =:= list andalso Title =:= album of
			true -> lists:filter(fun(#dbsong{key={AArtist,
							AAlbum, ATitle}}) ->
					AArtist =:= Artist andalso
					AAlbum  =:= Album  andalso
					ATitle  =/= album
				end, Cnt);
			false -> [Item]
			end,
		case Action of
		play when Page =:= queue ->
			ok = call_singleplayer(Ctx#mpl.mpd_active,
						{play_from_playlist, UseItems}),
			Ctx;
		play ->
			ok = call_singleplayer(Ctx#mpl.mpd_active,
						{play, UseItems}),
			Ctx;
		enqueue_end ->
			ok = call_singleplayer(Ctx#mpl.mpd_active,
						{enqueue_end, UseItems}),
			Ctx;
		enqueue_current ->
			call_singleplayer(Ctx#mpl.mpd_active,
						{enqueue_current, UseItems}),
			Ctx;
		queue_delete when Page =:= queue andalso
						length(UseItems) =:= 1 ->
			ok = call_singleplayer(Ctx#mpl.mpd_active,
						{queue_delete, UseItems}),
			NewLen = length(Cnt) - CSel - 1 - length(UseItems),
			QueueNew = List#dbscroll{cnt=lists:sublist(Cnt, CSel) ++
					case NewLen > 0 of
					true -> lists:sublist(Cnt, CSel + 1 +
						length(UseItems), NewLen);
					false -> []
					end, total=List#dbscroll.total - 1},
			% update UI and limits
			ui_scroll(Ctx#mpl{current_queue=QueueNew}, 0, QueueNew);
		rating_up when length(UseItems) =:= 1 ->
			edit_rating(Ctx, Page, Cnt, UseItems, +1);
		rating_down when length(UseItems) =:= 1 ->
			edit_rating(Ctx, Page, Cnt, UseItems, -1);
		_Other ->
			error_logger:info_msg("-> ignored ~w", [Action]), % TODO FOR DEBUG
			% ignore requests in wrong state etc.
			Ctx
		end
	end.

edit_rating(Ctx, Page, Cnt, [UIF|_UIT], Delta) ->
	NewR   = call_singleplayer(Ctx#mpl.mpd_ratings, {rating, Delta, UIF}),
	NewCnt = lists:keyreplace(UIF#dbsong.key, #dbsong.key, Cnt,
						UIF#dbsong{rating=NewR}),
	case Page of
        queue ->
		NewQueue = Ctx#mpl.current_queue#dbscroll{cnt=NewCnt},
		transform_and_send_to_ui(Ctx, NewQueue),
		Ctx#mpl{current_queue=NewQueue};
        list ->
		NewList = Ctx#mpl.current_list#dbscroll{cnt=NewCnt},
		transform_and_send_to_ui(Ctx, NewList),
		Ctx#mpl{current_list=NewList};
	_Any ->
		Ctx
	end.

% incremental routine. if output did not change nothing to do at this stage yet.
apply_player_change(Ctx = #mpl{outputs=OutputsIn}) when element(1,
				OutputsIn#dboutputs.cursor) =:=
				element(1, OutputsIn#dboutputs.active_set) ->
	Ctx;
apply_player_change(Ctx = #mpl{mpd_list=MPDList, outputs=OutputsIn}) ->
	NewIdx = element(1, OutputsIn#dboutputs.cursor),
	% OK we change output for real!
	ok = gen_server:cast(Ctx#mpl.ui, {db_cidx, NewIdx}),
	{Name, _Idx} = lists:nth(NewIdx, MPDList),
	reset_views(Ctx#mpl{mpd_active = Name}).

ui_horizontal_nav(output, Delta, Ctx = #mpl{outputs=OutputCTX}) ->
	{Player, Part, OutputID} = OutputCTX#dboutputs.cursor,
	{PBeforeRev, PAfter} = divide_list_by_pred_r1(fun(X) -> X =:= Part end,
			lists:nth(Player, OutputCTX#dboutputs.partitions)),
	NewOutputs = OutputCTX#dboutputs{cursor=if
	Delta > 0 andalso length(PAfter) > Delta ->
		{Player, lists:nth(Delta + 1, PAfter), OutputID};
	Delta < 0 andalso length(PBeforeRev) >= -Delta ->
		{Player, lists:nth(-Delta, PBeforeRev), OutputID};
	true ->
		{ABeforeRev, AAfter} = divide_list_by_pred_r1(fun(#dboutput{
						player_idx = SPlayer,
						partition_name = SPartition,
						output_id = SOutputID}) ->
				SPlayer =:= Player andalso SPartition =:= Part
						andalso SOutputID =:= OutputID
			end, OutputCTX#dboutputs.outputs),
		if
		Delta > 0 andalso length(AAfter) > 1 ->
			ASel = lists:nth(2, AAfter),
			{ASel#dboutput.player_idx, ASel#dboutput.partition_name,
						ASel#dboutput.output_id};
		Delta > 0 andalso length(AAfter) =< 1 ->
			% Wrap round semantics
			cursor_to_beginning(OutputCTX);
		Delta < 0 andalso length(ABeforeRev) >= 1 ->
			ASel = lists:nth(1, ABeforeRev),
			{ASel#dboutput.player_idx, ASel#dboutput.partition_name,
						ASel#dboutput.output_id};
		true ->
			% give up, nothing found to match
			OutputCTX#dboutputs.cursor
		end
	end},
	gen_server:cast(Ctx#mpl.ui, {db_outputs, NewOutputs}),
	Ctx#mpl{outputs=NewOutputs};
% ignore the horizontal navigation keys on unsupported pages...
ui_horizontal_nav(_Other, _Delta, Ctx) ->
	Ctx.

search(Direction, Ctx=#mpl{search_page=list, search_term=Query, current_list=
					#dbscroll{cnt=Cnt, csel=CSel}}) ->
	case CSel < 0 orelse CSel >= length(Cnt) of
	true ->
		% No start key
		search_list_outside(Direction, Ctx);
	false ->
		Item = lists:nth(CSel + 1, Cnt),
		StartKey = Item#dbsong.key,
		{LBeforeExclRev, [_Skip|LAfterExcl]} = divide_list_by_pred_r1(
				fun(#dbsong{key=K}) -> K =:= StartKey end,
				Cnt),
		case
			case Direction of
			 1 -> search_cnt(Direction, LAfterExcl, Query,
								CSel + 1);
			-1 -> search_cnt(Direction, LBeforeExclRev, Query,
								CSel - 1)
			end
		of
		false ->
			case search_list_outside(Direction, Ctx) of
			false ->
				case
					case Direction of
					 1 -> search_cnt(Direction,
						lists:reverse(LBeforeExclRev),
						Query, 0);
					-1 -> search_cnt(Direction,
						lists:reverse(LAfterExcl),
						Query, length(Cnt) - 1)
					end
				of
				false ->
					gen_server:cast(Ctx#mpl.ui, {db_info,
							"no more results"}),
					Ctx;
				ThirdResult ->
					search_cnt_scroll_to(ThirdResult, Ctx)
				end;
			SecondResult ->
				search_list_goto_key(SecondResult, Ctx)
			end;
		FirstResult ->
			search_cnt_scroll_to(FirstResult, Ctx)
		end
	end;
search(_Direction, Ctx) ->
	Ctx.

search_cnt(_Direction, [], _Query, _Ctr) -> false;
search_cnt(Direction, [Item|CRem], Query, Ctr) ->
	case item_matches_query(Item, Query) of
	true  -> Ctr;
	false -> search_cnt(Direction, CRem, Query, Ctr + Direction)
	end.

item_matches_query(#dbsong{key={Artist, Album, Title}}, Query) ->
	string:find(string:lowercase(Artist), Query) =/= nomatch orelse
	case Title of
	album  -> string:find(string:lowercase(Album), Query) =/= nomatch;
	_Title -> string:find(string:lowercase(Title), Query) =/= nomatch
	end.

% TODO x search: With sufficient query length may attain better performance by handing the query in its entirety over to the database and only from the results determine what is “after” and “before” current cursor in search direction. However, this would incur jet another path of the implementation to be tested. Remain with the current (slower) implementation for now.
search_list_outside(Direction, Ctx=#mpl{current_filter=Filter,
				current_frating=FRating, search_term=Query,
				current_list=#dbscroll{
				user_data={_Artists, ABeforeRev, AAfter}}}) ->
	ActivePlayers = get_active_players(Ctx),
	List1 = case Direction of
		 1 -> AAfter;
		-1 -> ABeforeRev
		end,
	case search_next_outside(Direction, List1, Query, Filter, FRating,
							ActivePlayers) of
	false ->
		List2 = lists:reverse(case Direction of
			1  -> ABeforeRev;
			-1 -> AAfter
			end),
		search_next_outside(Direction, List2, Query, Filter, FRating,
							ActivePlayers);
	SecondResultA ->
		SecondResultA
	end.

search_next_outside(_Direction, [], _Query, _Filter, _FR, _ActivePlayers) ->
	false;
% Limit number of artists to query at once (avoid sending overly large queries
% to MPD)
search_next_outside(Direction, Artists, Query, Filter, FRating, ActivePlayers)
			when length(Artists) > ?ARTIST_QUERY_CHUNK_SIZE ->
	case search_next_outside(Direction, lists:sublist(Artists,
				?ARTIST_QUERY_CHUNK_SIZE), Query,
				Filter, FRating, ActivePlayers) of
	false  -> search_next_outside(Direction, lists:sublist(Artists,
				?ARTIST_QUERY_CHUNK_SIZE, length(Artists)),
				Query, Filter, FRating, ActivePlayers);
	Result -> Result
	end;
search_next_outside(Direction, Artists, Query, Filter, FRating,
							ActivePlayers) ->
	Results = [call_singleplayer(Name, {search_by_artists, Direction,
				[Artist || {Artist, _Songs} <- Artists], Query,
				Filter, FRating}) || Name <- ActivePlayers],
	case [Value || Value <- Results, Value =/= false] of
	[] ->
		false;
	FRS ->
		% TODO x NOT EXACTLY CORRECT BECAUSE WE SHOULD COMPARE DISC NUMBER BEFORE TRACK NUMBER (NEED TO GET IT FIRST...)
		case lists:sort(fun(
			#dbsong{key={AArtist, AAlbum, ASong}, trackno=ATN},
			#dbsong{key={BArtist, BAlbum, BSong}, trackno=BTN}) ->
				AArtist =< BArtist orelse
				AAlbum  =< BAlbum  orelse
				ATN     =< BTN     orelse
				ASong   =< BSong
			end, FRS)
		of
		[H|_T] when Direction =:= 1 ->
			H#dbsong.key;
		SRT when Direction =:= -1 ->
			EL = lists:last(SRT),
			EL#dbsong.key
		end
	end.

search_cnt_scroll_to(CntResult, Ctx=#mpl{current_list=List}) ->
	ItemsRequested = List#dbscroll.last_query_len,
	% TODO SEARCH BUG TRY TO SEARCH FOR “YASHA” AND OBSERVE THE SCREEN NOT BEING FILLED ENTIRELY DESPITE RECOVERING AFTE RISSUING ANY SUBSEQUENT SCROLL OPERATION. NEED TO ENSURE THAT THIS HAPPENS AUTOMATICALLY ALREADY / IT WOULD SEEM THIS IS A BUG IN THE SCROLLING IMPLEMENTATION THAT ONLY TRIGGERS IN THIS CASE HERE?
	ui_query(Ctx, List#dbscroll{
		csel    = CntResult,
		coffset = max(0, CntResult - ItemsRequested div 2)
	}).

% similar, but non-equal to ui_scroll/Ctx, bottom, List = #dbscroll/type=list...
search_list_goto_key(Key, Ctx = #mpl{current_list = List}) ->
	ItemsRequested    = List#dbscroll.last_query_len,
	{Artists, _B, _C} = List#dbscroll.user_data,
	{ABeforeExclRev, AAfterIncl} = divide_list_by_pred_r1(
		fun({Name, _Songs}) -> Name =:= element(1, Key) end, Artists),
	{ASelInterleaved, _ABefRevInterleaved, _NRem} = assemble_artists(
		lists_merge_after_before(ABeforeExclRev, AAfterIncl, []),
		ItemsRequested * 3, []),
	case lists:sort(ASelInterleaved) of
	[] ->
		gen_server:cast(Ctx#mpl.ui, {db_error, search_miss_1}),
		Ctx;
	ASelSRT ->
		[{AFirstSel, _Songs1}|_ASelRem] = ASelSRT,
		{ALastSel, _Songs2} = lists:last(ASelSRT),
		{ABeforeExclRev2, AAfterInclInterm} = divide_list_by_pred_r1(
				fun({Name, _Songs}) -> Name =:= AFirstSel end,
				Artists),
		{_ABeforeRev3, [_ADropLastSel|AAfterExcl3]} =
				divide_list_by_pred_r1(fun({Name, _Songs}) ->
				Name =:= ALastSel end, AAfterInclInterm),
		% continue with ASelSRT, Artists, ABeforeExclRev2, AAfterExcl3
		NewCnt  = query_list_artists_songs(ASelSRT, Ctx),
		HaveCnt = length(NewCnt),
		QOffset = sum_artists(ABeforeExclRev2),
		case find_index_for_key(NewCnt, Key, 0) of
		false ->
			gen_server:cast(Ctx#mpl.ui, {db_error, search_miss_2}),
			Ctx;
		CSel ->
			NewList = query_playcount(Ctx, List#dbscroll{
				cnt       = NewCnt,
				coffset   = max(0, CSel - ItemsRequested div 2),
				csel      = CSel,
				qoffset   = QOffset,
				total     = QOffset + HaveCnt +
						sum_artists(AAfterExcl3),
				user_data = {Artists, ABeforeExclRev2,
						AAfterExcl3}
			}),
			transform_and_send_to_ui(Ctx, NewList),
			Ctx#mpl{current_list=NewList}
		end
	end.

lists_merge_after_before([], After, Acc) -> lists:reverse(Acc) ++ After;
lists_merge_after_before(Before, [], Acc) -> lists:reverse(Acc) ++ Before;
lists_merge_after_before([B|Before], [A|After], Acc) ->
			lists_merge_after_before(Before, After, [A|[B|Acc]]).

% find the index of the first matching key in the dbsong list passed as args
% this may become a generically useful function if instead of hard-matching the
% key we pass a callback function.
find_index_for_key([], _Key, _Idx) -> false;
find_index_for_key([H|Tail], Key, Idx) ->
	case H#dbsong.key =:= Key of
	true  -> Idx;
	false -> find_index_for_key(Tail, Key, Idx + 1)
	end.

set_filter(#dbsearchin{artist=Artist, album=Album, title=Title, format=Format,
				ymin=YMin, ymax=YMax, rmin=RMin, rmax=RMax,
				reqne=ReqNE}, Ctx) ->
	FilterIn = filter_string({tagop, artist, contains, Artist},
		   filter_string({tagop, album,  contains, Album},
		   filter_string({tagop, title,  contains, Title},
		   filter_string({audio_format_match, Format},
		   filter_year(YMin, YMax,
		   filter_reqne(ReqNE, [])))))),
	reset_list(Ctx#mpl{
		current_filter = case FilterIn of
			[]    -> {base, ""};   % filter that machtes everything!
			[It]  -> It;           % single item filter is easy
			Multi -> {land, Multi} % encapsulate multi-filter
			end,
		current_frating = {filter_rating(RMin), filter_rating(RMax)}
	}).

filter_string(Op, List0) ->
	case string:length(element(tuple_size(Op), Op)) of
	0     -> List0;
	_Non0 -> [Op|List0]
	end.

filter_reqne("x",  List0) -> [filter_reqne_const()|List0];
filter_reqne(_Any, List0) -> List0.

filter_reqne_const() ->
	{lnot, {land, [
		{tagop, artist, eq, ""},
		{tagop, album,  eq, ""},
		{tagop, title,  eq, ""}
	]}}.

% MPD does not seem to support searching date ranges hence we construct our own
% date range search based on regular expressions. This basically works by
% emulating the counting through regex and declaring that either the digits
% match and the next digit has to be checked or the digit already indicates that
% it is greater/lower the given string (and only the same number of digits may
% follow). Effectively, there is a loose assumption that years will always have
% four digits, but the code could be extended to account for more/less digits
% by taking begin and end of string into account and declaring a certain format
% for the dates that is supported.
filter_year(Min, Max, List0) ->
	List1 = case lists:dropwhile(fun(X) -> X == $0 end, Min) of
		[]  -> List0;
		Num -> [{tagop, date, match, filter_regex_ge_outer(Num)}|List0]
		end,
	case Max of
		[]   -> List1;
		% Need to change this place here if fewer than four digits
		% need to be supported as well...
		_Max -> [{tagop, date, match, filter_regex_le(Max)}|List1]
	end.

filter_regex_ge_outer(Num) ->
	io_lib:format("(~s)|([1-9][0-9]{~w,})",
					[filter_regex_ge(Num), length(Num)]).

filter_regex_ge([$9|[]]) -> "9";
filter_regex_ge([H|[]]) -> io_lib:format("[~c-9]", [H]);
filter_regex_ge([H|T]) ->
	io_lib:format("~s(~c(~s))", [case filter_charclass_gt(H) of
		none -> "";
		Pre  -> io_lib:format("(~s[0-9]{~w})|", [Pre, length(T)])
	end, H, filter_regex_ge(T)]).

filter_charclass_gt($9)   -> none;
filter_charclass_gt($8)   -> "9";
filter_charclass_gt(Char) -> io_lib:format("[~w-9]",
						[list_to_integer([Char]) + 1]).

filter_regex_le([$0|[]]) -> "0";
filter_regex_le([H|[]]) -> io_lib:format("[0-~c]", [H]);
filter_regex_le([H|T]) ->
	io_lib:format("~s(~c(~s))", [case filter_charclass_lt(H) of
		none -> "";
		Pre  -> io_lib:format("(~s[0-9]{~w})|", [Pre, length(T)])
	end, H, filter_regex_le(T)]).

filter_charclass_lt($0)   -> none;
filter_charclass_lt($1)   -> "0";
filter_charclass_lt(Char) -> io_lib:format("[0-~w]",
						[list_to_integer([Char]) - 1]).

% align with maenmpc_erlmpd:format_Rating
filter_rating("") -> any;
filter_rating("*") -> 20;
filter_rating("**") -> 40;
filter_rating("***") -> 60;
filter_rating("****") -> 80;
filter_rating("*****") -> 100;
filter_rating(RatingStr) ->
	case string:to_integer(RatingStr) of
	{Num, []}   -> Num;
	{error, _R} -> any % TODO x optionally generate error
	end.

handle_info(interrupt_idle, Ctx) ->
	call_singleplayer(Ctx#mpl.mpd_active, request_update),
	{noreply, Ctx};
handle_info(_Message, Ctx) ->
	{noreply, Ctx}.

code_change(_OldVersion, Ctx, _Extra) -> {ok, Ctx}.
