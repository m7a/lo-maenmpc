-module(maenmpc_multiplayer).
-behavior(gen_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, code_change/3]).
-include_lib("maenmpc_db.hrl").

-record(mpl, {
	ui, alsa, mpd_list, mpd_active, %mpd_ratings, % TODO UNUSED MAY NEED IT TO EDIT RATING!
	current_song, current_queue
}).

init([NotifyToUI]) ->
	%{ok, PrimaryRatings} = application:get_env(maenmpc, primary_ratings),
	{ok, MPDList}        = application:get_env(maenmpc, mpd),
	{ok, ALSAHWInfo}     = application:get_env(maenmpc, alsa),
	MPDFirst = m16, % TODO DEBUG ONLY
	%[MPDFirst|_Others] = MPDList,
	timer:send_interval(5000, interrupt_idle),
	MPDListIdx = [{Name, Idx} || {{Name, _ConnInfo}, Idx} <-
			lists:zip(MPDList, lists:seq(1, length(MPDList)))],
	gen_server:cast(NotifyToUI, {db_cidx,
				proplists:get_value(MPDFirst, MPDListIdx)}),
	{ok, #mpl{
		ui            = NotifyToUI,
		alsa          = ALSAHWInfo,
		mpd_list      = MPDListIdx, % [{name, idx}]
		mpd_active    = MPDFirst,
		%mpd_ratings   = PrimaryRatings,
		current_song  = #dbsong{key={<<>>, <<>>, <<>>}},
		current_queue = #queue{cnt=[], total=-1, qoffset=0, doffset=0,
					dsel=0}
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
			true  -> {Ctx, replace_song_info(Info,
							Ctx#mpl.current_song)};
			false -> NewSong = lists:foldl(fun({Name, _Idx}, S) ->
						complete_song_info(Name, S, Ctx)
					end, SongInfo, Ctx#mpl.mpd_list),
				 {Ctx#mpl{current_song=NewSong},
					replace_song_info(Info, NewSong)}
			end,
		gen_server:cast(Ctx#mpl.ui, {db_playing,
			[{x_maenmpc_alsa, query_alsa(Ctx#mpl.alsa)}|SendToUI]}),
		NewCtx;
	false ->
		% Don't care what other players are playing...
		Ctx
	end};
handle_cast(R={ui_simple, _A, _B}, Ctx) ->
	call_singleplayer(Ctx#mpl.mpd_active, R),
	{noreply, Ctx};
handle_cast(R={ui_simple, _A}, Ctx) ->
	call_singleplayer(Ctx#mpl.mpd_active, R),
	{noreply, Ctx};
handle_cast({ui_queue, ItemsRequested}, Ctx) ->
	{noreply, check_range_and_proc(ItemsRequested, Ctx)};
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
	{noreply, check_range_and_proc(ItemsRequested, Ctx#mpl{current_queue=
	Ctx#mpl.current_queue#queue{dsel = NewDSEL, doffset = NewDOffset}})};
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
	% TODO x RACE CONDITION? WHAT IF PLAYER DIES BETWEEN THESE LINES? / ALSO SHOULD PROBABLY DO ALL IN SINGLE TX WITH POSSIBILITY TO QUERY MULTIPLE KEYS AT ONCE!
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
		true -> SongInfo#dbsong{
			uris   = merge_tuple(SongInfo#dbsong.uris,
							OtherInfo#dbsong.uris),
			audios = merge_tuple(SongInfo#dbsong.audios,
							OtherInfo#dbsong.audios),
			rating = case SongInfo#dbsong.rating of
					-1   -> OtherInfo#dbsong.rating;
					_Any -> SongInfo#dbsong.rating
				end};
		false -> SongInfo
	end || {SongInfo, OtherInfo} <- lists:zip(SongInfos, OtherInfos)].

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

check_range_and_proc(ItemsRequested, Ctx) ->
	% TODO x could it make sense to inline the two functions?
	proc_range_result(check_in_range(ItemsRequested, Ctx),
							ItemsRequested, Ctx).

check_in_range(R, Ctx) ->
	Len     = length(Ctx#mpl.current_queue#queue.cnt),
	DOffset = Ctx#mpl.current_queue#queue.doffset,
	QOffset = Ctx#mpl.current_queue#queue.qoffset,
	if
	DOffset - QOffset >= R andalso Len - (DOffset + R) >= R ->
		{in_range, ok};
	DOffset - QOffset >= 0 andalso Len - (DOffset + R) >= 0 ->
		case DOffset - QOffset < R of
		true  -> {in_range, query_before};
		false -> {in_range, query_after}
		end;
	true ->
		out_of_range
	end.

proc_range_result(RangeResult, ItemsRequested, Ctx) ->
	PreCtx = case RangeResult of
		out_of_range -> Ctx#mpl{current_queue=query_queue(
			ItemsRequested * 5, Ctx#mpl.current_queue, Ctx)};
		_Other1 -> Ctx
		end,
	gen_server:cast(PreCtx#mpl.ui, {db_queue, PreCtx#mpl.current_queue,
				PreCtx#mpl.current_song#dbsong.playlist_id}),
	case RangeResult of
	{in_range, ok} ->
		PreCtx;
	{in_range, query_before} when
				PreCtx#mpl.current_queue#queue.qoffset > 0 ->
		QOffset = max(0, PreCtx#mpl.current_queue#queue.qoffset
							- ItemsRequested),
		QReq = min(PreCtx#mpl.current_queue#queue.qoffset,
							ItemsRequested),
		append_queue(query_queue(QReq, #queue{qoffset=QOffset}, PreCtx),
									PreCtx);
	{in_range, query_after} ->
		QOffset = PreCtx#mpl.current_queue#queue.qoffset +
				length(PreCtx#mpl.current_queue#queue.cnt),
		QReq = min(ItemsRequested, PreCtx#mpl.current_queue#queue.total
				- PreCtx#mpl.current_queue#queue.qoffset),
		case QReq =< 0 of
		true  -> PreCtx;
		false -> append_queue(query_queue(QReq,
				#queue{qoffset=QOffset}, PreCtx), PreCtx)
		end;
	_Other2 ->
		PreCtx
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
	% TODO QUERY PLAY COUNT FOR CURRENTLY SELLECTED ONE! [NEED TO DO THIS ALSO IN in_range, ok cases...

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

handle_info(interrupt_idle, Ctx) ->
	call_singleplayer(Ctx#mpl.mpd_active, request_update),
	{noreply, Ctx};
handle_info(_Message, Ctx) ->
	{noreply, Ctx}.

code_change(_OldVersion, Ctx, _Extra) -> {ok,      Ctx}.
