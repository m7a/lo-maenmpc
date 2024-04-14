-module(maenmpc_multiplayer).
-behavior(gen_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, code_change/3]).
-include_lib("maenmpc_db.hrl").
-record(mpl, {ui, alsa, mpd_list, mpd_active, mpd_ratings, current_song}).

init([NotifyToUI]) ->
	{ok, PrimaryRatings} = application:get_env(maenmpc, primary_ratings),
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
		ui           = NotifyToUI,
		alsa         = ALSAHWInfo,
		mpd_list     = MPDListIdx, % [{name, idx}]
		mpd_active   = MPDFirst,
		mpd_ratings  = PrimaryRatings,
		current_song = #dbsong{key={<<>>, <<>>, <<>>}}
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
			false -> NewSong = lists:foldl(fun(NameIdx, S) ->
						complete_song_info(NameIdx, S,
									Ctx)
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

complete_song_info({Name, _Idx}, SongInfo, Ctx) ->
	case Name =:= Ctx#mpl.mpd_active of
	true  -> SongInfo;
	% TODO x RACE CONDITION? WHAT IF PLAYER DIES BETWEEN THESE LINES?
	false -> case call_singleplayer(Name, is_online) of
		 true -> OtherInfo = call_singleplayer(Name, {query_by_key,
							SongInfo#dbsong.key}),
			 case OtherInfo#dbsong.key =:= SongInfo#dbsong.key of
			 true ->  SongInfo#dbsong{
				  uris   = merge_tuple(SongInfo#dbsong.uris,
						OtherInfo#dbsong.uris),
				  audios = merge_tuple(SongInfo#dbsong.audios,
						OtherInfo#dbsong.audios),
				  rating = case Name =:= Ctx#mpl.mpd_ratings of
					   true  -> OtherInfo#dbsong.rating;
					   false -> SongInfo#dbsong.rating
					   end};
			 false -> SongInfo
			 end;
		false -> SongInfo
		end
	end.

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

handle_info(interrupt_idle, Ctx) ->
	call_singleplayer(Ctx#mpl.mpd_active, request_update),
	{noreply, Ctx};
handle_info(_Message, Ctx) ->
	{noreply, Ctx}.

code_change(_OldVersion, Ctx, _Extra) -> {ok,      Ctx}.
