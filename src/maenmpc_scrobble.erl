-module(maenmpc_scrobble).
-behavior(gen_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, code_change/3]).
-include_lib("maenmpc_db.hrl").

-record(sc, {talk_to, maloja, calbumart, eps, active, song, complete}).

init([TalkTo]) ->
	{ok, MPDList} = application:get_env(maenmpc, mpd),
	{ok, Maloja}  = application:get_env(maenmpc, maloja),
	MPDListIdx    = [{Name, Idx} || {{Name, _ConnInfoI}, Idx} <-
			lists:zip(MPDList, lists:seq(1, length(MPDList)))],
	EPS           = maenmpc_erlmpd:epsilon_song(length(MPDList)),
	{ok, #sc{
		% static state
		talk_to   = TalkTo,
		maloja    = Maloja,
		calbumart =
			case proplists:get_value(primary_albumart, Maloja) of
			undefined -> undefined;
			MPDName   -> {proplists:get_value(MPDName, MPDListIdx),
					proplists:get_value(MPDName, MPDList)}
			end,
                eps       = EPS,
		% dynamic state
		active    = proplists:get_value(scrobble_send, Maloja, false),
		song      = EPS,
		complete  = false
	}}.

handle_call(_Any, _From, Ctx) ->
	{reply, ok, Ctx}.

log(Msg, Ctx) ->
	maenmpc_svc:log(Ctx#sc.talk_to, #dblog{msg=Msg, origin=scrobble}).

handle_cast({db_playing, PlayInfo}, Ctx) when Ctx#sc.active ->
	NewSong = case proplists:get_value(state, PlayInfo) of
		stop   -> Ctx#sc.eps;
		_Other -> proplists:get_value(x_maenmpc, PlayInfo)
		end,
	case Ctx#sc.song#dbsong.key =/= {<<>>,<<>>,<<>>} andalso
			Ctx#sc.song#dbsong.key =/= NewSong#dbsong.key andalso
			Ctx#sc.complete of
	true  -> scrobble(Ctx);
	false -> ok
	end,
	{noreply, Ctx#sc{song=NewSong, complete=is_complete(PlayInfo)}};
handle_cast(_Any, Ctx) ->
	{noreply, Ctx}.

% Per https://www.last.fm/api/scrobbling:
%  - “The track must be longer than 30 seconds.”
%  - “And the track has been played for at least half its duration,
%     or for 4 minutes (whichever occurs earlier.)”
is_complete(PlayInfo) ->
	Elapsed  = binary_to_float(proplists:get_value(
						elapsed, PlayInfo, <<"0.0">>)),
	Duration = binary_to_float(proplists:get_value(
						duration, PlayInfo, <<"1.0">>)),
	Duration >= 30 andalso
		(Elapsed >= 240 orelse (Elapsed * 100.0 / Duration) >= 50.0).

scrobble(Ctx = #sc{maloja = Maloja, song =
					#dbsong{key={Artist,Album,Track}}}) ->
	log(io_lib:format("send: ~s/~s/~s", [Artist, Album, Track]), Ctx),
	Payload = add_album_art(#{
		artist => Artist,
		album  => Album,
		title  => Track,
		time   => os:system_time(second)
	}, Ctx),
	case proplists:get_value(key, Maloja) of
	undefined ->
		% No API key defined, means scrobble should go to file
		scrobble_to_file(Payload,
			proplists:get_value(scrobble_file, Maloja), Ctx);
	_APIKey ->
		case maenmpc_maloja:scrobble(Payload,
						maenmpc_maloja:conn(Maloja)) of
		ok ->
			ok;
		ok_exists ->
			log("already exists - skipped - not an error", Ctx);
		{error, Error} ->
			log(io_lib:format("Error: ~s", [Error]), Ctx),
			case proplists:get_value(scrobble_file, Maloja) of
			undefined ->
				log("No file fallback defined. Dropped!", Ctx);
			File ->
				log("Fallback scrobble to file...", Ctx),
				scrobble_to_file(Payload, File, Ctx)
			end
		end
	end.

add_album_art(Payload, #sc{calbumart=undefined}) -> Payload;
add_album_art(Payload, Ctx = #sc{calbumart={IDX, ConnInfo},
						song=#dbsong{uris=URIS}}) ->
	{ok, Conn} = maenmpc_erlmpd:connect(ConnInfo),
	URI = element(IDX, URIS),
	PIC = erlmpd:readpicture(Conn, URI),
	erlmpd:disconnect(Conn),
	case PIC of
	{error, Error} ->
		log(io_lib:format("Failed read picture: ~w", [Error]), Ctx),
		Payload;
	{unknown, Binary} ->
		case iolist_size(Binary) of
		0 ->
			log(io_lib:format("No picture found for ~s", [URI]),
									Ctx),
			Payload;
		_Other ->
			log(io_lib:format("Unknown image format for ~s", [URI]),
									Ctx),
			Payload
		end;
	{Type, Binary} ->
		% TODO TO ALIGN WITH MOST RECENT VERSION OF THE PATCH USE `album_image` (or `track_image`)
		maps:put(image, iolist_to_binary([<<"data:">>,Type,
			<<";base64,">>,
			base64:encode(iolist_to_binary(Binary))]), Payload)
	end.

scrobble_to_file(Payload, File, Ctx) ->
	case file:write_file(File, [jiffy:encode(Payload), <<"\n">>],
								[append]) of
	ok ->
		ok;
	{error, Reason} ->
		log(io_lib:format("Failed to write file ~s: ~w",
							[File, Reason]), Ctx)
	end.

handle_info(_Message, Ctx)            -> {noreply, Ctx}.
code_change(_OldVersion, Ctx, _Extra) -> {ok, Ctx}.
