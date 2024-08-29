-module(maenmpc_scrobble).
-behavior(gen_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, code_change/3]).
-include_lib("maenmpc_db.hrl").

-record(sc, {talk_to, maloja, calbumart, active, key, complete}).

init([TalkTo]) ->
	{ok, MPDList} = application:get_env(maenmpc, mpd),
	{ok, Maloja} = application:get_env(maenmpc, maloja),
	{ok, #sc{
		% static state
		talk_to   = TalkTo,
		maloja    = Maloja,
		calbumart =
			case proplists:get_value(primary_albumart, Maloja) of
			undefined -> undefined;
			MPDName   -> proplists:get_value(MPDName, MPDList)
			end,
		% dynamic state
		active    = proplists:get_value(scrobble_send, Maloja, false),
		key       = {<<>>, <<>>, <<>>},
		complete  = false
	}}.

handle_call(_Any, _From, Ctx) ->
	{reply, ok, Ctx}.

log(Msg, Ctx) ->
	maenmpc_svc:log(Ctx#sc.talk_to, #dblog{msg=Msg, origin=scrobble}).

handle_cast({db_playing, PlayInfo}, Ctx) when Ctx#sc.active ->
	NewSong = proplists:get_value(x_maenmpc, PlayInfo),
	NewKey  = case proplists:get_value(state, PlayInfo) of
			stop   -> {<<>>,<<>>,<<>>};
			_Other -> NewSong#dbsong.key
		end,
	case Ctx#sc.key =/= {<<>>,<<>>,<<>>} andalso Ctx#sc.key =/= NewKey
						andalso Ctx#sc.complete of
	true  -> scrobble(Ctx);
	false -> ok
	end,
	{noreply, Ctx#sc{key=NewKey, complete=is_complete(PlayInfo)}};
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

scrobble(Ctx = #sc{maloja = Maloja, calbumart = CAlbumArt,
					key = {Artist, Album, Track}}) ->
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
			log(io_lib:format("Error: ~s", Error), Ctx),
			case proplists:get_value(scrobble_file, Maloja) of
			undefined ->
				log("No file fallback defined. Dropped!", Ctx);
			File ->
				log("Fallback scrobble to file...", Ctx),
				scrobble_to_file(Payload, File, Ctx)
			end
		end
	end.

% TODO x ADD IMAGE HERE!
%    def mpdscrobble_scrobble(self, track: MPDScrobbleTrack) -> None:
%        if "binary" in track.image:
%            payload["image"] = f"data:%s;base64,%s" % (track.image["type"],
%                               b64encode(track.image["binary"]).decode("ascii"))
% image = self.readpicture(currentsong("file"))
add_album_art(Payload, #sc{calbumart=undefined}) -> Payload;
add_album_art(Payload, #sc{calbumart=CAlbumArt}) ->
	Payload.

scrobble_to_file(Payload, File, Ctx) ->
	case file:write_file(File, jiffy:encode(Payload), [append]) of
	ok ->
		ok;
	{error, Reason} ->
		log(io_lib:format("Failed to write file ~s: ~w",
							[File, Reason]), Ctx)
	end.

handle_info(_Message, Ctx)            -> {noreply, Ctx}.
code_change(_OldVersion, Ctx, _Extra) -> {ok, Ctx}.
