-module(maenmpc_scrobble).
-behavior(gen_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, code_change/3]).
-include_lib("maenmpc_db.hrl").

-record(sc, {talk_to, maloja, active, key, complete}).

init([TalkTo]) ->
	{ok, Maloja} = application:get_env(maenmpc, maloja),
	{ok, #sc{
		% static state
		talk_to   = TalkTo,
		maloja    = Maloja,
		% dynamic state
		active    = proplists:get_value(scrobble_send, Maloja, false),
		key       = {<<>>, <<>>, <<>>},
		complete  = false
	}}.

handle_call(is_active, _From, Ctx) ->
	case Ctx#sc.active of
	true  -> log("scrobble client is active", Ctx);
	false -> ok
	end,
	{reply, Ctx#sc.active, Ctx};
handle_call(_Any, _From, Ctx) ->
	{reply, ok, Ctx}.

log(Msg, Ctx) ->
	maenmpc_svc:log(Ctx#sc.talk_to, #dblog{msg=Msg, origin=scrobble}).

% TODO ASTAT HOW WOULD WE GO ABOUT REQUESTING A PICTURE FROM HERE?
handle_cast({db_playing, PlayInfo}, Ctx) when Ctx#sc.active ->
	NewSong = proplists:get_value(x_maenmpc, PlayInfo),
	NewKey  = case proplists:get_value(state, PlayInfo) of
			stop   -> {<<>>,<<>>,<<>>};
			_Other -> NewSong#dbsong.key
		end,
	case Ctx#sc.key =/= {<<>>,<<>>,<<>>} andalso Ctx#sc.key =/= NewKey
						andalso Ctx#sc.complete of
	true ->
		{Artist, Album, Track} = Ctx#sc.key,
		log(io_lib:format("send: ~s/~s/~s", [Artist, Album, Track]),
									Ctx);
	false ->
		ok
	end,
	{noreply, Ctx#sc{key=NewKey, complete=is_complete(PlayInfo)}};
handle_cast(_Any, Ctx) ->
	{noreply, Ctx}.

% https://www.last.fm/api/scrobbling
% “The track must be longer than 30 seconds.”
% “And the track has been played for at least half its duration, or for 4 minutes (whichever occurs earlier.)”
is_complete(PlayInfo) ->
	Elapsed  = binary_to_float(proplists:get_value(
						elapsed, PlayInfo, <<"0.0">>)),
	Duration = binary_to_float(proplists:get_value(
						duration, PlayInfo, <<"1.0">>)),
	Duration >= 30 andalso
		(Elapsed >= 240 orelse (Elapsed * 100.0 / Duration) >= 50.0).

%    def mpdscrobble_scrobble(self, track: MPDScrobbleTrack) -> None:
%        payload = {
%            "artist": track.artist,
%            "title": track.title,
%            "time": track.timestamp,
%            "album": track.album,
%            "key": self.api_key,
%        }
%        if "binary" in track.image:
%            payload["image"] = f"data:%s;base64,%s" % (track.image["type"],
%                               b64encode(track.image["binary"]).decode("ascii"))
%        post_url = self.url + "/apis/mlj_1/newscrobble"
%        logger.debug("Maloja: sending %s to %s", payload, post_url)
%        response = httpx.post(post_url, data=payload)
%        logger.debug("Maloja response: %s", response.content)
%        response.raise_for_status()


handle_info(_Message, Ctx)            -> {noreply, Ctx}.
code_change(_OldVersion, Ctx, _Extra) -> {ok, Ctx}.
