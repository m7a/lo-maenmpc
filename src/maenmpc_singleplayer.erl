-module(maenmpc_singleplayer).
-behavior(gen_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, code_change/3]).
-include_lib("maenmpc_db.hrl").

-record(spl, {
	db, syncidle, idx, len, is_rating, current_song,
	% keep track of MPD states as to allow toggle commands to be sent etc.
	mpd_volume, mpd_state, mpd_plength, mpd_repeat, mpd_random, mpd_single,
	mpd_consume, mpd_xfade
}).

init(Properties) ->
	InitialState = #spl{
		db          = proplists:get_value(db,       Properties),
		syncidle    = proplists:get_value(syncidle, Properties),
		idx         = proplists:get_value(idx,      Properties),
		len         = proplists:get_value(len,      Properties),
		is_rating   = proplists:get_value(rating,   Properties, false),
		mpd_volume  = -1,
		mpd_state   = undefined,
		mpd_plength = -1,
		mpd_repeat  = false,
		mpd_random  = false,
		mpd_single  = false,
		mpd_consume = false,
		mpd_xfade   = 0
	},
	{ok, InitialState#spl{current_song=maenmpc_erlmpd:epsilon_song(
							InitialState#spl.len)}}.

% TODO CASE database, playlist, output, sticker?
%handle_idle(database, Context, _Name) ->
%	% the song database has been modified after update
%	Context; % TODO
%handle_idle(playlist, Context, _Name) ->
%	% the queue (i.e. the current playlist) has been modified
%	Context; % TODO
%handle_idle(output, Context) ->
%	% an audio output has been added, removed or modified
%	% (e.g. renamed, enabled or disabled)
%	Context; % TODO
%handle_idle(sticker, Context) ->
%	% the sticker database has been modified.
%	Context; % TODO

handle_call(is_online, _From, Ctx) ->
	{reply, maenmpc_sync_idle:is_online(Ctx#spl.syncidle), Ctx};
handle_call(request_update, _From, Ctx) ->
	maenmpc_sync_idle:interrupt_no_tx(Ctx#spl.syncidle),
	{reply, ok, Ctx};
handle_call({mpd_idle, Name, Subsystems, Conn}, _From, Ctx) ->
	Ctx1 = update_playing_info(Name, Conn, Ctx),
	case lists:member(playlist, Subsystems) of
	true   -> gen_server:cast(Ctx1#spl.db, {db_playlist_changed, Name});
	_Other -> ok
	end,
	{reply, ok, Ctx1};
handle_call({query_by_keys, Keys}, _From, Ctx) ->
	% replaces populate with conn
	% (slightly less efficient, but more regular approach)
	{reply, maenmpc_sync_idle:run_transaction(Ctx#spl.syncidle, fun(Conn) ->
		[case erlmpd:find(Conn,
				{land, [{tagop, artist, eq, element(1, Key)},
					{tagop, album,  eq, element(2, Key)},
					{tagop, title,  eq, element(3, Key)}]})
		of
		% nothing assigned
		[] -> maenmpc_erlmpd:epsilon_song(Ctx#spl.len);
		% Single element found -- assign it!
		[Element|[]] -> query_rating(parse_metadata(Element, Ctx),
								Conn, Ctx);
		% result not unique - cannot safely assign
		[_Element|_Others] -> maenmpc_erlmpd:epsilon_song(Ctx#spl.len)
		% else error is fatal because the connection state may
		% be disrupted.
		end || Key <- Keys]
	end), Ctx};
handle_call({ui_simple, volume_change, Delta}, _From, Ctx) ->
	NewVal = Ctx#spl.mpd_volume + Delta,
	{reply, case Ctx#spl.mpd_volume /= -1 andalso
					 NewVal >= 0 andalso NewVal =< 100 of
		true  -> maenmpc_sync_idle:run_transaction(Ctx#spl.syncidle,
				fun(Conn) -> erlmpd:setvol(Conn, NewVal) end);
		false -> ok
	end, Ctx};
handle_call({ui_simple, Action}, _From, Ctx) ->
	{reply, maenmpc_sync_idle:run_transaction(Ctx#spl.syncidle, fun(Conn) ->
		ui_simple_tx(Action, Conn, Ctx)
	end), Ctx};
handle_call({query_queue, ItemsRequested, CurrentQ}, _From, Ctx) ->
	{NewQ, Ctx2} = maenmpc_sync_idle:run_transaction(
						Ctx#spl.syncidle, fun(Conn) ->
		Ctx1 = case Ctx#spl.mpd_plength =< 0 of
			 true  -> update_status(erlmpd:status(Conn), Ctx);
			 false -> Ctx
			 end,
		Q0A = max(0, min(CurrentQ#dbscroll.qoffset,
						Ctx1#spl.mpd_plength - 1)),
		Q1A = min(Q0A + ItemsRequested, Ctx1#spl.mpd_plength),
		{CurrentQ#dbscroll{
			cnt=[query_rating(parse_metadata(El, Ctx1), Conn, Ctx1)
				|| El <- erlmpd:playlistinfo(Conn, {Q0A, Q1A})],
			total=Ctx1#spl.mpd_plength,
			qoffset=max(0, Q0A - ItemsRequested)
		}, Ctx1}
	end),
	{reply, NewQ, Ctx2};
handle_call({query_artists_count, Filter}, _From, Ctx) ->
	{reply, maenmpc_sync_idle:run_transaction(Ctx#spl.syncidle, fun(Conn) ->
		erlmpd:count_group(Conn, artist, Filter)
	end), Ctx};
handle_call({query_artists, QList, Filter}, _From, Ctx) ->
	{reply, maenmpc_sync_idle:run_transaction(Ctx#spl.syncidle, fun(Conn) ->
		[[query_rating(parse_metadata(El, Ctx), Conn, Ctx)
			|| El <- erlmpd:find(Conn, {land,
					[Filter, {tagop, artist, eq, Artist}]})]
		|| Artist <- QList]
	end), Ctx};
handle_call({enqueue, Song}, _From, Ctx) ->
	{reply, maenmpc_sync_idle:run_transaction(Ctx#spl.syncidle, fun(Conn) ->

		ok = erlmpd:add(Conn, element(Ctx#spl.idx, Song#dbsong.uris))
	end), Ctx};
% TODO ALL OTHER INTERACTIVE FUNCTION STUFF GOES HERE...
handle_call(_Call, _From, Ctx) ->
	{reply, ok, Ctx}.

%is_status_subsystem(player)  -> true;  % start stop seek new song, tags changed
%is_status_subsystem(mixer)   -> true;  % the volume has been changed
%is_status_subsystem(options) -> true;  % repeat, random, crossfade, replay gain
%is_status_subsystem(_Other)  -> false.

update_playing_info(Name, Conn, Ctx) ->
	% state, audio, volume, repeat, random, single, consume, xfade,
	% updating_db, time
	Status = erlmpd:status(Conn),
	% file [uri], Artist, Date, Album, Track, Title, Time [duration],
	CurrentSong = parse_metadata(erlmpd:currentsong(Conn), Ctx),
	send_playing_info(Name, Status, case CurrentSong#dbsong.key =:=
					Ctx#spl.current_song#dbsong.key of
			true  -> Ctx;
			false -> Ctx#spl{current_song=
					query_rating(CurrentSong, Conn, Ctx)}
			end).

parse_metadata(CurrentSong, Ctx) ->
	case proplists:get_value(file, CurrentSong) of
	undefined -> maenmpc_erlmpd:epsilon_song(Ctx#spl.len);
	_ValidVal -> maenmpc_erlmpd:to_dbsong(CurrentSong,
						Ctx#spl.idx, Ctx#spl.len)
	end.

query_rating(DBCMP, Conn, Ctx) ->
	case DBCMP#dbsong.key =:= {<<>>, <<>>, <<>>} orelse
							not Ctx#spl.is_rating of
	true  -> DBCMP;
	false -> DBCMP#dbsong{rating=rating_for_uri(element(Ctx#spl.idx,
						DBCMP#dbsong.uris), Conn)}
	end.

send_playing_info(Name, Status, Ctx) ->
	ok = gen_server:cast(Ctx#spl.db, {db_playing,
			[{x_maenmpc, Ctx#spl.current_song}|
			[{x_maenmpc_name, Name}|Status]]}),
	update_status(Status, Ctx).

update_status(Status, Ctx) ->
	Ctx#spl{mpd_volume  = proplists:get_value(volume,  Status, -1),
		mpd_plength = proplists:get_value(playlistlength, Status, -1),
		mpd_state   = proplists:get_value(state,   Status, undefined),
		mpd_repeat  = proplists:get_value(repeat,  Status, false),
		mpd_random  = proplists:get_value(random,  Status, false),
		mpd_single  = proplists:get_value(single,  Status, false),
		mpd_consume = proplists:get_value(consume, Status, false),
		mpd_xfade   = proplists:get_value(xfade,   Status, 0)}.

rating_for_uri(RatingURI, Conn) ->
	case erlmpd:sticker_get(Conn, "song", binary_to_list(RatingURI),
								"rating") of
	% Typically error just means not found here (OK)
	{error, _Any} -> ?RATING_UNRATED;
	ProperRating  -> maenmpc_erlmpd:convert_rating(
						list_to_integer(ProperRating))
	end.

ui_simple_tx(stop, Conn, _Ctx) ->
	erlmpd:stop(Conn);
ui_simple_tx(toggle_pause, Conn, Ctx) ->
	case Ctx#spl.mpd_state of
	undefined -> ok; % Cannot do anyhting in undefined state
	play      -> erlmpd:pause(Conn, true);
	pause     -> erlmpd:pause(Conn, false);
	stop      -> erlmpd:play(Conn)
	end;
ui_simple_tx(toggle_repeat, Conn, Ctx) ->
	erlmpd:repeat(Conn, not Ctx#spl.mpd_repeat);
ui_simple_tx(toggle_random, Conn, Ctx) ->
	erlmpd:random(Conn, not Ctx#spl.mpd_random);
ui_simple_tx(toggle_single, Conn, Ctx) ->
	erlmpd:single(Conn, not Ctx#spl.mpd_single);
ui_simple_tx(toggle_consume, Conn, Ctx) ->
	erlmpd:consume(Conn, not Ctx#spl.mpd_consume);
ui_simple_tx(toggle_xfade, Conn, Ctx) ->
	erlmpd:crossfade(Conn, max(0, 5 - Ctx#spl.mpd_xfade)).

handle_cast(Msg={mpd_assign_error, _MPDName, _Reason}, Ctx) ->
	% bubble-up error
	gen_server:cast(Ctx#spl.db, Msg),
	{noreply, Ctx};
handle_cast(_Cast, Ctx) ->
	{noreply, Ctx}.

handle_info(_Message,    Ctx)         -> {noreply, Ctx}.
code_change(_OldVersion, Ctx, _Extra) -> {ok,      Ctx}.
