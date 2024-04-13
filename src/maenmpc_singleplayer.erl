-module(maenmpc_singleplayer).
-behavior(gen_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, code_change/3]).
-include_lib("maenmpc_db.hrl").
-record(spl, {db, syncidle, idx, len, mpd_volume, current_song}).

init(Properties) ->
	InitialState = #spl{
		db         = proplists:get_value(db,       Properties),
		syncidle   = proplists:get_value(syncidle, Properties),
		idx        = proplists:get_value(idx,      Properties),
		len        = proplists:get_value(len,      Properties),
		mpd_volume = -1
	},
	{ok, InitialState#spl{current_song = epsilon_song(InitialState)}}.

epsilon_song(Ctx) ->
	EpsTPL = list_to_tuple(lists:duplicate(Ctx#spl.len, <<>>)),
	#dbsong{key={<<>>, <<>>, <<>>}, uris=EpsTPL, playcount=0, rating=0,
			duration=1, year = <<>>, trackno=0, audios=EpsTPL}.

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
	{reply, maenmpc_sync_idle:interrupt_no_tx(Ctx#spl.syncidle), Ctx};
handle_call({mpd_idle, Name, Subsystems, Conn}, _From, Ctx) ->
	{reply, ok, update_playing_info(Name, Conn, Ctx)};
handle_call({query_by_key, Key}, _From, Ctx) ->
	% replaces populate with conn
	% (slightly less efficient, but more regular approach)
	{reply, maenmpc_sync_idle:run_transaction(Ctx#spl.syncidle, fun(Conn) ->
		case erlmpd:find(Conn,
				{land, [{tagop, artist, eq, element(1, Key)},
					{tagop, album,  eq, element(2, Key)},
					{tagop, title,  eq, element(3, Key)}]})
		of
		% nothing assigned
		[] -> epsilon_song(Ctx);
		% Single element found -- assign it!
		[Element|[]] -> query_rating(parse_metadata(Element, Ctx),
								Conn, Ctx);
		% result not unique - cannot safely assign
		[_Element|_Others] -> epsilon_song(Ctx)
		% else error is fatal because the connection state may
		% be disrupted.
		end
	end), Ctx};
handle_call({volume_change, Delta}, _From, Ctx) ->
	NewVal = Ctx#spl.mpd_volume + Delta,
	{reply, case Ctx#spl.mpd_volume /= -1 andalso
					 NewVal >= 0 andalso NewVal =< 100 of
		true  -> maenmpc_sync_idle:run_transaction(Ctx#spl.syncidle,
				fun(Conn) -> erlmpd:setvol(Conn, NewVal) end);
		false -> ok
	end, Ctx};
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
	undefined -> epsilon_song(Ctx);
	_ValidVal -> erlmpd_to_dbsong(CurrentSong, Ctx)
	end.

query_rating(DBCMP, Conn, Ctx) ->
	case DBCMP#dbsong.key =:= {<<>>, <<>>, <<>>} of
	true  -> DBCMP;
	false -> DBCMP#dbsong{rating=rating_for_uri(element(Ctx#spl.idx,
						DBCMP#dbsong.uris), Conn)}
	end.

send_playing_info(Name, Status, Ctx) ->
	ok = gen_server:cast(Ctx#spl.db, {db_playing,
			[{x_maenmpc, Ctx#spl.current_song}|
			[{x_maenmpc_name, Name}|Status]]}),
	Ctx#spl{mpd_volume = proplists:get_value(volume, Status, -1)}.

erlmpd_to_dbsong(Entry, Ctx) ->
	#dbsong{key          = erlmpd_to_key(Entry),
		uris         = new_tuple(proplists:get_value(file, Entry), Ctx),
		playcount    = 0,
		rating       = ?RATING_UNRATED,
		duration     = proplists:get_value('Time',  Entry, 1),
		year         = proplists:get_value('Date',  Entry, <<>>),
		trackno      = proplists:get_value('Track', Entry, 0),
		audios       = new_tuple(proplists:get_value('Format', Entry,
								<<>>), Ctx)}.

new_tuple(Value, Ctx) ->
	list_to_tuple([case Idx =:= Ctx#spl.idx of
		true  -> normalize_always(Value);
		false -> <<>>
	end || Idx <- lists:seq(1, Ctx#spl.len)]).

erlmpd_to_key(Entry) ->
	{normalize_key(proplists:get_value('Artist',   Entry, <<>>)),
	 normalize_strong(proplists:get_value('Album', Entry, <<>>)),
	 normalize_key(proplists:get_value('Title',    Entry, <<>>))}.

% Expensive normalization option required due to the fact that scrobbling or
% Maloja seem to mess with the supplied metadata.
normalize_key(Value) ->
	normalize_always(normalize_safe(Value)).

normalize_safe(Value) ->
	re:replace(string:replace(string:replace(
				lists:join(<<" ">>, string:lexemes(Value, " ")),
			"[", "("), "]", ")"),
		" \\(?feat\\.? .*$", "").

normalize_always(Value) ->
	unicode:characters_to_nfc_binary(Value).

normalize_strong(Value) ->
	normalize_always(re:replace(normalize_safe(Value), " \\(.*\\)$", "")).

rating_for_uri(RatingURI, Conn) ->
	case erlmpd:sticker_get(Conn, "song", binary_to_list(RatingURI),
								"rating") of
	% Typically error just means not found here (OK)
	{error, _Any} -> ?RATING_UNRATED;
	ProperRating  -> case list_to_integer(ProperRating) of
			 1      -> 0;
			 NotOne -> NotOne * 10
			 end
	end.

handle_cast(Msg={mpd_assign_error, _MPDName, _Reason}, Ctx) ->
	% bubble-up error
	gen_server:cast(Ctx#spl.db, Msg),
	{noreply, Ctx};
handle_cast(_Cast, Ctx) ->
	{noreply, Ctx}.

handle_info(_Message,    Ctx)         -> {noreply, Ctx}.
code_change(_OldVersion, Ctx, _Extra) -> {ok,      Ctx}.
