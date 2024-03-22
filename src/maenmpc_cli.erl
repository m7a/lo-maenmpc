-module(maenmpc_cli).
-export([run/3]).
-include_lib("stdlib/include/ms_transform.hrl").

-define(SCROBBLEBASE,  1700000000).
-define(SCROBBLESTEP1, 3600).
-define(SCROBBLESTEP2, 240).

% return ok to signal success, {error, "Descr"} for failure, {next, _Params}
% to exec to app
run(MPDList, PrimaryRatings, Maloja) ->
	case init:get_argument(help) of
	{ok, _Any} -> usage();
	_NoHelpArg ->
		case init:get_argument(radio) of
		{ok, SelectedMPD} ->
			case SelectedMPD of
			[[]] ->
				[{MPDName, _Config}|_Rest] = MPDList,
				radio(MPDList, PrimaryRatings, Maloja, MPDName);
			[[MPDName]] ->
				radio(MPDList, PrimaryRatings, Maloja, MPDName);
			_Other ->
				io:fwrite(
				"Multiple MPD names are not supported.~n")
			end;
		_NoRadioArg ->
			case init:get_argument(gmbrc) of
			{ok, [[Path]]} -> use_gmbrc(MPDList, PrimaryRatings,
								Maloja, Path);
			_NoGMBRCArg    -> {next, []}
			end
		end
	end.

usage() ->
	io:fwrite(
"USAGE $0 foreground                          -- run regularly~n" ++
"USAGE $0 foregorund -help                    -- this page~n" ++
"USAGE $0 foregorund -radio [MPDNAME]         -- play custom music radio~n" ++
"USAGE $0 foregorund -mpdsticker -gmbrc GMBRC -- import ratings~n" ++
"USAGE $0 foreground -scrobble   -gmbrc GMBRC -- import playcounts~n"
	).

%--------------------------------------------------------------------[ Radio ]--
% TODO IMPLEMENTED HERE FOR TESTING PURPOSES ONLY. MOVE CODE TO SENSIBLE LOCATION
%      ONCE IT IS CLEAR WHAT IT WOULD BE
% https://mpd.readthedocs.io/en/latest/protocol.html
-record(plsong, {key, player, uri, artist, album, title, playcount, rating}).
-type plsong() :: #plsong{key::binary(), player::atom(), uri::binary(),
			artist::binary(), album::binary(), title::binary(),
			playcount::integer(), rating::integer()}.

radio(MPDList, PrimaryRatings, Maloja, UseMPD) ->
	io:fwrite("[ INFO  ] radio~n"),
	ets:new(pl_songs,    [bag, named_table, {keypos, #plsong.key}]),
	ets:new(album_cache, [bag, named_table]),

	%io:fwrite("[ INFO  ] query use~n"),
	%Use = query_all(UseMPD, MPDList),

	io:fwrite("[ INFO  ] query primary~n"),
	Primary = query_all(PrimaryRatings, MPDList),

	% TODO DROP ALL THAT ARE PRESENT IN ONLY ONE OF THE DBS?

	io:fwrite("[ INFO  ] associate playcounts~n"),
	URL = proplists:get_value(url, Maloja),
	Key = proplists:get_value(key, Maloja),
	{ok, {_Status, _Headers, AllScrobblesRaw}} = httpc:request(
			io_lib:format("~s/scrobbles?key=~s&perpage=10000000",
			[URL, uri_string:quote(Key)])),
	AllScrobbles = jiffy:decode(AllScrobblesRaw, [return_maps]),
	lists:foreach(fun(Scrobble) ->
			ScrobbleTrack = maps:get(<<"track">>, Scrobble),
			ScrobbleAlbum = maps:get(<<"album">>, ScrobbleTrack),
			[Artist|_Others] = maps:get(<<"artists">>, ScrobbleTrack),
			Title = maps:get(<<"title">>, ScrobbleTrack),
			Album = case ScrobbleAlbum of
				null ->
					CK = iolist_to_binary([Artist, <<"|">>,
									Title]),
					case ets:lookup(album_cache, CK) of
					% TODO MAY MAKE SENSE TO PRINT WARNINGS FOR ALL BUT [One] -> One case!
					[]
						-> <<>>;
					[{_Key, One}] ->
						One;
					[{_Key, First}|Rem] ->
						First
					end;
				_Other ->
					maps:get(<<"albumtitle">>,
								ScrobbleAlbum)
				end,
			DBKey = iolist_to_binary([Artist, <<"|">>, Album,
							<<"|">>, Title]),
			% TODO ASTAT FAILS FOR TABLE TYPE.
			%      CONSIDER INTRODUCING THE STEP TO CONVERT FROM bag -> set and retain all keys that are present in both variants in order to fix this here.
			%ets:update_counter(pl_songs, DBKey, {#plsong.playcount, 1}),
			io:fwrite("<~p>~n", [DBKey])
		end, maps:get(<<"list">>, AllScrobbles)),
	%io:fwrite("<~p>~n", [AllScrobbles]),
	% numscrobbles
	% TODO THE CALL THAT CAN QUERY INCLUDING ALBUM is `albuminfo` but it gives an error message
	% {"status": "error", "error": {"type": "missing_entity_parameter", "value": null, "desc": "This API call is not valid without an entity (track or artist)."}}
	% TODO ALSO THIS DOES NOT EVEN ACCEPT MULTIPLE PARAMETERS, WILL ALWAYS QUERY FOR ARTIST ONLY. NEED TO RE-THINK IT / HOW TO QUERY A SPECIFIC SCROBBLE. READ PYTHON SOURCE!
	%Prefix = URL ++ "/numscrobbles?key=" ++ uri_string:quote(Key),
	%ok = ets:foldr(fun(Element, _AccIn) ->
	%		Query = io_lib:format(
	%			"~s&album=~s&artist=~s&title=~s",
	%			[Prefix, uri_string:quote(Element#plsong.album),
	%			uri_string:quote(Element#plsong.artist),
	%			uri_string:quote(Element#plsong.title)]),
	%		{ok, Response} = httpc:request(Query),
	%		io:fwrite("~p [~s] ->~n   ~p~n", [Element, Query,
	%							Response]),
	%		ok
	%	end, ok, pl_songs),
	% TODO MALOJA INTERACTION HERE...
	%KeyParam = "?key=" ++ uri_string:quote(Key),
	%{ok, Result} = httpc:request(Endpoint ++ KeyParam ++ "&perpage=1000000"),
	%io:fwrite("-> ~p~n", [Result]),

	io:fwrite("[ INFO  ] associate ratings~n"),
	% ...
	% TODO STICEKRS INTERACTION HERE
	ets:delete(album_cache),
	ets:delete(pl_songs),
	ok.

query_all(MPDName, MPDList) ->
	HostPort = proplists:get_value(ip,
					proplists:get_value(MPDName, MPDList)),
	{Host, Port} = HostPort,
	{ok, Conn} = erlmpd:connect(Host, Port),
	lists:foreach(fun(Entry) ->
			PLS = entry_to_plsong(Entry),
			ets:insert(pl_songs, PLS#plsong{player=MPDName}),
			ets:insert(album_cache,
				{iolist_to_binary([PLS#plsong.artist, "|",
					PLS#plsong.title]), PLS#plsong.album})
		% Query all data (modified-since '0') or (base '')
		end, erlmpd:find(Conn, "(base '')")),
	erlmpd:disconnect(Conn),
	HostPort.

entry_to_plsong(Entry) ->
	URI    = proplists:get_value(file,     Entry),
	Artist = proplists:get_value('Artist', Entry, <<>>),
	Album  = proplists:get_value('Album',  Entry, <<>>),
	Title  = proplists:get_value('Title',  Entry, <<>>),
	#plsong{
		key=iolist_to_binary([Artist, "|", Album, "|", Title]),
		uri=URI,
		artist=Artist,
		album=Album,
		title=Title,
		playcount=0,
		rating=-1
	}.

%------------------------------------------------------------[ GMBRC Parsing ]--
-record(gmbsong, {gmbidx, path, artist, album, title, year, lastplay, playcount,
		rating}).
-type gmbsong() :: #gmbsong{gmbidx::integer(), path::binary(), artist::binary(),
		album::binary(), title::binary(), year::integer(),
		lastplay::integer(), playcount::integer(), rating::integer()}.

use_gmbrc(MPDList, PrimaryRatings, Maloja, Path) ->
	database_read(Path),
	RV = case init:get_argument(mpdsticker) of
	{ok, _Any}       -> import_ratings_to_stickers(MPDList, PrimaryRatings);
	_NoMPDStickerArg ->
		case init:get_argument(scrobble) of
		{ok, _Any}     -> import_playcounts_to_scrobbles(Maloja);
		_NoScrobbleArg -> {next, []}
		end
	end,
	ets:delete(gmb_songs),
	RV.

database_read(GMBRC) ->
	io:fwrite("Read GMBRC... "),
	{ok, RawDataBinary} = file:read_file(GMBRC),
	{await_eof, Lines, _Keys} = lists:foldl(fun database_line/2,
			{await_marker, invalid, invalid},
			binary:split(RawDataBinary, <<"\n">>, [global, trim])),
	ets:new(gmb_songs, [set, named_table, {keypos, #gmbsong.gmbidx}]),
	lists:foreach(fun database_convert_store/1, Lines),
	io:fwrite("OK~n").

database_line(<<"[Songs]">>, {await_marker, invalid, invalid}) ->
	{process_headings, invalid, invalid};
database_line(_Line, {await_marker, invalid, invalid}) ->
	{await_marker, invalid, invalid};
database_line(CSV, {process_headings, invalid, invalid}) ->
	{process_contents, [], [<<"idx">>|
				binary:split(CSV, <<"\t">>, [global, trim])]};
database_line(<<>>, {process_contents, List, Keys}) ->
	{await_eof, List, Keys};
database_line(CSV, {process_contents, Tail, Keys}) ->
	{process_contents, [lists:zip(Keys, binary:split(CSV,
				<<"\t">>, [global, trim]))|Tail], Keys};
database_line(_Line, {await_eof, List, Keys}) ->
	{await_eof, List, Keys}.

database_convert_store(L) ->
	Path = filename:join(database_keyfind(<<"path">>, L),
					database_keyfind(<<"file">>, L)),
	% file_exists => only import those ratings for which actual song files
	%                are present
	case file_exists(Path) of
	true ->
		ets:insert(gmb_songs, #gmbsong{
			gmbidx=binary_to_integer(
						database_keyfind(<<"idx">>, L)),
			path=Path,
			artist=database_keyfind(<<"artist">>, L),
			album=database_keyfind(<<"album">>, L),
			title=database_keyfind(<<"title">>, L),
			year=database_keyfind(<<"year">>, L),
			lastplay=binary_to_integer(
					database_keyfind(<<"lastplay">>, L)),
			playcount=binary_to_integer(
					database_keyfind(<<"playcount">>, L)),
			rating=case database_keyfind(<<"rating">>, L) of
				<<>>  -> -1;
				Value -> binary_to_integer(Value)
			end
		});
	false ->
		ignored
	end.

file_exists(File) ->
	case file:read_file_info(File) of
	{ok, _FileInfo} -> true;
	{error, enoent} -> false
	% other cases are special and reported as errors here!
	end.

database_keyfind(Key, List) ->
	{_K, Value} = lists:keyfind(Key, 1, List),
	Value.

%------------------------------------------------------[ Ratings to Stickers ]--
% /_build/default/rel/maenmpc/bin/maenmpc foreground
% -gmbrc /data/programs/music2/supplementary/gmusicbrowser/gmbrc -mpdsticker
import_ratings_to_stickers(MPDList, PrimaryRatings) ->
	io:fwrite("import_ratings_to_stickers~n"),
	{Host, Port} = proplists:get_value(ip,
				proplists:get_value(PrimaryRatings, MPDList)),
	io:fwrite("connecting to ~s:~w...~n", [Host, Port]),
	{ok, Conn} = erlmpd:connect(Host, Port),
	Root = find_root_directory(Conn),
	RootLen = byte_size(Root),
	io:fwrite("root = ~p~n", [Root]),
	RatedSongs = ets:select(gmb_songs, ets:fun2ms(
				fun(X) when X#gmbsong.rating /= -1 -> X end)),
	lists:foreach(fun(Song) ->
			CheckPrefix = binary:part(Song#gmbsong.path, 0,
								RootLen),
			case CheckPrefix == Root of
			true ->
				UseKey = binary_to_list(binary:part(
						Song#gmbsong.path, RootLen,
						byte_size(Song#gmbsong.path) -
						RootLen)),
				Rating = float_to_list(max(1,
						Song#gmbsong.rating / 10),
						[{decimals, 0}]),
				io:fwrite("ASSIGN ~s = ~s~n", [UseKey, Rating]),
				ok = erlmpd:sticker(Conn, set, "song", UseKey,
							"rating", Rating);
			false ->
				io:fwrite("ERROR Prefix mismatch for ~s: " ++
					"~s /= ~s~n. Skipped...~n",
					[Song#gmbsong.path, CheckPrefix, Root])
			end
		end, RatedSongs),
	erlmpd:disconnect(Conn).

% artist, album, title 
find_root_directory(Conn) ->
	Paths = [extract_root(Conn, Song) || Song <-
			find_alpha_songs(gmb_songs, ets:first(gmb_songs), [])],
	[PH|PT] = Paths,
	case lists:all(fun(El) -> El == PH end, PT) of
	% all elements are equal, PH is the prefix to remove from GMB entries
	true  -> PH;
	false -> throw(io_lib:format("Failed to find common root directory: " ++
							"~p~n", [Paths]))
	end.

extract_root(Conn, Song) ->
	PathGMB = filename:rootname(Song#gmbsong.path),
	% by enforcing ASCII subset we need not quote inside the matching
	% expression!
	[Result] = erlmpd:find(Conn, io_lib:format("((artist == '~s') "
				++ "AND (album == '~s') AND (title == '~s'))",
				[Song#gmbsong.artist, Song#gmbsong.album,
				Song#gmbsong.title])),
	PathMPD   = filename:rootname(proplists:get_value(file, Result)),
	SuffixLen = binary:longest_common_suffix([PathGMB, PathMPD]),
	case SuffixLen == byte_size(PathMPD) of
	true  -> binary:part(PathGMB, 0, byte_size(PathGMB) - SuffixLen);
	false -> {error, io_lib:format(
				"MPD path ~s is not a suffix of GMB path ~s!",
				[PathMPD, PathGMB])}
	end.

find_alpha_songs(_Tab, '$end_of_table', Acc) ->
	Acc;
find_alpha_songs(_Tab, _Key, Acc) when length(Acc) >= 3 ->
	Acc;
find_alpha_songs(Tab, Key, Acc) ->
	[Song] = ets:lookup(Tab, Key),
	find_alpha_songs(Tab, ets:next(Tab, Key),
			case validate_alpha(Song#gmbsong.artist) andalso
				validate_alpha(Song#gmbsong.album) andalso
				validate_alpha(Song#gmbsong.title) of
			true  -> [Song|Acc];
			false -> Acc
			end). 

% https://erlang.org/pipermail/erlang-questions/2015-August/085486.html
% https://erlang.org/pipermail/erlang-questions/2015-August/085489.html
validate_alpha(Binary) ->
	Binary == << <<C>> || <<C:8>> <= Binary, (C >= $A andalso C =< $z) >>.

%--------------------------------------------------[ Playcounts to Scrobbles ]--
import_playcounts_to_scrobbles(Maloja) ->
	io:fwrite("[ INFO  ] import_playcounts_to_scrobbles~n"),
	URL = proplists:get_value(url, Maloja),
	Key = proplists:get_value(key, Maloja),
	Endpoint = URL ++ "/newscrobble",
	KeyParam = "key=" ++ uri_string:quote(Key),
	PlayedSongs = ets:select(gmb_songs, ets:fun2ms(
				fun(X) when X#gmbsong.playcount >= 1 -> X end)),
	io:fwrite("[ INFO  ] begin scrobbling...~n"),
	{OK, OKExists, Fail} = merge_stats([merge_stats(
			[generate_scrobble(Endpoint, KeyParam, Song, X) ||
				X <- lists:seq(1, Song#gmbsong.playcount)]
		) || Song <- PlayedSongs]),
	io:fwrite("[ INFO  ] Added=~w AlreadyKnown=~w Failed=~w" ++
				" (see messages above for failures)~n",
				[OK, OKExists, Fail]).

% Return a triple of OK_Added|OK_Exists|Fail
-spec generate_scrobble(Endpoint::string(), KeyParam::string(), Song::gmbsong(),
			Nth::integer()) -> {integer(), integer(), integer()}.
% Example Data: Successful Scrobble
%   Result = {{"HTTP/1.1",200,"OK"},
%            [{"date","Sun, 17 Mar 2024 12:00:57 GMT"},
%             {"server","waitress"},
%             {"content-length","132"},
%             {"content-type","application/json"}],
%            "{\"status\": \"success\", \"track\": {\"artists\": [\"Dido\"],
%              \"title\": \"Who Makes You Feel\"},
%              \"desc\": \"Scrobbled Who Makes You Feel by Dido\"}"}
%
% Example Data: Exists Scrobble
%   Result = {{"HTTP/1.1",200,"OK"},
%            [{"date","Sun, 17 Mar 2024 12:01:10 GMT"},
%             {"server","waitress"},
%             {"content-length","258"},
%             {"content-type","application/json"}],
%            "{\"status\": \"success\", \"desc\": \"The scrobble [...].\",
%              \"track\": {}, \"warnings\": [{\"type\": \"scrobble_exists\",
%              \"value\": null, \"desc\": \"This scrobble [...].\"}]}"}
generate_scrobble(Endpoint, KeyParam, Song, Nth) ->
	% -- Virtual Scrobble Timestamp --
	% Since GMUSICBROWSER does not store the time when the song was played
	% exactly (only the last time when it was played is being recorded),
	% a virtual time stamp must be generated in order to allow multiple
	% play backs of a song to be reported as scrobbles.
	%   The algorithm here works _without_ relying on the “last played”
	% timestamp reported by gmusicbrowser because it could only work for
	% one (new since last syncronization) play back. Old play backs could
	% not be accounted for.
	%   Instead of using the last reported playback, the following approach
	% is implemented: We set a base date `SCROBBLEBASE` and subtract an
	% index-specific amount of time `SCROBBLESTEP1` * gmbidx from it to
	% arrive at a “base timestamp” for scrobbles of this song as generated
	% by the import. The idea is to have this timestamp in the past such
	% that it does not clash with any “real” scrobbles. Then, we subtract
	% a fixed amount of time (similar to “one playback length” which for
	% simplicity is just assumed to be 4 minutes here) for each playback.
	%   This will make all repeated playbacks appear as scrobbles “in
	% sequence” at some generated past date.
	%   By subtracting (rather than adding) we ensure that we always stay
	% in the past not to conflict with new (real) scrobbles. By using
	% constant offsets and lengths we ensure that multiple imports of the
	% same gmbrc (or a changed version of the same origin) only the
	% differences are imported because those get an “earlier” time stamp
	% computed compared to the previously submitted ones.
	%   TL;DR: This algorithm works incrementally and is idempotent such as
	% long as the GMBRC is not replaced but only “continued” compared to
	% previous imports.
	VirtualTimestamp = ?SCROBBLEBASE - Song#gmbsong.gmbidx * ?SCROBBLESTEP1
							- Nth * ?SCROBBLESTEP2,
	Query = io_lib:format("~s&artist=~s&title=~s&album=~s&time=~w",
			[KeyParam, uri_string:quote(Song#gmbsong.artist),
			uri_string:quote(Song#gmbsong.title),
			uri_string:quote(Song#gmbsong.album), VirtualTimestamp]),
	% https://stackoverflow.com/questions/19103694/simple-example-using-
	{ok, {_HTTPStatus, _Headers, Body}} = httpc:request(post, {Endpoint, [],
			"application/x-www-form-urlencoded", Query}, [], []),
	% https://github.com/davisp/jiffy
	Response = jiffy:decode(Body, [return_maps]),
	case maps:get(<<"status">>, Response) of
	<<"success">> ->
		case maps:get(<<"warnings">>, Response, no_warning) of
		no_warning ->
			{1, 0, 0};
		[OneWarning] ->
			case maps:get(<<"type">>, OneWarning) of
			<<"scrobble_exists">> -> {0, 1, 0};
			_OtherWarning         -> other_warnings(Query, Response)
			end;
		_MultipleWarnings ->
			other_warnings(Query, Response)
		end;
	_Other ->
		io:fwrite("[ ERROR ] Failed to scrobble ~s: ~p~n",
							[Query, Response]),
		{0, 0, 1}
	end.

other_warnings(Query, Response) ->
	io:fwrite("[WARNING] Scrobble may not have succeeded: ~s: ~p~n",
							[Query, Response]),
	{0, 0, 0}.

merge_stats(ScrobbleOutcomes) ->
	lists:foldl(fun({DeltaOK, DeltaOKExists, DeltaFail},
			{OK, OKExists, Fail}) ->
		{OK + DeltaOK, OKExists + DeltaOKExists, Fail + DeltaFail}
	end, {0, 0, 0}, ScrobbleOutcomes).
