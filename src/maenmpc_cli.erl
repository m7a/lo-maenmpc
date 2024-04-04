-module(maenmpc_cli).
-export([run/4]).
-include_lib("stdlib/include/ms_transform.hrl").

-define(SCROBBLEBASE,  1700000000).
-define(SCROBBLESTEP1, 3600).
-define(SCROBBLESTEP2, 240).

% return ok to signal success, {error, "Descr"} for failure, {next, _Params}
% to exec to app
run(MPDList, PrimaryRatings, Maloja, RadioConf) ->
	case init:get_argument(help) of
	{ok, _Any} -> usage();
	_NoHelpArg ->
		case init:get_argument(radio) of
		{ok, SelectedMPD} ->
			case SelectedMPD of
			[[]] ->
				[{MPDName, _Config}|_Rest] = MPDList,
				radio(MPDList, PrimaryRatings, Maloja, MPDName,
					RadioConf);
			[[MPDName]] ->
				radio(MPDList, PrimaryRatings, Maloja,
					list_to_atom(MPDName), RadioConf);
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

% key          := {Artist, Album, Title}
% assoc_status := primary_only, use_only, ok
-record(plsong, {key, rating_uri, use_uri, playcount, rating, assoc_status}).
-type plsong() :: #plsong{key::{binary(), binary(), binary()},
				rating_uri::binary(), use_uri::binary(),
				playcount::integer(), rating::integer(),
				assoc_status::atom()}.

radio(MPDList, PrimaryRatings, Maloja, UseMPD, RadioConf) ->
	io:fwrite("[ INFO  ] radio~n"),
	ets:new(plsongs, [set, named_table, {keypos, #plsong.key}]),
	DefaultRating = maps:get(default_rating, RadioConf),

	io:fwrite("[ INFO  ] query primary~n"),
	ConnPrimary = erlmpd_connect(PrimaryRatings, MPDList),
	EmptyKeys = lists:foldr(fun(Entry, Acc) ->
			PLS = erlmpd_to_plsong(Entry, DefaultRating),
			case PLS#plsong.key == {<<>>, <<>>, <<>>} of
			true ->
				% Not even a warning, because typically it
				% is OK to exclude those files w/o metadata.
				Acc + 1;
			false ->
				case ets:insert_new(plsongs,
					PLS#plsong{assoc_status=primary_only})
				of
				true  -> ok;
				false -> io:fwrite("[WARNING] Duplicate " ++
						"~p at ~s~n", [PLS#plsong.key,
						PLS#plsong.rating_uri])
				end,
				Acc
			end
		end, 0, erlmpd_find_all(ConnPrimary)),
	erlmpd:disconnect(ConnPrimary),
	io:fwrite("[ INFO  ] skipped ~w empty keys~n", [EmptyKeys]),

	io:fwrite("[ INFO  ] query use~n"),
	ConnUse = erlmpd_connect(UseMPD, MPDList),
	lists:foreach(fun(Entry) ->
			PLS = erlmpd_to_plsong(Entry, DefaultRating),
			case PLS#plsong.key == {<<>>, <<>>, <<>>} of
			true -> pass_empty_keys;
			false ->
				case ets:update_element(plsongs, PLS#plsong.key,
						[{#plsong.use_uri,
							PLS#plsong.rating_uri},
						{#plsong.assoc_status, ok}]) of
				true  -> updated_ok;
				false -> % Not found
					io:fwrite("[WARNING] Not found " ++
							"locally: ~p~n",
							[PLS#plsong.key]),
					ets:insert(plsongs, PLS#plsong{
						assoc_status=use_only,
						use_uri=PLS#plsong.rating_uri})
				end
			end
		end, erlmpd_find_all(ConnUse)),
	erlmpd:disconnect(ConnUse),

	io:fwrite("[ INFO  ] associate playcounts~n"),
	URL = proplists:get_value(url, Maloja),
	Key = proplists:get_value(key, Maloja),
	% TODO z if this limit is exceeded need to either adjust Maloja to
	%      do the querying for us or query multiple pages or limit the
	%      time range!
	{ok, {_Status, _Headers, AllScrobblesRaw}} = httpc:request(
			io_lib:format("~s/scrobbles?key=~s&perpage=16777216",
			[URL, uri_string:quote(Key)])),
	lists:foreach(fun scrobble_to_playcount/1, maps:get(<<"list">>,
				jiffy:decode(AllScrobblesRaw, [return_maps]))),

	io:fwrite("[ INFO  ] associate ratings~n"),
	ConnRatings = erlmpd_connect(PrimaryRatings, MPDList),
	RatingsRaw = erlmpd:sticker(ConnRatings, find, "song", "", "rating"),
	erlmpd:disconnect(ConnRatings),
	assign_ratings(RatingsRaw),

	io:fwrite("[ INFO  ] drop primary only...~n"),
	RM = ets:select(plsongs, ets:fun2ms(fun(X) when X#plsong.assoc_status ==
					primary_only -> X#plsong.key end)),
	lists:foreach(fun(K) -> ets:delete(plsongs, K) end, RM),
	io:fwrite("[ INFO  ] ~w items removed from DB~n", [length(RM)]),

	% TODO HIGHLY SPECIFIC FOR NOW / DEVISE SOMETHING ELSE. MAYBE
	%      ALLOW SPECIFYING A MATCH SPEC IN CONFIG OR SOMETHING?
	io:fwrite("[ INFO  ] filter non-radio songs...~n"),
	RM2 = ets:select(plsongs, ets:fun2ms(fun(X) when
			binary_part(X#plsong.rating_uri, {0, 5}) =:= <<"epic/">>
			-> X#plsong.key end)),
	lists:foreach(fun(K) -> ets:delete(plsongs, K) end, RM2),
	io:fwrite("[ INFO  ] ~w items removed from DB~n", [length(RM2)]),
	
	ConnUse2 = erlmpd_connect(UseMPD, MPDList),
	radio_play(ConnUse2, RadioConf),
	erlmpd:disconnect(ConnUse2),

	ets:delete(plsongs),
	ok.

erlmpd_connect(MPDName, MPDList) ->
	HostPort = proplists:get_value(ip,
					proplists:get_value(MPDName, MPDList)),
	{Host, Port} = HostPort,
	{ok, Conn} = erlmpd:connect(Host, Port),
	Conn.

% Query all data (modified-since '0') or (base '')
erlmpd_find_all(Conn) ->
	erlmpd:find(Conn, {base, <<>>}).

erlmpd_to_plsong(Entry, DefaultRating) ->
	#plsong{key=erlmpd_to_key(Entry),
		rating_uri=normalize_always(proplists:get_value(file, Entry)),
		playcount=0,
		rating=DefaultRating}.

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

normalize_strong(Value) ->
	normalize_always(re:replace(normalize_safe(Value), " \\(.*\\)$", "")).

normalize_always(Value) ->
	unicode:characters_to_nfc_binary(Value).

scrobble_to_playcount(Scrobble) ->
	ScrobbleTrack = maps:get(<<"track">>, Scrobble),
	ScrobbleAlbum = maps:get(<<"album">>, ScrobbleTrack),
	TitleRaw = maps:get(<<"title">>, ScrobbleTrack),
	Titles = [normalize_key(TitleRaw), normalize_strong(TitleRaw)],
	Artists = case maps:get(<<"artists">>, ScrobbleTrack) of
		[A1|[A2|[]]] -> [normalize_key([A1, <<" & ">>, A2])|
				[normalize_key([A2, <<" & ">>, A1])|
				[A1|[A2|[]]]]];
		OtherArtists -> OtherArtists
	end,
	Result = lists:foldl(fun({Title, ArtistRaw}, Acc) ->
		case Acc of
		1 -> 1;
		0 ->
			Artist = normalize_key(ArtistRaw),
			case ScrobbleAlbum of
			null ->
				case ets:select(plsongs, ets:fun2ms(fun(X) when
						element(1, X#plsong.key) ==
								Artist andalso
						element(3, X#plsong.key) ==
								Title
					-> element(2, X#plsong.key) end))
				of
				[] ->
					0;
				[AlbumI] ->
					plsongs_inc({Artist, AlbumI, Title});
				[AlbumI|_OtherAlbums] ->
					io:fwrite("[WARNING] Not unique: " ++
						"<~s/~s> -> <~s>~n",
						[Artist, Title, AlbumI]),
					plsongs_inc({Artist, AlbumI, Title})
				end;
			_Other ->
				AlbumTitle = normalize_strong(maps:get(
					<<"albumtitle">>, ScrobbleAlbum)),
				CheckKey = {Artist, AlbumTitle, Title},
				case ets:lookup(plsongs, CheckKey) of
				[]      -> 0;
				[_Item] -> plsongs_inc(CheckKey)
				% other case prevented by set property of table!
				end
			end
		end
	end, 0, [{Title, Artist} || Title <- Titles, Artist <- Artists]),
	case Result of
	1 -> ok;
	0 -> io:fwrite("[WARNING] Skipped: ~p~n", [ScrobbleTrack])
	end.

plsongs_inc(Key) ->
	ets:update_counter(plsongs, Key, {#plsong.playcount, 1}),
	1.

assign_ratings([]) ->
	ok;
assign_ratings([File|[Sticker|Remainder]]) ->
	[_ConstFile|[PathRaw|[]]] = string:split(File, ": "),
	[_ConstSticker|[Stickers|[]]] = string:split(Sticker, ": "),
	Path = normalize_always(PathRaw),
	DBKey = case ets:select(plsongs, ets:fun2ms(fun(X) when
				X#plsong.rating_uri == Path -> X#plsong.key
			end)) of
		[] -> io:fwrite("[WARNING] Not in DB: ~p~n", [Path]);
		[UniqueKey] -> UniqueKey;
		MultipleResults -> io:fwrite("[WARNING] Rating not unique: " ++
			"~s:~n  ~p. Skipped...~n", [Path, MultipleResults])
		end,
	% TODO GENERIC VERSION CAN BE FOUND IN DB sticker_line_to_rating FUNCTION!
	Rating = lists:foldl(fun(KV, Acc) ->
			[Key|[Value|[]]] = string:split(KV, "="),
			case Key == "rating" of
			true ->
				% Found rating, compute * 10 with 10 map to 0.
				case list_to_integer(Value) of
				1      -> 0;
				NotOne -> NotOne * 10
				end;
			% Skip this sticker
			false -> Acc
			end
		end, nothing, string:split(Stickers, " ")),
	case {DBKey, Rating} of
	{ok,       _AnyRating}  -> skip;
	{_AnyKey,  nothing}     -> skip;
	{_GoodKey, _GoodRating} -> ets:update_element(plsongs, DBKey,
						{#plsong.rating, Rating})
	end,
	assign_ratings(Remainder).

radio_play(ConnUse, RadioConf) ->
	io:fwrite("[ INFO  ] Compute schedule...~n"),
	Schedule = schedule_compute(RadioConf),
	[H|_T] = Schedule,
	radio_enqueue(ConnUse, H),
	radio_play(ConnUse, RadioConf, Schedule).

radio_enqueue(ConnUse, SongKey) ->
	[Value] = ets:lookup(plsongs, SongKey),
	URI = binary_to_list(Value#plsong.use_uri),
	ok = erlmpd:add(ConnUse, URI).

radio_play(ConnUse, RadioConf, [Await2|[]]) ->
	radio_play(ConnUse, RadioConf, [Await2|schedule_compute(RadioConf)]);
radio_play(ConnUse, RadioConf, Schedule) ->
	[Await|TSched] = Schedule,
	case erlmpd:idle(ConnUse, [player]) of
	[player] ->
		% Could be relevant
		CurrentSong = erlmpd_to_key(erlmpd:currentsong(ConnUse)),
		case CurrentSong =:= Await of
		true ->
			[Next|_Others] = TSched,
			plsongs_inc(CurrentSong),
			io:fwrite("FOUND ~s - ~s :: ENQ ~s - ~s~n",
					[element(1, CurrentSong),
					element(3, CurrentSong),
					element(1, Next), element(3, Next)]),
			radio_enqueue(ConnUse, Next),
			radio_play(ConnUse, RadioConf, TSched);
		false ->
			radio_play(ConnUse, RadioConf, Schedule)
		end;
	_Other ->
		% ignore
		radio_play(ConnUse, RadioConf, Schedule)
	end.

% Compute a Music Schedule according to the following algorithm:
% Partition songs by rating, drop all 1-star rated songs, shuffle the per-rating
% lists, sort them by play count ASC, ensure that 4+5 stars are at least 30%
% (if not, repeat them as necessary) and then interleave the lists as to produce
% a fair playlist with enough good songs. Ordering by play count ensures that
% repeated execution of the same algorithm always yields diverse playlists.
schedule_compute(Conf) ->
	MinGoodPerc   = maps:get(min_good_perc,  Conf),
	ChaosFactor   = maps:get(chaos_factor,   Conf),
	InitialFactor = maps:get(initial_factor, Conf),
	ScheduleLen   = maps:get(schedule_len,   Conf),
	% unclear why select_count did not return the intended output here?
	Count1 = length(ets:select(plsongs, schedule_construct_match(0))),
	Group2 = ets:select(plsongs, schedule_construct_match(20)),
	Group3 = ets:select(plsongs, schedule_construct_match(40)),
	Group4 = ets:select(plsongs, schedule_construct_match(60)),
	Group5 = ets:select(plsongs, schedule_construct_match(80)),
	Count2 = length(Group2),
	Count3 = length(Group3),
	Count4 = length(Group4),
	Count5 = length(Group5),
	CountT = Count2 + Count3 + Count4 + Count5,
	Perce2 = Count2 * 100 / CountT,
	Perce3 = Count3 * 100 / CountT,
	Perce4 = Count4 * 100 / CountT,
	Perce5 = Count5 * 100 / CountT,
	io:fwrite("Distribution of stars~n 1 Star  ~5w (ignore)~n" ++
			" 2 Stars ~5w (~5.2f%)~n 3 Stars ~5w (~5.2f%)~n" ++
			" 4 Stars ~5w (~5.2f%)~n 5 Stars ~5w (~5.2f%)~n",
			[Count1, Count2, Perce2, Count3, Perce3,
			Count4, Perce4, Count5, Perce5]),
	Duplicate = case Perce4 + Perce5 < MinGoodPerc of
			true  -> trunc(MinGoodPerc / (Perce4 + Perce5));
			false -> 1
			end,
	io:fwrite("Duplicate = ~p~n", [Duplicate]),
	Schedule = schedule_merge([
		schedule_shuffle(ChaosFactor, Group5, Duplicate),
		schedule_shuffle(ChaosFactor, Group4, Duplicate),
		schedule_shuffle(ChaosFactor, Group3, 1),
		schedule_shuffle(ChaosFactor, Group2, 1)
	], ScheduleLen, InitialFactor),
	io:fwrite("Schedule of ~w songs:~n", [length(Schedule)]),
	lists:foreach(fun(ID) ->
			[Entry] = ets:lookup(plsongs, ID),
			io:fwrite("R~3w C~4w ~s - ~s~n", [Entry#plsong.rating,
				Entry#plsong.playcount,
				element(1, Entry#plsong.key),
				element(3, Entry#plsong.key)])
		end, Schedule),
	Schedule.

schedule_construct_match(Rating) ->
	ets:fun2ms(fun(X) when X#plsong.rating > Rating andalso
				X#plsong.rating =< (Rating + 20) -> X end).

% https://stackoverflow.com/questions/8817171/shuffling-elements-in-a-list-
schedule_shuffle(ChaosFactor, Group, Duplicate) ->
	lists:flatten(lists:map(fun(_Ctr) ->
		[Y || {_, Y} <- lists:sort([{rand:uniform() * ChaosFactor +
			S#plsong.playcount, S#plsong.key} || S <- Group])]
	end, lists:seq(1, Duplicate))).

schedule_merge(Groups, Limit, InitialFactor) ->
	NonEmptyGroups = lists:filter(fun (X) -> X /= [] end, Groups),
	schedule_merge_annotated([], Limit, lists:zipwith(fun(Group, ID) ->
			LGroup = length(Group),
			% Perc (“0.0”),         ID, Num, Of,     Group
			{ InitialFactor/LGroup, ID, 0,   LGroup, Group }
		end, NonEmptyGroups, lists:seq(1, length(NonEmptyGroups)))).

schedule_merge_annotated(Schedule, _Limit, []) ->
	lists:reverse(Schedule);
schedule_merge_annotated(Schedule, Limit, _AnnotatedGroups)
					when length(Schedule) >= Limit ->
	lists:reverse(Schedule);
schedule_merge_annotated(Schedule, Limit, AnnotatedGroups) ->
	{_Perc, SelID, Num, Of, [SelItem|SelRem]} = lists:min(AnnotatedGroups),
	Others = lists:filter(fun({_Perc2, ID, _Num, _Of, _Group}) ->
					ID /= SelID end, AnnotatedGroups),
	NewNum = Num + 1,
	NewPerc = NewNum / Of,
	case NewNum < Of of
	true  -> schedule_merge_annotated([SelItem|Schedule], Limit,
			[{NewPerc, SelID, NewNum, Of, SelRem}|Others]);
	false -> schedule_merge_annotated([SelItem|Schedule], Limit, Others)
	end.

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
	[Result] = erlmpd:find(Conn, {land, [
				{tagop, artist, eq, Song#gmbsong.artist},
				{tagop, album,  eq, Song#gmbsong.album},
				{tagop, title,  eq, Song#gmbsong.title}]}),
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
