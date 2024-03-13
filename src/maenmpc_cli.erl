-module(maenmpc_cli).
-export([run/0]).

-record(song, {gmbidx, path, artist, album, title, year, lastplay, playcount,
		rating}).

% return ok to signal success, {error, "Descr"} for failure, {next, _Params}
% to exec to app
run() ->
	case init:get_argument(help) of
	{ok, _Any} -> usage();
	_NoHelpArg ->
		case init:get_argument(gmbrc) of
		{ok, [[Path]]} -> use_gmbrc(Path);
		_NoGMBRCArg    -> {next, []}
		end
	end.

usage() ->
	io:fwrite(
"USAGE $0 foreground                          -- run regularly~n" ++
"USAGE $0 foregorund -help                    -- this page~n" ++
"USAGE $0 foregorund -mpdsticker -gmbrc GMBRC -- import ratings~n" ++
"USAGE $0 foreground -scrobble   -gmbrc GMBRC -- import playcounts~n"
	).

use_gmbrc(Path) ->
	%database_read(Path), % -- TODO
	RV = case init:get_argument(mpdsticker) of
	{ok, _Any}       -> import_ratings_to_stickers();
	_NoMPDStickerArg ->
		case init:get_argument(scrobble) of
		{ok, _Any}     -> import_playcounts_to_scrobbles();
		_NoScrobbleArg -> {next, []}
		end
	end,
	%ets:delete(gmb_songs), % TODO
	RV.

%------------------------------------------------------------[ GMBRC Parsing ]--
database_read(GMBRC) ->
	io:fwrite("Read GMBRC... "),
	{ok, RawDataBinary} = file:read_file(GMBRC),
	{await_eof, Lines, _Keys} = lists:foldl(fun database_line/2,
			{await_marker, invalid, invalid},
			binary:split(RawDataBinary, <<"\n">>, [global, trim])),
	ets:new(gmb_songs, [set, named_table, {keypos, #song.gmbidx}]),
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
		ets:insert(gmb_songs, #song{
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
				Value -> min(1, binary_to_integer(Value))
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
import_ratings_to_stickers() ->
	io:fwrite("import_ratings_to_stickers~n").

import_playcounts_to_scrobbles() ->
	io:fwrite("import_ratings_to_scrobbles~n"),
	% TODO TEST HERE
	{ok, Conn} = erlmpd:connect("172.17.0.1", 6600),
	% TODO X MOST INTERESTING IS mpd config because it indicates music_directory but this CMD is only available for local socket file clients???
	% STILL, THIS SHOULD BE THE WAY TO GO (WOULD THEN SUGGEST TO CONNECT THROUGH LOCAL ....... -> implement `config` from Reflection submenu which may return up to  three key/values . Issue is also that client seems to only connect through TCP... analyzing...
	io:fwrite("Status = <~p>~n", [erlmpd:status(Conn)]),
	% TODO END TEST
	ok.
