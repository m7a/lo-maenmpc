-module(maenmpc_cli_server).
-behavior(gen_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, code_change/3]).
-include_lib("maenmpc_db.hrl").

-record(cli, {idx, len, notify}).

init(Props) ->
	MPDUse        = proplists:get_value(server,       Props),
	NotifyUnits   = proplists:get_value(notify_units, Props),
	{ok, MPDList} = application:get_env(maenmpc, mpd),
	MPDListIdx    = [{Name, Idx} || {{Name, _ConnInfoI}, Idx} <-
			lists:zip(MPDList, lists:seq(1, length(MPDList)))],
	lists:foreach(fun(Target) ->
			ok = gen_server:cast(Target, {service_start, MPDUse})
		end, NotifyUnits),
	log(info, io_lib:format("maenmpc_cli_server -- ~w", [NotifyUnits])),
	timer:send_interval(5000, interrupt_idle),
	{ok, #cli{
		idx    = proplists:get_value(MPDUse, MPDListIdx),
		len    = length(MPDList),
		notify = NotifyUnits
	}}.

handle_call(_Call, _From, Ctx) ->
	{reply, ok, Ctx}.

handle_cast({mpd_idle, _Name, _Subsystems}, Ctx) ->
	{noreply, maenmpc_sync_idle:run_transaction(maenmpc_sync_idle_primary,
								fun(Conn) ->
		Status = erlmpd:status(Conn),
		CurrentSong = parse_metadata(erlmpd:currentsong(Conn), Ctx),
		Msg = {db_playing, [{x_maenmpc, CurrentSong}|Status]},
		lists:foreach(fun(Target) ->
				ok = gen_server:cast(Target, Msg)
			end, Ctx#cli.notify),
		Ctx
	end)};
handle_cast({mpd_assign_error, _MPDName, Reason}, Ctx) ->
	log(error, io_lib:format("mpd_assign_error ~w~n", [Reason])),
	{noreply, Ctx};
handle_cast({radio_enqueue, DBSong}, Ctx) ->
	{noreply, maenmpc_sync_idle:run_transaction(maenmpc_sync_idle_primary,
								fun(Conn) ->
		log(info, io_lib:format("enqueue song ~s/~s/~s",
					[element(1, DBSong#dbsong.key),
					element(2, DBSong#dbsong.key),
					element(3, DBSong#dbsong.key)])),
		ok = erlmpd:add(Conn, element(Ctx#cli.idx, DBSong#dbsong.uris)),
		Ctx
	end)};
handle_cast(Log=#dblog{}, Ctx) ->
	print_log(Log),
	{noreply, Ctx};
handle_cast(_Cast, Ctx) ->
	{noreply, Ctx}.

parse_metadata({error, Descr}, Ctx) ->
	log(error, io_lib:format("parse metadata error ~s", [Descr])),
	maenmpc_erlmpd:epsilon_song(Ctx#cli.len);
parse_metadata(CurrentSong, Ctx) ->
	case proplists:get_value(file, CurrentSong) of
	undefined -> maenmpc_erlmpd:epsilon_song(Ctx#cli.len);
	_ValidVal -> maenmpc_erlmpd:to_dbsong(CurrentSong, Ctx#cli.idx,
								Ctx#cli.len)
	end.

log(Type, Log) ->
	{{Y,M,D}, {H,I,S}} = erlang:universaltime(),
	Time = io_lib:format("~w-~2..0w-~2..0wT~2..0w:~2..0w:~2..0wZ",
							[Y, M, D, H, I, S]),
	print_log(#dblog{msg=Log, timestamp=Time, origin=Type}).

print_log(#dblog{msg=Msg, timestamp=T, origin=Origin}) ->
	OriginCode =
		case Origin of
		error    -> "EEE";
		info     -> "iii";
		radio    -> "R  ";
		podcast  -> " p ";
		scrobble -> "  S";
		_Other   -> "___"
		end,
	io:fwrite("~s ~s ~s~n", [T, OriginCode, Msg]).

handle_info(interrupt_idle, Ctx) ->
	maenmpc_sync_idle:interrupt_no_tx(maenmpc_sync_idle_primary),
	{noreply, Ctx};
handle_info(_Message, Ctx) ->
	{noreply, Ctx}.

code_change(_OldVersion, Ctx, _Extra) -> {ok, Ctx}.
