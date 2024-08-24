-module(maenmpc_podcast).
-behavior(gen_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, code_change/3]).
-include_lib("maenmpc_db.hrl").

-record(rp, {mpd_list, talk_to, config, active, filelist}).

init([TalkTo]) ->
	{ok, Config} = application:get_env(maenmpc, podcast),
	timer:send_interval(maps:get(interval, Config), interrupt_check),
	{ok, #rp{
		% static wiring
		mpd_list = application:get_env(maenmpc, mpd),
		talk_to  = TalkTo,
		config   = Config,
		% dynamic state
		active   = false,
		filelist = []
	}}.

% no calls defined
handle_call(_Call, _From, Ctx) ->
	{ok, Ctx}.

% on start establish baseline, on stop just remove active marker
handle_cast({service_start, _MPDName}, Ctx) ->
	Ctx1 = Ctx#rp{active = true},
	log(Ctx1, "initializing..."),
	Ctx2 = podcast_run_inner(Ctx1),
	log(Ctx2, "startup completed"),
	{noreply, Ctx2};
handle_cast(service_stop, Ctx) ->
	{noreply, Ctx#rp{active = false}};
handle_cast(_Other, Ctx) ->
	{noreply, Ctx}.

% Ctx is RO
log(Ctx, Msg) ->
	maenmpc_svc:log(Ctx#rp.talk_to, #dblog{msg=Msg, origin=podcast}).

podcast_run_inner(Ctx=#rp{config=Conf}) ->
	case subprocess_run_await(["podget", "-d", maps:get(conf, Conf)],
					maps:get(timeout, Conf), error) of
	{ok, _Cnt} ->
		Ctx#rp{filelist =
			lists:sort(filelib:wildcard(maps:get(glob, Conf)))};
	{timeout, Lines} ->
		lists:foreach(fun(L) -> log(Ctx, io_lib:format("[timeout] ~s",
							[L])) end, Lines),
		Ctx;
	{error, Token, Error} ->
		log_multiline(Ctx, "error", {Token, Error}),
		Ctx
	end.

% CTX is RO
subprocess_run_await([ExecutableName|Args], Timeout, TimeoutAction) ->
	Port = open_port({spawn_executable, os:find_executable(ExecutableName)},
			[{args, Args}, stream, exit_status,
					use_stdio, stderr_to_stdout, in]),
	{ok, Timer} = timer:send_after(Timeout, {Port,
						{timeout, TimeoutAction}}),
	subprocess_get_data(Timer, Port, []).

subprocess_get_data(Timer, Port, Acc) ->
	receive
	{Port, {timeout, detach}} ->
		{timeout, lists:reverse(Acc)};
	{Port, {timeout, error}} ->
		port_close(Port),
		{error, timeout, lists:reverse(Acc)};
	{Port, {data, D}} ->
		subprocess_get_data(Timer, Port, [D|Acc]);
	{Port, {exit_status, RC}} ->
		% Race Condition: If the timer fires just after the exit status
		% but before we can cancel it here, then the message
		% {timeout, ...} is going to be ignored and processed upon
		% the next subprocess interaction (see subprocess_run_await)
		timer:cancel(Timer),
		case RC == 0 of
		true  -> {ok, lists:reverse(Acc)};
		false -> {error, RC, lists:reverse(Acc)}
		end;
	% Since we can have detached processes it may be the case that some
	% other process is still sending us messages. e.g. if its timer was
	% not cancelled correctly due to race condition or if the program was
	% detached to background and is now printing stuff to console or
	% exiting. This block catches all of these instances and drops their
	% data.
	{_Other, {timeout, _Setting}} ->
		subprocess_get_data(Timer, Port, Acc);
	{_Other, {data, _Data}} ->
		subprocess_get_data(Timer, Port, Acc);
	{_Other, {exit_status, _RC}} ->
		subprocess_get_data(Timer, Port, Acc)
	end.

log_multiline(Ctx, Prefix, Value) ->
	lists:foreach(fun(L) -> log(Ctx, L) end, string:split(
			io_lib:format("~s: ~p", [Prefix, Value]), "\n")).

handle_info(interrupt_check, Ctx) when Ctx#rp.active =:= true ->
	log(Ctx, "check..."),
	{noreply, podcast_process(Ctx)};
handle_info(_Message, Ctx) ->
	{noreply, Ctx}.

podcast_process(Ctx=#rp{filelist=OldState}) ->
	NewState = podcast_run_inner(Ctx),
	case NewState -- OldState of
	[]       -> Ctx; % do nothing
	NewFiles -> play(lists:last(NewFiles), Ctx#rp{filelist=NewState})
	end.

% In theory we could also query multiplayer about the active player. However,
% this would lock us out of offering this functionality as a dedicated instance
% without a “multiplayer”.
play(File, Ctx = #rp{mpd_list=MPDList}) ->
	log(Ctx, io_lib:format("new episode ~s~n", [File])),
	lists:foldl(fun ({Name, [{ip, {Host, Port}}]}, State) ->
		case State of
		none -> try_player(Name, Host, Port, File, Ctx);
		ok   -> ok
		end
	end, none, MPDList),
	Ctx.

try_player(Name, Host, Port, File, Ctx) ->
	case erlmpd:connect(Host, Port) of
	{ok, Conn1} ->
		case erlmpd:status(Conn1) of
		{error, E} ->
			log(Ctx, io_lib:format("skip ~w (ignore ~w)",
								[Name, E])),
			erlmpd:close(Conn1),
			none;
		List ->
			case proplists:get_value(state, List, none) of
			play ->
				log(Ctx, io_lib:format("~w is active", [Name])),
				erlmpd:close(Conn1),
				use_player(Name, Host, Port, File, Ctx);
			Status ->
				log(Ctx, io_lib:format(
						"skip ~w for wrong status: ~w",
						[Name, Status])),
				erlmpd:close(Conn1),
				none
			end
		end;
	Error ->
		log(Ctx, io_lib:format("connect skip ~w (ignore ~w)",
								[Name, Error])),
		none
	end.

% -- Begin Continuation passing Style --
% In these routines, CTX is always a read-only input

% 1 Check if we have resampling
use_player(Name, Host, Port, File, Ctx = #rp{config = Config}) ->
	% Patch connectivity information into podcast config (we pass it all
	% down to play_inner).
	PodcastConfig = [{ip, Host, Port}|proplists:get_value(Name,
						maps:get(mpd, Config))],
	case proplists:get_value(samplerate, PodcastConfig) of
	undefined -> loudgain_then_cont(File, PodcastConfig, Ctx);
	_Rate     -> to_flac_then_cont (File, PodcastConfig, Ctx)
	end.

% 1.1 Check if input file /= flac -> ffmpeg
to_flac_then_cont(File, PodcastConfig, Ctx) ->
	case lists:suffix(".flac", File) of
	true ->
		Tmp1 = tmpnam("pre.flac"),
		run_process_require_success(["ffmpeg", "-loglevel", "error",
						"-i", File, Tmp1], Ctx, fun() ->
			RV = resample_then_cont(Tmp1, PodcastConfig, Ctx),
			ok = file:delete(Tmp1),
			RV
		end);
	false ->
		resample_then_cont(File, PodcastConfig, Ctx)
	end.

run_process_require_success(Cmd, Ctx, Continue) ->
	log_multiline(Ctx, "run", Cmd),
	case subprocess_run_await(Cmd, maps:get(timeout, Ctx#rp.config),
								error) of
	{ok, _Output} -> Continue();
	% unexpected error, it does not help to try with other player thus "ok"
	Other -> log_multiline(Ctx, "failed", Other), ok
	end.

tmpnam(Suffix) ->
	"/tmp/podcast_" ++ Suffix. % TODO x maybe do something smarter?

% 1.2 resample
resample_then_cont(File, PodcastConfig, Ctx) ->
	SampleRate = proplists:get_value(samplerate, PodcastConfig),
	Tmp2 = tmpnam("resampled.flac"),
	run_process_require_success(["ReSampler", "-i", File, "-o", Tmp2, "-r",
			integer_to_list(SampleRate), "-b", "24"], Ctx, fun() ->
		RV = loudgain_then_cont(Tmp2, PodcastConfig, Ctx),
		ok = file:delete(Tmp2),
		RV
	end).

% 2 Loudgain depending on current output extension
loudgain_then_cont(File, PodcastConfig, Ctx) ->
	CMD = case lists:suffix(".flac", File) of
		true  -> ["loudgain", "-r", "-k", "-s", "e", File];
		false ->
			case lists:suffix(".mp3", File) of
			true  -> ["loudgain", "-I3", "-S", "-L",
						"-r", "-k", "-s", "e", File];
			false -> fail
			end
		end,
	case CMD of
	fail ->
		log(Ctx, io_lib:format("Skip replay gain for unknown extension "
							++ "~s", [File])),
		transfer_then_cont(File, PodcastConfig, Ctx);
	_RealCMD ->
		run_process_require_success(CMD, Ctx, fun() ->
			transfer_then_cont(File, PodcastConfig, Ctx)
		end)
	end.

transfer_then_cont(File, PodcastConfig, Ctx) ->
	TargetFS = proplists:get_value(target_fs, PodcastConfig),
	try
		case proplists:get_value(ssh, PodcastConfig) of
		undefined ->
			% No SSH, continue by simple copy
			{ok, _Bytes} = file:copy(File, TargetFS);
		SSH ->
			{Host, Port} = proplists:get_value(ip, SSH),
			FilteredList = lists:keydelete(ip, 1, SSH),
			% read file into DRAM
			{ok, Data} = file:read_file(File),
			% sftp connect
			{ok, Pid, Conn} = ssh_sftp:start_channel(Host, Port,
					[{user_interaction, false}|FilteredList]),
			% copy data
			ok = ssh_sftp:write_file(Pid, TargetFS, Data),
			% disconnect
			ok = ssh_sftp:stop_channel(Pid),
			ok = ssh:close(Conn)
		end
	of _AnyOK ->
		mpd_enqueue_then_fin(PodcastConfig, Ctx)
	catch Throwable ->
		log_multiline(Ctx, "transfer exception ~p", [Throwable]),
		ok
	end.

% -- End Continuation Passing Style --

mpd_enqueue_then_fin(PodcastConfig, Ctx) ->
	TargetMPD    = proplists:get_value(target_mpd, PodcastConfig),
	{Host, Port} = proplists:get_value(ip, PodcastConfig),
	log("enqueue ~s...", [TargetMPD]),
	{ok, Conn2} = erlmpd:connect(Host, Port),
	case erlmpd:addid_relative(Conn2, TargetMPD, 0) of
		{error, Err} -> log_multiline(Ctx, "enqueue error", Err), ok;
		_Other       -> ok
	end.

code_change(_OldVersion, Ctx, _Extra) -> {ok, Ctx}.
