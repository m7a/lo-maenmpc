#!/usr/bin/env escript
%% -*- erlang -*-
-mode(compile).

% Ma_Sys.ma Experimental Podcast Local/External/Single script
% (c) 2024 Ma_Sys.ma <info@masysma.net>
%
% This is intended to be an intermediate solution to podcast playback in
% conjunction with MAENMPC. It makes use of a few MAENMPC components already
% but is intended to run on a single node using an external podcast application
% and storing its results in local files.
%
% If you want to make use of this script yourself, you may need to adjust
% certain configuration variables in the script source code!
%
% Code is derived form gmusicradio and run_podget.sh, see
% <https://masysma.net/32/gmusicradio.xhtml>

%-------------------------------------------------------------------[ config ]--
podcast_conf() ->
	#{
		conf     => "/data/programs/music2/supplementary/news/conf",
		dir      => "/data/programs/music2/supplementary/news/pod",
		timeout  => 50000,
		interval => 300000
	}.

% [{local, [{ip, {"172.17.0.1", 6600}}]},
%  {m16,   [{ip, {"172.17.0.1", 6601}}]}]
maenmpc_mpd_list_conf() ->
	{ok, [Conf]} = file:consult("config/sys.config"),
	proplists:get_value(mpd, proplists:get_value(maenmpc, Conf)).

% -------------------------------------------[ customized special processing ]--
use_player(m16, Host, Port, File, Timeout) ->
	Tmp1 = "/tmp/podcast_pre.flac",
	run_process_require_success(["ffmpeg", "-loglevel", "error", "-i",
						File, Tmp1], Timeout, fun() ->
		Tmp2 = "/tmp/podcast.flac",
		run_process_require_success(["ReSampler", "-i", Tmp1,
						"-o", Tmp2, "-r", "96000",
						"-b", "24"], Timeout, fun() ->
			run_process_require_success(["loudgain", "-r", "-k",
					"-s", "e", Tmp2], Timeout, fun() ->
				ok = file:delete(Tmp1),
				{ok, Data} = file:read_file(Tmp2),
				ok = file:delete(Tmp2),
				upload_m16_and_play(Host, Port, Data)
			end)
		end)
	end);
use_player(_Name, Host, Port, File, Timeout) ->
	Target = "/data/programs/music2/track/x_podcast/podcast.mp3",
	{ok, _Bytes} = file:copy(File, Target),
	run_process_require_success(["loudgain", "-I3", "-S", "-L", "-r", "-k",
					"-s", "e", Target], Timeout, fun() ->
		mpd_enqueue(Host, Port, "track/x_podcast/podcast.mp3")
	end).

upload_m16_and_play(Host, Port, Data) ->
	io:fwrite("  upload to server...~n"),
	{ok, Pid, Conn} = sftp_connect_m16(),
	ok = ssh_sftp:write_file(Pid,
		"/var/lib/mpd/music/track/x_podcast/podcast.flac",
		Data),
	ok = ssh_sftp:stop_channel(Pid),
	ok = ssh:close(Conn),
	mpd_enqueue(Host, Port, "track/x_podcast/podcast.flac").

% Debug SSH connection as follows:
% -> First time need to do it manually with user interaction to populate
%    known_hosts. it uses a format that does not seem to be equal to what
%    OpenSSH writes...
% -> https://stackoverflow.com/questions/51700327
% -> https://elixirforum.com/t/how-can-you-debug-an-ssh-connection/60748
% -> https://github.com/erlang/otp/blob/922ef22d58ae5232fcb2a44776d9879e8433d71d/lib/ssh/src/ssh_dbg.erl
% ssh:start().
% ssh:shell("192.168.1.22", 22, [{user, "mpd"}, {user_interaction, false}, {user_dir, "/data/main/101_administrative/60t69_keys/65_machines/101.65.17_masysma-16/userdir"}]).
sftp_connect_m16() ->
	ssh_sftp:start_channel("192.168.1.22", 22, [
		{user, "mpd"},
		{user_interaction, false},
		{user_dir, "/data/main/101_administrative/60t69_keys/" ++
				"65_machines/101.65.17_masysma-16/userdir"}
	]).

% ---------------------------------------------------------------------[ run ]--
main([]) ->
	run(podcast_conf(), maenmpc_mpd_list_conf()).

run(PodcastConf, MPDList) ->
	ok = ssh:start(),
	loop(podcast_init(PodcastConf, MPDList),
					maps:get(interval, PodcastConf)).

loop(PC, Interval) ->
	timer:sleep(Interval),
	io:fwrite("CHECK~n"),
	loop(podcast_process(PC), Interval).

% -----------------------------------------[ podcast processing + subprocess ]--
podcast_init(Conf, MPDList) ->
	{Conf, MPDList, podcast_run_inner(Conf)}.

podcast_run_inner(Conf) ->
	{ok, _Cnt} = subprocess_run_await(["podget", "-d",
			maps:get(conf, Conf)], maps:get(timeout, Conf), error),
	lists:sort(filelib:wildcard(maps:get(dir, Conf) ++ "/**/*.mp3")).

podcast_process({Conf, MPDList, OldState}) ->
	NewState = podcast_run_inner(Conf),
	case NewState -- OldState of
	[] ->
		ok; % do nothing
	NewFiles ->
		PlayFile = lists:last(NewFiles),
		play(MPDList, PlayFile, maps:get(timeout, Conf))
	end,
	{Conf, MPDList, NewState}.

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

% --------------------------------------------------------------------[ Play ]--
play(MPDList, File, Timeout) ->
	io:fwrite("PODCAST ~s~n", [File]),
	lists:foldl(fun ({Name, [{ip, {Host, Port}}]}, State) ->
		case State of
		none -> try_player(Name, Host, Port, File, Timeout);
		ok   -> ok
		end
	end, none, MPDList).

try_player(Name, Host, Port, File, Timeout) ->
	case erlmpd:connect(Host, Port) of
	{ok, Conn1} ->
		case erlmpd:status(Conn1) of
		{error, E} ->
			io:fwrite("  status skip ~w (ignore ~p)~n", [Name, E]);
		List ->
			case proplists:get_value(state, List, none) of
			play ->
				io:fwrite("  ~w is active!~n", [Name]),
				erlmpd:close(Conn1),
				use_player(Name, Host, Port, File, Timeout);
			Status ->
				io:fwrite("  skip ~w for wrong status: ~w~n",
								[Name, Status]),
				none
			end
		end;
	Error ->
		io:fwrite("  connect skip ~w (ignore ~p)~n", [Name, Error]),
		none
	end.

run_process_require_success(Cmd, Timeout, Continue) ->
	io:fwrite("  run ~p~n", [Cmd]),
	case subprocess_run_await(Cmd, Timeout, error) of
	{ok, _Output} ->
		Continue();
	Other ->
		io:fwrite("  ! FAILED: ~p~n", [Other]),
		% unexpected error, it does not help to try with other player!
		ok
	end.

mpd_enqueue(Host, Port, Name) ->
	io:fwrite("  enqueue ~s...~n", [Name]),
	{ok, Conn2} =  erlmpd:connect(Host, Port),
	case erlmpd:addid_relative(Conn2, Name, 0) of
	{error, Err} -> io:fwrite("  ! FAILED: ~p~n", [Err]);
	_Other       -> ok
	end,
	erlmpd:close(Conn2).
