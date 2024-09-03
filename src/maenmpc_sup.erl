-module(maenmpc_sup).
-behaviour(supervisor).
-export([start_link/1, init/1]).
-define(SERVER, ?MODULE).

start_link(CLIParams) ->
	supervisor:start_link({local, ?SERVER}, ?MODULE, [CLIParams]).

init([CLIParams]) ->
	case proplists:get_value(services, CLIParams) of
	[]       -> init_gui();
	Services -> init_noninteractively(
			proplists:get_value(server, CLIParams), Services)
	end.

init_gui() ->
	{ok, MPDList}        = application:get_env(maenmpc, mpd),
	{ok, PrimaryRatings} = application:get_env(maenmpc, primary_ratings),
	application:start(cecho),
	Len = length(MPDList),
	{ok, {#{strategy => one_for_all, intensity => 0, period => 1}, [
		#{id => maenmpc_ui, start => {gen_server, start_link,
				[{local, maenmpc_ui}, maenmpc_ui,
				[maenmpc_multiplayer], []]}},
		#{id => maenmpc_input, start => {maenmpc_input, start,
				[maenmpc_ui]}},
		server_entry(maenmpc_radio,    maenmpc_multiplayer),
		server_entry(maenmpc_podcast,  maenmpc_multiplayer),
		server_entry(maenmpc_scrobble, maenmpc_multiplayer),
		#{id => maenmpc_multiplayer, start => {gen_server, start_link,
				[{local, maenmpc_multiplayer},
				maenmpc_multiplayer,
				[maenmpc_ui, maenmpc_radio, maenmpc_scrobble],
				[]]}}
	] ++ lists:flatten([
		generate_dynamic_entries(Name, Config, Idx, Len, PrimaryRatings)
	||
		{{Name, Config}, Idx} <- lists:zip(MPDList,
					lists:seq(1, length(MPDList)))
	])}}.

server_entry(Name, ReportTo) ->
	#{id => Name, start => {gen_server, start_link, [{local, Name},
							Name, [ReportTo], []]}}.

generate_dynamic_entries(Name, Config, Idx, Len, Ratings) ->
	NameList     = atom_to_list(Name),
	Singleplayer = list_to_atom("maenmpc_singleplayer_" ++ NameList),
	SyncIdle     = list_to_atom("maenmpc_sync_idle_"    ++ NameList),
	AsyncIdle    = list_to_atom("maenmpc_conn_"         ++ NameList),
	[#{
			id      => Singleplayer,
			start   => {gen_server, start_link, [{local,
							Singleplayer},
							maenmpc_singleplayer, [
					{db,       maenmpc_multiplayer},
					{syncidle, SyncIdle},
					{idx,      Idx},
					{len,      Len},
					{rating,   Name =:= Ratings}
				], []]}
	}|low_level_entries(Name, SyncIdle, Singleplayer, AsyncIdle, Config)].

low_level_entries(Name, SyncIdle, Singleplayer, AsyncIdle, Config) ->
	[#{
		id      => SyncIdle,
		start   => {gen_statem, start_link, [{local, SyncIdle},
				maenmpc_sync_idle, [Singleplayer], []]}
	}, #{
		id      => AsyncIdle,
		restart => transient,
		start   => {gen_server, start_link, [{local, AsyncIdle},
				maenmpc_mpd, [SyncIdle, Name, Config], []]}
	}].

init_noninteractively(MPDServer, Services) ->
	{ok, MPDList} = application:get_env(maenmpc, mpd),
	NotifyUnits = unit_if_enabled(radio, maenmpc_radio, Services) ++
			unit_if_enabled(scrobble, maenmpc_scrobble, Services),
	EnableUnits = unit_if_enabled(podcast, maenmpc_podcast, Services),

	{ok, {
		#{strategy => one_for_all, intensity => 0, period => 1},
		[server_entry(Name, maenmpc_cli_server) ||
					Name <- NotifyUnits ++ EnableUnits] ++
		[#{id => maenmpc_cli_server, start => {gen_server, start_link,
			[{local, maenmpc_cli_server}, maenmpc_cli_server,
			[{server, MPDServer}, {notify_units, NotifyUnits}],
			[]]}}] ++
		low_level_entries(primary, maenmpc_sync_idle_primary,
				maenmpc_cli_server, maenmpc_conn_primary,
				proplists:get_value(MPDServer, MPDList))
	}}.

unit_if_enabled(Key, Unit, Services) ->
	case lists:member(Key, Services) of
	true  -> [Unit];
	false -> []
	end.
