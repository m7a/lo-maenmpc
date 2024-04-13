-module(maenmpc_sup).
-behaviour(supervisor).
-export([start_link/1, init/1]).
-define(SERVER, ?MODULE).

start_link(CLIParams) ->
	supervisor:start_link({local, ?SERVER}, ?MODULE, [CLIParams]).

init([_CLIParams]) ->
	{ok, MPDList} = application:get_env(maenmpc, mpd),
	application:start(cecho),
	Len = length(MPDList),
	{ok, {#{strategy => one_for_all, intensity => 0, period => 1}, [
		% TODO CHILD SPECS GO HERE
		#{id => maenmpc_ui, start => {gen_server, start_link,
				[{local, maenmpc_ui}, maenmpc_ui,
				[maenmpc_multiplayer], []]}},
		#{id => maenmpc_input, start => {maenmpc_input, start,
				[maenmpc_ui]}},
		#{id => maenmpc_multiplayer, start => {gen_server, start_link,
				[{local, maenmpc_multiplayer},
				maenmpc_multiplayer, [maenmpc_ui], []]}}
	] ++ lists:flatten([
		generate_dynamic_entries(Name, Config, Idx, Len)
	||
		{{Name, Config}, Idx} <- lists:zip(MPDList,
					lists:seq(1, length(MPDList)))
	])}}.

generate_dynamic_entries(Name, Config, Idx, Len) ->
	NameList     = atom_to_list(Name),
	Singleplayer = list_to_atom("maenmpc_singleplayer_" ++ NameList),
	SyncIdle     = list_to_atom("maenmpc_sync_idle_"    ++ NameList),
	AsyncIdle    = list_to_atom("maenmpc_conn_"         ++ NameList),
	[
		#{
			id      => Singleplayer,
			start   => {gen_server, start_link, [{local,
							Singleplayer},
							maenmpc_singleplayer, [
					{db,       maenmpc_multiplayer},
					{syncidle, SyncIdle},
					{idx,      Idx},
					{len,      Len}
				], []]}
		},
		#{
			id      => SyncIdle,
			start   => {gen_server, start_link, [{local, SyncIdle},
					maenmpc_sync_idle, [Singleplayer], []]}
		},
		#{
			id      => AsyncIdle,
			restart => transient,
			start   => {maenmpc_mpd, start,
						[SyncIdle, Name, Config]}
		}
	].
