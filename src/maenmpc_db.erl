-module(maenmpc_db).
-behavior(gen_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, code_change/3]).

-record(db, {ui, mpd_map, mpd_active}).

init([NotifyToUI]) ->
	%{ok, PrimaryRatings} = application:get_env(maenmpc, primary_ratings), % TODO
	{ok, MPDList}      = application:get_env(maenmpc, mpd),
	% TODO DEBUG ONLY
	MPDFirst = m16,
	%[MPDFirst|_Others] = MPDList,
	{ok, #db{
		ui=NotifyToUI,
		mpd_map=lists:foldl(fun({Name, _ConnInfo}, Map) ->
			maps:put(Name, offline, Map)
		end, maps:new(), MPDList),
		mpd_active=MPDFirst
	}}.

handle_call({mpd_idle, Name, Subsystems}, _From, Context) ->
	{reply, ok, case Name =:= Context#db.mpd_active of
		true ->
			{OCtx, _OBL} = lists:foldl(fun(Subsystem,
							{Ctx, HasProcStatus}) ->
				case is_status_subsystem(Subsystem) of
				true ->
					case HasProcStatus of
					true  -> {Ctx, true};
					false -> {update_status(Ctx), true}
					end;
				false ->
					% TODO HANDLE OTHERS...
					{Ctx, HasProcStatus}
				end
			end, {Context, false}, Subsystems),
			OCtx;
		false ->
			% TODO IS IT ALWAYS IRRELEVANT WHEN STATUS OF INACTIVE UPDATES? MAYBE NOT E.G. FOR DB CHANGES ETC
			Context
		end};
handle_call(_Call, _From, Context) ->
	{reply, ok, Context}.

is_status_subsystem(player)  -> true;
is_status_subsystem(mixer)   -> true;
is_status_subsystem(options) -> true;
is_status_subsystem(_Other)  -> false.

%handle_idle(database, Context, _Name) ->
%	% the song database has been modified after update
%	Context; % TODO
%handle_idle(playlist, Context, _Name) ->
%	% the queue (i.e. the current playlist) has been modified
%	Context; % TODO
%handle_idle(player, Context, Name) ->
%	% the player has been started, stopped or seeked or tags of the
%	% currently playing song have changed 
%	update_status(Context, Name);
%handle_idle(mixer, Context, Name) ->
%	% the volume has been changed
%	update_status(Context, Name);
%handle_idle(output, Context) ->
%	% an audio output has been added, removed or modified
%	% (e.g. renamed, enabled or disabled)
%	Context; % TODO
%handle_idle(options, Context) ->
%	% options like repeat, random, crossfade, replay gain
%	update_status(Context);
%handle_idle(sticker, Context) ->
%	% the sticker database has been modified.
%	Context; % TODO
%handle_idle(_AnySubsystem, Context) ->
%	Context.

update_status(Context) ->
	gen_server:cast(Context#db.ui, {db_status, erlmpd:status(
			maps:get(Context#db.mpd_active, Context#db.mpd_map))}),
	Context.

handle_cast({mpd_assign, Name, Conn}, Context) ->
	{noreply, Context#db{mpd_map=maps:put(Name, Conn, Context#db.mpd_map)}};
handle_cast(_Cast, Context) ->
	{noreply, Context}.

handle_info(_Message, Context) -> {noreply, Context}.
code_change(_OldVersion, Context, _Extra) -> {ok, Context}.
