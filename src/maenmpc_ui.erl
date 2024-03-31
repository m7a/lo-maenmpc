-module(maenmpc_ui).
-behavior(gen_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, code_change/3]).

-include_lib("cecho/include/cecho.hrl").

-define(CPAIR_DEFAULT,     1).
-define(CPAIR_DEFAULT_SEL, 2).
-define(CPAIR_ACCENT1,     3).
-define(CPAIR_ACCENT1_SEL, 4).
-define(CPAIR_ACCENT2,     5).
-define(CPAIR_ACCENT2_SEL, 6).
-define(CPAIR_ERROR,       7).

-record(view, {
	height, width,
	wnd_song, wnd_card, wnd_main, wnd_status, wnd_keys,
	% TODO CIDX SHOULD BE SENT FROM DB SOMEHOW
	cidx
}).

init([]) ->
	cecho:start_color(),
	init_color_pairs(),
	cecho:cbreak(),
	cecho:noecho(),
	cecho:keypad(?ceSTDSCR, true),
	{Height, Width} = cecho:getmaxyx(),
	{ok, init_windows(#view{height = Height, width = Width, cidx = 1})}.

init_color_pairs() ->
	cecho:init_pair(?CPAIR_DEFAULT,     ?ceCOLOR_WHITE,  ?ceCOLOR_BLACK),
	cecho:init_pair(?CPAIR_DEFAULT_SEL, ?ceCOLOR_BLACK,  ?ceCOLOR_WHITE),
	cecho:init_pair(?CPAIR_ACCENT1,     ?ceCOLOR_GREEN,  ?ceCOLOR_BLACK),
	cecho:init_pair(?CPAIR_ACCENT1_SEL, ?ceCOLOR_WHITE,  ?ceCOLOR_GREEN),
	cecho:init_pair(?CPAIR_ACCENT2,     ?ceCOLOR_BLUE,   ?ceCOLOR_BLACK),
	cecho:init_pair(?CPAIR_ACCENT2_SEL, ?ceCOLOR_WHITE,  ?ceCOLOR_BLUE),
	cecho:init_pair(?CPAIR_ERROR,       ?ceCOLOR_RED,    ?ceCOLOR_BLACK).

init_windows(Ctx0) ->
	WW1 = max(0, Ctx0#view.width - 35),
	Ctx1 = Ctx0#view{
		wnd_song = cecho:newwin(4, WW1, 0, 0),
		wnd_card = cecho:newwin(4, 35, 0, WW1),
		wnd_main = cecho:newwin(Ctx0#view.height - 7,
						Ctx0#view.width, 6, 0),
		wnd_status = cecho:newwin(2, Ctx0#view.width,
						Ctx0#view.height - 3, 0),
		wnd_keys = cecho:newwin(1, Ctx0#view.width,
						Ctx0#view.height - 1, 0)
	},
	Ctx2 = wnd_static_draw(Ctx1),
	cecho:mvwaddstr(Ctx2#view.wnd_status, 1, 0, "Initializing..."),
	cecho:attroff(Ctx2#view.wnd_status, ?ceCOLOR_PAIR(?CPAIR_DEFAULT)),
	cecho:wrefresh(Ctx2#view.wnd_status),
	Ctx2.

wnd_static_draw(Ctx) ->
	% song
	accent(Ctx, Ctx#view.wnd_song, on, std),
	cecho:wborder(Ctx#view.wnd_song, $ , $|, $ , $-, $ , $|, $-, $+),
	accent(Ctx, Ctx#view.wnd_song, off, std),
	cecho:wrefresh(Ctx#view.wnd_song),
	% card
	draw_card_border(Ctx),
	cecho:wrefresh(Ctx#view.wnd_card),
	% status
	accent(Ctx, Ctx#view.wnd_status, on, std),
	cecho:wborder(Ctx#view.wnd_status, $ , $ , $-, $ , $-, $-, $ , $ ),
	accent(Ctx, Ctx#view.wnd_status, off, std),
	cecho:attron(Ctx#view.wnd_status, ?ceCOLOR_PAIR(?CPAIR_DEFAULT)),
	cecho:wrefresh(Ctx#view.wnd_status),
	% keys
	lists:foreach(fun({FKey, Msg}) -> 
		cecho:wmove(Ctx#view.wnd_keys, 0, (FKey - 1) * 8),
		accent(Ctx, Ctx#view.wnd_keys, on, std),
		cecho:waddstr(Ctx#view.wnd_keys, io_lib:format("~2w", [FKey])),
		accent(Ctx, Ctx#view.wnd_keys, off, std),
		Atts = ?ceCOLOR_PAIR(?CPAIR_DEFAULT),
		cecho:attron(Ctx#view.wnd_keys, Atts),
		cecho:waddstr(Ctx#view.wnd_keys, io_lib:format("~-6s", [Msg])),
		cecho:attroff(Ctx#view.wnd_keys, Atts)
	end, [
		{1, "Help"},   {2, "Queue"}, {3, "Tree"}, {4, "List"},
		{5, "Search"}, {6, "Radio"}, {7, "Info"}, {8, "Output"},
		{9, ""},       {0, "Quit"}
	]),
	cecho:wrefresh(Ctx#view.wnd_keys),
	Ctx.

accent(Ctx, Wnd, OnOff, Sel) ->
	Atts = case {Ctx#view.cidx, Sel} of
		{1, std} -> ?ceCOLOR_PAIR(?CPAIR_ACCENT1);
		{1, sel} -> ?ceCOLOR_PAIR(?CPAIR_ACCENT1_SEL);
		{2, std} -> ?ceCOLOR_PAIR(?CPAIR_ACCENT2);
		{2, sel} -> ?ceCOLOR_PAIR(?CPAIR_ACCENT2_SEL)
		end,
	case OnOff of
	on  -> cecho:attron(Wnd,  Atts);
	off -> cecho:attroff(Wnd, Atts)
	end.

draw_card_border(Ctx) ->
	accent(Ctx, Ctx#view.wnd_card, on, std),
	cecho:wborder(Ctx#view.wnd_card, $ , $ , $ , $-, $ , $ , $-, $-),
	accent(Ctx, Ctx#view.wnd_card, off, std).

handle_call(_Call, _From, Context) ->
	{reply, ok, Context}.

handle_cast({getch, Character}, Ctx) ->
	{noreply, case Character of
		%?ceKEY_F(2) ->
		%	page_new_start(Context);
		?ceKEY_F(10) ->
			init:stop(0),
			Ctx;
		_Any ->
			Ctx
		end};
handle_cast({db_status, Status}, Ctx) ->
	{noreply, case proplists:get_value(error, Status) of
			undefined -> draw_status_on_card(Ctx, Status);
			ErrorInfo -> display_error(Ctx, io_lib:format(
					"status query error: ~w", [ErrorInfo]))
		end};
handle_cast(_Cast, Ctx) ->
	{noreply, Ctx}.

draw_status_on_card(Ctx, Status) ->
	% TODO QUERY SOUNDCARD AND "OTHER ENTRY" INFO FOR BITRATE ETC.
	%      -> should probably do this within the DB and then pass a
	%         fully-featured status record here with some additional,
	%         generated entries....
	%%   <li>playlistlength: integer: the length of the playlist</li>
	%%   <li>song: integer: playlist song number of the current song
	%%       stopped on or playing</li>
	%%   <li>songid: integer: playlist songid of the current song
	%%       stopped on or playing </li>

	Volume = case proplists:get_value(volume, Status) of
			undefined -> "Volume    n/a";
			PercVal   -> io_lib:format("Volume   ~3w%", [PercVal])
		end,
	% TODO HOW TO GET TOTAL SONG LENGTH INTO THIS HERE?
	Time = case proplists:get_value(time, Status) of
			undefined -> "[__:__/__:__]";
			Elapsed   -> io_lib:format("[~2w:~2w/__:__]",
					[floor(Elapsed / 60),
					round(Elapsed) rem 60])
		end,
	BitrateCurrent = case proplists:get_value(audio, Status) of
			undefined -> "song:      unknown";
			AudioInfo -> io_lib:format("song:  ~11s", [AudioInfo])
		end,
	% TODO WORK WITH COLORS AND ALSO FILL-IN THIS DATA HERE!
	BitrateOther = "other:     unknown",
	BitrateCard  = "ALSA:      unknown",
	% ncmpc/src/TitleBar.cxx
	InfoSymbol = case proplists:get_value(state, Status) of
		undefined -> "??";
		play      -> "|>";
		stop      -> "[]";
		pause     -> "||"
		end,
	InfoChars = io_lib:format("P ~s S ~s~s~s~s~s~s", [InfoSymbol,
		status_flag(proplists:get_value(repeat, Status), "r"),
		status_flag(proplists:get_value(random, Status), "z"),
		status_flag(proplists:get_value(single, Status), "s"),
		status_flag(proplists:get_value(consume, Status), "c"),
		status_flag(proplists:get_value(xfade, Status, 0) > 0, "x"),
		status_flag(proplists:get_value(updating_db, Status)
							=/= undefined, "U")]),
	cecho:werase(Ctx#view.wnd_card),
	draw_card_border(Ctx),
	cecho:attron(Ctx#view.wnd_card, ?ceCOLOR_PAIR(?CPAIR_DEFAULT)),
	cecho:mvwaddstr(Ctx#view.wnd_card, 0, 1, BitrateCurrent),
	cecho:mvwaddstr(Ctx#view.wnd_card, 1, 1, BitrateOther),
	cecho:mvwaddstr(Ctx#view.wnd_card, 2, 1, BitrateCard),
	cecho:mvwaddstr(Ctx#view.wnd_card, 0, 21, Volume),
	cecho:mvwaddstr(Ctx#view.wnd_card, 1, 21, Time),
	cecho:mvwaddstr(Ctx#view.wnd_card, 2, 21, InfoChars),
	% TODO USE LINE FOR FLAGS repeat/random/single/consume/xfade
	cecho:attroff(Ctx#view.wnd_card, ?ceCOLOR_PAIR(?CPAIR_DEFAULT)),
	cecho:wrefresh(Ctx#view.wnd_card),
	Ctx.

status_flag(BVal, Flag) ->
	case BVal of
	true   -> Flag;
	_Other -> "-"
	end.

display_error(Ctx, Error) ->
	cecho:werase(Ctx#view.wnd_status),
	cecho:attron(Ctx#view.wnd_status, ?ceCOLOR_PAIR(?CPAIR_ERROR)),
	cecho:mvwaddstr(Ctx#view.wnd_status, 1, 0, Error),
	cecho:attroff(Ctx#view.wnd_status, ?ceCOLOR_PAIR(?CPAIR_ERROR)),
	Ctx.

handle_info(_Message,    Context)         -> {noreply, Context}.
code_change(_OldVersion, Context, _Extra) -> {ok,      Context}.
