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
	WW1 = min(50, round(Ctx0#view.width * 2 / 3)),
	Ctx1 = Ctx0#view{
		wnd_song = cecho:newwin(4, WW1, 0, 0),
		wnd_card = cecho:newwin(4, max(0, Ctx0#view.width - WW1),
						0, WW1),
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
	accent(Ctx, Ctx#view.wnd_card, on, std),
	cecho:wborder(Ctx#view.wnd_card, $ , $ , $ , $-, $ , $ , $-, $-),
	accent(Ctx, Ctx#view.wnd_card, off, std),
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

handle_call(_Call, _From, Context) ->
	{reply, ok, Context}.

handle_cast({getch, Character}, Context) ->
	{noreply, case Character of
		%?ceKEY_F(2) ->
		%	page_new_start(Context);
		?ceKEY_F(10) ->
			init:stop(0),
			Context;
		_Any ->
			Context
		end};
handle_cast(_Cast, Context) ->
	{noreply, Context}.

handle_info(_Message,    Context)         -> {noreply, Context}.
code_change(_OldVersion, Context, _Extra) -> {ok,      Context}.
