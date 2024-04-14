-module(maenmpc_ui).
-behavior(gen_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, code_change/3]).
-include_lib("cecho/include/cecho.hrl").
-include_lib("maenmpc_db.hrl").

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
	db, cidx
}).

init([NotifyToDB]) ->
	cecho:start_color(),
	init_color_pairs(),
	cecho:cbreak(),
	cecho:noecho(),
	cecho:keypad(?ceSTDSCR, true),
	{Height, Width} = cecho:getmaxyx(),
	{ok, init_windows(#view{height = Height, width = Width,
						db = NotifyToDB, cidx = 1})}.

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
	cecho:werase(Ctx#view.wnd_song),
	draw_song_border(Ctx),
	cecho:wrefresh(Ctx#view.wnd_song),
	% card
	cecho:werase(Ctx#view.wnd_card),
	draw_card_border(Ctx),
	cecho:wrefresh(Ctx#view.wnd_card),
	% status
	cecho:werase(Ctx#view.wnd_status),
	accent(Ctx, Ctx#view.wnd_status, on, std),
	cecho:wborder(Ctx#view.wnd_status, $ , $ , $-, $ , $-, $-, $ , $ ),
	accent(Ctx, Ctx#view.wnd_status, off, std),
	cecho:attron(Ctx#view.wnd_status, ?ceCOLOR_PAIR(?CPAIR_DEFAULT)),
	cecho:wrefresh(Ctx#view.wnd_status),
	% keys
	cecho:werase(Ctx#view.wnd_keys),
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
		{1,  std} -> ?ceCOLOR_PAIR(?CPAIR_ACCENT1);
		{1,  sel} -> ?ceCOLOR_PAIR(?CPAIR_ACCENT1_SEL);
		{2,  std} -> ?ceCOLOR_PAIR(?CPAIR_ACCENT2);
		{2,  sel} -> ?ceCOLOR_PAIR(?CPAIR_ACCENT2_SEL);
		{_X, std} -> ?ceCOLOR_PAIR(?CPAIR_DEFAULT);
		{_Y, sel} -> ?ceCOLOR_PAIR(?CPAIR_DEFAULT_SEL)
		end,
	case OnOff of
	on  -> cecho:attron(Wnd,  Atts);
	off -> cecho:attroff(Wnd, Atts)
	end.

draw_card_border(Ctx) ->
	accent(Ctx, Ctx#view.wnd_card, on, std),
	cecho:wborder(Ctx#view.wnd_card, $ , $ , $ , $-, $ , $ , $-, $-),
	accent(Ctx, Ctx#view.wnd_card, off, std).

draw_song_border(Ctx) ->
	accent(Ctx, Ctx#view.wnd_song, on, std),
	cecho:wborder(Ctx#view.wnd_song, $ , $|, $ , $-, $ , $|, $-, $+),
	accent(Ctx, Ctx#view.wnd_song, off, std).

handle_call(_Call, _From, Context) ->
	{reply, ok, Context}.

handle_cast({getch, Character}, Ctx) ->
	{noreply, case Character of
		?ceKEY_LEFT  -> ui_request(Ctx, {ui_simple, volume_change, -1});
		?ceKEY_RIGHT -> ui_request(Ctx, {ui_simple, volume_change, +1});
		$s           -> ui_request(Ctx, {ui_simple, stop});
		$P           -> ui_request(Ctx, {ui_simple, toggle_pause});
		$r           -> ui_request(Ctx, {ui_simple, toggle_repeat});
		$z           -> ui_request(Ctx, {ui_simple, toggle_random});
		$y           -> ui_request(Ctx, {ui_simple, toggle_single});
		$C           -> ui_request(Ctx, {ui_simple, toggle_consume});
		$x           -> ui_request(Ctx, {ui_simple, toggle_xfade});
		%?ceKEY_F(2) ->
		%	page_new_start(Context);
		?ceKEY_F(10) -> init:stop(0), Ctx;
		_Any         -> Ctx
	end};
handle_cast({db_cidx, CIDX}, Ctx) ->
	{noreply, wnd_static_draw(Ctx#view{cidx=CIDX})};
handle_cast({db_error, Info}, Ctx) ->
	{noreply, display_error(Ctx, io_lib:format("DB error: ~w", [Info]))};
handle_cast({db_playing, SongAndStatus}, Ctx) ->
	{noreply, case proplists:get_value(error, SongAndStatus) of
		undefined -> draw_song_and_status(Ctx, SongAndStatus);
		ErrorInfo -> display_error(Ctx, io_lib:format(
					"status query error: ~w", [ErrorInfo]))
	end};
handle_cast(_Cast, Ctx) ->
	{noreply, Ctx}.

ui_request(Ctx, Request) ->
	gen_server:cast(Ctx#view.db, Request),
	Ctx.

draw_song_and_status(Ctx, Info) ->
	% -- Song Info --
	cecho:werase(Ctx#view.wnd_song),
	draw_song_border(Ctx),
	cecho:attron(Ctx#view.wnd_song, ?ceCOLOR_PAIR(?CPAIR_DEFAULT)),
	{_WH, WW} = cecho:getmaxyx(Ctx#view.wnd_song),
	PadWidth = WW - 3,
	DBE = proplists:get_value(x_maenmpc, Info),
	cecho:mvwaddstr(Ctx#view.wnd_song, 0, 1, utf8pad(PadWidth,
			io_lib:format("~s, ~s: ~s", [element(1, DBE#dbsong.key),
			DBE#dbsong.year, element(2, DBE#dbsong.key)]))),
	cecho:mvwaddstr(Ctx#view.wnd_song, 1, 1, utf8pad(PadWidth - 6,
			io_lib:format("~2..0w - ~s",
			[DBE#dbsong.trackno, element(3, DBE#dbsong.key)]))),
	cecho:mvwaddstr(Ctx#view.wnd_song, 1, WW - 7,
			format_rating(DBE#dbsong.rating)),
	cecho:mvwaddstr(Ctx#view.wnd_song, 2, 1, progress(PadWidth,
			floor(proplists:get_value(time, Info, 0)),
			DBE#dbsong.duration)),
	% -- Status Info --
	Volume = case proplists:get_value(volume, Info) of
			undefined -> "Volume n/a";
			-1        -> "Volume n/a";
			PercVal   -> io_lib:format("Volume ~3w%", [PercVal])
		end,
	BitrateCurrent = io_lib:format("song:  ~11s",
				[element(Ctx#view.cidx, DBE#dbsong.audios)]),
	OtherIdx = (Ctx#view.cidx rem tuple_size(DBE#dbsong.audios)) + 1,
	BitrateOther = io_lib:format("other: ~11s",
				[element(OtherIdx, DBE#dbsong.audios)]),
	BitrateCard = io_lib:format("ALSA:  ~11s",
				[proplists:get_value(x_maenmpc_alsa, Info)]),
	InfoSymbol = case proplists:get_value(state, Info) of
		undefined -> "??";
		play      -> "|>";
		stop      -> "[]";
		pause     -> "||"
		end,
	% ncmpc/src/TitleBar.cxx
	InfoChars = io_lib:format("P ~s S ~s~s~s~s~s~s", [InfoSymbol,
		status_flag(proplists:get_value(repeat,  Info), "r"),
		status_flag(proplists:get_value(random,  Info), "z"),
		status_flag(proplists:get_value(single,  Info), "s"),
		status_flag(proplists:get_value(consume, Info), "c"),
		status_flag(proplists:get_value(xfade,   Info, 0) > 0, "x"),
		status_flag(proplists:get_value(updating_db, Info)
							=/= undefined, "U")]),
	cecho:werase(Ctx#view.wnd_card),
	draw_card_border(Ctx),
	cecho:attron(Ctx#view.wnd_card, ?ceCOLOR_PAIR(?CPAIR_DEFAULT)),
	cecho:mvwaddstr(Ctx#view.wnd_card, 0, 1, BitrateCurrent),
	cecho:mvwaddstr(Ctx#view.wnd_card, 1, 1, BitrateOther),
	cecho:mvwaddstr(Ctx#view.wnd_card, 2, 1, BitrateCard),
	cecho:mvwaddstr(Ctx#view.wnd_card, 0, 21, "MAENMPC 0.1.0"),
	cecho:mvwaddstr(Ctx#view.wnd_card, 1, 21, Volume),
	cecho:mvwaddstr(Ctx#view.wnd_card, 2, 21, InfoChars),
	cecho:attroff(Ctx#view.wnd_card, ?ceCOLOR_PAIR(?CPAIR_DEFAULT)),
	% -- Refresh and complete --
	cecho:wrefresh(Ctx#view.wnd_card),
	cecho:wrefresh(Ctx#view.wnd_song),
	Ctx.

utf8pad(Pad, Str) ->
	SL = string:length(Str),
	case SL > Pad of
	true  -> string:slice(Str, 0, Pad);
	false -> io_lib:format("~s~" ++ integer_to_list(Pad - SL) ++ "s",
								[Str, ""])
	end.

progress(PadWidth, Pos, OfTime) ->
	BarWidth  = PadWidth - 14,
	FillChars = Pos * BarWidth div OfTime,
	io_lib:format("~s~s [~2..0w:~2..0w|~2..0w:~2..0w]",
		[lists:duplicate(FillChars, $#),
		lists:duplicate(BarWidth - FillChars, $_),
		Pos div 60, Pos rem 60, OfTime div 60, OfTime rem 60]).

format_rating(?RATING_UNRATED) ->
	"?rate";
format_rating(?RATING_ERROR) ->
	"!ERR!";
format_rating(Rating) ->
	NumStars = Rating div 20,
	lists:duplicate(NumStars, $*) ++ lists:duplicate(5 - NumStars, $.).

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
