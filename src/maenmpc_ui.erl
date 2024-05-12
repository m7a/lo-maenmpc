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
	wnd_song, wnd_card, wnd_main, wnd_sel, wnd_sel_card, wnd_status,
	wnd_keys,
	db, cidx, page, storech
}).

init([NotifyToDB]) ->
	cecho:start_color(),
	init_color_pairs(),
	cecho:cbreak(),
	cecho:noecho(),
	cecho:keypad(?ceSTDSCR, true),
	{Height, Width} = cecho:getmaxyx(),
	{ok, init_windows(#view{height = Height, width = Width,
			db = NotifyToDB, cidx = 1, page = help, storech = $_},
			"Initializing...")}.

init_color_pairs() ->
	cecho:init_pair(?CPAIR_DEFAULT,     ?ceCOLOR_WHITE,  ?ceCOLOR_BLACK),
	cecho:init_pair(?CPAIR_DEFAULT_SEL, ?ceCOLOR_BLACK,  ?ceCOLOR_WHITE),
	cecho:init_pair(?CPAIR_ACCENT1,     ?ceCOLOR_GREEN,  ?ceCOLOR_BLACK),
	cecho:init_pair(?CPAIR_ACCENT1_SEL, ?ceCOLOR_WHITE,  ?ceCOLOR_GREEN),
	cecho:init_pair(?CPAIR_ACCENT2,     ?ceCOLOR_CYAN,   ?ceCOLOR_BLACK),
	cecho:init_pair(?CPAIR_ACCENT2_SEL, ?ceCOLOR_BLACK,  ?ceCOLOR_CYAN),
	cecho:init_pair(?CPAIR_ERROR,       ?ceCOLOR_RED,    ?ceCOLOR_BLACK).

init_windows(Ctx0, Status) ->
	WW1 = max(0, Ctx0#view.width - 35),
	WW2 = max(0, Ctx0#view.width - 21),
	YW2 = max(0, Ctx0#view.height - 7),
	Ctx1 = Ctx0#view{
		wnd_song     = cecho:newwin(4, WW1, 0, 0),
		wnd_card     = cecho:newwin(4, 35,  0, WW1),
		wnd_sel      = cecho:newwin(5, WW2, YW2, 0),
		wnd_sel_card = cecho:newwin(5, 21, YW2, WW2),
		wnd_main     = cecho:newwin(main_height(Ctx0),
					Ctx0#view.width, 4, 0),
		wnd_status   = cecho:newwin(1, Ctx0#view.width,
					max(0, Ctx0#view.height - 2), 0),
		wnd_keys     = cecho:newwin(1, Ctx0#view.width,
					max(0, Ctx0#view.height - 1), 0)
	},
	Ctx2 = wnd_static_draw(Ctx1),
	cecho:mvwaddstr(Ctx2#view.wnd_status, 0, 0, Status),
	cecho:attroff(Ctx2#view.wnd_status, ?ceCOLOR_PAIR(?CPAIR_DEFAULT)),
	cecho:wrefresh(Ctx2#view.wnd_status),
	Ctx2.

main_height(Ctx) ->
	max(0, Ctx#view.height - 11).

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
	cecho:wrefresh(Ctx#view.wnd_status),
	% sel
	cecho:werase(Ctx#view.wnd_sel),
	draw_sel_border(Ctx),
	cecho:wrefresh(Ctx#view.wnd_sel),
	% sel_card
	cecho:werase(Ctx#view.wnd_sel_card),
	draw_sel_card_border(Ctx),
	cecho:wrefresh(Ctx#view.wnd_sel_card),
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
		% Planned assignments: {3, "Tree"}, {7, "Info"}
		{1, "Help"},   {2, "Queue"}, {3, ""}, {4, "List"},
		{5, "Search"}, {6, "Radio"}, {7, ""}, {8, "Output"},
		{9, ""},       {0, "Quit"}
	]),
	cecho:wrefresh(Ctx#view.wnd_keys),
	case Ctx#view.page =:= help of
	true  -> draw_page_help(Ctx);
	false -> Ctx
	end.

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

draw_sel_border(Ctx) ->
	accent(Ctx, Ctx#view.wnd_sel, on, std),
	cecho:wborder(Ctx#view.wnd_sel, $ , $|, $-, $-, $-, $+, $-, $+),
	accent(Ctx, Ctx#view.wnd_sel, off, std).

draw_sel_card_border(Ctx) ->
	accent(Ctx, Ctx#view.wnd_sel_card, on, std),
	cecho:wborder(Ctx#view.wnd_sel_card, $ , $ , $-, $-, $-, $-, $-, $-),
	accent(Ctx, Ctx#view.wnd_sel_card, off, std).

draw_page_help(Ctx) ->
	cecho:werase(Ctx#view.wnd_main),
	X0 = max(0, (Ctx#view.width - 78) div 2),
	cecho:mvwaddstr(Ctx#view.wnd_main, 1, X0 + 7, "Ma_Sys.ma Erlang " ++
				"NCurses Music Player Client -- M A E N M P C"),
	cecho:mvwaddstr(Ctx#view.wnd_main, 2, X0, "(c) 2024 Ma_Sys.ma " ++
		"<info@masysma.net>. For documentation, consult README.md"),
	Default = ?ceCOLOR_PAIR(?CPAIR_DEFAULT),
	Accent = case Ctx#view.cidx of
		1    -> ?ceCOLOR_PAIR(?CPAIR_ACCENT1);
		2    -> ?ceCOLOR_PAIR(?CPAIR_ACCENT2);
		_Any -> Default
		end,
	draw_static_columns(Ctx#view.wnd_main, X0, 4, [
		[{10, "Navigate", Accent},
		 {6,  "Play",     Accent}, {14, "", Default},
		 {6,  "Toggle",   Accent}, {11, "", Default},
		 {9,  "Other",    Accent}, {19, "", Default} ],
		[<<"↑ k"/utf8>>,
		 <<"→ +"/utf8>>,      <<"Volume up"/utf8>>,
		 <<"P"/utf8>>,        <<"play"/utf8>>,
		 <<"d DEL"/utf8>>,    <<"queue:  Remove item"/utf8>>],
		[<<"↓ j"/utf8>>,
		 <<"← -"/utf8>>,      <<"Volume down"/utf8>>,
		 <<"r"/utf8>>,        <<"repeat"/utf8>>,
		 <<"CTRL-K/J"/utf8>>, <<"queue:  Move item"/utf8>>],
		[<<"PgUp"/utf8>>,
		 <<"←- s"/utf8>>,     <<"stop"/utf8>>,
		 <<"z"/utf8>>,        <<"random"/utf8>>,
		 <<"*"/utf8>>,        <<"rating: up"/utf8>>],
		[<<"PgDown"/utf8>>,
		 <<"Enter"/utf8>>,    <<"play now"/utf8>>,
		 <<"y"/utf8>>,        <<"single"/utf8>>,
		 <<"#"/utf8>>,        <<"rating: down"/utf8>>],
		[<<"Home gg"/utf8>>,
		 <<"a"/utf8>>,        <<"add @end"/utf8>>,
		 <<"C"/utf8>>,        <<"consume"/utf8>>,
		 <<"R"/utf8>>,        <<"radio:  start/reset"/utf8>>],
		[<<"End  G"/utf8>>,
		 <<"A"/utf8>>,        <<"add @current"/utf8>>,
		 <<"x"/utf8>>,        <<"crossfade"/utf8>>,
		 <<"T"/utf8>>,        <<"radio:  stop radio"/utf8>>],
		[<<"F1..F10"/utf8>>,
		 <<">"/utf8>>,        <<"next"/utf8>>,
		 <<"p"/utf8>>,        <<"podcasts"/utf8>>,
		 [],                  []],
		[[],
		 <<"<"/utf8>>,        <<"prev"/utf8>>,
		 <<"Space"/utf8>>,    <<"expand"/utf8>>,
		 <<"/ ? n p"/utf8>>,  <<"search on screen"/utf8>>]
	]),
	cecho:wrefresh(Ctx#view.wnd_main),
	Ctx.

draw_static_columns(Window, X, Y, All=[Heading|_T]) ->
	lists:foldl(fun({Width, Label, _Color}, CX) ->
			Atts = ?ceCOLOR_PAIR(?CPAIR_DEFAULT) bor ?ceA_BOLD,
			cecho:attron(Window, Atts),
			cecho:mvwaddstr(Window, Y, CX, utf8pad(Width, Label)),
			cecho:attroff(Window, Atts),
			CX + Width
		end, X, Heading),
	draw_static_columns_inner(Window, X, Y + 1, All).

draw_static_columns_inner(_Window, _X, Y, [_Heading|[]]) ->
	Y;
draw_static_columns_inner(Window, X, Y, [Heading|[Line|Rem]]) ->
	lists:foldl(fun({{Width, _Label, Atts}, Content}, CX) ->
			cecho:attron(Window, Atts),
			cecho:mvwaddstr(Window, Y, CX, utf8pad(Width, Content)),
			cecho:attroff(Window, Atts),
			CX + Width
		end, X, lists:zip(Heading, Line)),
	draw_static_columns_inner(Window, X, Y + 1, [Heading|Rem]).

handle_call(_Call, _From, Context) ->
	{reply, ok, Context}.

handle_cast({getch, Character}, Ctx) ->
	{noreply, case Ctx#view.storech =:= $_ of
		true ->
			single_key(Ctx, Character);
		false ->
			Ctx2 = Ctx#view{storech=$_},
			case {Ctx#view.storech, Character} of
			{$g, $g} -> ui_scroll(Ctx2, top);
			{$Z, $Q} -> init:stop(0), Ctx2;
			{$Z, $Z} -> init:stop(0), Ctx2;
			{_A, _B} -> Ctx2
			end
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
handle_cast({db_results, L}, Ctx) when L#dbscroll.type =:= Ctx#view.page ->
	{noreply, draw_results(Ctx, L)};
handle_cast(_Cast, Ctx) ->
	{noreply, Ctx}.

single_key(Ctx, Character) ->
	case Character of
	$-               -> ui_request(Ctx, {ui_simple, volume_change, -1});
	?ceKEY_LEFT      -> ui_request(Ctx, {ui_simple, volume_change, -1});
	$+               -> ui_request(Ctx, {ui_simple, volume_change, +1});
	?ceKEY_RIGHT     -> ui_request(Ctx, {ui_simple, volume_change, +1});
	?ceKEY_BACKSPACE -> ui_request(Ctx, {ui_simple, stop});
	$s               -> ui_request(Ctx, {ui_simple, stop});
	$P               -> ui_request(Ctx, {ui_simple, toggle_pause});
	$r               -> ui_request(Ctx, {ui_simple, toggle_repeat});
	$z               -> ui_request(Ctx, {ui_simple, toggle_random});
	$y               -> ui_request(Ctx, {ui_simple, toggle_single});
	$C               -> ui_request(Ctx, {ui_simple, toggle_consume});
	$x               -> ui_request(Ctx, {ui_simple, toggle_xfade});
	$<               -> ui_request(Ctx, {ui_simple, song_previous});
	$>               -> ui_request(Ctx, {ui_simple, song_next});
	$R               -> ui_request(Ctx, ui_radio_start);
	$T               -> ui_request(Ctx, ui_radio_stop);
	$k               -> ui_scroll(Ctx, -1);
	?ceKEY_UP        -> ui_scroll(Ctx, -1);
	$j               -> ui_scroll(Ctx, +1);
	?ceKEY_DOWN      -> ui_scroll(Ctx, +1);
	?ceKEY_PGDOWN    -> ui_scroll(Ctx, +current_page_height(Ctx));
	?ceKEY_PGUP      -> ui_scroll(Ctx, -current_page_height(Ctx));
	?ceKEY_HOME      -> ui_scroll(Ctx, top);
	$g               -> Ctx#view{storech=Character};
	$Z               -> Ctx#view{storech=Character};
	?ceKEY_END       -> ui_scroll(Ctx, bottom);
	$G               -> ui_scroll(Ctx, bottom);
	?ceKEY_F(1)      -> draw_page_help(Ctx#view{page=help});
	?ceKEY_F(2)      -> ui_request(Ctx#view{page=queue},
				{ui_query, queue, main_height(Ctx)-1});
	?ceKEY_F(4)      -> ui_request(Ctx#view{page=list},
				{ui_query, list,  main_height(Ctx)});
	?ceKEY_F(6)      -> ui_request(Ctx#view{page=radio},
				{ui_query, radio, main_height(Ctx)});
	$q               -> init:stop(0), Ctx;
	?ceKEY_F(10)     -> init:stop(0), Ctx;
	?ceKEY_RESIZE    -> ui_resize(Ctx);
	% TODO NEW KEYBINDINGS / REQUESTS
	$\n              -> ui_request(Ctx, {ui_selected, Ctx#view.page, play});
	$a               -> ui_request(Ctx, {ui_selected, Ctx#view.page,
							enqueue_end});
	$A               -> ui_request(Ctx, {ui_selected, Ctx#view.page,
							enqueue_current});
	?ceKEY_DEL       -> ui_request(Ctx, {ui_selected, Ctx#view.page,
							queue_delete});
	$d               -> ui_request(Ctx, {ui_selected, Ctx#view.page,
							queue_delete});
	$*               -> ui_request(Ctx, {ui_selected, Ctx#view.page,
							rating_up});
	$#               -> ui_request(Ctx, {ui_selected, Ctx#view.page,
							rating_down});
	% TODO F5       - search screen
	% TODO / ? n p  - search on screen
	% TODO F8       - output selection
	% TODO p        - podcasts
	% TODO Space    - expand/contract
	% TODO CTRL-K/J - move item in playlist
	_Any             -> Ctx
	end.

ui_request(Ctx, Request) ->
	gen_server:cast(Ctx#view.db, Request),
	Ctx.

draw_song_and_status(Ctx, Info) ->
	% -- Song Info --
	cecho:werase(Ctx#view.wnd_song),
	draw_song_border(Ctx),
	cecho:attron(Ctx#view.wnd_song, ?ceCOLOR_PAIR(?CPAIR_DEFAULT)),
	DBE = proplists:get_value(x_maenmpc, Info),
	PadWidth = draw_basic_song_info(Ctx#view.wnd_song, DBE, 0),
	cecho:mvwaddstr(Ctx#view.wnd_song, 2, 1, progress(PadWidth,
			floor(proplists:get_value(time, Info, 0)),
			DBE#dbsong.duration)),
	% -- Status Info --
	Volume = case proplists:get_value(volume, Info) of
			undefined -> "Volume n/a";
			-1        -> "Volume n/a";
			PercVal   -> io_lib:format("Volume ~3w%", [PercVal])
		end,
	{BitrateCurrent, BitrateOther} = get_bitrates(Ctx, DBE),
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

get_bitrates(Ctx, DBE) ->
	OtherIdx = (Ctx#view.cidx rem tuple_size(DBE#dbsong.audios)) + 1,
	{io_lib:format("song:  ~11s", [element(Ctx#view.cidx, DBE#dbsong.audios)]),
	io_lib:format("other: ~11s", [element(OtherIdx, DBE#dbsong.audios)])}.

% returns PadWidth
draw_basic_song_info(WndSong, DBE, Y0) ->
	{_WH, WW} = cecho:getmaxyx(WndSong),
	PadWidth = WW - 3,
	cecho:mvwaddstr(WndSong, Y0, 1, utf8pad(PadWidth,
			io_lib:format("~s, ~s: ~s", [element(1, DBE#dbsong.key),
			DBE#dbsong.year, element(2, DBE#dbsong.key)]))),
	cecho:mvwaddstr(WndSong, Y0 + 1, 1, utf8pad(PadWidth - 6,
			io_lib:format("~2..0w - ~s",
			[DBE#dbsong.trackno, element(3, DBE#dbsong.key)]))),
	cecho:mvwaddstr(WndSong, 1, PadWidth - 4,
			maenmpc_erlmpd:format_rating(DBE#dbsong.rating)),
	PadWidth.

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

status_flag(BVal, Flag) ->
	case BVal of
	true   -> Flag;
	_Other -> "-"
	end.

display_error(Ctx, Error) ->
	cecho:werase(Ctx#view.wnd_status),
	cecho:attron(Ctx#view.wnd_status, ?ceCOLOR_PAIR(?CPAIR_ERROR)),
	cecho:mvwaddstr(Ctx#view.wnd_status, 0, 0, Error),
	cecho:attroff(Ctx#view.wnd_status, ?ceCOLOR_PAIR(?CPAIR_ERROR)),
	cecho:wrefresh(Ctx#view.wnd_status),
	Ctx.

draw_results(Ctx, List=#dbscroll{cnt=Cnt, total=Total,
				user_data={_CurrentSongID, AbsoluteOffset}}) ->
	cecho:werase(Ctx#view.wnd_main),
	{Info, MaxDraw, DY} = draw_results_begin(Ctx, List),
	{SOffset, SHeight} = scroll_offset_height(
						AbsoluteOffset, Total, MaxDraw),
	SelVal = lists:foldl(fun({S, Y}, SelIn) ->
			IsSel = draw_result_line(Ctx, List, {S, Y}, Info),
			draw_scroll(Ctx, SHeight, SOffset + DY, Y),
			case IsSel of true -> S; false -> SelIn end
		end, none, lists:zip(Cnt, lists:seq(DY, length(Cnt) + DY - 1))),
	lists:foreach(fun(Y) -> draw_scroll(Ctx, SHeight, SOffset + DY, Y) end,
				lists:seq(length(Cnt) + DY, MaxDraw)),
	cecho:wrefresh(Ctx#view.wnd_main),
	case SelVal of
	none -> Ctx;
	_Sel -> draw_sel(Ctx, SelVal)
	end.

draw_results_begin(Ctx, #dbscroll{type=queue}) ->
	WA = max(1, (Ctx#view.width - 20) * 1 div 3),
	WT = max(1, (Ctx#view.width - 20) * 2 div 3),
	cecho:mvwaddstr(Ctx#view.wnd_main, 0, 0,
				io_lib:format("  Rated  ~s  ~s  MM:ss",
				[utf8pad(WA, "Artist"), utf8pad(WT, "Title")])),
	cecho:mvwaddstr(Ctx#view.wnd_main, 0, Ctx#view.width - 1, "_"),
	{{WA, WT}, max(0, main_height(Ctx) - 1), 1};
draw_results_begin(Ctx, #dbscroll{type=list}) ->
	{max(1, Ctx#view.width - 24), max(0, main_height(Ctx)), 0};
draw_results_begin(Ctx, #dbscroll{type=radio}) ->
	{max(1, Ctx#view.width - 3),  max(0, main_height(Ctx)), 0}.

scroll_offset_height(AOffset, Total, MaxDraw) ->
	TRef       = max(1, Total),
	SHeight    = min(MaxDraw, max(1, round(MaxDraw * MaxDraw / TRef))),
	SOffsetRaw = max(0,       round(AOffset * (MaxDraw - SHeight) / TRef)),
	if
	AOffset /= 0 andalso SOffsetRaw == 0 -> {1,                SHeight};
	Total - AOffset =< MaxDraw      -> {MaxDraw - SHeight,     SHeight};
	SOffsetRaw >= MaxDraw - SHeight -> {MaxDraw - SHeight - 1, SHeight};
	true                            -> {SOffsetRaw,            SHeight}
	end.

draw_result_line(Ctx, #dbscroll{type=queue, csel=CSel,
				user_data={CurrentSongID, _AbsoluteOffset}},
				{S, Y}, {WA, WT}) ->
	IsCurrent = S#dbsong.playlist_id =:= CurrentSongID andalso
							CurrentSongID /= -1,
	IsSel = Y - 1 =:= CSel,
	Atts = uris_to_cpair(S, IsSel) bor
		case IsCurrent of true -> ?ceA_BOLD; false -> 0 end,
	cecho:attron(Ctx#view.wnd_main, Atts),
	cecho:mvwaddstr(Ctx#view.wnd_main, Y, 0,
		io_lib:format("~c ~s  ~s  ~s  ~2..0w:~2..0w",
		[case IsCurrent of true -> $>; false -> $ end,
		maenmpc_erlmpd:format_rating(S#dbsong.rating),
		utf8pad(WA, element(1, S#dbsong.key)),
		utf8pad(WT, element(3, S#dbsong.key)),
		S#dbsong.duration div 60,
		S#dbsong.duration rem 60])),
	cecho:attroff(Ctx#view.wnd_main, Atts),
	IsSel;
draw_result_line(Ctx, #dbscroll{type=list, csel=CSel}, {S, Y}, WT) ->
	IsSel = Y =:= CSel,
	case S#dbsong.key of
	{Artist, Album, album} ->
		AttsD = uris_to_cpair(S, IsSel) bor ?ceA_BOLD,
		cecho:attron(Ctx#view.wnd_main, AttsD),
		cecho:mvwaddstr(Ctx#view.wnd_main, Y, 0, io_lib:format(
			"~s (~s): ~s", [Artist, S#dbsong.year, Album])),
		cecho:attroff(Ctx#view.wnd_main, AttsD);
	{_Artist, _Album, Title} ->
		Atts = uris_to_cpair(S, IsSel),
		cecho:attron(Ctx#view.wnd_main, Atts),
		cecho:mvwaddstr(Ctx#view.wnd_main, Y, 2,
			io_lib:format("~2..0w  ~s  ~s  ~2..0w:~2..0w",
			[S#dbsong.trackno,
			maenmpc_erlmpd:format_rating(S#dbsong.rating),
			utf8pad(WT, Title),
			S#dbsong.duration div 60,
			S#dbsong.duration rem 60])),
		cecho:attroff(Ctx#view.wnd_main, Atts)
	end,
	IsSel;
draw_result_line(Ctx, #dbscroll{type=radio, csel=CSel,
				user_data={CurrentLogID, _AbsoluteOffset}},
		{{ID, Line}, Y}, WT) ->
	IsCurrent = CurrentLogID =:= ID,
	IsSel = Y =:= CSel,
	Atts = case IsSel of
		true  -> ?ceCOLOR_PAIR(?CPAIR_DEFAULT_SEL);
		false -> ?ceCOLOR_PAIR(?CPAIR_DEFAULT)
		end bor case IsCurrent of
		true  -> ?ceA_BOLD;
		false -> 0
		end,
	cecho:attron(Ctx#view.wnd_main, Atts),
	cecho:mvwaddstr(Ctx#view.wnd_main, Y, 0, utf8pad(WT, Line)),
	cecho:attroff(Ctx#view.wnd_main, Atts),
	% We must not permit the default logic to draw the “sel” info here
	% [would be interesting wrt. viewing full info for truncated messages]
	% TODO x FACTOR OUT SPECIAL RADIO MODE EVEN?
	false.

uris_to_cpair(S, IsSel) ->
	case S#dbsong.uris of
	{<<>>, <<>>} when IsSel -> ?ceCOLOR_PAIR(?CPAIR_DEFAULT_SEL);
	{<<>>, <<>>}            -> ?ceCOLOR_PAIR(?CPAIR_DEFAULT);
	{_Any, <<>>} when IsSel -> ?ceCOLOR_PAIR(?CPAIR_ACCENT1_SEL);
	{_Any, <<>>}            -> ?ceCOLOR_PAIR(?CPAIR_ACCENT1);
	{<<>>, _Any} when IsSel -> ?ceCOLOR_PAIR(?CPAIR_ACCENT2_SEL);
	{<<>>, _Any}            -> ?ceCOLOR_PAIR(?CPAIR_ACCENT2);
	_Other when IsSel       -> ?ceCOLOR_PAIR(?CPAIR_DEFAULT_SEL);
	_Other                  -> ?ceCOLOR_PAIR(?CPAIR_DEFAULT)
	end.

draw_scroll(Ctx, SHeight, SOffset, Y) ->
	case Y >= SOffset andalso Y < (SOffset + SHeight) of
	true  -> cecho:mvwaddstr(Ctx#view.wnd_main, Y, Ctx#view.width - 1, "#");
	false -> ok
	end.

draw_sel(Ctx, S) ->
	% sel
	cecho:werase(Ctx#view.wnd_sel),
	draw_sel_border(Ctx),
	cecho:attron(Ctx#view.wnd_sel, ?ceCOLOR_PAIR(?CPAIR_DEFAULT)),
	PadWidth = draw_basic_song_info(Ctx#view.wnd_sel, S, 1),
	cecho:mvwaddstr(Ctx#view.wnd_sel, 3, 1, utf8pad(PadWidth,
			element(Ctx#view.cidx, S#dbsong.uris))),
	cecho:attroff(Ctx#view.wnd_sel, ?ceCOLOR_PAIR(?CPAIR_DEFAULT)),
	cecho:wrefresh(Ctx#view.wnd_sel),
	% sel_card
	cecho:werase(Ctx#view.wnd_sel_card),
	draw_sel_card_border(Ctx),
	cecho:attron(Ctx#view.wnd_sel_card, ?ceCOLOR_PAIR(?CPAIR_DEFAULT)),
	{BitrateCurrent, BitrateOther} = get_bitrates(Ctx, S),
	cecho:mvwaddstr(Ctx#view.wnd_sel_card, 1, 1, BitrateCurrent),
	cecho:mvwaddstr(Ctx#view.wnd_sel_card, 2, 1, BitrateOther),
	cecho:mvwaddstr(Ctx#view.wnd_sel_card, 3, 1,
			io_lib:format("playcount: ~w", [S#dbsong.playcount])),
	cecho:attroff(Ctx#view.wnd_sel_card, ?ceCOLOR_PAIR(?CPAIR_DEFAULT)),
	cecho:wrefresh(Ctx#view.wnd_sel_card),
	Ctx.

current_page_height(Ctx) ->
	case Ctx#view.page of
	queue  -> main_height(Ctx) - 1;
	_Other -> main_height(Ctx)
	end.

ui_scroll(Ctx=#view{page=PG}, Offset) ->
	case (PG =:= queue orelse PG =:= list orelse PG =:= radio) of
	true  -> ui_request(Ctx, {ui_scroll, PG, Offset,
						current_page_height(Ctx)});
	false -> Ctx
	end.

ui_resize(Ctx) ->
	cecho:endwin(),
	cecho:refresh(),
	lists:foreach(fun(Window) ->
				cecho:werase(Window),
				cecho:delwin(Window)
			end,
			[Ctx#view.wnd_song, Ctx#view.wnd_card, Ctx#view.wnd_sel,
				Ctx#view.wnd_sel_card, Ctx#view.wnd_main,
				Ctx#view.wnd_status, Ctx#view.wnd_keys]),
	{Height, Width} = cecho:getmaxyx(),
	init_windows(Ctx#view{height=Height, width=Width}, "resized...").

handle_info(_Message,    Context)         -> {noreply, Context}.
code_change(_OldVersion, Context, _Extra) -> {ok,      Context}.
