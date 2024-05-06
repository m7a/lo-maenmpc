-module(maenmpc_erlmpd).
-export([connect/1, to_dbsong/3, normalize_key/1, normalize_always/1,
	normalize_strong/1, merge_tuple/2, convert_rating/1, format_rating/1]).
-include_lib("maenmpc_db.hrl").

connect(Config) ->
	{Host, Port} = proplists:get_value(ip, Config),
	erlmpd:connect(Host, Port).

% Idx: What is our player index, Len: how many players are there in total
to_dbsong(Entry, Idx, Len) ->
	#dbsong{key          = erlmpd_to_key(Entry),
		uris         = new_tuple(proplists:get_value(file, Entry),
								Idx, Len),
		playcount    = -1,
		rating       = ?RATING_UNRATED,
		duration     = proplists:get_value('Time',  Entry, 1),
		year         = proplists:get_value('Date',  Entry, <<>>),
		trackno      = proplists:get_value('Track', Entry, 0),
		audios       = new_tuple(proplists:get_value('Format', Entry,
							<<>>), Idx, Len),
		playlist_id  = proplists:get_value('Id', Entry, -1)}.

erlmpd_to_key(Entry) ->
	{normalize_key(proplists:get_value('Artist',   Entry, <<>>)),
	 normalize_strong(proplists:get_value('Album', Entry, <<>>)),
	 normalize_key(proplists:get_value('Title',    Entry, <<>>))}.

new_tuple(Value, Idx, Len) ->
	list_to_tuple([case I =:= Idx of
				true  -> normalize_always(Value);
				false -> <<>>
			end || I <- lists:seq(1, Len)]).

% Expensive normalization option required due to the fact that scrobbling or
% Maloja seem to mess with the supplied metadata.
normalize_key(Value) ->
	normalize_always(normalize_safe(Value)).

normalize_safe(Value) ->
	re:replace(string:replace(string:replace(
				lists:join(<<" ">>, string:lexemes(Value, " ")),
			"[", "("), "]", ")"),
		" \\(?feat\\.? .*$", "").

normalize_always(Value) ->
	unicode:characters_to_nfc_binary(Value).

normalize_strong(Value) ->
	normalize_always(re:replace(normalize_safe(Value), " \\(.*\\)$", "")).

% common cases handled by pattern matching
merge_tuple({<<>>, B},    {C,    <<>>}) -> {C, B};
merge_tuple({A,    <<>>}, {<<>>, D})    -> {A, D};
merge_tuple({A,    B},    {_C,   _D})   -> {A, B};
% else less efficient (?) but generic variant
merge_tuple(T1, T2) ->
	list_to_tuple([case TE of {<<>>, TE2} -> TE2; {TE1, _Any} -> TE1 end ||
			TE <- lists:zip(tuple_to_list(T1), tuple_to_list(T2))]).

convert_rating(1)      -> 0;
convert_rating(NotOne) -> NotOne * 10.

format_rating(?RATING_UNRATED) ->
	"- - -";
format_rating(?RATING_ERROR) ->
	"!ERR!";
format_rating(Rating) ->
	NumStars = Rating div 20,
	lists:duplicate(NumStars, $*) ++ lists:duplicate(5 - NumStars, $.).
