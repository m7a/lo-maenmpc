-module(maenmpc_maloja).
-export([conn/1, foldl_scrobbles/3, query_playcount/3]).

conn(MalojaConf) ->
	{proplists:get_value(url, MalojaConf, none),
				proplists:get_value(key, MalojaConf, none)}.

foldl_scrobbles(_Callback, Acc, {none, none}) ->
	Acc;
foldl_scrobbles(Callback, Acc, {URL, Key}) ->
	% TODO z if this limit is exceeded need to either adjust Maloja to
	%      do the querying for us or query multiple pages or limit the
	%      time range!
	{ok, {_Status, _Headers, AllScrobblesRaw}} = httpc:request(
			io_lib:format("~s/scrobbles?key=~s&perpage=16777216",
			[URL, uri_string:quote(Key)])),
	lists:foldl(Callback, Acc, maps:get(<<"list">>,
				jiffy:decode(AllScrobblesRaw, [return_maps]))).

% TODO x Need to include album info otherwise this can return wrong results.
%        However, it is not that simple:
%        -> https://github.com/krateng/maloja/issues/301
%        -> https://github.com/krateng/maloja/issues/362
%        There could be a workaround by querying raw scrobbles somehow and
%        parsing-out the info that we want here, however it may be easier to
%        change the song metadata for now.
query_playcount(Trackartist, Title, {URL, Key}) ->
	Query = binary_to_list(iolist_to_binary(io_lib:format(
		"~s/trackinfo?key=~s&trackartist=~s&title=~s", [
			URL, uri_string:quote(Key),
			uri_string:quote(Trackartist),
			uri_string:quote(Title)
		]))),
	case httpc:request(Query) of
	{error, EDesc} ->
		{error, EDesc};
	{ok, {_Status, _Headers, InfoRaw}} ->
		Map = jiffy:decode(InfoRaw, [return_maps]),
		case maps:get(<<"scrobbles">>, Map, -1) of
		-1 ->
			EDsc = maps:get(<<"error">>, Map, unknown),
			case is_map(EDsc) andalso maps:get(<<"type">>, EDsc,
						unknown) =:=
						<<"entity_does_not_exist">> of
				true  -> {ok, 0};
				false -> {error, EDsc}
			end;
		Value1 ->
			{ok, Value1}
		end
	end.
