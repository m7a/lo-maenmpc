-module(maenmpc_util).
-export([directed_search/3]).

directed_search(forward, Item, List) ->
	 lists:search(fun(El) -> El > Item end, List);
directed_search(backward, Item, List) ->
	lists:search(fun(El) -> El < Item end, lists:reverse(List)).

