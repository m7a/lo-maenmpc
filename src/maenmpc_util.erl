-module(maenmpc_util).
-export([divide_list_by_pred/2]).

% Split list by predicate and return a tuple with exactly two lists
% First list contains all items before the predicate matches (excl)
% Second list contains all items after the predicate matches (incl)
divide_list_by_pred(Predicate, List) ->
	divide_list_by_pred(Predicate, List, {[], []}).
divide_list_by_pred(_Predicate, [], {PreAcc, PostAcc}) ->
	{lists:reverse(PreAcc), lists:reverse(PostAcc)};
divide_list_by_pred(Predicate, [H|T], {PreAcc, PostAcc}) ->
	case PostAcc =/= [] orelse Predicate(H) of
	true  -> divide_list_by_pred(Predicate, T, {PreAcc, [H|PostAcc]});
	false -> divide_list_by_pred(Predicate, T, {[H|PreAcc], PostAcc})
	end.
