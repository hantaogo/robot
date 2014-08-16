-module(codekit).
-export([
	coding/2
	]).

-spec duplicate_list(list(), integer()) -> list().
duplicate_list([], _N) ->
	error;
duplicate_list(List = [X], N) when length(List) == 1 ->
	lists:duplicate(N, X);
duplicate_list(List, N) when N > length(List) ->
	duplicate_list_iter(List, N - length(List), List);
duplicate_list(List, N) ->
	{Result, _} = lists:split(N, List),
	Result.

duplicate_list_iter(_List, 0, Acc) ->
	Acc;
duplicate_list_iter([H|Rest], N, Acc) ->
	duplicate_list_iter(Rest ++ [H], N-1, Acc ++ [H]).

-spec coding(string(), string()) -> list().
coding(Content, Key) ->
	Keys = duplicate_list(Key, length(Content)),
	lists:zipwith(fun(X, Y) -> X bxor Y end, Content, Keys).