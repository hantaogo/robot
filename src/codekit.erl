-module(codekit).
-export([
	coding/2,
	encode/1,
	decode/1
	]).

-export([
	do_encode/2,
	do_decode/2,
	int8/1,
	uint8/1
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

-spec encode(binary()) -> binary().
encode(Bin) ->
	Content = binary_to_list(Bin), 
	X = uint8(random:uniform(256)),
	L = do_encode(Content, X),
	List = [X] ++ L,
	Data = list_to_binary(List),
	Len = size(Data), 
	<<Len:32/integer, Data/binary>>.

-spec do_encode(list(), integer()) -> list().
do_encode(L, X) ->
	% 添加校验码
	F = fun
		(A, C) ->
			{A, C + A}
	end,
	{_, C} = lists:mapfoldl(F, 0, L),
	CC = uint8(C),
	% io:format("C == ~p~n", [CC]),
	L1 = [CC] ++ L,
	% io:format("L1 == ~p~n", [L1]),
	% 打乱
	Fun = fun
		(A, I) ->
			Z = uint8(X*I),
			B = uint8(A+uint8(X*X*I)),
			D = uint8(B bxor Z),
			% io:format("A: ~p Z: ~p B: ~p D: ~p~n", [A, Z, B, D]),
			{D, I+1}
	end,
	{L2, _} = lists:mapfoldl(Fun, 1, L1),
	% io:format("L2 == ~p~n", [L2]),
	L2.

-spec decode(binary()) -> {ok, binary()} | error.
decode(<<Len:32/integer, Bin/binary>>) ->
	case Len == 1014001516 of
		true ->
			<<>>;
		false ->
			case Len > size(Bin) of
				true ->
					{error, asdjksasadklsalkdsadsadsa};
				false ->
					[X | Content] = binary_to_list(Bin),
					case do_decode(Content, X) of
						error ->
							error;
						{ok, L} ->
							{ok, list_to_binary(L)} 
					end
			end
	end.

-spec do_decode(list(), integer()) -> {ok, list()} | error.
do_decode(L, X) ->
	% 反打乱
	Fun = fun
		(A, I) ->
			Z = uint8(X*I),
			B = uint8(A bxor Z),
			D = uint8(B-uint8(X*X*I)),
			% io:format("A: ~p Z: ~p B: ~p D:~p~n", [A, D, Z, B]),
			{D, I+1}
	end,
	{L1, _} = lists:mapfoldl(Fun, 1, L),
	% io:format("L1 == ~p~n", [L1]),
	% 检测校验码是否相符
	F = fun
		(A, C) ->
			{A, C + A}
	end,
	L2 = tl(L1),
	% io:format("L2 == ~p~n", [L2]),
	{_, C} = lists:mapfoldl(F, 0, L2),
	CC = uint8(C),
	% io:format("C == ~p~n", [CC]),
	case CC == hd(L1) of
		true ->
			{ok, L2};
		false ->
			error
	end.

-spec int8(integer()) -> integer().
int8(X) ->
	M = 128,
	N = 256,
	if
		abs(X) >= N ->
			int8(X rem N);
		X >= M ->
			X rem M - M;
		X < -M ->
			M - ((-X) rem M);
		true ->
			X
	end.

-spec uint8(integer()) -> integer().
uint8(X) ->
	X band 255.