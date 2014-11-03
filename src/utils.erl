-module (utils).

-export ([
	seconds1970/0,
	utf/1,
	md5/1,
	i2s/1,
	hashcode/1,
	int32/1,
	random_from_list/1,
	test/0
	]).

-spec seconds1970() -> integer().
seconds1970() ->
	{{Y, M, D}, T} = calendar:local_time(),
	calendar:datetime_to_gregorian_seconds({{Y-1970, M, D}, T}).

-spec utf(string()) -> binary().
utf(Str) ->
	Bin = unicode:characters_to_binary(Str),
	Len = size(Bin),
	<<Len:16/integer, Bin/binary>>.

-spec md5(string()) -> string().
md5(S) ->
	Md5_bin = erlang:md5(S),
	Md5_list = binary_to_list(Md5_bin),
	lists:flatten(list_to_hex(Md5_list)).

list_to_hex(L) ->
	lists:map(fun(X) -> int_to_hex(X) end, L).

int_to_hex(N) when N < 256 ->
	[hex(N div 16), hex(N rem 16)].

hex(N) when N < 10 ->
	$0+N;
hex(N) when N >= 10, N < 16 ->
	$a + (N-10). 

-spec i2s(integer()) -> string().
i2s(X) ->
	[S] = io_lib:format("~p", [X]),
	S.

-spec int32(integer()) -> integer().
int32(X) ->
	M = 2147483648,
	N = 4294967296,
	if
		abs(X) >= N ->
			X rem N;
		X >= M ->
			X rem M - M;
		X < -M ->
			M - ((-X) rem M);
		true ->
			X
	end.

-spec hashcode(string()) -> integer().
hashcode("") ->
	0;
hashcode(S) ->
	F = fun(C, H)-> 
			X = int32(31*H) + C,
			int32(X)
		end,
	lists:foldl(F, 0, S).

random_from_list(L) ->
	A = array:from_list(L),
	I = random:uniform(array:size(A))-1,
	array:get(I, A).

test() ->
	% User = "gm001",
	% Time = 1407405869,
	% K = "XCWXCWXCWXXX",
	% md5(User++utils:i2s(Time)++K).
	hashcode("qingZhuGu").
