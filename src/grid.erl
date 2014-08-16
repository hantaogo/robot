-module(grid).
-include("grid.hrl").

-export ([
	make/2,
	make/1,
	make_from_file/1,
	cells/1,
	get/2,
	set/3,
	size/1,
	random_walkable/1,
	is_in/2,
	is_block/1,
	around/2,
	find_path/3
	]).

-define(BLOCK_VALUE, 0).

-spec make(integer(), integer()) -> grid().
make(W, H) ->
	#grid{width=W, height=H, cells=array:new(W*H, [fixed]), walks=array:new()}.

-spec make(binary()) -> grid().
make(Data) ->
	Lines = string:tokens(binary_to_list(Data), "\r\n"),
	W = length(hd(Lines)),
	H = length(Lines),
	F = fun(Line, {Y, Cells, Walks}) ->
			Fun = 
				fun(C, {X, Cs, Ws}) ->
					[S] = io_lib:format("~c", [C]),
					V = list_to_integer(S),
					Cell = #cell{x=X,y=Y,v=V,g=0,h=0,px=-1,py=-1},
					case V == ?BLOCK_VALUE of
						true ->
							{X+1, array:set(Y*W+X, Cell, Cs), Ws};
						false ->
							{X+1, array:set(Y*W+X, Cell, Cs), [{Cell#cell.x, Cell#cell.y} | Ws]}
					end
				end,
			{_, NewCells, NewWalks} = lists:foldl(Fun, {0, Cells, Walks}, Line),
			{Y+1, NewCells, NewWalks}
		end,
	{_, Array, Walks} = lists:foldl(F, {0, array:new(W*H, [fixed]), []}, Lines),
	#grid{width=W, height=H, cells=Array, walks=array:from_list(Walks)}.

-spec make_from_file(string()) -> grid().
make_from_file(Filename) ->
	{ok, Data} = file:read_file(Filename),
	grid:make(Data).

-spec cells(grid()) -> array().
cells(#grid{cells=Cells}) ->
	Cells.

-spec get(point(), grid()) -> cell() | undefined.
get({X,Y}=P, #grid{width=W, cells=Cells}=G) ->
	case is_in(P, G) of
		true ->
			array:get(Y*W+X, Cells);
		false ->
			undefined
	end.

-spec set(point(), cell(), grid()) -> grid().
set({X,Y}, C, #grid{width=W, cells=Cells}=Grid) ->
	Grid#grid{cells=array:set(Y*W+X, C, Cells)}.

-spec size(grid()) -> {integer(), integer()}.
size(Grid) ->
	{Grid#grid.width, Grid#grid.height}.

-spec random_walkable(grid()) -> point().
random_walkable(#grid{walks=Walks}) ->
	I = random:uniform(array:size(Walks))-1,
	array:get(I, Walks).

-spec is_in(point(), grid()) -> boolean().
is_in({X,Y}, #grid{width=W, height=H}) ->
	X >= 0 andalso X < W andalso Y >= 0 andalso Y < H.

-spec is_block(cell()) -> boolean().
is_block(C) ->
	C#cell.v == ?BLOCK_VALUE.

-spec around(point(), grid()) -> cells().
around({X,Y}, G) ->
	Ps = [
		{-1,-1},{0,-1},{1,-1},
		{-1, 0},       {1, 0},
		{-1, 1},{0, 1},{1, 1}
		],
	F = fun(P, L) ->
			N = grid:get(P, G),
			case N == undefined orelse is_block(N) of
				true ->
					L;
				false ->
					[N | L]
			end
		end,
	lists:foldl(F, [], lists:map(fun({Dx,Dy})-> {X+Dx,Y+Dy} end, Ps)).

-spec best(cells()) -> point().
best(L) ->
	F = fun(#cell{g=G, h=H}=A, #cell{g=Cg, h=Ch}=B) ->
			case G + H < Cg + Ch of
				true ->
					A;
				false ->
					B
			end
		end,
	lists:foldl(F, hd(L), tl(L)).

-spec gscore(cell(), cell()) -> integer().
gscore(_, _) ->
	1.

-spec hscore(cell(), cell()) -> integer().
hscore(#cell{x=X1, y=Y1}, #cell{x=X2, y=Y2}) ->
	max(abs(X1-X2), abs(Y1-Y2)).
	
-spec find_path(point(), point(), grid()) -> points().
find_path(A, B, G) ->
	case A /= B andalso is_in(A, G) andalso is_in(B, G) of
		true ->
			find_path_iter([grid:get(A, G)], [], grid:get(B, G), G);
		false ->
			[]
	end.

find_path_iter([], _, _, _) ->
	[];
find_path_iter(OpenList, ClosedList, B, G) ->
	C = best(OpenList),
	O2 = OpenList--[C],
	C2 = [C | ClosedList],
	% io:format("C: ~p~nOpen: ~p~nClose: ~p~n~n", [C, O2, C2]),
	case C#cell.x == B#cell.x andalso C#cell.y == B#cell.y of
		true ->
			arrive(C, G);
		_ ->
			F = fun(P) ->
					not lists:member(P, ClosedList)
				end,
			L = lists:filter(F, around({C#cell.x, C#cell.y}, G)),
			{O3, G2} = iter_neighbors(C, B, G, O2, C2, L),
			find_path_iter(O3, C2, B, G2)
	end.

arrive_iter(C, G, L) ->
	case C#cell.px == -1 orelse C#cell.py == -1 of
		true ->
			L;
		false ->
			P = grid:get({C#cell.px, C#cell.py}, G),
			arrive_iter(P, G, [{P#cell.x, P#cell.y} | L])
	end.

-spec arrive(cell(), grid()) -> points().
arrive(C, G) ->
	arrive_iter(C, G, [{C#cell.x, C#cell.y}]).

iter_neighbors(_, _, G, Open, _, []) ->
	{Open, G};
iter_neighbors(C, B, G, Open, Close, [H|T]) ->
	iter_neighbors_internal(C, B, G, Open, Close, H, T).

iter_neighbors_internal(_, _, G, Open, _, _, []) ->
	{Open, G};
iter_neighbors_internal(C, B, G, Open, Close, W, L) ->
	case lists:member(W, Open) of
		true ->
			GG = C#cell.g + gscore(W, C),
			case GG + W#cell.h < C#cell.g + C#cell.h of
				true ->
					G2 = grid:set({W#cell.x, W#cell.y}, W#cell{g=GG, px=C#cell.x, py=C#cell.y}),
					iter_neighbors_internal(C, B, G2, Open, Close, hd(L), tl(L));
				false ->
					iter_neighbors_internal(C, B, G, Open, Close, hd(L), tl(L))
			end;
		false ->
			Q = W#cell{g=C#cell.g + gscore(W, C), h=hscore(W, B), px=C#cell.x, py=C#cell.y},
			G2 = grid:set({W#cell.x, W#cell.y}, Q, G),
			iter_neighbors_internal(C, B, G2, [Q | Open], Close, hd(L), tl(L))
	end.