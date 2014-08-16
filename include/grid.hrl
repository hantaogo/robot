-record(grid, {width, height, cells, walks}).

-type grid() :: {grid, integer(), integer(), array(), array()}.

-type cell() :: {integer(), integer(), integer(), integer(), integer(), integer()}.
-type cells() :: [cell()].

-type point() :: {integer(), integer()}.
-type points() :: [point()].

-record(cell, {x, y, v, g, h, px, py}).
