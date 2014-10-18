-module(t).

-export ([
	r/0,
	do/1
	]).

r() ->
	compile:file("src/"++atom_to_list(?MODULE)++".erl", [{outdir, "ebin"}]),
	code:purge(?MODULE),
	code:load_file(?MODULE).

do(cf) ->
	io:format("...country fight...~n"),
	bot:s(30, <<1, 110>>);

do({w, Content}) ->
	Words = utils:utf("-- " ++ Content),
	bot:s(105, <<8, 100, 0:16/integer, Words/binary, 0>>);

do(What) ->
	io:format("undefined -- do(~p)~n", [What]).
