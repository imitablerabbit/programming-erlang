-module(try_test).
-export([generate_expression/1, demo1/0, catcher/1, catcher1/1]).

generate_expression(1) -> a;
generate_expression(2) -> throw(a);
generate_expression(3) -> exit(a);
generate_expression(4) -> {'EXIT', a};
generate_expression(5) -> error(a).

demo1() -> [catcher(N) || N <- [1, 2, 3, 4, 5]].

catcher(N) ->
	try generate_expression(N) of
		Val -> {N, normal, Val}
	catch
		throw:X -> {N, caught, thrown, X};
		exit:X  -> {N, caught, exited, X};
		error:X -> {N, caught, error, X}
	end.

catcher1(N) ->
	try generate_expression(N) of
		Val -> {N, normal, Val}
	catch
		throw:X -> {{"Unforuntely there was an error"}, {N, caught, thrown, X}};
		exit:X  -> {{"Unforuntely there was an error"}, {N, caught, exited, X}};
		error:X -> {{"Unforuntely there was an error"}, {N, caught, error, X}}
	end.
