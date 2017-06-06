-module(fizzbuzz).
-export([fizzbuzz/1, fizzbuzz_list/1, fizzbuzz_list/2]).

fizzbuzz(N) ->
	if 
		N rem 10 == 0 -> fizzbuzz;
		N rem 3 == 0 -> buzz;
		N rem 2 == 0 -> fizz;
		true -> none
	end.

fizzbuzz_list(N) ->	fizzbuzz_list(N, []).
fizzbuzz_list(0, L) -> L;
fizzbuzz_list(N, L) -> 
	fizzbuzz_list(N-1, [{N, fizzbuzz(N)}|L]).
