-module(factorial).
-export([factorial/1, test_factorial/0]).

-spec factorial(non_neg_integer()) -> pos_integer().

factorial(0) -> 1;
factorial(N) -> factorial(N-1) * N.

test_factorial() ->
	factorial(5),
	factorial(0),
	factorial(0.5),
	factorial(-2).
	
