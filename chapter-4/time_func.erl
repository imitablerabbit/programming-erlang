-module(time_func).
-export([my_time_func/1, my_date_string/0]).

my_time_func(F) -> 
	X = erlang:monotonic_time(),
	F(),
	erlang:monotonic_time() - X.

my_date_string() ->
	{Year, Month, Day} = date(),
	io:format("~B/~B/~B~n", [Day, Month, Year]).

