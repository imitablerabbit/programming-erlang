-module(tuple_to_list).
-export([my_tuple_to_list/1]).

my_tuple_to_list({}) -> [];
my_tuple_to_list(T)  -> 
	my_tuple_to_list_helper(T, tuple_size(T), []).

my_tuple_to_list_helper(T, 0, Acc) -> Acc;
my_tuple_to_list_helper(T, N, Acc) -> 
	my_tuple_to_list_helper(T, N-1, [element(N, T)|Acc]).
