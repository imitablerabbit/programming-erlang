-module(math_functions).
-export([even/1, odd/1, filter/2, split/1, split_acc/1]).

even(N) when N rem 2 =:= 0 -> true;
even(_) -> false.

odd(N) when N rem 2 =:= 1 -> true;
odd(_) -> false.

filter(F, L) -> [X || X <- L, F(X) =:= true].

% Split funciton with inefficient
split(L) -> 
	Even = filter(fun even/1, L),
	Odd = filter(fun odd/1, L),
	{Even, Odd}.

% Start of split function with accumilator
split_acc(L) -> split_acc_helper(L, [], []). 
	
% Split helper function
split_acc_helper([], EAcc, OAcc) -> 
	{lists:reverse(EAcc), lists:reverse(OAcc)};
split_acc_helper([H|T], EAcc, OAcc) ->
       case even(H) of
	       true ->
		       split_acc_helper(T, [H|EAcc], OAcc);
	       false ->
		       split_acc_helper(T, EAcc, [H|OAcc])
	end.
	    
