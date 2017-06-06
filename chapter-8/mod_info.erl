-module(mod_info).
-export([most_functions/0, most_common_function/0, unambiguous_functions/0]).
%-compile(export_all).

% most_functions will return a tuple consisting of the name of
% the module which contains the most functions, as well as how
% many functions that module has.
most_functions() -> 
	ModList = code:all_loaded(),
	most_functions(ModList, start, -1).
most_functions([], Mod, N) -> {Mod, N};
most_functions([{Mod, _File}|T], ModPrev, N) -> 
	case count_functions(Mod) of
		X when X > N; X =:= N -> 
			most_functions(T, Mod, X);	
		X when X =< N ->
			most_functions(T, ModPrev, N)
	end.
	
% count_functions returns the number of functions inside a module
% Mod.
count_functions(Mod) ->
	L = get_functions(Mod),
	length(L).

% get_functions return a list of all the functions from a module
get_functions(Mod) -> 
	Info = Mod:module_info(),
	[_Mod, {exports, L}, _Attr, _Comp, _Native, _MD5] = Info,
	L.

% most_common_function returns a tuple which contains the name of the
% function and how many occurences that function name has over all of
% the currently loaded modules.
most_common_function() ->
	ModList = code:all_loaded(),
	most_common_function(ModList, #{}).
most_common_function([], Map) -> most_popular_from_map(Map);
most_common_function([{Mod, _File}|T], Map) ->
	Functions = get_functions(Mod),
	F = [F || {F, _} <- Functions],
	M = count_list_in_map(F, Map),
	most_common_function(T, M).

unambiguous_functions() ->
	ModList = code:all_loaded(),
	unambiguous_functions(ModList, #{}).
unambiguous_functions([], Map) -> least_popular_from_map(Map);
unambiguous_functions([{Mod, _File}|T], Map) ->
	Functions = get_functions(Mod),
	F = [F || {F, _} <- Functions],
	M = count_list_in_map(F, Map),
	unambiguous_functions(T, M).



% count_list_in_map returns a map which contains the occurences of the
% elements inside the list
count_list_in_map([], X) -> X;
count_list_in_map([H|T], X) ->
	case maps:is_key(H, X) of 
		true ->
		       	#{ H := N } = X,
			count_list_in_map(T, X#{ H := N+1 });
		false ->
		       	count_list_in_map(T, X#{ H => 1 })
	end.

% most_popular_from_map determines which is the most popular element
% in a map and returns it in the form of a tuple {Key, Occurence}.
most_popular_from_map(M) ->
	List = maps:to_list(M),
	most_popular_from_map(List, {start, -1}).
most_popular_from_map([], Tup)  -> Tup;
most_popular_from_map([{_K1, V1}|T], {_K2, V2}) ->
	if V1 > V2   -> most_popular_from_map(T, {_K1, V1});
	   V1 =:= V2 -> most_popular_from_map(T, {_K1, V1});
	   V1 < V2   -> most_popular_from_map(T, {_K2, V2})		
	end.

% least_popular_from_map returns a list which contains all the functions
% which are only used once over all of the modules.
least_popular_from_map(M) -> 
	List = maps:to_list(M),
	[F || {F, N} <- List, N =:= 1].
