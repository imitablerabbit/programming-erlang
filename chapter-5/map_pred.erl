-module(map_pred).
-export([map_search_pred/2]).

map_search_pred(Map, Pred) ->
	map_search_pred_helper(Map, Pred, maps:to_list(Map)).

map_search_pred_helper(_, _, []) -> error;
map_search_pred_helper(Map, Pred, [{K,V}|T]) ->
	case Pred(K, V) of
		true  -> {K, V};
		false -> map_search_pred_helper(Map, Pred, T)
	end.
