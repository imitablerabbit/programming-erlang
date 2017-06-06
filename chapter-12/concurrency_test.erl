-module(concurrency_test).
-export([start/2]).

start(AnAtom, Fun) ->
        case whereis(AnAtom) of
            undefined ->
                register(AnAtom, spawn(Fun));
            _ ->
                {error, registered}
        end.
