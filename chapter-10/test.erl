-module(test).
-export([hello/0, away/1]).

hello() -> 
    io:format("hello Mark, how are you?").

away(Time) -> 
    io:format("Mark is away and will be back in ~w minutes~n",
              [Time]).
