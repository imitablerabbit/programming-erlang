-module(hello).
-export([hello/0, hello/1]).

hello() -> io:format("Hello, world!").
hello(Name) -> io:format("Hello, ~s!", [Name]).
