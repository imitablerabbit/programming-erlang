-module(opaque).
-export([create_person/2, print_person/1]).
-export_type([person/0]).

-type name() :: {string(), string()}.
-opaque person() :: {person, name()}.

-spec create_person(Fname, Lname) -> person() when
	  Fname :: string(),
	  Lname :: string().
-spec print_person(person()) -> ok.
-spec is_alpha(string()) -> boolean().

create_person(Fname, Lname) ->
	{person, {Fname, Lname}}.

print_person(P) ->
	{person, {Fname, Lname}} = P,
	io:format("Name: ~p ~p~n", [Fname, Lname]).


