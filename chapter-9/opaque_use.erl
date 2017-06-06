-module(opaque_use).
-export([pull_person/1]).

pull_person(P) ->
	{person, {Fname, Lname}} = P,
	Fname.
