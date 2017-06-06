-module(json_map).
-export([read_json_file/1, convert_json_file_to_map/1]).

% Returns the Binary from a file and extracting it from the tuple
read_json_file(Filename) -> 
	{ok, Bin} = file:read_file(Filename),
	Bin.

convert_json_file_to_map(Filename) ->
	Bin = read_json_file(Filename),
	jsx:decode(Bin, [return_maps]).
