-module(mybinary).
-export([reverse_bytes/1, term_to_packet/1, packet_to_term/1, reverse_bits/1]).

reverse_bytes(Bin) -> reverse_bytes_helper(Bin, <<>>).

reverse_bytes_helper(<<>>, Acc) -> Acc;
reverse_bytes_helper(<<H:8, T/binary>>, Acc) -> 
	reverse_bytes_helper(T, <<H:8, Acc/binary>>).	

term_to_packet(Term) ->
	B = term_to_binary(Term),
	N = byte_size(B),
	<<N:4/integer-unit:8, B:N/binary-unit:8>>.

packet_to_term(Packet) ->
	<<N:4/integer-unit:8, B:N/binary-unit:8>> = Packet,
	binary_to_term(B).


reverse_bits(Bin) -> reverse_bits_helper(Bin, <<>>).

reverse_bits_helper(<<>>, Acc) -> Acc;
reverse_bits_helper(<<H:1, T/bitstring>>, Acc) ->
	reverse_bits_helper(T, <<H:1, Acc/bitstring>>).
