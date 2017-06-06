.SUFFIXES: .erl .beam

# Double suffix rule to compile all .erl files into 
# a .beam file, $< expands to the source file
# Can be replaced by
#%.beam: %.erl
#	erlc -W $<
.erl.beam:
	erlc -W $<

ERL = erl -boot start_clean

# The names of the files/modules to compile
MODS = test

all: compile
	${ERL} -pa '/home/imitablerabbit/Documents/erlang/programming-erlang/chapter-10' -s test hello

compile: ${MODS:%=%.beam}

clean:
	rm -rf *.beam erl_crash.dump
