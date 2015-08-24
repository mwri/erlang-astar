ERLC = erlc
ERL  = erl


all: compile

compile:
	rebar --verbose compile

docs:
	rebar doc skip_deps=true

clean:
	rebar clean
	rm -f doc/*.html doc/edoc-info doc/erlang.png doc/stylesheet.css dialyzer.log

otp.plt:
	dialyzer --build_plt --output_plt otp.plt --apps \
		erts kernel stdlib compiler crypto eunit \
			| fgrep -v dialyzer.ignore

astar.plt: compile otp.plt
	dialyzer --add_to_plt --plt otp.plt --output_plt astar.plt ebin

dialyzer: astar.plt
	dialyzer -o dialyzer.log --add_to_plt --plt otp.plt --output_plt astar.plt ebin
	dialyzer --plt astar.plt -o dialyzer.log ebin

test:
	rebar skip_deps=true eunit

node: compile
	rebar generate -fv

.PHONY: test
