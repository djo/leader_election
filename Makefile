compile:
	rebar compile skip_deps=true

compile-all:
	rebar compile

get-deps:
	rebar get-deps

clean:
	rebar clean skip_deps=true
	rm -f erl_crash.dump

clean-all:
	rebar clean
	rm -f erl_crash.dump

test:	compile
	rebar eunit skip_deps=true

d:
	dialyzer --src -I include src test

run:
	./start.sh 1@127.0.0.1
