REBAR=../tools/rebar
all: build_deps
	${REBAR} compile jobs=1

.PHONY : stest

test:
	${REBAR} eunit jobs=1

stest:
	erlc -o ebin -I ../include stest/gtracker_test.erl
	rm -rf ebin/Mnesia*
	cd ebin && ERL_LIBS=../deps erl -noshell -sname gtracker2_fake -boot start_sasl -config app.config -s gtracker_test test -s init stop

build_deps:
	${REBAR} get-deps

release:
	mkdir -p deps/gtracker2
	cp -r ebin ../include deps/gtracker2
	${REBAR} generate

clean:
	${REBAR} clean

