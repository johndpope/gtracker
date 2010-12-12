REBAR=../tools/rebar
all: build_deps
	${REBAR} compile jobs=1

test:
	${REBAR} eunit

stest:
	erlc -o ebin -I ../include stest/gtracker_test.erl
	rm -rf ebin/Mnesia.gtracker_db_fake@*
	cd ebin && ERL_LIBS=../deps erl -noshell -sname gtracker_db_fake -boot start_sasl -config app.config -s gtracker_test test -s init stop

build_deps:
	${REBAR} get-deps

release:
	mkdir -p deps/gtracker_db
	cp -r ebin deps/gtracker_db
	${REBAR} generate

clean:
	${REBAR} clean
