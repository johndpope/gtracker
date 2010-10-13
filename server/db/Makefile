REBAR=../tools/rebar
all: build_deps
	${REBAR} compile

test:
	${REBAR} eunit

build_deps:
	${REBAR} get-deps

release:
	mkdir -p deps/gtracker_db
	cp -r ebin deps/gtracker_db
	${REBAR} generate

clean:
	${REBAR} clean
