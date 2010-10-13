REBAR=../tools/rebar
all: build_deps
	${REBAR} compile

test:
	${REBAR} eunit

build_deps:
	${REBAR} get-deps

release:
	mkdir -p deps/gtracker
	cp -r ebin include deps/gtracker
	${REBAR} generate

clean:
	${REBAR} clean
