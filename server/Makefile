all: build_deps
	./rebar compile

test:
	./rebar eunit

build_deps:
	./rebar get-deps

release:
	mkdir -p deps/gtracker
	cp -r ebin include deps/gtracker
	./rebar generate

clean:
	./rebar clean
