all:
	@rebar get-deps compile

edoc:
	@rebar skip_deps=true doc

check:
	@rm -rf .eunit
	@mkdir -p .eunit
	@dialyzer -pa deps/mondemand/ebin -Wno_undefined_callbacks -Wno_opaque -Wno_return --src src
	@rebar skip_deps=true eunit

clean:
	@rebar clean

maintainer-clean:
	@rebar clean
	@rebar delete-deps
	@rm -rf deps
	@rm -rf tmp
	@rm -rf log
