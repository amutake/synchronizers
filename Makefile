REBAR=rebar3

all: compile check test edoc

compile:
	${REBAR} compile

test: eunit ct cover

eunit:
	${REBAR} eunit

ct:
	${REBAR} ct

cover:
	${REBAR} cover

edoc:
	${REBAR} edoc

check: xref dialyzer

xref:
	${REBAR} xref

dialyzer:
	${REBAR} dialyzer

clean:
	${REBAR} clean
