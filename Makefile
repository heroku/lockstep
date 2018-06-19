REBAR := $(shell which rebar3)

all: compile

compile: get-deps
	$(REBAR) compile

clean:
	$(REBAR) clean

get-deps:
	$(REBAR) get-deps

del-deps:
	$(REBAR) delete-deps
