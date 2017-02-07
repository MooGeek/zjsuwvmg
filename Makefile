export REBAR ?= $(shell echo `pwd`/rebar3)


all: build

clean:
	$(REBAR) clean

build:
	$(REBAR) compile

check:
	$(REBAR) xref
	$(REBAR) dialyzer


.PHONY: all clean build check
