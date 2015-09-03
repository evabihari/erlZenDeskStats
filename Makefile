#
# Compilation:
#
# - all (default): get and compile dependencies; compile the app
# - compile: compiles the app , including its dependencies
# - quickcompile: compiles only the app

ERLFLAGS= -pa $(CURDIR)/.eunit -pa $(CURDIR)/ebin -pa $(CURDIR)/deps/*/ebin

DEPS_PLT=$(CURDIR)/.dialyzer_plt
DEPS=erts kernel stdlib

# =============================================================================
# Verify that the programs we need to run are installed on this system
# =============================================================================
ERL = $(shell which erl)

ifeq ($(ERL),)
$(error "Erlang not available on this system")
endif

REBAR=$(shell which rebar)

ifeq ($(REBAR),)
$(error "Rebar not available on this system")
endif

.PHONY: all compile doc clean test dialyzer typer shell distclean pdf \
  update-deps clean-common-test-data rebuild

all: deps compile dialyzer test


ERLANG_DIALYZER_APPS = \
	asn1 \
	erts \
	hipe \
	kernel \
	stdlib \
	compiler \
	tools \
	crypto \
	ssl \
	runtime_tools \
	snmp otp_mibs \
	syntax_tools\
	test_server \
	os_mon \
	public_key \
	inets \
	xmerl \
	sasl \
	mnesia \
	et \
	debugger \
	common_test \
	observer \
	webtool \
	wx \
	ssh \
	edoc

DIALYZER_DEPS = \
	lager 

# Default target
all: deps compile
.PHONY: all

deps: rebar.config
	$(REBAR) get-deps update-deps
	touch deps

doc:
	$(REBAR) doc skip_deps=true
.PHONY: doc

compile: deps
	$(REBAR) compile
.PHONY: compile

quickcompile: 
	$(REBAR) compile skip_deps=true
.PHONY: quickcompile

xcompile: 
	$(REBAR) compile xref skip_deps=true
.PHONY: xcompile

purge:
	git ls-files -o --directory -x .dialyzer_plt | xargs rm -r
.PHONY: purge

clean:
	$(REBAR) clean
.PHONY: clean

distclean: clean 
	$(REBAR) delete-deps
	rm -rf deps
.PHONY: distclean


$(DEPS_PLT): | deps/lager/ebin/lager.beam
	@echo Building local plt at $(DEPS_PLT)
	@echo
	dialyzer --output_plt $(DEPS_PLT) --build_plt \
	   --apps $(DEPS) $(ERLANG_DIALYZER_APPS) -r deps

deps/lager/ebin/lager.beam:
	$(error Run 'make compile' first in order to build the dependencies) 

dialyzer: $(DEPS_PLT)
	dialyzer --verbose --fullpath --plt $(DEPS_PLT) -Wrace_conditions -r ./ebin

typer:
	typer --plt $(DEPS_PLT) -r ./src

xref:
#	r=0; for i in $$(ls apps); do $(REBAR) xref apps=$$i skip_deps=true; res=$$?; if [ "$$res" != 0 ]; then r=$$res; fi; done; exit $$r
	$(REBAR) xref skip_deps=true

.PHONY: xref

# this rule is for %-rules to depend on
PHONY:
	@true
.PHONY: PHONY

