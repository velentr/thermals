APPNAME := thermals

APPFILE := ebin/$(APPNAME).app
APPSRC := src/$(APPNAME).app.src
APPDEPS := erts kernel stdlib

ERLC ?= erlc
ERL ?= erl
EFLAGS += -o ebin

CT ?= ct_run
# don't clutter up the top-level directory
CTFLAGS += -logdir logs
# include the compiled .beam files
CTFLAGS += -pa ebin
# ignore stdin (needed for build server)
CTFLAGS += -noshell

DIALYZER ?= dialyzer
PLT ?= .plt

SUITE ?= $(APPNAME)_SUITE

include src/src.mk

EBIN := $(addprefix ebin/,$(EMOD:=.beam))
ESRC := $(addprefix src/,$(EMOD:=.erl))

all: $(EBIN) $(APPFILE)

clean:
	rm -rf ebin logs test/*.beam

ct: $(EBIN) $(APPFILE) | logs
	$(CT) -suite $(SUITE) $(CTFLAGS)

dialyze: | $(PLT)
	$(DIALYZER) $(ESRC) --plt $(PLT)

sh: $(EBIN)
	$(ERL) -pa ebin

logs:
	mkdir -p logs

ebin:
	mkdir -p ebin

$(PLT):
	$(DIALYZER) --build_plt --output_plt $(PLT) --apps $(APPDEPS)

ebin/%.beam: src/%.erl | ebin
	$(ERLC) $(EFLAGS) $^

ebin/%.app: src/%.app.src | ebin
	cp -u $^ $@

.PHONY: all clean ct sh
