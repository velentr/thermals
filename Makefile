APPNAME := thermals

APPFILE := ebin/$(APPNAME).app
APPSRC := src/$(APPNAME).app.src

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

SUITE ?= $(APPNAME)_SUITE

include src/src.mk

EBIN := $(addprefix ebin/,$(ESRC:=.beam))

all: $(EBIN) $(APPFILE)

shell: $(EBIN)
	$(ERL) -pa ebin

ct: $(EBIN) $(APPFILE) | logs
	$(CT) -suite $(SUITE) $(CTFLAGS)

logs:
	mkdir -p logs

ebin:
	mkdir -p ebin

ebin/%.beam: src/%.erl | ebin
	$(ERLC) $(EFLAGS) $^

ebin/%.app: src/%.app.src | ebin
	cp -u $^ $@

clean:
	rm -rf ebin logs test/*.beam
