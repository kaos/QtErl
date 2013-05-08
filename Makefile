ERL = erl

QMAKE = qmake
QMAKE_SPEC = win32-g++

QTERL_LIB_EXT = dll
QTERL_LIB_TARGET = release

QTERL_LIB = qterl.$(QTERL_LIB_EXT)

all: lib ebin

werl: all
	werl -pa ebin

.PHONY: check
check:
	$(MAKE) -C test

.PHONY: lib
lib: priv/$(QTERL_LIB)

priv/$(QTERL_LIB): priv/build/c_src/qterl/$(QTERL_LIB_TARGET)/$(QTERL_LIB)
	@echo Copy new $(QTERL_LIB) from build dir
	@cp $< $@

priv/build/c_src/qterl/$(QTERL_LIB_TARGET)/$(QTERL_LIB): priv/build/Makefile $(wildcard c_src/qterl/* c_src/libqte/*)
	@$(MAKE) -C $(dir $<) $(QTERL_LIB_TARGET)

priv/build/Makefile: QtErl.pro
	@mkdir -p $(dir $@)
	@cd $(dir $@) && $(QMAKE) -spec $(QMAKE_SPEC) -o Makefile ../../$<

.PHONY: ebin
ebin: $(wildcard src/*.erl) $(wildcard include/*.hrl)
	@mkdir -p ebin
	@$(ERL) -make
