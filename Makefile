QMAKE = qmake
QMAKE_SPEC = win32-g++

QTERL_LIB_EXT = dll
QTERL_LIB_TARGET = release

QTERL_LIB = QtErl.$(QTERL_LIB_EXT)

all: lib ebin

.PHONY: lib
lib: priv/$(QTERL_LIB)

priv/$(QTERL_LIB): priv/c_objs/$(QTERL_LIB_TARGET)/$(QTERL_LIB)
	@cp $< $@

priv/c_objs/$(QTERL_LIB_TARGET)/$(QTERL_LIB): priv/c_objs/Makefile.$(QTERL_LIB_TARGET)
	@$(MAKE) -C $(dir $<) $(QTERL_LIB_TARGET)

priv/c_objs/Makefile.$(QTERL_LIB_TARGET): priv/c_objs/Makefile

priv/c_objs/Makefile: QtErl.pro
	@mkdir -p $(dir $@)
	@cd $(dir $@) && $(QMAKE) -spec $(QMAKE_SPEC) -o Makefile ../../$<

.PHONY: ebin
ebin: $(wildcard src/*.erl) $(wildcard include/*.hrl)
	@mkdir -p ebin
	@erl -make
