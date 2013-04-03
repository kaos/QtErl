ERL = erl

all: eunit

.PHONY: eunit
eunit: ebin
	@$(ERL) -pa ebin -noshell -eval "eunit:test(qte_xml)" -s erlang halt

.PHONY: ebin
ebin: $(wildcard ../src/*.erl) $(wildcard ../include/*.hrl)
	@mkdir -p ebin
	@$(ERL) -make
