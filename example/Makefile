ERL = erl

all:
	@mkdir -p ebin
	@$(ERL) -make

run: all
	@$(ERL) -detached -pa ../ebin -pa ebin -s addrbook start &

debug: all
	@$(ERL) -pa ../ebin -pa ebin -s addrbook start
