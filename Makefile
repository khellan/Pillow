ERL          ?= erl
EBIN_DIRS    := $(wildcard deps/*/ebin)
APP          := pillow
APP2          := pillow_routing_table

all: ebin erl ebin/$(APP).app ebin/$(APP2).app
all_boot: all make_boot

erl:
	@$(ERL) -pa $(EBIN_DIRS) -noinput +B \
	  -eval 'case make:all() of up_to_date -> halt(0); error -> halt(1) end.'

docs:
	@erl -noshell -run edoc_run application '$(APP)' '"."' '[]'
	@erl -noshell -run edoc_run application '$(APP2)' '"."' '[]'

clean: 
	@echo "removing:"
	@rm -fv ebin/*.beam ebin/*.app

make_boot:
	(cd ebin; erl -pa ebin -noshell \
	  -run make_boot write_scripts rest_app)

ebin:
	@mkdir ebin

ebin/$(APP).app: src/$(APP).app
	@cp -v src/$(APP).app $@

ebin/$(APP2).app: src/$(APP2).app
	@cp -v src/$(APP2).app $@
