SRC=examples.c controller.c cards.c events.c time_profiles.c tasks.c
BINDINGS=../../bindings/c
LIB=../../lib
GO=GODEBUG=cgocheck=2

CONTROLLER=405419896
CARD=--card 10058400
DOOR=--door 3

FIRSTCARD ?= 08:30,16:45,normally open, firstcard only, [Mon,Tues,Fri]

ifeq ($(OS),Windows_NT)
	DLL=set PATH=$(LIB)
else
    UNAME = $(shell uname -s)
    ifeq ($(UNAME),Darwin)
        DLL=export DYLD_LIBRARY_PATH=$(LIB)
    else 
        DLL=export LD_LIBRARY_PATH=$(LIB)
    endif
endif

CMD=$(GO) && $(DLL) && ./examples

format: $(SRC)
	clang-format -i *.c
	clang-format -i $(BINDINGS)/src/*.c
	clang-format -i $(BINDINGS)/include/*.h

build: format 
	gcc -Wall -o examples $(SRC) $(BINDINGS)/src/uhppoted.c -I$(BINDINGS)/include -I$(LIB) -L$(LIB) -luhppoted

usage: build
	$(CMD)

help: build
	$(CMD) help

invalid: build
	$(CMD) mystery-meat

get-controllers: build
	$(CMD) get-controllers

get-controller: build
	$(CMD) get-controller --controller $(CONTROLLER)

set-address: build
	$(CMD) set-address --controller $(CONTROLLER) --ip-address 192.168.1.25 --subnet-mask 255.255.255.0 --gateway-address 192.168.0.1

get-status: build
	$(CMD) get-status --controller $(CONTROLLER)

get-time: build
	$(CMD) get-time --controller $(CONTROLLER)

set-time: build
	$(CMD) set-time --controller $(CONTROLLER)

get-listener: build
	$(CMD) get-listener --controller $(CONTROLLER)

set-listener: build
	$(CMD) set-listener --controller $(CONTROLLER) --listener-address 192.168.1.125:60001 --interval 13

get-door-control: build
	$(CMD) get-door-control --controller $(CONTROLLER) $(DOOR)

set-door-control: build
	$(CMD) set-door-control --controller $(CONTROLLER) $(DOOR)

open-door: build
	$(CMD) open-door --controller $(CONTROLLER) $(DOOR)

get-cards: build
	$(CMD) get-cards --controller $(CONTROLLER)

get-card: build
	$(CMD) get-card --controller $(CONTROLLER) $(CARD)

get-card-by-index: build
	$(CMD) get-card-by-index --controller $(CONTROLLER) --card-index 3

put-card: build
	$(CMD) put-card --controller $(CONTROLLER)

delete-card: build
	$(CMD) delete-card --controller $(CONTROLLER)

delete-cards: build
	$(CMD) delete-cards --controller $(CONTROLLER)

get-event-index: build
	$(CMD) get-event-index --controller $(CONTROLLER)

set-event-index: build
	$(CMD) set-event-index --controller $(CONTROLLER) --event-index 25

get-event: build
	$(CMD) get-event --controller $(CONTROLLER) --event-index 33

record-special-events: build
	$(CMD) record-special-events --controller $(CONTROLLER)

get-time-profile: build
	$(CMD) get-time-profile --controller $(CONTROLLER) --time-profile 29

set-time-profile: build
	$(CMD) set-time-profile --controller $(CONTROLLER)

clear-time-profiles: build
	$(CMD) clear-time-profiles --controller $(CONTROLLER)

add-task: build
	$(CMD) add-task --controller $(CONTROLLER)

refresh-tasklist: build
	$(CMD) refresh-tasklist --controller $(CONTROLLER)

clear-tasklist: build
	$(CMD) clear-tasklist --controller $(CONTROLLER)

set-pc-control: build
	$(CMD) set-pc-control --controller $(CONTROLLER)

set-interlock: build
	$(CMD) set-interlock --controller $(CONTROLLER)

activate-keypads: build
	$(CMD) activate-keypads --controller $(CONTROLLER)

set-door-passcodes: build
	$(CMD) set-door-passcodes --controller $(CONTROLLER) $(DOOR)

get-antipassback: build
	$(CMD) get-antipassback --controller $(CONTROLLER)

set-antipassback: build
	$(CMD) set-antipassback --controller $(CONTROLLER) --antipassback "(1,3):(2,4)"

set-firstcard: build
	$(CMD) set-firstcard --controller $(CONTROLLER) --firstcard $(FIRSTCARD)

restore-default-parameters: build
	$(CMD) restore-default-parameters $(CONTROLLER)

listen: build
	$(CMD) listen

listen-debug: format
	gcc -Wall -o examples $(SRC) $(BINDINGS)/src/uhppoted.c -I$(BINDINGS)/include -I$(LIB)/debug -L$(LIB)/debug -luhppoted
	$(GO) && export DYLD_LIBRARY_PATH=$(LIB)/debug && ./examples listen

debug: format
	gcc -Wall -o examples $(SRC) $(BINDINGS)/src/uhppoted.c -I$(BINDINGS)/include -I$(LIB)/debug -L$(LIB)/debug -luhppoted
	$(GO) && export DYLD_LIBRARY_PATH=$(LIB)/debug && ./examples listen
