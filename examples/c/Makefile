SRC=examples.c device.c cards.c events.c time_profiles.c tasks.c
BINDINGS=../../bindings/c
LIB=../../lib
GO=GODEBUG=cgocheck=2
CMD=./examples

ifeq ($(OS),Windows_NT)
	DLL=export DYLD_LIBRARY_PATH=$(LIB)
else
    UNAME = $(shell uname -s)
    ifeq ($(UNAME),Darwin)
        DLL=export DYLD_LIBRARY_PATH=$(LIB)
    else 
        DLL=export LD_LIBRARY_PATH=$(LIB)
    endif
endif

lib: 
	cd ../../go && make all

format: $(SRC)
	clang-format -i *.c
	clang-format -i $(BINDINGS)/src/*.c
	clang-format -i $(BINDINGS)/include/*.h

build: format 
	clang -o examples $(SRC) $(BINDINGS)/src/uhppoted.c -I$(BINDINGS)/include -I$(LIB) -L$(LIB) -luhppoted

usage: build
	$(GO) && $(DLL) && $(CMD)

help: build
	$(GO) && $(DLL) && $(CMD) help

invalid: build
	$(GO) && $(DLL) && $(CMD) mystery-meat

get-devices: build
	$(GO) && $(DLL) && $(CMD) get-devices

get-device: build
	$(GO) && $(DLL) && $(CMD) get-device

set-address: build
	$(GO) && $(DLL) && $(CMD) set-address

get-status: build
	$(GO) && $(DLL) && $(CMD) get-status

get-time: build
	$(GO) && $(DLL) && $(CMD) get-time

set-time: build
	$(GO) && $(DLL) && $(CMD) set-time

get-listener: build
	$(GO) && $(DLL) && $(CMD) get-listener

set-listener: build
	$(GO) && $(DLL) && $(CMD) set-listener

get-door-control: build
	$(GO) && $(DLL) && $(CMD) get-door-control

set-door-control: build
	$(GO) && $(DLL) && $(CMD) set-door-control

open-door: build
	$(GO) && $(DLL) && $(CMD) open-door

get-cards: build
	$(GO) && $(DLL) && $(CMD) get-cards

get-card: build
	$(GO) && $(DLL) && $(CMD) get-card

get-card-by-index: build
	$(GO) && $(DLL) && $(CMD) get-card-by-index

put-card: build
	$(GO) && $(DLL) && $(CMD) put-card

delete-card: build
	$(GO) && $(DLL) && $(CMD) delete-card

delete-cards: build
	$(GO) && $(DLL) && $(CMD) delete-cards

get-event-index: build
	$(GO) && $(DLL) && $(CMD) get-event-index

set-event-index: build
	$(GO) && $(DLL) && $(CMD) get-event-index

get-event: build
	$(GO) && $(DLL) && $(CMD) get-event

record-special-events: build
	$(GO) && $(DLL) && $(CMD) record-special-events

get-time-profile: build
	$(GO) && $(DLL) && $(CMD) get-time-profile

set-time-profile: build
	$(GO) && $(DLL) && $(CMD) set-time-profile

clear-time-profiles: build
	$(GO) && $(DLL) && $(CMD) clear-time-profiles

add-task: build
	$(GO) && $(DLL) && $(CMD) add-task

refresh-tasklist: build
	$(GO) && $(DLL) && $(CMD) refresh-tasklist

clear-tasklist: build
	$(GO) && $(DLL) && $(CMD) clear-tasklist

debug: format
	clang -o examples $(SRC) $(BINDINGS)/src/uhppoted.c -I$(BINDINGS)/include -I$(LIB)/debug -L$(LIB)/debug -luhppoted
	$(GO) && $(DLL)/debug && $(CMD) clear-tasklist

