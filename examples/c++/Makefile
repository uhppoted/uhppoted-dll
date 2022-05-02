SRC=examples.cpp device.cpp cards.cpp events.cpp time_profiles.cpp tasks.cpp
BINDINGS=../../bindings/c++
LIB=../../lib
CMD=./examples

ifeq ($(OS),Windows_NT)
	DLL=export LD_LIBRARY_PATH=$(LIB)
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
	clang-format -i *.cpp
	clang-format -i $(BINDINGS)/src/*.cpp
	clang-format -i $(BINDINGS)/include/*.hpp

build: format
	clang -std=c++17 -o examples $(SRC) $(BINDINGS)/src/uhppoted.cpp -I$(BINDINGS)/include -I$(LIB) -lc++ -L$(LIB) -luhppoted

usage: build
	GODEBUG=cgocheck=2 && $(DLL) && $(CMD)

help: build
	GODEBUG=cgocheck=2 && $(DLL) && $(CMD) help

invalid: build
	GODEBUG=cgocheck=2 && $(DLL) && $(CMD) mystery-meat

get-devices: build
	GODEBUG=cgocheck=2 && $(DLL) && $(CMD) get-devices

get-device: build
	GODEBUG=cgocheck=2 && $(DLL) && $(CMD) get-device

set-address: build
	GODEBUG=cgocheck=2 && $(DLL) && $(CMD) set-address

get-status: build
	GODEBUG=cgocheck=2 && $(DLL) && $(CMD) get-status

get-time: build
	GODEBUG=cgocheck=2 && $(DLL) && $(CMD) get-time

set-time: build
	GODEBUG=cgocheck=2 && $(DLL) && $(CMD) set-time

get-listener: build
	GODEBUG=cgocheck=2 && $(DLL) && $(CMD) get-listener

set-listener: build
	GODEBUG=cgocheck=2 && $(DLL) && $(CMD) set-listener

get-door-control: build
	GODEBUG=cgocheck=2 && $(DLL) && $(CMD) get-door-control

set-door-control: build
	GODEBUG=cgocheck=2 && $(DLL) && $(CMD) set-door-control

open-door: build
	GODEBUG=cgocheck=2 && $(DLL) && $(CMD) open-door

get-cards: build
	GODEBUG=cgocheck=2 && $(DLL) && $(CMD) get-cards

get-card: build
	GODEBUG=cgocheck=2 && $(DLL) && $(CMD) get-card

get-card-by-index: build
	GODEBUG=cgocheck=2 && $(DLL) && $(CMD) get-card-by-index

put-card: build
	GODEBUG=cgocheck=2 && $(DLL) && $(CMD) put-card

delete-card: build
	GODEBUG=cgocheck=2 && $(DLL) && $(CMD) delete-card

delete-cards: build
	GODEBUG=cgocheck=2 && $(DLL) && $(CMD) delete-cards

get-event-index: build
	GODEBUG=cgocheck=2 && $(DLL) && $(CMD) get-event-index

set-event-index: build
	GODEBUG=cgocheck=2 && $(DLL) && $(CMD) set-event-index

get-event: build
	GODEBUG=cgocheck=2 && $(DLL) && $(CMD) get-event

record-special-events: build
	GODEBUG=cgocheck=2 && $(DLL) && $(CMD) record-special-events

get-time-profile: build
	GODEBUG=cgocheck=2 && $(DLL) && $(CMD) get-time-profile

set-time-profile: build
	GODEBUG=cgocheck=2 && $(DLL) && $(CMD) set-time-profile

clear-time-profiles: build
	GODEBUG=cgocheck=2 && $(DLL) && $(CMD) clear-time-profiles

add-task: build
	GODEBUG=cgocheck=2 && $(DLL) && $(CMD) add-task

refresh-tasklist: build
	GODEBUG=cgocheck=2 && $(DLL) && $(CMD) refresh-tasklist

clear-tasklist: build
	GODEBUG=cgocheck=2 && $(DLL) && $(CMD) clear-tasklist

debug: format
	clang -std=c++11  -o examples $(SRC) $(BINDINGS)/src/uhppoted.cpp -I$(BINDINGS)/include -I$(LIB)/debug -lc++ -L$(LIB)/debug -luhppoted
	GODEBUG=cgocheck=2 && $(DLL)/debug && $(CMD) clear-tasklist
