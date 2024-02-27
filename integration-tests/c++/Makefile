LIB=../../lib
BINDINGS=../../bindings/c++
GO=GODEBUG=cgocheck=2
SRC=tests.cpp device.cpp cards.cpp events.cpp time_profiles.cpp tasks.cpp

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

CMD=$(GO) && $(DLL) && ./tests

.PHONY: build-gcc
	
format: $(SRC)
	clang-format -i *.cpp
	clang-format -i *.hpp

build: format
	g++ -std=c++17 -Wall  -o tests $(SRC) $(BINDINGS)/src/uhppoted.cpp -I$(BINDINGS)/include -I$(LIB) -L$(LIB) -luhppoted

get-devices: 
	$(CMD) get-devices

get-device: 
	$(CMD) get-device

set-address: 
	$(CMD) set-address

get-status: 
	$(CMD) get-status
	$(CMD) get-status-no-event

get-time: 
	$(CMD) get-time

set-time: 
	$(CMD) set-time

get-listener: 
	$(CMD) get-listener

set-listener: 
	$(CMD) set-listener

get-door-control: 
	$(CMD) get-door-control

set-door-control: 
	$(CMD) set-door-control

open-door: 
	$(CMD) open-door

get-cards: 
	$(CMD) get-cards

get-card: 
	$(CMD) get-card

get-card-by-index: 
	$(CMD) get-card-by-index

put-card: 
	$(CMD) put-card

delete-card: 
	$(CMD) delete-card

delete-cards: 
	$(CMD) delete-cards

get-event-index: 
	$(CMD) get-event-index

set-event-index: 
	$(CMD) set-event-index

get-event: 
	$(CMD) get-event

record-special-events: 
	$(CMD) record-special-events

get-time-profile: 
	$(CMD) get-time-profile

set-time-profile: 
	$(CMD) set-time-profile

clear-time-profiles: 
	$(CMD) clear-time-profiles

add-task: 
	$(CMD) add-task

refresh-tasklist: 
	$(CMD) refresh-tasklist

clear-tasklist: 
	$(CMD) clear-tasklist

set-pc-control: 
	$(CMD) set-pc-control

set-interlock: 
	$(CMD) set-interlock

activate-keypads: 
	$(CMD) activate-keypads

set-door-passcodes: 
	$(CMD) set-door-passcodes

restore-default-parameters: 
	$(CMD) restore-default-parameters

all: 
	$(CMD) all
