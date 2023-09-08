LIB=../../lib/tests
GO=GODEBUG=cgocheck=2
CMD=$(GO) && $(DLL) && python3 tests.py 

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

format:
	yapf -ri .
	yapf -ri ../../bindings/python

build: format

debug: format
	cd ../.. && make build
	$(CMD) structs

usage: build
	$(CMD) help

invalid: build
	$(CMD) mystery-meat

get-devices: build
	$(CMD) get-devices

get-device: build
	$(CMD) get-device

set-address: build
	$(CMD) set-address

get-status: build
	$(CMD) get-status

get-time: build
	$(CMD) get-time

set-time: build
	$(CMD) set-time

get-listener: build
	$(CMD) get-listener

set-listener: build
	$(CMD) set-listener

get-door-control: build
	$(CMD) get-door-control

set-door-control: build
	$(CMD) set-door-control

open-door: build
	$(CMD) open-door

get-cards: build
	$(CMD) get-cards

get-card: build
	$(CMD) get-card

get-card-by-index: build
	$(CMD) get-card-by-index

put-card: build
	$(CMD) put-card

delete-card: build
	$(CMD) delete-card

delete-cards: build
	$(CMD) delete-cards

get-event-index: build
	$(CMD) get-event-index

set-event-index: build
	$(CMD) set-event-index

get-event: build
	$(CMD) get-event

record-special-events: build
	$(CMD) record-special-events

get-time-profile: build
	$(CMD) get-time-profile

set-time-profile: build
	$(CMD) set-time-profile

clear-time-profiles: build
	$(CMD) clear-time-profiles

add-task: build
	$(CMD) add-task

refresh-tasklist: build
	$(CMD) refresh-tasklist

clear-tasklist: build
	$(CMD) clear-tasklist

set-pc-control: build
	$(CMD) set-pc-control

set-interlock: build
	$(CMD) set-interlock

activate-keypads: build
	$(CMD) activate-keypads

set-super-passwords: build
	$(CMD) set-super-passwords

lookup: build
	$(CMD) lookup

structs: build
	$(CMD) structs

all: build
	$(CMD) all

tests: build
	$(CMD) 
