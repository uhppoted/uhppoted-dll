LIB=../../lib
GO=GODEBUG=cgocheck=2
CMD=python3 examples.py 
CONTROLLER ?= 405419896
CARD ?= 10058400
DOOR ?= 3
INTERLOCK  ?=3
ANTIPASSBACK ?= "(1,3):(2,4)"

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

usage: build
	$(GO) && $(DLL) && $(CMD)

help: build
	$(GO) && $(DLL) && $(CMD) help

invalid: build
	$(GO) && $(DLL) && $(CMD) mystery-meat

get-controllers: build
	$(GO) && $(DLL) && $(CMD) get-controllers

get-controller: build
	$(GO) && $(DLL) && $(CMD) get-controller

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
	$(GO) && $(DLL) && $(CMD) set-listener --listener-interval 15

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

delete-all-cards: build
	$(GO) && $(DLL) && $(CMD) delete-all-cards

get-event-index: build
	$(GO) && $(DLL) && $(CMD) get-event-index

set-event-index: build
	$(GO) && $(DLL) && $(CMD) set-event-index

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

set-pc-control: build
	$(GO) && $(DLL) && $(CMD) set-pc-control

set-interlock: build
	$(GO) && $(DLL) && $(CMD) set-interlock --controller $(CONTROLLER) --interlock $(INTERLOCK)

activate-keypads: build
	$(GO) && $(DLL) && $(CMD) activate-keypads

set-door-passcodes: build
	$(GO) && $(DLL) && $(CMD) set-door-passcodes  --controller $(CONTROLLER) --door $(DOOR)

get-antipassback: build
	$(GO) && $(DLL) && $(CMD) get-antipassback --controller $(CONTROLLER)

set-antipassback: build
	$(GO) && $(DLL) && $(CMD) set-antipassback --controller $(CONTROLLER) --antipassback $(ANTIPASSBACK)

restore-default-parameters: build
	$(GO) && $(DLL) && $(CMD) restore-default-parameters  --controller $(CONTROLLER)

listen: build
	$(GO) && $(DLL) && $(CMD) listen

listen-debug: build
	$(GO) && export DYLD_LIBRARY_PATH=$(LIB)/debug && $(CMD) listen

debug: build
	$(GO) && export DYLD_LIBRARY_PATH=$(LIB)/debug && $(CMD) listen
