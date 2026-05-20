LIB=../../lib
BINDINGS=../../bindings/ccl
CMD=./examples

CONTROLLER=--controller 405419896
CARD=--card 10058400
DOOR=--door 3

ifeq ($(OS),Windows_NT)
	DLL=(setenv "PATH" "$(LIB)")
else
    UNAME = $(shell uname -s)
    ifeq ($(UNAME),Darwin)
        DLL=(setenv "DYLD_LIBRARY_PATH" "$(LIB)")
    else 
        DLL=(setenv "LD_LIBRARY_PATH" "$(realpath $(LIB))")
    endif
endif

.PHONY: run
.PHONY: debug

build: 
	GODEBUG=cgocheck=2 && rlwrap ccl64 --eval '$(DLL)' --load main.lisp --eval '(make-app)' 

usage: build
	$(CMD) 

help: build
	$(CMD) help

invalid: build
	$(CMD) mystery-meat

get-controllers: build
	$(CMD) get-controllers

get-controller: build
	$(CMD) get-controller $(CONTROLLER)

set-address: build
	$(CMD) set-address $(CONTROLLER) --ip-address "192.168.1.125" --subnet-mask "255.255.255.0" --gateway-address "192.168.1.1"

get-status: build
	$(CMD) get-status $(CONTROLLER)

get-time: build
	$(CMD) get-time $(CONTROLLER)

set-time: build
	$(CMD) set-time $(CONTROLLER)

get-listener: build
	$(CMD) get-listener $(CONTROLLER)

set-listener: build
	$(CMD) set-listener $(CONTROLLER) --listener-address "192.168.1.125:60002" --listener-interval 15

get-door-control: build
	$(CMD) get-door-control $(CONTROLLER) $(DOOR)

set-door-control: build
	$(CMD) set-door-control $(CONTROLLER) $(DOOR)

open-door: build
	$(CMD) open-door $(CONTROLLER) $(DOOR)

get-cards: build
	$(CMD) get-cards $(CONTROLLER)

get-card: build
	$(CMD) get-card $(CONTROLLER) $(CARD)

get-card-by-index: build
	$(CMD) get-card-by-index $(CONTROLLER) --card-index 3

put-card: build
	$(CMD) put-card $(CONTROLLER) $(CARD)

delete-card: build
	$(CMD) delete-card $(CONTROLLER) $(CARD)

delete-cards: build
	$(CMD) delete-cards $(CONTROLLER)

get-event-index: build
	$(CMD) get-event-index $(CONTROLLER) --event-index 91

set-event-index: build
	$(CMD) set-event-index $(CONTROLLER) --event-index 27

get-event: build
	$(CMD) get-event $(CONTROLLER)

record-special-events: build
	$(CMD) record-special-events $(CONTROLLER)

get-time-profile: build
	$(CMD) get-time-profile $(CONTROLLER) --time-profile 29

set-time-profile: build
	$(CMD) set-time-profile $(CONTROLLER) --time-profile 29

clear-time-profiles: build
	$(CMD) clear-time-profiles $(CONTROLLER)

add-task: build
	$(CMD) add-task $(CONTROLLER)

refresh-tasklist: build
	$(CMD) refresh-tasklist $(CONTROLLER)

clear-tasklist: build
	$(CMD) clear-tasklist $(CONTROLLER)

set-pc-control: build
	$(CMD) set-pc-control $(CONTROLLER)

set-interlock: build
	$(CMD) set-interlock $(CONTROLLER)

activate-keypads: build
	$(CMD) activate-keypads $(CONTROLLER)

set-door-passcodes: build
	$(CMD) set-door-passcodes $(CONTROLLER) $(DOOR)

get-antipassback: build
	$(CMD) get-antipassback $(CONTROLLER)

set-antipassback: build
	$(CMD) set-antipassback $(CONTROLLER)

set-firstcard: build
	$(CMD) set-firstcard $(CONTROLLER) $(DOOR)

restore-default-parameters: build
	$(CMD) restore-default-parameters $(CONTROLLER)

listen: build
	$(CMD) listen 

listen-debug:
	GODEBUG=cgocheck=2 && rlwrap ccl64 --eval '(setenv "DYLD_LIBRARY_PATH" "$(LIB)/debug")' --load main.lisp --eval '(make-app)' 
	$(CMD) listen

run: 
	GODEBUG=cgocheck=2 && rlwrap ccl64 --eval '$(DLL)' --load main.lisp

debug: build
	# GODEBUG=cgocheck=2 && rlwrap ccl64 --eval '(setenv "DYLD_LIBRARY_PATH" "$(LIB)/debug")' --load main.lisp
	GODEBUG=cgocheck=2 && rlwrap ccl64 --eval '(setenv "DYLD_LIBRARY_PATH" "$(LIB)/debug")' --load main.lisp --eval '(make-app)' 
	$(CMD) listen

