SRC=examples.c device.c cards.c events.c time_profiles.c tasks.c
BINDINGS=../../bindings/c
LIB=../../lib
GO=GODEBUG=cgocheck=2

DEVICEID=--controller 405419896
CARD=--card 8000001
DOOR=--door 3

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

get-devices: build
	$(CMD) get-devices

get-device: build
	$(CMD) get-device --controller $(DEVICEID)

set-address: build
	$(CMD) set-address --controller $(DEVICEID) --ip-address 192.168.1.125 --subnet-mask 255.255.255.0 --gateway-address 192.168.0.1

get-status: build
	$(CMD) get-status --controller $(DEVICEID)

get-time: build
	$(CMD) get-time --controller $(DEVICEID)

set-time: build
	$(CMD) set-time --controller $(DEVICEID)

get-listener: build
	$(CMD) get-listener --controller $(DEVICEID)

set-listener: build
	$(CMD) set-listener --controller $(DEVICEID) --listener-address 192.168.1.100:60001

get-door-control: build
	$(CMD) get-door-control --controller $(DEVICEID) $(DOOR)

set-door-control: build
	$(CMD) set-door-control --controller $(DEVICEID) $(DOOR)

open-door: build
	$(CMD) open-door --controller $(DEVICEID) $(DOOR)

get-cards: build
	$(CMD) get-cards --controller $(DEVICEID)

get-card: build
	$(CMD) get-card --controller $(DEVICEID) $(CARD)

get-card-by-index: build
	$(CMD) get-card-by-index --controller $(DEVICEID) --card-index 3

put-card: build
	$(CMD) put-card --controller $(DEVICEID)

delete-card: build
	$(CMD) delete-card --controller $(DEVICEID)

delete-cards: build
	$(CMD) delete-cards --controller $(DEVICEID)

get-event-index: build
	$(CMD) get-event-index --controller $(DEVICEID)

set-event-index: build
	$(CMD) get-event-index --controller $(DEVICEID)

get-event: build
	$(CMD) get-event --controller $(DEVICEID) --event-index 33

record-special-events: build
	$(CMD) record-special-events --controller $(DEVICEID)

get-time-profile: build
	$(CMD) get-time-profile --controller $(DEVICEID) --time-profile 29

set-time-profile: build
	$(CMD) set-time-profile --controller $(DEVICEID)

clear-time-profiles: build
	$(CMD) clear-time-profiles --controller $(DEVICEID)

add-task: build
	$(CMD) add-task --controller $(DEVICEID)

refresh-tasklist: build
	$(CMD) refresh-tasklist --controller $(DEVICEID)

clear-tasklist: build
	$(CMD) clear-tasklist --controller $(DEVICEID)

set-pc-control: build
	$(CMD) set-pc-control --controller $(DEVICEID)

set-interlock: build
	$(CMD) set-interlock --controller $(DEVICEID)

activate-keypads: build
	$(CMD) activate-keypads --controller $(DEVICEID)

set-door-passcodes: build
	$(CMD) set-door-passcodes $(DEVICEID) $(DOOR)

restore-default-parameters: build
	$(CMD) restore-default-parameters $(DEVICEID)

listen: build
	$(CMD) listen

debug: format
	gcc -Wall -o examples $(SRC) $(BINDINGS)/src/uhppoted.c -I$(BINDINGS)/include -I$(LIB)/debug -L$(LIB)/debug -luhppoted
	$(CMD) get-status --controller 303986753

