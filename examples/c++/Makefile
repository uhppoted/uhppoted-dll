SRC=examples.cpp device.cpp cards.cpp events.cpp time_profiles.cpp tasks.cpp
BINDINGS=../../bindings/c++
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
	clang-format -i *.cpp
	clang-format -i $(BINDINGS)/src/*.cpp
	clang-format -i $(BINDINGS)/include/*.hpp

build: format
	g++ -std=c++17 -Wall -o examples $(SRC) $(BINDINGS)/src/uhppoted.cpp -I$(BINDINGS)/include -I$(LIB) -L$(LIB) -luhppoted

usage: build
	$(CMD)

help: build
	$(CMD) help

invalid: build
	$(CMD) mystery-meat

get-devices: build
	$(CMD) get-devices

get-device: build
	$(CMD) get-device $(DEVICEID)

set-address: build
	$(CMD) set-address $(DEVICEID) --ip-address 192.168.1.125 --subnet-mask 255.255.255.0 --gateway-address 192.168.0.1

get-status: build
	$(CMD) get-status $(DEVICEID)

get-time: build
	$(CMD) get-time $(DEVICEID)

set-time: build
	$(CMD) set-time $(DEVICEID)

get-listener: build
	$(CMD) get-listener $(DEVICEID)

set-listener: build
	$(CMD) set-listener $(DEVICEID) --listener-address 192.168.1.100:60001

get-door-control: build
	$(CMD) get-door-control $(DEVICEID) $(DOOR)

set-door-control: build
	$(CMD) set-door-control $(DEVICEID) $(DOOR)

open-door: build
	$(CMD) open-door $(DEVICEID) $(DOOR)

get-cards: build
	$(CMD) get-cards $(DEVICEID)

get-card: build
	$(CMD) get-card $(DEVICEID) $(CARD)

get-card-by-index: build
	$(CMD) get-card-by-index $(DEVICEID) --card-index 3

put-card: build
	$(CMD) put-card $(DEVICEID)

delete-card: build
	$(CMD) delete-card $(DEVICEID)

delete-cards: build
	$(CMD) delete-cards $(DEVICEID)

get-event-index: build
	$(CMD) get-event-index $(DEVICEID)

set-event-index: build
	$(CMD) get-event-index $(DEVICEID)

get-event: build
	$(CMD) get-event $(DEVICEID) --event-index 33

record-special-events: build
	$(CMD) record-special-events $(DEVICEID)

get-time-profile: build
	$(CMD) get-time-profile $(DEVICEID) --time-profile 29

set-time-profile: build
	$(CMD) set-time-profile $(DEVICEID)

clear-time-profiles: build
	$(CMD) clear-time-profiles $(DEVICEID)

add-task: build
	$(CMD) add-task $(DEVICEID)

refresh-tasklist: build
	$(CMD) refresh-tasklist $(DEVICEID)

clear-tasklist: build
	$(CMD) clear-tasklist $(DEVICEID)

set-pc-control: build
	$(CMD) set-pc-control $(DEVICEID)

debug: format
	g++ -std=c++17 -Wall -o examples $(SRC) $(BINDINGS)/src/uhppoted.cpp -I$(BINDINGS)/include -I$(LIB)/debug -lc++ -L$(LIB)/debug -luhppoted
	$(GO) && $(DLL)/debug && ./examples get-status --controller 12345678
