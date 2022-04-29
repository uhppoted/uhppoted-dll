LIB=../../lib/tests
BINDINGS=../../bindings/c++
CMD=GODEBUG=cgocheck=2 && export DYLD_LIBRARY_PATH=$(LIB) && export LD_LIBRARY_PATH=$(LIB) && ./tests
SRC=tests.cpp device.cpp cards.cpp events.cpp time_profiles.cpp tasks.cpp

lib: 
	cd ../../go && make all

format: $(SRC)
	clang-format -i *.cpp
	clang-format -i *.hpp
	clang-format -i $(BINDINGS)/src/*.cpp
	clang-format -i $(BINDINGS)/include/*.hpp

build: format
	clang -std=c++11 $(SRC) -D_GLIBCXX_USE_CXX11_ABI=0 -o tests  $(BINDINGS)/src/uhppoted.cpp -I$(BINDINGS)/include -I$(LIB) -lc++ -L$(LIB) -luhppoted

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

all: build
	$(CMD) all

test: build
	$(CMD)
