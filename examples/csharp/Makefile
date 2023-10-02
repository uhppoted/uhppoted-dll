LIB=../../lib
BINDINGS=../../bindings/csharp
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

CMD=$(GO) && $(DLL) && mono ./examples.exe

format:
	clang-format -i *.cs
	clang-format -i $(BINDINGS)/*.cs

format-dotnet:
	dotnet format --include *.cs --folder
	dotnet format --include $(BINDINGS)/*.cs --folder

build: format
	csc $(BINDINGS)/uhppoted.cs $(BINDINGS)/lookup.cs examples.cs

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
	$(CMD) set-address $(DEVICEID) --ip-address "192.168.1.100" --subnet-mask "255.255.255.0" --gateway "192.168.1.1"

get-status: build
	$(CMD) get-status $(DEVICEID)

get-time: build
	$(CMD) get-time $(DEVICEID)

set-time: build
	$(CMD) set-time $(DEVICEID)

get-listener: build
	$(CMD) get-listener $(DEVICEID)

set-listener: build
	$(CMD) set-listener $(DEVICEID) "192.168.1.100:60001"

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
	$(CMD) put-card $(DEVICEID) $(CARD)

delete-card: build
	$(CMD) delete-card $(DEVICEID) $(CARD)

delete-cards: build
	$(CMD) delete-cards $(DEVICEID) 

get-event-index: build
	$(CMD) get-event-index $(DEVICEID) 

set-event-index: build
	$(CMD) set-event-index $(DEVICEID) --event-index 29

get-event: build
	$(CMD) get-event $(DEVICEID)

record-special-events: build
	$(CMD) record-special-events $(DEVICEID)

get-time-profile: build
	$(CMD) get-time-profile $(DEVICEID) --time-profile 29

set-time-profile: build
	$(CMD) set-time-profile $(DEVICEID) --time-profile 29

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

set-interlock: build
	$(CMD) set-interlock $(DEVICEID)

activate-keypads: build
	$(CMD) activate-keypads $(DEVICEID)

set-door-passcodes: build
	$(CMD) set-door-passcodes $(DEVICEID) $(DOOR)

debug: build
	$(GO) && export $(DLL)/debug && mono ./examples.exe get-status --controller 12345678
	$(CMD) get-status --controller 303986753
