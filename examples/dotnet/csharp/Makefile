CLI = GODEBUG=cgocheck=2 && dotnet run --no-build
CONTROLLER ?= 405419896
DOOR ?= 3
CARD ?= 10058400
FIRSTCARD ?= 08:30,16:45,normally open,firstcard only,[Mon,Tue,Fri]

format: $(SRC)
	dotnet format uhppoted-dll-cli.sln --include *.cs --folder

build: format
	dotnet build uhppoted-dll-cli.sln

get-controllers: build
	$(CLI) get-controllers

get-controller: build
	$(CLI) get-controller --controller $(CONTROLLER)

set-address: build
	$(CLI) set-address --controller $(CONTROLLER) --address 192.168.1.125 --netmask 255.255.255.0 --gateway 192.168.1.255

get-status: build
	$(CLI) get-status --controller $(CONTROLLER)

get-time: build
	$(CLI) get-time --controller $(CONTROLLER)

set-time: build
	$(CLI) set-time --controller $(CONTROLLER)

get-listener: build
	$(CLI) get-listener --controller $(CONTROLLER)

set-listener: build
	$(CLI) set-listener --controller $(CONTROLLER) --listener 192.168.1.125: build60001 --listener-interval 15

get-door-control: build
	$(CLI) get-door-control --controller $(CONTROLLER) --door 4

set-door-control: build
	$(CLI) set-door-control --controller $(CONTROLLER) --door 4 --mode "normally-open" --delay 7

open-door: build
	$(CLI) open-door --controller $(CONTROLLER) --door 4

get-cards: build
	$(CLI) get-cards --controller $(CONTROLLER)

get-card: build
	$(CLI) get-card --controller $(CONTROLLER) --card 10058400

get-card-by-index: build
	$(CLI) get-card-by-index --controller $(CONTROLLER) --index 3

put-card: build
	$(CLI) put-card --controller $(CONTROLLER) --card 10058400 --start 2023-01-01 --end 2023-12-31 --doors 1,2,4 --PIN 7531

delete-card: build
	$(CLI) delete-card --controller $(CONTROLLER) --card 10058400

delete-all-cards: build
	$(CLI) delete-all-cards --controller $(CONTROLLER)

get-event-index: build
	$(CLI) get-event-index --controller $(CONTROLLER)

set-event-index: build
	$(CLI) set-event-index --controller $(CONTROLLER) --index 37

get-event: build
	$(CLI) get-event --controller $(CONTROLLER) --index 37

record-special-events: build
	$(CLI) record-special-events --controller $(CONTROLLER) 

get-time-profile: build
	$(CLI) get-time-profile --controller $(CONTROLLER) --profile 29

set-time-profile: build
	$(CLI) set-time-profile --controller $(CONTROLLER) --profile 33 --linked 0 --start 2023-12-01 --end 2023-12-31 --weekdays Mon,Tue,Fri --segments 08: build30-12: build00,13: build15-16: build45

clear-time-profiles: build
	$(CLI) clear-time-profiles --controller $(CONTROLLER)

add-task: build
	$(CLI) add-task --controller $(CONTROLLER) --door 4 --start 2023-12-01 --end 2023-12-31 --weekdays Tue,Wed,Thu --at 09: build30 --more-card 0 --task "unlock door"

refresh-tasklist: build
	$(CLI) refresh-tasklist --controller $(CONTROLLER)

clear-tasklist: build
	$(CLI) clear-tasklist --controller $(CONTROLLER)

set-pc-control: build
	$(CLI) set-pc-control --controller $(CONTROLLER)

set-interlock: build
	$(CLI) set-interlock --controller $(CONTROLLER) --interlock "1&2,3&4"

activate-keypads: build
	$(CLI) activate-keypads --controller $(CONTROLLER) --keypads 1,2,4

set-door-passcodes: build
	$(CLI) set-door-passcodes --controller $(CONTROLLER) --door 4 --passcodes 12345,7531

get-antipassback: build
	$(CLI) get-antipassback --controller $(CONTROLLER)

set-antipassback: build
	$(CLI) set-antipassback --controller $(CONTROLLER) --antipassback "(1,3): build(2,4)"

set-firstcard: build
	$(CLI) set-firstcard --controller $(CONTROLLER) --door $(DOOR) --firstcard "$(FIRSTCARD)"

restore-default-parameters: build
	$(CLI) restore-default-parameters --controller $(CONTROLLER)

listen:
	$(CLI) listen

