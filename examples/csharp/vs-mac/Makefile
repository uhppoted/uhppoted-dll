DLL = ../../../lib
CLI = export DYLD_LIBRARY_PATH=$(DLL) && ./bin/Debug/net7.0/uhppoted-dll-cli
DEBUG = export DYLD_LIBRARY_PATH=$(DLL)/debug && ./bin/Debug/net7.0/uhppoted-dll-cli
TEST = export DYLD_LIBRARY_PATH=$(DLL)/tests && ./bin/Debug/net7.0/uhppoted-dll-cli
CONTROLLER = 405419896

ltsc:
	$(CLI)   get-status --controller 423155481
	$(DEBUG) get-status --controller 423155481
	$(TEST)  get-status --controller 423155481

get-controllers:
	$(CLI) get-controllers

get-controller:
	$(CLI) get-controller --controller $(CONTROLLER)

set-address:
	$(CLI) set-address --controller $(CONTROLLER) --address 192.168.1.100 --netmask 255.255.255.0 --gateway 192.168.1.255

get-status:
	$(CLI) get-status --controller $(CONTROLLER)

get-time:
	$(CLI) get-time --controller $(CONTROLLER)

set-time:
	$(CLI) set-time --controller $(CONTROLLER)

get-listener:
	$(CLI) get-listener --controller $(CONTROLLER)

set-listener:
	$(CLI) set-listener --controller $(CONTROLLER) --listener 192.168.1.100:60001

get-door-control:
	$(CLI) get-door-control --controller $(CONTROLLER) --door 4

set-door-control:
	$(CLI) set-door-control --controller $(CONTROLLER) --door 4 --mode "normally-open" --delay 7

open-door:
	$(CLI) open-door --controller $(CONTROLLER) --door 4

get-cards:
	$(CLI) get-cards --controller $(CONTROLLER)

get-card:
	$(CLI) get-card --controller $(CONTROLLER) --card 10058400

get-card-by-index:
	$(CLI) get-card-by-index --controller $(CONTROLLER) --index 3

put-card:
	$(CLI) put-card --controller $(CONTROLLER) --card 10058400 --start 2023-01-01 --end 2023-12-31 --doors 1,2,4 --PIN 7531

delete-card:
	$(CLI) delete-card --controller $(CONTROLLER) --card 10058400

delete-all-cards:
	$(CLI) delete-all-cards --controller $(CONTROLLER)

get-event-index:
	$(CLI) get-event-index --controller $(CONTROLLER)

set-event-index:
	$(CLI) set-event-index --controller $(CONTROLLER) --index 37
 
get-event:
	$(CLI) get-event --controller $(CONTROLLER) --index 37

record-special-events:
	$(CLI) record-special-events --controller $(CONTROLLER) 

get-time-profile:
	$(CLI) get-time-profile --controller $(CONTROLLER) --profile 29

set-time-profile:
	$(CLI) set-time-profile --controller $(CONTROLLER) --profile 33 --linked 0 --start 2023-12-01 --end 2023-12-31 --weekdays Mon,Tue,Fri --segments 08:30-12:00,13:15-16:45

clear-time-profiles:
	$(CLI) clear-time-profiles --controller $(CONTROLLER)

add-task:
	$(CLI) add-task --controller $(CONTROLLER) --door 4 --start 2023-12-01 --end 2023-12-31 --weekdays Tue,Wed,Thu --at 09:30 --more-card 0 --task "unlock door"

refresh-tasklist:
	$(CLI) refresh-tasklist --controller $(CONTROLLER)

clear-tasklist:
	$(CLI) clear-tasklist --controller $(CONTROLLER)

set-pc-control:
	$(CLI) set-pc-control --controller $(CONTROLLER)

set-interlock:
	$(CLI) set-interlock --controller $(CONTROLLER) --interlock "1&2,3&4"

activate-keypads:
	$(CLI) activate-keypads --controller $(CONTROLLER) --keypads 1,2,4

set-door-passcodes:
	$(CLI) set-door-passcodes --controller $(CONTROLLER) --door 4 --passcodes 12345,7531

restore-default-parameters:
	$(CLI) restore-default-parameters --controller $(CONTROLLER)
