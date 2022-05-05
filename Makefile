SRC=go/devices.go go/cards.go go/events.go go/time_profiles.go go/tasks.go go/main.go 
DEBUG=go/devices_debug.go go/cards_debug.go go/events_debug.go go/time_profiles_debug.go go/tasks_debug.go go/main.go 
TESTS=go/devices_tests.go go/cards_tests.go go/events_tests.go go/time_profiles_tests.go go/tasks_tests.go go/main.go 
LIB=./lib

ifeq ($(OS),Windows_NT)
    DLL = uhppoted.dll
else
    UNAME = $(shell uname -s)
    ifeq ($(UNAME),Darwin)
        DLL = libuhppoted.dylib
    else 
        DLL = libuhppoted.so
    endif
endif

.PHONY: python
.PHONY: cpp
.PHONY: csharp
.PHONY: ccl
.PHONY: examples
.PHONY: tests

build: 
	go fmt ./go/...
	go build -trimpath -buildmode=c-shared             -o $(LIB)/$(DLL) $(SRC)
	go build -trimpath -buildmode=c-shared -tags debug -o $(LIB)/debug/$(DLL) $(DEBUG)
	go build -trimpath -buildmode=c-shared -tags tests -o $(LIB)/tests/$(DLL) $(TESTS)

c: ./examples/c/example ./tests/c/test
	make -C ./examples/c -f Makefile build
	make -C ./tests/c    -f Makefile build

cpp: 
	make -C ./examples/c++ -f Makefile build
	make -C ./tests/c++    -f Makefile build

csharp: 
	make -C ./examples/csharp -f Makefile build
	make -C ./tests/csharp    -f Makefile build

python: 
	make -C ./examples/python -f Makefile build

ccl: 
	make -C ./examples/ccl -f Makefile build

examples:
	make -C ./examples/c      -f Makefile build
	make -C ./examples/c++    -f Makefile build
	make -C ./examples/csharp -f Makefile build
	make -C ./examples/python -f Makefile build
	make -C ./examples/ccl    -f Makefile build

tests: 
	make -C ./tests/c      -f Makefile tests
	make -C ./tests/c++    -f Makefile tests
	make -C ./tests/csharp -f Makefile tests
	make -C ./tests/python -f Makefile tests
	make -C ./tests/ccl    -f Makefile tests

build-all: build
	make -C ./tests/c   -f Makefile tests
	make -C ./tests/c++ -f Makefile tests

build-debug: build
	make -C ./tests/python -f Makefile tests


