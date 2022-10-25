CODEGEN ?= ../uhppoted-codegen/bin/uhppoted-codegen

SRC=go/devices.go go/events.go go/time_profiles.go go/tasks.go go/main.go 
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
.PHONY: c
.PHONY: cpp
.PHONY: examples
.PHONY: tests
.PHONY: update

update:
	go get -u github.com/uhppoted/uhppote-core@master
	go get -u github.com/uhppoted/uhppoted-lib@master

update-release:
	go get -u github.com/uhppoted/uhppote-core
	go get -u github.com/uhppoted/uhppoted-lib

regen:
	$(CODEGEN) --models .codegen/.models --templates .codegen/c      --out bindings/c
	$(CODEGEN) --models .codegen/.models --templates .codegen/c++    --out bindings/c++
	$(CODEGEN) --models .codegen/.models --templates .codegen/python --out bindings/python

format:
	go fmt ./go/...

build: format
	go build -trimpath -buildmode=c-shared -o $(LIB)/$(DLL) $(SRC)

build-all: build
	env GOWORK=off go build -trimpath -buildmode=c-shared -o $(LIB)/$(DLL) $(SRC)

	make -C ./tests/c         -f Makefile tests
	make -C ./tests/c++       -f Makefile tests
	make -C ./tests/python    -f Makefile tests
	make -C ./examples/c      -f Makefile build
	make -C ./examples/c++    -f Makefile build
	make -C ./examples/python -f Makefile build

build-debug: build
	# make -C ./examples/csharp -f Makefile build

c: 
	make -C ./examples/c -f Makefile build
	make -C ./tests/c    -f Makefile build

cpp: 
	make -C ./examples/c++ -f Makefile build
	make -C ./tests/c++    -f Makefile build

python: 
	make -C ./examples/python -f Makefile build

examples:
	make -C ./examples/c      -f Makefile build
	make -C ./examples/c++    -f Makefile build
	make -C ./examples/python -f Makefile build

tests: 
	make -C ./tests/c      -f Makefile tests
	make -C ./tests/c++    -f Makefile tests
	make -C ./tests/python -f Makefile tests

