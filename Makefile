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
.PHONY: ccl
.PHONY: examples
.PHONY: tests

build: 
	go fmt ./go/...
	go build -trimpath -buildmode=c-shared             -o $(LIB)/$(DLL)       ./...
	go build -trimpath -buildmode=c-shared -tags debug -o $(LIB)/debug/$(DLL) ./...
	go build -trimpath -buildmode=c-shared -tags tests -o $(LIB)/tests/$(DLL) ./...

c: ./examples/c/example ./tests/c/test
	make -C ./examples/c -f Makefile build
	make -C ./tests/c    -f Makefile build

cpp: 
	make -C ./examples/c++ -f Makefile build
	make -C ./tests/c++    -f Makefile build

csharp: ./examples/c#/example.exe ./tests/c#/test.exe
	make -C ./examples/c# -f Makefile build
	make -C ./tests/c#    -f Makefile build

python: 
	make -C ./examples/python -f Makefile build

ccl: 
	make -C ./examples/ccl -f Makefile build

examples:
	make -C ./examples/c      -f Makefile build
	make -C ./examples/c++    -f Makefile build
	make -C ./examples/c#     -f Makefile build
	make -C ./examples/python -f Makefile build
	make -C ./examples/ccl    -f Makefile build

tests: 
	make -C ./tests/c      -f Makefile tests
	make -C ./tests/c++    -f Makefile tests
	make -C ./tests/c#     -f Makefile tests
	make -C ./tests/python -f Makefile tests
	make -C ./tests/ccl    -f Makefile tests

build-all: build
	make -C ./tests/c   -f Makefile tests
	make -C ./tests/c++ -f Makefile tests

build-debug: build
	make -C ./tests/c++ -f Makefile test


