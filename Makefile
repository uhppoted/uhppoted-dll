CODEGEN ?= ../uhppoted-codegen/bin/uhppoted-codegen

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

.DEFAULT_GOAL := build-all
.PHONY: python
.PHONY: c
.PHONY: cpp
.PHONY: csharp
.PHONY: ccl
.PHONY: examples
.PHONY: tests
.PHONY: update
.PHONY: vuln

update:
	go get -u github.com/uhppoted/uhppote-core@master
	go get -u github.com/uhppoted/uhppoted-lib@master

update-release:
	go get -u github.com/uhppoted/uhppote-core
	go get -u github.com/uhppoted/uhppoted-lib

regen:
	# find .codegen/.models -name "*.json" -exec sh -c 'jq . {} | sponge {}' \;
	$(CODEGEN) --models .codegen/.models --templates .codegen/c      --out bindings/c
	$(CODEGEN) --models .codegen/.models --templates .codegen/c++    --out bindings/c++
	$(CODEGEN) --models .codegen/.models --templates .codegen/python --out bindings/python
	$(CODEGEN) --models .codegen/.models --templates .codegen/csharp --out bindings/csharp
	$(CODEGEN) --models .codegen/.models --templates .codegen/ccl    --out bindings/ccl

format:
	go fmt ./go/...

build: 
	go fmt ./go/...
	go build -trimpath -buildmode=c-shared             -o $(LIB)/$(DLL) $(SRC)
	go build -trimpath -buildmode=c-shared -tags debug -o $(LIB)/debug/$(DLL) $(DEBUG)
	go build -trimpath -buildmode=c-shared -tags tests -o $(LIB)/tests/$(DLL) $(TESTS)

vet: 
	go vet ./...

lint: 
	env GOOS=darwin  GOARCH=amd64 staticcheck ./...
	env GOOS=linux   GOARCH=amd64 staticcheck ./...
	env GOOS=windows GOARCH=amd64 staticcheck ./...

vuln:
	govulncheck ./...

build-all: build lint
	go fmt ./go/...
	env GOWORK=off go build -trimpath -buildmode=c-shared             -o $(LIB)/$(DLL) $(SRC)
	env GOWORK=off go build -trimpath -buildmode=c-shared -tags debug -o $(LIB)/debug/$(DLL) $(DEBUG)
	env GOWORK=off go build -trimpath -buildmode=c-shared -tags tests -o $(LIB)/tests/$(DLL) $(TESTS)

	make -C ./tests/c         -f Makefile tests
	make -C ./tests/c++       -f Makefile tests
	make -C ./tests/csharp    -f Makefile tests
	make -C ./tests/python    -f Makefile tests
	make -C ./tests/ccl       -f Makefile tests
	make -C ./examples/c      -f Makefile build
	make -C ./examples/c++    -f Makefile build
	make -C ./examples/csharp -f Makefile build
	make -C ./examples/python -f Makefile build
	make -C ./examples/ccl    -f Makefile build

build-debug: build
	make -C ./tests/c++       -f Makefile tests

godoc:
	godoc -http=:80	-index_interval=60s

release: update-release build-all
	# No binary release

publish: release
	echo "Releasing version $(VERSION)"
	gh release create "$(VERSION)" --draft --prerelease --title "$(VERSION)-beta" --notes-file release-notes.md

c: 
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

