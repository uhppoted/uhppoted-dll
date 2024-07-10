CODEGEN ?= ../uhppoted-codegen/bin/uhppoted-codegen
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
.PHONY: integration-tests
.PHONY: update
.PHONY: vuln

update:
	go get -u github.com/uhppoted/uhppote-core@main
	go get -u github.com/uhppoted/uhppoted-lib@main

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

build: format
	go build -trimpath -buildmode=c-shared             -o $(LIB)/$(DLL)       ./go/...
	go build -trimpath -buildmode=c-shared -tags debug -o $(LIB)/debug/$(DLL) ./go/...
	go build -trimpath -buildmode=c-shared -tags tests -o $(LIB)/tests/$(DLL) ./go/...

test:
	cd go && make test

integration-tests:
	cd integration-tests && make test

vet: 
	go vet ./...

lint: 
	env GOOS=darwin  GOARCH=amd64 staticcheck ./...
	env GOOS=linux   GOARCH=amd64 staticcheck ./...
	env GOOS=windows GOARCH=amd64 staticcheck ./...

vuln:
	govulncheck ./...

build-all: build test lint
	go fmt ./go/...
	env GOWORK=off go build -trimpath -buildmode=c-shared             -o $(LIB)/$(DLL)       ./go/...
	env GOWORK=off go build -trimpath -buildmode=c-shared -tags debug -o $(LIB)/debug/$(DLL) ./go/...
	env GOWORK=off go build -trimpath -buildmode=c-shared -tags tests -o $(LIB)/tests/$(DLL) ./go/...

	make -C ./tests/c              -f Makefile tests
	make -C ./tests/c++            -f Makefile tests
	make -C ./tests/csharp         -f Makefile tests
	make -C ./tests/python         -f Makefile tests
	make -C ./tests/ccl            -f Makefile tests
	make -C ./examples/c           -f Makefile build
	make -C ./examples/c++         -f Makefile build
	make -C ./examples/csharp/mono -f Makefile build
	make -C ./examples/python      -f Makefile build
	make -C ./examples/ccl         -f Makefile build

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
	make -C ./examples/csharp/mono -f Makefile build
	make -C ./tests/csharp         -f Makefile build

python: 
	make -C ./examples/python -f Makefile build

ccl: 
	make -C ./examples/ccl -f Makefile build

examples:
	make -C ./examples/c           -f Makefile build
	make -C ./examples/c++         -f Makefile build
	make -C ./examples/csharp/mono -f Makefile build
	make -C ./examples/python      -f Makefile build
	make -C ./examples/ccl         -f Makefile build

tests: 
	make -C ./tests/c      -f Makefile tests
	make -C ./tests/c++    -f Makefile tests
	make -C ./tests/csharp -f Makefile tests || true
	make -C ./tests/python -f Makefile tests
	make -C ./tests/ccl    -f Makefile tests

swipe:
	curl -X POST "http://127.0.0.1:8000/uhppote/simulator/405419896/swipe" \
         -H "accept: application/json"                                     \
         -H "Content-Type: application/json"                               \
         -d '{"door":3,"card-number":10058400,"direction":1,"PIN":7531}'

