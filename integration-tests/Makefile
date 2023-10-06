.DEFAULT_GOAL := test

.PHONY: python

format:
	go fmt ./...

build:
	cd c      && make build
	cd python && make build

test: format
	GOFLAGS="-count=1" go test ./...
