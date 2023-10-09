.DEFAULT_GOAL := test

.PHONY: python

format:
	go fmt ./...

build: format
	cd c      && make build
	cd c++    && make build
	cd csharp && make build
	cd python && make build
	cd ccl    && make build

test: build
	GOFLAGS="-count=1" go test ./...
