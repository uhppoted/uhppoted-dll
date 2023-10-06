.DEFAULT_GOAL := test

.PHONY: python

format:
	go fmt ./...
	yapf -ri ./python

test: format
	GOFLAGS="-count=1" go test ./...
