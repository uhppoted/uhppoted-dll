SRC=devices.go events.go time_profiles.go tasks.go main.go 
LIB=../lib

format:
	go fmt ./...

build: format
	go build -trimpath -buildmode=c-shared -o $(LIB)/libuhppoted.so $(SRC)

debug: format
	go build -trimpath -buildmode=c-shared -tags debug -o $(LIB)/debug/libuhppoted.so $(DEBUG)

tests: format
	go build -trimpath -buildmode=c-shared -tags tests -o $(LIB)/tests/libuhppoted.so $(TESTS)

all: format
	go build -trimpath -buildmode=c-shared             -o $(LIB)/libuhppoted.so       $(SRC)
	go build -trimpath -buildmode=c-shared -tags debug -o $(LIB)/debug/libuhppoted.so $(DEBUG)
	go build -trimpath -buildmode=c-shared -tags tests -o $(LIB)/tests/libuhppoted.so $(TESTS)

