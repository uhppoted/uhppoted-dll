SRC=devices.go cards.go events.go time_profiles.go tasks.go main.go 
DEBUG=devices_debug.go cards_debug.go events_debug.go time_profiles_debug.go tasks_debug.go main.go 
TESTS=devices_tests.go cards_tests.go events_tests.go time_profiles_tests.go tasks_tests.go main.go 
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

