## v0.7.x

### IN PROGRESS


- [ ] C++/Linux
      - [ ] ???
```
g++ -std=c++17 -Wall tests.cpp device.cpp cards.cpp events.cpp time_profiles.cpp tasks.cpp -o tests  ../../bindings/c++/src/uhppoted.cpp -I../../bindings/c++/include -I../../lib/tests -lc++ -L../../lib/tests -luhppoted
GODEBUG=cgocheck=2 &export LD_LIBRARY_PATH=../../lib/tests && ./tests

 *** ERROR ��w~�U is not a valid UDP address:port
```

- [ ] OS specific DLL naming
      - [x] Go
      - [ ] C
      - [ ] C++
      - [ ] C#
      - [x] Python
      - [ ] CCL

- [ ] Change all bind/broadcast/listen addresses to generic system IPs
      - [x] C
      - [ ] C++
      - [ ] C#
      - [x] Python
      - [ ] CCL

- [ ] Replace slice'ing code with standard cgo functions
      - https://pkg.go.dev/cmd/cgo
      - func C.CBytes([]byte) unsafe.Pointer
      - func C.GoStringN(*C.char, C.int) string
      - func C.GoBytes(unsafe.Pointer, C.int) []byte
      - (?) Pass string arguments rather than const char *

- [ ] Commonalize examples output
      - [x] C
      - [ ] C++
      - [ ] C#
      - [x] Python
      - [ ] CCL

- [ ] Extend examples to use command line args
      - [ ] C
      - [ ] C++
      - [ ] C#
      - [x] Python
      - [ ] CCL
  
- [ ] Usage and notes
      - [ ] C
      - [ ] C++
      - [ ] C#
      - [ ] Python
      - [ ] CCL
      - [ ] LIB_PATH/DYLIB_PATH

- [ ] Github workflow
      - [ ] tests
            - [x] C
            - [ ] C++
            - [ ] C#
            - [ ] Python
            - [ ] CCL
      - [ ] examples

  - [ ] Constants for event types, reason, etc
  - [ ] C++ 
        - make doors/inputs in status bool
  - [ ] CCL
        - DOLIST
        - simplify main loop with `thereis` (https://gigamonkeys.com/book/loop-for-black-belts.html)
  - [ ] GetCardByIndex should return 'card deleted' error for ff ff ff ff in response

## TODO

- (?) lint
- (?) Cross-compile
      - https://github.com/elastic/golang-crossbuild
      - https://dh1tw.de/2019/12/cross-compiling-golang-cgo-projects/

