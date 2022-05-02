## v0.7.x

### IN PROGRESS

- [ ] And now revert back to C++17 again

- [ ] Replace slice'ing code with standard cgo functions
      - https://pkg.go.dev/cmd/cgo
      - func C.CBytes([]byte) unsafe.Pointer
      - func C.GoStringN(*C.char, C.int) string
      - func C.GoBytes(unsafe.Pointer, C.int) []byte

- [ ] Pass string arguments rather than const char *

- [ ] Commonalize examples output
      - [ ] C
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
      - [ ] examples
      - [ ] tests
            - [x] C
            - [ ] C++
```
../../bindings/c++/include/../include/uhppoted.hpp:49:11: error: declaration of ‘event status::event’ changes meaning of ‘event’ [-fpermissive]
   49 |     event event;
      |           ^~~~~
```
            - [ ] C#
            - [ ] Python
            - [ ] CCL

  - [ ] Change all bind/broadcast/listen addresses to generic system IPs
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

