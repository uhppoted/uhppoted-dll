## v0.7.x

### IN PROGRESS

- [x] C/Ubuntu

- [ ] Rename status.event throughout (because it's special in C# and g++ doesn't like it much either)

- [x] OS specific DLL naming
      - [x] Go
      - [x] C
      - [x] C++
      - [x] C#
      - [x] Python
      - [x] CCL

- [ ] Commonalize examples output
      - [x] C
      - [x] C++
      - [ ] C#
      - [x] Python
      - [ ] CCL

- [x] Change all bind/broadcast/listen addresses to generic system IPs
      - [x] C
      - [x] C++
      - [x] C#
      - [x] Python
      - [x] CCL

- [ ] Replace slice'ing code with standard cgo functions
      - https://pkg.go.dev/cmd/cgo
      - func C.CBytes([]byte) unsafe.Pointer
      - func C.GoStringN(*C.char, C.int) string
      - func C.GoBytes(unsafe.Pointer, C.int) []byte
      - (?) Pass string arguments rather than const char *

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
            - [x] C++
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

