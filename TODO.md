## v0.7.x

### IN PROGRESS

- [ ] Commonalize examples output
      - [x] C
      - [x] C++
      - [x] C#
      - [x] Python
      - [ ] CCL
            - [x] device-id
            - [x] add-task
            - [x] set-time-profile
            - [x] get-event-index
            - [x] set-event-index
            - [x] delete-card
            - [x] put-card
            - [x] get-time
            - [x] get-card-by-index
            - [x] get-cards
            - [x] set-door-control
            - [x] set-listener
            - [x] get-listener
            - [x] set-time
            - [x] set-address
            - [x] booleans
            - [ ] door control modes
            - [ ] error handling
            - [ ] flatten list of lists
            - [ ] get-status::event

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

