## v0.7.x

### IN PROGRESS

- [ ] C/Ubuntu
```
tasks.c:54:12: note: referencing argument 3 of type ‘const result *’
In file included from tasks.c:5:
tests.h:77:13: note: in a call to function ‘evaluate’
   77 | extern bool evaluate(const char *, int, const result[]);
      |             ^~~~~~~~
```

- [x] C++/Ubuntu
```
 *** ERROR ��w~�U is not a valid UDP address:port
```

- [ ] Rename c# to csharp (confusing with # prompt on Ubuntu)

- [ ] OS specific DLL naming
      - [x] Go
      - [ ] C
      - [ ] C++
      - [ ] C#
      - [x] Python
      - [ ] CCL

- [ ] Change all bind/broadcast/listen addresses to generic system IPs
      - [x] C
      - [x] C++
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

