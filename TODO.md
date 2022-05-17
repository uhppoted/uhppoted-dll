## v0.7.x

### IN PROGRESS

- [ ] Github workflow
      - [ ] tests
            - [x] C
            - [x] C++
            - [x] C#
            - [x] Python
            - [ ] CCL
```
rlwrap ccl64 --eval '(setenv "LD_LIBRARY_PATH" "../../lib/tests")' --load main.lisp --eval '(make-app)' 
>>> TARGET-LINUX
>>> >> LD_LIBRARY_PATH ../../lib/tests
>>> >> path ../../lib/tests/libuhppoted.so
>>> >> native-path ../../lib/tests/libuhppoted.so
GODEBUG=cgocheck=2 && ./tests
```
      - [ ] examples
            - [ ] C
            - [ ] C++
            - [ ] C#
            - [ ] Python
            - [ ] CCL

- [ ] Constants for event types, reason, etc
      - [ ] lookup(...)
      - [ ] Update examples
      - [ ] Update tests

- [ ] Extend examples to use command line args
      - [x] C
      - [x] C++
      - [x] C#
      - [x] Python
      - [ ] CCL
  
- [ ] Usage and notes
      - [ ] C
      - [ ] C++
      - [ ] C#
      - [ ] Python
      - [ ] CCL
      - [ ] LIB_PATH/DYLIB_PATH

- [ ] CCL
      - DOLIST
      - simplify main loop with `thereis` (https://gigamonkeys.com/book/loop-for-black-belts.html)

- [ ] GetCardByIndex should return 'card deleted' error for ff ff ff ff in response

## TODO

- (?) lint
- (?) Cross-compile
      - https://github.com/elastic/golang-crossbuild
      - https://dh1tw.de/2019/12/cross-compiling-golang-cgo-projects/

