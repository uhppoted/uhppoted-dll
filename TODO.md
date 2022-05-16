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
make -C ./tests/ccl -f Makefile tests
make[1]: Entering directory '/home/runner/work/uhppoted-dll/uhppoted-dll/tests/ccl'
rlwrap ccl64 --eval '(setenv "LD_LIBRARY_PATH" "../../lib/tests")' --load main.lisp --eval '(make-app)' 
> Error: Error opening shared library libuhppoted.dylib : libuhppoted.dylib: cannot open shared object file: No such file or directory.
```
      - [ ] examples

- [x] Replace unsafe.Slice code with standard cgo functions
      - [x] func C.GoBytes(unsafe.Pointer, C.int) []byte

- [ ] Constants for event types, reason, etc

- [ ] Extend examples to use command line args
      - [x] C
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

