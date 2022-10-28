# TODO

### IN PROGRESS

- [ ] https://github.com/uhppoted/uhppoted-dll/issues/2
      - [ ] UHPPOTE struct test
      - [x] C
      - [ ] C++
      - [x] C#
      - [x] Python
      - [x] CCL

- [ ] C++ _structs_ test fails on github
```
g++ -std=c++17 -Wall  -o tests tests.cpp device.cpp cards.cpp events.cpp time_profiles.cpp tasks.cpp lookup.cpp structs.cpp ../../bindings/c++/src/uhppoted.cpp -I../../bindings/c++/include -I../../lib/tests -L../../lib/tests -luhppoted
GODEBUG=cgocheck=2 && export LD_LIBRARY_PATH=../../lib/tests && ./tests
get-devices           ok
...
get-card-by-index     ok

 *** ERROR ���U is not a valid UDP address:port
 ```

- [ ] https://github.com/uhppoted/uhppoted-dll/issues/1
      - [x] tests for lookup(...)
      - [ ] codegen lookup(..)

## TODO

- [ ] CCL
      - [ ] Rework timeout logic for get-device etc

- (?) Cross-compile
      - (?) https://github.com/elastic/golang-crossbuild
      - (?) https://dh1tw.de/2019/12/cross-compiling-golang-cgo-projects/
      - (?) zig-cc: https://jakstys.lt/2022/how-uber-uses-zig/
      - (?) github actions

- (?) lint

