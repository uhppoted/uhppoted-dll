# TODO

- [ ] https://github.com/uhppoted/uhppoted-dll/issues/7
      - [x] Go
      - [x] C examples
      - [x] C++ examples
      - [x] Python examples
      - [x] C# examples
      - [x] CCL examples
      - [x] Add get-status-no-event use case to tests
      - [ ] integration tests
      - [x] CHANGELOG
      - [x] README
      - [x] doc

```
... request
 ...          00000000  17 20 00 00 51 8d 38 19  00 00 00 00 00 00 00 00
 ...          00000010  00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00
 ...          00000020  00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00
 ...          00000030  00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00
 ...

 ... received 64 bytes from 192.168.1.100:60000
 ... response
 ...          00000000  17 20 00 00 51 8d 38 19  00 00 00 00 00 00 00 00
 ...          00000010  00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00
 ...          00000020  00 00 00 00 83 11 16 20  00 00 00 00 00 00 00 00
 ...          00000030  00 00 00 23 09 29 00 00  00 00 00 00 00 00 00 00
```

- [x] `set-door-passcodes` (cf. https://github.com/uhppoted/uhppoted/issues/40)


## TODO

- [ ] C++: rework to use unique_ptr
- (?) clang-tidy

- [ ] Explore building DLLs on github
      - MacOS actions
      - https://github.blog/2022-11-02-github-partners-with-arm-to-revolutionize-internet-of-things-software-development-with-github-actions/

- [ ] https://github.com/uhppoted/uhppoted-dll/issues/1
      - [ ] codegen lookup(..)

- [ ] Link to musl for Linux
      - https://honnef.co/posts/2015/06/statically_compiled_go_programs__always__even_with_cgo__using_musl

- [ ] CCL
      - [ ] Rework timeout logic for get-device etc

- (?) Cross-compile
      - (?) https://github.com/elastic/golang-crossbuild
      - (?) https://dh1tw.de/2019/12/cross-compiling-golang-cgo-projects/
      - (?) zig-cc: https://jakstys.lt/2022/how-uber-uses-zig/
      - (?) github actions

- (?) lint

