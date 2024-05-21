# TODO

- [ ] Rework to use preallocated strings and errors (cf. https://github.com/uhppoted/uhppoted-dll/issues/12)
      - [x] error handling
      - [x] cstring
      - [x] `possible null reference`
      - [x] get-devices
      - [x] get-device
      - [x] set-address
      - [x] get-status
      - [x] get-time
      - [x] set-time
      - [x] get-listener
      - [x] set-listener
      - [x] get-door-control
      - [x] set-door-control
      - [x] open-door
      - [x] get-cards
      - [x] get-card
      - [x] get-card-by-index
      - [x] put-card
      - [x] delete-card
      - [x] delete-cards
      - [x] set-pc-control

- [ ] event listener (cf. https://github.com/uhppoted/uhppoted-dll/issues/11)

- [ ] TCP/IP protocol (cf. https://github.com/uhppoted/uhppote-core/issues/17)

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

