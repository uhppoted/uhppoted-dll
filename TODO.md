# TODO

- [ ] event listener (cf. https://github.com/uhppoted/uhppoted-dll/issues/11)
      - [x] Merge to main
      - [x] Go
            - [ ] Relook at error handling
            - [ ] Move implementation to events.go
      - [x] C
      - [ ] C++
            - [ ] on_error
            - [ ] MAYBE: https://stackoverflow.com/questions/28746744/passing-capturing-lambda-as-function-pointer
            - [ ] FIXME rework uhppoted_exception for const char *
      - [x] C#
            - [ ] mono: hangs on closing after swipe
                  - https://stackoverflow.com/questions/11006506/console-mono-executables-do-not-exit-when-completed
                  - https://geekswithblogs.net/gwbarchive/application-exit-vs-environment-exit/
      - [ ] Python
      - [ ] CCL

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

