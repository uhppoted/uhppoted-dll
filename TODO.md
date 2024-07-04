# TODO

- [ ] event listener (cf. https://github.com/uhppoted/uhppoted-dll/issues/11)
      - [x] Go
            - [ ] debug
            - [ ] test
            - [ ] Relook at error handling
            - (?) Move implementation to events.go
      - [x] C
      - [x] CCL
      - [ ] C++
            - [ ] on_error
            - [ ] MAYBE: https://stackoverflow.com/questions/28746744/passing-capturing-lambda-as-function-pointer
            - [ ] FIXME rework uhppoted_exception for const char *
      - [ ] C#
            - [ ] Compile and test vs-mac
            - [ ] Copy vs-mac to vs-win
      - [ ] Python
            - [ ] return code is ctypes.c_int32

- [ ] TCP/IP protocol (cf. https://github.com/uhppoted/uhppote-core/issues/17)

- [ ] C++: use `nullptr`
      - https://stackoverflow.com/questions/1282295/what-is-the-nullptr-keyword-and-why-is-it-better-than-null

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

