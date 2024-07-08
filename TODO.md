# TODO

- [ ] event listener (cf. https://github.com/uhppoted/uhppoted-dll/issues/11)
      - [x] Go
            - [x] debug
            - [x] test
            - [ ] Change timestamp back to const char *
            - [ ] Relook at error handling
      - [x] C
            - [x] test
      - [ ] C++
            - [x] test
            - [ ] on_error
            - [ ] rework using <functional>
                  - https://stackoverflow.com/questions/28746744/passing-capturing-lambda-as-function-pointer
            - [ ] FIXME rework uhppoted_exception for const char *
      - [ ] C#
            - [ ] test
            - [ ] Compile and test vs-mac
            - [ ] Copy vs-mac to vs-win
            - [ ] Use CallingConvention and [[MarshalAs(UnmanagedType.LPUTF8Str)] ]
                  - https://stackoverflow.com/questions/20752001/passing-strings-from-c-sharp-to-c-dll-and-back-minimal-example
                  - https://limbioliong.wordpress.com/2011/06/16/returning-strings-from-a-c-api
                  - https://learn.microsoft.com/en-us/dotnet/api/system.runtime.interopservices.unmanagedtype?view=net-8.0

      - [ ] Python
            - [ ] test
            - [ ] return code is ctypes.c_int32
      - [ ] CCL
            - [ ] test

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

