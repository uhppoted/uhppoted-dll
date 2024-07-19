# TODO

- [ ] event listener (cf. https://github.com/uhppoted/uhppoted-dll/issues/11)
      - [x] CHANGELOG
      - [x] README
      - [x] _userdata_
            - [x] C
            - [x] C++
            - [x] Python
                  - [ ] wrap userdata in struct
            - [x] C#
            - [x] CCL
            - [x] documentation
      - [x] Change _running_ to _listening_
      - [x] documentation
      - [x] Go
      - [x] C
      - [x] Python
      - [x] C++
      - [x] CCL
      - [ ] C#
            - https://learn.microsoft.com/en-us/dotnet/standard/native-interop/pinvoke
            - [ ] run/stop should be volatile
                  - VolatileRead/Write
                  - https://learn.microsoft.com/en-us/dotnet/api/system.threading.thread?view=net-8.0
                  - https://learn.microsoft.com/en-us/dotnet/standard/threading/using-threads-and-threading#how-to-stop-a-thread
            - [ ] Use ThreadCancellationToken
                  - https://learn.microsoft.com/en-us/dotnet/api/system.threading.cancellationtoken?view=net-8.0
                  - https://learn.microsoft.com/en-us/dotnet/standard/threading/using-threads-and-threading#how-to-stop-a-thread
            - [ ] Compile and test vs-mac
            - [ ] Copy vs-mac to vs-win
            - [ ] Use CallingConvention and [[MarshalAs(UnmanagedType.LPUTF8Str)] ]
                  - https://stackoverflow.com/questions/20752001/passing-strings-from-c-sharp-to-c-dll-and-back-minimal-example
                  - https://limbioliong.wordpress.com/2011/06/16/returning-strings-from-a-c-api
                  - https://learn.microsoft.com/en-us/dotnet/api/system.runtime.interopservices.unmanagedtype?view=net-8.0
                  - https://learn.microsoft.com/en-us/dotnet/api/system.runtime.interopservices.marshal.ptrtostringansi?view=net-8.0

- [ ] TCP/IP protocol (cf. https://github.com/uhppoted/uhppote-core/issues/17)


## TODO

- (?) [llgo](https://github.com/goplus/llgo)
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

