# TODO

- [x] event listener (cf. https://github.com/uhppoted/uhppoted-dll/issues/11)
- [x] TCP/IP protocol (cf. https://github.com/uhppoted/uhppote-core/issues/17)
- [ ] Merge LTSC changes back into main branch (cf. https://github.com/uhppoted/uhppoted-dll/issues/12)
      - [ ] CCL: return code is a pointer ??
      - [ ] CCL: fix stacked multiple-value-binds
      - [x] get-devices
      - [x] get-device
            - [ ] Fix parameters ordering
            - [ ] Use C.cchar_t
      - [x] set-address
      - [x] get-status
      - [x] get-time
      - [ ] set-time
          - [ ] preallocate struct strings
          - [ ] C
          - [ ] C++
          - [ ] Python
          - [ ] C#: mono
          - [ ] C#: vs-mac
          - [ ] C#: vs-win
          - [ ] CCL
      - [ ] get-listener
      - [ ] set-listener
      - [ ] get-door-control
      - [ ] set-door-control
      - [ ] open-door
      - [ ] get-cards
      - [ ] get-card
      - [ ] get-card-by-index
      - [ ] put-card
      - [ ] delete-card
      - [ ] delete-cards
      - [ ] get-event-index
      - [ ] set-event-index
      - [ ] get-event
      - [ ] record-special-events
      - [ ] get-time-profile
      - [ ] set-time-profile
      - [ ] clear-time-profiles
      - [ ] add-task
      - [ ] refresh-tasklist
      - [ ] clear-tasklist
      - [ ] set-pc-control
      - [ ] set-interlock
      - [ ] activate-keypads
      - [ ] set-door-passcodes
      - [ ] restore-default-parameters
      - [ ] listen

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

## Notes

1. .NET
    - https://learn.microsoft.com/en-us/dotnet/standard/native-interop/pinvoke
    - https://stackoverflow.com/questions/20752001/passing-strings-from-c-sharp-to-c-dll-and-back-minimal-example
    - https://limbioliong.wordpress.com/2011/06/16/returning-strings-from-a-c-api
    - https://learn.microsoft.com/en-us/dotnet/api/system.runtime.interopservices.unmanagedtype?view=net-8.0
    - https://learn.microsoft.com/en-us/dotnet/api/system.runtime.interopservices.marshal.ptrtostringansi?view=net-8.0


