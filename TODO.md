# TODO

- [x] Migrate to codeberg.org (cf. https://codeberg.org/uhppoted/uhppoted/issues/1)
- [x] 'first card' API (cf. https://codeberg.org/uhppoted/uhppoted/issues/2)
- [x] github build warning
- [x] add first-card privilege to `get-card` and `put-card` (cf. https://codeberg.org/uhppoted/uhppoted/issues/3)
    - [x] API
    - [x] C
    - [x] C++
    - [x] dotnet
    - [x] python
    - [x] CCL

## To Be Done

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


