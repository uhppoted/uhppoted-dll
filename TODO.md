# TODO

- [x] Rework to use preallocated strings and errors (cf. https://github.com/uhppoted/uhppoted-dll/issues/12)
- [ ] event listener (cf. https://github.com/uhppoted/uhppoted-dll/issues/11)
      - [x] Import event listener code from _main_
      - [x] Update vs-mac example
      - [x] Update vs-windows example
      - [ ] LTSC
            - Use CallingConvention
            - Ref. https://stackoverflow.com/questions/20752001/passing-strings-from-c-sharp-to-c-dll-and-back-minimal-example
            - Ref. https://limbioliong.wordpress.com/2011/06/16/returning-strings-from-a-c-api
            - Ref. https://limbioliong.wordpress.com/2011/06/03/passing-structures-between-managed-and-unmanaged-code
            - https://stackoverflow.com/questions/827672/how-do-i-marshal-a-structure-as-a-pointer-to-a-structure
            - https://learn.microsoft.com/en-us/dotnet/standard/native-interop/best-practices#guids
            - https://github.com/dotnet/docs/blob/main/docs/standard/native-interop/best-practices.md
            - https://limbioliong.wordpress.com/2011/11/01/using-the-stringbuilder-in-unmanaged-api-calls/

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

