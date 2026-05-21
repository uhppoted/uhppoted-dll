# Build Notes for Visual Studio for Mac

1. The local copy of the _uhppoted_ bindings have been updated to use the `libuhppoted.dylib` 
   shared library on Mac.

2. The local copy of the _uhppoted_ bindings have been partially updated for C# 8.0. The original 
   bindings are compatible with _Mono_ which is only C# 7.0 compatible and does not include some
   nice features (like e.g. nullable references).

3. The project must set environment variable DYLD_LIBRARY_PATH to include the `libuhppoted.dylib` 
   folder.


