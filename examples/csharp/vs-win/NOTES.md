# Build Notes for Visual Studio for Windows

1. Building the DLL locally on Windows is straightforward - but **absolutely** requires the _mingw_ compiler
   to be installed.

2. The local copy of the _uhppoted_ bindings have been partially updated for C# 8.0. The original 
   bindings are compatible with _Mono_ which is only C# 7.0 compatible and does not include some
   nice features (like e.g. nullable references).

3. The project must set the PATH environment variable to include the `uhppoted.dll` folder (e.g. see the 
   included _Makefile_).


