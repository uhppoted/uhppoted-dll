## v0.7.x

### IN PROGRESS
  - [x] revert to C++11 (among other things it seems github's VPSs don't have glib suppport for C++17)

  - [ ] Commonalize examples output
        - [ ] C
        - [ ] C++
        - [ ] C#
        - [x] Python
        - [ ] CCL

  - [ ] Usage and notes
        - [ ] C
        - [ ] C++
        - [ ] C#
        - [ ] Python
        - [ ] CCL
        - [ ] LIB_PATH/DYLIB_PATH

  - [ ] Github workflow
        - [ ] examples
        - [ ] tests
              - [x] C
              - [ ] C++
```
tests.cpp:(.text+0x1e): undefined reference to `std::__cxx11::basic_string<char, std::char_traits<char>, std::allocator<char> >::basic_string()'
```
```
gcc (Ubuntu 9.4.0-1ubuntu1~20.04.1) 9.4.0
Copyright (C) 2019 Free Software Foundation, Inc.
This is free software; see the source for copying conditions.  There is NO
warranty; not even for MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

g++ (Ubuntu 9.4.0-1ubuntu1~20.04.1) 9.4.0
Copyright (C) 2019 Free Software Foundation, Inc.
This is free software; see the source for copying conditions.  There is NO
warranty; not even for MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

Ubuntu clang version 11.0.0-2~ubuntu20.04.1
Target: x86_64-pc-linux-gnu
Thread model: posix
InstalledDir: /usr/bin
Ubuntu clang version 11.0.0-2~ubuntu20.04.1
Target: x86_64-pc-linux-gnu
Thread model: posix
InstalledDir: /usr/bi
```
              - [ ] C#
              - [ ] Python
              - [ ] CCL

  - [ ] Change all bind/broadcast/listen addresses to generic system IPs
  - [ ] Constants for event types, reason, etc
  - [ ] Extend examples to use command line args
  - [ ] C++ 
        - make doors/inputs in status bool
  - [ ] CCL
        - DOLIST
        - simplify main loop with `thereis` (https://gigamonkeys.com/book/loop-for-black-belts.html)
  - [ ] GetCardByIndex should return 'card deleted' error for ff ff ff ff in response

## TODO

- (?) lint
- (?) Cross-compile
      - https://github.com/elastic/golang-crossbuild

