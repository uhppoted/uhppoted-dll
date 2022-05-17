## v0.7.x

### IN PROGRESS

- [ ] Github workflow
      - [x] tests
            - [x] C
            - [x] C++
            - [x] C#
            - [x] Python
            - [x] CCL

      - [ ] examples
```
examples.cpp:181:15: error: ‘find_if’ was not declared in this scope
  181 |     auto it = find_if(commands.begin(), commands.end(), [=](const command &v) { return v.cmd == cmd; });
      |               ^~~~~~~
```

- [ ] Constants for event types, reason, etc
      - [ ] lookup(...)
      - [ ] Update examples
      - [ ] Update tests

- [ ] Extend examples to use command line args
      - [x] C
      - [x] C++
      - [x] C#
      - [x] Python
      - [ ] CCL
  
- [ ] Usage and notes
      - [ ] C
      - [ ] C++
      - [ ] C#
      - [ ] Python
      - [ ] CCL
      - [ ] LIB_PATH/DYLIB_PATH

- [ ] CCL
      - DOLIST
      - simplify main loop with `thereis` (https://gigamonkeys.com/book/loop-for-black-belts.html)

- [ ] GetCardByIndex should return 'card deleted' error for ff ff ff ff in response

## TODO

- (?) lint
- (?) Cross-compile
      - https://github.com/elastic/golang-crossbuild
      - https://dh1tw.de/2019/12/cross-compiling-golang-cgo-projects/

