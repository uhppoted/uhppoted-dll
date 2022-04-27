## v0.7.x

### IN PROGRESS
  - [ ] Fix CLI set-time-profile

  - [x] README
        - [ ] Usage and notes
              - [ ] C
              - [ ] C++
              - [ ] C#
              - [ ] Python
              - [ ] CCL
              - [ ] LIB_PATH/DYLIB_PATH

  - [ ] Github workflow
        - [x] build
        - [ ] examples
        - [ ] tests
              - [x] C
              - [ ] C++
                    - revert to C++11 (seems github's VPSs don't have glib suppport for C++17)
              - [ ] C#
              - [ ] Python
              - [ ] CCL

  - [x] Commonalise test failure handling
        - [x] C
        - [x] C++
        - [x] C#
        - [x] Python
        - [x] CCL

  - [x] `refresh-tasklist`
        - [x] C
        - [x] C++
        - [x] C#
        - [x] Python
        - [x] CCL
        - [x] task constants

  - [ ] Figure out why can't put get/set/clearTimeProfile in a seperate file
        - [ ] Create basic example and log issue

  - [ ] `clear-task-list`
  - [ ] Extend examples to use command line args
  - [ ] C++ 
        - make doors/inputs in status bool
  - [ ] python: 
        - https://martinheinz.dev/blog/70
  - [ ] Remove doubled up exception handling in examples
  - [ ] CCL
        - DOLIST
        - simplify main loop with `thereis` (https://gigamonkeys.com/book/loop-for-black-belts.html)
  - [ ] GetCardByIndex should return 'card deleted' error for ff ff ff ff in response
  - [ ] Constants for event types, reason, etc

## TODO

- (?) lint
- (?) Cross-compile
      - https://github.com/elastic/golang-crossbuild

