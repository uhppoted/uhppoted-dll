# CHANGELOG

## Unreleased

### Added
1. VB.NET example GUI application authored by @threeeye.


## [0.8.8](https://github.com/uhppoted/uhppoted-dll/releases/tag/v0.8.8) - 2024-03-27

### Added
1. Implemented `restore-default-parameters` function to reset a controller to the manufacturer default configuration.

### Updated
1. Bumped Go version to 1.22.


## [0.8.7](https://github.com/uhppoted/uhppoted-dll/releases/tag/v0.8.7) - 2023-12-01

### Added
1. `set-door-passcodes` command to set supervisor passcodes for a door.
2. Visual Studio examples for C#

### Updated
1. Updated bindings documentation for card PINs.
2. Fixed handling for `get-status` response without a valid event (cf. https://github.com/uhppoted/uhppoted-dll/issues/7)
3. Replaced `nil` event pointer in `get-status` with zero value.


## [0.8.6](https://github.com/uhppoted/uhppoted-dll/releases/tag/v0.8.6) - 2023-08-30

### Added
1. `activate-keypads` command to activate and deactivate a controller reader access keypads.


## [0.8.5](https://github.com/uhppoted/uhppoted-dll/releases/tag/v0.8.5) - 2023-06-13

### Added
1. `set-interlock` command to set a controller door interlock mode.

### Updated
1. Reworked card _from_ and _to_ date fields to use zero type rather than pointers.
2. Added _govulncheck_ to CI build.
3. Added _staticcheck_ to CI build.


## [0.8.4](https://github.com/uhppoted/uhppoted-dll/releases/tag/v0.8.4) - 2023-03-17

### Added
1. Overview Go doc.
2. `SetPCControl` command to enable or disable controller remote access control.
3. Added card PIN field to models and bindings.


## [0.8.3](https://github.com/uhppoted/uhppoted-dll/releases/tag/v0.8.3) - 2022-12-16

### Changed
1. Removed unused `ndevices` field from Python bindings (ref. https://github.com/uhppoted/uhppoted-dll/issues/2)
2. Fixed address copy bug in C++ bindings.
3. Fixed hard-coded controller list length in C# bindings (ref. https://github.com/uhppoted/uhppoted-dll/issues/4)


## [0.8.2](https://github.com/uhppoted/uhppoted-dll/releases/tag/v0.8.2) - 2022-10-14

### Changed
1. Added event reason lookup for _swipe open_ and _swipe closed_
2. Started internationalising messages:
   - Restructured sources to use seperate `lookup.xxx` file for lookup codes
   - Codegen'd lookup codes from single internationalisation JSON model


## [0.8.1](https://github.com/uhppoted/uhppoted-dll/releases/tag/v0.8.1) - 2022-08-01

### Changed
1. Maintenance release for compatibility with [uhppoted-core](https://github.com/uhppoted/uhppote-core) v0.8.1


## [0.8.0](https://github.com/uhppoted/uhppoted-dll/releases/tag/v0.8.0) - 2022-07-01

### Changed
1. Updated for compatibility with [uhppoted-core](https://github.com/uhppoted/uhppote-core) v0.8.0


## [0.7.3](https://github.com/uhppoted/uhppoted-dll/releases/tag/v0.7.3) - 2022-06-01

### Added
1. Initial release
2. Includes bindings and examples for:
   - C
   - C++
   - C#
   - Python
   - Clozure Common Lisp

