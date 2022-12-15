![build](https://github.com/uhppoted/uhppoted-dll/workflows/build/badge.svg)

# uhppoted-dll

Shared library (DLL) for `uhppote-core` with bindings and examples for:

- [C](https://github.com/uhppoted/uhppoted-dll/blob/master/doc/C.md)
- [C++](https://github.com/uhppoted/uhppoted-dll/blob/master/doc/C.md)
- [C#](https://github.com/uhppoted/uhppoted-dll/blob/master/doc/csharp.md)
- [Python]((https://github.com/uhppoted/uhppoted-dll/blob/master/doc/python.md))
- [Clozure Common Lisp]((https://github.com/uhppoted/uhppoted-dll/blob/master/doc/ccl.md))

3rd party bindings:
- [Rust](https://github.com/wouterdebie/uhppote-rs)

Supported operating systems:
- Linux
- MacOS
- Windows
- RaspberryPi (ARM7)

## Releases

| *Version* | *Description*                                                                             |
| --------- | ----------------------------------------------------------------------------------------- |
| v0.8.3    | Bug fixes for Python, C++ and C# bindings                                                 |
| v0.8.2    | Fixed runtime crash (concurrent writes) and added _swipe open_ and _swipe close_ events   |
| v0.8.1    | Maintenance release to update dependencies on `uhppote-core` and `uhppoted-lib`           |
| v0.8.0    | Maintenance release to update dependencies on `uhppote-core` and `uhppoted-lib`           |
| v0.7.3    | Initial release                                                                           |

The releases do not include binaries - cross-compiling a DLL/shared-lib is not straightforward because
the _cgo_ compiler links in a platfrom specific version of _glibc_. Building the DLL/shared-lib 
is straightforward (see below) and only requires that _go_ and _cgo_ are installed on the system.

## Development

### Installation

The DLL/shared lib/dylib has to be built from source and requires `Go`:

```
git clone https://github.com/uhppoted/uhppoted-dll.git
cd uhppoted-dll
make build
```

If you prefer to build manually:
```
   git clone https://github.com/uhppoted/uhppoted-dll.git
   cd uhppoted-dll
   go build -trimpath -buildmode=c-shared -o <lib> go/devices.go go/cards.go go/events.go go/time_profiles.go go/tasks.go go/main.go 
```

_NOTE: _The list of source files is required pending a fix for [cmd/cgo: inconsistent compiler behaviour when compiling a C.struct](https://github.com/golang/go/issues/52611#issuecomment-1120322135)_

Copy the generated DLL (Windows), shared lib (Linux) or dylib (MacOS) to the library search path for the platform:
- LD_LIBRARY (Linux)
- DYLIB_LIBRARY (MacOS)
- Windows - see [Dynamic-Link Library Search Order](https://docs.microsoft.com/en-us/windows/win32/dlls/dynamic-link-library-search-order)
- (OR) to the same folder as the project executable.

The usage for the language specfic bindings are described in their own documents:
- [C](https://github.com/uhppoted/uhppoted-dll/blob/master/doc/C.md)
- [C++](https://github.com/uhppoted/uhppoted-dll/blob/master/doc/C.md)
- [C#](https://github.com/uhppoted/uhppoted-dll/blob/master/doc/csharp.md)
- [Python]((https://github.com/uhppoted/uhppoted-dll/blob/master/doc/python.md))
- [Clozure Common Lisp]((https://github.com/uhppoted/uhppoted-dll/blob/master/doc/ccl.md))


### `debug` lib

The `debug` shared-lib/DLL displays the parameters with which the function was invoked. It _fakes_ the call
to the real controller, returning a plausible response instead.

### `test` lib

The `test` shared-lib/DLL is included for integration testing and validates that the parameters with which a 
function is invoked match expected values. As with the `debug` lib, it does not access a real controller and
returns a fixed response.


#### Dependencies

| *Dependency*                                             | *Description*                                          |
| -------------------------------------------------------- | ------------------------------------------------------ |
| [uhppote-core](https://github.com/uhppoted/uhppote-core) | Device level API implementation                        |
| [uhppoted-lib](https://github.com/uhppoted/uhppoted-lib) | Library of commonalised functionality            s      |
|                                                          |                                                        |

## Documentation

- [C](https://github.com/uhppoted/uhppoted-dll/blob/master/doc/C.md)
- [C++](https://github.com/uhppoted/uhppoted-dll/blob/master/doc/C++.md)
- [C#](https://github.com/uhppoted/uhppoted-dll/blob/master/doc/csharp.md)
- [Python](https://github.com/uhppoted/uhppoted-dll/blob/master/doc/python.md)
- [Clozure Common Lisp](https://github.com/uhppoted/uhppoted-dll/blob/master/doc/ccl.md)

## API

- [`GetDevices`](#getdevices)
- [`GetDevice`](#getdevice)
- [`SetAddress`](#setaddress)
- [`GetListener`](#getlistener)
- [`SetListener`](#setlistener)
- [`GetTime`](#gettime)
- [`SetTime`](#settime)
- [`GetDoorControlState`](#getdoorcontrolstate)
- [`SetDoorControlState`](#setdoorcontrolstate)
- [`OpenDoor`](#opendoor)
- [`GetStatus`](#getstatus)
- [`GetCards`](#getcards)
- [`GetCard`](#getcard)
- [`GetCardByIndex`](#getcardbyindex)
- [`PutCard`](#putcard)
- [`DeleteCard`](#deletecard)
- [`DeleteCards`](#deletecards)
- [`GetEvent`](#getevent)
- [`GetEventIndex`](#geteventindex)
- [`SetEventIndex`](#seteventindex)
- [`RecordSpecialEvents`](#recordspecialevents)
- [`GetTimeProfile`](#gettimeprofile)
- [`SetTimeProfile`](#settimeprofile)
- [`ClearTimeProfiles`](#cleartimeprofiles)
- [`ClearTaskList`](#cleartasklist)
- [`AddTask`](#addtask)
- [`RefreshTaskList`](#refreshtasklist)

#### `GetDevices`

Retrieves a list of all UHPPOTE controllers that respond to a broadcast `get-device` request.

#### `GetDevice`

Retrieves the controller information for a specific UHPPOTE controller from the response to a `get-device` request. 

#### `SetAddress`

Sets the IPv4 address, subnet mask and gateway address for a controller.

#### `GetListener`

Retrieves the IPv4 address of the host configured to receive events from the controller.

#### `SetListener`

Sets the IPv4 address of the host to receive events from the controller.

#### `GetTime`

Retrieves the controller date and time.

#### `SetTime`

Sets the controller date and time.

#### `GetDoorControlState`

Retrieves a door control state (`normally open`, `normally closed` or `controlled`) from the controller.

#### `SetDoorControlState`

Sets a door control state (`normally open`, `normally closed` or `controlled`) on the controller.

#### `OpenDoor`

Unlocks a door remotely.

#### `GetStatus`

Retrieves the controller current status.

#### `GetCards`

Retrieves the number of cards stored on a controller.

#### `GetCard`

Retrieves a stored card's information using the card number.

#### `GetCardByIndex`

Retrieves a stored card's information using an index into the card list.

#### `PutCard`

Adds or updates a card record on the controller.

#### `DeleteCard`

Deletes a card record from the controller.

#### `DeleteCards`

Deletes all card records from the controller.

#### `GetEvent`

Retrieves a single stored event (by index) from the controller.

#### `GetEventIndex`

Retrieves the current value of the controller event index (typically used as a marker for events
that have been retrieved).

#### `SetEventIndex`

Sets the current value of the controller event index (typically used to update the marker for events
that have been retrieved).

#### `RecordSpecialEvents`

Enables or disables the door and relay events on a controller. 

#### `GetTimeProfile`

Retrieves a numbered time profile from the controller.

#### `SetTimeProfile`

Adds or updates a numbered time profile on the controller.

#### `ClearTimeProfiles`

Deletes all stored time profiles from a controller.

#### `ClearTaskList`

Clears the scheduled task list on a specific UHPPOTE controller, preparatory to using `AddTask` and `RefreshTask`. 

#### `AddTask`

Adds a scheduled task to the task list on a specific UHPPOTE controller. The task is not activated until `RefreshTaskList` is invoked. `ClearTaskList` should have been invoked prior to invoking a sequence of `AddTask` invocations to put the task list in a known state.

#### `RefreshTaskList`

Activates all tasks added by `AddTask`.

