![build](https://github.com/uhppoted/uhppoted-dll/workflows/build/badge.svg)

# uhppoted-dll

Shared library (DLL) for `uhppote-core` with bindings and examples for:

- C
- C++
- C#
- Python
- Clozure Common Lisp

Supported operating systems:
- Linux
- MacOS
- Windows
- RaspberryPi (ARM7)

## Releases

| *Version* | *Description*                                                                             |
| --------- | ----------------------------------------------------------------------------------------- |
|           |                                                                                           |
|           |                                                                                           |
|           |                                                                                           |

## Development

### Building from source

Assuming you have `Go` and `make` installed:

```
git clone https://github.com/uhppoted/uhppoted-dll.git
cd uhppoted-dll
make build
```

If you prefer to build manually:
```
git clone https://github.com/uhppoted/uhppoted-dll.git
cd uhppoted-core
mkdir lib
go build -trimpath -buildmode=c-shared -o ./lib/libuhppoted.so ./...
```

#### Dependencies

| *Dependency*                                             | *Description*                                          |
| -------------------------------------------------------- | ------------------------------------------------------ |
| [uhppote-core](https://github.com/uhppoted/uhppote-core) | Device level API implementation                        |
|                                                          |                                                        |

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
- [`GetCardById`](#getcardbyid)
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

#### `GetCardByID`

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

