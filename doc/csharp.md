# C\#: Usage and Notes

The 'C\#' bindings comprise a single file [uhppoted.cs](https://github.com/uhppoted/uhppoted-dll/blob/master/bindings/csharp/uhppoted.cs) - copy this to wherever makes it sense in your project. 

The generated DLL (Windows), shared lib (Linux) or dylib (MacOS) should be placed in either the library search path for 
the platform:
- LD_LIBRARY (Linux)
- DYLIB_LIBRARY (MacOS)
- Windows - see [Dynamic-Link Library Search Order](https://docs.microsoft.com/en-us/windows/win32/dlls/dynamic-link-library-search-order)

or in the same folder as the executable.

Examples illustrating the use of the 'C\#' API can be found in the [examples/csharp](https://github.com/uhppoted/uhppoted-dll/tree/master/examples/csharp) folder.

## API

Invoking an API function requires an instance of the `Uhppoted` class initialised with the information required to access a controller:

```
public Uhppoted(string bind, string broadcast, string listen, int timeout, Controller[] controllers, bool debug)

bind        IPv4 address:port to which to bind the UDP socket. Defaults to 0.0.0.0:0
broadcast   IPv4 address:port for broadcast UDP packets. Defaults to 255.255.255.255:60000
listen      IPv4 address:port for events from controller (unused). Defaults to 0.0.0.0:60001
timeout     milliseconds to wait for a reply. Defaults to 5 seconds.
controllers Optional list of specific controllers and their associated IPv4 addresses (e.g.for controllers
            located on a different interface, a VLAN or a VPN)
debug       Displays the DLL and controller requests/responses if true.
```

The `Uhppoted` _destructor_ frees any dynamically allocated memory associated with:
- the internal _UHPPOTE_ _struct_
- the global error message

### `UhppotedException`
All API functions throw a `UhppotedException` if the call fails for any reason whatsoever.

```
public class UhppotedException : Exception {
    public UhppotedException(string message) : base(message) {}
}
```

### `lookup`

The `lookup.find` method is a static utility function to looku text equivalents to the following byte codes:
- door control mode
- event type
- event reason
- event direction

```
public class lookup {
    ...
    public static string find(string category, uint code, string locale)
    ...
}

category  Lookup table to use ("door.mode", "event.direction", "event.type" or "event.reason")
code      code from the returned information
locale    Language code to use for the text lookup. Defaults to uk-en.
```

### `Uhppoted::GetDevices`
```
public uint[] GetDevices()

Returns an array of controller serial numbers if the call succeeded.

Throws a UhppotedException if the call failed.
```

### `Uhppoted::GetDevice`
```
public Device GetDevice(uint ID)

ID  controller serial number 

Returns a Device class with populated with the controller device information if the call succeeded.

Throws a UhppotedException if the call failed.
```

### `Uhppoted::SetAddress`
```
public void SetAddress(uint ID, string address, string subnet, string gateway) 

ID       controller serial number 
address  controller IPv4 address
subnet   controller IPv4 subnet mask
gateway  controller gateway IPv4 address

Throws a UhppotedException if the call failed.
```

### `Uhppoted::GetStatus`
```
public Status GetStatus(uint ID)

ID  controller serial number 

Returns a Status class with populated with the controller status information if the call succeeded.

Throws a UhppotedException if the call failed.
```

### `Uhppoted::GetTime`
```
public string GetTime(uint ID)

ID  controller serial number 

Returns a date/time string (YYYY-MM-dd HH:mm:ss) the controller current date/time if the call succeeded.

Throws a UhppotedException if the call failed.
```

### `Uhppoted::SetTime`
```
public void SetTime(uint ID, string datetime)

ID        controller serial number 
datetime  date/time string (YYYY-MM-dd HH:mm:ss)

Throws a UhppotedException if the call failed.
```

### `Uhppoted::GetListener`
```
public string GetListener(uint ID)

ID  controller serial number 

Returns the controller event listener IPv4 address:port as a string if the call succeeded.

Throws a UhppotedException if the call failed.
```

### `Uhppoted::SetListener`
```
public void SetListener(uint ID, string listener)

ID        controller serial number 
listener  listener IPv4 address:port string

Throws a UhppotedException if the call failed.
```

### `Uhppoted::GetDoorControl`
```
public DoorControl GetDoorControl(uint ID, byte door) 

ID    controller serial number 
door  door ID [1..4]

Returns a DoorControl class with populated with the controller door configuration if the call succeeded.

Throws a UhppotedException if the call failed.
```

### `Uhppoted::SetDoorControl`
```
public void SetDoorControl(uint ID, byte door, byte mode, byte delay)

ID    controller serial number 
door  door ID [1..4]
mode  normally open (1), normally closed (2) or controlled (3)
delay door open delay in seconds

Throws a UhppotedException if the call failed.
```

### `Uhppoted::OpenDoor`
```
public void OpenDoor(uint ID, byte door)

ID    controller serial number 
door  door ID [1..4]

Throws a UhppotedException if the call failed.
```

### `Uhppoted::GetCards`
```
public uint GetCards(uint ID) 

ID  controller serial number 

Returns the number of cards stored on the controller if the call succeeded.

Throws a UhppotedException if the call failed.
```

### `Uhppoted::GetCard`
```
public Card GetCard(uint ID, uint cardNumber)

ID           controller serial number 
card_number  card number

Returns a Card class with the controller card information if the call succeeded.

Throws a UhppotedException if the call failed.
```

### `Uhppoted::GetCardByIndex`
```
public Card GetCardByIndex(uint ID, uint index)

ID     controller serial number 
index  index of card to retrieve

Returns a Card class with the controller card information if the call succeeded.

Throws a UhppotedException if the call failed.
```

### `Uhppoted::PutCard`
```
public void PutCard(uint ID, uint cardNumber, string from, string to, byte[] doors)

ID           controller serial number 
card_number  card number
from         card valid from date, inclusive (YYYY-MM-dd)
to           card valid until, inclusive (YYYY-MM-dd)
doors        4 byte array with card permissions

Throws a UhppotedException if the call failed.
```

### `Uhppoted::DeleteCcard`
```
public void DeleteCard(uint ID, uint cardNumber)

ID           controller serial number 
card_number  card number

Throws a UhppotedException if the call failed.
```

### `Uhppoted::DeleteCards`
```
public void DeleteCards(uint ID)

ID  controller serial number 

Throws a UhppotedException if the call failed.
```

### `Uhppoted::GetEventIndex`
```
public uint GetEventIndex(uint ID)

ID     controller serial number 

Returns the controller event index if the call succeeded.

Throws a UhppotedException if the call failed.
```

### `Uhppoted::SetEventIndex`
```
public void SetEventIndex(uint ID, uint index) 

ID     controller serial number 
index  controller event index

Throws a UhppotedException if the call failed.
```

### `Uhppoted::GetEvent`
```
public Event GetEvent(uint ID, uint index)

ID     controller serial number 
index  index of event to retrieve

Returns an event class with the controller event stored at the index.

Throws a UhppotedException if the call failed.
```

### `Uhppoted::RecordSpecialEvents`
```
public void RecordSpecialEvents(uint ID, bool enabled)

ID       controller serial number 
enabled  Enables/disables recording of door, etc events

Throws a UhppotedException if the call failed.
```

### `Uhppoted::GetTimeProfile`
```
public TimeProfile GetTimeProfile(uint ID, byte profileID)

ID          controller serial number 
profile_ID  ID [2..254] of time profile to retrieve

Returns a TimeProfile class with the time profile stored at the profile ID on the controller.

Throws a UhppotedException if the call failed.
```

### `Uhppoted::SetTimeProfile`
```
public void SetTimeProfile(uint ID, TimeProfile profile)

ID       controller serial number 
profile  time_profile class initialised with the time profile to store on the controller.

Throws a UhppotedException if the call failed.
```

### `Uhppoted::ClearTimeProfiles`
```
public void ClearTimeProfiles(uint ID)

ID  controller serial number 

Throws a UhppotedException if the call failed.
```

### `Uhppoted::AddTask`
```
public void AddTask(uint ID, Task task)

ID    controller serial number 
task  task class initialised with the task to store on the controller.

Throws a UhppotedException if the call failed.
```

### `Uhppoted::RefreshTasklist`
```
public void RefreshTaskList(uint ID)

ID  controller serial number 

Throws a UhppotedException if the call failed.
```

### `Uhppoted::ClearTasklist`
```
public void ClearTaskList(uint ID)

ID  controller serial number 

Throws a UhppotedException if the call failed.
```

## Types

### `Controller`
Container class for use with the `uhppoted` _conclassor_.
```
public class Controller {
    public uint ID;
    public string address;

}
```

### `Device`
Container class for the controller information retrieved by `get_device`
```
public class Device {
    public uint ID;
    public string address;
    public string subnet;
    public string gateway;
    public string MAC;
    public string version;
    public string date;
}
```

### `Event`
Container class for the event information retrieved by `get_event`.
```
public class Event {
    public string timestamp;
    public uint index;
    public byte eventType;
    public bool granted;
    public byte door;
    public byte direction;
    public uint card;
    public byte reason;
}
```

### `Status`
Container class for the controller status information retrieved by `get_status`
```
public class Status {
    public uint ID;
    public string sysdatetime;
    public bool[] doors;
    public bool[] buttons;
    public byte relays;
    public byte inputs;
    public byte syserror;
    public byte info;
    public uint seqno;
    public Event evt;
}
```

### `DoorControl`
Container class for door configuration for `get_door_control` and `set_door_control`.
```
public class DoorControl {
    public byte mode;
    public byte delay;
}
```

### `Card`
Container class for card information retrieved by `get_card` and `get_card_by_index`.
```
public class Card {
    public uint cardNumber;
    public string from;
    public string to;
    public byte[] doors;
}
```

### `TimeProfile`
Container class for time profile information retrieved/set by `get_time_profile` and 
`set_time_profile`.
```
public class TimeProfile {
    public byte ID;
    public byte linked;
    public string from;
    public string to;
    public bool monday;
    public bool tuesday;
    public bool wednesday;
    public bool thursday;
    public bool friday;
    public bool saturday;
    public bool sunday;
    public string segment1start;
    public string segment1end;
    public string segment2start;
    public string segment2end;
    public string segment3start;
    public string segment3end;
}
```

### task
Container class for the task information required for `add_task`.
```
public class Task {
    public byte task;
    public byte door;
    public string from;
    public string to;
    public bool monday;
    public bool tuesday;
    public bool wednesday;
    public bool thursday;
    public bool friday;
    public bool saturday;
    public bool sunday;
    public string at;
    public byte cards;
}
```
