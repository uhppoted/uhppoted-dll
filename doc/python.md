# Python: Usage and Notes

The Python bindings comprise a single file [uhppoted.py](https://github.com/uhppoted/uhppoted-dll/blob/master/bindings/python/uhppoted.py) - copy this to wherever makes it sense in your project. 

The generated DLL (Windows), shared lib (Linux) or dylib (MacOS) should be placed in either the library search path for the platform:
- LD_LIBRARY (Linux)
- DYLIB_LIBRARY (MacOS)
- Windows - see [Dynamic-Link Library Search Order](https://docs.microsoft.com/en-us/windows/win32/dlls/dynamic-link-library-search-order)

or in the same folder as the executable.

Examples illustrating the use of the Python API can be found in the [examples/python](https://github.com/uhppoted/uhppoted-dll/tree/master/examples/python) folder.

## API

Invoking an API function requires an instance of the `uhppoted.Uhppote` class initialised with the information required to access a controller:

```
class Uhppote:
    def __init__(self, uhppote=None):

where uhppote is an instance of 

class UHPPOTE:
    bind: str
    broadcast: str
    listen: str
    timeout: int
    controllers: list[Controller]
    debug: bool

bind        IPv4 address:port to which to bind the UDP socket. Defaults to 0.0.0.0:0
broadcast   IPv4 address:port for broadcast UDP packets. Defaults to 255.255.255.255:60000
listen      IPv4 address:port for events from controller (unused). Defaults to 0.0.0.0:60001
timeout     milliseconds to wait for a reply. Defaults to 5 seconds.
controllers Optional list of specific controllers and their associated IPv4 addresses (e.g.for controllers
            located on a different interface, a VLAN or a VPN)
debug       Displays the DLL and controller requests/responses if true.
```

All API functions raise an `Exception` if the call fails for any reason whatsoever.

### `lookup`

The `lookup` method is a static utility function to lookup text equivalents to the following byte codes:
- door control mode
- event type
- event reason
- event direction

```
lookup(category, code, locale)

category  Lookup table to use ("door.mode", "event.direction", "event.type" or "event.reason")
code      code from the returned information
locale    Language code to use for the text lookup. Defaults to uk-en.
```

### `get_devices`
```
uhppoted.get_devices()

Returns an array of controller serial numbers if the call succeeded.

Raises an Exception if the call failed.
```

### `get_device`
```
uhppoted.get_device(ID)

ID  controller serial number 

Returns a Device dataclass instance populated with the controller device information if the call succeeded.

Raises an Exception if the call failed.
```

### `set_address`
```
uhppoted.set_address(self, ID, address, subnet, gateway)

ID       controller serial number 
address  controller IPv4 address
subnet   controller IPv4 subnet mask
gateway  controller gateway IPv4 address

Raises an Exception if the call failed.
```

### `get_status`
```
uhppoted.get_status(ID)

ID  controller serial number 

Returns a Status dataclass instance populated with the controller status information if the call succeeded.

The controller status includes the most recently logged event but if the controller has no events
the `Event` field will have an index of 0 and all the other fields will be invalid.

Raises an Exception if the call failed.
```

### `get_time`
```
uhppoted.get_time(ID)

ID  controller serial number 

Returns a date/time string (YYYY-MM-dd HH:mm:ss) with the controller current date/time if the call succeeded.

Raises an Exception if the call failed.
```

### `set_time`
```
uhppoted.set_time(ID, datetime)

ID        controller serial number 
datetime  date/time string (YYYY-MM-dd HH:mm:ss)

Raises an Exception if the call failed.
```

### `get_listener`
```
uhppoted.get_listener(ID)

ID  controller serial number 

Returns the controller event listener IPv4 address:port as a string if the call succeeded.

Raises an Exception if the call failed.
```

### `set_listener`
```
uhppoted.set_listener(ID, listener)

ID        controller serial number 
listener  listener IPv4 address:port string

Raises an Exception if the call failed.
```

### `get_door_control`
```
uhppoted.get_door_control(ID, door)

ID    controller serial number 
door  door ID [1..4]

Returns a DoorControl dataclass instance populated with the controller door configuration if the call succeeded.

Raises an Exception if the call failed.
```

### `set_door_control`
```
uhppoted.set_door_control(ID, door, mode, delay)

ID    controller serial number 
door  door ID [1..4]
mode  normally open (1), normally closed (2) or controlled (3)
delay door open delay in seconds

Raises an Exception if the call failed.
```

### `open_door`
```
uhppoted.open_door(ID, door)

ID    controller serial number 
door  door ID [1..4]

Raises an Exception if the call failed.
```

### `get_cards`
```
uhppoted.get_cards(ID)

ID  controller serial number 

Returns the number of cards stored on the controller if the call succeeded.

Raises an Exception if the call failed.
```

### `get_card`
```
uhppoted.get_card(ID, cardNumber)

ID          controller serial number 
cardNumber  card number

Returns a Card dataclass instance with the controller card information if the call succeeded.

Raises an Exception if the call failed.
```

### `get_card_by_index`
```
uhppoted.get_card_by_index(ID, index)

ID     controller serial number 
index  index of card to retrieve

Returns a Card dataclass instance with the controller card information if the call succeeded.

Raises an Exception if the call failed.
```

### `put_card`
```
uhppoted.put_card(ID, cardNumber, start, end, doors, PIN)

ID           controller serial number 
card_number  card number
from         card valid from date, inclusive (YYYY-MM-dd)
to           card valid until, inclusive (YYYY-MM-dd)
doors        4 byte array with card permissions
PIN          reader keypad PIN ([0..999999], 0 for 'no PIN')

Raises an Exception if the call failed.
```

### `delete_card`
```
uhppoted.delete_card(ID, cardNumber)

ID          controller serial number 
cardNumber  card number

Raises an Exception if the call failed.
```

### `delete_cards`
```
uhppoted.delete_cards(ID)

ID  controller serial number 

Raises an Exception if the call failed.
```

### `get_event_index`
```
uhppoted.get_event_index(ID)

ID  controller serial number 

Returns the controller event index if the call succeeded.

Raises an Exception if the call failed.
```

### `set_event_index`
```
uhppoted.set_event_index(ID, index)

ID     controller serial number 
index  controller event index

Raises an Exception if the call failed.
```

### `get_event`
```
uhppoted.get_event(ID, index)

ID     controller serial number 
index  index of event to retrieve

Returns an event dataclass instance with the controller event stored at the index.

Raises an Exception if the call failed.
```

### `record_special_events`
```
uhppoted.record_special_events(ID, enabled)

ID       controller serial number 
enabled  Enables/disables recording of door, etc events

Raises an Exception if the call failed.
```

### `get_time_profile`
```
uhppoted.get_time_profile(ID, profileID)

ID          controller serial number 
profile_ID  ID [2..254] of time profile to retrieve

Returns a TimeProfile dataclass instance with the time profile stored at the profile ID on the controller.

Raises an Exception if the call failed.
```

### `set_time_profile`
```
uhppoted.set_time_profile(ID, profile)

ID       controller serial number 
profile  TimeProfile dataclass instance initialised with the time profile to store on the controller.

Raises an Exception if the call failed.
```

### `clear_time_profiles`
```
uhppoted.clear_time_profiles(ID)

ID  controller serial number 

Raises an Exception if the call failed.
```

### `add_task`
```
uhppoted.add_task(ID, task)

ID    controller serial number 
task  Task dataclass instance initialised with the task to store on the controller.

Raises an Exception if the call failed.
```

### `refresh_tasklist`
```
uhppoted.refresh_tasklist(ID)

ID  controller serial number 

Raises an Exception if the call failed.
```

### `clear_tasklist`
```
uhppoted.clear_tasklist(ID)

ID  controller serial number 

Raises an Exception if the call failed.
```

### `set_pc_control`
```
uhppoted.set_pc_control(ID, enabled)

ID       controller serial number 
enabled  enables/disables host control

Raises an Exception if the call failed.
```

### `set_interlock`
```
uhppoted.set_interlock(ID, interlock)

ID        controller serial number 
interlock controller door interlock mode
          0: no interlock
          1: doors 1&2
          2: doors 3&4
          3: doors 1&2,3&4
          4: doors 1&2&3
          8: doors 1&2&3&4


Raises an Exception if the call failed.
```

### `activate_keypads`
```
uhppoted.activate_keypads(ID, reader1, reader2, reader3, reader4)

ID      controller serial number 
reader1 activates/deactivates reader 1 access keypad
reader2 activates/deactivates reader 2 access keypad
reader3 activates/deactivates reader 3 access keypad
reader4 activates/deactivates reader 4 access keypad


Raises an Exception if the call failed.
```

### `set-door-passcodes`
```
uhppoted.set_door_passcodes(ID, door, passcode1, passcode2, passcode3, passcode4)

ID        controller serial number 
door      door ID [1..4]
passcode1 PIN code in the range [1..999999] or 0 (for none)
passcode2 PIN code in the range [1..999999] or 0 (for none)
passcode3 PIN code in the range [1..999999] or 0 (for none)
passcode4 PIN code in the range [1..999999] or 0 (for none)

Raises an Exception if the call failed.
```


## Types

### `Controller`
Container class for use with the `uhppoted` _conclassor_.
```
@dataclass
class Controller:
    id: int
    address: str
```

### `Device`
Container class for the controller information retrieved by `get_device`
```
@dataclass
class Device:
    ID: int
    address: str
    subnet: str
    gateway: str
    MAC: str
    version: str
    date: str
```

### `Event`
Container class for the event information retrieved by `get_event`.
```
@dataclass
class Event:
    timestamp: str
    index: int
    eventType: int
    granted: bool
    door: int
    direction: int
    card: int
    reason: int
```

### `Status`
Container class for the controller status information retrieved by `get_status`

The controller status includes the most recently logged event but if the controller has no events
the `Event` field will have an index of 0 and all the other fields will be invalid.
```
@dataclass
class Status:
    ID: int
    sysdatetime: str
    doors: list[bool]
    buttons: list[bool]
    relays: int
    inputs: int
    syserror: int
    seqno: int
    info: int
    evt: Event

```

### `DoorControl`
Container class for door configuration for `get_door_control` and `set_door_control`.
```
@dataclass
class DoorControl:
    mode: int
    delay: int
```

### `Card`
Container class for card information retrieved by `get_card` and `get_card_by_index`.
```
@dataclass
class Card:
    cardNumber: int
    start: str
    end: str
    doors: list[int]
    PIN: uint
```

### `TimeProfile`
Container class for time profile information retrieved/set by `get_time_profile` and 
`set_time_profile`.
```
@dataclass
class TimeProfile:
    ID: int
    linked: int
    start: str
    end: str
    monday: bool
    tuesday: bool
    wednesday: bool
    thursday: bool
    friday: bool
    saturday: bool
    sunday: bool
    segment1start: str
    segment1end: str
    segment2start: str
    segment2end: str
    segment3start: str
    segment3end: str
```

### task
Container class for the task information required for `add_task`.
```
@dataclass
class Task:
    task: int
    door: int
    start: str
    end: str
    monday: bool
    tuesday: bool
    wednesday: bool
    thursday: bool
    friday: bool
    saturday: bool
    sunday: bool
    at: str
    cards: int
```
