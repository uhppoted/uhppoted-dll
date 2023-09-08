# C++: Usage and Notes

The C++ bindings comprise a single header ([uhppoted.h](https://github.com/uhppoted/uhppoted-dll/blob/master/bindings/c++/include/uhppoted.h)) and  source file [uhppoted.cpp](https://github.com/uhppoted/uhppoted-dll/blob/master/bindings/c++/src/uhppoted.cpp) - copy these to wherever makes it sense in your project. 

The generated DLL (Windows), shared lib (Linux) or dylib (MacOS) should be placed in either the library search path for 
the platform:
- LD_LIBRARY (Linux)
- DYLIB_LIBRARY (MacOS)
- Windows - see [Dynamic-Link Library Search Order](https://docs.microsoft.com/en-us/windows/win32/dlls/dynamic-link-library-search-order)

or in the same folder as the executable.

Examples illustrating the use of the C++ API can be found in the [examples/c++](https://github.com/uhppoted/uhppoted-dll/tree/master/examples/c++) folder.

## API

Invoking an API function requires an instance of the `uhppoted` class initialised with the information required to access a controller:

```
uhppoted(const std::string bind, 
         const std::string broadcast,
         const std::string listen, 
         int timeout,
         const std::vector<controller> controllers, 
         bool debug);

bind        IPv4 address:port to which to bind the UDP socket. Defaults to 0.0.0.0:0
broadcast   IPv4 address:port for broadcast UDP packets. Defaults to 255.255.255.255:60000
listen      IPv4 address:port for events from controller (unused). Defaults to 0.0.0.0:60001
timeout     milliseconds to wait for a reply. Defaults to 5 seconds.
controllers Optional list of specific controllers and their associated IPv4 addresses (e.g.for controllers
            located on a different interface, a VLAN or a VPN)
debug       Displays the DLL and controller requests/responses if true.
```

The `uhppoted` _destructor_ frees any dynamically allocated memory associated with:
- the internal _UHPPOTE_ _struct_
- the global error message

### `uhppoted_exception`
All API functions throw a `uhppoted_exception` if the call fails for any reason whatsoever.

```
class uhppoted_exception : public virtual std::exception
```

The error message can be retrieved with e.what().

### `lookup`
Utility function to provide text equivalents to the following uint8_t codes:
- door control mode
- event type
- event reason
- event direction

```
const std::string uhppoted::lookup(const std::string &category, uint8_t code, const std::string &locale);

category  Lookup table to use ("door.mode", "event.direction", "event.type" or "event.reason")
code      code from the returned information
locale    Language code to use for the text lookup. Defaults to uk-en.
```

### `get_devices`
```
std::vector<uint32_t> uhppoted::get_devices();

Returns vector of controller serial numbers if the call succeeded.

Throws a uhppoted_exception if the call failed. The error message can be retrieved using the 
uhppoted_exception::what() method.
```

### `get_device`
```
device uhppoted::get_device(uint32_t id);

id  controller serial number 

Returns a device struct with populated with the controller device information if the call succeeded.

Throws a uhppoted_exception if the call failed. The error message can be retrieved using the 
uhppoted_exception::what() method.
```

### `set_address`
```
void uhppoted::set_address(uint32_t id, 
                           std::string &address, 
                           std::string &subnet,
                           std::string &gateway);

id       controller serial number 
address  controller IPv4 address
subnet   controller IPv4 subnet mask
gateway  controller gateway IPv4 address

Throws a uhppoted_exception if the call failed. The error message can be retrieved using the 
uhppoted_exception::what() method.
```

### `get_status`
```
status uhppoted::get_status(uint32_t id);

id  controller serial number 

Returns a status struct with populated with the controller status information if the call succeeded.

Throws a uhppoted_exception if the call failed. The error message can be retrieved using the 
uhppoted_exception::what() method.
```

### `get_time`
```
std::string uhppoted::get_time(uint32_t id);

id  controller serial number 

Returns a date/time string (YYYY-MM-dd HH:mm:ss) the controller current date/time if the call succeeded.

Throws a uhppoted_exception if the call failed. The error message can be retrieved using the 
uhppoted_exception::what() method.
```

### `set_time`
```
void uhppoted::set_time(uint32_t id, std::string &datetime);

id        controller serial number 
datetime  date/time string (YYYY-MM-dd HH:mm:ss)

Throws a uhppoted_exception if the call failed. The error message can be retrieved using the 
uhppoted_exception::what() method.
```

### `get_listener`
```
std::string uhppoted::get_listener(uint32_t id);

id  controller serial number 

Returns the controller event listener IPv4 address:port as a string if the call succeeded.

Throws a uhppoted_exception if the call failed. The error message can be retrieved using the 
uhppoted_exception::what() method.
```

### `set_listener`
```
void uhppoted::set_listener(uint32_t id, std::string &);

id        controller serial number 
listener  listener IPv4 address:port string

Throws a uhppoted_exception if the call failed. The error message can be retrieved using the 
uhppoted_exception::what() method.
```

### `get_door_control`
```
door_control uhppoted::get_door_control(uint32_t id, uint8_t door);

id    controller serial number 
door  door ID [1..4]

Returns a door_control struct with populated with the controller door configuration if the call succeeded.

Throws a uhppoted_exception if the call failed. The error message can be retrieved using the 
uhppoted_exception::what() method.
```

### `set_door_control`
```
void uhppoted::set_door_control(uint32_t id, uint8_t door, uint8_t mode, uint8_t delay);

id    controller serial number 
door  door ID [1..4]
mode  normally open (1), normally closed (2) or controlled (3)
delay door open delay in seconds

Throws a uhppoted_exception if the call failed. The error message can be retrieved using the 
uhppoted_exception::what() method.
```

### `open_door`
```
void uhppoted::open_door(uint32_t id, uint8_t door);

id    controller serial number 
door  door ID [1..4]

Throws a uhppoted_exception if the call failed. The error message can be retrieved using the 
uhppoted_exception::what() method.
```

### `get_cards`
```
int uhppoted::get_cards(uint32_t id);

id  controller serial number 

Returns the number of cards stored on the controller if the call succeeded.

Throws a uhppoted_exception if the call failed. The error message can be retrieved using the 
uhppoted_exception::what() method.
```

### `get_card`
```
card uhppoted::get_card(uint32_t id, uint32_t card_number);

id           controller serial number 
card_number  card number

Returns a card struct with the controller card information if the call succeeded.

Throws a uhppoted_exception if the call failed. The error message can be retrieved using the 
uhppoted_exception::what() method.
```

### `get_card_by_index`
```
card uhppoted::get_card_by_index(uint32_t id, uint32_t index);

id     controller serial number 
index  index of card to retrieve

Returns a card struct with the controller card information if the call succeeded.

Throws a uhppoted_exception if the call failed. The error message can be retrieved using the 
uhppoted_exception::what() method.
```

### `put_card`
```
void uhppoted::put_card(uint32_t id, 
                        uint32_t card_number, 
                        std::string from, 
                        std::string to, 
                        uint8_t doors[4],
                        uint32_t PIN);

id           controller serial number 
card_number  card number
from         card valid from date, inclusive (YYYY-MM-dd)
to           card valid until, inclusive (YYYY-MM-dd)
doors        4 byte array with card permissions
PIN          reader keypad PIN ([0..999999], 0 for 'no PIN')

Throws a uhppoted_exception if the call failed. The error message can be retrieved using the 
uhppoted_exception::what() method.
```

### `delete_card`
```
void uhppoted::delete_card(uint32_t id, uint32_t card_number);

id           controller serial number 
card_number  card number

Throws a uhppoted_exception if the call failed. The error message can be retrieved using the 
uhppoted_exception::what() method.
```

### `delete_cards`
```
void uhppoted::delete_cards(uint32_t id);

id  controller serial number 

Throws a uhppoted_exception if the call failed. The error message can be retrieved using the 
uhppoted_exception::what() method.
```

### `get_event_index`
```
uint32_t uhppoted::get_event_index(uint32_t id);

id     controller serial number 

Returns the controller event index if the call succeeded.

Throws a uhppoted_exception if the call failed. The error message can be retrieved using the 
uhppoted_exception::what() method.
```

### `set_event_index`
```
void uhppoted::set_event_index(uint32_t id, uint32_t index);

id     controller serial number 
index  controller event index

Throws a uhppoted_exception if the call failed. The error message can be retrieved using the 
uhppoted_exception::what() method.
```

### `get_event`
```
event uhppoted::get_event(uint32_t id, uint32_t index);

id     controller serial number 
index  index of event to retrieve

Returns an event struct with the controller event stored at the index.

Throws a uhppoted_exception if the call failed. The error message can be retrieved using the 
uhppoted_exception::what() method.
```

### `record_special_events`
```
void uhppoted::record_special_events(uint32_t id, bool enabled);

id       controller serial number 
enabled  Enables/disables recording of door, etc events

Throws a uhppoted_exception if the call failed. The error message can be retrieved using the 
uhppoted_exception::what() method.
```

### `get_time_profile`
```
time_profile uhppoted::get_time_profile(uint32_t id, uint8_t profile_id);

id          controller serial number 
profile_id  ID [2..254] of time profile to retrieve

Returns a time_profile struct with the time profile stored at the profile ID on the controller.

Throws a uhppoted_exception if the call failed. The error message can be retrieved using the 
uhppoted_exception::what() method.
```

### `set_time_profile`
```
void uhppoted::set_time_profile(uint32_t id, const time_profile &profile);

id       controller serial number 
profile  time_profile struct initialised with the time profile to store on the controller.

Throws a uhppoted_exception if the call failed. The error message can be retrieved using the 
uhppoted_exception::what() method.
```

### `clear_time_profiles`
```
void uhppoted::clear_time_profiles(uint32_t id);

id  controller serial number 

Throws a uhppoted_exception if the call failed. The error message can be retrieved using the 
uhppoted_exception::what() method.
```

### `add_task`
```
void uhppoted::add_task(uint32_t id, const task &task);

id    controller serial number 
task  task struct initialised with the task to store on the controller.

Throws a uhppoted_exception if the call failed. The error message can be retrieved using the 
uhppoted_exception::what() method.
```

### `refresh_tasklist`
```
void uhppoted::refresh_tasklist(uint32_t id);

id  controller serial number 

Throws a uhppoted_exception if the call failed. The error message can be retrieved using the 
uhppoted_exception::what() method.
```

### `clear_tasklist`
```
void uhppoted::clear_tasklist(uint32_t id);

id  controller serial number 

Throws a uhppoted_exception if the call failed. The error message can be retrieved using the 
uhppoted_exception::what() method.
```

### `set_pc_control`
```
int set_pc_control(uint32_t id, bool enabled);

id      controller serial number 
enabled enables/disables PC control

Returns:
- 0  if the call succeeded. 
- -1 if the call failed. The error message can be retrieved using errmsg().
```

### `set_interlock`
```
int set_interlock(uint32_t id, uint8_t interlock);

id        controller serial number 
interlock controller door interlock mode
          0: no interlock
          1: doors 1&2
          2: doors 3&4
          3: doors 1&2,3&4
          4: doors 1&2&3
          8: doors 1&2&3&4

Returns:
- 0  if the call succeeded. 
- -1 if the call failed. The error message can be retrieved using errmsg().
```

### `activate-keypads`
```
int activate_keypads(uint32_t id, bool reader1, bool reader2, bool reader3, bool reader4);

id      controller serial number 
reader1 activate/deactivate reader 1 access keypad
reader2 activate/deactivate reader 2 access keypad
reader3 activate/deactivate reader 3 access keypad
reader4 activate/deactivate reader 4 access keypad

Returns:
- 0  if the call succeeded. 
- -1 if the call failed. The error message can be retrieved using errmsg().
```


### `set-super-passwords`
```
int set_super_passwords(uint32_t id, uint8_t door, uint32_t password1, uint32_t password2, uint32_t password3, uint32_t password4);

id        controller serial number 
door      door ID [1..4]
password1 passcode in the range [1..999999] or 0 (for none)
password2 passcode in the range [1..999999] or 0 (for none)
password3 passcode in the range [1..999999] or 0 (for none)
password4 passcode in the range [1..999999] or 0 (for none)

Returns:
- 0  if the call succeeded. 
- -1 if the call failed. The error message can be retrieved using errmsg().
```


## Types

### `controller`
Container struct for use with the `uhppoted` _constructor_.
```
typedef struct controller {
    uint32_t id;
    std::string address;
} controller;

```

### `device`
Container struct for the controller information retrieved by `get_device`
```
typedef struct device {
    uint32_t ID;
    std::string address;
    std::string subnet;
    std::string gateway;
    std::string MAC;
    std::string version;
    std::string date;
} device;
```

### `event`
Container struct for the event information retrieved by `get_event`.
```
typedef struct event {
    std::string timestamp;
    uint32_t index;
    uint8_t eventType;
    bool granted;
    uint8_t door;
    uint8_t direction;
    uint32_t card;
    uint8_t reason;
} event;
```

### `status`
Container struct for the controller status information retrieved by `get_status`
```
typedef struct status {
    uint32_t ID;
    std::string sysdatetime;
    bool doors[4];
    bool buttons[4];
    uint8_t relays;
    uint8_t inputs;
    uint8_t syserror;
    uint8_t info;
    uint32_t seqno;
    event evt;
} status;
```

### `door_control`
Container struct for door configuration for `get_door_control` and `set_door_control`.
```
typedef struct door_control {
    uint8_t mode;
    uint8_t delay;
} door_control;
```

### `card`
Container struct for card information retrieved by `get_card` and `get_card_by_index`.
```
typedef struct card {
    uint32_t card_number;
    std::string from;
    std::string to;
    uint8_t doors[4];
    uint32_t PIN;
} card;
```

### `time_profile`
Container struct for time profile information retrieved/set by `get_time_profile` and 
`set_time_profile`.
```
typedef struct time_profile {
    uint8_t ID;
    uint8_t linked;
    std::string from;
    std::string to;
    bool monday;
    bool tuesday;
    bool wednesday;
    bool thursday;
    bool friday;
    bool saturday;
    bool sunday;
    std::string segment1start;
    std::string segment1end;
    std::string segment2start;
    std::string segment2end;
    std::string segment3start;
    std::string segment3end;
} time_profile;
```

### task
Container struct for the task information required for `add_task`.
```
typedef struct task {
    uint8_t task;
    uint8_t door;
    std::string from;
    std::string to;
    bool monday;
    bool tuesday;
    bool wednesday;
    bool thursday;
    bool friday;
    bool saturday;
    bool sunday;
    std::string at;
    uint8_t cards;
} task;
```
