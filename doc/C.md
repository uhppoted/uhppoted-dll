# C: Usage and Notes

The 'C' bindings comprise a single header ([uhppoted.h](https://github.com/uhppoted/uhppoted-dll/blob/master/bindings/c/include/uhppoted.h)) and  source file [uhppoted.c](https://github.com/uhppoted/uhppoted-dll/blob/master/bindings/c/src/uhppoted.c) - copy these to wherever makes it sense in your project. 

The generated DLL (Windows), shared lib (Linux) or dylib (MacOS) should be placed in either the library search path for 
the platform:
- LD_LIBRARY (Linux)
- DYLIB_LIBRARY (MacOS)
- Windows - see [Dynamic-Link Library Search Order](https://docs.microsoft.com/en-us/windows/win32/dlls/dynamic-link-library-search-order)

or in the same folder as the executable.

Examples illustrating the use of the 'C' API can be found in the [examples/c](https://github.com/uhppoted/uhppoted-dll/tree/master/examples/c) folder.

## API

Internally, the API functions use a `UHPPOTE` struct that contains the information required to access a controller. The
`UHPPOTE` struct is initialised by invoking `setup` and cleaned up by invoking `teardown` e.g.:

```
    setup("0.0.0.0:0", "255.255.255.255", "0.0.0.0:60001", 2500, true, &alpha, &beta, NULL);
    ...
    ...
    teardown();
```

The API functions all return an 0 _int_ result code if the function call was successful. A non-zero code indicates
a failure and the associated text error message can be retrieved with `errmsg`.

### `setup`

Initialises the internal _UHPPOTE_ _struct_ that contains the information used to access the controllers.

```
void setup(const char *bind, const char *broadcast, const char *listen, int timeout, int debug, ...);

bind       IP address:port to which to bind the UDP socket. Defaults to 0.0.0.0:0
broadcast  IP address:port for broadcast UDP packets. Defaults to 255.255.255.255:60000
listen     IP address:port for events from controller (unused). Defaults to 0.0.0.0:60001
timeout    milliseconds to wait for a reply. Defaults to 5 seconds.
debug      Displays the DLL and controller requests/responses if true.
(varargs)  List of specific controllers and their associated IP addresses (e.g.for controllers
           located on a different interface, a VLAN or a VPN)
```

### `teardown`
Frees any dynamically allocated memory associated with:
- the internal _UHPPOTE_ _struct_ created by `setup`
- the global error message

```
void teardown();
```

### `errmsg`
Returns the text description associated with an error if an API function returns a non-zero result. The memory
allocation is managed internally and released by `teardown`.

```
const char *errmsg();
```

### `lookup`
Utility function to provide text equivalents to the following uint8_t codes:
- door control mode
- event type
- event reason
- event direction

```
const char *lookup(const char *category, uint8_t code, const char *locale);

category  Lookup table to use ("door.mode", "event.direction", "event.type" or "event.reason")
code      code from the returned information
locale    Language code to use for the text lookup. Defaults to uk-en.
```

### `get_devices`
```
int get_devices(uint32_t **devices, int *N);

devices  Address of controller array
N        size of devices array

Returns:
- 0 if the call succeeded. The devices array will contain a list of N controller serial numbers. The devices
    array is reallocated internally and the caller is responsible for freeing the associated memory.
- -1 if the call failed. The error message can be retrieved using errmsg().
```

### `get_device`
```
int get_device(uint32_t id, struct device *controller);

id          controller serial number 
controller  Preallocated device struct

Returns:
- 0  if the call succeeded. The device struct will be populated with the device information.
- -1 if the call failed. The error message can be retrieved using errmsg().

```

### `set_address`
```
int set_address(uint32_t id, const char *address, const char *subnet, const char *gateway);

id       controller serial number 
address  controller IPv4 address
subnet   controller IPv4 subnet mask
gateway  controller gateway IPv4 address

Returns:
- 0  if the call succeeded.
- -1 if the call failed. The error message can be retrieved using errmsg().
```

### `get_status`
```
int get_status(uint32_t id, struct status *status);

id      controller serial number 
status  Preallocated status struct

Returns:
- 0  if the call succeeded. The status struct will be populated with the controller status information.
- -1 if the call failed. The error message can be retrieved using errmsg().
```

### `get_time`
```
int get_time(uint32_t id, char **time);

id    controller serial number 
time  pointer for a time string

Returns:
- 0  if the call succeeded. The time variable will be initialised to a string containing the controller
     date/time. The string is allocated internally and the caller is responsible for freeing the associated 
     memory.
- -1 if the call failed. The error message can be retrieved using errmsg().
```

### `set_time`
```
int set_time(uint32_t id, char *time);

id    controller serial number 
time  date/time string (YYYY-MM-dd HH:mm:ss)

Returns:
- 0  if the call succeeded. 
- -1 if the call failed. The error message can be retrieved using errmsg().
```

### `get_listener`
```
int get_listener(uint32_t id, char **time);

id        controller serial number 
listener  pointer for a listener address string

Returns:
- 0  if the call succeeded. The listener variable will be initialised to a string containing the controller
     listener IPv4 address. The string is allocated internally and the caller is responsible for freeing the
     associated memory.
- -1 if the call failed. The error message can be retrieved using errmsg().
```

### `set_listener`
```
int set_listener(uint32_t id, char *time);

id        controller serial number 
listener  listener IPv4 address:port

Returns:
- 0  if the call succeeded. 
- -1 if the call failed. The error message can be retrieved using errmsg().
```

### `get_door_control`
```
int get_door_control(uint32_t id, uint8_t door, struct door_control *control);

id       controller serial number 
door     door ID [1..4]
control  Preallocated door_control struct

Returns:
- 0  if the call succeeded. The control struct will be populated with the controller door configuration.
- -1 if the call failed. The error message can be retrieved using errmsg().
```

### `set_door_control`
```
int set_door_control(uint32_t id, uint8_t door, uint8_t mode, uint8_t delay);

id       controller serial number 
door     door ID [1..4]
control  door configuration

Returns:
- 0  if the call succeeded. 
- -1 if the call failed. The error message can be retrieved using errmsg().
```

### `open_door`
```
int open_door(uint32_t id, uint8_t door);

id       controller serial number 
door     door ID [1..4]

Returns:
- 0  if the call succeeded. 
- -1 if the call failed. The error message can be retrieved using errmsg().
```

### `get_cards`
```
int get_cards(uint32_t id, int *N);

id  controller serial number 
N   variable for number of cards stored on the controller

Returns:
- 0  if the call succeeded. N will be set to the number of cards stored on the controller.
- -1 if the call failed. The error message can be retrieved using errmsg().
```

### `get_card`
```
int get_card(uint32_t id, uint32_t card_number, card *card);

id           controller serial number 
card_number  card number
card         Preallocated card struct

Returns:
- 0  if the call succeeded. The card struct will be populated with the card information from the controller.
- -1 if the call failed. The error message can be retrieved using errmsg().
```

### `get_card_by_index`
```
int get_card_by_index(uint32_t id, uint32_t index, card *card);

id     controller serial number 
index  index of card to retrieve
card   Preallocated card struct

Returns:
- 0  if the call succeeded. The card struct will be populated with the card information from the controller.
- -1 if the call failed. The error message can be retrieved using errmsg().
```

### `put_card`
```
int put_card(uint32_t id, uint32_t card_number, const char *from, const char *to, const uint8_t doors[4]);

id           controller serial number 
card_number  card number
from         card valid from date, inclusive (YYYY-MM-dd)
to           card valid until, inclusive (YYYY-MM-dd)
doors        4 byte array with card permissions

Returns:
- 0  if the call succeeded. 
- -1 if the call failed. The error message can be retrieved using errmsg().
```

### `delete_card`
```
int delete_card(uint32_t id, uint32_t card_number);

id           controller serial number 
card_number  card number

Returns:
- 0  if the call succeeded. 
- -1 if the call failed. The error message can be retrieved using errmsg().
```

### `delete_cards`
```
int delete_cards(uint32_t id);

id  controller serial number 

Returns:
- 0  if the call succeeded. 
- -1 if the call failed. The error message can be retrieved using errmsg().
```

### `get_event_index`
```
int get_event_index(uint32_t id, uint32_t *index);

id     controller serial number 
index  variable for controller event index

Returns:
- 0  if the call succeeded. index will be set to the controller event index.
- -1 if the call failed. The error message can be retrieved using errmsg().
```

### `set_event_index`
```
int set_event_index(uint32_t id, uint32_t index);

id     controller serial number 
index  controller event index

Returns:
- 0  if the call succeeded. 
- -1 if the call failed. The error message can be retrieved using errmsg().
```

### `get_event`
```
int get_event(uint32_t id, uint32_t index, event *event);

id     controller serial number 
index  index of event to retrieve
event  Preallocated event struct

Returns:
- 0  if the call succeeded. The event struct will be populated with the event information 
     from the controller.
- -1 if the call failed. The error message can be retrieved using errmsg().
```

### `record_special_events`
```
int record_special_events(uint32_t id, bool enabled);

id       controller serial number 
enabled  Enables/disables recording of door, etc events

Returns:
- 0  if the call succeeded.
- -1 if the call failed. The error message can be retrieved using errmsg().
```

### `get_time_profile`
```
int get_time_profile(uint32_t id, uint8_t profile_id, time_profile *profile);

id          controller serial number 
profile_id  ID [2..254] of time profile to retrieve
profile     Preallocated time_profile struct

Returns:
- 0  if the call succeeded. The profile struct will be populated with the time profile information 
     from the controller.
- -1 if the call failed. The error message can be retrieved using errmsg().
```

### `set_time_profile`
```
int set_time_profile(uint32_t id, time_profile *profile);

id       controller serial number 
profile  time_profile struct initialised with the time profile to store on the controller.

Returns:
- 0  if the call succeeded. 
- -1 if the call failed. The error message can be retrieved using errmsg().
```

### `clear_time_profiles`
```
int clear_time_profiles(uint32_t id);

id  controller serial number 

Returns:
- 0  if the call succeeded. 
- -1 if the call failed. The error message can be retrieved using errmsg().
```

### `add_task`
```
int add_task(uint32_t id, task *task);

id    controller serial number 
task  task struct initialised with the task to store on the controller.

Returns:
- 0  if the call succeeded. 
- -1 if the call failed. The error message can be retrieved using errmsg().
```

### `refresh_tasklist`
```
int refresh_tasklist(uint32_t id);

id  controller serial number 

Returns:
- 0  if the call succeeded. 
- -1 if the call failed. The error message can be retrieved using errmsg().
```

### `clear_tasklist`
```
int clear_tasklist(uint32_t id);

id  controller serial number 

Returns:
- 0  if the call succeeded. 
- -1 if the call failed. The error message can be retrieved using errmsg().
```

## Types

### `controller`
Container struct for use with the `setup` function.
```
typedef struct controller {
    uint32_t id;
    const char *address;
} controller;
```

### `device`
Container struct for the controller information retrieved by `get_device`
```
typedef struct device {
    uint32_t ID;
    char address[16];
    char subnet[16];
    char gateway[16];
    char MAC[18];
    char version[6];
    char date[11];
} device;
```

### `event`
Container struct for the event information retrieved by `get_event`.
```
typedef struct event {
    char timestamp[20];
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
    char sysdatetime[20];
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
    char from[11];
    char to[11];
    uint8_t doors[4];
} card;
```

### `time_profile`
Container struct for time profile information retrieved/set by `get_time_profile` and 
`set_time_profile`.
```
typedef struct time_profile {
    uint8_t ID;
    uint8_t linked;
    char from[11];
    char to[11];
    bool monday;
    bool tuesday;
    bool wednesday;
    bool thursday;
    bool friday;
    bool saturday;
    bool sunday;
    char segment1start[6];
    char segment1end[6];
    char segment2start[6];
    char segment2end[6];
    char segment3start[6];
    char segment3end[6];
} time_profile;
```

### task
Container struct for the task information required for `add_task`.
```
typedef struct task {
    uint8_t task;
    uint8_t door;
    char from[11];
    char to[11];
    bool monday;
    bool tuesday;
    bool wednesday;
    bool thursday;
    bool friday;
    bool saturday;
    bool sunday;
    char at[6];
    uint8_t cards;
} task;
```
