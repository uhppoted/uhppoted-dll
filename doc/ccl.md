# Clozure Common Lisp: Usage and Notes

The CCL bindings comprise two files:

- [uhppoted.lisp](https://github.com/uhppoted/uhppoted-dll/blob/master/bindings/ccl/uhppoted.lisp)
- [packages.lisp](https://github.com/uhppoted/uhppoted-dll/blob/master/bindings/ccl/packages.lisp)
 
Copy these to wherever makes it sense in your project. 

The generated DLL (Windows), shared lib (Linux) or dylib (MacOS) should be placed in either the library search path for the platform:
- LD_LIBRARY (Linux)
- DYLIB_LIBRARY (MacOS)
- Windows - see [Dynamic-Link Library Search Order](https://docs.microsoft.com/en-us/windows/win32/dlls/dynamic-link-library-search-order)

or in the same folder as the executable.

Examples illustrating the use of the CCL API can be found in the [examples/ccl](https://github.com/uhppoted/uhppoted-dll/tree/master/examples/ccl) folder.

## API

API functions are invoked indirectly via the `uhppoted` function which (optionally) takes the additional
information required to access a controller, e.g.:

```
(uhppoted f 
          :bind-addr      "0.0.0.0"
          :broadcast-addr "255.255.255.255"
          :listen-addr    "0.0.0.0:60001"
          :timeout        2500
          :controllers    (list '(405419896 "192.168.1.100") '(303986753 "192.168.1.100"))
          :debug          T)))

bind-addr        IPv4 address:port to which to bind the UDP socket. Defaults to 0.0.0.0:0
broadcast-addr   IPv4 address:port for broadcast UDP packets. Defaults to 255.255.255.255:60000
listen-addr      IPv4 address:port for events from controller (unused). Defaults to 0.0.0.0:60001
timeout          milliseconds to wait for a reply. Defaults to 5 seconds.
controllers      Optional list of specific controllers and their associated IPv4 addresses (e.g.for controllers
                 located on a different interface, a VLAN or a VPN)
debug            Displays the DLL and controller requests/responses if true.
```

All API functions raise a `uhppoted-error` _condition_ if the call fails for any reason whatsoever:
```
(define-condition uhppoted-error (error)
  ((message :initarg :message :reader message)))
```

Restart cases are defined for `ignore`, `use-value` and `with-warning`:
...
          (restart-case (funcall f uhppote)
                  (ignore       ()      nil)
                    (use-value    (value) value)
                    (with-warning (err)   (warn err)))

```

For example:
```
  (handler-bind
   ((uhppoted-error
     #'(lambda (c) 
        (format t "~%*** ERROR: ~a~%~%" (uhppoted:message c))
        (invoke-restart 'ignore))))
  (uhppoted f 
            :bind-addr      "0.0.0.0"
            ...
```

### `lookup`

The `lookup` function provides text equivalents to the following byte codes:
- door control mode
- event type
- event reason
- event direction

```
(defun uhppoted-lookup (category code locale) ...)

category  Lookup table to use ("door.mode", "event.direction", "event.type" or "event.reason")
code      code from the returned information
locale    Language code to use for the text lookup. Defaults to uk-en.
```

### `get-devices`
```
(defun uhppoted-get-devices (uhppote &optional (N 16)) ...)

Returns an array of controller serial numbers if the call succeeded. 

Raises a `uhppoted-error` condition if the call failed.
```

### `get-device`
```
(defun uhppoted-get-device (uhppote device-id) ...)

device-id  controller serial number 

Returns a `device` struct populated with the controller device information if the call succeeded.

Raises a `uhppoted-error` condition if the call failed.
```

### `set-address`
```
(defun uhppoted-set-address (uhppote device-id ip-addr subnet-mask gateway-addr) ...)

device-id     controller serial number 
ip-addr       controller IPv4 address
subnet-mask   controller IPv4 subnet mask
gateway-addr  controller gateway IPv4 address

Raises a `uhppoted-error` condition if the call failed.
```

### `get-status`
```
(defun uhppoted-get-status (uhppote device-id) ...)

device-id  controller serial number 

Returns a `status` struct populated with the controller status information if the call succeeded.

Raises a `uhppoted-error` condition if the call failed.
```

### `get-time`
```
(defun uhppoted-get-time (uhppote device-id) ...)

device-id  controller serial number 

Returns a date/time string (YYYY-MM-dd HH:mm:ss) with the controller current date/time if the call succeeded.

Raises a `uhppoted-error` condition if the call failed.
```

### `set-time`
```
(defun uhppoted-set-time (uhppote device-id datetime) ...)

device-id  controller serial number 
datetime   date/time string (YYYY-MM-dd HH:mm:ss)

Raises a `uhppoted-error` condition if the call failed.
```

### `get-listener`
```
(defun uhppoted-get-listener (uhppote device-id) ...)

device-id  controller serial number 

Returns the controller event listener IPv4 address:port as a string if the call succeeded.

Raises a `uhppoted-error` condition if the call failed.
```

### `set-listener`
```
(defun uhppoted-set-listener (uhppote device-id listener) ...)

device-id  controller serial number 
listener   listener IPv4 address:port string

Raises a `uhppoted-error` condition if the call failed.
```

### `get-door-control`
```
(defun uhppoted-get-door-control (uhppote device-id door) ...)

device-id  controller serial number 
door       door ID [1..4]

Returns a `door-control` struct populated with the controller door configuration if the call succeeded.

Raises a `uhppoted-error` condition if the call failed.
```

### `set-door-control`
```
(defun uhppoted-set-door-control (uhppote device-id door mode delay) ...)

device-id  controller serial number 
door       door ID [1..4]
mode       normally open (1), normally closed (2) or controlled (3)
delay      door open delay in seconds

Raises a `uhppoted-error` condition if the call failed.
```

### `open-door`
```
(defun uhppoted-open-door (uhppote device-id door) ...)

device-id  controller serial number 
door       door ID [1..4]

Raises a `uhppoted-error` condition if the call failed.
```

### `get-cards`
```
(defun uhppoted-get-cards (uhppote device-id) ...)

device-id  controller serial number 

Returns the number of cards stored on the controller if the call succeeded.

Raises a `uhppoted-error` condition if the call failed.
```

### `get-card`
```
(defun uhppoted-get-card (uhppote device-id card-number) ...)

device-id    controller serial number 
card-number  card number

Returns a `card` struct with the controller card information if the call succeeded.

Raises a `uhppoted-error` condition if the call failed.
```

### `get-card-by-index`
```
(defun uhppoted-get-card-by-index (uhppote device-id index) ...)

device-id  controller serial number 
index      index of card to retrieve

Returns a `card` struct with the controller card information if the call succeeded.

Raises a `uhppoted-error` condition if the call failed.
```

### `put-card`
```
(defun uhppoted-put-card (uhppote device-id card-number from to doors) ...)

device-id    controller serial number 
card-number  card number
from         card valid from date, inclusive (YYYY-MM-dd)
to           card valid until, inclusive (YYYY-MM-dd)
doors        4 byte array with card permissions

Raises a `uhppoted-error` condition if the call failed.
```

### `delete-card`
```
(defun uhppoted-delete-card (uhppote device-id card-number) ...)

device-id    controller serial number 
card-number  card number

Raises a `uhppoted-error` condition if the call failed.
```

### `delete-cards`
```
(defun uhppoted-delete-cards (uhppote device-id) ...)

device-id  controller serial number 

Raises a `uhppoted-error` condition if the call failed.
```

### `get-event-index`
```
(defun uhppoted-get-event-index (uhppote device-id) ...)

device-id  controller serial number 

Returns the controller event index if the call succeeded.

Raises a `uhppoted-error` condition if the call failed.
```

### `set-event-index`
```
(defun uhppoted-record-special-events (uhppote device-id enabled) ...)

device-id  controller serial number 
index      controller event index

Raises a `uhppoted-error` condition if the call failed.
```

### `get-event`
```
(defun uhppoted-get-event (uhppote device-id index) ...)

device-id  controller serial number 
index      index of event to retrieve

Returns an `event` struct with the controller event stored at the index.

Raises a `uhppoted-error` condition if the call failed.
```

### `record-special-events`
```
(defun uhppoted-record-special-events (uhppote device-id enabled) ...)

device-id controller serial number 
enabled   Enables/disables recording of door, etc events

Raises a `uhppoted-error` condition if the call failed.
```

### `get-time-profile`
```
(defun uhppoted-get-time-profile (uhppote device-id profile-id) ...)

device-id   controller serial number 
profile-id  ID [2..254] of time profile to retrieve

Returns a `time-profile` struct with the time profile stored at the profile ID on the controller.

Raises a `uhppoted-error` condition if the call failed.
```

### `set-time-profile`
```
(defun uhppoted-set-time-profile (uhppote device-id profile) ...)

device-id controller serial number 
profile   `time-profile` struct  initialised with the time profile to store on the controller.

Raises a `uhppoted-error` condition if the call failed.
```

### `clear-time-profiles`
```
(defun uhppoted-clear-time-profiles (uhppote device-id) ...)

device-id  controller serial number 

Raises a `uhppoted-error` condition if the call failed.
```

### `add-task`
```
(defun uhppoted-add-task (uhppote device-id task) ...)

device-id controller serial number 
task      `task` struct initialised with the task to store on the controller.

Raises a `uhppoted-error` condition if the call failed.
```

### `refresh-tasklist`
```
(defun uhppoted-refresh-tasklist (uhppote device-id) ...)

device-id  controller serial number 

Raises a `uhppoted-error` condition if the call failed.
```

### `clear-tasklist`
```
(defun uhppoted-clear-tasklist (uhppote device-id) ...)

device-id  controller serial number 

Raises a `uhppoted-error` condition if the call failed.
```

### `set-pc-control`
```
(defun uhppoted-set-pc-control (uhppote device-id enabled) ...)

device-id  controller serial number 
enabled    enables/disables host control

Raises a `uhppoted-error` condition if the call failed.
```

### `set-interlock`
```
(defun uhppoted-set-interlock (uhppote device-id interlock) ...)

device-id  controller serial number 
interlock  controller door interlock mode
           0: no interlock
           1: doors 1&2
           2: doors 3&4
           3: doors 1&2,3&4
           4: doors 1&2&3
           8: doors 1&2&3&4

Raises a `uhppoted-error` condition if the call failed.
```

### `activate-keypads`
```
(defun uhppoted-activate-keypads (uhppote device-id reader1 reader2 reader3 reader4) ...)

device-id  controller serial number 
reader1    activates/deactivates reader 1 access keypad
reader2    activates/deactivates reader 2 access keypad
reader3    activates/deactivates reader 3 access keypad
reader4    activates/deactivates reader 4 access keypad

Raises a `uhppoted-error` condition if the call failed.
```

## Types

### `Device`
Container class for the controller information retrieved by `get_device`
```
(defstruct device ID 
                  address
                  subnet
                  gateway
                  MAC
                  version
                  date)

```

### `Event`
Container class for the event information retrieved by `get_event`.
```
(defstruct event timestamp
                 index
                 event-type
                 granted
                 door
                 direction
                 card
                 reason)
```

### `Status`
Container class for the controller status information retrieved by `get_status`
```
(defstruct status ID 
                  timestamp
                  doors
                  buttons
                  relays
                  inputs
                  syserror
                  info
                  seqno
                  event)
```

### `DoorControl`
Container class for door configuration for `get_door_control` and `set_door_control`.
```
(defstruct door-control mode
                        delay)
```

### `Card`
Container class for card information retrieved by `get_card` and `get_card_by_index`.
```
(defstruct card card-number 
                from
                to
                doors)
```

### `TimeProfile`
Container class for time profile information retrieved/set by `get_time_profile` and 
`set_time_profile`.
```
(defstruct time-profile ID
                        linked
                        from
                        to
                        monday
                        tuesday
                        wednesday
                        thursday
                        friday
                        saturday
                        sunday
                        segment1start
                        segment1end
                        segment2start
                        segment2end
                        segment3start
                        segment3end)
```

### task
Container class for the task information required for `add_task`.
```
(defstruct task task
                door
                from
                to
                monday
                tuesday
                wednesday
                thursday
                friday
                saturday
                sunday
                at
                cards)
```
