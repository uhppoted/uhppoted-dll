package main

/*
#include <stdbool.h>
#include <stdint.h>

typedef struct udevice {
	uint32_t    id;
	const char *address;
} udevice;

typedef struct udevices {
	uint32_t  N;        // number of devicess
	udevice  *devices;  // array non-local devices
} udevices;

typedef struct UHPPOTE {
	const char *bind;
	const char *broadcast;
	const char *listen;
	int         timeout;  // milliseconds
	udevices   *devices;  // (optional) list of non-local devices
	bool        debug;
} UHPPOTE;

typedef struct Device {
    uint32_t ID;
	char *address;
	char *subnet;
	char *gateway;
	char *MAC;
	char *version;
	char *date;
} Device;

typedef struct Event {
	char  *timestamp;
    uint32_t index;
	uint8_t eventType;
	uint8_t granted;
	uint8_t door;
	uint8_t direction;
	uint32_t card;
	uint8_t reason;
} Event;

typedef struct Status {
    uint32_t ID;
	char *sysdatetime;
	uint8_t  *doors;   // uint_8[4]
	uint8_t  *buttons; // uint_8[4]
	uint8_t relays;
	uint8_t inputs;
	uint8_t syserror;
	uint8_t info;
	uint32_t seqno;
	Event *event;
} Status;

typedef struct DoorControl {
    uint8_t mode;
    uint8_t delay;
} DoorControl;

typedef struct Card {
    uint32_t card_number;
    char* from;
    char* to;
	uint8_t *doors; // uint_8[4]
    uint32_t PIN;
} Card;

typedef struct TimeProfile {
    uint8_t ID;
    uint8_t linked;
    char *from;
    char *to;
    uint8_t monday;
    uint8_t tuesday;
    uint8_t wednesday;
    uint8_t thursday;
    uint8_t friday;
    uint8_t saturday;
    uint8_t sunday;
    char * segment1start;
    char * segment1end;
    char * segment2start;
    char * segment2end;
    char * segment3start;
    char * segment3end;
} TimeProfile;

typedef struct Task {
	uint8_t task;
	uint8_t door;
	const char *from;
	const char *to;
    uint8_t monday;
    uint8_t tuesday;
    uint8_t wednesday;
    uint8_t thursday;
    uint8_t friday;
    uint8_t saturday;
    uint8_t sunday;
	const char *at;
	uint8_t cards;
} Task;

*/
import "C"

import (
	"fmt"
	"net"
	"time"
	"unsafe"

	"github.com/uhppoted/uhppote-core/types"
	"github.com/uhppoted/uhppote-core/uhppote"
)

var DEBUG bool

func main() {}

//export GetDevices
func GetDevices(u *C.struct_UHPPOTE, N *C.int, list *C.uint) *C.char {
	if uu, err := makeUHPPOTE(u); err != nil {
		return C.CString(err.Error())
	} else if err := getDevices(uu, N, list); err != nil {
		return C.CString(err.Error())
	}

	return nil
}

//export GetDevice
func GetDevice(u *C.struct_UHPPOTE, device *C.struct_Device, deviceID uint32) *C.char {
	if uu, err := makeUHPPOTE(u); err != nil {
		return C.CString(err.Error())
	} else if err := getDevice(uu, device, deviceID); err != nil {
		return C.CString(err.Error())
	}

	return nil
}

//export SetAddress
func SetAddress(u *C.struct_UHPPOTE, deviceID uint32, addr, subnet, gateway *C.char) *C.char {
	if uu, err := makeUHPPOTE(u); err != nil {
		return C.CString(err.Error())
	} else if err := setAddress(uu, deviceID, addr, subnet, gateway); err != nil {
		return C.CString(err.Error())
	}

	return nil
}

//export GetStatus
func GetStatus(u *C.struct_UHPPOTE, status *C.struct_Status, deviceID uint32) *C.char {
	if uu, err := makeUHPPOTE(u); err != nil {
		return C.CString(err.Error())
	} else if err := getStatus(uu, status, deviceID); err != nil {
		return C.CString(err.Error())
	}

	return nil
}

//export GetTime
func GetTime(u *C.struct_UHPPOTE, datetime **C.char, deviceID uint32) *C.char {
	if uu, err := makeUHPPOTE(u); err != nil {
		return C.CString(err.Error())
	} else if err := getTime(uu, datetime, deviceID); err != nil {
		return C.CString(err.Error())
	}

	return nil
}

//export SetTime
func SetTime(u *C.struct_UHPPOTE, deviceID uint32, datetime *C.char) *C.char {
	if uu, err := makeUHPPOTE(u); err != nil {
		return C.CString(err.Error())
	} else if err := setTime(uu, deviceID, datetime); err != nil {
		return C.CString(err.Error())
	}

	return nil
}

//export GetListener
func GetListener(u *C.struct_UHPPOTE, address **C.char, deviceID uint32) *C.char {
	if uu, err := makeUHPPOTE(u); err != nil {
		return C.CString(err.Error())
	} else if err := getListener(uu, address, deviceID); err != nil {
		return C.CString(err.Error())
	}

	return nil
}

//export SetListener
func SetListener(u *C.struct_UHPPOTE, deviceID uint32, listener *C.char) *C.char {
	if uu, err := makeUHPPOTE(u); err != nil {
		return C.CString(err.Error())
	} else if err := setListener(uu, deviceID, listener); err != nil {
		return C.CString(err.Error())
	}

	return nil
}

//export GetDoorControl
func GetDoorControl(u *C.struct_UHPPOTE, control *C.struct_DoorControl, deviceID uint32, door uint8) *C.char {
	if uu, err := makeUHPPOTE(u); err != nil {
		return C.CString(err.Error())
	} else if err := getDoorControl(uu, control, deviceID, door); err != nil {
		return C.CString(err.Error())
	}

	return nil
}

//export SetDoorControl
func SetDoorControl(u *C.struct_UHPPOTE, deviceID uint32, door uint8, mode uint8, delay uint8) *C.char {
	if uu, err := makeUHPPOTE(u); err != nil {
		return C.CString(err.Error())
	} else if err := setDoorControl(uu, deviceID, door, types.ControlState(mode), delay); err != nil {
		return C.CString(err.Error())
	}

	return nil
}

//export OpenDoor
func OpenDoor(u *C.struct_UHPPOTE, deviceID uint32, door uint8) *C.char {
	if uu, err := makeUHPPOTE(u); err != nil {
		return C.CString(err.Error())
	} else if err := openDoor(uu, deviceID, door); err != nil {
		return C.CString(err.Error())
	}

	return nil
}

//export GetCards
func GetCards(u *C.struct_UHPPOTE, N *C.int, deviceID uint32) *C.char {
	if uu, err := makeUHPPOTE(u); err != nil {
		return C.CString(err.Error())
	} else if err := getCards(uu, N, deviceID); err != nil {
		return C.CString(err.Error())
	}

	return nil
}

//export GetCard
func GetCard(u *C.struct_UHPPOTE, card *C.struct_Card, deviceID uint32, cardNumber uint32) *C.char {
	if uu, err := makeUHPPOTE(u); err != nil {
		return C.CString(err.Error())
	} else if err := getCard(uu, card, deviceID, cardNumber); err != nil {
		return C.CString(err.Error())
	}

	return nil
}

//export GetCardByIndex
func GetCardByIndex(u *C.struct_UHPPOTE, card *C.struct_Card, deviceID uint32, index uint32) *C.char {
	if uu, err := makeUHPPOTE(u); err != nil {
		return C.CString(err.Error())
	} else if err := getCardByIndex(uu, card, deviceID, index); err != nil {
		return C.CString(err.Error())
	}

	return nil
}

//export PutCard
func PutCard(u *C.struct_UHPPOTE, deviceID uint32, cardNumber uint32, from, to *C.char, doors *uint8, PIN uint32) *C.char {
	if uu, err := makeUHPPOTE(u); err != nil {
		return C.CString(err.Error())
	} else if err := putCard(uu, deviceID, cardNumber, from, to, doors, PIN); err != nil {
		return C.CString(err.Error())
	}

	return nil
}

//export DeleteCard
func DeleteCard(u *C.struct_UHPPOTE, deviceID uint32, cardNumber uint32) *C.char {
	if uu, err := makeUHPPOTE(u); err != nil {
		return C.CString(err.Error())
	} else if err := deleteCard(uu, deviceID, cardNumber); err != nil {
		return C.CString(err.Error())
	}

	return nil
}

//export DeleteCards
func DeleteCards(u *C.struct_UHPPOTE, deviceID uint32) *C.char {
	if uu, err := makeUHPPOTE(u); err != nil {
		return C.CString(err.Error())
	} else if err := deleteCards(uu, deviceID); err != nil {
		return C.CString(err.Error())
	}

	return nil
}

//export GetEventIndex
func GetEventIndex(u *C.struct_UHPPOTE, index *uint32, deviceID uint32) *C.char {
	if uu, err := makeUHPPOTE(u); err != nil {
		return C.CString(err.Error())
	} else if err := getEventIndex(uu, index, deviceID); err != nil {
		return C.CString(err.Error())
	}

	return nil
}

//export SetEventIndex
func SetEventIndex(u *C.struct_UHPPOTE, deviceID uint32, index uint32) *C.char {
	if uu, err := makeUHPPOTE(u); err != nil {
		return C.CString(err.Error())
	} else if err := setEventIndex(uu, deviceID, index); err != nil {
		return C.CString(err.Error())
	}

	return nil
}

//export GetEvent
func GetEvent(u *C.struct_UHPPOTE, event *C.struct_Event, deviceID uint32, index uint32) *C.char {
	if uu, err := makeUHPPOTE(u); err != nil {
		return C.CString(err.Error())
	} else if err := getEvent(uu, event, deviceID, index); err != nil {
		return C.CString(err.Error())
	}

	return nil
}

//export RecordSpecialEvents
func RecordSpecialEvents(u *C.struct_UHPPOTE, deviceID uint32, enabled bool) *C.char {
	if uu, err := makeUHPPOTE(u); err != nil {
		return C.CString(err.Error())
	} else if err := recordSpecialEvents(uu, deviceID, enabled); err != nil {
		return C.CString(err.Error())
	}

	return nil
}

//export GetTimeProfile
func GetTimeProfile(u *C.struct_UHPPOTE, profile *C.struct_TimeProfile, deviceID uint32, profileID uint8) *C.char {
	if uu, err := makeUHPPOTE(u); err != nil {
		return C.CString(err.Error())
	} else if err := getTimeProfile(uu, profile, deviceID, profileID); err != nil {
		return C.CString(err.Error())
	}

	return nil
}

//export SetTimeProfile
func SetTimeProfile(u *C.struct_UHPPOTE, deviceID uint32, profile *C.struct_TimeProfile) *C.char {
	if uu, err := makeUHPPOTE(u); err != nil {
		return C.CString(err.Error())
	} else if err := setTimeProfile(uu, deviceID, profile); err != nil {
		return C.CString(err.Error())
	}

	return nil
}

//export ClearTimeProfiles
func ClearTimeProfiles(u *C.struct_UHPPOTE, deviceID uint32) *C.char {
	if uu, err := makeUHPPOTE(u); err != nil {
		return C.CString(err.Error())
	} else if err := clearTimeProfiles(uu, deviceID); err != nil {
		return C.CString(err.Error())
	}

	return nil
}

//export AddTask
func AddTask(u *C.struct_UHPPOTE, deviceID uint32, task *C.struct_Task) *C.char {
	if uu, err := makeUHPPOTE(u); err != nil {
		return C.CString(err.Error())
	} else if err := addTask(uu, deviceID, task); err != nil {
		return C.CString(err.Error())
	}

	return nil
}

//export RefreshTaskList
func RefreshTaskList(u *C.struct_UHPPOTE, deviceID uint32) *C.char {
	uu, err := makeUHPPOTE(u)
	if err != nil {
		return C.CString(err.Error())
	}

	if err := refreshTaskList(uu, deviceID); err != nil {
		return C.CString(err.Error())
	}

	return nil
}

//export ClearTaskList
func ClearTaskList(u *C.struct_UHPPOTE, deviceID uint32) *C.char {
	uu, err := makeUHPPOTE(u)
	if err != nil {
		return C.CString(err.Error())
	}

	if err := clearTaskList(uu, deviceID); err != nil {
		return C.CString(err.Error())
	}

	return nil
}

//export SetPCControl
func SetPCControl(u *C.struct_UHPPOTE, controller uint32, enabled bool) *C.char {
	if uu, err := makeUHPPOTE(u); err != nil {
		return C.CString(err.Error())
	} else if err := setPCControl(uu, controller, enabled); err != nil {
		return C.CString(err.Error())
	}

	return nil
}

//export SetInterlock
func SetInterlock(u *C.struct_UHPPOTE, controller uint32, interlock uint8) *C.char {
	if uu, err := makeUHPPOTE(u); err != nil {
		return C.CString(err.Error())
	} else if err := setInterlock(uu, controller, interlock); err != nil {
		return C.CString(err.Error())
	}

	return nil
}

//export ActivateKeypads
func ActivateKeypads(u *C.struct_UHPPOTE, controller uint32, reader1, reader2, reader3, reader4 bool) *C.char {
	if uu, err := makeUHPPOTE(u); err != nil {
		return C.CString(err.Error())
	} else if err := activateKeypads(uu, controller, reader1, reader2, reader3, reader4); err != nil {
		return C.CString(err.Error())
	}

	return nil
}

// Sets the supervisor passcodes for a door managed by the controller.
//
// Valid passcodes are in the range [1..999999] or 0 (no code) - invalid passcodes will be replaced by
// a 0 (no code).
//
//export SetDoorPasscodes
func SetDoorPasscodes(u *C.struct_UHPPOTE, controller uint32, door uint8, passcode1, passcode2, passcode3, passcode4 uint32) *C.char {
	if uu, err := makeUHPPOTE(u); err != nil {
		return C.CString(err.Error())
	} else if err := setDoorPasscodes(uu, controller, door, passcode1, passcode2, passcode3, passcode4); err != nil {
		return C.CString(err.Error())
	}

	return nil
}

// Resets a controller to the manufacturer default configuration.
//
//export RestoreDefaultParameters
func RestoreDefaultParameters(u *C.struct_UHPPOTE, controller uint32) *C.char {
	if uu, err := makeUHPPOTE(u); err != nil {
		return C.CString(err.Error())
	} else if err := restoreDefaultParameters(uu, controller); err != nil {
		return C.CString(err.Error())
	}

	return nil
}

func makeUHPPOTE(u *C.struct_UHPPOTE) (uhppote.IUHPPOTE, error) {
	bind := types.BindAddr{IP: []byte{0, 0, 0, 0}, Port: 0}
	broadcast := types.BroadcastAddr{IP: []byte{255, 255, 255, 255}, Port: 60000}
	listen := types.ListenAddr{IP: []byte{0, 0, 0, 0}, Port: 60001}
	timeout := 5 * time.Second
	devices := []uhppote.Device{}
	debug := false

	if u != nil {
		if s := C.GoString(u.bind); s != "" {
			if addr, err := types.ResolveBindAddr(s); err != nil {
				return nil, err
			} else if addr != nil {
				bind = *addr
			}
		}

		if s := C.GoString(u.broadcast); s != "" {
			if addr, err := types.ResolveBroadcastAddr(s); err != nil {
				return nil, err
			} else if addr != nil {
				broadcast = *addr
			}
		}

		if s := C.GoString(u.listen); s != "" {
			if addr, err := types.ResolveListenAddr(s); err != nil {
				return nil, err
			} else if addr != nil {
				listen = *addr
			}
		}

		if u.timeout > 0 {
			timeout = time.Duration(u.timeout) * time.Millisecond
		}

		debug = bool(u.debug)

		if u.devices != nil && u.devices.N > 0 && u.devices.devices != nil {
			list := unsafe.Slice(u.devices.devices, u.devices.N)
			for _, d := range list {
				if d.id != 0 {
					addr, err := types.ResolveAddr(C.GoString(d.address))
					if err != nil {
						return nil, err
					}

					devices = append(devices, uhppote.Device{
						DeviceID: uint32(d.id),
						Address:  (*net.UDPAddr)(addr),
					})
				}
			}
		}
	}

	DEBUG = debug

	return uhppote.NewUHPPOTE(bind, broadcast, listen, timeout, devices, debug), nil
}

// Ref. https://go-review.googlesource.com/c/go/+/277432/2/doc/go1.15.html
func makeTimeProfile(profile *C.struct_TimeProfile) (*types.TimeProfile, error) {
	p := types.TimeProfile{
		ID:              uint8(profile.ID),
		LinkedProfileID: uint8(profile.linked),

		Weekdays: map[time.Weekday]bool{
			time.Monday:    profile.monday != 0,
			time.Tuesday:   profile.tuesday != 0,
			time.Wednesday: profile.wednesday != 0,
			time.Thursday:  profile.thursday != 0,
			time.Friday:    profile.friday != 0,
			time.Saturday:  profile.saturday != 0,
			time.Sunday:    profile.sunday != 0,
		},

		Segments: map[uint8]types.Segment{},
	}

	if from, err := types.DateFromString(C.GoString(profile.from)); err != nil {
		return nil, fmt.Errorf("invalid 'from' date (%v)", err)
	} else {
		p.From = &from
	}

	if to, err := types.DateFromString(C.GoString(profile.to)); err != nil {
		return nil, fmt.Errorf("invalid 'to' date (%v)", err)
	} else {
		p.To = &to
	}

	hhmm := map[uint8][2]string{
		1: {C.GoString(profile.segment1start), C.GoString(profile.segment1end)},
		2: {C.GoString(profile.segment2start), C.GoString(profile.segment2end)},
		3: {C.GoString(profile.segment3start), C.GoString(profile.segment3end)},
	}

	for k, v := range hhmm {
		var start types.HHmm
		var end types.HHmm

		if v[0] != "" {
			if hhmm, err := types.HHmmFromString(v[0]); err != nil {
				return nil, fmt.Errorf("invalid segment %v start (%v)", k, v[0])
			} else if hhmm == nil {
				return nil, fmt.Errorf("invalid segment %v start (%v)", k, v[0])
			} else {
				start = *hhmm
			}
		}

		if v[1] != "" {
			if hhmm, err := types.HHmmFromString(v[1]); err != nil {
				return nil, fmt.Errorf("invalid segment %v end (%v)", k, v[1])
			} else if hhmm == nil {
				return nil, fmt.Errorf("invalid segment %v end (%v)", k, v[1])
			} else {
				end = *hhmm
			}
		}

		p.Segments[k] = types.Segment{
			Start: start,
			End:   end,
		}
	}

	return &p, nil
}

func makeTask(task *C.struct_Task) (*types.Task, error) {
	from, err := types.DateFromString(C.GoString(task.from))
	if err != nil {
		return nil, fmt.Errorf("invalid 'from' date (%v)", err)
	}

	to, err := types.DateFromString(C.GoString(task.to))
	if err != nil {
		return nil, fmt.Errorf("invalid 'to' date (%v)", err)
	}

	at, err := types.HHmmFromString(C.GoString(task.at))
	if err != nil {
		return nil, fmt.Errorf("invalid 'start' time (%v)", err)
	}

	t := types.Task{
		Task: types.TaskType(task.task),
		Door: uint8(task.door),
		From: from,
		To:   to,
		Weekdays: map[time.Weekday]bool{
			time.Monday:    task.monday != 0,
			time.Tuesday:   task.tuesday != 0,
			time.Wednesday: task.wednesday != 0,
			time.Thursday:  task.thursday != 0,
			time.Friday:    task.friday != 0,
			time.Saturday:  task.saturday != 0,
			time.Sunday:    task.sunday != 0,
		},

		Start: *at,
		Cards: uint8(task.cards),
	}

	return &t, nil
}

func cbool(b bool) C.uchar {
	if b {
		return 1
	} else {
		return 0
	}
}
