//go:build tests

package main

import (
	"C"
	"fmt"
	"os"
	"time"

	"github.com/uhppoted/uhppote-core/types"
	"github.com/uhppoted/uhppote-core/uhppote"
)

func getEventIndex(uu uhppote.IUHPPOTE, index *uint32, deviceID uint32) error {
	if index == nil {
		return fmt.Errorf("invalid argument (index) - expected valid pointer")
	}

	if deviceID != 405419896 {
		return fmt.Errorf("Incorrect device ID (%v)", deviceID)
	}

	*index = 47

	return nil
}

func setEventIndex(uu uhppote.IUHPPOTE, deviceID uint32, index uint32) error {
	if deviceID != 405419896 {
		return fmt.Errorf("Incorrect device ID (%v)", deviceID)
	}

	if index != 51 {
		return fmt.Errorf("Incorrect event index (%v)", index)
	}

	return nil
}

func getEvent(uu uhppote.IUHPPOTE, event *C.struct_Event, deviceID uint32, index uint32) error {
	if event == nil {
		return fmt.Errorf("invalid argument (event) - expected valid pointer")
	}

	if deviceID != 405419896 {
		return fmt.Errorf("Incorrect device ID (%v)", deviceID)
	}

	if index != 51 {
		return fmt.Errorf("Incorrect event index (%v)", index)
	}

	cstring("2022-04-15 12:29:15", event.timestamp, 20)
	event.index = C.uint(index)
	event.eventType = C.uchar(0x06)
	event.granted = cbool(true)
	event.door = C.uchar(3)
	event.direction = C.uchar(1)
	event.card = C.uint(8165538)
	event.reason = C.uchar(0x15)

	return nil
}

func recordSpecialEvents(uu uhppote.IUHPPOTE, deviceID uint32, enabled bool) error {
	if deviceID != 405419896 {
		return fmt.Errorf("Incorrect device ID (%v)", deviceID)
	}

	if !enabled {
		return fmt.Errorf("Incorrect enabled value (%v)", enabled)
	}

	return nil
}

func listen(uu uhppote.IUHPPOTE, listener uhppote.Listener, quit chan os.Signal) error {
	timestamp := time.Date(2024, time.July, 5, 12, 36, 45, 0, time.Local)

	on_event := func() {
		event := types.Status{
			SerialNumber:   405419896,
			DoorState:      map[uint8]bool{1: false, 2: false, 3: false, 4: false},
			DoorButton:     map[uint8]bool{1: false, 2: false, 3: false, 4: false},
			SystemError:    0x00,
			SystemDateTime: types.DateTime(timestamp),
			SequenceId:     12345678,
			SpecialInfo:    0x00,
			RelayState:     0x00,
			InputState:     0x00,

			Event: types.StatusEvent{
				Index:      17,
				Type:       6,
				Granted:    true,
				Door:       2,
				Direction:  1,
				CardNumber: 10058400,
				Timestamp:  types.DateTime(timestamp),
				Reason:     21,
			},
		}

		listener.OnEvent(&event)
	}

	time.AfterFunc(500*time.Millisecond, on_event)
	listener.OnConnected()

	<-quit

	return nil
}
