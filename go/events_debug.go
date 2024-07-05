//go:build debug

package main

import (
	"C"
	"fmt"
	"os"
	"time"

	"github.com/uhppoted/uhppote-core/messages"
	"github.com/uhppoted/uhppote-core/types"
	"github.com/uhppoted/uhppote-core/uhppote"
)

type event messages.GetStatusResponse

func getEventIndex(uu uhppote.IUHPPOTE, index *uint32, deviceID uint32) error {
	if index == nil {
		return fmt.Errorf("invalid argument (index) - expected valid pointer")
	}

	if DEBUG {
		fmt.Printf(">>> get-event-index\n")
		fmt.Printf("    ID: %v\n", deviceID)
		fmt.Println()
	}

	*index = 73

	return nil
}

func setEventIndex(uu uhppote.IUHPPOTE, deviceID uint32, index uint32) error {
	if DEBUG {
		fmt.Printf(">>> set-event-index\n")
		fmt.Printf("    ID:    %v\n", deviceID)
		fmt.Printf("    index: %v\n", index)
		fmt.Println()
	}

	return nil
}

func getEvent(uu uhppote.IUHPPOTE, event *C.struct_Event, deviceID uint32, index uint32) error {
	if event == nil {
		return fmt.Errorf("invalid argument (event) - expected valid pointer")
	}

	if DEBUG {
		fmt.Printf(">>> get-event\n")
		fmt.Printf("    ID:    %v\n", deviceID)
		fmt.Printf("    index: %v\n", index)
		fmt.Println()
	}

	event.timestamp = C.CString("2022-04-15 12:29:15")
	event.index = C.uint(index)
	event.eventType = C.uchar(1)
	event.granted = cbool(true)
	event.door = C.uchar(3)
	event.direction = C.uchar(1)
	event.card = C.uint(8165538)
	event.reason = C.uchar(15)

	return nil
}

func recordSpecialEvents(uu uhppote.IUHPPOTE, deviceID uint32, enabled bool) error {
	if DEBUG {
		fmt.Printf(">>> record-special-events\n")
		fmt.Printf("    ID:      %v\n", deviceID)
		fmt.Printf("    enabled: %v\n", enabled)
		fmt.Println()
	}

	return nil
}

func listen(uu uhppote.IUHPPOTE, listener uhppote.Listener, quit chan os.Signal) error {
	if DEBUG {
		fmt.Printf(">>> listen\n")
	}

	index := uint32(17)
	closed := make(chan any)

	on_event := func() {
		event := types.Status{
			SerialNumber:   405419896,
			DoorState:      map[uint8]bool{1: false, 2: false, 3: false, 4: false},
			DoorButton:     map[uint8]bool{1: false, 2: false, 3: false, 4: false},
			SystemError:    0x00,
			SystemDateTime: types.DateTimeNow(),
			SequenceId:     12345678,
			SpecialInfo:    0x00,
			RelayState:     0x00,
			InputState:     0x00,

			Event: types.StatusEvent{
				Index:      index,
				Type:       1, // swipe
				Granted:    true,
				Door:       2,
				Direction:  1, // in
				CardNumber: 10058400,
				Timestamp:  types.DateTimeNow(),
				Reason:     1, // swipe
			},
		}

		index++

		listener.OnEvent(&event)
	}

	// ... generate controller events
	go func() {
		ticker := time.NewTicker(2500 * time.Millisecond)

		defer ticker.Stop()

	loop:
		for {
			select {
			case <-quit:
				break loop

			case <-ticker.C:
				on_event()
			}
		}

		close(closed)
	}()

	// ... 'listening'
	listener.OnConnected()

	<-closed

	return nil
}
