//go:build !debug && !tests

package main

import (
	"C"
	"fmt"
	"os"
	"os/signal"
	"syscall"

	"github.com/uhppoted/uhppote-core/uhppote"
)

func getEventIndex(uu uhppote.IUHPPOTE, index *uint32, deviceID uint32) error {
	if index == nil {
		return fmt.Errorf("invalid argument (index) - expected valid pointer")
	}

	reply, err := uu.GetEventIndex(deviceID)
	if err != nil {
		return err
	}

	*index = reply.Index

	return nil
}

func setEventIndex(uu uhppote.IUHPPOTE, deviceID uint32, index uint32) error {
	reply, err := uu.SetEventIndex(deviceID, index)
	if err != nil {
		return err
	} else if !reply.Changed {
		return fmt.Errorf("%v: failed to set event index", deviceID)
	}

	return nil
}

func getEvent(uu uhppote.IUHPPOTE, event *C.struct_Event, deviceID uint32, index uint32) error {
	if event == nil {
		return fmt.Errorf("invalid argument (event) - expected valid pointer")
	}

	e, err := uu.GetEvent(deviceID, index)
	if err != nil {
		return err
	} else if e == nil {
		return fmt.Errorf("%v: no response to get-event %v", deviceID, index)
	}

	event.timestamp = C.CString(fmt.Sprintf("%v", e.Timestamp))
	event.index = C.uint(e.Index)
	event.eventType = C.uchar(e.Type)
	event.granted = cbool(e.Granted)
	event.door = C.uchar(e.Door)
	event.direction = C.uchar(e.Direction)
	event.card = C.uint(e.CardNumber)
	event.reason = C.uchar(e.Reason)

	return nil
}

func recordSpecialEvents(uu uhppote.IUHPPOTE, deviceID uint32, enabled bool) error {
	ok, err := uu.RecordSpecialEvents(deviceID, enabled)
	if err != nil {
		return err
	} else if !ok && enabled {
		return fmt.Errorf("%v: failed to enable record special events", deviceID)
	} else if !ok && !enabled {
		return fmt.Errorf("%v: failed to disable record special events", deviceID)
	}

	return nil
}

func listen(uu uhppote.IUHPPOTE, f uhppote.Listener) error {
	os.Remove("/tmp/uhppoted-dll.pipe")

	if err := syscall.Mkfifo("/tmp/uhppoted-dll.pipe", 0666); err != nil {
		return err
	}

	if _, err := os.OpenFile("/tmp/uhppoted-dll.pipe", os.O_RDWR|os.O_CREATE|os.O_APPEND, os.ModeNamedPipe); err != nil {
		return err
	} else {
		go func() {
			q := make(chan os.Signal, 1)

			defer close(q)

			signal.Notify(q, os.Interrupt)

			uu.Listen(f, q)
		}()

		return nil
	}
}

// func listen(uu uhppote.IUHPPOTE, f uhppote.Listener) error {
// 	q := make(chan os.Signal, 1)
//
// 	defer close(q)
//
// 	signal.Notify(q, os.Interrupt)
//
// 	uu.Listen(f, q)
// 	return nil
// }
