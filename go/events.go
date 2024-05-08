//go:build !debug && !tests

package main

import (
	"C"
	"bytes"
	"encoding/binary"
	"fmt"
	"os"
	"os/signal"
	"syscall"

	"github.com/uhppoted/uhppote-core/types"
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

func listen(uu uhppote.IUHPPOTE, file string) error {
	os.Remove(file)

	if err := syscall.Mkfifo(file, 0666); err != nil {
		return err
	}

	if pipe, err := os.OpenFile(file, os.O_RDWR|os.O_CREATE|os.O_APPEND, os.ModeNamedPipe); err != nil {
		return err
	} else {
		go func() {
			l := listener{
				pipe: pipe,
			}

			q := make(chan os.Signal, 1)

			defer close(q)
			defer l.pipe.Close()

			signal.Notify(q, os.Interrupt)

			uu.Listen(&l, q)
		}()

		return nil
	}

}

type listener struct {
	pipe *os.File
}

func (l *listener) OnConnected() {
}

func (l *listener) OnEvent(status *types.Status) {
	if status != nil {
		if event, err := l.pack(*status); err != nil {
			fmt.Printf(">>> ERROR:%v\n", err)
		} else if _, err := l.pipe.Write(event); err != nil {
			fmt.Printf(">>> ERROR:%v\n", err)
		}
	}
}

func (l *listener) OnError(err error) bool {
	return false
}

func (l *listener) pack(status types.Status) ([]byte, error) {
	var b bytes.Buffer

	if err := binary.Write(&b, binary.LittleEndian, status.SerialNumber); err != nil {
		return nil, err
	}

	for _, k := range []uint8{1, 2, 3, 4} {
		if v, ok := status.DoorState[k]; ok {
			if err := binary.Write(&b, binary.LittleEndian, v); err != nil {
				return nil, err
			}
		} else {
			if err := binary.Write(&b, binary.LittleEndian, byte(0)); err != nil {
				return nil, err
			}
		}
	}

	return b.Bytes(), nil
}
