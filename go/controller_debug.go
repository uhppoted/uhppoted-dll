//go:build debug

package main

import (
	"C"
	"fmt"
	"net"
	"time"
	"unsafe"

	"github.com/uhppoted/uhppote-core/types"
	"github.com/uhppoted/uhppote-core/uhppote"
)

func getDevices(uu uhppote.IUHPPOTE, N *C.int, list *C.uint) error {
	if N == nil {
		return fmt.Errorf("invalid argument (N) - expected valid pointer")
	}

	if list == nil {
		return fmt.Errorf("invalid argument (list) - expected valid pointer to list")
	}

	if DEBUG {
		fmt.Printf(">>> get-devices\n")
		fmt.Printf("    N:  %v\n", *N)
		fmt.Println()
	}

	devices := []uint32{201020304, 303986753, 405419896}

	slice := unsafe.Slice(list, *N)
	for ix, device := range devices {
		if ix < int(*N) {
			slice[ix] = C.uint(device)
		} else {
			break
		}
	}

	*N = C.int(len(devices))

	return nil
}

func getDevice(uu uhppote.IUHPPOTE, device *C.struct_Device, deviceID uint32) error {
	if device == nil {
		return fmt.Errorf("invalid argument (device) - expected valid pointer to Device struct")
	}

	if DEBUG {
		fmt.Printf(">>> get-device\n")
		fmt.Printf("    ID: %v\n", deviceID)
		fmt.Println()
	}

	device.ID = C.uint(deviceID)
	cstring("192.168.1.101", device.address, 16)
	cstring("255.255.255.0", device.subnet, 16)
	cstring("192.168.1.1", device.gateway, 16)
	cstring("00:12:23:34:45:56", device.MAC, 18)
	cstring("v8.92", device.version, 7)
	cstring("2018-11-05", device.date, 11)

	return nil
}

func setAddress(uu uhppote.IUHPPOTE, deviceID uint32, address, subnet, gateway *C.char) error {
	_address := net.ParseIP(C.GoString(address))
	if _address == nil {
		return fmt.Errorf("invalid IP address (%v)", C.GoString(address))
	}

	_subnet := net.ParseIP(C.GoString(subnet))
	if _subnet == nil {
		return fmt.Errorf("invalid IP subnet mask (%v)", C.GoString(subnet))
	}

	_gateway := net.ParseIP(C.GoString(gateway))
	if _gateway == nil {
		return fmt.Errorf("invalid IP gateway address (%v)", C.GoString(gateway))
	}

	if DEBUG {
		fmt.Printf(">>> set-address\n")
		fmt.Printf("    ID:      %v\n", deviceID)
		fmt.Printf("    address: %v\n", _address)
		fmt.Printf("    subnet:  %v\n", _subnet)
		fmt.Printf("    gateway: %v\n", _gateway)
		fmt.Println()
	}

	return nil
}

func getStatus(uu uhppote.IUHPPOTE, status *C.struct_Status, deviceID uint32) error {
	if status == nil {
		return fmt.Errorf("invalid argument (status) - expected valid pointer to Status struct")
	}

	if status.event == nil {
		return fmt.Errorf("invalid argument (status) - expected valid pointer to Status.Event struct")
	}

	if DEBUG {
		fmt.Printf(">>> get-status\n")
		fmt.Printf("    ID: %v\n", deviceID)
		fmt.Println()
	}

	status.ID = C.uint(405419896)

	cstring("2022-03-19 15:48:32", status.sysdatetime, 20)

	doors := unsafe.Slice(status.doors, 4)
	buttons := unsafe.Slice(status.buttons, 4)

	doors[0] = 1
	doors[1] = 0
	doors[2] = 0
	doors[3] = 1

	buttons[0] = 1
	buttons[1] = 0
	buttons[2] = 1
	buttons[3] = 0

	status.relays = 0x12
	status.inputs = 0x34

	status.syserror = 0x56
	status.info = 253
	status.seqno = 9876

	cstring("2022-01-02 12:34:56", status.event.timestamp, 20)
	status.event.index = 135
	status.event.eventType = 0x06
	status.event.granted = 1
	status.event.door = 3
	status.event.direction = 1
	status.event.card = 8100023
	status.event.reason = 0x15

	return nil
}

func getTime(uu uhppote.IUHPPOTE, deviceID uint32, datetime *C.char) error {
	if datetime == nil {
		return fmt.Errorf("invalid argument (datetime) - expected valid pointer to char[20]")
	}

	if DEBUG {
		fmt.Printf(">>> get-time\n")
		fmt.Printf("    ID: %v\n", deviceID)
		fmt.Println()
	}

	cstring("2022-01-02 12:34:56", datetime, 20)

	return nil
}

func setTime(uu uhppote.IUHPPOTE, deviceID uint32, datetime *C.char) error {
	if datetime == nil {
		return fmt.Errorf("invalid argument (datetime) - expected valid pointer to string")
	}

	if dt, err := time.Parse("2006-01-02 15:04:05", C.GoString(datetime)); err != nil {
		return err
	} else {
		if DEBUG {
			fmt.Printf(">>> set-time\n")
			fmt.Printf("    ID:        %v\n", deviceID)
			fmt.Printf("    date/time: %v\n", dt.Format("2006-01-02 15:04:05"))
			fmt.Println()
		}

		return nil
	}
}

func getListener(uu uhppote.IUHPPOTE, controller uint32, address *C.char, interval *uint8) error {
	if address == nil {
		return fmt.Errorf("invalid argument (address) - expected valid pointer to char[22]")
	}

	if DEBUG {
		fmt.Printf(">>> get-listener\n")
		fmt.Printf("    ID: %v\n", controller)
		fmt.Println()
	}

	cstring("192.168.1.100:60001", address, 22)

	if interval == nil {
		*interval = 15
	}

	return nil
}

func setListener(uu uhppote.IUHPPOTE, controller uint32, listener *C.char, interval uint8) error {
	if listener == nil {
		return fmt.Errorf("invalid argument (listener) - expected valid pointer to string")
	}

	if address, err := net.ResolveUDPAddr("udp", C.GoString(listener)); err != nil {
		return err
	} else if address == nil || address.IP.To4() == nil {
		return fmt.Errorf("Invalid UDP address: %v", listener)
	} else if DEBUG {
		fmt.Printf(">>> set-listener\n")
		fmt.Printf("    ID:       %v\n", controller)
		fmt.Printf("    listener: %v\n", address.IP.To4())
		fmt.Printf("    interval: %d\n", interval)
		fmt.Println()
	}

	return nil
}

func getDoorControl(uu uhppote.IUHPPOTE, control *C.struct_DoorControl, deviceID uint32, door uint8) error {
	if control == nil {
		return fmt.Errorf("invalid argument (device) - expected valid pointer to DoorControl struct")
	}

	if DEBUG {
		fmt.Printf(">>> get-door-control\n")
		fmt.Printf("    ID:   %v\n", deviceID)
		fmt.Printf("    door: %v\n", door)
		fmt.Println()
	}

	control.mode = C.uchar(types.Controlled)
	control.delay = C.uchar(7)

	return nil
}

func setDoorControl(uu uhppote.IUHPPOTE, deviceID uint32, door uint8, mode types.ControlState, delay uint8) error {
	if DEBUG {
		fmt.Printf(">>> set-door-control\n")
		fmt.Printf("    ID:    %v\n", deviceID)
		fmt.Printf("    door:  %v\n", door)
		fmt.Printf("    mode:  %v\n", mode)
		fmt.Printf("    delay: %v\n", delay)
		fmt.Println()
	}

	return nil
}

func openDoor(uu uhppote.IUHPPOTE, deviceID uint32, door uint8) error {
	if DEBUG {
		fmt.Printf(">>> open-doorl\n")
		fmt.Printf("    ID:    %v\n", deviceID)
		fmt.Printf("    door:  %v\n", door)
		fmt.Println()
	}

	return nil
}

func setPCControl(uu uhppote.IUHPPOTE, controller uint32, enabled bool) error {
	if DEBUG {
		fmt.Printf(">>> set-pc-control\n")
		fmt.Printf("    ID:      %v\n", controller)
		fmt.Printf("    enabled: %v\n", enabled)
		fmt.Println()
	}

	return nil
}

func setInterlock(uu uhppote.IUHPPOTE, controller uint32, interlock uint8) error {
	if DEBUG {
		fmt.Printf(">>> set-interlock\n")
		fmt.Printf("    ID:        %v\n", controller)
		fmt.Printf("    interlock: %v\n", interlock)
		fmt.Println()
	}

	return nil
}

func activateKeypads(uu uhppote.IUHPPOTE, controller uint32, reader1, reader2, reader3, reader4 bool) error {
	if DEBUG {
		fmt.Printf(">>> activate-keypads\n")
		fmt.Printf("    ID:        %v\n", controller)
		fmt.Printf("    readers 1: %v\n", reader1)
		fmt.Printf("            2: %v\n", reader2)
		fmt.Printf("            3: %v\n", reader3)
		fmt.Printf("            4: %v\n", reader4)
		fmt.Println()
	}

	return nil
}

// Debug implementation of setDoorPasscodes.
//
// Prints the controller ID, door ID and passcodes to the console (if DEBUG is enabled) and returns nil.
func setDoorPasscodes(uu uhppote.IUHPPOTE, controller uint32, door uint8, passcode1, passcode2, passcode3, passcode4 uint32) error {
	if DEBUG {
		fmt.Printf(">>> set-door-passcodes\n")
		fmt.Printf("    ID:          %v\n", controller)
		fmt.Printf("    door:        %v\n", door)
		fmt.Printf("    passcodes 1: %v\n", passcode1)
		fmt.Printf("              2: %v\n", passcode2)
		fmt.Printf("              3: %v\n", passcode3)
		fmt.Printf("              4: %v\n", passcode4)
		fmt.Println()
	}

	return nil
}

// Debug implementation of getAntiPassback
func getAntiPassback(uu uhppote.IUHPPOTE, controller uint32, antipassback *uint8) error {
	if antipassback == nil {
		return fmt.Errorf("invalid argument (antipassback) - expected valid pointer to uint8")
	}

	if DEBUG {
		fmt.Printf(">>> get-antipassback\n")
		fmt.Printf("    ID:           %v\n", controller)
		fmt.Printf("    antipassback: %v\n", antipassback)
		fmt.Println()
	}

	*antipassback = 2

	return nil
}

// Debug implementation of setAntiPassback
func setAntiPassback(uu uhppote.IUHPPOTE, controller uint32, antipassback uint8) error {
	if DEBUG {
		fmt.Printf(">>> set-antipassback\n")
		fmt.Printf("    ID:           %v\n", controller)
		fmt.Printf("    antipassback: %v\n", antipassback)
		fmt.Println()
	}

	return nil
}

// Debug implementation of restoreDefaultParameters
//
// Prints the controller ID to the console (if DEBUG is enabled) and returns nil.
func restoreDefaultParameters(uu uhppote.IUHPPOTE, controller uint32) error {
	if DEBUG {
		fmt.Printf(">>> restore-default-parameters\n")
		fmt.Printf("    controller: %v\n", controller)
		fmt.Println()
	}

	return nil
}
