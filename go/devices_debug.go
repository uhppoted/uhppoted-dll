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
	device.address = C.CString("192.168.1.101")
	device.subnet = C.CString("255.255.255.0")
	device.gateway = C.CString("192.168.1.1")
	device.MAC = C.CString("00:12:23:34:45:56")
	device.version = C.CString("v8.92")
	device.date = C.CString("2018-11-05")

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
	fmt.Println("get-status:debug ltsc.6")

	if status == nil {
		return fmt.Errorf("invalid argument (status) - expected valid pointer to Status struct")
	}

	if status.event == nil {
		return fmt.Errorf("invalid argument (status) - expected valid pointer to Status.Event struct")
	}

	fmt.Println("get-status:debug ltsc.6#1")

	if DEBUG {
		fmt.Printf(">>> get-status\n")
		fmt.Printf("    ID: %v\n", deviceID)
		fmt.Println()
	}

	sysdatetime := unsafe.Slice(status.sysdatetime, 20)
	doors := unsafe.Slice(status.doors, 4)
	buttons := unsafe.Slice(status.buttons, 4)
	// timestamp := unsafe.Slice(status.event.timestamp, 20)

	status.ID = C.uint(405419896)

	{
		s := "2022-03-19 15:48:32"
		v := []byte(s)

		sysdatetime[0] = C.uchar(v[0])
		sysdatetime[1] = C.uchar(v[1])
		sysdatetime[2] = C.uchar(v[2])
		sysdatetime[3] = C.uchar(v[3])
		sysdatetime[4] = C.uchar(v[4])
		sysdatetime[5] = C.uchar(v[5])
		sysdatetime[6] = C.uchar(v[6])
		sysdatetime[7] = C.uchar(v[7])
		sysdatetime[8] = C.uchar(v[8])
		sysdatetime[9] = C.uchar(v[9])
		sysdatetime[10] = C.uchar(v[10])
		sysdatetime[11] = C.uchar(v[11])
		sysdatetime[12] = C.uchar(v[12])
		sysdatetime[13] = C.uchar(v[13])
		sysdatetime[14] = C.uchar(v[14])
		sysdatetime[15] = C.uchar(v[15])
		sysdatetime[16] = C.uchar(v[16])
		sysdatetime[17] = C.uchar(v[17])
		sysdatetime[18] = C.uchar(v[18])
		sysdatetime[19] = C.uchar(0)

		fmt.Println("get-status:debug ltsc.6#2")
	}

	doors[0] = 1
	doors[1] = 0
	doors[2] = 0
	doors[3] = 1

	fmt.Println("get-status:debug ltsc.6#3")

	buttons[0] = 1
	buttons[1] = 0
	buttons[2] = 1
	buttons[3] = 0

	fmt.Println("get-status:debug ltsc.6#3")

	status.relays = 0x12
	status.inputs = 0x34

	status.syserror = 0x56
	status.info = 253
	status.seqno = 9876

	fmt.Println("get-status:debug ltsc.6#4")

	// {
	// 	s := "2022-01-02 12:34:56"
	// 	v := []byte(s)
	//
	// 	timestamp[0] = C.uchar(v[0])
	// 	timestamp[1] = C.uchar(v[1])
	// 	timestamp[2] = C.uchar(v[2])
	// 	timestamp[3] = C.uchar(v[3])
	// 	timestamp[4] = C.uchar(v[4])
	// 	timestamp[5] = C.uchar(v[5])
	// 	timestamp[6] = C.uchar(v[6])
	// 	timestamp[7] = C.uchar(v[7])
	// 	timestamp[8] = C.uchar(v[8])
	// 	timestamp[9] = C.uchar(v[9])
	// 	timestamp[10] = C.uchar(v[10])
	// 	timestamp[11] = C.uchar(v[11])
	// 	timestamp[12] = C.uchar(v[12])
	// 	timestamp[13] = C.uchar(v[13])
	// 	timestamp[14] = C.uchar(v[14])
	// 	timestamp[15] = C.uchar(v[15])
	// 	timestamp[16] = C.uchar(v[16])
	// 	timestamp[17] = C.uchar(v[17])
	// 	timestamp[18] = C.uchar(v[18])
	// 	timestamp[19] = C.uchar(0)
	//
	// 	fmt.Println("get-status:debug ltsc.6#5")
	// }

	status.event.index = 135
	status.event.eventType = 0x06
	status.event.granted = 1
	status.event.door = 3
	status.event.direction = 1
	status.event.card = 8100023
	status.event.reason = 0x15

	fmt.Println("get-status:debug ltsc.6#6")

	return nil
}

// func getStatus(uu uhppote.IUHPPOTE, status *C.struct_Status, deviceID uint32) error {
// 	if status == nil {
// 		return fmt.Errorf("invalid argument (status) - expected valid pointer to Status struct")
// 	}
//
// 	if status.event == nil {
// 		return fmt.Errorf("invalid argument (status) - expected valid pointer to Status.Event struct")
// 	}
//
// 	if DEBUG {
// 		fmt.Printf(">>> get-status\n")
// 		fmt.Printf("    ID: %v\n", deviceID)
// 		fmt.Println()
// 	}
//
// 	status.ID = C.uint(405419896)
// 	status.sysdatetime = C.CString("2022-03-19 15:48:32")
//
// 	doors := unsafe.Slice(status.doors, 4)
// 	buttons := unsafe.Slice(status.buttons, 4)
//
// 	doors[0] = 1
// 	doors[1] = 0
// 	doors[2] = 0
// 	doors[3] = 1
//
// 	buttons[0] = 1
// 	buttons[1] = 0
// 	buttons[2] = 1
// 	buttons[3] = 0
//
// 	status.relays = 0x12
// 	status.inputs = 0x34
//
// 	status.syserror = 0x56
// 	status.info = 253
// 	status.seqno = 9876
//
// 	status.event.timestamp = C.CString("2022-01-02 12:34:56")
// 	status.event.index = 135
// 	status.event.eventType = 0x06
// 	status.event.granted = 1
// 	status.event.door = 3
// 	status.event.direction = 1
// 	status.event.card = 8100023
// 	status.event.reason = 0x15
//
// 	return nil
// }

func getTime(uu uhppote.IUHPPOTE, datetime **C.char, deviceID uint32) error {
	if datetime == nil {
		return fmt.Errorf("invalid argument (datetime) - expected valid pointer to string")
	}

	if DEBUG {
		fmt.Printf(">>> get-time\n")
		fmt.Printf("    ID: %v\n", deviceID)
		fmt.Println()
	}

	*datetime = C.CString("2022-01-02 12:34:56")

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

func getListener(uu uhppote.IUHPPOTE, address **C.char, deviceID uint32) error {
	if address == nil {
		return fmt.Errorf("invalid argument (address) - expected valid pointer to string")
	}

	if DEBUG {
		fmt.Printf(">>> get-listener\n")
		fmt.Printf("    ID: %v\n", deviceID)
		fmt.Println()
	}

	*address = C.CString("192.168.1.100:60001")

	return nil
}

func setListener(uu uhppote.IUHPPOTE, deviceID uint32, listener *C.char) error {
	if listener == nil {
		return fmt.Errorf("invalid argument (listener) - expected valid pointer to string")
	}

	if address, err := net.ResolveUDPAddr("udp", C.GoString(listener)); err != nil {
		return err
	} else if address == nil || address.IP.To4() == nil {
		return fmt.Errorf("Invalid UDP address: %v", listener)
	} else if DEBUG {
		fmt.Printf(">>> set-listener\n")
		fmt.Printf("    ID:       %v\n", deviceID)
		fmt.Printf("    listener: %v\n", address.IP.To4())
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
