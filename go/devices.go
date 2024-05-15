//go:build !debug && !tests

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

const DEBUG_TAG = "get-status:dll ltsc.7"

func getDevices(uu uhppote.IUHPPOTE, N *C.int, list *C.uint) error {
	if N == nil {
		return fmt.Errorf("invalid argument (N) - expected valid pointer")
	}

	if list == nil {
		return fmt.Errorf("invalid argument (list) - expected valid pointer to list")
	}

	devices, err := uu.GetDevices()
	if err != nil {
		return err
	}

	slice := unsafe.Slice(list, *N)
	for ix, device := range devices {
		if ix < int(*N) {
			slice[ix] = C.uint(device.SerialNumber)
		} else {
			break
		}
	}

	*N = C.int(len(devices))

	return nil
}

func getDevice(uu uhppote.IUHPPOTE, d *C.struct_Device, deviceID uint32) error {
	if d == nil {
		return fmt.Errorf("invalid argument (device) - expected valid pointer to Device struct")
	}

	response, err := uu.GetDevice(deviceID)
	if err != nil {
		return err
	} else if response == nil {
		return fmt.Errorf("%v: no response to get-device", deviceID)
	}

	d.ID = C.uint(deviceID)
	d.address = C.CString(fmt.Sprintf("%v", response.IpAddress))
	d.subnet = C.CString(fmt.Sprintf("%v", response.SubnetMask))
	d.gateway = C.CString(fmt.Sprintf("%v", response.Gateway))
	d.MAC = C.CString(fmt.Sprintf("%v", response.MacAddress))
	d.version = C.CString(fmt.Sprintf("%v", response.Version))
	d.date = C.CString(fmt.Sprintf("%v", response.Date))

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

	if response, err := uu.SetAddress(deviceID, _address, _subnet, _gateway); err != nil {
		return err
	} else if response == nil {
		return fmt.Errorf("invalid reply from device (%v)", response)
	} else if !response.Succeeded {
		return fmt.Errorf("failed to set device address")
	}

	return nil
}

func getStatus(uu uhppote.IUHPPOTE, status *C.struct_Status, deviceID uint32) error {
	fmt.Printf("%v\n", DEBUG_TAG)

	if status == nil {
		return fmt.Errorf("invalid argument (status) - expected valid pointer to Status struct")
	}

	fmt.Printf("%v#1\n", DEBUG_TAG)

	response, err := uu.GetStatus(deviceID)
	if err != nil {
		return err
	} else if response == nil {
		return fmt.Errorf("%v: no response to get-status", deviceID)
	}

	status.ID = C.uint(response.SerialNumber)

	sysdatetime := unsafe.Slice(status.sysdatetime, 20)
	doors := unsafe.Slice(status.doors, 4)
	buttons := unsafe.Slice(status.buttons, 4)
	timestamp := unsafe.Slice(status.eventTimestamp, 20)

	{
		s := time.Time(response.SystemDateTime).Format("2006-01-02 15:04:05")
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

		fmt.Printf("%v#2\n", DEBUG_TAG)
	}

	doors[0] = cbool(response.DoorState[1])
	doors[1] = cbool(response.DoorState[2])
	doors[2] = cbool(response.DoorState[3])
	doors[3] = cbool(response.DoorState[4])

	fmt.Println("get-status:dll %v#3\n", DEBUG_TAG)

	buttons[0] = cbool(response.DoorButton[1])
	buttons[1] = cbool(response.DoorButton[2])
	buttons[2] = cbool(response.DoorButton[3])
	buttons[3] = cbool(response.DoorButton[4])

	fmt.Println("get-status:dll %v#4\n", DEBUG_TAG)

	status.relays = C.uchar(response.RelayState)
	status.inputs = C.uchar(response.InputState)

	status.syserror = C.uchar(response.SystemError)
	status.seqno = C.uint(response.SequenceId)
	status.info = C.uchar(response.SpecialInfo)

	fmt.Println("get-status:dll %v#5\n", DEBUG_TAG)

	if response.Event.IsZero() {
		fmt.Printf("%v#7\n", DEBUG_TAG)

		s := ""
		v := []byte(s)

		timestamp[0] = C.uchar(v[0])
		timestamp[1] = C.uchar(v[1])
		timestamp[2] = C.uchar(v[2])
		timestamp[3] = C.uchar(v[3])
		timestamp[4] = C.uchar(v[4])
		timestamp[5] = C.uchar(v[5])
		timestamp[6] = C.uchar(v[6])
		timestamp[7] = C.uchar(v[7])
		timestamp[8] = C.uchar(v[8])
		timestamp[9] = C.uchar(v[9])
		timestamp[10] = C.uchar(v[10])
		timestamp[11] = C.uchar(v[11])
		timestamp[12] = C.uchar(v[12])
		timestamp[13] = C.uchar(v[13])
		timestamp[14] = C.uchar(v[14])
		timestamp[15] = C.uchar(v[15])
		timestamp[16] = C.uchar(v[16])
		timestamp[17] = C.uchar(v[17])
		timestamp[18] = C.uchar(v[18])
		timestamp[19] = C.uchar(0)

		status.eventIndex = C.uint(0)
		status.eventType = C.uchar(0)
		status.eventGranted = cbool(false)
		status.eventDoor = C.uchar(0)
		status.eventDirection = C.uchar(0)
		status.eventCard = C.uint(0)
		status.eventReason = C.uchar(0)
	} else {
		fmt.Println("get-status:dll %v#8\n", DEBUG_TAG)

		s := time.Time(response.Event.Timestamp).Format("2006-01-02 15:04:05")
		v := []byte(s)

		timestamp[0] = C.uchar(v[0])
		timestamp[1] = C.uchar(v[1])
		timestamp[2] = C.uchar(v[2])
		timestamp[3] = C.uchar(v[3])
		timestamp[4] = C.uchar(v[4])
		timestamp[5] = C.uchar(v[5])
		timestamp[6] = C.uchar(v[6])
		timestamp[7] = C.uchar(v[7])
		timestamp[8] = C.uchar(v[8])
		timestamp[9] = C.uchar(v[9])
		timestamp[10] = C.uchar(v[10])
		timestamp[11] = C.uchar(v[11])
		timestamp[12] = C.uchar(v[12])
		timestamp[13] = C.uchar(v[13])
		timestamp[14] = C.uchar(v[14])
		timestamp[15] = C.uchar(v[15])
		timestamp[16] = C.uchar(v[16])
		timestamp[17] = C.uchar(v[17])
		timestamp[18] = C.uchar(v[18])
		timestamp[19] = C.uchar(0)

		status.eventIndex = C.uint(response.Event.Index)
		status.eventType = C.uchar(response.Event.Type)
		status.eventGranted = cbool(response.Event.Granted)
		status.eventDoor = C.uchar(response.Event.Door)
		status.eventDirection = C.uchar(response.Event.Direction)
		status.eventCard = C.uint(response.Event.CardNumber)
		status.eventReason = C.uchar(response.Event.Reason)
	}

	fmt.Println("get-status:dll DEBUG_TAG#9\n", DEBUG_TAG)

	return nil
}

// func getStatus(uu uhppote.IUHPPOTE, status *C.struct_Status, deviceID uint32) error {
// 	if status == nil {
// 		return fmt.Errorf("invalid argument (status) - expected valid pointer to Status struct")
// 	}
//
// 	response, err := uu.GetStatus(deviceID)
// 	if err != nil {
// 		return err
// 	} else if response == nil {
// 		return fmt.Errorf("%v: no response to get-status", deviceID)
// 	}
//
// 	format := func(t types.DateTime) string {
// 		return time.Time(t).Format("2006-01-02 15:04:05")
// 	}
//
// 	status.ID = C.uint(response.SerialNumber)
// 	status.sysdatetime = C.CString(format(response.SystemDateTime))
//
// 	doors := unsafe.Slice(status.doors, 4)
// 	buttons := unsafe.Slice(status.buttons, 4)
//
// 	doors[0] = cbool(response.DoorState[1])
// 	doors[1] = cbool(response.DoorState[2])
// 	doors[2] = cbool(response.DoorState[3])
// 	doors[3] = cbool(response.DoorState[4])
//
// 	buttons[0] = cbool(response.DoorButton[1])
// 	buttons[1] = cbool(response.DoorButton[2])
// 	buttons[2] = cbool(response.DoorButton[3])
// 	buttons[3] = cbool(response.DoorButton[4])
//
// 	status.relays = C.uchar(response.RelayState)
// 	status.inputs = C.uchar(response.InputState)
//
// 	status.syserror = C.uchar(response.SystemError)
// 	status.seqno = C.uint(response.SequenceId)
// 	status.info = C.uchar(response.SpecialInfo)
//
// 	if response.Event.IsZero() {
// 		status.event.timestamp = C.CString("")
// 		status.event.index = C.uint(0)
// 		status.event.eventType = C.uchar(0)
// 		status.event.granted = cbool(false)
// 		status.event.door = C.uchar(0)
// 		status.event.direction = C.uchar(0)
// 		status.event.card = C.uint(0)
// 		status.event.reason = C.uchar(0)
// 	} else {
// 		status.event.timestamp = C.CString(format(response.Event.Timestamp))
// 		status.event.index = C.uint(response.Event.Index)
// 		status.event.eventType = C.uchar(response.Event.Type)
// 		status.event.granted = cbool(response.Event.Granted)
// 		status.event.door = C.uchar(response.Event.Door)
// 		status.event.direction = C.uchar(response.Event.Direction)
// 		status.event.card = C.uint(response.Event.CardNumber)
// 		status.event.reason = C.uchar(response.Event.Reason)
// 	}
//
// 	return nil
// }

func getTime(uu uhppote.IUHPPOTE, datetime **C.char, deviceID uint32) error {
	if datetime == nil {
		return fmt.Errorf("invalid argument (datetime) - expected valid pointer to string")
	}

	response, err := uu.GetTime(deviceID)
	if err != nil {
		return err
	} else if response == nil {
		return fmt.Errorf("%v: no response to get-time", deviceID)
	}

	*datetime = C.CString(fmt.Sprintf("%v", response.DateTime))

	return nil
}

func setTime(uu uhppote.IUHPPOTE, deviceID uint32, datetime *C.char) error {
	if datetime == nil {
		return fmt.Errorf("invalid argument (datetime) - expected valid pointer to string")
	}

	if dt, err := time.Parse("2006-01-02 15:04:05", C.GoString(datetime)); err != nil {
		return err
	} else {
		response, err := uu.SetTime(deviceID, dt)
		if err != nil {
			return err
		} else if response == nil {
			return fmt.Errorf("%v: no response to set-time", deviceID)
		}

		return nil
	}
}

func getListener(uu uhppote.IUHPPOTE, listener **C.char, deviceID uint32) error {
	if listener == nil {
		return fmt.Errorf("invalid argument (address) - expected valid pointer to string")
	}

	response, err := uu.GetListener(deviceID)
	if err != nil {
		return err
	} else if response == nil {
		return fmt.Errorf("%v: no response to get-listener", deviceID)
	}

	*listener = C.CString(fmt.Sprintf("%v:%v", response.Address.IP, response.Address.Port))

	return nil
}

func setListener(uu uhppote.IUHPPOTE, deviceID uint32, listener *C.char) error {
	if listener == nil {
		return fmt.Errorf("invalid argument (listener) - expected valid pointer to string")
	}

	if address, err := net.ResolveUDPAddr("udp", C.GoString(listener)); err != nil {
		return err
	} else if address == nil || address.IP.To4() == nil {
		return fmt.Errorf("invalid UDP address: %v", listener)
	} else {
		if response, err := uu.SetListener(deviceID, *address); err != nil {
			return err
		} else if response == nil {
			return fmt.Errorf("%v: no response to set-listener", deviceID)
		}
	}

	return nil
}

func getDoorControl(uu uhppote.IUHPPOTE, control *C.struct_DoorControl, deviceID uint32, door uint8) error {
	if control == nil {
		return fmt.Errorf("invalid argument (device) - expected valid pointer to DoorControl struct")
	}

	response, err := uu.GetDoorControlState(deviceID, door)
	if err != nil {
		return err
	} else if response == nil {
		return fmt.Errorf("%v: no response to get-door-control-state", deviceID)
	}

	control.mode = C.uchar(response.ControlState)
	control.delay = C.uchar(response.Delay)

	return nil
}

func setDoorControl(uu uhppote.IUHPPOTE, deviceID uint32, door uint8, mode types.ControlState, delay uint8) error {
	response, err := uu.SetDoorControlState(deviceID, door, mode, delay)
	if err != nil {
		return err
	} else if response == nil {
		return fmt.Errorf("%v: no response to set-door-control-state", deviceID)
	}

	return nil
}

func openDoor(uu uhppote.IUHPPOTE, deviceID uint32, door uint8) error {
	response, err := uu.OpenDoor(deviceID, door)
	if err != nil {
		return err
	} else if response == nil {
		return fmt.Errorf("%v: no response to open door %d", deviceID, door)
	}

	return nil
}

func setPCControl(uu uhppote.IUHPPOTE, controller uint32, enabled bool) error {
	ok, err := uu.SetPCControl(controller, enabled)
	if err != nil {
		return err
	} else if !ok && enabled {
		return fmt.Errorf("%v: failed to enable remote access control", controller)
	} else if !ok && !enabled {
		return fmt.Errorf("%v: failed to disable remote access control", controller)
	}

	return nil
}

func setInterlock(uu uhppote.IUHPPOTE, controller uint32, interlock uint8) error {
	if interlock != 0 && interlock != 1 && interlock != 2 && interlock != 3 && interlock != 4 && interlock != 8 {
		return fmt.Errorf("invalid interlock value (%v)", interlock)
	} else if ok, err := uu.SetInterlock(controller, types.Interlock(interlock)); err != nil {
		return err
	} else if !ok {
		return fmt.Errorf("%v: failed to set controller interlock", controller)
	}

	return nil
}

func activateKeypads(uu uhppote.IUHPPOTE, controller uint32, reader1, reader2, reader3, reader4 bool) error {
	keypads := map[uint8]bool{
		1: reader1,
		2: reader2,
		3: reader3,
		4: reader4,
	}

	if ok, err := uu.ActivateKeypads(controller, keypads); err != nil {
		return err
	} else if !ok {
		return fmt.Errorf("%v: failed to activate/deactivate controller reader access keypads", controller)
	}

	return nil
}

// Sets the supervisor passcodes for a door managed by the controller.
//
// Valid passcodes are in the range [1..999999] or 0 (no code) - invalid codes will be replaced by
// a 0 (no passcode).
func setDoorPasscodes(uu uhppote.IUHPPOTE, controller uint32, door uint8, passcode1, passcode2, passcode3, passcode4 uint32) error {
	if ok, err := uu.SetDoorPasscodes(controller, door, passcode1, passcode2, passcode3, passcode4); err != nil {
		return err
	} else if !ok {
		return fmt.Errorf("%v: failed to set supervisor passcodes for door %v", controller, door)
	}

	return nil
}

// Resets a controller to the manufacturer default configuration.
func restoreDefaultParameters(uu uhppote.IUHPPOTE, controller uint32) error {
	ok, err := uu.RestoreDefaultParameters(controller)
	if err != nil {
		return err
	} else if !ok {
		return fmt.Errorf("%v: failed to reset controller", controller)
	}

	return nil
}
