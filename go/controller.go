//go:build !debug && !tests

package main

import (
	"C"
	"fmt"
	"net"
	"net/netip"
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

func getDevice(uu uhppote.IUHPPOTE, device *C.struct_Device, deviceID uint32) error {
	if device == nil {
		return fmt.Errorf("invalid argument (device) - expected valid pointer to Device struct")
	}

	response, err := uu.GetDevice(deviceID)
	if err != nil {
		return err
	} else if response == nil {
		return fmt.Errorf("%v: no response to get-device", deviceID)
	}

	device.ID = C.uint(deviceID)

	cstring(response.IpAddress, device.address, 16)
	cstring(response.SubnetMask, device.subnet, 16)
	cstring(response.Gateway, device.gateway, 16)
	cstring(response.MacAddress, device.MAC, 18)
	cstring(response.Version, device.version, 7)
	cstring(response.Date, device.date, 11)

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
	if status == nil {
		return fmt.Errorf("invalid argument (status) - expected valid pointer to Status struct")
	}

	if status.event == nil {
		return fmt.Errorf("invalid argument (status) - expected valid pointer to Status.Event struct")
	}

	response, err := uu.GetStatus(deviceID)
	if err != nil {
		return err
	} else if response == nil {
		return fmt.Errorf("%v: no response to get-status", deviceID)
	}

	format := func(t types.DateTime) string {
		return time.Time(t).Format("2006-01-02 15:04:05")
	}

	status.ID = C.uint(response.SerialNumber)

	cstring(response.SystemDateTime, status.sysdatetime, 20)

	doors := unsafe.Slice(status.doors, 4)
	buttons := unsafe.Slice(status.buttons, 4)

	doors[0] = cbool(response.DoorState[1])
	doors[1] = cbool(response.DoorState[2])
	doors[2] = cbool(response.DoorState[3])
	doors[3] = cbool(response.DoorState[4])

	buttons[0] = cbool(response.DoorButton[1])
	buttons[1] = cbool(response.DoorButton[2])
	buttons[2] = cbool(response.DoorButton[3])
	buttons[3] = cbool(response.DoorButton[4])

	status.relays = C.uchar(response.RelayState)
	status.inputs = C.uchar(response.InputState)

	status.syserror = C.uchar(response.SystemError)
	status.seqno = C.uint(response.SequenceId)
	status.info = C.uchar(response.SpecialInfo)

	if response.Event.IsZero() {
		cstring("", status.event.timestamp, 20)
		status.event.index = C.uint(0)
		status.event.eventType = C.uchar(0)
		status.event.granted = cbool(false)
		status.event.door = C.uchar(0)
		status.event.direction = C.uchar(0)
		status.event.card = C.uint(0)
		status.event.reason = C.uchar(0)
	} else {
		cstring(format(response.Event.Timestamp), status.event.timestamp, 20)
		status.event.index = C.uint(response.Event.Index)
		status.event.eventType = C.uchar(response.Event.Type)
		status.event.granted = cbool(response.Event.Granted)
		status.event.door = C.uchar(response.Event.Door)
		status.event.direction = C.uchar(response.Event.Direction)
		status.event.card = C.uint(response.Event.CardNumber)
		status.event.reason = C.uchar(response.Event.Reason)
	}

	return nil
}

func getTime(uu uhppote.IUHPPOTE, deviceID uint32, datetime *C.char) error {
	if datetime == nil {
		return fmt.Errorf("invalid argument (datetime) - expected valid pointer to char[20]")
	}

	response, err := uu.GetTime(deviceID)
	if err != nil {
		return err
	} else if response == nil {
		return fmt.Errorf("%v: no response to get-time", deviceID)
	}

	cstring(response.DateTime, datetime, 20)

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

func getListener(uu uhppote.IUHPPOTE, controller uint32, address *C.char, interval *uint8) error {
	if address == nil {
		return fmt.Errorf("invalid argument (address) - expected valid pointer to char[22]")
	}

	if _address, _interval, err := uu.GetListener(controller); err != nil {
		return err
	} else if !_address.IsValid() {
		return fmt.Errorf("%v: invalid response to get-listener", controller)
	} else {
		cstring(_address, address, 22)

		if interval != nil {
			*interval = _interval
		}
	}

	return nil
}

func setListener(uu uhppote.IUHPPOTE, controller uint32, listener *C.char, interval uint8) error {
	if listener == nil {
		return fmt.Errorf("invalid argument (listener) - expected valid pointer to string")
	}

	if address, err := netip.ParseAddrPort(C.GoString(listener)); err != nil {
		return err
	} else if ok, err := uu.SetListener(controller, address, interval); err != nil {
		return err
	} else if !ok {
		return fmt.Errorf("failed to set event listener")
	}

	return nil
}

func getDoorControl(uu uhppote.IUHPPOTE, control *C.struct_DoorControl, deviceID uint32, door uint8) error {
	if control == nil {
		return fmt.Errorf("invalid argument (device) - expected valid pointer to DoorControl struct")
	}

	if response, err := uu.GetDoorControlState(deviceID, door); err != nil {
		return err
	} else if response == nil {
		return fmt.Errorf("%v: no response to get-door-control-state", deviceID)
	} else {
		control.mode = C.uchar(response.ControlState)
		control.delay = C.uchar(response.Delay)
	}

	return nil
}

func setDoorControl(uu uhppote.IUHPPOTE, deviceID uint32, door uint8, mode types.ControlState, delay uint8) error {
	if response, err := uu.SetDoorControlState(deviceID, door, mode, delay); err != nil {
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

// Sets the door interlock mode for a controller.
//
// Valid interlock modes are:
// - 0: no interlocks
// - 1: doors 1 and 2 are interlocked
// - 2: doors 3 and 4 are interlocked
// - 3: doors 1 and 2 are interlocked and doors 3 and 4 are interlocked
// - 4: doors 1,2 and 3 are interlocked
// - 8: doors 1,2,3 and 4 are interlocked
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

// Retrieves the anti-passback mode for a controller.
func getAntiPassback(uu uhppote.IUHPPOTE, controller uint32, antipassback *uint8) error {
	if antipassback == nil {
		return fmt.Errorf("invalid argument (antipassback) - expected valid pointer to uint8")
	}

	v, err := uu.GetAntiPassback(controller)
	if err != nil {
		return err
	} else {
		*antipassback = uint8(v)
	}

	return nil
}

// Sets the anti-passback mode for a controller.
//
// Valid anti-passback modes are:
// - 0: anti-passback disabled
// - 1: doors 1 and 2 are anti-passbacked and doors 3 and 4 are anti-passbacked
// - 2: doors 1 and 3 are anti-passbacked with doors 2 and 4
// - 3: door 1 is anti-passbacked with doors 2 and 3
// - 4: door 1 is anti-passbacked with doors 2,3 and 4
func setAntiPassback(uu uhppote.IUHPPOTE, controller uint32, antipassback uint8) error {
	if antipassback != 0 && antipassback != 1 && antipassback != 2 && antipassback != 3 && antipassback != 4 {
		return fmt.Errorf("invalid antipassback value (%v)", antipassback)
	} else if ok, err := uu.SetAntiPassback(controller, types.AntiPassback(antipassback)); err != nil {
		return err
	} else if !ok {
		return fmt.Errorf("%v: failed to set controller anti-passback", controller)
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
