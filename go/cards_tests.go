//go:build tests

package main

import (
	"C"
	"fmt"
	"time"
	"unsafe"

	"github.com/uhppoted/uhppote-core/types"
	"github.com/uhppoted/uhppote-core/uhppote"
)

func getCards(uu uhppote.IUHPPOTE, N *C.int, deviceID uint32) error {
	if N == nil {
		return fmt.Errorf("invalid argument (N) - expected valid pointer")
	}

	if deviceID != 405419896 {
		return fmt.Errorf("Incorrect device ID (%v)", deviceID)
	}

	*N = C.int(39)

	return nil
}

func getCard(uu uhppote.IUHPPOTE, card *C.struct_Card, deviceID uint32, cardNumber uint32) error {
	if card == nil {
		return fmt.Errorf("invalid argument (card) - expected valid pointer")
	}

	if deviceID != 405419896 {
		return fmt.Errorf("Incorrect device ID (%v)", deviceID)
	}

	if cardNumber != 8165538 {
		return fmt.Errorf("Incorrect card number (%v)", cardNumber)
	}

	card.card_number = C.uint(8165538)
	cstring("2022-01-01", card.from, 11)
	cstring("2022-12-31", card.to, 11)
	card.PIN = C.uint(7531)

	doors := unsafe.Slice(card.doors, 4)

	doors[0] = 0
	doors[1] = 1
	doors[2] = 31
	doors[3] = 75

	return nil
}

func getCardByIndex(uu uhppote.IUHPPOTE, card *C.struct_Card, deviceID uint32, index uint32) error {
	if card == nil {
		return fmt.Errorf("invalid argument (card) - expected valid pointer")
	}

	if deviceID != 405419896 {
		return fmt.Errorf("Incorrect device ID (%v)", deviceID)
	}

	if index != 19 {
		return fmt.Errorf("Incorrect card index (%v)", index)
	}

	card.card_number = C.uint(8165538)
	cstring("2022-01-01", card.from, 11)
	cstring("2022-12-31", card.to, 11)
	card.PIN = C.uint(7531)

	doors := unsafe.Slice(card.doors, 4)

	doors[0] = 0
	doors[1] = 1
	doors[2] = 31
	doors[3] = 75

	return nil
}

func putCard(uu uhppote.IUHPPOTE, deviceID uint32, cardNumber uint32, from, to *C.char, doors *uint8, PIN uint32) error {
	if deviceID != 405419896 {
		return fmt.Errorf("Incorrect device ID (%v)", deviceID)
	}

	if cardNumber != 8165538 {
		return fmt.Errorf("Incorrect card number (%v)", cardNumber)
	}

	if _from, err := time.Parse("2006-01-02", C.GoString(from)); err != nil {
		return fmt.Errorf("Invalid 'from' date (%v)", err)
	} else if fmt.Sprintf("%v", types.Date(_from)) != "2022-01-01" {
		return fmt.Errorf("Incorrect 'from' date (%v)", from)
	}

	if _to, err := time.Parse("2006-01-02", C.GoString(to)); err != nil {
		return fmt.Errorf("Invalid 'to' date (%v)", err)
	} else if fmt.Sprintf("%v", types.Date(_to)) != "2022-12-31" {
		return fmt.Errorf("Incorrect 'to' date (%v)", to)
	}

	if doors == nil {
		return fmt.Errorf("invalid argument (doors) - expected valid pointer")
	}

	_doors := C.GoBytes(unsafe.Pointer(doors), 4)
	for i := 0; i < 4; i++ {
		if _doors[i] != []uint8{0, 1, 31, 75}[i] {
			return fmt.Errorf("Incorrect doors[%v] (%v)", i+1, _doors[i])
		}
	}

	if PIN != 7531 {
		return fmt.Errorf("Incorrect card PIN (%v)", PIN)
	}

	return nil
}

func deleteCard(uu uhppote.IUHPPOTE, deviceID uint32, cardNumber uint32) error {
	if deviceID != 405419896 {
		return fmt.Errorf("Incorrect device ID (%v)", deviceID)
	}

	if cardNumber != 8165538 {
		return fmt.Errorf("Incorrect card number (%v)", cardNumber)
	}

	return nil
}

func deleteCards(uu uhppote.IUHPPOTE, deviceID uint32) error {
	if deviceID != 405419896 {
		return fmt.Errorf("Incorrect device ID (%v)", deviceID)
	}

	return nil
}
