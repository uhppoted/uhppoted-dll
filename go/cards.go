//go:build !debug && !tests

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

	cards, err := uu.GetCards(deviceID)
	if err != nil {
		return err
	}

	*N = C.int(cards)

	return nil
}

func getCard(uu uhppote.IUHPPOTE, card *C.struct_Card, deviceID uint32, cardNumber uint32) error {
	if card == nil {
		return fmt.Errorf("invalid argument (card) - expected valid pointer")
	}

	c, err := uu.GetCardByID(deviceID, cardNumber)
	if err != nil {
		return err
	} else if c == nil {
		return fmt.Errorf("%v: no response to get-card %v", deviceID, cardNumber)
	}

	card.card_number = C.uint(c.CardNumber)
	card.from = C.CString(fmt.Sprintf("%v", c.From))
	card.to = C.CString(fmt.Sprintf("%v", c.To))
	card.PIN = C.uint(c.PIN)

	doors := unsafe.Slice(card.doors, 4)

	doors[0] = C.uchar(c.Doors[1])
	doors[1] = C.uchar(c.Doors[2])
	doors[2] = C.uchar(c.Doors[3])
	doors[3] = C.uchar(c.Doors[4])

	return nil
}

func getCardByIndex(uu uhppote.IUHPPOTE, card *C.struct_Card, deviceID uint32, index uint32) error {
	if card == nil {
		return fmt.Errorf("invalid argument (card) - expected valid pointer")
	}

	c, err := uu.GetCardByIndex(deviceID, index)
	if err != nil {
		return err
	} else if c == nil {
		return fmt.Errorf("%v: no card found at index %v", deviceID, index)
	}

	card.card_number = C.uint(c.CardNumber)
	card.from = C.CString(fmt.Sprintf("%v", c.From))
	card.to = C.CString(fmt.Sprintf("%v", c.To))
	card.PIN = C.uint(c.PIN)

	doors := unsafe.Slice(card.doors, 4)

	doors[0] = C.uchar(c.Doors[1])
	doors[1] = C.uchar(c.Doors[2])
	doors[2] = C.uchar(c.Doors[3])
	doors[3] = C.uchar(c.Doors[4])

	return nil
}

func putCard(uu uhppote.IUHPPOTE, deviceID uint32, cardNumber uint32, from, to *C.char, doors *uint8, PIN uint32) error {
	_from, err := time.Parse("2006-01-02", C.GoString(from))
	if err != nil {
		return fmt.Errorf("Invalid 'from' date (%v)", err)
	}

	_to, err := time.Parse("2006-01-02", C.GoString(to))
	if err != nil {
		return fmt.Errorf("Invalid 'to' date (%v)", err)
	}

	if doors == nil {
		return fmt.Errorf("invalid argument (doors) - expected valid pointer")
	}

	if PIN > 999999 {
		return fmt.Errorf("Invalid PIN code (%v)", PIN)
	}

	_doors := C.GoBytes(unsafe.Pointer(doors), 4)

	card := types.Card{
		CardNumber: cardNumber,
		From:       (*types.Date)(&_from),
		To:         (*types.Date)(&_to),
		Doors: map[uint8]uint8{
			1: _doors[0],
			2: _doors[1],
			3: _doors[2],
			4: _doors[3],
		},
		PIN: types.PIN(PIN),
	}

	ok, err := uu.PutCard(deviceID, card)
	if err != nil {
		return err
	} else if !ok {
		return fmt.Errorf("%v: put-card failed", deviceID)
	}

	return nil
}

func deleteCard(uu uhppote.IUHPPOTE, deviceID uint32, cardNumber uint32) error {
	deleted, err := uu.DeleteCard(deviceID, cardNumber)
	if err != nil {
		return err
	} else if !deleted {
		return fmt.Errorf("%v: delete-card %v failed", deviceID, cardNumber)
	}

	return nil
}

func deleteCards(uu uhppote.IUHPPOTE, deviceID uint32) error {
	deleted, err := uu.DeleteCards(deviceID)
	if err != nil {
		return err
	} else if !deleted {
		return fmt.Errorf("%v: delete-cards failed", deviceID)
	}

	return nil
}
