//go:build tests

package main

import (
	"C"
	"fmt"
	"time"

	"github.com/uhppoted/uhppote-core/types"
	"github.com/uhppoted/uhppote-core/uhppote"
)

func addTask(uu uhppote.IUHPPOTE, deviceID uint32, task *C.struct_Task) error {
	if task == nil {
		return fmt.Errorf("invalid argument (task) - expected valid pointer")
	}

	t, err := makeTask(task)
	if err != nil {
		return err
	} else if t == nil {
		return fmt.Errorf("invalid task (%v)", t)
	}

	if deviceID != 405419896 {
		return fmt.Errorf("Incorrect device ID (%v)", deviceID)
	}

	if t.Task != types.EnableTimeProfile {
		return fmt.Errorf("Incorrect task type (%v)", t.Task)
	}

	if t.Door != 3 {
		return fmt.Errorf("Incorrect task door (%v)", t.Door)
	}

	if from := fmt.Sprintf("%v", t.From); from != "2022-02-01" {
		return fmt.Errorf("Incorrect 'from' date (%v)", from)
	}

	if to := fmt.Sprintf("%v", t.To); to != "2022-06-30" {
		return fmt.Errorf("Incorrect 'to' date (%v)", to)
	}

	if !t.Weekdays[time.Monday] {
		return fmt.Errorf("Incorrect Monday value (%v)", t.Weekdays[time.Monday])
	}

	if t.Weekdays[time.Tuesday] {
		return fmt.Errorf("Incorrect Tuesday value (%v)", t.Weekdays[time.Tuesday])
	}

	if !t.Weekdays[time.Wednesday] {
		return fmt.Errorf("Incorrect Wednesday value (%v)", t.Weekdays[time.Wednesday])
	}

	if !t.Weekdays[time.Thursday] {
		return fmt.Errorf("Incorrect Thursday value (%v)", t.Weekdays[time.Thursday])
	}

	if t.Weekdays[time.Friday] {
		return fmt.Errorf("Incorrect Friday value (%v)", t.Weekdays[time.Friday])
	}

	if t.Weekdays[time.Saturday] {
		return fmt.Errorf("Incorrect Saturday value (%v)", t.Weekdays[time.Saturday])
	}

	if !t.Weekdays[time.Sunday] {
		return fmt.Errorf("Incorrect Sunday value (%v)", t.Weekdays[time.Sunday])
	}

	if at := fmt.Sprintf("%v", t.Start); at != "09:45" {
		return fmt.Errorf("Incorrect task 'at' time (%v)", at)
	}

	if t.Cards != 11 {
		return fmt.Errorf("Incorrect task cards (%v)", t.Cards)
	}

	return nil
}

func refreshTaskList(uu uhppote.IUHPPOTE, deviceID uint32) error {
	if deviceID != 405419896 {
		return fmt.Errorf("Incorrect device ID (%v)", deviceID)
	}

	return nil
}

func clearTaskList(uu uhppote.IUHPPOTE, deviceID uint32) error {
	if deviceID != 405419896 {
		return fmt.Errorf("Incorrect device ID (%v)", deviceID)
	}

	return nil
}
