//go:build tests

package main

import (
	"C"
	"fmt"
	"time"

	"github.com/uhppoted/uhppote-core/types"
	"github.com/uhppoted/uhppote-core/uhppote"
)

func addTask(uu uhppote.IUHPPOTE, deviceID uint32, task types.Task) error {
	if deviceID != 405419896 {
		return fmt.Errorf("Incorrect device ID (%v)", deviceID)
	}

	if task.Task != types.EnableTimeProfile {
		return fmt.Errorf("Incorrect task type (%v)", task.Task)
	}

	if task.Door != 3 {
		return fmt.Errorf("Incorrect task door (%v)", task.Door)
	}

	if from := fmt.Sprintf("%v", task.From); from != "2022-02-01" {
		return fmt.Errorf("Incorrect 'from' date (%v)", from)
	}

	if to := fmt.Sprintf("%v", task.To); to != "2022-06-30" {
		return fmt.Errorf("Incorrect 'to' date (%v)", to)
	}

	if !task.Weekdays[time.Monday] {
		return fmt.Errorf("Incorrect Monday value (%v)", task.Weekdays[time.Monday])
	}

	if task.Weekdays[time.Tuesday] {
		return fmt.Errorf("Incorrect Tuesday value (%v)", task.Weekdays[time.Tuesday])
	}

	if !task.Weekdays[time.Wednesday] {
		return fmt.Errorf("Incorrect Wednesday value (%v)", task.Weekdays[time.Wednesday])
	}

	if !task.Weekdays[time.Thursday] {
		return fmt.Errorf("Incorrect Thursday value (%v)", task.Weekdays[time.Thursday])
	}

	if task.Weekdays[time.Friday] {
		return fmt.Errorf("Incorrect Friday value (%v)", task.Weekdays[time.Friday])
	}

	if task.Weekdays[time.Saturday] {
		return fmt.Errorf("Incorrect Saturday value (%v)", task.Weekdays[time.Saturday])
	}

	if !task.Weekdays[time.Sunday] {
		return fmt.Errorf("Incorrect Sunday value (%v)", task.Weekdays[time.Sunday])
	}

	if at := fmt.Sprintf("%v", task.Start); at != "09:45" {
		return fmt.Errorf("Incorrect task 'at' time (%v)", at)
	}

	if task.Cards != 11 {
		return fmt.Errorf("Incorrect task cards (%v)", task.Cards)
	}

	return nil
}
