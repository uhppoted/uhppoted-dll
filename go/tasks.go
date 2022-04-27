//go:build !debug && !tests

package main

import (
	"C"
	"fmt"

	"github.com/uhppoted/uhppote-core/types"
	"github.com/uhppoted/uhppote-core/uhppote"
)

func addTask(uu uhppote.IUHPPOTE, deviceID uint32, task types.Task) error {
	ok, err := uu.AddTask(deviceID, task)
	if err != nil {
		return err
	} else if !ok {
		return fmt.Errorf("%v: add-task failed for %v", deviceID, task.Task)
	}

	return nil
}

func refreshTaskList(uu uhppote.IUHPPOTE, deviceID uint32) error {
	ok, err := uu.RefreshTaskList(deviceID)
	if err != nil {
		return err
	} else if !ok {
		return fmt.Errorf("%v: refresh-tasklist failed", deviceID)
	}

	return nil
}
