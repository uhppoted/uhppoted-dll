//go:build !debug && !tests

package main

import (
	"C"
	"fmt"

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

	if ok, err := uu.AddTask(deviceID, *t); err != nil {
		return err
	} else if !ok {
		return fmt.Errorf("%v: add-task failed for %v", deviceID, t.Task)
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

func clearTaskList(uu uhppote.IUHPPOTE, deviceID uint32) error {
	ok, err := uu.ClearTaskList(deviceID)
	if err != nil {
		return err
	} else if !ok {
		return fmt.Errorf("%v: clear-tasklist failed", deviceID)
	}

	return nil
}
