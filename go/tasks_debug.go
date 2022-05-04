//go:build debug

package main

import (
	"C"
	"fmt"
	"time"

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

	if DEBUG {
		fmt.Printf(">>> add-task\n")
		fmt.Printf("    ID:                   %v\n", deviceID)
		fmt.Printf("    task:                 %v\n", t.Task)
		fmt.Printf("    door:                 %v\n", t.Door)
		fmt.Printf("    enabled from:         %v\n", t.From)
		fmt.Printf("            to:           %v\n", t.To)
		fmt.Printf("    enabled on Monday:    %v\n", t.Weekdays[time.Monday])
		fmt.Printf("               Tuesday:   %v\n", t.Weekdays[time.Tuesday])
		fmt.Printf("               Wednesday: %v\n", t.Weekdays[time.Wednesday])
		fmt.Printf("               Thursday:  %v\n", t.Weekdays[time.Thursday])
		fmt.Printf("               Friday:    %v\n", t.Weekdays[time.Friday])
		fmt.Printf("               Saturday:  %v\n", t.Weekdays[time.Saturday])
		fmt.Printf("               Sunday:    %v\n", t.Weekdays[time.Sunday])
		fmt.Printf("   run at:                %v\n", t.Start)
		fmt.Printf("   cards:                 %v\n", t.Cards)
		fmt.Println()
	}

	return nil
}

func refreshTaskList(uu uhppote.IUHPPOTE, deviceID uint32) error {
	if DEBUG {
		fmt.Printf(">>> refresh-tasklist\n")
		fmt.Printf("    ID: %v\n", deviceID)
		fmt.Println()
	}

	return nil
}

func clearTaskList(uu uhppote.IUHPPOTE, deviceID uint32) error {
	if DEBUG {
		fmt.Printf(">>> clear-tasklist\n")
		fmt.Printf("    ID: %v\n", deviceID)
		fmt.Println()
	}

	return nil
}
