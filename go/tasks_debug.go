//go:build debug

package main

import (
	"C"
	"fmt"
	"time"

	"github.com/uhppoted/uhppote-core/types"
	"github.com/uhppoted/uhppote-core/uhppote"
)

func addTask(uu uhppote.IUHPPOTE, deviceID uint32, task types.Task) error {
	if DEBUG {
		fmt.Printf(">>> add-task\n")
		fmt.Printf("    ID:                   %v\n", deviceID)
		fmt.Printf("    task:                 %v\n", task.Task)
		fmt.Printf("    door:                 %v\n", task.Door)
		fmt.Printf("    enabled from:         %v\n", task.From)
		fmt.Printf("            to:           %v\n", task.To)
		fmt.Printf("    enabled on Monday:    %v\n", task.Weekdays[time.Monday])
		fmt.Printf("               Tuesday:   %v\n", task.Weekdays[time.Tuesday])
		fmt.Printf("               Wednesday: %v\n", task.Weekdays[time.Wednesday])
		fmt.Printf("               Thursday:  %v\n", task.Weekdays[time.Thursday])
		fmt.Printf("               Friday:    %v\n", task.Weekdays[time.Friday])
		fmt.Printf("               Saturday:  %v\n", task.Weekdays[time.Saturday])
		fmt.Printf("               Sunday:    %v\n", task.Weekdays[time.Sunday])
		fmt.Printf("   run at:                %v\n", task.Start)
		fmt.Printf("   cards:                 %v\n", task.Cards)
		fmt.Println()
	}

	return nil
}

func refreshTaskList(uu uhppote.IUHPPOTE, deviceID uint32) error {
	if DEBUG {
		fmt.Printf(">>> refresh-tasklist\n")
		fmt.Printf("    ID:                   %v\n", deviceID)
		fmt.Println()
	}

	return nil
}
