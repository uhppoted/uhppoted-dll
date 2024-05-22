//go:build debug

package main

import (
	"C"
	"fmt"
	"time"

	"github.com/uhppoted/uhppote-core/uhppote"
)

func getTimeProfile(uu uhppote.IUHPPOTE, profile *C.struct_TimeProfile, deviceID uint32, profileID uint8) error {
	if profile == nil {
		return fmt.Errorf("invalid argument (profile) - expected valid pointer")
	}

	if DEBUG {
		fmt.Printf(">>> get-time-profile\n")
		fmt.Printf("    ID:      %v\n", deviceID)
		fmt.Printf("    profile: %v\n", profileID)
		fmt.Println()
	}

	profile.ID = 29
	profile.linked = 71
	cstring("2022-02-01", profile.from, 11)
	cstring("2022-06-30", profile.to, 11)

	profile.monday = cbool(true)
	profile.tuesday = cbool(false)
	profile.wednesday = cbool(true)
	profile.thursday = cbool(true)
	profile.friday = cbool(false)
	profile.saturday = cbool(false)
	profile.sunday = cbool(true)

	cstring("08:30", profile.segment1start, 6)
	cstring("11:30", profile.segment1end, 6)
	cstring("00:00", profile.segment2start, 6)
	cstring("00:00", profile.segment2end, 6)
	cstring("00:00", profile.segment3start, 6)
	cstring("18:00", profile.segment3end, 6)

	return nil
}

func setTimeProfile(uu uhppote.IUHPPOTE, deviceID uint32, profile *C.struct_TimeProfile) error {
	if profile == nil {
		return fmt.Errorf("invalid argument (profile) - expected valid pointer")
	}

	p, err := makeTimeProfile(profile)
	if err != nil {
		return err
	} else if p == nil {
		return fmt.Errorf("invalid time profile (%v)", p)
	}

	if DEBUG {
		fmt.Printf(">>> set-time-profile\n")
		fmt.Printf("    ID:                   %v\n", deviceID)
		fmt.Printf("    profile ID:           %v\n", p.ID)
		fmt.Printf("    enabled    from:      %v\n", p.From)
		fmt.Printf("               to:        %v\n", p.To)
		fmt.Printf("    enabled on Monday:    %v\n", p.Weekdays[time.Monday])
		fmt.Printf("               Tuesday:   %v\n", p.Weekdays[time.Tuesday])
		fmt.Printf("               Wednesday: %v\n", p.Weekdays[time.Wednesday])
		fmt.Printf("               Thursday:  %v\n", p.Weekdays[time.Thursday])
		fmt.Printf("               Friday:    %v\n", p.Weekdays[time.Friday])
		fmt.Printf("               Saturday:  %v\n", p.Weekdays[time.Saturday])
		fmt.Printf("               Sunday:    %v\n", p.Weekdays[time.Sunday])
		fmt.Printf("    segment 1:            %v-%v\n", p.Segments[1].Start, p.Segments[1].End)
		fmt.Printf("    segment 2:            %v-%v\n", p.Segments[2].Start, p.Segments[2].End)
		fmt.Printf("    segment 3:            %v-%v\n", p.Segments[3].Start, p.Segments[3].End)
		fmt.Println()
	}

	return nil
}

func clearTimeProfiles(uu uhppote.IUHPPOTE, deviceID uint32) error {
	if DEBUG {
		fmt.Printf(">>> clear-time-profiles\n")
		fmt.Printf("    ID: %v\n", deviceID)
		fmt.Println()
	}

	return nil
}
