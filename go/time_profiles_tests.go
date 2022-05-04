//go:build tests

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

	if deviceID != 405419896 {
		return fmt.Errorf("Incorrect device ID (%v)", deviceID)
	}

	if profileID != 49 {
		return fmt.Errorf("Incorrect profile ID (%v)", profileID)
	}

	profile.ID = 49
	profile.linked = 71
	profile.from = C.CString("2022-02-01")
	profile.to = C.CString("2022-06-30")

	profile.monday = cbool(true)
	profile.tuesday = cbool(false)
	profile.wednesday = cbool(true)
	profile.thursday = cbool(true)
	profile.friday = cbool(false)
	profile.saturday = cbool(false)
	profile.sunday = cbool(true)

	profile.segment1start = C.CString("08:30")
	profile.segment1end = C.CString("11:30")
	profile.segment2start = C.CString("00:00")
	profile.segment2end = C.CString("00:00")
	profile.segment3start = C.CString("00:00")
	profile.segment3end = C.CString("18:00")

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

	if deviceID != 405419896 {
		return fmt.Errorf("Incorrect device ID (%v)", deviceID)
	}

	if p.ID != 49 {
		return fmt.Errorf("Incorrect profile ID (%v)", p.ID)
	}

	if p.LinkedProfileID != 71 {
		return fmt.Errorf("Incorrect linked profile ID (%v)", p.LinkedProfileID)
	}

	if from := fmt.Sprintf("%v", p.From); from != "2022-02-01" {
		return fmt.Errorf("Incorrect 'from' date (%v)", from)
	}

	if to := fmt.Sprintf("%v", p.To); to != "2022-06-30" {
		return fmt.Errorf("Incorrect 'to' date (%v)", to)
	}

	if !p.Weekdays[time.Monday] {
		return fmt.Errorf("Incorrect Monday value (%v)", p.Weekdays[time.Monday])
	}

	if p.Weekdays[time.Tuesday] {
		return fmt.Errorf("Incorrect Tuesday value (%v)", p.Weekdays[time.Tuesday])
	}

	if !p.Weekdays[time.Wednesday] {
		return fmt.Errorf("Incorrect Wednesday value (%v)", p.Weekdays[time.Wednesday])
	}

	if !p.Weekdays[time.Thursday] {
		return fmt.Errorf("Incorrect Thursday value (%v)", p.Weekdays[time.Thursday])
	}

	if p.Weekdays[time.Friday] {
		return fmt.Errorf("Incorrect Friday value (%v)", p.Weekdays[time.Friday])
	}

	if p.Weekdays[time.Saturday] {
		return fmt.Errorf("Incorrect Saturday value (%v)", p.Weekdays[time.Saturday])
	}

	if !p.Weekdays[time.Sunday] {
		return fmt.Errorf("Incorrect Sunday value (%v)", p.Weekdays[time.Sunday])
	}

	if start := fmt.Sprintf("%v", p.Segments[1].Start); start != "08:30" {
		return fmt.Errorf("Incorrect segment 1 start (%v)", start)
	}

	if end := fmt.Sprintf("%v", p.Segments[1].End); end != "11:30" {
		return fmt.Errorf("Incorrect segment 1 end (%v)", end)
	}

	if start := fmt.Sprintf("%v", p.Segments[2].Start); start != "00:00" {
		return fmt.Errorf("Incorrect segment 2 start (%v)", start)
	}

	if end := fmt.Sprintf("%v", p.Segments[2].End); end != "00:00" {
		return fmt.Errorf("Incorrect segment 2 end (%v)", end)
	}

	if start := fmt.Sprintf("%v", p.Segments[3].Start); start != "00:00" {
		return fmt.Errorf("Incorrect segment 3 start (%v)", start)
	}

	if end := fmt.Sprintf("%v", p.Segments[3].End); end != "18:00" {
		return fmt.Errorf("Incorrect segment 3 end (%v)", end)
	}

	return nil
}

func clearTimeProfiles(uu uhppote.IUHPPOTE, deviceID uint32) error {
	if deviceID != 405419896 {
		return fmt.Errorf("Incorrect device ID (%v)", deviceID)
	}

	return nil
}
