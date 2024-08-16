//go:build !debug && !tests

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

	p, err := uu.GetTimeProfile(deviceID, profileID)
	if err != nil {
		return err
	} else if p == nil {
		return fmt.Errorf("%v: no response to get-time-profile %v", deviceID, profileID)
	}

	profile.ID = C.uchar(p.ID)
	profile.linked = C.uchar(p.LinkedProfileID)
	cstring(p.From, profile.from, 11)
	cstring(p.To, profile.to, 11)

	profile.monday = cbool(p.Weekdays[time.Monday])
	profile.tuesday = cbool(p.Weekdays[time.Tuesday])
	profile.wednesday = cbool(p.Weekdays[time.Wednesday])
	profile.thursday = cbool(p.Weekdays[time.Thursday])
	profile.friday = cbool(p.Weekdays[time.Friday])
	profile.saturday = cbool(p.Weekdays[time.Saturday])
	profile.sunday = cbool(p.Weekdays[time.Sunday])

	cstring(p.Segments[1].Start, profile.segment1start, 6)
	cstring(p.Segments[1].End, profile.segment1end, 6)
	cstring(p.Segments[2].Start, profile.segment2start, 6)
	cstring(p.Segments[2].End, profile.segment2end, 6)
	cstring(p.Segments[3].Start, profile.segment3start, 6)
	cstring(p.Segments[3].End, profile.segment3end, 6)

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

	if ok, err := uu.SetTimeProfile(deviceID, *p); err != nil {
		return err
	} else if !ok {
		return fmt.Errorf("%v: set-time-profile failed for %v", deviceID, profile.ID)
	}

	return nil
}

func clearTimeProfiles(uu uhppote.IUHPPOTE, deviceID uint32) error {
	cleared, err := uu.ClearTimeProfiles(deviceID)
	if err != nil {
		return err
	} else if !cleared {
		return fmt.Errorf("%v: clear-time-profiles failed", deviceID)
	}

	return nil
}
