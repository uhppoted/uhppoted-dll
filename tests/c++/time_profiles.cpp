#include <iomanip>
#include <iostream>

#include "../include/uhppoted.hpp"
#include "tests.hpp"

using namespace std;

bool getTimeProfile(uhppoted &u) {
    auto profile = u.get_time_profile(DEVICE_ID, PROFILE_ID);

    vector<result> rs = {
        result("profile ID", "uint8", {.uint8 = 49}, {.uint8 = profile.ID}),
        result("linked profile", "uint8", {.uint8 = 71}, {.uint8 = profile.linked}),
        result("profile 'from' date", "string", {.string = "2022-02-01"}, {.string = profile.from.c_str()}),
        result("profile 'to' date", "string", {.string = "2022-06-30"}, {.string = profile.to.c_str()}),
        result("profile Monday", "boolean", {.boolean = true}, {.boolean = profile.monday}),
        result("profile Tuesday", "boolean", {.boolean = false}, {.boolean = profile.tuesday}),
        result("profile Wednesday", "boolean", {.boolean = true}, {.boolean = profile.wednesday}),
        result("profile Thursday", "boolean", {.boolean = true}, {.boolean = profile.thursday}),
        result("profile Friday", "boolean", {.boolean = false}, {.boolean = profile.friday}),
        result("profile Saturday", "boolean", {.boolean = false}, {.boolean = profile.saturday}),
        result("profile Sunday", "boolean", {.boolean = true}, {.boolean = profile.sunday}),
        result("profile segment 1 start", "string", {.string = "08:30"}, {.string = profile.segment1start.c_str()}),
        result("profile segment 1 end", "string", {.string = "11:30"}, {.string = profile.segment1end.c_str()}),
        result("profile segment 2 start", "string", {.string = "00:00"}, {.string = profile.segment2start.c_str()}),
        result("profile segment 2 end", "string", {.string = "00:00"}, {.string = profile.segment2end.c_str()}),
        result("profile segment 3 start", "string", {.string = "00:00"}, {.string = profile.segment3start.c_str()}),
        result("profile segment 3 end", "string", {.string = "18:00"}, {.string = profile.segment3end.c_str()}),
    };

    return evaluate("get-time-profile", rs);
}

bool setTimeProfile(uhppoted &u) {
    time_profile profile = {
        .ID = PROFILE_ID,
        .linked = 71,
        .from = "2022-02-01",
        .to = "2022-06-30",
        .monday = true,
        .tuesday = false,
        .wednesday = true,
        .thursday = true,
        .friday = false,
        .saturday = false,
        .sunday = true,
        .segment1start = "08:30",
        .segment1end = "11:30",
        .segment2start = "",
        .segment2end = "",
        .segment3start = "",
        .segment3end = "18:00",
    };

    u.set_time_profile(DEVICE_ID, profile);

    vector<result> rs = {};

    return evaluate("set-time-profile", rs);
}

bool clearTimeProfiles(uhppoted &u) {
    u.clear_time_profiles(DEVICE_ID);

    vector<result> rs = {};

    return evaluate("clear-time-profiles", rs);
}
