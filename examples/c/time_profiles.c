#include <stdio.h>
#include <stdlib.h>

#include "examples.h"
#include "uhppoted.h"

extern uint32_t DEVICE_ID;
extern uint8_t PROFILE_ID;

int getTimeProfile(int argc, char **argv) {
    uint32_t deviceID = DEVICE_ID;
    uint8_t profileID = PROFILE_ID;
    time_profile profile;

    if (get_time_profile(deviceID, profileID, &profile) < 0) {
        printf("ERROR %s\n", errmsg());
        return -1;
    }

    field fields[] = {
        {.field = "ID", .type = "uint32", .value.uint32 = deviceID},
        {.field = "profile", .type = "uint8", .value.uint8 = profile.ID},
        {.field = "linked profile", .type = "uint8", .value.uint8 = profile.linked},
        {.field = "enabled from", .type = "string", .value.string = profile.from},
        {.field = "        to", .type = "string", .value.string = profile.to},
        {.field = "enabled on Monday", .type = "bool", .value.boolean = profile.monday},
        {.field = "           Tuesday", .type = "bool", .value.boolean = profile.tuesday},
        {.field = "           Wednesday", .type = "bool", .value.boolean = profile.wednesday},
        {.field = "           Thursday", .type = "bool", .value.boolean = profile.thursday},
        {.field = "           Friday", .type = "bool", .value.boolean = profile.friday},
        {.field = "           Saturday", .type = "bool", .value.boolean = profile.saturday},
        {.field = "           Sunday", .type = "bool", .value.boolean = profile.sunday},
        {.field = "segment 1 start", .type = "string", .value.string = profile.segment1start},
        {.field = "          end", .type = "string", .value.string = profile.segment1end},
        {.field = "segment 2 start", .type = "string", .value.string = profile.segment2start},
        {.field = "          end", .type = "string", .value.string = profile.segment2end},
        {.field = "segment 3 start", .type = "string", .value.string = profile.segment3start},
        {.field = "          end", .type = "string", .value.string = profile.segment3end},
    };

    display("get-time-profile", sizeof(fields) / sizeof(field), fields);

    return 0;
}

int setTimeProfile(int argc, char **argv) {
    uint32_t deviceID = DEVICE_ID;
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

    if (set_time_profile(deviceID, &profile) < 0) {
        printf("ERROR %s\n", errmsg());
        return -1;
    }

    field fields[] = {
        {.field = "ID", .type = "uint32", .value.uint32 = deviceID},
        {.field = "profile", .type = "uint8", .value.uint8 = profile.ID},
        {.field = "linked profile", .type = "uint8", .value.uint8 = profile.linked},
        {.field = "enabled from", .type = "string", .value.string = profile.from},
        {.field = "        to", .type = "string", .value.string = profile.to},
        {.field = "enabled on Monday", .type = "bool", .value.boolean = profile.monday},
        {.field = "           Tuesday", .type = "bool", .value.boolean = profile.tuesday},
        {.field = "           Wednesday", .type = "bool", .value.boolean = profile.wednesday},
        {.field = "           Thursday", .type = "bool", .value.boolean = profile.thursday},
        {.field = "           Friday", .type = "bool", .value.boolean = profile.friday},
        {.field = "           Saturday", .type = "bool", .value.boolean = profile.saturday},
        {.field = "           Sunday", .type = "bool", .value.boolean = profile.sunday},
        {.field = "segment 1 start", .type = "string", .value.string = profile.segment1start},
        {.field = "          end", .type = "string", .value.string = profile.segment1end},
        {.field = "segment 2 start", .type = "string", .value.string = profile.segment2start},
        {.field = "          end", .type = "string", .value.string = profile.segment2end},
        {.field = "segment 3 start", .type = "string", .value.string = profile.segment3start},
        {.field = "          end", .type = "string", .value.string = profile.segment3end},
    };

    display("set-time-profile", sizeof(fields) / sizeof(field), fields);

    return 0;
}

int clearTimeProfiles(int argc, char **argv) {
    uint32_t deviceID = DEVICE_ID;

    if (clear_time_profiles(deviceID) < 0) {
        printf("ERROR %s\n", errmsg());
        return -1;
    }

    field fields[] = {
        {.field = "ID", .type = "uint32", .value.uint32 = deviceID},
    };

    display("clear-time-profiles", sizeof(fields) / sizeof(field), fields);

    return 0;
}
