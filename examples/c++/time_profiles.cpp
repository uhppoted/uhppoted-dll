#include <iostream>

#include "../include/uhppoted.hpp"
#include "examples.hpp"

using namespace std;

extern const uint32_t DEVICE_ID;
extern const uint8_t PROFILE_ID;

void getTimeProfile(uhppoted &u, int argc, char **argv) {
    uint32_t deviceID = DEVICE_ID;
    uint32_t profileID = PROFILE_ID;

    time_profile profile = u.get_time_profile(deviceID, profileID);

    vector<field> fields = {
        field("ID", deviceID),
        field("profile ID", profile.ID),
        field("linked profile", profile.linked),
        field("enabled from", profile.from),
        field("        to", profile.to),
        field("enabled on Monday", profile.monday),
        field("           Tuesday", profile.tuesday),
        field("           Wednesday", profile.wednesday),
        field("           Thursday", profile.thursday),
        field("           Friday", profile.friday),
        field("           Saturday", profile.saturday),
        field("           Sunday", profile.sunday),
        field("segment 1 start", profile.segment1start),
        field("          end", profile.segment1end),
        field("segment 2 start", profile.segment2start),
        field("          end", profile.segment2end),
        field("segment 3 start", profile.segment3start),
        field("          end", profile.segment3end),
    };

    display("get-time-profile", fields);
}

void setTimeProfile(uhppoted &u, int argc, char **argv) {
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

    u.set_time_profile(deviceID, profile);

    vector<field> fields = {
        field("ID", deviceID),
        field("profile ID", profile.ID),
        field("linked profile", profile.linked),
        field("enabled from", profile.from),
        field("        to", profile.to),
        field("enabled on Monday", profile.monday),
        field("           Tuesday", profile.tuesday),
        field("           Wednesday", profile.wednesday),
        field("           Thursday", profile.thursday),
        field("           Friday", profile.friday),
        field("           Saturday", profile.saturday),
        field("           Sunday", profile.sunday),
        field("segment 1 start", profile.segment1start),
        field("          end", profile.segment1end),
        field("segment 2 start", profile.segment2start),
        field("          end", profile.segment2end),
        field("segment 3 start", profile.segment3start),
        field("          end", profile.segment3end),
    };

    display("set-time-profile", fields);
}

void clearTimeProfiles(uhppoted &u, int argc, char **argv) {
    uint32_t deviceID = DEVICE_ID;

    u.clear_time_profiles(deviceID);

    vector<field> fields = {
        field("ID", deviceID),
    };

    display("clear-time-profiles", fields);
}
