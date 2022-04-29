#include <iomanip>
#include <iostream>

#include "../include/uhppoted.hpp"
#include "tests.hpp"

using namespace std;

bool getEventIndex(uhppoted &u) {
    auto index = u.get_event_index(DEVICE_ID);

    vector<result> rs = {
        result("event index", "uint32", {.uint32 = 47}, {.uint32 = index}),
    };

    return evaluate("get-event-index", rs);
}

bool setEventIndex(uhppoted &u) {
    u.set_event_index(DEVICE_ID, EVENT_INDEX);

    vector<result> rs = {};

    return evaluate("set-event-index", rs);
}

bool getEvent(uhppoted &u) {
    auto event = u.get_event(DEVICE_ID, EVENT_INDEX);

    vector<result> rs = {
        result("event index", "uint32", {.uint32 = 51}, {.uint32 = event.index}),
        result("event timestamp", "string", {.string = "2022-04-15 12:29:15"}, {.string = event.timestamp.c_str()}),
        result("event granted", "uint8", {.uint8 = 6}, {.uint8 = event.eventType}),
        result("event granted", "boolean", {.boolean = true}, {.boolean = event.granted}),
        result("event door", "uint8", {.uint8 = 3}, {.uint8 = event.door}),
        result("event direction", "uint8", {.uint8 = 1}, {.uint8 = event.direction}),
        result("event card", "uint32", {.uint32 = 8165538}, {.uint32 = event.card}),
        result("event reason", "uint8", {.uint8 = 21}, {.uint8 = event.reason}),
    };

    return evaluate("get-event-index", rs);
}

bool recordSpecialEvents(uhppoted &u) {
    u.record_special_events(DEVICE_ID, true);

    vector<result> rs = {};

    return evaluate("record-special-events", rs);
}
