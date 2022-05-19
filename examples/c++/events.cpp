#include <iostream>

#include "../include/uhppoted.hpp"
#include "examples.hpp"

using namespace std;

void getEventIndex(uhppoted &u, int argc, char **argv) {
    auto options = parse(argc, argv);
    uint32_t deviceID = options.device_id;

    uint32_t index = u.get_event_index(deviceID);

    vector<field> fields = {
        field("ID", deviceID),
        field("index", index),
    };

    display("get-event-index", fields);
}

void setEventIndex(uhppoted &u, int argc, char **argv) {
    auto options = parse(argc, argv);
    uint32_t deviceID = options.device_id;
    uint32_t index = options.event_index;

    vector<field> fields = {
        field("ID", deviceID),
        field("index", index),
    };

    display("set-event-index", fields);
}

void getEvent(uhppoted &u, int argc, char **argv) {
    auto options = parse(argc, argv);
    uint32_t deviceID = options.device_id;
    uint32_t index = options.event_index;

    event e = u.get_event(deviceID, index);

    vector<field> fields = {
        field("ID", deviceID),
        field("event index:", e.index),
        field("      timestamp", e.timestamp),
        field("      type", u.lookup(LOOKUP_EVENT_TYPE, e.eventType, LOCALE)),
        field("      granted", e.granted),
        field("      direction", u.lookup(LOOKUP_DIRECTION, e.direction, LOCALE)),
        field("      card number", e.card),
        field("      reason", u.lookup(LOOKUP_EVENT_REASON, e.reason, LOCALE)),
    };

    display("get-event", fields);
}

void recordSpecialEvents(uhppoted &u, int argc, char **argv) {
    auto options = parse(argc, argv);
    uint32_t deviceID = options.device_id;
    bool enabled = true;

    u.record_special_events(deviceID, enabled);

    vector<field> fields = {
        field("ID", deviceID),
        field("enabled", enabled),
    };

    display("record-special-events", fields);
}
