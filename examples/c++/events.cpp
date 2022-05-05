#include <iostream>

#include "../include/uhppoted.hpp"
#include "examples.hpp"

using namespace std;

extern const uint32_t DEVICE_ID;
extern const uint32_t CARD_NUMBER;
extern const uint32_t CARD_INDEX;
extern const uint8_t DOOR;
extern const uint32_t EVENT_INDEX;

void getEventIndex(uhppoted &u, int argc, char **argv) {
    uint32_t deviceID = DEVICE_ID;

    uint32_t index = u.get_event_index(deviceID);

    vector<field> fields = {
        field("ID", deviceID),
        field("index", index),
    };

    display("get-event-index", fields);
}

void setEventIndex(uhppoted &u, int argc, char **argv) {
    uint32_t deviceID = DEVICE_ID;
    uint32_t index = EVENT_INDEX;

    vector<field> fields = {
        field("ID", deviceID),
        field("index", index),
    };

    display("set-event-index", fields);
}

void getEvent(uhppoted &u, int argc, char **argv) {
    uint32_t deviceID = DEVICE_ID;
    uint32_t index = EVENT_INDEX;

    event e = u.get_event(deviceID, index);

    vector<field> fields = {
        field("ID", deviceID),
        field("event index:", e.index),
        field("      timestamp", e.timestamp),
        field("      type", e.eventType),
        field("      granted", e.granted),
        field("      direction", e.direction),
        field("      card number", e.card),
        field("      reason", e.reason),
    };

    display("get-event", fields);
}

void recordSpecialEvents(uhppoted &u, int argc, char **argv) {
    uint32_t deviceID = DEVICE_ID;
    bool enabled = true;

    u.record_special_events(deviceID, enabled);

    vector<field> fields = {
        field("ID", deviceID),
        field("enabled", enabled),
    };

    display("record-special-events", fields);
}
