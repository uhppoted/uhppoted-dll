#include <iostream>

#include "../include/uhppoted.hpp"

using namespace std;

extern const uint32_t DEVICE_ID;
extern const uint32_t CARD_NUMBER;
extern const uint32_t CARD_INDEX;
extern const uint8_t DOOR;
extern const uint32_t EVENT_INDEX;

void getEventIndex(uhppoted &u, int argc, char **argv) {
    string tag = "get-event-index";
    uint32_t deviceID = DEVICE_ID;

    uint32_t index = u.get_event_index(deviceID);

    cout << endl
         << tag << endl;
    cout << "  ID:    " << deviceID << endl;
    cout << "  index: " << index << endl;
    cout << endl;
}

void setEventIndex(uhppoted &u, int argc, char **argv) {
    string tag = "set-event-index";
    uint32_t deviceID = DEVICE_ID;
    uint32_t index = EVENT_INDEX;

    u.set_event_index(deviceID, index);

    cout << endl
         << tag << endl;
    cout << "  ID:    " << deviceID << endl;
    cout << "  index: " << index << endl;
    cout << endl;
}

void getEvent(uhppoted &u, int argc, char **argv) {
    uint32_t deviceID = DEVICE_ID;
    uint32_t index = EVENT_INDEX;

    event e = u.get_event(deviceID, index);

    cout << endl
         << "get-event" << endl;
    cout << "  ID:                " << deviceID << endl;
    cout << "  event index:       " << e.index << endl;
    cout << "        timestamp:   " << e.timestamp << endl;
    cout << "        type:        " << static_cast<int>(e.eventType) << endl;
    cout << "        granted:     " << static_cast<int>(e.granted) << endl;
    cout << "        direction:   " << static_cast<int>(e.direction) << endl;
    cout << "        card number: " << static_cast<int>(e.card) << endl;
    cout << "        reason:      " << static_cast<int>(e.reason) << endl;
    cout << endl;
}

void recordSpecialEvents(uhppoted &u, int argc, char **argv) {
    string tag = "record-special-events";
    uint32_t deviceID = DEVICE_ID;
    uint32_t enabled = true;

    u.record_special_events(deviceID, enabled);

    cout << endl
         << tag << endl;
    cout << "  ID:      " << deviceID << endl;
    cout << "  enabled: " << enabled << endl;
    cout << endl;
}
