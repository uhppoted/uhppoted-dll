#include <chrono>
#include <iomanip>
#include <iostream>
#include <thread>

#include "../include/uhppoted.hpp"
#include "tests.hpp"

using namespace std;
using namespace std::this_thread;

bool getEventIndex(uhppoted &u) {
    auto index = u.get_event_index(DEVICE_ID);

    vector<result> rs = {
        result("event index", uint32_t(47), index),
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
        result("event index", uint32_t(51), event.index),
        result("event timestamp", string("2022-04-15 12:29:15"), event.timestamp),
        result("event type", uint8_t(6), event.eventType),
        result("event granted", true, event.granted),
        result("event door", uint8_t(3), event.door),
        result("event direction", uint8_t(1), event.direction),
        result("event card", uint32_t(8165538), event.card),
        result("event reason", uint8_t(21), event.reason),
    };

    return evaluate("get-event-index", rs);
}

bool recordSpecialEvents(uhppoted &u) {
    u.record_special_events(DEVICE_ID, true);

    vector<result> rs = {};

    return evaluate("record-special-events", rs);
}

bool listen(uhppoted &u) {
    bool listening = false;
    bool stop = false;
    vector<result> rs;

    on_event callback = +[](const struct ListenEvent e, void *userdata) {
        vector<result> *rs = (vector<result> *)userdata;

        *rs = {
            result("event controller", uint32_t(405419896), e.controller),
            result("event index", uint32_t(17), e.index),
            result("event timestamp", string("2024-07-05 12:36:45"), string(e.timestamp)),
            result("event type", uint8_t(6), e.event),
            result("event granted", true, e.granted),
            result("event door", uint8_t(2), e.door),
            result("event direction", uint8_t(1), e.direction),
            result("event card", uint32_t(10058400), e.card),
            result("event reason", uint8_t(21), e.reason),
        };
    };

    u.listen(callback, &listening, &stop, nullptr, &rs);

    sleep_for(chrono::milliseconds(100));
    for (int count = 0; count < 5 && !listening; count++) {
        cout << " ... waiting " << count << " " << (listening ? "listening" : "pending") << endl;
        sleep_for(chrono::milliseconds(500));
    }

    if (!listening) {
        throw uhppoted_exception((char *)"failed to start event listener");
    }

    sleep_for(chrono::milliseconds(1000));

    stop = true;
    for (int count = 0; count < 5 && listening; count++) {
        cout << " ... stoppping event listener " << count << " " << (listening ? "listening" : "stopped") << endl;
        sleep_for(chrono::milliseconds(100));
    }

    if (listening) {
        throw uhppoted_exception((char *)"failed to stop event listener");
    }

    return evaluate("listen", rs);
}
