#include <chrono>
#include <condition_variable>
#include <csignal>
#include <iostream>
#include <thread>

#include "../include/uhppoted.hpp"
#include "examples.hpp"

using namespace std;
using namespace std::this_thread;
using namespace std::chrono;

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

std::condition_variable sigint;
std::mutex guard;

void on_sigint(int signal) {
    std::lock_guard<std::mutex> lock(guard);

    sigint.notify_all();
}

void listenEvents(uhppoted &u, int argc, char **argv) {
    bool running = false;
    bool stop = false;

    static struct {
        uhppoted &u;
    } q = {
        .u = u,
    };

    // NTS: '+' converts lambda to function pointer apparently. Ye gods :-(
    //      Ref. https://stackoverflow.com/questions/28746744/passing-capturing-lambda-as-function-pointer/28746827#28746827
    //      Ref. https://stackoverflow.com/questions/28746744/passing-capturing-lambda-as-function-pointer
    //      Ref. https://en.cppreference.com/w/cpp/language/lambda
    on_event callback = +[](const struct ListenEvent evt) {
        string _event = q.u.lookup(LOOKUP_EVENT_TYPE, evt.event, LOCALE);
        string _direction = q.u.lookup(LOOKUP_DIRECTION, evt.direction, LOCALE);
        string _reason = q.u.lookup(LOOKUP_EVENT_REASON, evt.reason, LOCALE);
        string _timestamp = "-";

        if ((evt.timestamp != nullptr) && (strcmp(evt.timestamp, "") != 0)) {
            _timestamp = evt.timestamp;
        }

        vector<field> fields = {
            field("controller", evt.controller),
            field("timestamp", _timestamp),
            field("index", evt.index),
            field("type", _event),
            field("granted", evt.granted),
            field("door", evt.door),
            field("direction", _direction),
            field("card", evt.card),
            field("reason", _reason),
        };

        display("event", fields);
    };

    on_error on_err = +[](const char *err) {
        cout << " **** ERROR " << err << endl;
    };

    u.listen(callback, &running, &stop, on_err);

    sleep_for(milliseconds(1000));
    for (int count = 0; count < 5 && !running; count++) {
        cout << " ... waiting " << count << " " << (running ? "running" : "pending") << endl;
        sleep_for(milliseconds(1000));
    }

    if (!running) {
        throw uhppoted_exception((char *)"failed to start event listener");
    }

    std::signal(SIGINT, on_sigint);
    std::unique_lock<std::mutex> lock(guard);

    sigint.wait(lock);

    stop = true;
    sleep_for(milliseconds(1000));
    for (int count = 0; count < 5 && running; count++) {
        cout << " ... stoppping event listener " << count << " " << (running ? "running" : "stopped") << endl;
        sleep_for(milliseconds(1000));
    }

    if (running) {
        throw uhppoted_exception((char *)"failed to stop event listener");
    }
}
