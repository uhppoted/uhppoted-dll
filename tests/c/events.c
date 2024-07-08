#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

#include "tests.h"
#include "uhppoted.h"

void listen_on_event(const struct ListenEvent evt);
void listen_on_error(const char *err);

bool getEventIndex() {
    uint32_t index;

    if (get_event_index(DEVICE_ID, &index) < 0) {
        printf("ERROR %s\n", errmsg());
        return false;
    }

    const result resultset[] = {
        {
            .field = "event index",
            .type = "uint32",
            .value.uint32.expected = 47,
            .value.uint32.value = index,
        },
    };

    return evaluate("get-event-index", sizeof(resultset) / sizeof(result), resultset);
}

bool setEventIndex() {
    const char *tag = "set-event-index";

    if (set_event_index(DEVICE_ID, EVENT_INDEX) < 0) {
        printf("ERROR %s\n", errmsg());
        return false;
    }

    const result resultset[] = {};

    return evaluate(tag, sizeof(resultset) / sizeof(result), resultset);
}

bool getEvent() {
    const char *tag = "set-event";
    event event;

    if (get_event(DEVICE_ID, EVENT_INDEX, &event) < 0) {
        printf("ERROR %s\n", errmsg());
        return false;
    }

    // clang-format off
    const result resultset[] = {
        { .field = "event timestamp", .type = "string",  .value.string.expected = "2022-04-15 12:29:15", .value.string.value = event.timestamp },
        { .field = "event index",     .type = "uint32",  .value.uint32.expected = 51,                    .value.uint32.value = event.index },
        { .field = "event type",      .type = "uint8",   .value.uint8.expected = 6,                      .value.uint8.value = event.eventType },
        { .field = "event granted",   .type = "boolean", .value.boolean.expected = true,                 .value.boolean.value = event.granted },
        { .field = "event door",      .type = "uint8",   .value.uint8.expected = 3,                      .value.uint8.value = event.door },
        { .field = "event direction", .type = "uint8",   .value.uint8.expected = 1,                      .value.uint8.value = event.direction },
        { .field = "event card",      .type = "uint32",  .value.uint32.expected = 8165538,               .value.uint32.value = event.card },
        { .field = "event reason",    .type = "uint8",   .value.uint8.expected = 21,                     .value.uint8.value = event.reason },
    };
    // clang-format on

    return evaluate(tag, sizeof(resultset) / sizeof(result), resultset);
}

bool recordSpecialEvents() {
    const char *tag = "record-special-events";

    if (record_special_events(DEVICE_ID, true) < 0) {
        printf("ERROR %s\n", errmsg());
        return false;
    }

    const result resultset[] = {};

    return evaluate(tag, sizeof(resultset) / sizeof(result), resultset);
}

struct ListenEvent evt = {
    .controller = 0,
    .timestamp = "",
    .index = 0,
    .event = 0,
    .granted = false,
    .door = 0,
    .direction = 0,
    .card = 0,
    .reason = 0,
};

bool listen() {
    const char *tag = "listen";

    bool running = false;
    bool stop = false;

    if (listen_events(listen_on_event, &running, &stop, listen_on_error) < 0) {
        printf("ERROR %s\n", "error starting event listener");
        return false;
    }

    sleep(1);
    for (int count = 0; count < 5 && !running; count++) {
        sleep(1);
    }

    if (!running) {
        printf("ERROR %s\n", "failed to start event listener");
        return false;
    }

    sleep(2);

    stop = true;
    sleep(1);
    for (int count = 0; count < 5 && running; count++) {
        sleep(1);
    }

    if (running) {
        printf("ERROR %s\n", "failed to stop event listener");
        return false;
    }

    const result resultset[] = {
        {.field = "event controller", .type = "uint32", .value.uint32.expected = DEVICE_ID, .value.uint32.value = evt.controller},
        {.field = "event timestamp", .type = "string", .value.string.expected = "2024-07-05 12:36:45", .value.string.value = evt.timestamp},
        {.field = "event index", .type = "uint32", .value.uint32.expected = 17, .value.uint32.value = evt.index},
        {.field = "event type", .type = "uint8", .value.uint8.expected = 6, .value.uint8.value = evt.event},
        {.field = "event granted", .type = "boolean", .value.boolean.expected = true, .value.boolean.value = evt.granted},
        {.field = "event door", .type = "uint8", .value.uint8.expected = 2, .value.uint8.value = evt.door},
        {.field = "event direction", .type = "uint8", .value.uint8.expected = 1, .value.uint8.value = evt.direction},
        {.field = "event card", .type = "uint32", .value.uint32.expected = 10058400, .value.uint32.value = evt.card},
        {.field = "event reason", .type = "uint8", .value.uint8.expected = 21, .value.uint8.value = evt.reason},
    };

    return evaluate(tag, sizeof(resultset) / sizeof(result), resultset);
}

void listen_on_event(const struct ListenEvent e) {
    evt.controller = e.controller;
    evt.timestamp = strdup(e.timestamp);
    evt.index = e.index;
    evt.event = e.event;
    evt.granted = e.granted;
    evt.door = e.door;
    evt.direction = e.direction;
    evt.card = e.card;
    evt.reason = e.reason;
}

void listen_on_error(const char *err) {
    printf("ERROR %s\n", err);
}
