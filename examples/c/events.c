#include <signal.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

#include "examples.h"
#include "uhppoted.h"

void listen_on_event(const struct ListenEvent evt, void *userdata);
void listen_on_error(const char *err);

int getEventIndex(int argc, char **argv) {
    options opts = parse(argc, argv);
    uint32_t deviceID = opts.device_id;
    uint32_t index;

    if (get_event_index(deviceID, &index) < 0) {
        printf("ERROR %s\n", errmsg());
        return -1;
    }

    field fields[] = {
        {.field = "ID", .type = "uint32", .value.uint32 = deviceID},
        {.field = "index", .type = "uint32", .value.uint32 = index},
    };

    display("get-event=index", sizeof(fields) / sizeof(field), fields);

    return 0;
}

int setEventIndex(int argc, char **argv) {
    options opts = parse(argc, argv);
    uint32_t deviceID = opts.device_id;
    uint32_t index = opts.event_index;

    if (set_event_index(deviceID, index) < 0) {
        printf("ERROR %s\n", errmsg());
        return -1;
    }

    field fields[] = {
        {.field = "ID", .type = "uint32", .value.uint32 = deviceID},
        {.field = "index", .type = "uint32", .value.uint32 = index},
    };

    display("set-event-index", sizeof(fields) / sizeof(field), fields);

    return 0;
}

int getEvent(int argc, char **argv) {
    options opts = parse(argc, argv);
    uint32_t deviceID = opts.device_id;
    uint32_t index = opts.event_index;
    event event;

    if (get_event(deviceID, index, &event) < 0) {
        printf("ERROR %s\n", errmsg());
        return -1;
    }

    const char *eventType = lookup(LOOKUP_EVENT_TYPE, event.eventType, locale);
    const char *direction = lookup(LOOKUP_DIRECTION, event.direction, locale);
    const char *reason = lookup(LOOKUP_EVENT_REASON, event.reason, locale);

    field fields[] = {
        {.field = "ID", .type = "uint32", .value.uint32 = deviceID},
        {.field = "event index", .type = "uint32", .value.uint32 = event.index},
        {.field = "      timestamp", .type = "string", .value.string = event.timestamp},
        {.field = "      type", .type = "string", .value.string = eventType},
        {.field = "      granted", .type = "bool", .value.boolean = event.granted},
        {.field = "      direction", .type = "string", .value.string = direction},
        {.field = "      card number", .type = "uint32", .value.uint32 = event.card},
        {.field = "      reason", .type = "string", .value.string = reason},
    };

    display("get-event", sizeof(fields) / sizeof(field), fields);

    return 0;
}

int recordSpecialEvents(int argc, char **argv) {
    uint32_t deviceID = parse(argc, argv).device_id;
    bool enabled = true;

    if (record_special_events(deviceID, enabled) < 0) {
        printf("ERROR %s\n", errmsg());
        return -1;
    }

    field fields[] = {
        {.field = "ID", .type = "uint32", .value.uint32 = deviceID},
        {.field = "enabled", .type = "bool", .value.boolean = enabled},
    };

    display("record-special-events", sizeof(fields) / sizeof(field), fields);

    return 0;
}

int listen(int argc, char **argv) {
    bool listening = false;
    bool stop = false;

    if (listen_events(listen_on_event, &listening, &stop, listen_on_error, NULL) < 0) {
        printf("ERROR %s\n", errmsg());
        return -1;
    }

    sleep(1);
    for (int count = 0; count < 5 && !listening; count++) {
        printf(" ... waiting %d %s\n", count, listening ? "listening" : "pending");
        sleep(1);
    }

    if (!listening) {
        printf("ERROR %s\n", "failed to start event listener");
        return -1;
    }

    printf("INFO  listening\n");

    sigset_t set;
    int sig;
    int rc;

    sigemptyset(&set);

    if ((rc = sigaddset(&set, SIGINT)) == -1) {
        printf("ERROR sigaddset (%d)", rc);
        return -1;
    }

    if ((rc = sigwait(&set, &sig)) != 0) {
        printf("ERROR sigwait (%d)\n", rc);
        return -1;
    }

    stop = true;
    sleep(1);
    for (int count = 0; count < 5 && listening; count++) {
        printf("DEBUG ... stoppping event listener %d %s\n", count, listening ? "listening" : "stopped");
        sleep(1);
    }

    if (listening) {
        printf("ERROR %s\n", "failed to stop event listener");
        return -1;
    }

    return 0;
}

void listen_on_event(const struct ListenEvent evt, void *userdata) {
    const char *_direction = lookup(LOOKUP_DIRECTION, evt.direction, locale);
    const char *_event = lookup(LOOKUP_EVENT_TYPE, evt.event, locale);
    const char *_reason = lookup(LOOKUP_EVENT_REASON, evt.reason, locale);
    char _timestamp[20] = "-";

    if ((evt.timestamp != NULL) && (strcmp(evt.timestamp, "") != 0)) {
        snprintf(_timestamp, sizeof(_timestamp), "%s", evt.timestamp);
    }

    field fields[] = {
        {.field = "controller", .type = "uint32", .value.uint32 = evt.controller},
        {.field = "event timestamp", .type = "string", .value.string = _timestamp},
        {.field = "      index", .type = "uint32", .value.uint32 = evt.index},
        {.field = "      type", .type = "string", .value.string = _event},
        {.field = "      granted", .type = "bool", .value.boolean = evt.granted},
        {.field = "      door", .type = "uint8", .value.uint8 = evt.door},
        {.field = "      direction", .type = "string", .value.string = _direction},
        {.field = "      card", .type = "uint32", .value.uint32 = evt.card},
        {.field = "      reason", .type = "string", .value.string = _reason},
    };

    display("event", sizeof(fields) / sizeof(field), fields);
}

void listen_on_error(const char *err) {
    printf("ERROR %s\n", err);
}
