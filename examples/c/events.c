#include <signal.h>
#include <stdio.h>
#include <stdlib.h>

#include "examples.h"
#include "uhppoted.h"

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

static void onSignal(int signum) {
    if (signum == SIGUSR1) {
        printf(">>> SIGUSR1!\n");
    }
}

int listen(int argc, char **argv) {
    sigset_t mask, oldmask;

    if (listen_events() < 0) {
        printf("ERROR %s\n", errmsg());
        return -1;
    }

    printf(">>>> listening...\n");

    signal(SIGUSR1, onSignal);

    sigemptyset(&mask);
    sigaddset(&mask, SIGUSR1);
    sigprocmask(SIG_BLOCK, &mask, &oldmask);

    while (true) {
        sigsuspend(&oldmask);
        printf(">>>> gotcha ...\n");
    }

    sigprocmask(SIG_UNBLOCK, &mask, NULL);

    printf(">>>> listened...\n");

    return 0;
}
