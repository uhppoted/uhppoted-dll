#include <stdio.h>
#include <string.h>

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

void handler(
    uint32_t controller,
    uint32_t index,
    const char *timestamp,
    uint8_t event,
    uint32_t card,
    uint8_t door,
    uint8_t granted,
    uint8_t direction,
    uint8_t reason) {
    const char *_direction = lookup(LOOKUP_DIRECTION, direction, locale);
    const char *_event = lookup(LOOKUP_EVENT_TYPE, event, locale);
    const char *_reason = lookup(LOOKUP_EVENT_REASON, reason, locale);
    char _timestamp[20] = "-";

    if ((timestamp != NULL) && (strcmp(timestamp, "") != 0)) {
        snprintf(_timestamp, sizeof(_timestamp), "%s", timestamp);
    }

    field fields[] = {
        {.field = "controller", .type = "uint32", .value.uint32 = controller},
        {.field = "event timestamp", .type = "string", .value.string = _timestamp},
        {.field = "      index", .type = "uint32", .value.uint32 = index},
        {.field = "      type", .type = "string", .value.string = _event},
        {.field = "      granted", .type = "bool", .value.boolean = granted},
        {.field = "      door", .type = "uint8", .value.uint8 = door},
        {.field = "      direction", .type = "string", .value.string = _direction},
        {.field = "      card", .type = "uint32", .value.uint32 = card},
        {.field = "      reason", .type = "string", .value.string = _reason},
    };

    display("event", sizeof(fields) / sizeof(field), fields);
}

int listen(int argc, char **argv) {
    if (listen_events(handler) < 0) {
        printf("ERROR %s\n", errmsg());
        return -1;
    }

    return 0;
}
