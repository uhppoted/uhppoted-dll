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

// void handler(const uint32_t controller, const event evt) {
//     const char *direction = lookup(LOOKUP_DIRECTION, evt.direction, locale);
//     const char *eventType = lookup(LOOKUP_EVENT_TYPE, evt.eventType, locale);
//     const char *reason = lookup(LOOKUP_EVENT_REASON, evt.reason, locale);
//     const char *timestamp = evt.timestamp;
//
//     if (strlen(evt.timestamp) == 0) {
//         timestamp = "-";
//     }
//
//     field fields[] = {
//         {.field = "controller", .type = "uint32", .value.uint32 = controller},
//         {.field = "event timestamp", .type = "string", .value.string = timestamp},
//         {.field = "      index", .type = "uint32", .value.uint32 = evt.index},
//         {.field = "      type", .type = "string", .value.string = eventType},
//         {.field = "      granted", .type = "bool", .value.boolean = evt.granted},
//         {.field = "      door", .type = "uint8", .value.uint8 = evt.door},
//         {.field = "      direction", .type = "string", .value.string = direction},
//         {.field = "      card", .type = "uint32", .value.uint32 = evt.card},
//         {.field = "      reason", .type = "string", .value.string = reason},
//     };
//
//     display("event", sizeof(fields) / sizeof(field), fields);
// }

void handler(
    uint32_t controller,
    uint32_t index,
    uint32_t date,
    uint32_t time,
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

    if ((index != 0) && (date != 0)) {
        uint32_t year = (date >> 16) & 0x0000ffff;
        uint32_t month = (date >> 8) & 0x0000ff;
        uint32_t day = (date >> 0) & 0x0000ff;
        uint32_t hour = (time >> 16) & 0x0000ff;
        uint32_t minute = (time >> 8) & 0x0000ff;
        uint32_t second = (time >> 0) & 0x0000ff;

        snprintf(_timestamp, sizeof(_timestamp), "%04x-%02x-%02x %02x:%02x:%02x", year, month, day, hour, minute, second);
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
