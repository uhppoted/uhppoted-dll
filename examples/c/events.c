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

void callback(const status *e) {
    if (e != NULL) {
        const char *direction = lookup(LOOKUP_DIRECTION, e->evt.direction, locale);
        const char *eventType = lookup(LOOKUP_EVENT_TYPE, e->evt.eventType, locale);
        const char *reason = lookup(LOOKUP_EVENT_REASON, e->evt.reason, locale);
        const char *timestamp = e->evt.timestamp;

        if (strlen(e->evt.timestamp) == 0) {
            timestamp = "-";
        }

        field fields[] = {
            {.field = "controller", .type = "uint32", .value.uint32 = e->ID},
            {.field = "date/time", .type = "string", .value.string = e->sysdatetime},
            {.field = "doors[1]", .type = "bool", .value.boolean = e->doors[0]},
            {.field = "doors[2]", .type = "bool", .value.boolean = e->doors[1]},
            {.field = "doors[3]", .type = "bool", .value.boolean = e->doors[2]},
            {.field = "doors[4]", .type = "bool", .value.boolean = e->doors[3]},
            {.field = "buttons[1]", .type = "bool", .value.boolean = e->buttons[0]},
            {.field = "buttons[2]", .type = "bool", .value.boolean = e->buttons[1]},
            {.field = "buttons[3]", .type = "bool", .value.boolean = e->buttons[2]},
            {.field = "buttons[4]", .type = "bool", .value.boolean = e->buttons[3]},
            {.field = "relays", .type = "uint8", .value.uint8 = e->relays},
            {.field = "inputs", .type = "uint8", .value.uint8 = e->inputs},
            {.field = "error", .type = "uint8", .value.uint8 = e->syserror},
            {.field = "seq no.", .type = "uint32", .value.uint32 = e->seqno},
            {.field = "info", .type = "uint8", .value.uint8 = e->info},
            {.field = "event timestamp", .type = "string", .value.string = timestamp},
            {.field = "      index", .type = "uint32", .value.uint32 = e->evt.index},
            {.field = "      type", .type = "string", .value.string = eventType},
            {.field = "      granted", .type = "bool", .value.boolean = e->evt.granted},
            {.field = "      door", .type = "uint8", .value.uint8 = e->evt.door},
            {.field = "      direction", .type = "string", .value.string = direction},
            {.field = "      card", .type = "uint32", .value.uint32 = e->evt.card},
            {.field = "      reason", .type = "string", .value.string = reason},
        };

        display("event", sizeof(fields) / sizeof(field), fields);
    }
}

int listen(int argc, char **argv) {
    if (listen_events("/tmp/uhppoted.pipe", callback) < 0) {
        printf("ERROR %s\n", errmsg());
        return -1;
    }

    return 0;
}
