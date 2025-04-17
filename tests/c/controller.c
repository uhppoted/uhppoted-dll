#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "tests.h"
#include "uhppoted.h"

bool getControllers() {
    const char *tag = "get-controllers";
    uint32_t *devices = NULL;
    int N;

    if (get_devices(&devices, &N) < 0) {
        printf("ERROR %s\n", errmsg());
        return false;
    }

    // clang-format off
    const result resultset[] = {
        { .field = "device count", .type = "uint32", .value.uint32.expected = 3,         .value.uint32.value = N },
        { .field = "device[0]",    .type = "uint32", .value.uint32.expected = 201020304, .value.uint32.value = devices[0] },
        { .field = "device[1]",    .type = "uint32", .value.uint32.expected = 303986753, .value.uint32.value = devices[1] },
        { .field = "device[2]",    .type = "uint32", .value.uint32.expected = 405419896, .value.uint32.value = devices[2] },
    };
    // clang-format on

    bool ok = evaluate(tag, sizeof(resultset) / sizeof(result), resultset);

    free(devices);

    return ok;
}

bool getController() {
    const char *tag = "get-controller";
    struct device d;

    if (get_device(DEVICE_ID, &d) != 0) {
        printf("ERROR %s\n", errmsg());
        return false;
    }

    // clang-format off
    const result resultset[] = {
        { .field = "device ID",       .type = "uint32", .value.uint32.expected = 405419896,           .value.uint32.value = d.ID },
        { .field = "IP address",      .type = "string", .value.string.expected = "192.168.1.101",     .value.string.value = d.address },
        { .field = "subnet mask",     .type = "string", .value.string.expected = "255.255.255.0",     .value.string.value = d.subnet },
        { .field = "gateway address", .type = "string", .value.string.expected = "192.168.1.1",       .value.string.value = d.gateway },
        { .field = "MAC address",     .type = "string", .value.string.expected = "00:12:23:34:45:56", .value.string.value = d.MAC },
        { .field = "version",         .type = "string", .value.string.expected = "v8.92",             .value.string.value = d.version },
        { .field = "date",            .type = "string", .value.string.expected = "2018-11-05",        .value.string.value = d.date },
    };
    // clang-format on

    return evaluate(tag, sizeof(resultset) / sizeof(result), resultset);
}

bool setAddress(uint32_t deviceID, const char *address, const char *subnet,
                const char *gateway) {
    const char *tag = "set-address";

    if (set_address(DEVICE_ID, "192.168.1.125", "255.255.254.0", "192.168.1.0") != 0) {
        printf("ERROR %s\n", errmsg());
        return false;
    }

    const result resultset[] = {};

    return evaluate(tag, sizeof(resultset) / sizeof(result), resultset);
}

bool getStatus() {
    const char *tag = "get-status";
    struct status s;

    if (get_status(DEVICE_ID, &s) != 0) {
        printf("ERROR %s\n", errmsg());
        return false;
    }

    // clang-format off
    const result resultset[] = {
        { .field = "device ID",        .type = "uint32",  .value.uint32.expected = 405419896,             .value.uint32.value = s.ID },
        { .field = "system date/time", .type = "string",  .value.string.expected = "2022-03-19 15:48:32", .value.string.value = s.sysdatetime },
        { .field = "doors[1] state",   .type = "uint8",   .value.uint8.expected = 1,                      .value.uint8.value = s.doors[0] },
        { .field = "doors[2] state",   .type = "uint8",   .value.uint8.expected = 0,                      .value.uint8.value = s.doors[1] },
        { .field = "doors[3] state",   .type = "uint8",   .value.uint8.expected = 0,                      .value.uint8.value = s.doors[2] },
        { .field = "doors[4] state",   .type = "uint8",   .value.uint8.expected = 1,                      .value.uint8.value = s.doors[3] },
        { .field = "buttons[1] state", .type = "uint8",   .value.uint8.expected = 1,                      .value.uint8.value = s.buttons[0] },
        { .field = "buttons[2] state", .type = "uint8",   .value.uint8.expected = 0,                      .value.uint8.value = s.buttons[1] },
        { .field = "buttons[3] state", .type = "uint8",   .value.uint8.expected = 1,                      .value.uint8.value = s.buttons[2] },
        { .field = "buttons[4] state", .type = "uint8",   .value.uint8.expected = 0,                      .value.uint8.value = s.buttons[3] },
        { .field = "relays state",     .type = "uint8",   .value.uint8.expected = 0x12,                   .value.uint8.value = s.relays },
        { .field = "inputs state",     .type = "uint8",   .value.uint8.expected = 0x34,                   .value.uint8.value = s.inputs },
        { .field = "special info",     .type = "uint8",   .value.uint8.expected = 253,                    .value.uint8.value = s.info },
        { .field = "sequence number",  .type = "uint32",  .value.uint32.expected = 9876,                  .value.uint32.value = s.seqno },
        { .field = "event timestamp",  .type = "string",  .value.string.expected = "2022-01-02 12:34:56", .value.string.value  = s.evt.timestamp },
        { .field = "event index",      .type = "uint32",  .value.uint32.expected = 135,                   .value.uint32.value = s.evt.index },
        { .field = "event type",       .type = "uint8",   .value.uint8.expected = 6,                      .value.uint8.value = s.evt.eventType },
        { .field = "event granted",    .type = "boolean", .value.boolean.expected = true,                 .value.boolean.value = s.evt.granted },
        { .field = "event door",       .type = "uint8",   .value.uint8.expected = 3,                      .value.uint8.value = s.evt.door },
        { .field = "event direction",  .type = "uint8",   .value.uint8.expected = 1,                      .value.uint8.value = s.evt.direction },
        { .field = "event card",       .type = "uint32",  .value.uint32.expected = 8100023,               .value.uint32.value = s.evt.card },
        { .field = "event reason",     .type = "uint8",   .value.uint8.expected = 21,                     .value.uint8.value = s.evt.reason },
    };
    // clang-format on

    return evaluate(tag, sizeof(resultset) / sizeof(result), resultset);
}

bool getStatusNoEvent() {
    const char *tag = "get-status-no-event";
    struct status s;

    if (get_status(DEVICE_ID2, &s) != 0) {
        printf("ERROR %s\n", errmsg());
        return false;
    }

    // clang-format off
    const result resultset[] = {
        { .field = "device ID",        .type = "uint32",  .value.uint32.expected = 303986753,             .value.uint32.value = s.ID },
        { .field = "system date/time", .type = "string",  .value.string.expected = "2022-03-19 15:48:32", .value.string.value = s.sysdatetime },
        { .field = "doors[1] state",   .type = "uint8",   .value.uint8.expected = 1,                      .value.uint8.value = s.doors[0] },
        { .field = "doors[2] state",   .type = "uint8",   .value.uint8.expected = 0,                      .value.uint8.value = s.doors[1] },
        { .field = "doors[3] state",   .type = "uint8",   .value.uint8.expected = 0,                      .value.uint8.value = s.doors[2] },
        { .field = "doors[4] state",   .type = "uint8",   .value.uint8.expected = 1,                      .value.uint8.value = s.doors[3] },
        { .field = "buttons[1] state", .type = "uint8",   .value.uint8.expected = 1,                      .value.uint8.value = s.buttons[0] },
        { .field = "buttons[2] state", .type = "uint8",   .value.uint8.expected = 0,                      .value.uint8.value = s.buttons[1] },
        { .field = "buttons[3] state", .type = "uint8",   .value.uint8.expected = 1,                      .value.uint8.value = s.buttons[2] },
        { .field = "buttons[4] state", .type = "uint8",   .value.uint8.expected = 0,                      .value.uint8.value = s.buttons[3] },
        { .field = "relays state",     .type = "uint8",   .value.uint8.expected = 0x12,                   .value.uint8.value = s.relays },
        { .field = "inputs state",     .type = "uint8",   .value.uint8.expected = 0x34,                   .value.uint8.value = s.inputs },
        { .field = "special info",     .type = "uint8",   .value.uint8.expected = 253,                    .value.uint8.value = s.info },
        { .field = "sequence number",  .type = "uint32",  .value.uint32.expected = 9876,                  .value.uint32.value = s.seqno },
        { .field = "event timestamp",  .type = "string",  .value.string.expected = "",                    .value.string.value  = s.evt.timestamp },
        { .field = "event index",      .type = "uint32",  .value.uint32.expected = 0,                     .value.uint32.value = s.evt.index },
        { .field = "event type",       .type = "uint8",   .value.uint8.expected = 0,                      .value.uint8.value = s.evt.eventType },
        { .field = "event granted",    .type = "boolean", .value.boolean.expected = false,                .value.boolean.value = s.evt.granted },
        { .field = "event door",       .type = "uint8",   .value.uint8.expected = 0,                      .value.uint8.value = s.evt.door },
        { .field = "event direction",  .type = "uint8",   .value.uint8.expected = 0,                      .value.uint8.value = s.evt.direction },
        { .field = "event card",       .type = "uint32",  .value.uint32.expected = 0,                     .value.uint32.value = s.evt.card },
        { .field = "event reason",     .type = "uint8",   .value.uint8.expected = 0,                      .value.uint8.value = s.evt.reason },
    };
    // clang-format on

    return evaluate(tag, sizeof(resultset) / sizeof(result), resultset);
}

bool getTime() {
    const char *tag = "get-time";
    char *datetime;

    if (get_time(DEVICE_ID, &datetime) != 0) {
        printf("ERROR %s\n", errmsg());
        return false;
    }

    const result resultset[] = {
        {
            .field = "date/time",
            .type = "string",
            .value.string.expected = "2022-01-02 12:34:56",
            .value.string.value = datetime,
        },
    };

    bool ok = evaluate(tag, sizeof(resultset) / sizeof(result), resultset);

    free(datetime);

    return ok;
}

bool setTime() {
    const char *tag = "set-time";

    if (set_time(DEVICE_ID, "2022-03-23 12:24:17") != 0) {
        printf("ERROR %s\n", errmsg());
        return false;
    }

    const result resultset[] = {};

    return evaluate(tag, sizeof(resultset) / sizeof(result), resultset);
}

bool getListener() {
    const char *tag = "get-listener";
    char *listener;
    uint8_t interval;

    if (get_listener(DEVICE_ID, &listener, &interval) != 0) {
        printf("ERROR %s\n", errmsg());
        return false;
    }

    const result resultset[] = {
        {
            .field = "listener",
            .type = "string",
            .value.string.expected = "192.168.1.100:60001",
            .value.string.value = listener,
        },
        {
            .field = "interval",
            .type = "uint8",
            .value.uint8.expected = 15,
            .value.uint8.value = interval,
        },
    };

    bool ok = evaluate(tag, sizeof(resultset) / sizeof(result), resultset);

    free(listener);

    return ok;
}

bool setListener() {
    const char *tag = "set-listener";

    if (set_listener(DEVICE_ID, "192.168.1.100:60001", 15) != 0) {
        printf("ERROR %s\n", errmsg());
        return false;
    }

    const result resultset[] = {};

    return evaluate(tag, sizeof(resultset) / sizeof(result), resultset);
}

bool getDoorControl() {
    const char *tag = "get-door-control";
    struct door_control d;

    if (get_door_control(DEVICE_ID, DOOR, &d) != 0) {
        printf("ERROR %s\n", errmsg());
        return false;
    }

    const result resultset[] = {
        {
            .field = "door control mode",
            .type = "uint8",
            .value.uint8.expected = CONTROLLED,
            .value.uint8.value = d.mode,
        },
        {
            .field = "door delay",
            .type = "uint8",
            .value.uint8.expected = 7,
            .value.uint8.value = d.delay,
        },
    };

    return evaluate(tag, sizeof(resultset) / sizeof(result), resultset);
}

bool setDoorControl() {
    const char *tag = "set-door-control";

    if (set_door_control(DEVICE_ID, DOOR, NORMALLY_CLOSED, 6) != 0) {
        printf("ERROR %s\n", errmsg());
        return false;
    }

    const result resultset[] = {};

    return evaluate(tag, sizeof(resultset) / sizeof(result), resultset);
}

bool openDoor() {
    const char *tag = "open-door";

    if (open_door(DEVICE_ID, DOOR) != 0) {
        printf("ERROR %s\n", errmsg());
        return false;
    }

    const result resultset[] = {};

    return evaluate(tag, sizeof(resultset) / sizeof(result), resultset);
}

bool setPCControl() {
    const char *tag = "set-pc-control";

    if (set_pc_control(DEVICE_ID, true) < 0) {
        printf("ERROR %s\n", errmsg());
        return false;
    }

    const result resultset[] = {};

    return evaluate(tag, sizeof(resultset) / sizeof(result), resultset);
}

bool setInterlock() {
    const char *tag = "set-interlock";

    if (set_interlock(DEVICE_ID, 4) < 0) {
        printf("ERROR %s\n", errmsg());
        return false;
    }

    const result resultset[] = {};

    return evaluate(tag, sizeof(resultset) / sizeof(result), resultset);
}

bool activateKeypads() {
    const char *tag = "activate-keypads";

    if (activate_keypads(DEVICE_ID, true, true, false, true) < 0) {
        printf("ERROR %s\n", errmsg());
        return false;
    }

    const result resultset[] = {};

    return evaluate(tag, sizeof(resultset) / sizeof(result), resultset);
}

bool setDoorPasscodes() {
    const char *tag = "set-door-passcodes";

    if (set_door_passcodes(DEVICE_ID, DOOR, 12345, 999999, 0, 54321) < 0) {
        printf("ERROR %s\n", errmsg());
        return false;
    }

    const result resultset[] = {};

    return evaluate(tag, sizeof(resultset) / sizeof(result), resultset);
}

bool getAntiPassback() {
    const char *tag = "get-antipassback";
    uint8_t antipassback;

    if (get_antipassback(DEVICE_ID, &antipassback) < 0) {
        printf("ERROR %s\n", errmsg());
        return false;
    }

    const result resultset[] = {
        {
            .field = "antipassback",
            .type = "uint8",
            .value.uint8.expected = 2,
            .value.uint8.value = antipassback,
        },
    };

    return evaluate(tag, sizeof(resultset) / sizeof(result), resultset);
}

bool setAntiPassback() {
    const char *tag = "set-antipassback";

    if (set_antipassback(DEVICE_ID, 2) < 0) {
        printf("ERROR %s\n", errmsg());
        return false;
    }

    const result resultset[] = {};

    return evaluate(tag, sizeof(resultset) / sizeof(result), resultset);
}

bool restoreDefaultParameters() {
    const char *tag = "restore-default-parameters";

    if (restore_default_parameters(DEVICE_ID) < 0) {
        printf("ERROR %s\n", errmsg());
        return false;
    }

    const result resultset[] = {};

    return evaluate(tag, sizeof(resultset) / sizeof(result), resultset);
}
