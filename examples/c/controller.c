#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>

#include "examples.h"
#include "uhppoted.h"

int getControllers(int argc, char **argv) {
    uint32_t *devices = NULL;
    int N;

    if (get_devices(&devices, &N) < 0) {
        printf("ERROR %s\n", errmsg());
        return -1;
    }

    char tag[32];
    field fields[N];

    snprintf(tag, sizeof(tag), "get-devices(%d)", N);

    for (int i = 0; i < N; i++) {
        fields[i].field = "";
        fields[i].type = "uint32";
        fields[i].value.uint32 = devices[i];
    }

    display(tag, N, fields);

    free(devices);

    return 0;
}

int getController(int argc, char **argv) {
    uint32_t deviceID = parse(argc, argv).device_id;
    struct device d;

    if (get_device(deviceID, &d) != 0) {
        printf("ERROR %s\n", errmsg());
        return -1;
    }

    field fields[] = {
        {.field = "ID", .type = "uint32", .value.uint32 = d.ID},
        {.field = "address", .type = "string", .value.string = d.address},
        {.field = "subnet mask", .type = "string", .value.string = d.subnet},
        {.field = "gateway address", .type = "string", .value.string = d.gateway},
        {.field = "MAC", .type = "string", .value.string = d.MAC},
        {.field = "version", .type = "string", .value.string = d.version},
        {.field = "released", .type = "string", .value.string = d.date},
    };

    display("get-controller", sizeof(fields) / sizeof(field), fields);

    return 0;
}

int setAddress(int argc, char **argv) {
    options opts = parse(argc, argv);
    uint32_t deviceID = opts.device_id;
    const char *address = opts.ip_address;
    const char *subnet = opts.subnet_mask;
    const char *gateway = opts.gateway;

    if (set_address(deviceID, address, subnet, gateway) != 0) {
        printf("ERROR %s\n", errmsg());
        return -1;
    }

    field fields[] = {
        {.field = "ID", .type = "uint32", .value.uint32 = deviceID},
        {.field = "IP address", .type = "string", .value.string = (char *)address},
        {.field = "subnet mask", .type = "string", .value.string = (char *)subnet},
        {.field = "gateway address", .type = "string", .value.string = (char *)gateway},
    };

    display("set-address", sizeof(fields) / sizeof(field), fields);

    return 0;
}

int getStatus(int argc, char **argv) {
    uint32_t deviceID = parse(argc, argv).device_id;
    struct status s;

    if (get_status(deviceID, &s) != 0) {
        printf("ERROR %s\n", errmsg());
        return -1;
    }

    const char *direction = lookup(LOOKUP_DIRECTION, s.evt.direction, locale);
    const char *eventType = lookup(LOOKUP_EVENT_TYPE, s.evt.eventType, locale);
    const char *reason = lookup(LOOKUP_EVENT_REASON, s.evt.reason, locale);
    const char *timestamp = s.evt.timestamp;

    if (strncmp(s.evt.timestamp, "", 0) == 0) {
        timestamp = "-";
    }

    field fields[] = {
        {.field = "ID", .type = "uint32", .value.uint32 = s.ID},
        {.field = "date/time", .type = "string", .value.string = s.sysdatetime},
        {.field = "doors[1]", .type = "bool", .value.boolean = s.doors[0]},
        {.field = "doors[2]", .type = "bool", .value.boolean = s.doors[1]},
        {.field = "doors[3]", .type = "bool", .value.boolean = s.doors[2]},
        {.field = "doors[4]", .type = "bool", .value.boolean = s.doors[3]},
        {.field = "buttons[1]", .type = "bool", .value.boolean = s.buttons[0]},
        {.field = "buttons[2]", .type = "bool", .value.boolean = s.buttons[1]},
        {.field = "buttons[3]", .type = "bool", .value.boolean = s.buttons[2]},
        {.field = "buttons[4]", .type = "bool", .value.boolean = s.buttons[3]},
        {.field = "relays", .type = "uint8", .value.uint8 = s.relays},
        {.field = "inputs", .type = "uint8", .value.uint8 = s.inputs},
        {.field = "error", .type = "uint8", .value.uint8 = s.syserror},
        {.field = "seq no.", .type = "uint32", .value.uint32 = s.seqno},
        {.field = "info", .type = "uint8", .value.uint8 = s.info},
        {.field = "event timestamp", .type = "string", .value.string = timestamp},
        {.field = "      index", .type = "uint32", .value.uint32 = s.evt.index},
        {.field = "      type", .type = "string", .value.string = eventType},
        {.field = "      granted", .type = "bool", .value.boolean = s.evt.granted},
        {.field = "      door", .type = "uint8", .value.uint8 = s.evt.door},
        {.field = "      direction", .type = "string", .value.string = direction},
        {.field = "      card", .type = "uint32", .value.uint32 = s.evt.card},
        {.field = "      reason", .type = "string", .value.string = reason},
    };

    display("get-status", sizeof(fields) / sizeof(field), fields);

    return 0;
}

int getTime(int argc, char **argv) {
    uint32_t deviceID = parse(argc, argv).device_id;
    char *datetime;

    if (get_time(deviceID, &datetime) != 0) {
        printf("ERROR %s\n", errmsg());
        return -1;
    }

    field fields[] = {
        {.field = "ID", .type = "uint32", .value.uint32 = deviceID},
        {.field = "date/time", .type = "string", .value.string = datetime},
    };

    display("get-time", sizeof(fields) / sizeof(field), fields);

    free(datetime);

    return 0;
}

int setTime(int argc, char **argv) {
    uint32_t deviceID = parse(argc, argv).device_id;
    time_t utc;
    struct tm *local;
    char datetime[20];

    time(&utc);
    local = localtime(&utc);

    strftime(datetime, 20, "%Y-%m-%d %H:%M:%S", local);

    if (set_time(deviceID, (char *)datetime) != 0) {
        printf("ERROR %s\n", errmsg());
        return -1;
    }

    field fields[] = {
        {.field = "ID", .type = "uint32", .value.uint32 = deviceID},
        {.field = "date/time", .type = "string", .value.string = datetime},
    };

    display("set-time", sizeof(fields) / sizeof(field), fields);

    return 0;
}

int getListener(int argc, char **argv) {
    uint32_t deviceID = parse(argc, argv).device_id;
    char *listener;

    if (get_listener(deviceID, &listener) != 0) {
        printf("ERROR %s\n", errmsg());
        return -1;
    }

    field fields[] = {
        {.field = "ID", .type = "uint32", .value.uint32 = deviceID},
        {.field = "event listener", .type = "string", .value.string = listener},
    };

    display("get-listener", sizeof(fields) / sizeof(field), fields);

    free(listener);

    return 0;
}

int setListener(int argc, char **argv) {
    options opts = parse(argc, argv);
    uint32_t deviceID = opts.device_id;
    const char *listener = opts.listener;

    if (set_listener(deviceID, (char *)listener) != 0) {
        printf("ERROR %s\n", errmsg());
        return -1;
    }

    field fields[] = {
        {.field = "ID", .type = "uint32", .value.uint32 = deviceID},
        {.field = "event listener", .type = "string", .value.string = (char *)listener},
    };

    display("set-listener", sizeof(fields) / sizeof(field), fields);

    return 0;
}

int getDoorControl(int argc, char **argv) {
    options opts = parse(argc, argv);
    uint32_t deviceID = opts.device_id;
    uint8_t door = opts.door;
    struct door_control control;

    if (get_door_control(deviceID, door, &control) != 0) {
        printf("ERROR %s\n", errmsg());
        return -1;
    }

    const char *mode = lookup(LOOKUP_MODE, control.mode, locale);

    field fields[] = {
        {.field = "ID", .type = "uint32", .value.uint32 = deviceID},
        {.field = "door", .type = "uint8", .value.uint8 = door},
        {.field = "mode", .type = "string", .value.string = mode},
        {.field = "delay", .type = "uint8", .value.uint8 = control.delay},
    };

    display("get-door-control", sizeof(fields) / sizeof(field), fields);

    return 0;
}

int setDoorControl(int argc, char **argv) {
    options opts = parse(argc, argv);
    uint32_t deviceID = opts.device_id;
    uint8_t door = opts.door;
    uint8_t mode = NORMALLY_OPEN;
    uint8_t delay = 9;

    if (set_door_control(deviceID, door, mode, delay) != 0) {
        printf("ERROR %s\n", errmsg());
        return -1;
    }

    const char *smode = lookup(LOOKUP_MODE, NORMALLY_OPEN, locale);

    field fields[] = {
        {.field = "ID", .type = "uint32", .value.uint32 = deviceID},
        {.field = "door", .type = "uint8", .value.uint8 = door},
        {.field = "mode", .type = "string", .value.string = smode},
        {.field = "delay", .type = "uint8", .value.uint8 = delay},
    };

    display("set-door-control", sizeof(fields) / sizeof(field), fields);

    return 0;
}

int openDoor(int argc, char **argv) {
    options opts = parse(argc, argv);
    uint32_t deviceID = opts.device_id;
    uint8_t door = opts.door;

    if (open_door(deviceID, door) != 0) {
        printf("ERROR %s\n", errmsg());
        return -1;
    }

    field fields[] = {
        {.field = "ID", .type = "uint32", .value.uint32 = deviceID},
        {.field = "door", .type = "uint8", .value.uint8 = door},
    };

    display("open-door", sizeof(fields) / sizeof(field), fields);

    return 0;
}

int setPCControl(int argc, char **argv) {
    uint32_t deviceID = parse(argc, argv).device_id;
    bool enabled = true;

    if (set_pc_control(deviceID, enabled) < 0) {
        printf("ERROR %s\n", errmsg());
        return -1;
    }

    field fields[] = {
        {.field = "ID", .type = "uint32", .value.uint32 = deviceID},
        {.field = "enabled", .type = "bool", .value.boolean = enabled},
    };

    display("set-pc-control", sizeof(fields) / sizeof(field), fields);

    return 0;
}

int setInterlock(int argc, char **argv) {
    uint32_t deviceID = parse(argc, argv).device_id;
    uint8_t interlock = 1;

    if (set_interlock(deviceID, interlock) < 0) {
        printf("ERROR %s\n", errmsg());
        return -1;
    }

    field fields[] = {
        {.field = "ID", .type = "uint32", .value.uint32 = deviceID},
        {.field = "interlock", .type = "uint8", .value.uint8 = interlock},
    };

    display("set-interlock", sizeof(fields) / sizeof(field), fields);

    return 0;
}

int activateKeypads(int argc, char **argv) {
    uint32_t controller = parse(argc, argv).device_id;
    bool reader1 = true;
    bool reader2 = true;
    bool reader3 = false;
    bool reader4 = true;

    if (activate_keypads(controller, reader1, reader2, reader3, reader4) < 0) {
        printf("ERROR %s\n", errmsg());
        return -1;
    }

    field fields[] = {
        {.field = "ID", .type = "uint32", .value.uint32 = controller},
        {.field = "reader1", .type = "bool", .value.boolean = reader1},
        {.field = "reader2", .type = "bool", .value.boolean = reader2},
        {.field = "reader3", .type = "bool", .value.boolean = reader3},
        {.field = "reader4", .type = "bool", .value.boolean = reader4},
    };

    display("activate-keypads", sizeof(fields) / sizeof(field), fields);

    return 0;
}

int setDoorPasscodes(int argc, char **argv) {
    uint32_t controller = parse(argc, argv).device_id;
    uint8_t door = parse(argc, argv).door;
    uint32_t passcode1 = 12345;
    uint32_t passcode2 = 999999;
    uint32_t passcode3 = 0;
    uint32_t passcode4 = 54321;

    if (set_door_passcodes(controller, door, passcode1, passcode2, passcode3, passcode4) < 0) {
        printf("ERROR %s\n", errmsg());
        return -1;
    }

    field fields[] = {
        {.field = "ID", .type = "uint32", .value.uint32 = controller},
        {.field = "door", .type = "uint8", .value.uint8 = door},
        {.field = "passcode1", .type = "uint32", .value.uint32 = passcode1},
        {.field = "passcode2", .type = "uint32", .value.uint32 = passcode2},
        {.field = "passcode3", .type = "uint32", .value.uint32 = passcode3},
        {.field = "passcode4", .type = "uint32", .value.uint32 = passcode4},
    };

    display("set-door-passcodes", sizeof(fields) / sizeof(field), fields);

    return 0;
}

int restoreDefaultParameters(int argc, char **argv) {
    uint32_t controller = parse(argc, argv).device_id;

    if (restore_default_parameters(controller) < 0) {
        printf("ERROR %s\n", errmsg());
        return -1;
    }

    field fields[] = {
        {.field = "ID", .type = "uint32", .value.uint32 = controller},
    };

    display("restore-default-parameters", sizeof(fields) / sizeof(field), fields);

    return 0;
}
