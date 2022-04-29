#include <iomanip>
#include <iostream>

#include "../include/uhppoted.hpp"
#include "tests.hpp"

using namespace std;

bool getDevices(uhppoted &u) {
    auto devices = u.get_devices();

    vector<result> rs = {
        result("device count", "uint32", {.uint32 = 3}, {.uint32 = static_cast<uint32_t>(devices.size())}),
        result("device[0] ID", "uint32", {.uint32 = 201020304}, {.uint32 = devices[0]}),
        result("device[1] ID", "uint32", {.uint32 = 303986753}, {.uint32 = devices[1]}),
        result("device[2] ID", "uint32", {.uint32 = 405419896}, {.uint32 = devices[2]}),
    };

    return evaluate("get-devices", rs);
}

bool getDevice(uhppoted &u) {
    auto d = u.get_device(DEVICE_ID);

    vector<result> rs = {
        result("device ID", "uint32", {.uint32 = 405419896}, {.uint32 = d.ID}),
        result("IP address", "string", {.string = "192.168.1.101"}, {.string = d.address.c_str()}),
        result("subnet mask", "string", {.string = "255.255.255.0"}, {.string = d.subnet.c_str()}),
        result("gateway address", "string", {.string = "192.168.1.1"}, {.string = d.gateway.c_str()}),
        result("MAC address", "string", {.string = "00:12:23:34:45:56"}, {.string = d.MAC.c_str()}),
        result("version", "string", {.string = "v8.92"}, {.string = d.version.c_str()}),
        result("date", "string", {.string = "2018-11-05"}, {.string = d.date.c_str()}),
    };

    return evaluate("get-device", rs);
}

bool setAddress(uhppoted &u) {
    string address = "192.168.1.125";
    string subnet = "255.255.254.0";
    string gateway = "192.168.1.0";

    u.set_address(DEVICE_ID, address, subnet, gateway);

    vector<result> rs = {};

    return evaluate("set-address", rs);
}

bool getStatus(uhppoted &u) {
    auto s = u.get_status(DEVICE_ID);

    vector<result> rs = {
        result("device ID", "uint32", {.uint32 = 405419896}, {.uint32 = s.ID}),
        result("system date/time", "string", {.string = "2022-03-19 15:48:32"}, {.string = s.sysdatetime.c_str()}),
        result("doors[1]", "boolean", {.boolean = true}, {.boolean = s.doors[0] != 0}),
        result("doors[2]", "boolean", {.boolean = false}, {.boolean = s.doors[1] != 0}),
        result("doors[3]", "boolean", {.boolean = false}, {.boolean = s.doors[2] != 0}),
        result("doors[4]", "boolean", {.boolean = true}, {.boolean = s.doors[3] != 0}),
        result("buttons[1]", "boolean", {.boolean = true}, {.boolean = s.buttons[0] != 0}),
        result("buttons[2]", "boolean", {.boolean = false}, {.boolean = s.buttons[1] != 0}),
        result("buttons[3]", "boolean", {.boolean = true}, {.boolean = s.buttons[2] != 0}),
        result("buttons[4]", "boolean", {.boolean = false}, {.boolean = s.buttons[3] != 0}),
        result("relays", "uint8", {.uint8 = 0x12}, {.uint8 = s.relays}),
        result("inputs", "uint8", {.uint8 = 0x34}, {.uint8 = s.inputs}),
        result("system error", "uint8", {.uint8 = 0x56}, {.uint8 = s.syserror}),
        result("special info", "uint8", {.uint8 = 253}, {.uint8 = s.info}),
        result("sequence number", "uint32", {.uint32 = 9876}, {.uint32 = s.seqno}),
        result("event timestamp", "string", {.string = "2022-01-02 12:34:56"}, {.string = s.event.timestamp.c_str()}),
        result("event index", "uint32", {.uint32 = 135}, {.uint32 = s.event.index}),
        result("event type", "uint8", {.uint8 = 6}, {.uint8 = s.event.eventType}),
        result("event granted", "boolean", {.boolean = true}, {.boolean = s.event.granted}),
        result("event door", "uint8", {.uint8 = 3}, {.uint8 = s.event.door}),
        result("event direction", "uint8", {.uint8 = 1}, {.uint8 = s.event.direction}),
        result("event card", "uint32", {.uint32 = 8100023}, {.uint32 = s.event.card}),
        result("event reason", "uint8", {.uint8 = 21}, {.uint8 = s.event.reason}),
    };

    return evaluate("get-status", rs);
}

bool getTime(uhppoted &u) {
    string datetime = u.get_time(DEVICE_ID);

    vector<result> rs = {
        result("date/time", "string", {.string = "2022-01-02 12:34:56"}, {.string = datetime.c_str()}),
    };

    return evaluate("get-time", rs);
}

bool setTime(uhppoted &u) {
    string datetime = "2022-03-23 12:24:17";

    u.set_time(DEVICE_ID, datetime);

    vector<result> rs = {};

    return evaluate("set-time", rs);
}

bool getListener(uhppoted &u) {
    string listener = u.get_listener(DEVICE_ID);

    vector<result> rs = {
        result("listener address", "string", {.string = "192.168.1.100:60001"}, {.string = listener.c_str()}),
    };

    return evaluate("get-listener", rs);
}

bool setListener(uhppoted &u) {
    string listener = "192.168.1.100:60001";

    u.set_listener(DEVICE_ID, listener);

    vector<result> rs = {};

    return evaluate("set-listener", rs);
}

bool getDoorControl(uhppoted &u) {
    auto d = u.get_door_control(DEVICE_ID, DOOR);

    vector<result> rs = {
        result("door control mode", "uint8", {.uint8 = CONTROLLED}, {.uint8 = d.mode}),
        result("door delay", "uint8", {.uint8 = 7}, {.uint8 = d.delay}),
    };

    return evaluate("get-door-control", rs);
}

bool setDoorControl(uhppoted &u) {
    u.set_door_control(DEVICE_ID, DOOR, NORMALLY_CLOSED, 6);

    vector<result> rs = {};

    return evaluate("set-door-control", rs);
}

bool openDoor(uhppoted &u) {
    u.open_door(DEVICE_ID, DOOR);

    vector<result> rs = {};

    return evaluate("open-door", rs);
}
