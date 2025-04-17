#include <iomanip>
#include <iostream>

#include "../include/uhppoted.hpp"
#include "tests.hpp"

using namespace std;

bool getControllers(uhppoted &u) {
    auto controllers = u.get_devices();

    vector<result> rs = {
        result("device count", uint32_t(3), uint32_t(controllers.size())),
        result("device[0] ID", uint32_t(201020304), controllers[0]),
        result("device[1] ID", uint32_t(303986753), controllers[1]),
        result("device[2] ID", uint32_t(405419896), controllers[2]),
    };

    return evaluate("get-controllers", rs);
}

bool getController(uhppoted &u) {
    auto d = u.get_device(DEVICE_ID);

    vector<result> rs = {
        result("device ID", uint32_t(405419896), d.ID),
        result("IP address", string("192.168.1.101"), d.address),
        result("subnet mask", string("255.255.255.0"), d.subnet),
        result("gateway address", string("192.168.1.1"), d.gateway),
        result("MAC address", string("00:12:23:34:45:56"), d.MAC),
        result("version", string("v8.92"), d.version),
        result("date", string("2018-11-05"), d.date),
    };

    return evaluate("get-controller", rs);
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
        result("device ID", uint32_t(405419896), s.ID),
        result("system date/time", string("2022-03-19 15:48:32"), s.sysdatetime),
        result("doors[1]", true, s.doors[0]),
        result("doors[2]", false, s.doors[1]),
        result("doors[3]", false, s.doors[2]),
        result("doors[4]", true, s.doors[3]),
        result("buttons[1]", true, s.buttons[0]),
        result("buttons[2]", false, s.buttons[1]),
        result("buttons[3]", true, s.buttons[2]),
        result("buttons[4]", false, s.buttons[3]),
        result("relays", uint8_t(0x12), s.relays),
        result("inputs", uint8_t(0x34), s.inputs),
        result("system error", uint8_t(0x56), s.syserror),
        result("special info", uint8_t(253), s.info),
        result("sequence number", uint32_t(9876), s.seqno),
        result("event timestamp", string("2022-01-02 12:34:56"), s.evt.timestamp),
        result("event index", uint32_t(135), s.evt.index),
        result("event type", uint8_t(6), s.evt.eventType),
        result("event granted", true, s.evt.granted),
        result("event door", uint8_t(3), s.evt.door),
        result("event direction", uint8_t(1), s.evt.direction),
        result("event card", uint32_t(8100023), s.evt.card),
        result("event reason", uint8_t(21), s.evt.reason),
    };

    return evaluate("get-status", rs);
}

bool getStatusNoEvent(uhppoted &u) {
    auto s = u.get_status(DEVICE_ID2);

    vector<result> rs = {
        result("device ID", uint32_t(303986753), s.ID),
        result("system date/time", string("2022-03-19 15:48:32"), s.sysdatetime),
        result("doors[1]", true, s.doors[0]),
        result("doors[2]", false, s.doors[1]),
        result("doors[3]", false, s.doors[2]),
        result("doors[4]", true, s.doors[3]),
        result("buttons[1]", true, s.buttons[0]),
        result("buttons[2]", false, s.buttons[1]),
        result("buttons[3]", true, s.buttons[2]),
        result("buttons[4]", false, s.buttons[3]),
        result("relays", uint8_t(0x12), s.relays),
        result("inputs", uint8_t(0x34), s.inputs),
        result("system error", uint8_t(0x56), s.syserror),
        result("special info", uint8_t(253), s.info),
        result("sequence number", uint32_t(9876), s.seqno),
        result("event timestamp", string(""), s.evt.timestamp),
        result("event index", uint32_t(0), s.evt.index),
        result("event type", uint8_t(0), s.evt.eventType),
        result("event granted", false, s.evt.granted),
        result("event door", uint8_t(0), s.evt.door),
        result("event direction", uint8_t(0), s.evt.direction),
        result("event card", uint32_t(0), s.evt.card),
        result("event reason", uint8_t(0), s.evt.reason),
    };

    return evaluate("get-status-no-event", rs);
}
bool getTime(uhppoted &u) {
    string datetime = u.get_time(DEVICE_ID);

    vector<result> rs = {
        result("date/time", string("2022-01-02 12:34:56"), datetime),
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
    uint8_t interval = u.get_listener_interval(DEVICE_ID);

    vector<result> rs = {
        result("listener address", string("192.168.1.100:60001"), listener),
        result("listener interval", uint8_t(15), interval),
    };

    return evaluate("get-listener", rs);
}

bool setListener(uhppoted &u) {
    string listener = "192.168.1.100:60001";
    uint8_t interval = 15;

    u.set_listener(DEVICE_ID, listener, interval);

    vector<result> rs = {};

    return evaluate("set-listener", rs);
}

bool getDoorControl(uhppoted &u) {
    auto d = u.get_door_control(DEVICE_ID, DOOR);

    vector<result> rs = {
        result("door control mode", uint8_t(CONTROLLED), d.mode),
        result("door delay", uint8_t(7), d.delay),
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

bool setPCControl(uhppoted &u) {
    u.set_pc_control(DEVICE_ID, true);

    vector<result> rs = {};

    return evaluate("set-pc-control", rs);
}

bool setInterlock(uhppoted &u) {
    u.set_interlock(DEVICE_ID, 4);

    vector<result> rs = {};

    return evaluate("set-interlock", rs);
}

bool activateKeypads(uhppoted &u) {
    u.activate_keypads(DEVICE_ID, true, true, false, true);

    vector<result> rs = {};

    return evaluate("activate-keypads", rs);
}

bool setDoorPasscodes(uhppoted &u) {
    u.set_door_passcodes(DEVICE_ID, DOOR, 12345, 999999, 0, 54321);

    vector<result> rs = {};

    return evaluate("set-door-passcodes", rs);
}

bool getAntiPassback(uhppoted &u) {
    auto antipassback = u.get_antipassback(DEVICE_ID);

    vector<result> rs = {
        result("antipassback", uint8_t(2), antipassback),
    };

    return evaluate("get-antipassback", rs);
}

bool setAntiPassback(uhppoted &u) {
    u.set_antipassback(DEVICE_ID, 2);

    vector<result> rs = {};

    return evaluate("set-passback", rs);
}

bool restoreDefaultParameters(uhppoted &u) {
    u.restore_default_parameters(DEVICE_ID);

    vector<result> rs = {};

    return evaluate("restore-default-parameters", rs);
}
