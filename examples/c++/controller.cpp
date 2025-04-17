#include <iostream>
#include <sstream>

#include "../include/uhppoted.hpp"
#include "examples.hpp"

using namespace std;

void getControllers(uhppoted &u, int argc, char **argv) {
    auto controllers = u.get_devices();

    stringstream tag;
    vector<field> fields(controllers.size());
    int ix = 0;

    tag << "get-controllers (" << controllers.size() << ")";
    for (auto id : controllers) {
        fields[ix++] = field("", id); // doing this with 'push_back' doesn't compile on github (seems fine everywhere else)
    }

    display(tag.str(), fields);
}

void getController(uhppoted &u, int argc, char **argv) {
    auto options = parse(argc, argv);
    uint32_t deviceID = options.device_id;

    auto d = u.get_device(deviceID);

    vector<field> fields = {
        field("ID", deviceID),
        field("address", d.address),
        field("subnet", d.subnet),
        field("gateway", d.gateway),
        field("MAC", d.MAC),
        field("version", d.version),
        field("released", d.date),
    };

    display("get-controller", fields);
}

void setAddress(uhppoted &u, int argc, char **argv) {
    auto options = parse(argc, argv);
    uint32_t deviceID = options.device_id;
    string address = options.ip_address;
    string subnet = options.subnet_mask;
    string gateway = options.gateway;

    u.set_address(deviceID, address, subnet, gateway);

    vector<field> fields = {
        field("ID", deviceID),
        field("address", address),
        field("subnet", subnet),
        field("gateway", gateway),
    };

    display("set-address", fields);
}

void getStatus(uhppoted &u, int argc, char **argv) {
    auto options = parse(argc, argv);
    uint32_t deviceID = options.device_id;

    auto s = u.get_status(deviceID);

    string timestamp = s.evt.timestamp;
    if (s.evt.timestamp == "") {
        timestamp = "-";
    }

    vector<field> fields = {
        field("ID", s.ID),
        field("date/time", s.sysdatetime),
        field("doors[1]", s.doors[0]),
        field("doors[2]", s.doors[1]),
        field("doors[3]", s.doors[2]),
        field("doors[4]", s.doors[3]),
        field("buttons[1]", s.buttons[0]),
        field("buttons[2]", s.buttons[1]),
        field("buttons[3]", s.buttons[2]),
        field("buttons[4]", s.buttons[3]),
        field("relays", s.relays),
        field("inputs", s.inputs),
        field("error", s.syserror),
        field("seq no.", s.seqno),
        field("info", s.info),
        field("event timestamp", timestamp),
        field("      index", s.evt.index),
        field("      type", u.lookup(LOOKUP_EVENT_TYPE, s.evt.eventType, LOCALE)),
        field("      granted", s.evt.granted),
        field("      door", s.evt.door),
        field("      direction", u.lookup(LOOKUP_DIRECTION, s.evt.direction, LOCALE)),
        field("      card", s.evt.card),
        field("      reason", u.lookup(LOOKUP_EVENT_REASON, s.evt.reason, LOCALE)),
    };

    display("get-status", fields);
}

void getTime(uhppoted &u, int argc, char **argv) {
    auto options = parse(argc, argv);
    uint32_t deviceID = options.device_id;

    auto datetime = u.get_time(deviceID);

    vector<field> fields = {
        field("ID", deviceID),
        field("date/time", datetime),
    };

    display("get-time", fields);
}

void setTime(uhppoted &u, int argc, char **argv) {
    auto options = parse(argc, argv);
    uint32_t deviceID = options.device_id;
    char s[20];
    time_t now = time(nullptr);

    strftime(s, sizeof(s), "%Y-%m-%d %H:%M:%S", localtime(&now));

    string datetime = s;

    u.set_time(deviceID, datetime);

    vector<field> fields = {
        field("ID", deviceID),
        field("date/time", datetime),
    };

    display("set-time", fields);
}

void getListener(uhppoted &u, int argc, char **argv) {
    auto options = parse(argc, argv);
    uint32_t deviceID = options.device_id;

    auto listener = u.get_listener(deviceID);
    uint8_t interval = u.get_listener_interval(deviceID);

    vector<field> fields = {
        field("ID", deviceID),
        field("event listener", listener),
        field("interval", interval),
    };

    display("get-listener", fields);
}

void setListener(uhppoted &u, int argc, char **argv) {
    auto options = parse(argc, argv);
    uint32_t deviceID = options.device_id;
    string listener = options.listener;
    uint8_t interval = options.auto_send_interval;

    u.set_listener(deviceID, listener, interval);

    vector<field> fields = {
        field("ID", deviceID),
        field("event listener", listener),
        field("interval", interval),
    };

    display("set-listener", fields);
}

void getDoorControl(uhppoted &u, int argc, char **argv) {
    auto options = parse(argc, argv);
    uint32_t deviceID = options.device_id;
    uint32_t door = options.door;

    auto d = u.get_door_control(deviceID, door);

    vector<field> fields = {
        field("ID", deviceID),
        field("door", door),
        field("mode", u.lookup(LOOKUP_MODE, d.mode, LOCALE)),
        field("delay", d.delay),
    };

    display("get-door-control", fields);
}

void setDoorControl(uhppoted &u, int argc, char **argv) {
    auto options = parse(argc, argv);
    uint32_t deviceID = options.device_id;
    uint32_t door = options.door;
    uint8_t mode = NORMALLY_OPEN;
    uint8_t delay = 9;

    u.set_door_control(deviceID, door, mode, delay);

    vector<field> fields = {
        field("ID", deviceID),
        field("door", door),
        field("mode", u.lookup(LOOKUP_MODE, mode, LOCALE)),
        field("delay", delay),
    };

    display("set-door-control", fields);
}

void openDoor(uhppoted &u, int argc, char **argv) {
    auto options = parse(argc, argv);
    uint32_t deviceID = options.device_id;
    uint32_t door = options.door;

    u.open_door(deviceID, door);

    vector<field> fields = {
        field("ID", deviceID),
        field("door", door),
    };

    display("open-door", fields);
}

void setPCControl(uhppoted &u, int argc, char **argv) {
    auto options = parse(argc, argv);
    uint32_t deviceID = options.device_id;
    bool enabled = true;

    u.set_pc_control(deviceID, enabled);

    vector<field> fields = {
        field("ID", deviceID),
        field("enabled", enabled),
    };

    display("set-pc-control", fields);
}

void setInterlock(uhppoted &u, int argc, char **argv) {
    auto options = parse(argc, argv);
    uint32_t deviceID = options.device_id;
    uint8_t interlock = 1;

    u.set_interlock(deviceID, interlock);

    vector<field> fields = {
        field("ID", deviceID),
        field("interlock", interlock),
    };

    display("set-interlock", fields);
}

void activateKeypads(uhppoted &u, int argc, char **argv) {
    auto options = parse(argc, argv);
    uint32_t controller = options.device_id;
    bool reader1 = true;
    bool reader2 = true;
    bool reader3 = false;
    bool reader4 = true;

    u.activate_keypads(controller, reader1, reader2, reader3, reader4);

    vector<field> fields = {
        field("ID", controller),
        field("reader 1", reader1),
        field("reader 2", reader2),
        field("reader 3", reader3),
        field("reader 4", reader4),
    };

    display("activate-keypads", fields);
}

void setDoorPasscodes(uhppoted &u, int argc, char **argv) {
    auto options = parse(argc, argv);
    uint32_t controller = options.device_id;
    uint8_t door = options.door;
    uint32_t passcode1 = 12345;
    uint32_t passcode2 = 999999;
    uint32_t passcode3 = 0;
    uint32_t passcode4 = 54321;

    u.set_door_passcodes(controller, door, passcode1, passcode2, passcode3, passcode4);

    vector<field> fields = {
        field("ID", controller),
        field("door", door),
        field("passcode 1", passcode1),
        field("passcode 2", passcode2),
        field("passcode 3", passcode3),
        field("passcode 4", passcode4),
    };

    display("set-door-passcodes", fields);
}

void getAntiPassback(uhppoted &u, int argc, char **argv) {
    auto options = parse(argc, argv);
    uint32_t controller = options.device_id;
    uint8_t antipassback = u.get_antipassback(controller);

    vector<field> fields = {
        field("ID", controller),
        field("antipassback", antipassback),
    };

    display("set-antipassback", fields);
}

void setAntiPassback(uhppoted &u, int argc, char **argv) {
    auto options = parse(argc, argv);
    uint32_t controller = options.device_id;
    uint8_t antipassback = options.antipassback;

    u.set_antipassback(controller, antipassback);

    vector<field> fields = {
        field("ID", controller),
        field("antipassback", antipassback),
    };

    display("set-antipassback", fields);
}

void restoreDefaultParameters(uhppoted &u, int argc, char **argv) {
    auto options = parse(argc, argv);
    uint32_t controller = options.device_id;

    u.restore_default_parameters(controller);

    vector<field> fields = {
        field("ID", controller),
    };

    display("restore-default-parameters", fields);
}
