#include <iostream>
#include <map>
#include <stdlib.h>

#include "../include/uhppoted.hpp"

using namespace std;

const string LOOKUP_MODE = "door.mode";
const string LOOKUP_DIRECTION = "event.direction";
const string LOOKUP_EVENT_TYPE = "event.type";
const string LOOKUP_EVENT_REASON = "event.reason";

const char *ModeNormallyOpen = "normally open";
const char *ModeNormallyClosed = "normally closed";
const char *ModeControlled = "controlled";
const char *ModeUnknown = "unknown";

const char *DirectionIn = "in";
const char *DirectionOut = "out";
const char *DirectionUnknown = "unknown";

const char *EventTypeNone = "none";
const char *EventTypeSwipe = "swipe";
const char *EventTypeDoor = "door";
const char *EventTypeAlarm = "alarm";
const char *EventTypeOverwritten = "overwritten";
const char *EventTypeUnknown = "unknown";

const char *EventReasonNone = "";
const char *EventReasonSwipe = "swipe";
const char *EventReasonSwipeOpen = "swipe open";
const char *EventReasonSwipeClose = "swipe close";
const char *EventReasonDenied = "swipe:denied (system)";
const char *EventReasonNoAccessRights = "no access rights";
const char *EventReasonIncorrectPassword = "incorrect password";
const char *EventReasonAntiPassback = "anti-passback";
const char *EventReasonMoreCards = "more cards";
const char *EventReasonFirstCardOpen = "first card open";
const char *EventReasonDoorIsNormallyClosed = "door is normally closed";
const char *EventReasonInterlock = "interlock";
const char *EventReasonNotInAllowedTimePeriod = "not in allowed time period";
const char *EventReasonInvalidTimezone = "invalid timezone";
const char *EventReasonAccessDenied = "access denied";
const char *EventReasonPushButtonOk = "pushbutton ok";
const char *EventReasonDoorOpened = "door opened";
const char *EventReasonDoorClosed = "door closed";
const char *EventReasonDoorOpenedSupervisorPassword = "door opened (supervisor password)";
const char *EventReasonControllerPowerOn = "controller power on";
const char *EventReasonControllerReset = "controller reset";
const char *EventReasonPushbuttonInvalidDoorLocked = "pushbutton invalid (door locked)";
const char *EventReasonPushbuttonInvalidOffline = "pushbutton invalid (offline)";
const char *EventReasonPushbuttonInvalidInterlock = "pushbutton invalid (interlock)";
const char *EventReasonPushbuttonInvalidThreat = "pushbutton invalid (threat)";
const char *EventReasonDoorOpenTooLong = "door open too long";
const char *EventReasonForcedOpen = "forced open";
const char *EventReasonFire = "fire";
const char *EventReasonForcedClosed = "forced closed";
const char *EventReasonTheftPrevention = "theft prevention";
const char *EventReason24x7Zone = "24x7 zone";
const char *EventReasonEmergency = "emergency";
const char *EventReasonRemoteOpenDoor = "remote open door";
const char *EventReasonRemoteOpenDoorUSBReader = "remote open door (USB reader)";
const char *EventReasonUnknown = "unknown";

uhppoted_exception::uhppoted_exception(char *err) {
    message = std::string(err);
    free(err);
}

uhppoted_exception::uhppoted_exception(const char *err, int N) { message = std::string(err, N); }

uhppoted_exception::~uhppoted_exception() {}

const char *uhppoted_exception::what() const noexcept { return message.c_str(); }

uhppoted::uhppoted() { u = nullptr; }

/* (optional) setup for UHPPOTE network configuration. Defaults to:
 * - bind:        0.0.0.0:0
 * - broadcast:   255.255.255.255:60000
 * - listen:      0.0.0.0:60001
 * - timeout:     5s
 * - controllers: (none)
 * - debug:       false
 *
 */
uhppoted::uhppoted(const string &_bind, const string &_broadcast, const string &_listen, int timeout, const vector<controller> controllers,
                   bool debug)
    : bind_addr(_bind), broadcast_addr(_broadcast), listen_addr(_listen) {
    if ((u = new UHPPOTE) != nullptr) {
        u->bind = this->bind_addr.c_str();
        u->broadcast = this->broadcast_addr.c_str();
        u->listen = this->listen_addr.c_str();
        u->timeout = timeout;
        u->devices = nullptr;
        u->debug = debug;

        udevices *devices;
        udevice *list;
        uint32_t N = controllers.size();
        int ix = 0;

        if ((devices = new udevices) == nullptr) {
            return;
        }

        if ((list = new udevice[N]) == nullptr) {
            delete devices;
            return;
        }

        for (auto p : controllers) {
            // NTS: reallocate const char * fields because the controllers may go out of scope
            // after the invocation of the constructor and c_str() returns a pointer to the
            // underlying string char array
            size_t N = p.address.size() + 1;
            size_t M = p.transport.size() + 1;
            char *addr = new char[N];
            char *transport = new char[M];

            memset(addr, 0, N); // NTS: supposedly new char[N} zeroes out the memory, but apparently not
            p.address.copy(addr, N);

            memset(transport, 0, M); // NTS: supposedly new char[N} zeroes out the memory, but apparently not
            p.transport.copy(transport, M);

            list[ix].id = p.id;
            list[ix].address = addr;
            list[ix].transport = transport;
            ix++;
        }

        u->devices = devices;
        u->devices->N = N;
        u->devices->devices = list;
    }
}

uhppoted::~uhppoted() {
    if (u != nullptr) {
        udevices *devices;

        if ((devices = u->devices) != nullptr) {
            delete[] devices->devices;
            delete devices;
        }
    }

    delete u;
}

vector<uint32_t> uhppoted::get_devices() {
    vector<uint32_t> list;
    int allocated = 0;

    for (;;) {
        allocated += 16;
        list.resize(allocated);

        int count = allocated;
        char err[256] = "";
        int errN = sizeof(err);
        int rc;

        if ((rc = GetDevices(u, list.data(), &count, err, &errN)) != 0) {
            throw uhppoted_exception(err, errN);
        }

        if (count <= allocated) {
            vector<uint32_t> devices;
            for (int i = 0; i < count; i++) {
                devices.push_back(list[i]);
            }

            return devices;
        }
    }
}

struct device uhppoted::get_device(uint32_t id) {
    char err[256] = "";
    int errN = sizeof(err);
    int rc;

    char address[16];
    char subnet[16];
    char gateway[16];
    char MAC[18];
    char version[7];
    char date[11];

    struct Device device = {
        .address = address,
        .subnet = subnet,
        .gateway = gateway,
        .MAC = MAC,
        .version = version,
        .date = date,
    };

    if ((rc = GetDevice(u, &device, id, err, &errN)) != 0) {
        throw uhppoted_exception(err, errN);
    }

    struct device d;

    d.ID = device.ID;
    d.address = device.address;
    d.subnet = device.subnet;
    d.gateway = device.gateway;
    d.MAC = device.MAC;
    d.version = device.version;
    d.date = device.date;

    return d;
}

void uhppoted::set_address(uint32_t id, std::string &address, std::string &subnet, std::string &gateway) {
    char err[256] = "";
    int errN = sizeof(err);
    int rc;

    if ((rc = SetAddress(u, id, (char *)address.c_str(), (char *)subnet.c_str(), (char *)gateway.c_str(), err, &errN)) != 0) {
        throw uhppoted_exception(err, errN);
    }
}

status uhppoted::get_status(unsigned id) {
    char err[256] = "";
    int errN = sizeof(err);
    int rc;

    char sysdatetime[20];
    vector<uint8_t> doors(4);
    vector<uint8_t> buttons(4);
    char timestamp[20];

    struct Event event = {
        .timestamp = timestamp,
    };

    struct Status status = {
        .sysdatetime = sysdatetime,
        .doors = doors.data(),
        .buttons = buttons.data(),
        .event = &event,
    };

    if ((rc = GetStatus(u, &status, id, err, &errN)) != 0) {
        throw uhppoted_exception(err, errN);
    }

    struct status s;

    s.ID = status.ID;
    s.sysdatetime = status.sysdatetime;

    s.doors[0] = doors[0];
    s.doors[1] = doors[1];
    s.doors[2] = doors[2];
    s.doors[3] = doors[3];

    s.buttons[0] = buttons[0];
    s.buttons[1] = buttons[1];
    s.buttons[2] = buttons[2];
    s.buttons[3] = buttons[3];

    s.relays = status.relays;
    s.inputs = status.inputs;
    s.syserror = status.syserror;
    s.seqno = status.seqno;
    s.info = status.info;

    if (status.event) {
        s.evt.timestamp = status.event->timestamp;
        s.evt.index = status.event->index;
        s.evt.eventType = status.event->eventType;
        s.evt.granted = status.event->granted;
        s.evt.door = status.event->door;
        s.evt.direction = status.event->direction;
        s.evt.card = status.event->card;
        s.evt.reason = status.event->reason;
    }

    return s;
}

string uhppoted::get_time(uint32_t id) {
    char err[256] = "";
    int errN = sizeof(err);
    int rc;
    char datetime[20] = "";

    if ((rc = GetTime(u, datetime, id, err, &errN)) != 0) {
        throw uhppoted_exception(err, errN);
    }

    return string(datetime);
}

void uhppoted::set_time(uint32_t id, std::string &datetime) {
    char err[256] = "";
    int errN = sizeof(err);
    int rc;

    if ((rc = SetTime(u, id, (char *)datetime.c_str(), err, &errN)) != 0) {
        throw uhppoted_exception(err, errN);
    }
}

string uhppoted::get_listener(uint32_t id) {
    char err[256] = "";
    int errN = sizeof(err);
    int rc;
    char listener[22] = "";

    if ((rc = GetListener(u, listener, id, err, &errN)) != 0) {
        throw uhppoted_exception(err, errN);
    }

    return listener;
}

void uhppoted::set_listener(uint32_t id, std::string &listener) {
    char err[256] = "";
    int errN = sizeof(err);
    int rc;

    if ((rc = SetListener(u, id, listener.c_str(), err, &errN)) != 0) {
        throw uhppoted_exception(err, errN);
    }
}

struct door_control uhppoted::get_door_control(uint32_t id, uint8_t door) {
    struct DoorControl control;

    char *err = GetDoorControl(u, &control, id, door);
    if (err != nullptr) {
        throw uhppoted_exception(err);
    }

    struct door_control d;

    d.mode = control.mode;
    d.delay = control.delay;

    return d;
}

void uhppoted::set_door_control(uint32_t id, uint8_t door, uint8_t mode, uint8_t delay) {
    char *err = SetDoorControl(u, id, door, mode, delay);
    if (err != nullptr) {
        throw uhppoted_exception(err);
    }
}

void uhppoted::open_door(uint32_t id, uint8_t door) {
    char *err = OpenDoor(u, id, door);
    if (err != nullptr) {
        throw uhppoted_exception(err);
    }
}

int uhppoted::get_cards(uint32_t id) {
    int N;

    char *err = GetCards(u, &N, id);
    if (err != nullptr) {
        throw uhppoted_exception(err);
    }

    return N;
}

card uhppoted::get_card(uint32_t id, uint32_t card_number) {
    Card card;

    vector<uint8_t> doors(4);

    card.doors = doors.data();

    char *err = GetCard(u, &card, id, card_number);
    if (err != nullptr) {
        throw uhppoted_exception(err);
    }

    struct card c;

    c.card_number = card.card_number;
    c.from = card.from;
    c.to = card.to;
    c.doors[0] = card.doors[0];
    c.doors[1] = card.doors[1];
    c.doors[2] = card.doors[2];
    c.doors[3] = card.doors[3];
    c.PIN = card.PIN;

    return c;
}

card uhppoted::get_card_by_index(uint32_t id, uint32_t index) {
    Card card;

    vector<uint8_t> doors(4);

    card.doors = doors.data();

    char *err = GetCardByIndex(u, &card, id, index);
    if (err != nullptr) {
        throw uhppoted_exception(err);
    }

    struct card c;

    c.card_number = card.card_number;
    c.from = card.from;
    c.to = card.to;
    c.doors[0] = card.doors[0];
    c.doors[1] = card.doors[1];
    c.doors[2] = card.doors[2];
    c.doors[3] = card.doors[3];
    c.PIN = card.PIN;

    return c;
}

void uhppoted::put_card(uint32_t id, uint32_t card_number, string from, string to, uint8_t doors[4], uint32_t PIN) {
    char *err = PutCard(u, id, card_number, (char *)from.c_str(), (char *)to.c_str(), (uint8_t *)doors, PIN);

    if (err != nullptr) {
        throw uhppoted_exception(err);
    }
}

void uhppoted::delete_card(uint32_t id, uint32_t card_number) {
    char *err = DeleteCard(u, id, card_number);

    if (err != nullptr) {
        throw uhppoted_exception(err);
    }
}

void uhppoted::delete_cards(uint32_t id) {
    char *err = DeleteCards(u, id);

    if (err != nullptr) {
        throw uhppoted_exception(err);
    }
}

uint32_t uhppoted::get_event_index(uint32_t id) {
    uint32_t index;

    char *err = GetEventIndex(u, &index, id);
    if (err != nullptr) {
        throw uhppoted_exception(err);
    }

    return index;
}

void uhppoted::set_event_index(uint32_t id, uint32_t index) {
    char *err = SetEventIndex(u, id, index);
    if (err != nullptr) {
        throw uhppoted_exception(err);
    }
}

event uhppoted::get_event(uint32_t id, uint32_t index) {
    Event event;

    char *err = GetEvent(u, &event, id, index);
    if (err != nullptr) {
        throw uhppoted_exception(err);
    }

    struct event e;

    e.timestamp = event.timestamp;
    e.index = event.index;
    e.eventType = event.eventType;
    e.granted = event.granted;
    e.door = event.door;
    e.direction = event.direction;
    e.card = event.card;
    e.reason = event.reason;

    return e;
}

void uhppoted::record_special_events(uint32_t id, bool enabled) {
    char *err = RecordSpecialEvents(u, id, enabled);
    if (err != nullptr) {
        throw uhppoted_exception(err);
    }
}

time_profile uhppoted::get_time_profile(uint32_t id, uint8_t profile_id) {
    TimeProfile profile;

    char *err = GetTimeProfile(u, &profile, id, profile_id);
    if (err != nullptr) {
        throw uhppoted_exception(err);
    }

    struct time_profile p;

    p.ID = profile.ID;
    p.linked = profile.linked;
    p.from = profile.from;
    p.to = profile.to;

    p.monday = profile.monday;
    p.tuesday = profile.tuesday;
    p.wednesday = profile.wednesday;
    p.thursday = profile.thursday;
    p.friday = profile.friday;
    p.saturday = profile.saturday;
    p.sunday = profile.sunday;

    p.segment1start = profile.segment1start;
    p.segment1end = profile.segment1end;
    p.segment2start = profile.segment2start;
    p.segment2end = profile.segment2end;
    p.segment3start = profile.segment3start;
    p.segment3end = profile.segment3end;

    return p;
}

void uhppoted::set_time_profile(uint32_t id, const time_profile &p) {
    TimeProfile profile;

    profile.ID = p.ID;
    profile.linked = p.linked;
    profile.from = (char *)p.from.c_str();
    profile.to = (char *)p.to.c_str();
    profile.monday = p.monday;
    profile.tuesday = p.tuesday;
    profile.wednesday = p.wednesday;
    profile.thursday = p.thursday;
    profile.friday = p.friday;
    profile.saturday = p.saturday;
    profile.sunday = p.sunday;
    profile.segment1start = (char *)p.segment1start.c_str();
    profile.segment1end = (char *)p.segment1end.c_str();
    profile.segment2start = (char *)p.segment2start.c_str();
    profile.segment2end = (char *)p.segment2end.c_str();
    profile.segment3start = (char *)p.segment3start.c_str();
    profile.segment3end = (char *)p.segment3end.c_str();

    char *err = SetTimeProfile(u, id, &profile);
    if (err != nullptr) {
        throw uhppoted_exception(err);
    }
}

void uhppoted::clear_time_profiles(uint32_t id) {
    char *err = ClearTimeProfiles(u, id);

    if (err != nullptr) {
        throw uhppoted_exception(err);
    }
}

void uhppoted::add_task(uint32_t id, const task &t) {
    Task task;

    task.task = t.task;
    task.door = t.door;
    task.from = (char *)t.from.c_str();
    task.to = (char *)t.to.c_str();
    task.monday = t.monday;
    task.tuesday = t.tuesday;
    task.wednesday = t.wednesday;
    task.thursday = t.thursday;
    task.friday = t.friday;
    task.saturday = t.saturday;
    task.sunday = t.sunday;
    task.at = (char *)t.at.c_str();
    task.cards = t.cards;

    char *err = AddTask(u, id, &task);
    if (err != nullptr) {
        throw uhppoted_exception(err);
    }
}

void uhppoted::refresh_tasklist(uint32_t id) {
    char *err = RefreshTaskList(u, id);
    if (err != nullptr) {
        throw uhppoted_exception(err);
    }
}

void uhppoted::clear_tasklist(uint32_t id) {
    char *err = ClearTaskList(u, id);
    if (err != nullptr) {
        throw uhppoted_exception(err);
    }
}

void uhppoted::set_pc_control(uint32_t controller, bool enabled) {
    char *err = SetPCControl(u, controller, enabled);
    if (err != nullptr) {
        throw uhppoted_exception(err);
    }
}

void uhppoted::set_interlock(uint32_t controller, uint8_t interlock) {
    char *err = SetInterlock(u, controller, interlock);
    if (err != nullptr) {
        throw uhppoted_exception(err);
    }
}

void uhppoted::activate_keypads(uint32_t controller, bool reader1, bool reader2, bool reader3, bool reader4) {
    char *err = ActivateKeypads(u, controller, reader1, reader2, reader3, reader4);
    if (err != nullptr) {
        throw uhppoted_exception(err);
    }
}

void uhppoted::set_door_passcodes(uint32_t controller, uint8_t door, uint32_t passcode1, uint32_t passcode2, uint32_t passcode3,
                                  uint32_t passcode4) {
    char *err = SetDoorPasscodes(u, controller, door, passcode1, passcode2, passcode3, passcode4);
    if (err != nullptr) {
        throw uhppoted_exception(err);
    }
}

void uhppoted::restore_default_parameters(uint32_t controller) {
    char *err = RestoreDefaultParameters(u, controller);
    if (err != nullptr) {
        throw uhppoted_exception(err);
    }
}

void uhppoted::listen(on_event handler, bool *listening, bool *stop, on_error err_handler, void *userdata) {
    int32_t err = Listen(u, handler, (uint8_t *)listening, (uint8_t *)stop, err_handler, userdata);
    if (err != 0) {
        throw uhppoted_exception((char *)"error starting event listener");
    }
}

const map<uint8_t, string> LookupMode = {
    {NORMALLY_OPEN, ModeNormallyOpen},
    {NORMALLY_CLOSED, ModeNormallyClosed},
    {CONTROLLED, ModeControlled},
};

const map<uint8_t, string> LookupDirection = {
    {DIRECTION_IN, DirectionIn},
    {DIRECTION_OUT, DirectionOut},
};

const map<uint8_t, string> LookupEventType = {
    {EVENT_TYPE_NONE, EventTypeNone},
    {EVENT_TYPE_SWIPE, EventTypeSwipe},
    {EVENT_TYPE_DOOR, EventTypeDoor},
    {EVENT_TYPE_ALARM, EventTypeAlarm},
    {EVENT_TYPE_OVERWRITTEN, EventTypeOverwritten},
};

const map<uint8_t, string> LookupEventReason = {
    {EVENT_REASON_NONE, EventReasonNone},
    {EVENT_REASON_SWIPE, EventReasonSwipe},
    {EVENT_REASON_SWIPE_OPEN, EventReasonSwipeOpen},
    {EVENT_REASON_SWIPE_CLOSE, EventReasonSwipeClose},
    {EVENT_REASON_DENIED, EventReasonDenied},
    {EVENT_REASON_NO_ACCESS_RIGHTS, EventReasonNoAccessRights},
    {EVENT_REASON_INCORRECT_PASSWORD, EventReasonIncorrectPassword},
    {EVENT_REASON_ANTI_PASSBACK, EventReasonAntiPassback},
    {EVENT_REASON_MORE_CARDS, EventReasonMoreCards},
    {EVENT_REASON_FIRST_CARD_OPEN, EventReasonFirstCardOpen},
    {EVENT_REASON_DOOR_IS_NORMALLY_CLOSED, EventReasonDoorIsNormallyClosed},
    {EVENT_REASON_INTERLOCK, EventReasonInterlock},
    {EVENT_REASON_NOT_IN_ALLOWED_TIME_PERIOD, EventReasonNotInAllowedTimePeriod},
    {EVENT_REASON_INVALID_TIMEZONE, EventReasonInvalidTimezone},
    {EVENT_REASON_ACCESS_DENIED, EventReasonAccessDenied},
    {EVENT_REASON_PUSHBUTTON_OK, EventReasonPushButtonOk},
    {EVENT_REASON_DOOR_OPENED, EventReasonDoorOpened},
    {EVENT_REASON_DOOR_CLOSED, EventReasonDoorClosed},
    {EVENT_REASON_DOOR_OPENED_SUPERVISOR_PASSWORD, EventReasonDoorOpenedSupervisorPassword},
    {EVENT_REASON_CONTROLLER_POWER_ON, EventReasonControllerPowerOn},
    {EVENT_REASON_CONTROLLER_RESET, EventReasonControllerReset},
    {EVENT_REASON_PUSHBUTTON_INVALID_DOOR_LOCKED, EventReasonPushbuttonInvalidDoorLocked},
    {EVENT_REASON_PUSHBUTTON_INVALID_OFFLINE, EventReasonPushbuttonInvalidOffline},
    {EVENT_REASON_PUSHBUTTON_INVALID_INTERLOCK, EventReasonPushbuttonInvalidInterlock},
    {EVENT_REASON_PUSHBUTTON_INVALID_THREAT, EventReasonPushbuttonInvalidThreat},
    {EVENT_REASON_DOOR_OPEN_TOO_LONG, EventReasonDoorOpenTooLong},
    {EVENT_REASON_FORCED_OPEN, EventReasonForcedOpen},
    {EVENT_REASON_FIRE, EventReasonFire},
    {EVENT_REASON_FORCED_CLOSED, EventReasonForcedClosed},
    {EVENT_REASON_THEFT_PREVENTION, EventReasonTheftPrevention},
    {EVENT_REASON_ZONE_24X7, EventReason24x7Zone},
    {EVENT_REASON_EMERGENCY, EventReasonEmergency},
    {EVENT_REASON_REMOTE_OPEN_DOOR, EventReasonRemoteOpenDoor},
    {EVENT_REASON_REMOTE_OPEN_DOOR_USB_READER, EventReasonRemoteOpenDoorUSBReader},
};

const map<string, map<uint8_t, string>> dictionaries = {
    {LOOKUP_MODE, LookupMode},
    {LOOKUP_DIRECTION, LookupDirection},
    {LOOKUP_EVENT_TYPE, LookupEventType},
    {LOOKUP_EVENT_REASON, LookupEventReason},
};

const map<string, string> unknown = {
    {LOOKUP_MODE, ModeUnknown},
    {LOOKUP_DIRECTION, DirectionUnknown},
    {LOOKUP_EVENT_TYPE, EventTypeUnknown},
    {LOOKUP_EVENT_REASON, EventReasonUnknown},
};

const string uhppoted::lookup(const string &category, uint8_t code, const string &locale) {
    auto dictionary = dictionaries.find(category);

    if (dictionary != dictionaries.end()) {
        auto it = dictionary->second.find(code);
        if (it != dictionary->second.end()) {
            return it->second;
        }

        return unknown.at(category);
    }

    return "?";
}