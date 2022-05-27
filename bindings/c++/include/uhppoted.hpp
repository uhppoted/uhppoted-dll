#pragma once

#include <memory>
#include <string>
#include <vector>

#include "libuhppoted.h"

const uint8_t NORMALLY_OPEN = 1;
const uint8_t NORMALLY_CLOSED = 2;
const uint8_t CONTROLLED = 3;

const uint8_t DIRECTION_IN = 1;
const uint8_t DIRECTION_OUT = 2;

const uint8_t EVENT_TYPE_NONE = 0;
const uint8_t EVENT_TYPE_SWIPE = 1;
const uint8_t EVENT_TYPE_DOOR = 2;
const uint8_t EVENT_TYPE_ALARM = 3;
const uint8_t EVENT_TYPE_OVERWRITTEN = 0xff;

const uint8_t EVENT_REASON_NONE = 0;
const uint8_t EVENT_REASON_SWIPE = 1;
const uint8_t EVENT_REASON_DENIED = 5;
const uint8_t EVENT_REASON_NO_ACCESS_RIGHTS = 6;
const uint8_t EVENT_REASON_INCORRECT_PASSWORD = 7;
const uint8_t EVENT_REASON_ANTI_PASSBACK = 8;
const uint8_t EVENT_REASON_MORE_CARDS = 9;
const uint8_t EVENT_REASON_FIRST_CARD_OPEN = 10;
const uint8_t EVENT_REASON_DOOR_IS_NORMALLY_CLOSED = 11;
const uint8_t EVENT_REASON_INTERLOCK = 12;
const uint8_t EVENT_REASON_NOT_IN_ALLOWED_TIME_PERIOD = 13;
const uint8_t EVENT_REASON_INVALID_TIMEZONE = 15;
const uint8_t EVENT_REASON_ACCESS_DENIED = 18;
const uint8_t EVENT_REASON_PUSH_BUTTON_OK = 20;
const uint8_t EVENT_REASON_DOOR_OPENED = 23;
const uint8_t EVENT_REASON_DOOR_CLOSED = 24;
const uint8_t EVENT_REASON_DOOR_OPENED_SUPERVISOR_PASSWORD = 25;
const uint8_t EVENT_REASON_CONTROLLER_POWER_ON = 28;
const uint8_t EVENT_REASON_CONTROLLER_RESET = 29;
const uint8_t EVENT_REASON_PUSHBUTTON_INVALID_DOOR_LOCKED = 31;
const uint8_t EVENT_REASON_PUSHBUTTON_INVALID_OFFLINE = 32;
const uint8_t EVENT_REASON_PUSHBUTTON_INVALID_INTERLOCK = 33;
const uint8_t EVENT_REASON_PUSHBUTTON_INVALID_THREAT = 34;
const uint8_t EVENT_REASON_DOOR_OPEN_TOO_LONG = 37;
const uint8_t EVENT_REASON_FORCED_OPEN = 38;
const uint8_t EVENT_REASON_FIRE = 39;
const uint8_t EVENT_REASON_FORCED_CLOSED = 40;
const uint8_t EVENT_REASON_THEFT_PREVENTION = 41;
const uint8_t EVENT_REASON_24X7_ZONE = 42;
const uint8_t EVENT_REASON_EMERGENCY = 43;
const uint8_t EVENT_REASON_REMOTE_OPEN_DOOR = 44;
const uint8_t EVENT_REASON_REMOTE_OPEN_DOOR_USB_READER = 45;

extern const std::string LOOKUP_MODE;
extern const std::string LOOKUP_DIRECTION;
extern const std::string LOOKUP_EVENT_TYPE;
extern const std::string LOOKUP_EVENT_REASON;

typedef struct controller {
    uint32_t id;
    std::string address;
} controller;

typedef struct device {
    uint32_t ID;
    std::string address;
    std::string subnet;
    std::string gateway;
    std::string MAC;
    std::string version;
    std::string date;
} device;

typedef struct event {
    std::string timestamp;
    uint32_t index;
    uint8_t eventType;
    bool granted;
    uint8_t door;
    uint8_t direction;
    uint32_t card;
    uint8_t reason;
} event;

typedef struct status {
    uint32_t ID;
    std::string sysdatetime;
    bool doors[4];
    bool buttons[4];
    uint8_t relays;
    uint8_t inputs;
    uint8_t syserror;
    uint8_t info;
    uint32_t seqno;
    event evt;
} status;

typedef struct door_control {
    uint8_t mode;
    uint8_t delay;
} door_control;

typedef struct card {
    uint32_t card_number;
    std::string from;
    std::string to;
    uint8_t doors[4];
} card;

typedef struct time_profile {
    uint8_t ID;
    uint8_t linked;
    std::string from;
    std::string to;
    bool monday;
    bool tuesday;
    bool wednesday;
    bool thursday;
    bool friday;
    bool saturday;
    bool sunday;
    std::string segment1start;
    std::string segment1end;
    std::string segment2start;
    std::string segment2end;
    std::string segment3start;
    std::string segment3end;
} time_profile;

typedef struct task {
    uint8_t task;
    uint8_t door;
    std::string from;
    std::string to;
    bool monday;
    bool tuesday;
    bool wednesday;
    bool thursday;
    bool friday;
    bool saturday;
    bool sunday;
    std::string at;
    uint8_t cards;
} task;

class uhppoted {
  public:
    uhppoted();
    uhppoted(const std::string bind, const std::string broadcast,
             const std::string listen, int timeout,
             const std::vector<controller> controllers, bool debug);
    virtual ~uhppoted();

  public:
    std::vector<uint32_t> get_devices();
    device get_device(uint32_t id);
    void set_address(uint32_t id, std::string &address, std::string &subnet,
                     std::string &gateway);
    status get_status(uint32_t id);
    std::string get_time(uint32_t id);
    void set_time(uint32_t id, std::string &);
    std::string get_listener(uint32_t id);
    void set_listener(uint32_t id, std::string &);
    door_control get_door_control(uint32_t id, uint8_t door);
    void set_door_control(uint32_t id, uint8_t door, uint8_t mode, uint8_t delay);
    void open_door(uint32_t id, uint8_t door);

    int get_cards(uint32_t id);
    card get_card(uint32_t id, uint32_t card_number);
    card get_card_by_index(uint32_t id, uint32_t index);
    void put_card(uint32_t id, uint32_t card_number, std::string from, std::string to, uint8_t doors[4]);
    void delete_card(uint32_t id, uint32_t card_number);
    void delete_cards(uint32_t id);

    uint32_t get_event_index(uint32_t id);
    void set_event_index(uint32_t id, uint32_t index);
    event get_event(uint32_t id, uint32_t index);
    void record_special_events(uint32_t id, bool enabled);

    time_profile get_time_profile(uint32_t id, uint8_t profile_id);
    void set_time_profile(uint32_t id, const time_profile &profile);
    void clear_time_profiles(uint32_t id);

    void add_task(uint32_t id, const task &task);
    void refresh_tasklist(uint32_t id);
    void clear_tasklist(uint32_t id);

    const std::string lookup(const std::string &, uint8_t, const std::string &);

  private:
    UHPPOTE *u;
};

// Ref. https://www.boost.org/community/error_handling.html
class uhppoted_exception : public virtual std::exception {
  public:
    uhppoted_exception(char *);
    virtual ~uhppoted_exception();

    virtual const char *what() const noexcept;

  private:
    std::shared_ptr<char *> message;
};