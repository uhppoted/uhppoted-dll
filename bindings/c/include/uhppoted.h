#pragma once

#include <stdbool.h>
#include <stdint.h>

#define NORMALLY_OPEN 1
#define NORMALLY_CLOSED 2
#define CONTROLLED 3

#define DIRECTION_IN 1
#define DIRECTION_OUT 2

#define EVENT_TYPE_NONE 0
#define EVENT_TYPE_SWIPE 1
#define EVENT_TYPE_DOOR 2
#define EVENT_TYPE_ALARM 3
#define EVENT_TYPE_OVERWRITTEN 0xff

#define EVENT_REASON_NONE 0
#define EVENT_REASON_SWIPE 1
#define EVENT_REASON_DENIED 5
#define EVENT_REASON_NO_ACCESS_RIGHTS 6
#define EVENT_REASON_INCORRECT_PASSWORD 7
#define EVENT_REASON_ANTI_PASSBACK 8
#define EVENT_REASON_MORE_CARDS 9
#define EVENT_REASON_FIRST_CARD_OPEN 10
#define EVENT_REASON_DOOR_IS_NORMALLY_CLOSED 11
#define EVENT_REASON_INTERLOCK 12
#define EVENT_REASON_NOT_IN_ALLOWED_TIME_PERIOD 13
#define EVENT_REASON_INVALID_TIMEZONE 15
#define EVENT_REASON_ACCESS_DENIED 18
#define EVENT_REASON_PUSH_BUTTON_OK 20
#define EVENT_REASON_DOOR_OPENED 23
#define EVENT_REASON_DOOR_CLOSED 24
#define EVENT_REASON_DOOR_OPENED_SUPERVISOR_PASSWORD 25
#define EVENT_REASON_CONTROLLER_POWER_ON 28
#define EVENT_REASON_CONTROLLER_RESET 29
#define EVENT_REASON_PUSHBUTTON_INVALID_DOOR_LOCKED 31
#define EVENT_REASON_PUSHBUTTON_INVALID_OFFLINE 32
#define EVENT_REASON_PUSHBUTTON_INVALID_INTERLOCK 33
#define EVENT_REASON_PUSHBUTTON_INVALID_THREAT 34
#define EVENT_REASON_DOOR_OPEN_TOO_LONG 37
#define EVENT_REASON_FORCED_OPEN 38
#define EVENT_REASON_FIRE 39
#define EVENT_REASON_FORCED_CLOSED 40
#define EVENT_REASON_THEFT_PREVENTION 41
#define EVENT_REASON_24X7_ZONE 42
#define EVENT_REASON_EMERGENCY 43
#define EVENT_REASON_REMOTE_OPEN_DOOR 44
#define EVENT_REASON_REMOTE_OPEN_DOOR_USB_READER 45

extern const char *LOOKUP_MODE;
extern const char *LOOKUP_DIRECTION;
extern const char *LOOKUP_EVENT_TYPE;
extern const char *LOOKUP_EVENT_REASON;

typedef struct controller {
    uint32_t id;
    const char *address;
} controller;

typedef struct device {
    uint32_t ID;
    char address[16];
    char subnet[16];
    char gateway[16];
    char MAC[18];
    char version[6];
    char date[11];
} device;

typedef struct event {
    char timestamp[20];
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
    char sysdatetime[20];
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
    char from[11];
    char to[11];
    uint8_t doors[4];
} card;

typedef struct time_profile {
    uint8_t ID;
    uint8_t linked;
    char from[11];
    char to[11];
    bool monday;
    bool tuesday;
    bool wednesday;
    bool thursday;
    bool friday;
    bool saturday;
    bool sunday;
    char segment1start[6];
    char segment1end[6];
    char segment2start[6];
    char segment2end[6];
    char segment3start[6];
    char segment3end[6];
} time_profile;

typedef struct task {
    uint8_t task;
    uint8_t door;
    char from[11];
    char to[11];
    bool monday;
    bool tuesday;
    bool wednesday;
    bool thursday;
    bool friday;
    bool saturday;
    bool sunday;
    char at[6];
    uint8_t cards;
} task;

void setup(const char *bind, const char *broadcast, const char *listen, int timeout, int debug, ...);
void teardown();
const char *errmsg();
const char *lookup(const char *, uint8_t, const char *);

int get_devices(uint32_t **devices, int *N);
int get_device(uint32_t id, struct device *);
int set_address(uint32_t id, const char *address, const char *subnet,
                const char *gateway);
int get_status(uint32_t id, struct status *);
int get_time(uint32_t id, char **);
int set_time(uint32_t id, char *);
int get_listener(uint32_t id, char **);
int set_listener(uint32_t id, char *);
int get_door_control(uint32_t id, uint8_t door, struct door_control *);
int set_door_control(uint32_t id, uint8_t door, uint8_t mode, uint8_t delay);
int open_door(uint32_t id, uint8_t door);

int get_cards(uint32_t id, int *N);
int get_card(uint32_t id, uint32_t card_number, card *card);
int get_card_by_index(uint32_t id, uint32_t index, card *card);
int put_card(uint32_t id, uint32_t card_number, const char *from, const char *to, const uint8_t doors[4]);
int delete_card(uint32_t id, uint32_t card_number);
int delete_cards(uint32_t id);

int get_event_index(uint32_t id, uint32_t *index);
int set_event_index(uint32_t id, uint32_t index);
int get_event(uint32_t id, uint32_t index, event *event);
int record_special_events(uint32_t id, bool enabled);

int get_time_profile(uint32_t id, uint8_t profile_id, time_profile *profile);
int set_time_profile(uint32_t id, time_profile *profile);
int clear_time_profiles(uint32_t id);

int add_task(uint32_t id, task *task);
int refresh_tasklist(uint32_t id);
int clear_tasklist(uint32_t id);
