#include <iomanip>
#include <iostream>

#include "../include/uhppoted.hpp"
#include "tests.hpp"

using namespace std;

bool internationalisation(uhppoted &u) {
    const string normally_open = u.lookup(LOOKUP_MODE, NORMALLY_OPEN, "");
    const string normally_closed = u.lookup(LOOKUP_MODE, NORMALLY_CLOSED, "");
    const string controlled = u.lookup(LOOKUP_MODE, CONTROLLED, "");

    const string direction_in = u.lookup(LOOKUP_DIRECTION, DIRECTION_IN, "");
    const string direction_out = u.lookup(LOOKUP_DIRECTION, DIRECTION_OUT, "");

    const string event_type_none = u.lookup(LOOKUP_EVENT_TYPE, EVENT_TYPE_NONE, "");
    const string event_type_swipe = u.lookup(LOOKUP_EVENT_TYPE, EVENT_TYPE_SWIPE, "");
    const string event_type_door = u.lookup(LOOKUP_EVENT_TYPE, EVENT_TYPE_DOOR, "");
    const string event_type_alarm = u.lookup(LOOKUP_EVENT_TYPE, EVENT_TYPE_ALARM, "");
    const string event_type_overwritten = u.lookup(LOOKUP_EVENT_TYPE, EVENT_TYPE_OVERWRITTEN, "");

    const string none = u.lookup(LOOKUP_EVENT_REASON, EVENT_REASON_NONE, "");
    const string swipe = u.lookup(LOOKUP_EVENT_REASON, EVENT_REASON_SWIPE, "");
    const string swipe_open = u.lookup(LOOKUP_EVENT_REASON, EVENT_REASON_SWIPE_OPEN, "");
    const string swipe_close = u.lookup(LOOKUP_EVENT_REASON, EVENT_REASON_SWIPE_CLOSE, "");
    const string denied = u.lookup(LOOKUP_EVENT_REASON, EVENT_REASON_DENIED, "");
    const string no_access_rights = u.lookup(LOOKUP_EVENT_REASON, EVENT_REASON_NO_ACCESS_RIGHTS, "");
    const string incorrect_password = u.lookup(LOOKUP_EVENT_REASON, EVENT_REASON_INCORRECT_PASSWORD, "");
    const string anti_passback = u.lookup(LOOKUP_EVENT_REASON, EVENT_REASON_ANTI_PASSBACK, "");
    const string more_cards = u.lookup(LOOKUP_EVENT_REASON, EVENT_REASON_MORE_CARDS, "");
    const string first_card_open = u.lookup(LOOKUP_EVENT_REASON, EVENT_REASON_FIRST_CARD_OPEN, "");
    const string door_is_normally_closed = u.lookup(LOOKUP_EVENT_REASON, EVENT_REASON_DOOR_IS_NORMALLY_CLOSED, "");
    const string interlock = u.lookup(LOOKUP_EVENT_REASON, EVENT_REASON_INTERLOCK, "");
    const string not_in_allowed_time_period = u.lookup(LOOKUP_EVENT_REASON, EVENT_REASON_NOT_IN_ALLOWED_TIME_PERIOD, "");
    const string invalid_timezone = u.lookup(LOOKUP_EVENT_REASON, EVENT_REASON_INVALID_TIMEZONE, "");
    const string access_denied = u.lookup(LOOKUP_EVENT_REASON, EVENT_REASON_ACCESS_DENIED, "");
    const string push_button_ok = u.lookup(LOOKUP_EVENT_REASON, EVENT_REASON_PUSH_BUTTON_OK, "");
    const string door_opened = u.lookup(LOOKUP_EVENT_REASON, EVENT_REASON_DOOR_OPENED, "");
    const string door_closed = u.lookup(LOOKUP_EVENT_REASON, EVENT_REASON_DOOR_CLOSED, "");
    const string door_opened_supervisor_password = u.lookup(LOOKUP_EVENT_REASON, EVENT_REASON_DOOR_OPENED_SUPERVISOR_PASSWORD, "");
    const string controller_power_on = u.lookup(LOOKUP_EVENT_REASON, EVENT_REASON_CONTROLLER_POWER_ON, "");
    const string controller_reset = u.lookup(LOOKUP_EVENT_REASON, EVENT_REASON_CONTROLLER_RESET, "");
    const string pushbutton_invalid_door_locked = u.lookup(LOOKUP_EVENT_REASON, EVENT_REASON_PUSHBUTTON_INVALID_DOOR_LOCKED, "");
    const string pushbutton_invalid_offline = u.lookup(LOOKUP_EVENT_REASON, EVENT_REASON_PUSHBUTTON_INVALID_OFFLINE, "");
    const string pushbutton_invalid_interlock = u.lookup(LOOKUP_EVENT_REASON, EVENT_REASON_PUSHBUTTON_INVALID_INTERLOCK, "");
    const string pushbutton_invalid_threat = u.lookup(LOOKUP_EVENT_REASON, EVENT_REASON_PUSHBUTTON_INVALID_THREAT, "");
    const string door_open_too_long = u.lookup(LOOKUP_EVENT_REASON, EVENT_REASON_DOOR_OPEN_TOO_LONG, "");
    const string forced_open = u.lookup(LOOKUP_EVENT_REASON, EVENT_REASON_FORCED_OPEN, "");
    const string fire = u.lookup(LOOKUP_EVENT_REASON, EVENT_REASON_FIRE, "");
    const string forced_closed = u.lookup(LOOKUP_EVENT_REASON, EVENT_REASON_FORCED_CLOSED, "");
    const string theft_prevention = u.lookup(LOOKUP_EVENT_REASON, EVENT_REASON_THEFT_PREVENTION, "");
    const string zone24x7 = u.lookup(LOOKUP_EVENT_REASON, EVENT_REASON_24X7_ZONE, "");
    const string emergency = u.lookup(LOOKUP_EVENT_REASON, EVENT_REASON_EMERGENCY, "");
    const string remote_open_door = u.lookup(LOOKUP_EVENT_REASON, EVENT_REASON_REMOTE_OPEN_DOOR, "");
    const string remote_open_door_usb_reader = u.lookup(LOOKUP_EVENT_REASON, EVENT_REASON_REMOTE_OPEN_DOOR_USB_READER, "");

    vector<result> rs = {
        result("normally open", string("normally open"), normally_open),
        result("normally closed", string("normally closed"), normally_closed),
        result("controlled", string("controlled"), controlled),

        result("direction:in", string("in"), direction_in),
        result("direction:out", string("out"), direction_out),

        result("event type:none", string("none"), event_type_none),
        result("event type:swipe", string("swipe"), event_type_swipe),
        result("event type:door", string("door"), event_type_door),
        result("event type:alarm", string("alarm"), event_type_alarm),
        result("event type:overwritten", string("overwritten"), event_type_overwritten),

        result("none", string(""), none),
        result("swipe", string("swipe"), swipe),
        result("swipe open", string("swipe open"), swipe_open),
        result("swipe close", string("swipe close"), swipe_close),
        result("denied", string("swipe:denied (system)"), denied),
        result("no_access_rights", string("no access rights"), no_access_rights),
        result("incorrect_password", string("incorrect password"), incorrect_password),
        result("anti_passback", string("anti-passback"), anti_passback),
        result("more_cards", string("more cards"), more_cards),
        result("first_card_open", string("first card open"), first_card_open),
        result("door_is_normally_closed", string("door is normally closed"), door_is_normally_closed),
        result("interlock", string("interlock"), interlock),
        result("not_in_allowed_time_period", string("not in allowed time period"), not_in_allowed_time_period),
        result("invalid_timezone", string("invalid timezone"), invalid_timezone),
        result("access_denied", string("access denied"), access_denied),
        result("push_button_ok", string("pushbutton ok"), push_button_ok),
        result("door_opened", string("door opened"), door_opened),
        result("door_closed", string("door closed"), door_closed),
        result("door_opened_supervisor_password", string("door opened (supervisor password)"), door_opened_supervisor_password),
        result("controller_power_on", string("controller power on"), controller_power_on),
        result("controller_reset", string("controller reset"), controller_reset),
        result("pushbutton_invalid_door_locked", string("pushbutton invalid (door locked)"), pushbutton_invalid_door_locked),
        result("pushbutton_invalid_offline", string("pushbutton invalid (offline)"), pushbutton_invalid_offline),
        result("pushbutton_invalid_interlock", string("pushbutton invalid (interlock)"), pushbutton_invalid_interlock),
        result("pushbutton_invalid_threat", string("pushbutton invalid (threat)"), pushbutton_invalid_threat),
        result("door_open_too_long", string("door open too long"), door_open_too_long),
        result("forced_open", string("forced open"), forced_open),
        result("fire", string("fire"), fire),
        result("forced_closed", string("forced closed"), forced_closed),
        result("theft_prevention", string("theft prevention"), theft_prevention),
        result("24x7 zone", string("24x7 zone"), zone24x7),
        result("emergency", string("emergency"), emergency),
        result("remote_open_door", string("remote open door"), remote_open_door),
        result("remote_open_door_usb_reader", string("remote open door (USB reader)"), remote_open_door_usb_reader),
    };

    return evaluate("lookup", rs);
}
