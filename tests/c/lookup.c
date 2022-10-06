#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "tests.h"
#include "uhppoted.h"

bool internationalisation() {
    const char *normally_open = lookup(LOOKUP_MODE, NORMALLY_OPEN, "");
    const char *normally_closed = lookup(LOOKUP_MODE, NORMALLY_CLOSED, "");
    const char *controlled = lookup(LOOKUP_MODE, CONTROLLED, "");

    const char *direction_in = lookup(LOOKUP_DIRECTION, DIRECTION_IN, "");
    const char *direction_out = lookup(LOOKUP_DIRECTION, DIRECTION_OUT, "");

    const char *event_type_none = lookup(LOOKUP_EVENT_TYPE, EVENT_TYPE_NONE, "");
    const char *event_type_swipe = lookup(LOOKUP_EVENT_TYPE, EVENT_TYPE_SWIPE, "");
    const char *event_type_door = lookup(LOOKUP_EVENT_TYPE, EVENT_TYPE_DOOR, "");
    const char *event_type_alarm = lookup(LOOKUP_EVENT_TYPE, EVENT_TYPE_ALARM, "");
    const char *event_type_overwritten = lookup(LOOKUP_EVENT_TYPE, EVENT_TYPE_OVERWRITTEN, "");

    const char *none = lookup(LOOKUP_EVENT_REASON, EVENT_REASON_NONE, "");
    const char *swipe = lookup(LOOKUP_EVENT_REASON, EVENT_REASON_SWIPE, "");
    const char *swipe_open = lookup(LOOKUP_EVENT_REASON, EVENT_REASON_SWIPE_OPEN, "");
    const char *swipe_close = lookup(LOOKUP_EVENT_REASON, EVENT_REASON_SWIPE_CLOSE, "");
    const char *denied = lookup(LOOKUP_EVENT_REASON, EVENT_REASON_DENIED, "");
    const char *no_access_rights = lookup(LOOKUP_EVENT_REASON, EVENT_REASON_NO_ACCESS_RIGHTS, "");
    const char *incorrect_password = lookup(LOOKUP_EVENT_REASON, EVENT_REASON_INCORRECT_PASSWORD, "");
    const char *anti_passback = lookup(LOOKUP_EVENT_REASON, EVENT_REASON_ANTI_PASSBACK, "");
    const char *more_cards = lookup(LOOKUP_EVENT_REASON, EVENT_REASON_MORE_CARDS, "");
    const char *first_card_open = lookup(LOOKUP_EVENT_REASON, EVENT_REASON_FIRST_CARD_OPEN, "");
    const char *door_is_normally_closed = lookup(LOOKUP_EVENT_REASON, EVENT_REASON_DOOR_IS_NORMALLY_CLOSED, "");
    const char *interlock = lookup(LOOKUP_EVENT_REASON, EVENT_REASON_INTERLOCK, "");
    const char *not_in_allowed_time_period = lookup(LOOKUP_EVENT_REASON, EVENT_REASON_NOT_IN_ALLOWED_TIME_PERIOD, "");
    const char *invalid_timezone = lookup(LOOKUP_EVENT_REASON, EVENT_REASON_INVALID_TIMEZONE, "");
    const char *access_denied = lookup(LOOKUP_EVENT_REASON, EVENT_REASON_ACCESS_DENIED, "");
    const char *push_button_ok = lookup(LOOKUP_EVENT_REASON, EVENT_REASON_PUSHBUTTON_OK, "");
    const char *door_opened = lookup(LOOKUP_EVENT_REASON, EVENT_REASON_DOOR_OPENED, "");
    const char *door_closed = lookup(LOOKUP_EVENT_REASON, EVENT_REASON_DOOR_CLOSED, "");
    const char *door_opened_supervisor_password = lookup(LOOKUP_EVENT_REASON, EVENT_REASON_DOOR_OPENED_SUPERVISOR_PASSWORD, "");
    const char *controller_power_on = lookup(LOOKUP_EVENT_REASON, EVENT_REASON_CONTROLLER_POWER_ON, "");
    const char *controller_reset = lookup(LOOKUP_EVENT_REASON, EVENT_REASON_CONTROLLER_RESET, "");
    const char *pushbutton_invalid_door_locked = lookup(LOOKUP_EVENT_REASON, EVENT_REASON_PUSHBUTTON_INVALID_DOOR_LOCKED, "");
    const char *pushbutton_invalid_offline = lookup(LOOKUP_EVENT_REASON, EVENT_REASON_PUSHBUTTON_INVALID_OFFLINE, "");
    const char *pushbutton_invalid_interlock = lookup(LOOKUP_EVENT_REASON, EVENT_REASON_PUSHBUTTON_INVALID_INTERLOCK, "");
    const char *pushbutton_invalid_threat = lookup(LOOKUP_EVENT_REASON, EVENT_REASON_PUSHBUTTON_INVALID_THREAT, "");
    const char *door_open_too_long = lookup(LOOKUP_EVENT_REASON, EVENT_REASON_DOOR_OPEN_TOO_LONG, "");
    const char *forced_open = lookup(LOOKUP_EVENT_REASON, EVENT_REASON_FORCED_OPEN, "");
    const char *fire = lookup(LOOKUP_EVENT_REASON, EVENT_REASON_FIRE, "");
    const char *forced_closed = lookup(LOOKUP_EVENT_REASON, EVENT_REASON_FORCED_CLOSED, "");
    const char *theft_prevention = lookup(LOOKUP_EVENT_REASON, EVENT_REASON_THEFT_PREVENTION, "");
    const char *zone24x7 = lookup(LOOKUP_EVENT_REASON, EVENT_REASON_24X7_ZONE, "");
    const char *emergency = lookup(LOOKUP_EVENT_REASON, EVENT_REASON_EMERGENCY, "");
    const char *remote_open_door = lookup(LOOKUP_EVENT_REASON, EVENT_REASON_REMOTE_OPEN_DOOR, "");
    const char *remote_open_door_usb_reader = lookup(LOOKUP_EVENT_REASON, EVENT_REASON_REMOTE_OPEN_DOOR_USB_READER, "");

    const result resultset[] = {
        {.field = "normally open", .type = "string", .value.string.expected = "normally open", .value.string.value = normally_open},
        {.field = "normally closed", .type = "string", .value.string.expected = "normally closed", .value.string.value = normally_closed},
        {.field = "controlled", .type = "string", .value.string.expected = "controlled", .value.string.value = controlled},

        {.field = "direction:in", .type = "string", .value.string.expected = "in", .value.string.value = direction_in},
        {.field = "direction:out", .type = "string", .value.string.expected = "out", .value.string.value = direction_out},

        {.field = "event type:none", .type = "string", .value.string.expected = "none", .value.string.value = event_type_none},
        {.field = "event type:swipe", .type = "string", .value.string.expected = "swipe", .value.string.value = event_type_swipe},
        {.field = "event type:door", .type = "string", .value.string.expected = "door", .value.string.value = event_type_door},
        {.field = "event type:alarm", .type = "string", .value.string.expected = "alarm", .value.string.value = event_type_alarm},
        {.field = "event type:overwritten", .type = "string", .value.string.expected = "overwritten", .value.string.value = event_type_overwritten},

        {.field = "none", .type = "string", .value.string.expected = "", .value.string.value = none},
        {.field = "swipe", .type = "string", .value.string.expected = "swipe", .value.string.value = swipe},
        {.field = "swipe open", .type = "string", .value.string.expected = "swipe open", .value.string.value = swipe_open},
        {.field = "swipe close", .type = "string", .value.string.expected = "swipe close", .value.string.value = swipe_close},
        {.field = "denied", .type = "string", .value.string.expected = "swipe:denied (system)", .value.string.value = denied},
        {.field = "no_access_rights", .type = "string", .value.string.expected = "no access rights", .value.string.value = no_access_rights},
        {.field = "incorrect_password", .type = "string", .value.string.expected = "incorrect password", .value.string.value = incorrect_password},
        {.field = "anti_passback", .type = "string", .value.string.expected = "anti-passback", .value.string.value = anti_passback},
        {.field = "more_cards", .type = "string", .value.string.expected = "more cards", .value.string.value = more_cards},
        {.field = "first_card_open", .type = "string", .value.string.expected = "first card open", .value.string.value = first_card_open},
        {.field = "door_is_normally_closed", .type = "string", .value.string.expected = "door is normally closed", .value.string.value = door_is_normally_closed},
        {.field = "interlock", .type = "string", .value.string.expected = "interlock", .value.string.value = interlock},
        {.field = "not_in_allowed_time_period", .type = "string", .value.string.expected = "not in allowed time period", .value.string.value = not_in_allowed_time_period},
        {.field = "invalid_timezone", .type = "string", .value.string.expected = "invalid timezone", .value.string.value = invalid_timezone},
        {.field = "access_denied", .type = "string", .value.string.expected = "access denied", .value.string.value = access_denied},
        {.field = "push_button_ok", .type = "string", .value.string.expected = "pushbutton ok", .value.string.value = push_button_ok},
        {.field = "door_opened", .type = "string", .value.string.expected = "door opened", .value.string.value = door_opened},
        {.field = "door_closed", .type = "string", .value.string.expected = "door closed", .value.string.value = door_closed},
        {.field = "door_opened_supervisor_password", .type = "string", .value.string.expected = "door opened (supervisor password)", .value.string.value = door_opened_supervisor_password},
        {.field = "controller_power_on", .type = "string", .value.string.expected = "controller power on", .value.string.value = controller_power_on},
        {.field = "controller_reset", .type = "string", .value.string.expected = "controller reset", .value.string.value = controller_reset},
        {.field = "pushbutton_invalid_door_locked", .type = "string", .value.string.expected = "pushbutton invalid (door locked)", .value.string.value = pushbutton_invalid_door_locked},
        {.field = "pushbutton_invalid_offline", .type = "string", .value.string.expected = "pushbutton invalid (offline)", .value.string.value = pushbutton_invalid_offline},
        {.field = "pushbutton_invalid_interlock", .type = "string", .value.string.expected = "pushbutton invalid (interlock)", .value.string.value = pushbutton_invalid_interlock},
        {.field = "pushbutton_invalid_threat", .type = "string", .value.string.expected = "pushbutton invalid (threat)", .value.string.value = pushbutton_invalid_threat},
        {.field = "door_open_too_long", .type = "string", .value.string.expected = "door open too long", .value.string.value = door_open_too_long},
        {.field = "forced_open", .type = "string", .value.string.expected = "forced open", .value.string.value = forced_open},
        {.field = "fire", .type = "string", .value.string.expected = "fire", .value.string.value = fire},
        {.field = "forced_closed", .type = "string", .value.string.expected = "forced closed", .value.string.value = forced_closed},
        {.field = "theft_prevention", .type = "string", .value.string.expected = "theft prevention", .value.string.value = theft_prevention},
        {.field = "24x7 zone", .type = "string", .value.string.expected = "24x7 zone", .value.string.value = zone24x7},
        {.field = "emergency", .type = "string", .value.string.expected = "emergency", .value.string.value = emergency},
        {.field = "remote_open_door", .type = "string", .value.string.expected = "remote open door", .value.string.value = remote_open_door},
        {.field = "remote_open_door_usb_reader", .type = "string", .value.string.expected = "remote open door (USB reader)", .value.string.value = remote_open_door_usb_reader},
    };

    return evaluate("lookup", sizeof(resultset) / sizeof(result), resultset);
}
