from typing import Final

#pragma once

NORMALLY_OPEN: Final[int] = 1
NORMALLY_CLOSED: Final[int] = 2
CONTROLLED: Final[int] = 3

DIRECTION_IN: Final[int] = 1
DIRECTION_OUT: Final[int] = 2

EVENT_TYPE_NONE: Final[int] = 0
EVENT_TYPE_SWIPE: Final[int] = 1
EVENT_TYPE_DOOR: Final[int] = 2
EVENT_TYPE_ALARM: Final[int] = 3
EVENT_TYPE_OVERWRITTEN: Final[int] = 255

EVENT_REASON_NONE: Final[int] = 0
EVENT_REASON_SWIPE: Final[int] = 1
EVENT_REASON_SWIPE_OPEN: Final[int] = 2
EVENT_REASON_SWIPE_CLOSE: Final[int] = 3
EVENT_REASON_DENIED: Final[int] = 5
EVENT_REASON_NO_ACCESS_RIGHTS: Final[int] = 6
EVENT_REASON_INCORRECT_PASSWORD: Final[int] = 7
EVENT_REASON_ANTI_PASSBACK: Final[int] = 8
EVENT_REASON_MORE_CARDS: Final[int] = 9
EVENT_REASON_FIRST_CARD_OPEN: Final[int] = 10
EVENT_REASON_DOOR_IS_NORMALLY_CLOSED: Final[int] = 11
EVENT_REASON_INTERLOCK: Final[int] = 12
EVENT_REASON_NOT_IN_ALLOWED_TIME_PERIOD: Final[int] = 13
EVENT_REASON_INVALID_TIMEZONE: Final[int] = 15
EVENT_REASON_ACCESS_DENIED: Final[int] = 18
EVENT_REASON_PUSHBUTTON_OK: Final[int] = 20
EVENT_REASON_DOOR_OPENED: Final[int] = 23
EVENT_REASON_DOOR_CLOSED: Final[int] = 24
EVENT_REASON_DOOR_OPENED_SUPERVISOR_PASSWORD: Final[int] = 25
EVENT_REASON_CONTROLLER_POWER_ON: Final[int] = 28
EVENT_REASON_CONTROLLER_RESET: Final[int] = 29
EVENT_REASON_PUSHBUTTON_INVALID_DOOR_LOCKED: Final[int] = 31
EVENT_REASON_PUSHBUTTON_INVALID_OFFLINE: Final[int] = 32
EVENT_REASON_PUSHBUTTON_INVALID_INTERLOCK: Final[int] = 33
EVENT_REASON_PUSHBUTTON_INVALID_THREAT: Final[int] = 34
EVENT_REASON_DOOR_OPEN_TOO_LONG: Final[int] = 37
EVENT_REASON_FORCED_OPEN: Final[int] = 38
EVENT_REASON_FIRE: Final[int] = 39
EVENT_REASON_FORCED_CLOSED: Final[int] = 40
EVENT_REASON_THEFT_PREVENTION: Final[int] = 41
EVENT_REASON_ZONE_24X7: Final[int] = 42
EVENT_REASON_EMERGENCY: Final[int] = 43
EVENT_REASON_REMOTE_OPEN_DOOR: Final[int] = 44
EVENT_REASON_REMOTE_OPEN_DOOR_USB_READER: Final[int] = 45
