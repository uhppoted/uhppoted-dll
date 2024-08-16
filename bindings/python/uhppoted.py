import ctypes
import platform
import lookup as internationalisation

from ctypes import c_bool
from ctypes import c_char_p
from ctypes import c_ubyte
from ctypes import c_int
from ctypes import c_longlong
from ctypes import c_uint32
from ctypes import c_ulong
from ctypes import c_void_p
from ctypes import pointer
from ctypes import byref
from ctypes import addressof
from ctypes import Structure
from ctypes import POINTER

from dataclasses import dataclass
from functools import cache
from types import SimpleNamespace
from typing import Final
from time import sleep

if 'Windows' in platform.system():
    lib = ctypes.windll.LoadLibrary("uhppoted")
elif 'Darwin' in platform.system():
    lib = ctypes.cdll.LoadLibrary("libuhppoted.dylib")
else:
    lib = ctypes.cdll.LoadLibrary("libuhppoted.so")

# yapf: disable
NORMALLY_OPEN:    Final[int] = internationalisation.NORMALLY_OPEN
NORMALLY_CLOSED:  Final[int] = internationalisation.NORMALLY_CLOSED
CONTROLLED:       Final[int] = internationalisation.CONTROLLED

DIRECTION_IN:     Final[int] = internationalisation.DIRECTION_IN
DIRECTION_OUT:    Final[int] = internationalisation.DIRECTION_OUT

EVENT_TYPE_NONE:        Final[int] = internationalisation.EVENT_TYPE_NONE
EVENT_TYPE_SWIPE:       Final[int] = internationalisation.EVENT_TYPE_SWIPE
EVENT_TYPE_DOOR:        Final[int] = internationalisation.EVENT_TYPE_DOOR
EVENT_TYPE_ALARM:       Final[int] = internationalisation.EVENT_TYPE_ALARM
EVENT_TYPE_OVERWRITTEN: Final[int] = internationalisation.EVENT_TYPE_OVERWRITTEN

EVENT_REASON_NONE:                            Final[int] = internationalisation.EVENT_REASON_NONE
EVENT_REASON_SWIPE:                           Final[int] = internationalisation.EVENT_REASON_SWIPE
EVENT_REASON_SWIPE_OPEN:                      Final[int] = internationalisation.EVENT_REASON_SWIPE_OPEN
EVENT_REASON_SWIPE_CLOSE:                     Final[int] = internationalisation.EVENT_REASON_SWIPE_CLOSE
EVENT_REASON_DENIED:                          Final[int] = internationalisation.EVENT_REASON_DENIED
EVENT_REASON_NO_ACCESS_RIGHTS:                Final[int] = internationalisation.EVENT_REASON_NO_ACCESS_RIGHTS
EVENT_REASON_INCORRECT_PASSWORD:              Final[int] = internationalisation.EVENT_REASON_INCORRECT_PASSWORD
EVENT_REASON_ANTI_PASSBACK:                   Final[int] = internationalisation.EVENT_REASON_ANTI_PASSBACK
EVENT_REASON_MORE_CARDS:                      Final[int] = internationalisation.EVENT_REASON_MORE_CARDS
EVENT_REASON_FIRST_CARD_OPEN:                 Final[int] = internationalisation.EVENT_REASON_FIRST_CARD_OPEN
EVENT_REASON_DOOR_IS_NORMALLY_CLOSED:         Final[int] = internationalisation.EVENT_REASON_DOOR_IS_NORMALLY_CLOSED
EVENT_REASON_INTERLOCK:                       Final[int] = internationalisation.EVENT_REASON_INTERLOCK
EVENT_REASON_NOT_IN_ALLOWED_TIME_PERIOD:      Final[int] = internationalisation.EVENT_REASON_NOT_IN_ALLOWED_TIME_PERIOD
EVENT_REASON_INVALID_TIMEZONE:                Final[int] = internationalisation.EVENT_REASON_INVALID_TIMEZONE
EVENT_REASON_ACCESS_DENIED:                   Final[int] = internationalisation.EVENT_REASON_ACCESS_DENIED
EVENT_REASON_PUSHBUTTON_OK:                   Final[int] = internationalisation.EVENT_REASON_PUSHBUTTON_OK
EVENT_REASON_DOOR_OPENED:                     Final[int] = internationalisation.EVENT_REASON_DOOR_OPENED
EVENT_REASON_DOOR_CLOSED:                     Final[int] = internationalisation.EVENT_REASON_DOOR_CLOSED
EVENT_REASON_DOOR_OPENED_SUPERVISOR_PASSWORD: Final[int] = internationalisation.EVENT_REASON_DOOR_OPENED_SUPERVISOR_PASSWORD
EVENT_REASON_CONTROLLER_POWER_ON:             Final[int] = internationalisation.EVENT_REASON_CONTROLLER_POWER_ON
EVENT_REASON_CONTROLLER_RESET:                Final[int] = internationalisation.EVENT_REASON_CONTROLLER_RESET
EVENT_REASON_PUSHBUTTON_INVALID_DOOR_LOCKED:  Final[int] = internationalisation.EVENT_REASON_PUSHBUTTON_INVALID_DOOR_LOCKED
EVENT_REASON_PUSHBUTTON_INVALID_OFFLINE:      Final[int] = internationalisation.EVENT_REASON_PUSHBUTTON_INVALID_OFFLINE
EVENT_REASON_PUSHBUTTON_INVALID_INTERLOCK:    Final[int] = internationalisation.EVENT_REASON_PUSHBUTTON_INVALID_INTERLOCK
EVENT_REASON_PUSHBUTTON_INVALID_THREAT:       Final[int] = internationalisation.EVENT_REASON_PUSHBUTTON_INVALID_THREAT
EVENT_REASON_DOOR_OPEN_TOO_LONG:              Final[int] = internationalisation.EVENT_REASON_DOOR_OPEN_TOO_LONG
EVENT_REASON_FORCED_OPEN:                     Final[int] = internationalisation.EVENT_REASON_FORCED_OPEN
EVENT_REASON_FIRE:                            Final[int] = internationalisation.EVENT_REASON_FIRE
EVENT_REASON_FORCED_CLOSED:                   Final[int] = internationalisation.EVENT_REASON_FORCED_CLOSED
EVENT_REASON_THEFT_PREVENTION:                Final[int] = internationalisation.EVENT_REASON_THEFT_PREVENTION
EVENT_REASON_ZONE_24X7:                       Final[int] = internationalisation.EVENT_REASON_ZONE_24X7
EVENT_REASON_EMERGENCY:                       Final[int] = internationalisation.EVENT_REASON_EMERGENCY
EVENT_REASON_REMOTE_OPEN_DOOR:                Final[int] = internationalisation.EVENT_REASON_REMOTE_OPEN_DOOR
EVENT_REASON_REMOTE_OPEN_DOOR_USB_READER:     Final[int] = internationalisation.EVENT_REASON_REMOTE_OPEN_DOOR_USB_READER
# yapf: enable


@dataclass
class Controller:
    id: int
    address: str
    transport: str = ''


@dataclass
class UHPPOTE:
    bind: str
    broadcast: str
    listen: str
    timeout: int
    controllers: list[Controller]
    debug: bool


@dataclass
class Device:
    ID: int
    address: str
    subnet: str
    gateway: str
    MAC: str
    version: str
    date: str


@dataclass
class Event:
    timestamp: str
    index: int
    eventType: int
    granted: bool
    door: int
    direction: int
    card: int
    reason: int


@dataclass
class Status:
    ID: int
    sysdatetime: str
    doors: list[bool]
    buttons: list[bool]
    relays: int
    inputs: int
    syserror: int
    seqno: int
    info: int
    evt: Event


@dataclass
class DoorControl:
    mode: int
    delay: int


@dataclass
class Card:
    cardNumber: int
    start: str
    end: str
    doors: list[int]
    PIN: int


@dataclass
class TimeProfile:
    ID: int
    linked: int
    start: str
    end: str
    monday: bool
    tuesday: bool
    wednesday: bool
    thursday: bool
    friday: bool
    saturday: bool
    sunday: bool
    segment1start: str
    segment1end: str
    segment2start: str
    segment2end: str
    segment3start: str
    segment3end: str


@dataclass
class Task:
    task: int
    door: int
    start: str
    end: str
    monday: bool
    tuesday: bool
    wednesday: bool
    thursday: bool
    friday: bool
    saturday: bool
    sunday: bool
    at: str
    cards: int


@dataclass
class ListenEvent:
    controller: int
    timestamp: str
    index: int
    event: int
    granted: bool
    door: int
    direction: int
    card: int
    reason: int


class Uhppote:

    def __init__(self, uhppote=None):
        self.ffi = FFI(self.errcheck)
        self.ffix = FFIX(self.errcheckx)
        self._uhppote = None
        if uhppote:
            self._uhppote = GoUHPPOTE(uhppote.bind, uhppote.broadcast, uhppote.listen, uhppote.timeout, uhppote.controllers, uhppote.debug)

    @staticmethod
    def errcheck(err, func, args):
        # error code
        if isinstance(err, int):
            if err != 0:
                raise Exception(f'error code {err}')

        # UTF-8 error message
        if err and isinstance(err, bytes):
            raise Exception(f"{err.decode('utf-8')}")

        # other error
        if err:
            raise Exception(f'{err}')

        return args

    @staticmethod
    def errcheckx(err, func, args):
        return args

    def get_devices(self):
        N = 0
        while True:
            N = N + 16
            count = ctypes.c_int(N)
            list = (c_uint32 * N)(*[0] * N)
            errN = ctypes.c_int(256)
            err = c_char_p(bytes('*' * errN.value, 'utf-8'))

            if self.ffix.GetDevices(self._uhppote, list, byref(count), err, byref(errN)) != 0:
                raise Exception(f"{err.value.decode('utf-8')}")

            if count.value <= N:
                break

        return list[0:count.value]

    def get_device(self, deviceID):
        errN = ctypes.c_int(256)
        err = c_char_p(bytes('*' * errN.value, 'utf-8'))

        device = GoDevice()

        if self.ffix.GetDevice(self._uhppote, byref(device), deviceID, err, byref(errN)) != 0:
            raise Exception(f"{err.value.decode('utf-8')}")

        return Device(device.ID, device.address.decode('utf-8'), device.subnet.decode('utf-8'), device.gateway.decode('utf-8'),
                      device.MAC.decode('utf-8'), device.version.decode('utf-8'), device.date.decode('utf-8'))

    def set_address(self, deviceID, address, subnet, gateway):
        errN = ctypes.c_int(256)
        err = c_char_p(bytes('*' * errN.value, 'utf-8'))

        c_address = c_char_p(bytes(address, 'utf-8'))
        c_subnet = c_char_p(bytes(subnet, 'utf-8'))
        c_gateway = c_char_p(bytes(gateway, 'utf-8'))

        if self.ffix.SetAddress(self._uhppote, deviceID, c_address, c_subnet, c_gateway, err, byref(errN)) != 0:
            raise Exception(f"{err.value.decode('utf-8')}")

    def get_status(self, deviceID):
        errN = ctypes.c_int(256)
        err = c_char_p(bytes('*' * errN.value, 'utf-8'))
        status = GoStatus()

        if self.ffix.GetStatus(self._uhppote, ctypes.byref(status), deviceID, err, byref(errN)) != 0:
            raise Exception(f"{err.value.decode('utf-8')}")

        doors = [False, False, False, False]
        buttons = [False, False, False, False]
        for i in range(4):
            if status.doors[i] != 0:
                doors[i] = True

            if status.buttons[i] != 0:
                buttons[i] = True

        event = Event(
            status.event.contents.timestamp.decode('utf-8'),
            status.event.contents.index,
            status.event.contents.eventType,
            status.event.contents.granted,
            status.event.contents.door,
            status.event.contents.direction,
            status.event.contents.card,
            status.event.contents.reason,
        )

        return Status(status.ID, status.sysdatetime.decode('utf-8'), doors, buttons, status.relays, status.inputs, status.syserror,
                      status.seqno, status.info, event)

    def get_time(self, deviceID):
        errN = ctypes.c_int(256)
        err = c_char_p(bytes('*' * errN.value, 'utf-8'))
        datetime = c_char_p(bytes(' ' * 20, 'utf-8'))

        if self.ffix.GetTime(self._uhppote, datetime, deviceID, err, byref(errN)) != 0:
            raise Exception(f"{err.value.decode('utf-8')}")

        return datetime.value.decode('utf-8')

    def set_time(self, deviceID, datetime):
        errN = ctypes.c_int(256)
        err = c_char_p(bytes('*' * errN.value, 'utf-8'))

        if self.ffix.SetTime(self._uhppote, deviceID, c_char_p(bytes(datetime, 'utf-8')), err, byref(errN)) != 0:
            raise Exception(f"{err.value.decode('utf-8')}")

    def get_listener(self, deviceID):
        errN = ctypes.c_int(256)
        err = c_char_p(bytes('*' * errN.value, 'utf-8'))
        listener = c_char_p(bytes(' ' * 22, 'utf-8'))

        if self.ffix.GetListener(self._uhppote, listener, deviceID, err, byref(errN)) != 0:
            raise Exception(f"{err.value.decode('utf-8')}")

        return listener.value.decode('utf-8')

    def set_listener(self, deviceID, listener):
        errN = ctypes.c_int(256)
        err = c_char_p(bytes('*' * errN.value, 'utf-8'))

        if self.ffix.SetListener(self._uhppote, deviceID, c_char_p(bytes(listener, 'utf-8')), err, byref(errN)) != 0:
            raise Exception(f"{err.value.decode('utf-8')}")

    def get_door_control(self, deviceID, door):
        errN = ctypes.c_int(256)
        err = c_char_p(bytes('*' * errN.value, 'utf-8'))
        control = GoDoorControl()

        if self.ffix.GetDoorControl(self._uhppote, byref(control), deviceID, door, err, byref(errN)) != 0:
            raise Exception(f"{err.value.decode('utf-8')}")

        return DoorControl(control.control, control.delay)

    def set_door_control(self, deviceID, door, mode, delay):
        errN = ctypes.c_int(256)
        err = c_char_p(bytes('*' * errN.value, 'utf-8'))

        if self.ffix.SetDoorControl(self._uhppote, deviceID, door, mode, delay, err, byref(errN)) != 0:
            raise Exception(f"{err.value.decode('utf-8')}")

    def open_door(self, deviceID, door):
        errN = ctypes.c_int(256)
        err = c_char_p(bytes('*' * errN.value, 'utf-8'))

        if self.ffix.OpenDoor(self._uhppote, deviceID, door, err, byref(errN)) != 0:
            raise Exception(f"{err.value.decode('utf-8')}")

    def get_cards(self, deviceID):
        cards = ctypes.c_int(0)
        errN = ctypes.c_int(256)
        err = c_char_p(bytes('*' * errN.value, 'utf-8'))

        if self.ffix.GetCards(self._uhppote, byref(cards), deviceID, err, byref(errN)) != 0:
            raise Exception(f"{err.value.decode('utf-8')}")

        return cards.value

    def get_card(self, deviceID, cardNumber):
        card = GoCard()
        errN = ctypes.c_int(256)
        err = c_char_p(bytes('*' * errN.value, 'utf-8'))

        if self.ffix.GetCard(self._uhppote, byref(card), deviceID, cardNumber, err, byref(errN)) != 0:
            raise Exception(f"{err.value.decode('utf-8')}")

        doors = [0, 0, 0, 0]
        for i in range(4):
            doors[i] = card.doors[i]

        return Card(card.cardNumber, card.start.decode('utf-8'), card.end.decode('utf-8'), doors, card.PIN)

    def get_card_by_index(self, deviceID, index):
        card = GoCard()
        errN = ctypes.c_int(256)
        err = c_char_p(bytes('*' * errN.value, 'utf-8'))

        if self.ffix.GetCardByIndex(self._uhppote, byref(card), deviceID, index, err, byref(errN)) != 0:
            raise Exception(f"{err.value.decode('utf-8')}")

        doors = [0, 0, 0, 0]
        for i in range(4):
            doors[i] = card.doors[i]

        return Card(card.cardNumber, card.start.decode('utf-8'), card.end.decode('utf-8'), doors, card.PIN)

    def put_card(self, deviceID, cardNumber, start, end, doors, PIN):
        _doors = (c_ubyte * 4)(*[0] * 4)
        _doors[0] = doors[0]
        _doors[1] = doors[1]
        _doors[2] = doors[2]
        _doors[3] = doors[3]

        errN = ctypes.c_int(256)
        err = c_char_p(bytes('*' * errN.value, 'utf-8'))

        if self.ffix.PutCard(self._uhppote, deviceID, cardNumber, c_char_p(bytes(start, 'utf-8')), c_char_p(bytes(end, 'utf-8')), _doors,
                             PIN, err, byref(errN)) != 0:
            raise Exception(f"{err.value.decode('utf-8')}")

    def delete_card(self, deviceID, cardNumber):
        errN = ctypes.c_int(256)
        err = c_char_p(bytes('*' * errN.value, 'utf-8'))

        if self.ffix.DeleteCard(self._uhppote, deviceID, cardNumber, err, byref(errN)) != 0:
            raise Exception(f"{err.value.decode('utf-8')}")

    def delete_cards(self, deviceID):
        errN = ctypes.c_int(256)
        err = c_char_p(bytes('*' * errN.value, 'utf-8'))

        if self.ffix.DeleteCards(self._uhppote, deviceID, err, byref(errN)) != 0:
            raise Exception(f"{err.value.decode('utf-8')}")

    def get_event_index(self, deviceID):
        index = ctypes.c_ulong(0)
        errN = ctypes.c_int(256)
        err = c_char_p(bytes('*' * errN.value, 'utf-8'))

        if self.ffix.GetEventIndex(self._uhppote, byref(index), deviceID, err, byref(errN)) != 0:
            raise Exception(f"{err.value.decode('utf-8')}")

        return index.value

    def set_event_index(self, deviceID, index):
        errN = ctypes.c_int(256)
        err = c_char_p(bytes('*' * errN.value, 'utf-8'))

        if self.ffix.SetEventIndex(self._uhppote, deviceID, index, err, byref(errN)) != 0:
            raise Exception(f"{err.value.decode('utf-8')}")

    def get_event(self, deviceID, index):
        event = GoEvent()
        errN = ctypes.c_int(256)
        err = c_char_p(bytes('*' * errN.value, 'utf-8'))

        if self.ffix.GetEvent(self._uhppote, byref(event), deviceID, index, err, byref(errN)) != 0:
            raise Exception(f"{err.value.decode('utf-8')}")

        return Event(event.timestamp.decode('utf-8'), event.index, event.eventType, event.granted, event.door, event.direction, event.card,
                     event.reason)

    def record_special_events(self, deviceID, enabled):
        errN = ctypes.c_int(256)
        err = c_char_p(bytes('*' * errN.value, 'utf-8'))

        if self.ffix.RecordSpecialEvents(self._uhppote, deviceID, enabled, err, byref(errN)) != 0:
            raise Exception(f"{err.value.decode('utf-8')}")

    def get_time_profile(self, deviceID, profileID):
        profile = GoTimeProfile()
        errN = ctypes.c_int(256)
        err = c_char_p(bytes('*' * errN.value, 'utf-8'))

        if self.ffix.GetTimeProfile(self._uhppote, byref(profile), deviceID, profileID, err, byref(errN)) != 0:
            raise Exception(f"{err.value.decode('utf-8')}")

        return TimeProfile(profile.ID, profile.linked, profile.start.decode('utf-8'), profile.end.decode('utf-8'), profile.monday != 0,
                           profile.tuesday != 0, profile.wednesday != 0, profile.thursday != 0, profile.friday != 0,
                           profile.saturday != 0, profile.sunday != 0, profile.segment1start.decode('utf-8'),
                           profile.segment1end.decode('utf-8'), profile.segment2start.decode('utf-8'), profile.segment2end.decode('utf-8'),
                           profile.segment3start.decode('utf-8'), profile.segment3end.decode('utf-8'))

    def set_time_profile(self, deviceID, p):
        profile = GoTimeProfile(ID=p.ID,
                                linked=p.linked,
                                start=p.start,
                                end=p.end,
                                monday=p.monday,
                                tuesday=p.tuesday,
                                wednesday=p.wednesday,
                                thursday=p.thursday,
                                friday=p.friday,
                                saturday=p.saturday,
                                sunday=p.sunday,
                                segment1start=p.segment1start,
                                segment1end=p.segment1end,
                                segment2start=p.segment2start,
                                segment2end=p.segment2end,
                                segment3start=p.segment3start,
                                segment3end=p.segment3end)

        errN = ctypes.c_int(256)
        err = c_char_p(bytes('*' * errN.value, 'utf-8'))

        if self.ffix.SetTimeProfile(self._uhppote, deviceID, byref(profile), err, byref(errN)) != 0:
            raise Exception(f"{err.value.decode('utf-8')}")

    def clear_time_profiles(self, deviceID):
        errN = ctypes.c_int(256)
        err = c_char_p(bytes('*' * errN.value, 'utf-8'))

        if self.ffix.ClearTimeProfiles(self._uhppote, deviceID, err, byref(errN)) != 0:
            raise Exception(f"{err.value.decode('utf-8')}")

    def add_task(self, deviceID, t):
        task = GoTask(t.task, t.door, c_char_p(bytes(t.start, 'utf-8')), c_char_p(bytes(t.end, 'utf-8')), 1 if t.monday else 0,
                      1 if t.tuesday else 0, 1 if t.wednesday else 0, 1 if t.thursday else 0, 1 if t.friday else 0, 1 if t.saturday else 0,
                      1 if t.sunday else 0, c_char_p(bytes(t.at, 'utf-8')), t.cards)

        self.ffi.AddTask(self._uhppote, deviceID, byref(task))

    def refresh_tasklist(self, deviceID):
        self.ffi.RefreshTaskList(self._uhppote, deviceID)

    def clear_tasklist(self, deviceID):
        self.ffi.ClearTaskList(self._uhppote, deviceID)

    def set_pc_control(self, deviceID, enabled):
        self.ffi.SetPCControl(self._uhppote, deviceID, enabled)

    def set_interlock(self, deviceID, interlock):
        self.ffi.SetInterlock(self._uhppote, deviceID, interlock)

    def activate_keypads(self, deviceID, reader1, reader2, reader3, reader4):
        self.ffi.ActivateKeypads(self._uhppote, deviceID, reader1, reader2, reader3, reader4)

    def set_door_passcodes(self, deviceID, door, passcode1, passcode2, passcode3, passcode4):
        self.ffi.SetDoorPasscodes(self._uhppote, deviceID, door, passcode1, passcode2, passcode3, passcode4) # yapf: disable

    def restore_default_parameters(self, deviceID):
        self.ffi.RestoreDefaultParameters(self._uhppote, deviceID)

    # Ref. https://docs.python.org/3/library/ctypes.html#callback-functions
    # Ref. https://stackoverflow.com/questions/24912065/how-to-access-data-from-pointer-in-struct-from-python-with-ctypes
    def listen_events(self, onevent, onerror, ev_listening, ev_stop, userdata):
        callback = on_event(lambda e, v: on_listen_event(onevent, e, v))
        err_handler = on_error(lambda v: on_listen_error(onerror, v))
        listening = c_bool(False)
        stop = c_bool(False)
        p = None

        if userdata is not None:
            v = tuple([userdata])
            p = pointer(ctypes.py_object(v))

        try:
            self.ffi.Listen(self._uhppote, callback, byref(listening), byref(stop), err_handler, p)
            count = 0
            while (not listening) and (count < 10):
                print(f' ... waiting {count}')
                count += 1
                sleep(0.1)

            if not listening:
                raise Exception(f'timeout starting event listener')

            ev_listening.set()
            ev_stop.wait()

            stop = True
            count = 0
            while listening and (count < 10):
                print(f' ... stopping {count}')
                count += 1
                sleep(0.1)

            if listening:
                raise Exception(f'timeout stopping event listener')
        except Exception as err:
            raise Exception(f'event listener ({err})')


def on_listen_event(handler, event, userdata):
    u = None
    if userdata is not None:
        u = ctypes.cast(userdata, ctypes.POINTER(ctypes.py_object)).contents.value[0]

    # yapf: disable
    e = ListenEvent(event.controller,
                    event.timestamp.decode('utf-8'),
                    event.index,
                    event.event,
                    event.granted,
                    event.door,
                    event.direction,
                    event.card,
                    event.reason)
    # yapf: enable

    handler(e, u)


def on_listen_error(handler, err):
    handler(err.decode('utf-8'))


# lookup

LOOKUP_MODE: Final[str] = 'door.mode'
LOOKUP_DIRECTION: Final[str] = 'event.direction'
LOOKUP_EVENT_TYPE: Final[str] = 'event.type'
LOOKUP_EVENT_REASON: Final[str] = 'event.reason'

ModeNormallyOpen: Final[str] = 'normally open'
ModeNormallyClosed: Final[str] = 'normally closed'
ModeControlled: Final[str] = 'controlled'
ModeUnknown: Final[str] = 'unknown'

DirectionIn: Final[str] = 'in'
DirectionOut: Final[str] = 'out'
DirectionUnknown: Final[str] = 'unknown'

EventTypeNone: Final[str] = 'none'
EventTypeSwipe: Final[str] = 'swipe'
EventTypeDoor: Final[str] = 'door'
EventTypeAlarm: Final[str] = 'alarm'
EventTypeOverwritten: Final[str] = 'overwritten'
EventTypeUnknown: Final[str] = 'unknown'

EventReasonNone: Final[str] = ''
EventReasonSwipe: Final[str] = 'swipe'
EventReasonSwipeOpen: Final[str] = 'swipe open'
EventReasonSwipeClose: Final[str] = 'swipe close'
EventReasonDenied: Final[str] = 'swipe:denied (system)'
EventReasonNoAccessRights: Final[str] = 'no access rights'
EventReasonIncorrectPassword: Final[str] = 'incorrect password'
EventReasonAntiPassback: Final[str] = 'anti-passback'
EventReasonMoreCards: Final[str] = 'more cards'
EventReasonFirstCardOpen: Final[str] = 'first card open'
EventReasonDoorIsNormallyClosed: Final[str] = 'door is normally closed'
EventReasonInterlock: Final[str] = 'interlock'
EventReasonNotInAllowedTimePeriod: Final[str] = 'not in allowed time period'
EventReasonInvalidTimezone: Final[str] = 'invalid timezone'
EventReasonAccessDenied: Final[str] = 'access denied'
EventReasonPushButtonOk: Final[str] = 'pushbutton ok'
EventReasonDoorOpened: Final[str] = 'door opened'
EventReasonDoorClosed: Final[str] = 'door closed'
EventReasonDoorOpenedSupervisorPassword: Final[str] = 'door opened (supervisor password)'
EventReasonControllerPowerOn: Final[str] = 'controller power on'
EventReasonControllerReset: Final[str] = 'controller reset'
EventReasonPushbuttonInvalidDoorLocked: Final[str] = 'pushbutton invalid (door locked)'
EventReasonPushbuttonInvalidOffline: Final[str] = 'pushbutton invalid (offline)'
EventReasonPushbuttonInvalidInterlock: Final[str] = 'pushbutton invalid (interlock)'
EventReasonPushbuttonInvalidThreat: Final[str] = 'pushbutton invalid (threat)'
EventReasonDoorOpenTooLong: Final[str] = 'door open too long'
EventReasonForcedOpen: Final[str] = 'forced open'
EventReasonFire: Final[str] = 'fire'
EventReasonForcedClosed: Final[str] = 'forced closed'
EventReasonTheftPrevention: Final[str] = 'theft prevention'
EventReason24x7Zone: Final[str] = '24x7 zone'
EventReasonEmergency: Final[str] = 'emergency'
EventReasonRemoteOpenDoor: Final[str] = 'remote open door'
EventReasonRemoteOpenDoorUSBReader: Final[str] = 'remote open door (USB reader)'
EventReasonUnknown: Final[str] = 'unknown'


def LookupMode():
    return {
        NORMALLY_OPEN: ModeNormallyOpen,
        NORMALLY_CLOSED: ModeNormallyClosed,
        CONTROLLED: ModeControlled,
    }


def LookupDirection():
    return {
        DIRECTION_IN: DirectionIn,
        DIRECTION_OUT: DirectionOut,
    }


def LookupEventType():
    return {
        EVENT_TYPE_NONE: EventTypeNone,
        EVENT_TYPE_SWIPE: EventTypeSwipe,
        EVENT_TYPE_DOOR: EventTypeDoor,
        EVENT_TYPE_ALARM: EventTypeAlarm,
        EVENT_TYPE_OVERWRITTEN: EventTypeOverwritten,
    }


def LookupEventReason():
    return {
        EVENT_REASON_NONE: EventReasonNone,
        EVENT_REASON_SWIPE: EventReasonSwipe,
        EVENT_REASON_SWIPE_OPEN: EventReasonSwipeOpen,
        EVENT_REASON_SWIPE_CLOSE: EventReasonSwipeClose,
        EVENT_REASON_DENIED: EventReasonDenied,
        EVENT_REASON_NO_ACCESS_RIGHTS: EventReasonNoAccessRights,
        EVENT_REASON_INCORRECT_PASSWORD: EventReasonIncorrectPassword,
        EVENT_REASON_ANTI_PASSBACK: EventReasonAntiPassback,
        EVENT_REASON_MORE_CARDS: EventReasonMoreCards,
        EVENT_REASON_FIRST_CARD_OPEN: EventReasonFirstCardOpen,
        EVENT_REASON_DOOR_IS_NORMALLY_CLOSED: EventReasonDoorIsNormallyClosed,
        EVENT_REASON_INTERLOCK: EventReasonInterlock,
        EVENT_REASON_NOT_IN_ALLOWED_TIME_PERIOD: EventReasonNotInAllowedTimePeriod,
        EVENT_REASON_INVALID_TIMEZONE: EventReasonInvalidTimezone,
        EVENT_REASON_ACCESS_DENIED: EventReasonAccessDenied,
        EVENT_REASON_PUSHBUTTON_OK: EventReasonPushButtonOk,
        EVENT_REASON_DOOR_OPENED: EventReasonDoorOpened,
        EVENT_REASON_DOOR_CLOSED: EventReasonDoorClosed,
        EVENT_REASON_DOOR_OPENED_SUPERVISOR_PASSWORD: EventReasonDoorOpenedSupervisorPassword,
        EVENT_REASON_CONTROLLER_POWER_ON: EventReasonControllerPowerOn,
        EVENT_REASON_CONTROLLER_RESET: EventReasonControllerReset,
        EVENT_REASON_PUSHBUTTON_INVALID_DOOR_LOCKED: EventReasonPushbuttonInvalidDoorLocked,
        EVENT_REASON_PUSHBUTTON_INVALID_OFFLINE: EventReasonPushbuttonInvalidOffline,
        EVENT_REASON_PUSHBUTTON_INVALID_INTERLOCK: EventReasonPushbuttonInvalidInterlock,
        EVENT_REASON_PUSHBUTTON_INVALID_THREAT: EventReasonPushbuttonInvalidThreat,
        EVENT_REASON_DOOR_OPEN_TOO_LONG: EventReasonDoorOpenTooLong,
        EVENT_REASON_FORCED_OPEN: EventReasonForcedOpen,
        EVENT_REASON_FIRE: EventReasonFire,
        EVENT_REASON_FORCED_CLOSED: EventReasonForcedClosed,
        EVENT_REASON_THEFT_PREVENTION: EventReasonTheftPrevention,
        EVENT_REASON_ZONE_24X7: EventReason24x7Zone,
        EVENT_REASON_EMERGENCY: EventReasonEmergency,
        EVENT_REASON_REMOTE_OPEN_DOOR: EventReasonRemoteOpenDoor,
        EVENT_REASON_REMOTE_OPEN_DOOR_USB_READER: EventReasonRemoteOpenDoorUSBReader,
    }


def dictionaries():
    return {
        LOOKUP_MODE: LookupMode,
        LOOKUP_DIRECTION: LookupDirection,
        LOOKUP_EVENT_TYPE: LookupEventType,
        LOOKUP_EVENT_REASON: LookupEventReason,
    }


def unknown():
    return {
        LOOKUP_MODE: ModeUnknown,
        LOOKUP_DIRECTION: DirectionUnknown,
        LOOKUP_EVENT_TYPE: EventTypeUnknown,
        LOOKUP_EVENT_REASON: EventReasonUnknown,
    }


def lookup(category, code, locale):
    tables = dictionaries()

    if category in tables:
        table = tables[category]()
        if code in table:
            return table[code]
        else:
            return unknown()[category]

    return "?"


# Go FFI types


class FFI:

    def __init__(self, errcheck):
        # self.GetDevices = ffi('GetDevices', errcheck)
        # self.GetDevice = ffi('GetDevice', errcheck)
        # self.SetAddress = ffi('SetAddress', errcheck)
        # self.GetStatus = ffi('GetStatus', errcheck)
        # self.GetTime = ffi('GetTime', errcheck)
        # self.SetTime = ffi('SetTime', errcheck)
        # self.GetListener = ffi('GetListener', errcheck)
        # self.SetListener = ffi('SetListener', errcheck)
        # self.GetDoorControl = ffi('GetDoorControl', errcheck)
        # self.SetDoorControl = ffi('SetDoorControl', errcheck)
        # self.OpenDoor = ffi('OpenDoor', errcheck)
        # self.GetCards = ffi('GetCards', errcheck)
        # self.GetCard = ffi('GetCard', errcheck)
        # self.GetCardByIndex = ffi('GetCardByIndex', errcheck)
        # self.PutCard = ffi('PutCard', errcheck)
        # self.DeleteCard = ffi('DeleteCard', errcheck)
        # self.DeleteCards = ffi('DeleteCards', errcheck)
        # self.GetEventIndex = ffi('GetEventIndex', errcheck)
        # self.SetEventIndex = ffi('SetEventIndex', errcheck)
        # self.GetEvent = ffi('GetEvent', errcheck)
        # self.RecordSpecialEvents = ffi('RecordSpecialEvents', errcheck)
        # self.GetTimeProfile = ffi('GetTimeProfile', errcheck)
        # self.SetTimeProfile = ffi('SetTimeProfile', errcheck)
        # self.ClearTimeProfiles = ffi('ClearTimeProfiles', errcheck)
        self.AddTask = ffi('AddTask', errcheck)
        self.RefreshTaskList = ffi('RefreshTaskList', errcheck)
        self.ClearTaskList = ffi('ClearTaskList', errcheck)
        self.SetPCControl = ffi('SetPCControl', errcheck)
        self.SetInterlock = ffi('SetInterlock', errcheck)
        self.ActivateKeypads = ffi('ActivateKeypads', errcheck)
        self.SetDoorPasscodes = ffi('SetDoorPasscodes', errcheck)
        self.RestoreDefaultParameters = ffi('RestoreDefaultParameters', errcheck)
        self.Listen = ffil('Listen', errcheck)


class FFIX:

    def __init__(self, errcheck):
        self.GetDevices = ffix('GetDevices', errcheck)
        self.GetDevice = ffix('GetDevice', errcheck)
        self.SetAddress = ffix('SetAddress', errcheck)
        self.GetStatus = ffix('GetStatus', errcheck)
        self.GetTime = ffix('GetTime', errcheck)
        self.SetTime = ffix('SetTime', errcheck)
        self.GetListener = ffix('GetListener', errcheck)
        self.SetListener = ffix('SetListener', errcheck)
        self.GetDoorControl = ffix('GetDoorControl', errcheck)
        self.SetDoorControl = ffix('SetDoorControl', errcheck)
        self.OpenDoor = ffix('OpenDoor', errcheck)
        self.GetCards = ffix('GetCards', errcheck)
        self.GetCard = ffix('GetCard', errcheck)
        self.GetCardByIndex = ffix('GetCardByIndex', errcheck)
        self.PutCard = ffix('PutCard', errcheck)
        self.DeleteCard = ffix('DeleteCard', errcheck)
        self.DeleteCards = ffix('DeleteCards', errcheck)
        self.GetEventIndex = ffix('GetEventIndex', errcheck)
        self.SetEventIndex = ffix('SetEventIndex', errcheck)
        self.GetEvent = ffix('GetEvent', errcheck)
        self.RecordSpecialEvents = ffix('RecordSpecialEvents', errcheck)
        self.GetTimeProfile = ffix('GetTimeProfile', errcheck)
        self.SetTimeProfile = ffix('SetTimeProfile', errcheck)
        self.ClearTimeProfiles = ffix('ClearTimeProfiles', errcheck)


def ffi(tag, errcheck):
    (ff, argtypes) = libfunctions()[tag]

    ff.argtypes = argtypes
    ff.restype = ctypes.c_char_p
    ff.errcheck = errcheck

    return ff


def ffix(tag, errcheck):
    (ff, argtypes) = libfunctions()[tag]

    ff.argtypes = argtypes
    ff.restype = ctypes.c_int
    ff.errcheck = errcheck

    return ff


def ffil(tag, errcheck):
    (ff, argtypes) = libfunctions()[tag]

    ff.argtypes = argtypes
    ff.restype = ctypes.c_int32
    ff.errcheck = errcheck

    return ff


# yapf: disable
@cache
def libfunctions():
    return {
        'GetDevices':               (lib.GetDevices,               [POINTER(GoUHPPOTE), POINTER(ctypes.c_uint32), POINTER(ctypes.c_int), c_char_p, POINTER(ctypes.c_int)]),
        'GetDevice':                (lib.GetDevice,                [POINTER(GoUHPPOTE), POINTER(GoDevice),  c_ulong, c_char_p, POINTER(ctypes.c_int)]),
        'SetAddress':               (lib.SetAddress,               [POINTER(GoUHPPOTE), c_ulong, c_char_p, c_char_p, c_char_p, c_char_p, POINTER(ctypes.c_int)]),
        'GetStatus':                (lib.GetStatus,                [POINTER(GoUHPPOTE), POINTER(GoStatus), c_ulong, c_char_p, POINTER(ctypes.c_int)]),
        'GetTime':                  (lib.GetTime,                  [POINTER(GoUHPPOTE), c_char_p, c_ulong, c_char_p, POINTER(ctypes.c_int)]),
        'SetTime':                  (lib.SetTime,                  [POINTER(GoUHPPOTE), c_ulong, c_char_p, c_char_p, POINTER(ctypes.c_int)]),
        'GetListener':              (lib.GetListener,              [POINTER(GoUHPPOTE), c_char_p, c_ulong, c_char_p, POINTER(ctypes.c_int)]),
        'SetListener':              (lib.SetListener,              [POINTER(GoUHPPOTE), c_ulong, c_char_p, c_char_p, POINTER(ctypes.c_int)]),
        'GetDoorControl':           (lib.GetDoorControl,           [POINTER(GoUHPPOTE), POINTER(GoDoorControl), c_ulong, c_ubyte, c_char_p, POINTER(ctypes.c_int)]),
        'SetDoorControl':           (lib.SetDoorControl,           [POINTER(GoUHPPOTE), c_ulong, c_ubyte, c_ubyte, c_ubyte, c_char_p, POINTER(ctypes.c_int)]),
        'OpenDoor':                 (lib.OpenDoor,                 [POINTER(GoUHPPOTE), c_ulong, c_ubyte, c_char_p, POINTER(ctypes.c_int)]),
        'GetCards':                 (lib.GetCards,                 [POINTER(GoUHPPOTE), POINTER(c_int), c_ulong, c_char_p, POINTER(ctypes.c_int)]),
        'GetCard':                  (lib.GetCard,                  [POINTER(GoUHPPOTE), POINTER(GoCard), c_ulong, c_ulong, c_char_p, POINTER(ctypes.c_int)]),
        'GetCardByIndex':           (lib.GetCardByIndex,           [POINTER(GoUHPPOTE), POINTER(GoCard), c_ulong, c_ulong, c_char_p, POINTER(ctypes.c_int)]),
        'PutCard':                  (lib.PutCard,                  [POINTER(GoUHPPOTE), c_ulong, c_ulong, c_char_p, c_char_p, POINTER(c_ubyte), c_ulong, c_char_p, POINTER(ctypes.c_int)]),
        'DeleteCard':               (lib.DeleteCard,               [POINTER(GoUHPPOTE), c_ulong, c_ulong, c_char_p, POINTER(ctypes.c_int)]),
        'DeleteCards':              (lib.DeleteCards,              [POINTER(GoUHPPOTE), c_ulong, c_char_p, POINTER(ctypes.c_int)]),
        'GetEventIndex':            (lib.GetEventIndex,            [POINTER(GoUHPPOTE), POINTER(c_ulong), c_ulong, c_char_p, POINTER(ctypes.c_int)]),
        'SetEventIndex':            (lib.SetEventIndex,            [POINTER(GoUHPPOTE), c_ulong, c_ulong, c_char_p, POINTER(ctypes.c_int)]),
        'GetEvent':                 (lib.GetEvent,                 [POINTER(GoUHPPOTE), POINTER(GoEvent), c_ulong, c_ulong, c_char_p, POINTER(ctypes.c_int)]),
        'RecordSpecialEvents':      (lib.RecordSpecialEvents,      [POINTER(GoUHPPOTE), c_ulong, c_bool, c_char_p, POINTER(ctypes.c_int)]),
        'GetTimeProfile':           (lib.GetTimeProfile,           [POINTER(GoUHPPOTE), POINTER(GoTimeProfile), c_ulong, c_ubyte, c_char_p, POINTER(ctypes.c_int)]),
        'SetTimeProfile':           (lib.SetTimeProfile,           [POINTER(GoUHPPOTE), c_ulong, POINTER(GoTimeProfile), c_char_p, POINTER(ctypes.c_int)]),
        'ClearTimeProfiles':        (lib.ClearTimeProfiles,        [POINTER(GoUHPPOTE), c_ulong, c_char_p, POINTER(ctypes.c_int)]),
        'AddTask':                  (lib.AddTask,                  [POINTER(GoUHPPOTE), c_ulong, POINTER(GoTask)]),
        'RefreshTaskList':          (lib.RefreshTaskList,          [POINTER(GoUHPPOTE), c_ulong]),
        'ClearTaskList':            (lib.ClearTaskList,            [POINTER(GoUHPPOTE), c_ulong]),
        'SetPCControl':             (lib.SetPCControl,             [POINTER(GoUHPPOTE), c_ulong, c_bool]),
        'SetInterlock':             (lib.SetInterlock,             [POINTER(GoUHPPOTE), c_ulong, c_ubyte]),
        'ActivateKeypads':          (lib.ActivateKeypads,          [POINTER(GoUHPPOTE), c_ulong, c_bool, c_bool, c_bool, c_bool]),
        'SetDoorPasscodes':         (lib.SetDoorPasscodes,         [POINTER(GoUHPPOTE), c_ulong, c_ubyte, c_ulong, c_ulong, c_ulong, c_ulong]),
        'RestoreDefaultParameters': (lib.RestoreDefaultParameters, [POINTER(GoUHPPOTE), c_ulong]),
        'Listen':                   (lib.Listen,                   [POINTER(GoUHPPOTE), on_event, POINTER(c_bool), POINTER(c_bool), on_error, c_void_p]),
    }
# yapf: enable


class GoController(Structure):
    _fields_ = [('id', c_uint32), ('address', c_char_p), ('transport', c_char_p)]


class GoControllers(Structure):
    _fields_ = [('N', c_uint32), ('devices', POINTER(GoController))]


class GoUHPPOTE(Structure):
    _fields_ = [('bind', c_char_p), ('broadcast', c_char_p), ('listen', c_char_p), ('timeout', c_int), ('devices', POINTER(GoControllers)),
                ('debug', c_bool)]

    def __init__(self, bind, broadcast, listen, timeout, controllers, debug):
        super(GoUHPPOTE, self).__init__()
        self.bind = c_char_p(bytes(bind, 'utf-8'))
        self.broadcast = c_char_p(bytes(broadcast, 'utf-8'))
        self.listen = c_char_p(bytes(listen, 'utf-8'))
        self.timeout = timeout
        self.devices = None
        self.debug = c_bool(debug)

        N = len(controllers)
        if N > 0:
            list = GoControllers(N, (GoController * N)())

            for ix, c in enumerate(controllers):
                list.devices[ix] = GoController(c.id, c_char_p(bytes(c.address, 'utf-8')), c_char_p(bytes(c.transport, 'utf-8')))

            self.devices = pointer(list)


class GoDevice(Structure):
    _fields_ = [
        ('ID', c_ulong),
        ('address', c_char_p),
        ('subnet', c_char_p),
        ('gateway', c_char_p),
        ('MAC', c_char_p),
        ('version', c_char_p),
        ('date', c_char_p),
    ]

    def __init__(self):
        super(GoDevice, self).__init__()
        self.ID = 0
        self.address = c_char_p(bytes(' ' * 16, 'utf-8'))
        self.subnet = c_char_p(bytes(' ' * 16, 'utf-8'))
        self.gateway = c_char_p(bytes(' ' * 16, 'utf-8'))
        self.MAC = c_char_p(bytes(' ' * 18, 'utf-8'))
        self.version = c_char_p(bytes(' ' * 7, 'utf-8'))
        self.date = c_char_p(bytes(' ' * 11, 'utf-8'))


class GoEvent(Structure):
    _fields_ = [
        ('timestamp', c_char_p),
        ('index', c_uint32),
        ('eventType', c_ubyte),
        ('granted', c_bool),
        ('door', c_ubyte),
        ('direction', c_ubyte),
        ('card', c_uint32),
        ('reason', c_ubyte),
    ]

    def __init__(self):
        super(GoEvent, self).__init__()
        self.timestamp = c_char_p(bytes(' ' * 20, 'utf-8'))


class GoStatus(Structure):
    _fields_ = [
        ('ID', c_uint32),
        ('sysdatetime', c_char_p),
        ('doors', POINTER(c_ubyte)),  # uint8_t[4]
        ('buttons', POINTER(c_ubyte)),  # uint8_t[4]
        ('relays', c_ubyte),
        ('inputs', c_ubyte),
        ('syserror', c_ubyte),
        ('info', c_ubyte),
        ('seqno', c_uint32),
        ('event', POINTER(GoEvent)),
    ]

    def __init__(self):
        super(GoStatus, self).__init__()
        self.ID = 0
        self.sysdatetime = c_char_p(bytes(' ' * 20, 'utf-8'))
        self.doors = (c_ubyte * 4)(*[0] * 4)
        self.buttons = (c_ubyte * 4)(*[0] * 4)
        self.event = pointer(GoEvent())


class GoDoorControl(Structure):
    _fields_ = [
        ('control', c_ubyte),
        ('delay', c_ubyte),
    ]


class GoCard(Structure):
    _fields_ = [
        ('cardNumber', c_uint32),
        ('start', c_char_p),
        ('end', c_char_p),
        ('doors', POINTER(c_ubyte)),  # uint8_t[4]
        ('PIN', c_uint32),
    ]

    def __init__(self):
        super(GoCard, self).__init__()
        self.cardNumber = 0
        self.start = c_char_p(bytes(' ' * 11, 'utf-8'))
        self.end = c_char_p(bytes(' ' * 11, 'utf-8'))
        self.doors = (c_ubyte * 4)(*[0] * 4)
        self.PIN = 0


class GoTimeProfile(Structure):
    _fields_ = [
        ('ID', c_ubyte),
        ('linked', c_ubyte),
        ('start', c_char_p),
        ('end', c_char_p),
        ('monday', c_ubyte),
        ('tuesday', c_ubyte),
        ('wednesday', c_ubyte),
        ('thursday', c_ubyte),
        ('friday', c_ubyte),
        ('saturday', c_ubyte),
        ('sunday', c_ubyte),
        ('segment1start', c_char_p),
        ('segment1end', c_char_p),
        ('segment2start', c_char_p),
        ('segment2end', c_char_p),
        ('segment3start', c_char_p),
        ('segment3end', c_char_p),
    ]

    def __init__(self,
                 ID=0,
                 linked=0,
                 start=None,
                 end=None,
                 monday=False,
                 tuesday=False,
                 wednesday=False,
                 thursday=False,
                 friday=False,
                 saturday=False,
                 sunday=False,
                 segment1start=None,
                 segment1end=None,
                 segment2start=None,
                 segment2end=None,
                 segment3start=None,
                 segment3end=None):
        super(GoTimeProfile, self).__init__()

        self.ID = ID
        self.linked = linked
        self.start = c_char_p(bytes(start, 'utf-8')) if start else c_char_p(bytes(' ' * 11, 'utf-8'))
        self.end = c_char_p(bytes(end, 'utf-8')) if end else c_char_p(bytes(' ' * 11, 'utf-8'))
        self.monday = 1 if monday else 0
        self.tuesday = 1 if tuesday else 0
        self.wednesday = 1 if wednesday else 0
        self.thursday = 1 if thursday else 0
        self.friday = 1 if friday else 0
        self.saturday = 1 if saturday else 0
        self.sunday = 1 if sunday else 0
        self.segment1start = c_char_p(bytes(segment1start, 'utf-8')) if segment1start else c_char_p(bytes('00:00', 'utf-8'))
        self.segment1end = c_char_p(bytes(segment1end, 'utf-8')) if segment1end else c_char_p(bytes('00:00', 'utf-8'))
        self.segment2start = c_char_p(bytes(segment1start, 'utf-8')) if segment2start else c_char_p(bytes('00:00', 'utf-8'))
        self.segment2end = c_char_p(bytes(segment1end, 'utf-8')) if segment2end else c_char_p(bytes('00:00', 'utf-8'))
        self.segment3start = c_char_p(bytes(segment1start, 'utf-8')) if segment3start else c_char_p(bytes('00:00', 'utf-8'))
        self.segment3end = c_char_p(bytes(segment3end, 'utf-8')) if segment3end else c_char_p(bytes('00:00', 'utf-8'))


class GoTask(Structure):
    _fields_ = [
        ('task', c_ubyte),
        ('door', c_ubyte),
        ('start', c_char_p),
        ('end', c_char_p),
        ('monday', c_ubyte),
        ('tuesday', c_ubyte),
        ('wednesday', c_ubyte),
        ('thursday', c_ubyte),
        ('friday', c_ubyte),
        ('saturday', c_ubyte),
        ('sunday', c_ubyte),
        ('at', c_char_p),
        ('cards', c_ubyte),
    ]


class GoListenEvent(Structure):
    _fields_ = [
        ('controller', c_uint32),
        ('timestamp', c_char_p),
        ('index', c_uint32),
        ('event', c_ubyte),
        ('granted', c_bool),
        ('door', c_ubyte),
        ('direction', c_ubyte),
        ('card', c_uint32),
        ('reason', c_ubyte),
    ]


on_event = ctypes.CFUNCTYPE(None, GoListenEvent, c_void_p)
on_error = ctypes.CFUNCTYPE(None, c_char_p)
