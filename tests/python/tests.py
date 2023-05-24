#!python

import ctypes
import sys
import platform
import os

from functools import reduce

sys.path.append('../../bindings/python')

if 'Windows' in platform.system():
    pwd = os.path.abspath('.')
    os.add_dll_directory(pwd)

    dlldir = os.path.abspath('../../lib/tests')
    os.add_dll_directory(dlldir)

import uhppoted

from uhppoted import NORMALLY_OPEN
from uhppoted import NORMALLY_CLOSED
from uhppoted import CONTROLLED
from uhppoted import lookup
from uhppoted import LOOKUP_MODE
from uhppoted import LOOKUP_DIRECTION
from uhppoted import LOOKUP_EVENT_TYPE
from uhppoted import LOOKUP_EVENT_REASON

DEVICE_ID = 405419896
CARD_NUMBER = 8165538
CARD_INDEX = 19
EVENT_INDEX = 51
DOOR = 4
PROFILE_ID = 49


def tests():
    return {
        'get-devices': get_devices,
        'get-device': get_device,
        'set-address': set_address,
        'get-status': get_status,
        'get-time': get_time,
        'set-time': set_time,
        'get-listener': get_listener,
        'set-listener': set_listener,
        'get-door-control': get_door_control,
        'set-door-control': set_door_control,
        'open-door': open_door,
        'get-cards': get_cards,
        'get-card': get_card,
        'get-card-by-index': get_card_by_index,
        'put-card': put_card,
        'delete-card': delete_card,
        'delete-cards': delete_cards,
        'get-event-index': get_event_index,
        'set-event-index': set_event_index,
        'get-event': get_event,
        'record-special-events': record_special_events,
        'get-time-profile': get_time_profile,
        'set-time-profile': set_time_profile,
        'clear-time-profiles': clear_time_profiles,
        'add-task': add_task,
        'refresh-tasklist': refresh_tasklist,
        'clear-tasklist': clear_tasklist,
        'set-pc-control': set_pc_control,
        'set-interlock': set_interlock,
        'lookup': internationalisation,
        'structs': structs,
    }


def main():
    cmd = '' if len(sys.argv) < 2 else sys.argv[1]

    bind = '0.0.0.0'
    broadcast = '255.255.255.255'
    listen = '0.0.0.0:60001'
    timeout = 2500
    debug = True

    controllers = [
        uhppoted.Controller(405419896, '192.168.1.100'),
        uhppoted.Controller(303986753, '192.168.1.100'),
    ]

    u = uhppoted.Uhppote(uhppote=uhppoted.UHPPOTE(bind, broadcast, listen, timeout, controllers, debug))

    try:
        if cmd in tests():
            if not tests()[cmd](u):
                sys.exit(-1)

        elif cmd == '' or cmd == 'all':
            if not reduce(lambda ok, f: f(u) and ok, tests().values(), True):
                sys.exit(-1)

        elif cmd == 'help':
            print()
            usage()

        else:
            print()
            print(f'   *** ERROR invalid command ({cmd})')
            print()
            usage()

    except BaseException as x:
        print()
        print(f'*** ERROR  {cmd} failed: {x}')
        print()

        sys.exit(1)


def get_devices(u):
    devices = u.get_devices()

    return evaluate('get-devices', [
        ('device count', 3, len(devices)),
        ('device list', [201020304, 303986753, 405419896], [devices[0], devices[1], devices[2]]),
    ])


def get_device(u):
    info = u.get_device(DEVICE_ID)

    return evaluate('get-device', [
        ('device ID', 405419896, info.ID),
        ('IP address', '192.168.1.101', info.address),
        ('subnet mask', '255.255.255.0', info.subnet),
        ('gateway address', '192.168.1.1', info.gateway),
        ('MAC address', '00:12:23:34:45:56', info.MAC),
        ('version', 'v8.92', info.version),
        ('date', '2018-11-05', info.date),
    ])


def set_address(u):
    u.set_address(DEVICE_ID, '192.168.1.125', '255.255.254.0', '192.168.1.0')

    return evaluate('set-address', [])


def get_status(u):
    status = u.get_status(DEVICE_ID)

    return evaluate('get-status', [
        ('device ID', 405419896, status.ID),
        ('system date/time', '2022-03-19 15:48:32', status.sysdatetime),
        ('doors state', [1, 0, 0, 1], [status.doors[0], status.doors[1], status.doors[2], status.doors[3]]),
        ('buttons state', [1, 0, 1, 0], [status.buttons[0], status.buttons[1], status.buttons[2], status.buttons[3]]),
        ('relay state', 0x12, status.relays),
        ('inputs state', 0x34, status.inputs),
        ('system error', 0x56, status.syserror),
        ('special info', 253, status.info),
        ('sequence number', 9876, status.seqno),
        ('event timestamp', '2022-01-02 12:34:56', status.evt.timestamp),
        ('event index', 135, status.evt.index),
        ('event type', 6, status.evt.eventType),
        ('event granted', 1, status.evt.granted),
        ('event door', 3, status.evt.door),
        ('event direction', 1, status.evt.direction),
        ('event card', 8100023, status.evt.card),
        ('event reason', 21, status.evt.reason),
    ])


def get_time(u):
    datetime = u.get_time(DEVICE_ID)

    return evaluate('get-time', [
        ('date/time', '2022-01-02 12:34:56', datetime),
    ])


def set_time(u):
    u.set_time(DEVICE_ID, '2022-03-23 12:24:17')

    return evaluate('set-time', [])


def get_listener(u):
    listener = u.get_listener(DEVICE_ID)

    return evaluate('get-listener', [
        ('event listener', '192.168.1.100:60001', listener),
    ])


def set_listener(u):
    u.set_listener(DEVICE_ID, '192.168.1.100:60001')

    return evaluate('set-listener', [])


def get_door_control(u):
    control = u.get_door_control(DEVICE_ID, DOOR)

    return evaluate('get-door-control', [
        ('door control mode', CONTROLLED, control.mode),
        ('door open delay', 7, control.delay),
    ])


def set_door_control(u):
    u.set_door_control(DEVICE_ID, DOOR, NORMALLY_CLOSED, 6)

    return evaluate('set-door-control', [])


def open_door(u):
    u.open_door(DEVICE_ID, DOOR)

    return evaluate('open-door', [])


def get_cards(u):
    cards = u.get_cards(DEVICE_ID)

    return evaluate('get-cards', [
        ('card count', 39, cards),
    ])


def get_card(u):
    card = u.get_card(DEVICE_ID, CARD_NUMBER)

    return evaluate('get-card', [
        ('card number', 8165538, card.cardNumber),
        ('from date', '2022-01-01', card.start),
        ('to date', '2022-12-31', card.end),
        ('door[1]', 0, card.doors[0]),
        ('door[2]', 1, card.doors[1]),
        ('door[3]', 31, card.doors[2]),
        ('door[42]', 75, card.doors[3]),
        ('card PIN', 7531, card.PIN),
    ])


def get_card_by_index(u):
    card = u.get_card_by_index(DEVICE_ID, CARD_INDEX)

    return evaluate('get-card-by-index', [
        ('card number', 8165538, card.cardNumber),
        ('from date', '2022-01-01', card.start),
        ('to date', '2022-12-31', card.end),
        ('door[1]', 0, card.doors[0]),
        ('door[2]', 1, card.doors[1]),
        ('door[3]', 31, card.doors[2]),
        ('door[42]', 75, card.doors[3]),
        ('card PIN', 7531, card.PIN),
    ])


def put_card(u):
    u.put_card(DEVICE_ID, CARD_NUMBER, '2022-01-01', '2022-12-31', [0, 1, 31, 75], 7531)

    return evaluate('put-card', [])


def delete_card(u):
    u.delete_card(DEVICE_ID, CARD_NUMBER)

    return evaluate('delete-card', [])


def delete_cards(u):
    u.delete_cards(DEVICE_ID)

    return evaluate('delete-cards', [])


def get_event_index(u):
    index = u.get_event_index(DEVICE_ID)

    return evaluate('get-event-index', [
        ('event index', 47, index),
    ])


def set_event_index(u):
    u.set_event_index(DEVICE_ID, EVENT_INDEX)

    return evaluate('set-event-index', [])


def get_event(u):
    event = u.get_event(DEVICE_ID, EVENT_INDEX)

    return evaluate('get-event', [
        ('event index', 51, event.index),
        ('event timestamp', '2022-04-15 12:29:15', event.timestamp),
        ('event type', 6, event.eventType),
        ('event granted', True, event.granted),
        ('event door', 3, event.door),
        ('event direction', 1, event.direction),
        ('event card', 8165538, event.card),
        ('event reason', 21, event.reason),
    ])


def record_special_events(u):
    tag = 'record-special-events'
    u.record_special_events(DEVICE_ID, True)

    return evaluate(tag, [])


def get_time_profile(u):
    profile = u.get_time_profile(DEVICE_ID, PROFILE_ID)

    return evaluate('get-time-profile', [
        ('profile ID', 49, profile.ID),
        ('linked profile', 71, profile.linked),
        ("profile 'from' date", '2022-02-01', profile.start),
        ("profile 'to' date", '2022-06-30', profile.end),
        ('profile Monday', True, profile.monday),
        ('profile Tuesday', False, profile.tuesday),
        ('profile Wednesday', True, profile.wednesday),
        ('profile Thursday', True, profile.thursday),
        ('profile Friday', False, profile.friday),
        ('profile Saturday', False, profile.saturday),
        ('profile Sunday', True, profile.sunday),
        ('profile segment 1 start', '08:30', profile.segment1start),
        ('profile segment 1 end', '11:30', profile.segment1end),
        ('profile segment 2 start', '00:00', profile.segment2start),
        ('profile segment 2 end', '00:00', profile.segment2end),
        ('profile segment 3 start', '00:00', profile.segment3start),
        ('profile segment 3 end', '18:00', profile.segment3end),
    ])


def set_time_profile(u):
    profile = uhppoted.TimeProfile(PROFILE_ID, 71, "2022-02-01", "2022-06-30", True, False, True, True, False, False,
                                   True, "08:30", "11:30", "", "", "", "18:00")

    u.set_time_profile(DEVICE_ID, profile)

    return evaluate('set-time-profile', [])


def clear_time_profiles(u):
    u.clear_time_profiles(DEVICE_ID)

    return evaluate('clear-time-profiles', [])


def add_task(u):
    task = uhppoted.Task(4, 3, "2022-02-01", "2022-06-30", True, False, True, True, False, False, True, "09:45", 11)

    u.add_task(DEVICE_ID, task)

    return evaluate('set-time-profile', [])


def refresh_tasklist(u):
    u.refresh_tasklist(DEVICE_ID)

    return evaluate('refresh-tasklist', [])


def clear_tasklist(u):
    u.clear_tasklist(DEVICE_ID)

    return evaluate('clear-tasklist', [])


def set_pc_control(u):
    tag = 'set-pc-control'
    u.set_pc_control(DEVICE_ID, True)

    return evaluate(tag, [])


def set_interlock(u):
    tag = 'set-interlock'
    u.set_interlock(DEVICE_ID, 3)

    return evaluate(tag, [])


def internationalisation(u):
    normally_open = lookup(LOOKUP_MODE, uhppoted.NORMALLY_OPEN, '')
    normally_closed = lookup(LOOKUP_MODE, uhppoted.NORMALLY_CLOSED, '')
    controlled = lookup(LOOKUP_MODE, uhppoted.CONTROLLED, '')

    direction_in = lookup(LOOKUP_DIRECTION, uhppoted.DIRECTION_IN, '')
    direction_out = lookup(LOOKUP_DIRECTION, uhppoted.DIRECTION_OUT, '')

    event_type_none = lookup(LOOKUP_EVENT_TYPE, uhppoted.EVENT_TYPE_NONE, '')
    event_type_swipe = lookup(LOOKUP_EVENT_TYPE, uhppoted.EVENT_TYPE_SWIPE, '')
    event_type_door = lookup(LOOKUP_EVENT_TYPE, uhppoted.EVENT_TYPE_DOOR, '')
    event_type_alarm = lookup(LOOKUP_EVENT_TYPE, uhppoted.EVENT_TYPE_ALARM, '')
    event_type_overwritten = lookup(LOOKUP_EVENT_TYPE, uhppoted.EVENT_TYPE_OVERWRITTEN, '')

    none = lookup(LOOKUP_EVENT_REASON, uhppoted.EVENT_REASON_NONE, '')
    swipe = lookup(LOOKUP_EVENT_REASON, uhppoted.EVENT_REASON_SWIPE, '')
    swipe_open = lookup(LOOKUP_EVENT_REASON, uhppoted.EVENT_REASON_SWIPE_OPEN, '')
    swipe_close = lookup(LOOKUP_EVENT_REASON, uhppoted.EVENT_REASON_SWIPE_CLOSE, '')
    denied = lookup(LOOKUP_EVENT_REASON, uhppoted.EVENT_REASON_DENIED, '')
    no_access_rights = lookup(LOOKUP_EVENT_REASON, uhppoted.EVENT_REASON_NO_ACCESS_RIGHTS, '')
    incorrect_password = lookup(LOOKUP_EVENT_REASON, uhppoted.EVENT_REASON_INCORRECT_PASSWORD, '')
    anti_passback = lookup(LOOKUP_EVENT_REASON, uhppoted.EVENT_REASON_ANTI_PASSBACK, '')
    more_cards = lookup(LOOKUP_EVENT_REASON, uhppoted.EVENT_REASON_MORE_CARDS, '')
    first_card_open = lookup(LOOKUP_EVENT_REASON, uhppoted.EVENT_REASON_FIRST_CARD_OPEN, '')
    door_is_normally_closed = lookup(LOOKUP_EVENT_REASON, uhppoted.EVENT_REASON_DOOR_IS_NORMALLY_CLOSED, '')
    interlock = lookup(LOOKUP_EVENT_REASON, uhppoted.EVENT_REASON_INTERLOCK, '')
    not_in_allowed_time_period = lookup(LOOKUP_EVENT_REASON, uhppoted.EVENT_REASON_NOT_IN_ALLOWED_TIME_PERIOD, '')
    invalid_timezone = lookup(LOOKUP_EVENT_REASON, uhppoted.EVENT_REASON_INVALID_TIMEZONE, '')
    access_denied = lookup(LOOKUP_EVENT_REASON, uhppoted.EVENT_REASON_ACCESS_DENIED, '')
    push_button_ok = lookup(LOOKUP_EVENT_REASON, uhppoted.EVENT_REASON_PUSHBUTTON_OK, '')
    door_opened = lookup(LOOKUP_EVENT_REASON, uhppoted.EVENT_REASON_DOOR_OPENED, '')
    door_closed = lookup(LOOKUP_EVENT_REASON, uhppoted.EVENT_REASON_DOOR_CLOSED, '')
    door_opened_supervisor_password = lookup(LOOKUP_EVENT_REASON, uhppoted.EVENT_REASON_DOOR_OPENED_SUPERVISOR_PASSWORD,
                                             '')
    controller_power_on = lookup(LOOKUP_EVENT_REASON, uhppoted.EVENT_REASON_CONTROLLER_POWER_ON, '')
    controller_reset = lookup(LOOKUP_EVENT_REASON, uhppoted.EVENT_REASON_CONTROLLER_RESET, '')
    pushbutton_invalid_door_locked = lookup(LOOKUP_EVENT_REASON, uhppoted.EVENT_REASON_PUSHBUTTON_INVALID_DOOR_LOCKED,
                                            '')
    pushbutton_invalid_offline = lookup(LOOKUP_EVENT_REASON, uhppoted.EVENT_REASON_PUSHBUTTON_INVALID_OFFLINE, '')
    pushbutton_invalid_interlock = lookup(LOOKUP_EVENT_REASON, uhppoted.EVENT_REASON_PUSHBUTTON_INVALID_INTERLOCK, '')
    pushbutton_invalid_threat = lookup(LOOKUP_EVENT_REASON, uhppoted.EVENT_REASON_PUSHBUTTON_INVALID_THREAT, '')
    door_open_too_long = lookup(LOOKUP_EVENT_REASON, uhppoted.EVENT_REASON_DOOR_OPEN_TOO_LONG, '')
    forced_open = lookup(LOOKUP_EVENT_REASON, uhppoted.EVENT_REASON_FORCED_OPEN, '')
    fire = lookup(LOOKUP_EVENT_REASON, uhppoted.EVENT_REASON_FIRE, '')
    forced_closed = lookup(LOOKUP_EVENT_REASON, uhppoted.EVENT_REASON_FORCED_CLOSED, '')
    theft_prevention = lookup(LOOKUP_EVENT_REASON, uhppoted.EVENT_REASON_THEFT_PREVENTION, '')
    zone24x7 = lookup(LOOKUP_EVENT_REASON, uhppoted.EVENT_REASON_ZONE_24X7, '')
    emergency = lookup(LOOKUP_EVENT_REASON, uhppoted.EVENT_REASON_EMERGENCY, '')
    remote_open_door = lookup(LOOKUP_EVENT_REASON, uhppoted.EVENT_REASON_REMOTE_OPEN_DOOR, '')
    remote_open_door_usb_reader = lookup(LOOKUP_EVENT_REASON, uhppoted.EVENT_REASON_REMOTE_OPEN_DOOR_USB_READER, '')

    return evaluate('lookup', [
        ('normally open', 'normally open', normally_open),
        ('normally closed', 'normally closed', normally_closed),
        ('controlled', 'controlled', controlled),
        ('direction:in', 'in', direction_in),
        ('direction:out', 'out', direction_out),
        ('event type:none', 'none', event_type_none),
        ('event type:swipe', 'swipe', event_type_swipe),
        ('event type:door', 'door', event_type_door),
        ('event type:alarm', 'alarm', event_type_alarm),
        ('event type:overwritten', 'overwritten', event_type_overwritten),
        ('none', '', none),
        ('swipe', 'swipe', swipe),
        ('swipe open', 'swipe open', swipe_open),
        ('swipe close', 'swipe close', swipe_close),
        ('denied', 'swipe:denied (system)', denied),
        ('no_access_rights', 'no access rights', no_access_rights),
        ('incorrect_password', 'incorrect password', incorrect_password),
        ('anti_passback', 'anti-passback', anti_passback),
        ('more_cards', 'more cards', more_cards),
        ('first_card_open', 'first card open', first_card_open),
        ('door_is_normally_closed', 'door is normally closed', door_is_normally_closed),
        ('interlock', 'interlock', interlock),
        ('not_in_allowed_time_period', 'not in allowed time period', not_in_allowed_time_period),
        ('invalid_timezone', 'invalid timezone', invalid_timezone),
        ('access_denied', 'access denied', access_denied),
        ('push_button_ok', 'pushbutton ok', push_button_ok),
        ('door_opened', 'door opened', door_opened),
        ('door_closed', 'door closed', door_closed),
        ('door_opened_supervisor_password', 'door opened (supervisor password)', door_opened_supervisor_password),
        ('controller_power_on', 'controller power on', controller_power_on),
        ('controller_reset', 'controller reset', controller_reset),
        ('pushbutton_invalid_door_locked', 'pushbutton invalid (door locked)', pushbutton_invalid_door_locked),
        ('pushbutton_invalid_offline', 'pushbutton invalid (offline)', pushbutton_invalid_offline),
        ('pushbutton_invalid_interlock', 'pushbutton invalid (interlock)', pushbutton_invalid_interlock),
        ('pushbutton_invalid_threat', 'pushbutton invalid (threat)', pushbutton_invalid_threat),
        ('door_open_too_long', 'door open too long', door_open_too_long),
        ('forced_open', 'forced open', forced_open),
        ('fire', 'fire', fire),
        ('forced_closed', 'forced closed', forced_closed),
        ('theft_prevention', 'theft prevention', theft_prevention),
        ('24x7 zone', '24x7 zone', zone24x7),
        ('emergency', 'emergency', emergency),
        ('remote_open_door', 'remote open door', remote_open_door),
        ('remote_open_door_usb_reader', 'remote open door (USB reader)', remote_open_door_usb_reader),
    ])


def structs(u):
    bind = '0.0.0.0'
    broadcast = '255.255.255.255'
    listen = '0.0.0.0:60001'
    timeout = 2500
    controllers = []

    u1 = uhppoted.Uhppote(uhppote=uhppoted.UHPPOTE(bind, broadcast, listen, timeout, controllers, True))
    u2 = uhppoted.Uhppote(uhppote=uhppoted.UHPPOTE(bind, broadcast, listen, timeout, controllers, False))

    u1.get_device(0xffffffff)
    u2.get_device(0xfffffffe)

    return passed('structs')


def evaluate(tag, resultset):
    ok = True
    for row in resultset:
        field, expected, actual = row
        if actual != expected:
            print(f'{tag:<21} incorrect {field} (expected:{expected}, got:{actual})')
            ok = False

    return passed(tag) if ok else failed(tag)


def passed(tag):
    print(f'{tag:<21} ok')
    return True


def failed(tag):
    print(f'{tag:<21} failed')
    return False


def usage():
    print('   Usage: python test.py <command>')
    print()
    print('   Supported commands:')
    print('      all')
    for k in tests():
        print(f'      {k}')
    print()


if __name__ == '__main__':
    main()
