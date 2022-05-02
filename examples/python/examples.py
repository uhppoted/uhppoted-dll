#!python

import argparse
import ctypes
import sys
import datetime
import platform
import os

sys.path.append('../../bindings/python')

if 'Windows' in platform.system():
    dlldir = os.path.abspath('../../lib/debug')
    os.add_dll_directory(dlldir)

import uhppoted

from uhppoted import NORMALLY_OPEN
from uhppoted import NORMALLY_CLOSED
from uhppoted import CONTROLLED

DEVICE_ID = 405419896
CARD_NUMBER = 8000001
CARD_INDEX = 7
EVENT_INDEX = 43
DOOR = 4
PROFILE_ID = 29


def commands():
    return {
        'get-devices': {
            'help': "Retrieves a list of UHPPOTE controller IDs findable on the local LAN",
            'fn': get_devices,
        },
        'get-device': {
            'help': "Retrieves the basic device information for a single UHPPOTE controller",
            'fn': get_device,
        },
        'set-address': {
            'help': "Sets the controller IPv4 address, subnet mask and gateway address",
            'fn': set_address,
        },
        'get-status': {
            'help': "Retrieves a controller status",
            'fn': get_status,
        },
        'get-time': {
            'help': "Retrieves a controller current date/time (YYYY-MM-DD HH:mm:ss)",
            'fn': get_time,
        },
        'set-time': {
            'help': "Sets a controller current date/time (YYYY-MM-DD HH:mm:ss)",
            'fn': set_time,
        },
        'get-listener': {
            'help': "Retrieves a controller's configured event listener address",
            'fn': get_listener,
        },
        'set-listener': {
            'help': "Configures a controller's event listener address and port",
            'fn': set_listener,
        },
        'get-door-control': {
            'help': "Retrieves the control state and open delay for a controller door",
            'fn': get_door_control,
        },
        'set-door-control': {
            'help': "Sets the control mode and delay for a controller door",
            'fn': set_door_control,
        },
        'open-door': {
            'help': "Remotely opens a controller door",
            'fn': open_door,
        },
        'get-cards': {
            'help': "Retrieves the number of cards stored on a controller",
            'fn': get_cards,
        },
        'get-card': {
            'help': "Retrieves the card detail for card number from a controller",
            'fn': get_card,
        },
        'get-card-by-index': {
            'help': "Retrieves the card detail for the card stored at an index on a controller",
            'fn': get_card_by_index,
        },
        'put-card': {
            'help': "Adds or updates the card detail for card number stored on a controller",
            'fn': put_card,
        },
        'delete-card': {
            'help': "Deletes a card from a controller",
            'fn': delete_card,
        },
        'delete-cards': {
            'help': "Deletes all cards from a controller",
            'fn': delete_cards,
        },
        'get-event-index': {
            'help': "Retrieves the current event index from a controller",
            'fn': get_event_index,
        },
        'set-event-index': {
            'help': "Sets the current event index on a controller",
            'fn': set_event_index,
        },
        'get-event': {
            'help': "Retrieves the event at the index from a controller",
            'fn': get_event,
        },
        'record-special-events': {
            'help': "Enables/disables recording additional events for a controller",
            'fn': record_special_events,
        },
        'get-time-profile': {
            'help': "Retrieves a time profile from a controller",
            'fn': get_time_profile,
        },
        'set-time-profile': {
            'help': "Adds or updates a time profile on a controller",
            'fn': set_time_profile,
        },
        'clear-time-profiles': {
            'help': "Deletes all time profiles from a controller",
            'fn': clear_time_profiles,
        },
        'add-task': {
            'help': "Adds a scheduled task to  a controller",
            'fn': add_task,
        },
        'refresh-tasklist': {
            'help': "Refreshes a controller task list to activate added tasks",
            'fn': refresh_tasklist,
        },
        'clear-tasklist': {
            'help': "Clears a controller task list",
            'fn': clear_tasklist,
        },
    }


def main():
    if len(sys.argv) < 2:
        usage()
        return -1

    cmd = sys.argv[1]

    if cmd == 'help':
        help()
    else:
        bind = '0.0.0.0'
        broadcast = '255.255.255.255'
        listen = '0.0.0.0:60001'
        timeout = 2500
        debug = True
        controllers = [
            uhppoted.Controller(405419896, '192.168.1.100'),
            uhppoted.Controller(303986753, '192.168.1.100'),
        ]

        u = uhppoted.Uhppote(
            uhppote=uhppoted.UHPPOTE(bind, broadcast, listen, timeout, controllers, debug))

        try:
            if cmd in commands():
                commands()[cmd]['fn'](u, None)
            else:
                print()
                print(f'  ERROR: invalid command ({cmd})')
                usage()

        except BaseException as x:
            print()
            print(f'*** ERROR  {cmd}: {x}')
            print()

            sys.exit(1)


def usage():
    print()
    print('  Usage: python example.py <command>')
    print()
    print('  Supported commands:')

    for cmd, _ in commands().items():
        print(f'    {cmd}')

    print()


def help():
    print()
    print('  Usage: python example.py <command>')
    print()
    print('  Commands')

    for cmd, v in commands().items():
        print(f"    {cmd:<21}  {v['help']}")

    print()


def get_devices(u, args):
    list = u.get_devices()

    fields = []
    for id in list:
        fields.append(('', id))

    display(f'get-devices ({len(list)})', fields)


def get_device(u, args):
    deviceID = DEVICE_ID

    info = u.get_device(deviceID)

    display('get-device', [
        ('ID', deviceID),
        ('IP address', info.address),
        ('subnet mask', info.subnet),
        ('gateway address', info.gateway),
        ('MAC', info.MAC),
        ('version', info.version),
        ('released', info.date),
    ])


def set_address(u, args):
    deviceID = DEVICE_ID
    address = '192.168.1.125'
    subnet = '255.255.255.253'
    gateway = '192.168.1.5'

    u.set_address(deviceID, address, subnet, gateway)

    display('set-address', [
        ('ID', deviceID),
        ('address', address),
        ('subnet', subnet),
        ('gateway', gateway),
    ])


def get_status(u, args):
    deviceID = DEVICE_ID

    status = u.get_status(deviceID)

    display('get-status', [
        ('ID', deviceID),
        ('date/time', status.sysdatetime),
        ('doors[1]', status.doors[0]),
        ('doors[2]', status.doors[1]),
        ('doors[3]', status.doors[2]),
        ('doors[4]', status.doors[3]),
        ('buttons[1]', status.buttons[0]),
        ('buttons[2]', status.buttons[1]),
        ('buttons[3]', status.buttons[2]),
        ('buttons[4]', status.buttons[3]),
        ('relays   ', '{0:#0x}'.format(status.relays)),
        ('inputs   ', '{0:#0x}'.format(status.inputs)),
        ('error    ', '{0:#0x}'.format(status.syserror)),
        ('info     ', status.info),
        ('seq no.  ', status.seqno),
        ('event timestamp', status.event.timestamp),
        ('      index', status.event.index),
        ('      type', status.event.eventType),
        ('      granted', status.event.granted),
        ('      door', status.event.door),
        ('      direction', status.event.direction),
        ('      card', status.event.card),
        ('      reason', status.event.reason),
    ])


def get_time(u, args):
    deviceID = DEVICE_ID

    dt = u.get_time(deviceID)

    display('set-time', [
        ('ID', deviceID),
        ('date/time', dt),
    ])


def set_time(u, args):
    deviceID = DEVICE_ID
    dt = datetime.datetime.now().strftime('%Y-%m-%d %H:%M:%S')

    u.set_time(deviceID, dt)

    display('set-time', [
        ('ID', deviceID),
        ('date/time', dt),
    ])


def get_listener(u, args):
    deviceID = DEVICE_ID

    listener = u.get_listener(deviceID)

    display('get-listener', [
        ('ID', deviceID),
        ('event listener', listener),
    ])


def set_listener(u, args):
    deviceID = DEVICE_ID
    listener = '192.168.1.100:60001'

    u.set_listener(deviceID, listener)

    display('set-listener', [
        ('ID', deviceID),
        ('event listener', listener),
    ])


def get_door_control(u, args):
    deviceID = DEVICE_ID
    door = DOOR

    control = u.get_door_control(deviceID, door)

    modes = {
        NORMALLY_OPEN: 'normally open',
        NORMALLY_CLOSED: 'normally closed',
        CONTROLLED: 'controlled',
    }

    display('get-door-control', [
        ('ID', deviceID),
        ('door', door),
        ('mode', modes[control.mode]),
        ('delay', control.delay),
    ])


def set_door_control(u, args):
    deviceID = DEVICE_ID
    door = DOOR
    mode = NORMALLY_OPEN
    delay = 9

    u.set_door_control(deviceID, door, mode, delay)

    modes = {
        NORMALLY_OPEN: 'normally open',
        NORMALLY_CLOSED: 'normally closed',
        CONTROLLED: 'controlled',
    }

    display('set-door-control', [
        ('ID', deviceID),
        ('door', door),
        ('mode', modes[mode]),
        ('delay', delay),
    ])


def open_door(u, args):
    deviceID = DEVICE_ID
    door = DOOR

    u.open_door(deviceID, door)

    display('open-door', [
        ('ID', deviceID),
        ('door', door),
    ])


def get_cards(u, args):
    deviceID = DEVICE_ID

    cards = u.get_cards(deviceID)

    display('get-cards', [
        ('ID', deviceID),
        ('cards', cards),
    ])


def get_card(u, args):
    deviceID = DEVICE_ID
    cardNumber = CARD_NUMBER

    card = u.get_card(deviceID, cardNumber)

    display('get-card', [
        ('ID', deviceID),
        ('card number', card.cardNumber),
        ('     from', card.start),
        ('     to', card.end),
        ('     door[1]', card.doors[0]),
        ('     door[2]', card.doors[1]),
        ('     door[3]', card.doors[2]),
        ('     door[4]', card.doors[3]),
    ])


def get_card_by_index(u, args):
    deviceID = DEVICE_ID
    index = CARD_INDEX

    card = u.get_card_by_index(deviceID, index)

    display('get-card-by-index', [
        ('ID', deviceID),
        ('index', index),
        ('card number', card.cardNumber),
        ('     from', card.start),
        ('     to', card.end),
        ('     door[1]', card.doors[0]),
        ('     door[2]', card.doors[1]),
        ('     door[3]', card.doors[2]),
        ('     door[4]', card.doors[3]),
    ])


def put_card(u, args):
    deviceID = DEVICE_ID
    cardNumber = CARD_NUMBER
    start = '2022-01-01'
    end = '2022-12-31'
    doors = [0, 1, 31, 75]

    card = u.put_card(deviceID, cardNumber, start, end, doors)

    display('put-card', [
        ('ID', deviceID),
        ('card-number', cardNumber),
        ('     from', start),
        ('     to', end),
        ('     door[1]', doors[0]),
        ('     door[2]', doors[1]),
        ('     door[3]', doors[2]),
        ('     door[4]', doors[3]),
    ])


def delete_card(u, args):
    deviceID = DEVICE_ID
    cardNumber = CARD_NUMBER

    u.delete_card(deviceID, cardNumber)

    display('delete-card', [
        ('ID', deviceID),
        ('card number', cardNumber),
    ])


def delete_cards(u, args):
    deviceID = DEVICE_ID

    u.delete_cards(deviceID)

    display('delete-cards', [
        ('ID', deviceID),
    ])


def get_event_index(u, args):
    deviceID = DEVICE_ID

    index = u.get_event_index(deviceID)

    display('get-event-index', [
        ('ID', deviceID),
        ('index', index),
    ])


def set_event_index(u, args):
    deviceID = DEVICE_ID
    index = EVENT_INDEX

    u.set_event_index(deviceID, index)

    display('set-event-index', [
        ('ID', deviceID),
        ('index', index),
    ])


def get_event(u, args):
    deviceID = DEVICE_ID
    index = EVENT_INDEX

    event = u.get_event(deviceID, index)

    display('get-event', [
        ('ID', deviceID),
        ('event index', event.index),
        ('      timestamp', event.timestamp),
        ('      type', event.eventType),
        ('      granted', event.granted),
        ('      door', event.door),
        ('      direction', event.direction),
        ('      card number', event.card),
        ('      reason', event.reason),
    ])


def record_special_events(u, args):
    deviceID = DEVICE_ID
    enabled = True

    u.record_special_events(deviceID, enabled)

    display('record-special-events', [
        ('ID', deviceID),
        ('enabled', enabled),
    ])


def get_time_profile(u, args):
    deviceID = DEVICE_ID
    profileID = PROFILE_ID

    profile = u.get_time_profile(deviceID, profileID)

    display('get-time-profile', [
        ('ID', deviceID),
        ('profile ID', profile.ID),
        ('linked profile', profile.linked),
        ('enabled    from', profile.start),
        ('           to', profile.end),
        ('enabled on Monday', profile.monday),
        ('           Tuesday', profile.tuesday),
        ('           Wednesday', profile.wednesday),
        ('           Thursday', profile.thursday),
        ('           Friday', profile.friday),
        ('           Saturday', profile.saturday),
        ('           Sunday', profile.sunday),
        ('segment 1  start', profile.segment1start),
        ('           end', profile.segment1end),
        ('segment 2  start', profile.segment2start),
        ('           end', profile.segment2end),
        ('segment 3  start', profile.segment3start),
        ('           end', profile.segment3end),
    ])


def set_time_profile(u, args):
    deviceID = DEVICE_ID
    profile = uhppoted.TimeProfile(PROFILE_ID, 71, "2022-02-01", "2022-06-30", True, False, True,
                                   True, False, False, True, "08:30", "11:30", "", "", "", "18:00")

    u.set_time_profile(deviceID, profile)

    display('set-time-profile', [
        ('ID', deviceID),
        ('profile ID', profile.ID),
        ('linked profile', profile.linked),
        ('enabled    from', profile.start),
        ('           to', profile.end),
        ('enabled on Monday', profile.monday),
        ('           Tuesday', profile.tuesday),
        ('           Wednesday', profile.wednesday),
        ('           Thursday', profile.thursday),
        ('           Friday', profile.friday),
        ('           Saturday', profile.saturday),
        ('           Sunday', profile.sunday),
        ('segment 1  start', profile.segment1start),
        ('           end', profile.segment1end),
        ('segment 2  start', profile.segment2start),
        ('           end', profile.segment2end),
        ('segment 3  start', profile.segment3start),
        ('           end', profile.segment3end),
    ])


def clear_time_profiles(u, args):
    deviceID = DEVICE_ID

    u.clear_time_profiles(deviceID)

    display('clear-time-profiles', [
        ('ID', deviceID),
    ])


def add_task(u, args):
    deviceID = DEVICE_ID
    task = uhppoted.Task(6, 4, "2022-02-01", "2022-06-30", True, False, True, True, False, False,
                         True, "08:30", 11)

    u.add_task(deviceID, task)

    display('add-task', [
        ('ID', deviceID),
        ('task', task.task),
        ('door', task.door),
        ('enabled from', task.start),
        ('        to', task.end),
        ('enabled on Monday', task.monday),
        ('           Tuesday', task.tuesday),
        ('           Wednesday', task.wednesday),
        ('           Thursday', task.thursday),
        ('           Friday', task.friday),
        ('           Saturday', task.saturday),
        ('           Sunday', task.sunday),
        ('run at', task.at),
        ('cards', task.cards),
    ])


def refresh_tasklist(u, args):
    deviceID = DEVICE_ID

    u.refresh_tasklist(deviceID)

    display('refresh-tasklist', [
        ('ID', deviceID),
    ])


def clear_tasklist(u, args):
    deviceID = DEVICE_ID

    u.clear_tasklist(deviceID)

    display('clear-tasklist', [
        ('ID', deviceID),
    ])


def display(tag, fields):
    w = 0
    for (f, _) in fields:
        w = len(f) if len(f) > w else w

    print(f'{tag}')
    for (f, v) in fields:
        print(f'  {f:<{w}}  {v}')
    print()


if __name__ == '__main__':
    main()
