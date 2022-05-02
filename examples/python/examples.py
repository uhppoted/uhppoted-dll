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
TIME_PROFILE_ID = 29


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

    parser = argparse.ArgumentParser(description='uhppoted-dll examples')

    parser.add_argument('command', type=str, help='command')

    parser.add_argument('--controller',
                        type=int,
                        default=DEVICE_ID,
                        help='controller serial number')

    parser.add_argument('--ip-address',
                        type=str,
                        default='192.168.1.100',
                        help='controller IP address')

    parser.add_argument('--subnet-mask',
                        type=str,
                        default='255.255.255.0',
                        help='controller subnet mask')

    parser.add_argument('--gateway-address',
                        type=str,
                        default='192.168.1.5',
                        help='controller gateway address')

    parser.add_argument('--listener-address',
                        type=str,
                        default='192.168.1.100:60001',
                        help='controller event listener address')

    parser.add_argument('--card', type=int, default=CARD_NUMBER, help='card number')
    parser.add_argument('--card-index', type=int, default=CARD_INDEX, help='card index')
    parser.add_argument('--door', type=int, default=DOOR, help='controller door ID [1..4]')
    parser.add_argument('--event-index', type=int, default=EVENT_INDEX, help='event index')
    parser.add_argument('--time-profile', type=int, default=TIME_PROFILE_ID, help='time profile ID')

    args = parser.parse_args()
    cmd = args.command

    print(args)

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
                commands()[cmd]['fn'](u, args)
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
    device_id = args.controller

    info = u.get_device(device_id)

    display('get-device', [
        ('ID', device_id),
        ('IP address', info.address),
        ('subnet mask', info.subnet),
        ('gateway address', info.gateway),
        ('MAC', info.MAC),
        ('version', info.version),
        ('released', info.date),
    ])


def set_address(u, args):
    device_id = args.controller
    address = args.ip_address
    subnet = args.subnet_mask
    gateway = args.gateway_address

    u.set_address(device_id, address, subnet, gateway)

    display('set-address', [
        ('ID', device_id),
        ('address', address),
        ('subnet', subnet),
        ('gateway', gateway),
    ])


def get_status(u, args):
    device_id = args.controller

    status = u.get_status(device_id)

    display('get-status', [
        ('ID', device_id),
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
    device_id = args.controller

    dt = u.get_time(device_id)

    display('set-time', [
        ('ID', device_id),
        ('date/time', dt),
    ])


def set_time(u, args):
    device_id = args.controller
    dt = datetime.datetime.now().strftime('%Y-%m-%d %H:%M:%S')

    u.set_time(device_id, dt)

    display('set-time', [
        ('ID', device_id),
        ('date/time', dt),
    ])


def get_listener(u, args):
    device_id = args.controller

    listener = u.get_listener(device_id)

    display('get-listener', [
        ('ID', device_id),
        ('event listener', listener),
    ])


def set_listener(u, args):
    device_id = args.controller
    listener = args.listener_address

    u.set_listener(device_id, listener)

    display('set-listener', [
        ('ID', device_id),
        ('event listener', listener),
    ])


def get_door_control(u, args):
    device_id = args.controller
    door = args.door

    control = u.get_door_control(device_id, door)

    modes = {
        NORMALLY_OPEN: 'normally open',
        NORMALLY_CLOSED: 'normally closed',
        CONTROLLED: 'controlled',
    }

    display('get-door-control', [
        ('ID', device_id),
        ('door', door),
        ('mode', modes[control.mode]),
        ('delay', control.delay),
    ])


def set_door_control(u, args):
    device_id = args.controller
    door = args.door
    mode = NORMALLY_OPEN
    delay = 9

    u.set_door_control(device_id, door, mode, delay)

    modes = {
        NORMALLY_OPEN: 'normally open',
        NORMALLY_CLOSED: 'normally closed',
        CONTROLLED: 'controlled',
    }

    display('set-door-control', [
        ('ID', device_id),
        ('door', door),
        ('mode', modes[mode]),
        ('delay', delay),
    ])


def open_door(u, args):
    device_id = args.controller
    door = args.door

    u.open_door(device_id, door)

    display('open-door', [
        ('ID', device_id),
        ('door', door),
    ])


def get_cards(u, args):
    device_id = args.controller

    cards = u.get_cards(device_id)

    display('get-cards', [
        ('ID', device_id),
        ('cards', cards),
    ])


def get_card(u, args):
    device_id = args.controller
    card_number = args.card

    card = u.get_card(device_id, card_number)

    display('get-card', [
        ('ID', device_id),
        ('card number', card.cardNumber),
        ('     from', card.start),
        ('     to', card.end),
        ('     door[1]', card.doors[0]),
        ('     door[2]', card.doors[1]),
        ('     door[3]', card.doors[2]),
        ('     door[4]', card.doors[3]),
    ])


def get_card_by_index(u, args):
    device_id = args.controller
    index = args.card_index

    card = u.get_card_by_index(device_id, index)

    display('get-card-by-index', [
        ('ID', device_id),
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
    device_id = args.controller
    card_number = args.card
    start = '2022-01-01'
    end = '2022-12-31'
    doors = [0, 1, 31, 75]

    card = u.put_card(device_id, card_number, start, end, doors)

    display('put-card', [
        ('ID', device_id),
        ('card-number', card_number),
        ('     from', start),
        ('     to', end),
        ('     door[1]', doors[0]),
        ('     door[2]', doors[1]),
        ('     door[3]', doors[2]),
        ('     door[4]', doors[3]),
    ])


def delete_card(u, args):
    device_id = args.controller
    card_number = args.card

    u.delete_card(device_id, card_number)

    display('delete-card', [
        ('ID', device_id),
        ('card number', card_number),
    ])


def delete_cards(u, args):
    device_id = args.controller

    u.delete_cards(device_id)

    display('delete-cards', [
        ('ID', device_id),
    ])


def get_event_index(u, args):
    device_id = args.controller

    index = u.get_event_index(device_id)

    display('get-event-index', [
        ('ID', device_id),
        ('index', index),
    ])


def set_event_index(u, args):
    device_id = args.controller
    index = args.event_index

    u.set_event_index(device_id, index)

    display('set-event-index', [
        ('ID', device_id),
        ('index', index),
    ])


def get_event(u, args):
    device_id = args.controller
    index = args.event_index

    event = u.get_event(device_id, index)

    display('get-event', [
        ('ID', device_id),
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
    device_id = args.controller
    enabled = True

    u.record_special_events(device_id, enabled)

    display('record-special-events', [
        ('ID', device_id),
        ('enabled', enabled),
    ])


def get_time_profile(u, args):
    device_id = args.controller
    profile_id = args.time_profile_id

    profile = u.get_time_profile(device_id, profile_id)

    display('get-time-profile', [
        ('ID', device_id),
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
    device_id = args.controller
    profile_id = args.time_profile_id
    profile = uhppoted.TimeProfile(profile_id, 71, "2022-02-01", "2022-06-30", True, False, True,
                                   True, False, False, True, "08:30", "11:30", "", "", "", "18:00")

    u.set_time_profile(device_id, profile)

    display('set-time-profile', [
        ('ID', device_id),
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
    device_id = args.controller

    u.clear_time_profiles(device_id)

    display('clear-time-profiles', [
        ('ID', device_id),
    ])


def add_task(u, args):
    device_id = args.controller
    task = uhppoted.Task(6, 4, "2022-02-01", "2022-06-30", True, False, True, True, False, False,
                         True, "08:30", 11)

    u.add_task(device_id, task)

    display('add-task', [
        ('ID', device_id),
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
    device_id = args.controller

    u.refresh_tasklist(device_id)

    display('refresh-tasklist', [
        ('ID', device_id),
    ])


def clear_tasklist(u, args):
    device_id = args.controller

    u.clear_tasklist(device_id)

    display('clear-tasklist', [
        ('ID', device_id),
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
