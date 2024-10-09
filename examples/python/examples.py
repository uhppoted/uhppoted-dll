#!python

import argparse
import ctypes
import sys
import datetime
import platform
import os
import threading
import signal
import sys

from time import sleep

sys.path.append('../../bindings/python')

if 'Windows' in platform.system():
    pwd = os.path.abspath('.')
    os.add_dll_directory(pwd)

    dlldir = os.path.abspath('../../lib')
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
CARD_NUMBER = 10058400
CARD_INDEX = 3
EVENT_INDEX = 43
DOOR = 4
TIME_PROFILE_ID = 29
LOCALE = ""


def commands():
    return {
        'get-controllers': {
            'help': "Retrieves a list of UHPPOTE controller IDs findable on the local LAN",
            'fn': get_controllers,
        },
        'get-controller': {
            'help': "Retrieves the basic controller information for a single UHPPOTE controller",
            'fn': get_controller,
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
        'set-pc-control': {
            'help': "Enables/disables controller remote access control",
            'fn': set_pc_control,
        },
        'set-interlock': {
            'help': "Sets the controller interlock mode",
            'fn': set_interlock,
        },
        'activate-keypads': {
            'help': "Activates and deactivates the controller reader access keypads",
            'fn': activate_keypads,
        },
        'set-door-passcodes': {
            'help': "Sets the supervisor passcodes for keypad-only access to a door",
            'fn': set_door_passcodes,
        },
        'restore-default-parameters': {
            'help': "Resets a controller to the manufacturer default configuration",
            'fn': restore_default_parameters,
        },
        'listen': {
            'help': "Listens for events from controllers",
            'fn': listen,
        },
    }


def main():
    if len(sys.argv) < 2:
        usage()
        return -1

    parser = argparse.ArgumentParser(description='uhppoted-dll examples')

    parser.add_argument('command', type=str, help='command')
    parser.add_argument('--controller', type=int, default=DEVICE_ID, help='controller serial number')
    parser.add_argument('--interlock', type=int, default=1, help='controller door interlock')
    parser.add_argument('--ip-address', type=str, default='192.168.1.100', help='controller IP address')
    parser.add_argument('--subnet-mask', type=str, default='255.255.255.0', help='controller subnet mask')
    parser.add_argument('--gateway-address', type=str, default='192.168.1.5', help='controller gateway address')
    parser.add_argument('--listener-address',
                        type=str,
                        default='192.168.1.100:60001',
                        help='controller event listener address')
    parser.add_argument('--listener-interval', type=int, default=0, help='controller event listener auto-send interval')

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
        bind_addr = '0.0.0.0'
        broadcast_addr = '255.255.255.255'
        listen_addr = '0.0.0.0:60001'
        timeout = 2500
        debug = True
        controllers = [
            uhppoted.Controller(405419896, '192.168.1.100', 'tcp'),
            uhppoted.Controller(303986753, '192.168.1.100'),
        ]

        u = uhppoted.Uhppote(
            uhppote=uhppoted.UHPPOTE(bind_addr, broadcast_addr, listen_addr, timeout, controllers, debug))

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


def get_controllers(u, args):
    list = u.get_devices()

    fields = []
    for id in list:
        fields.append(('', id))

    display(f'get-controllers({len(list)})', fields)


def get_controller(u, args):
    device_id = args.controller

    info = u.get_device(device_id)

    display('get-controller', [
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
        ('ID', status.ID),
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
        ('event timestamp', '-' if status.evt.timestamp == '' else status.evt.timestamp),
        ('      index', status.evt.index),
        ('      type', lookup(LOOKUP_EVENT_TYPE, status.evt.eventType, LOCALE)),
        ('      granted', status.evt.granted),
        ('      door', status.evt.door),
        ('      direction', lookup(LOOKUP_DIRECTION, status.evt.direction, LOCALE)),
        ('      card', status.evt.card),
        ('      reason', lookup(LOOKUP_EVENT_REASON, status.evt.reason, LOCALE)),
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
    interval = u.get_listener_interval(device_id)

    display('get-listener', [
        ('ID', device_id),
        ('event listener', listener),
        ('interval', interval),
    ])


def set_listener(u, args):
    device_id = args.controller
    listener = args.listener_address
    interval = args.listener_interval

    u.set_listener(device_id, listener, interval)

    display('set-listener', [
        ('ID', device_id),
        ('event listener', listener),
        ('interval', interval),
    ])


def get_door_control(u, args):
    device_id = args.controller
    door = args.door

    control = u.get_door_control(device_id, door)

    display('get-door-control', [
        ('ID', device_id),
        ('door', door),
        ('mode', lookup(LOOKUP_MODE, control.mode, LOCALE)),
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
        ('     PIN', card.PIN),
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
        ('     PIN', card.PIN),
    ])


def put_card(u, args):
    device_id = args.controller
    card_number = args.card
    start = '2023-01-01'
    end = '2023-12-31'
    doors = [0, 1, 31, 75]
    PIN = 7531

    card = u.put_card(device_id, card_number, start, end, doors, PIN)

    display('put-card', [
        ('ID', device_id),
        ('card-number', card_number),
        ('     from', start),
        ('     to', end),
        ('     door[1]', doors[0]),
        ('     door[2]', doors[1]),
        ('     door[3]', doors[2]),
        ('     door[4]', doors[3]),
        ('     PIN', PIN),
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
        ('      type', lookup(LOOKUP_EVENT_TYPE, event.eventType, LOCALE)),
        ('      granted', event.granted),
        ('      door', event.door),
        ('      direction', lookup(LOOKUP_DIRECTION, event.direction, LOCALE)),
        ('      card number', event.card),
        ('      reason', lookup(LOOKUP_EVENT_REASON, event.reason, LOCALE)),
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
    profile_id = args.time_profile

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
    profile_id = args.time_profile
    profile = uhppoted.TimeProfile(profile_id, 71, "2022-02-01", "2022-06-30", True, False, True, True, False, False,
                                   True, "08:30", "11:30", "", "", "", "18:00")

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
    task = uhppoted.Task(6, 4, "2022-02-01", "2022-06-30", True, False, True, True, False, False, True, "08:30", 11)

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


def set_pc_control(u, args):
    controller = args.controller
    enabled = True

    u.set_pc_control(controller, enabled)

    display('set-pc-control', [
        ('ID', controller),
        ('enabled', enabled),
    ])


def set_interlock(u, args):
    controller = args.controller
    interlock = args.interlock

    u.set_interlock(controller, interlock)

    display('set-interlock', [
        ('ID', controller),
        ('interlock', interlock),
    ])


def activate_keypads(u, args):
    controller = args.controller
    reader1 = True
    reader2 = True
    reader3 = False
    reader4 = True

    u.activate_keypads(controller, reader1, reader2, reader3, reader4)

    display('activate-keypads', [
        ('ID', controller),
        ('reader 1', reader1),
        ('reader 2', reader2),
        ('reader 3', reader3),
        ('reader 4', reader4),
    ])


def set_door_passcodes(u, args):
    controller = args.controller
    door = args.door
    passcode1 = 12345
    passcode2 = 54321
    passcode3 = 0
    passcode4 = 999999

    u.set_door_passcodes(controller, door, passcode1, passcode2, passcode3, passcode4)

    display('set-door-passcodes', [
        ('ID', controller),
        ('door', door),
        ('passcode 1', passcode1),
        ('passcode 2', passcode2),
        ('passcode 3', passcode3),
        ('passcode 4', passcode4),
    ])


def restore_default_parameters(u, args):
    controller = args.controller

    u.restore_default_parameters(controller)

    display('restore-default-parameters', [
        ('ID', controller),
    ])


def listen(u, args):
    on_sigint = lambda sig, frame: print('CTRL-C')
    listening = threading.Event()
    stop = threading.Event()
    error = threading.Event()
    message = [None]

    thread = threading.Thread(target=listen_events, args=[u, listening, stop, error, message])
    thread.daemon = True
    thread.start()

    count = 0
    while not error.is_set() and not listening.is_set() and count < 10:
        count += 1
        sleep(0.25)

    if error.is_set():
        if message[0]:
            raise Exception(message[0])
        else:
            raise Exception('error starting event listener')

    if not listening.is_set():
        raise Exception(f'timeout starting event listener')

    signal.signal(signal.SIGINT, on_sigint)
    signal.pause()

    stop.set()
    thread.join()


def listen_events(u, listening, stop, error, message):
    try:
        u.listen_events(on_listen_event, on_listen_error, listening, stop, None)
    except Exception as err:
        message[0] = f'{err}'
        error.set()


def on_listen_event(evt, userdata):
    display('event', [
        ('controller', evt.controller),
        ('timestamp', evt.timestamp),
        ('index', evt.index),
        ('event', lookup(LOOKUP_EVENT_TYPE, evt.event, LOCALE)),
        ('granted', evt.granted),
        ('door', evt.door),
        ('direction', lookup(LOOKUP_DIRECTION, evt.direction, LOCALE)),
        ('card', evt.card),
        ('reason', lookup(LOOKUP_EVENT_REASON, evt.reason, LOCALE)),
    ])


def on_listen_error(err):
    print(f' *** ERROR {err}')


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
