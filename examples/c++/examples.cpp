#include <algorithm>
#include <ctime>
#include <iomanip>
#include <iostream>

#include "../include/uhppoted.hpp"
#include "examples.hpp"

using namespace std;

void usage();
void help();

extern const uint32_t DEVICE_ID = 405419896;
extern const uint32_t CARD_NUMBER = 8000001;
extern const uint32_t CARD_INDEX = 7;
extern const uint32_t EVENT_INDEX = 7;
extern const uint8_t DOOR = 4;
extern const uint8_t PROFILE_ID = 29;

extern const string LOCALE = "";

const controller ALPHA = {.id = 405419896, .address = "192.168.1.100"};
const controller BETA = {.id = 303986753, .address = "192.168.1.100"};

typedef void(f)(uhppoted &, int, char **);

typedef struct command {
    string cmd;
    string help;
    f *fn;
} command;

const vector<command> commands = {
    {
        .cmd = "get-devices",
        .help = "Retrieves a list of UHPPOTE controller IDs findable on the local LAN.",
        .fn = getDevices,
    },
    {
        .cmd = "get-device",
        .help = "Retrieves the basic device information for a single UHPPOTE controller.",
        .fn = getDevice,
    },
    {
        .cmd = "set-address",
        .help = "Sets the controller IPv4 address, subnet mask and gateway address.",
        .fn = setAddress,
    },
    {
        .cmd = "get-status",
        .help = "Retrieves a controller status.",
        .fn = getStatus,
    },
    {
        .cmd = "get-time",
        .help = "Retrieves a controller current date/time (YYYY-MM-DD HH:mm:ss).",
        .fn = getTime,
    },
    {
        .cmd = "set-time",
        .help = "Sets a controller current date/time (YYYY-MM-DD HH:mm:ss).",
        .fn = setTime,
    },
    {
        .cmd = "get-listener",
        .help = "Retrieves a controller's configured event listener address.",
        .fn = getListener,
    },
    {
        .cmd = "set-listener",
        .help = "Configures a controller's event listener address and port.",
        .fn = setListener,
    },
    {
        .cmd = "get-door-control",
        .help = "Retrieves the control state and open delay for a controller door.",
        .fn = getDoorControl,
    },
    {
        .cmd = "set-door-control",
        .help = "Sets the control mode and delay for a controller door.",
        .fn = setDoorControl,
    },
    {
        .cmd = "open-door",
        .help = "Remotely opens a controller door.",
        .fn = openDoor,
    },
    {
        .cmd = "get-cards",
        .help = "Retrieves the number of cards stored on a controller.",
        .fn = getCards,
    },
    {
        .cmd = "get-card",
        .help = "Retrieves the card detail for card number from a controller.",
        .fn = getCard,
    },
    {
        .cmd = "get-card-by-index",
        .help = "Retrieves the card detail for the card stored at an index on a controller.",
        .fn = getCardByIndex,
    },
    {
        .cmd = "put-card",
        .help = "Adds or updates the card detail for card number stored on a controller.",
        .fn = putCard,
    },
    {
        .cmd = "delete-card",
        .help = "Deletes a card from a controller.",
        .fn = deleteCard,
    },
    {
        .cmd = "delete-cards",
        .help = "Deletes all cards from a controller.",
        .fn = deleteCards,
    },
    {
        .cmd = "get-event-index",
        .help = "Retrieves the current event index from a controller.",
        .fn = getEventIndex,
    },
    {
        .cmd = "set-event-index",
        .help = "Sets the current event index on a controller.",
        .fn = setEventIndex,
    },
    {
        .cmd = "get-event",
        .help = "Retrieves the event at the index from a controller.",
        .fn = getEvent,
    },
    {
        .cmd = "record-special-events",
        .help = "Enables/disables recording additional events for a controller.",
        .fn = recordSpecialEvents,
    },
    {
        .cmd = "get-time-profile",
        .help = "Retrieves a time profile from a controller.",
        .fn = getTimeProfile,
    },
    {
        .cmd = "set-time-profile",
        .help = "Adds or update a time profile on a controller.",
        .fn = setTimeProfile,
    },
    {
        .cmd = "clear-time-profiles",
        .help = "Deletes all time profiles from a controller.",
        .fn = clearTimeProfiles,
    },
    {
        .cmd = "add-task",
        .help = "Adds a scheduled task to a controller.",
        .fn = addTask,
    },
    {
        .cmd = "refresh-tasklist",
        .help = "Refreshes a controller task list to activate added tasks.",
        .fn = refreshTaskList,
    },
    {
        .cmd = "clear-tasklist",
        .help = "Clear a controller task list.",
        .fn = clearTaskList,
    },
    {
        .cmd = "set-pc-control",
        .help = "Enables/disables controller remote access control.",
        .fn = setPCControl,
    },
    {
        .cmd = "set-interlock",
        .help = "Sets a controller interlock mode.",
        .fn = setInterlock,
    },
    {
        .cmd = "activate-keypads",
        .help = "Activates/deactivates a controller reader access keypads.",
        .fn = activateKeypads,
    },
    {
        .cmd = "set-door-passcodes",
        .help = "Sets the supervisor passcodes for keypad only access to a door.",
        .fn = setDoorPasscodes,
    },
    {
        .cmd = "restore-default-parameters",
        .help = "Resets a controller to the manufacturer default configuration.",
        .fn = restoreDefaultParameters,
    },
    {
        .cmd = "listen",
        .help = "Listens for events from controllers.",
        .fn = listenEvents,
    },
};

int main(int argc, char **argv) {
    if (argc < 2) {
        usage();
        return -1;
    }

    string cmd(argv[1]);
    if (cmd == "help") {
        help();
        return 0;
    }

    auto it = find_if(commands.begin(), commands.end(), [=](const command &v) { return v.cmd == cmd; });
    if (it == commands.end()) {
        cerr << endl
             << "   *** ERROR invalid command '" << cmd << "'" << endl;
        usage();
        return -1;
    }

    try {
        uhppoted u("0.0.0.0", "255.255.255.255", "0.0.0.0:60001", 2500, {ALPHA, BETA}, true);
        it->fn(u, argc, argv);
    } catch (const exception &e) {
        cerr << endl
             << " *** ERROR " << e.what() << endl
             << endl;
        return -1;
    }

    return 0;
}

void usage() {
    cout << endl;
    cout << "   Usage: example <command>" << endl;
    cout << endl;
    cout << "   Supported commands:" << endl;

    for (auto it = commands.begin(); it != commands.end(); it++) {
        cout << "      " << it->cmd << endl;
    }

    cout << endl;
}

void help() {
    cout << endl;
    cout << "   Usage: example <command>" << endl;
    cout << endl;
    cout << "   Commands:" << endl;

    for (auto it = commands.begin(); it != commands.end(); it++) {
        cout << "      " << setw(18) << left << it->cmd << "  " << it->help << endl;
    }

    cout << endl;
}

options parse(int argc, char **argv) {
    options opts = {
        .device_id = DEVICE_ID,
        .ip_address = "192.168.1.125",
        .subnet_mask = "255.255.255.0",
        .gateway = "192.168.1.0",
        .listener = "192.168.1.100:60001",
        .card = CARD_NUMBER,
        .card_index = CARD_INDEX,
        .door = DOOR,
        .event_index = EVENT_INDEX,
        .time_profile_id = PROFILE_ID,
    };

    long lval;
    int ix = 2;

    while (ix < argc) {
        const string arg = argv[ix++];

        if (arg == "--controller" && ix < argc) {
            if ((lval = strtol(argv[ix++], NULL, 10)) > 0) {
                opts.device_id = lval;
            }
        }

        if (arg == "--ip-address" && ix < argc) {
            opts.ip_address = argv[ix++];
        }

        if (arg == "--subnet-mask" && ix < argc) {
            opts.subnet_mask = argv[ix++];
        }

        if (arg == "--gateway-address" && ix < argc) {
            opts.gateway = argv[ix++];
        }

        if (arg == "--listener-address" && ix < argc) {
            opts.listener = argv[ix++];
        }

        if (arg == "--card" && ix < argc) {
            if ((lval = strtol(argv[ix++], NULL, 10)) > 0) {
                opts.card = lval;
            }
        }

        if (arg == "--card-index" && ix < argc) {
            if ((lval = strtol(argv[ix++], NULL, 10)) > 0) {
                opts.card_index = lval;
            }
        }

        if (arg == "--door" && ix < argc) {
            lval = strtol(argv[ix++], NULL, 10);
            if (lval >= 1 && lval <= 4) {
                opts.door = (uint8_t)lval;
            }
        }

        if (arg == "--event-index" && ix < argc) {
            if ((lval = strtol(argv[ix++], NULL, 10)) > 0) {
                opts.event_index = lval;
            }
        }

        if (arg == "--time-profile" && ix < argc) {
            lval = strtol(argv[ix++], NULL, 10);
            if (lval >= 2 && lval <= 255) {
                opts.time_profile_id = (uint8_t)lval;
            }
        }
    }

    return opts;
}

void display(const std::string &tag, const std::vector<field> &fields) {
    unsigned w = 0;
    for (auto ix = fields.begin(); ix != fields.end(); ix++) {
        string field = get<0>(*ix);

        w = field.length() > w ? field.length() : w;
    }

    cout << endl
         << tag << endl;

    for (auto ix = fields.begin(); ix != fields.end(); ix++) {
        auto field = get<0>(*ix);
        auto value = get<1>(*ix);
        auto type = value.type().name();

        if (type == typeid(uint8_t).name()) {
            cout << "  " << setw(w) << left << field << " " << static_cast<int>(any_cast<uint8_t>(value)) << endl;
        } else if (type == typeid(uint32_t).name()) {
            cout << "  " << setw(w) << left << field << " " << any_cast<uint32_t>(value) << endl;
        } else if (type == typeid(bool).name()) {
            cout << "  " << setw(w) << left << field << " " << (any_cast<bool>(value) == 1 ? "Y" : "N") << endl;
        } else if (type == typeid(string).name()) {
            cout << "  " << setw(w) << left << field << " " << any_cast<string>(value) << endl;
        } else {
            cout << "invalid field type: field::" << field << ",type:" << type << endl;
        }
    }

    cout << endl;
}
