#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>

#include "examples.h"
#include "uhppoted.h"

void usage();
void help();

const uint32_t DEVICE_ID = 405419896;
const uint8_t DOOR = 4;
const uint32_t CARD_NUMBER = 10058400;
const uint32_t CARD_INDEX = 7;
const uint32_t EVENT_INDEX = 91;
const uint8_t PROFILE_ID = 29;

const char *locale = "";

typedef int(f)(int, char **a);

typedef struct command {
    char *cmd;
    char *help;
    f *fn;
} command;

const command commands[] = {
    {
        .cmd = "get-controllers",
        .help = "Retrieves a list of UHPPOTE controller IDs findable on the local LAN.",
        .fn = getControllers,
    },
    {
        .cmd = "get-controller",
        .help = "Retrieves the basic controller information for a single UHPPOTE controller.",
        .fn = getController,
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
        .help = "Adds or updates a time profile on a controller.",
        .fn = setTimeProfile,
    },
    {
        .cmd = "clear-time-profiles",
        .help = "Deletes all time profiles from a controller.",
        .fn = clearTimeProfiles,
    },
    {
        .cmd = "add-task",
        .help = "Adds a scheduled task to the controller.",
        .fn = addTask,
    },
    {
        .cmd = "refresh-tasklist",
        .help = "Refreshes a controller task list to activate added tasks.",
        .fn = refreshTaskList,
    },
    {
        .cmd = "clear-tasklist",
        .help = "Clears a controller task list.",
        .fn = clearTaskList,
    },
    {
        .cmd = "set-pc-control",
        .help = "Enables/disables remote access control.",
        .fn = setPCControl,
    },
    {
        .cmd = "set-interlock",
        .help = "Sets a controller interlock mode.",
        .fn = setInterlock,
    },
    {
        .cmd = "activate-keypads",
        .help = "Activates and deactivates controller reader access keypads.",
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
        .fn = listen,
    },
};

controller alpha = {.id = 405419896, .address = "192.168.1.100", .transport = "tcp"};
controller beta = {.id = 303986753, .address = "192.168.1.100"};

int main(int argc, char **argv) {
    if (argc < 2) {
        usage();
        return -1;
    }

    char *cmd = argv[1];

    if (strcmp(cmd, "help") == 0) {
        help();
        return 0;
    }

    int N = sizeof(commands) / sizeof(command);
    for (int i = 0; i < N; i++) {
        command c = commands[i];
        int rc;

        if (strcmp(c.cmd, cmd) == 0) {
            setup("0.0.0.0:0", "255.255.255.255", "0.0.0.0:60001", 2500, true, &alpha, &beta, NULL);
            rc = c.fn(argc, argv);
            teardown();

            return rc;
        }
    }

    printf("\n   *** ERROR invalid command (%s)\n", cmd);
    usage();
    return -1;
}

void usage() {
    int N = sizeof(commands) / sizeof(command);

    printf("\n");
    printf("   Usage: example <command>\n");
    printf("\n");
    printf("   Supported commands:\n");

    for (int i = 0; i < N; i++) {
        printf("      %s\n", commands[i].cmd);
    }

    printf("\n");
}

void help() {
    int N = sizeof(commands) / sizeof(command);

    printf("\n");
    printf("   Usage: example <command>\n");
    printf("\n");
    printf("   Commands:\n");

    for (int i = 0; i < N; i++) {
        printf("      %-17s %s\n", commands[i].cmd, commands[i].help);
    }

    printf("\n");
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
        if ((strcmp(argv[ix], "--controller") == 0) && ++ix < argc) {
            if ((lval = strtol(argv[ix], NULL, 10)) > 0) {
                opts.device_id = lval;
            }
        }

        if ((strcmp(argv[ix], "--ip-address") == 0) && ++ix < argc) {
            opts.ip_address = argv[ix];
        }

        if ((strcmp(argv[ix], "--subnet-mask") == 0) && ++ix < argc) {
            opts.subnet_mask = argv[ix];
        }

        if ((strcmp(argv[ix], "--gateway-address") == 0) && ++ix < argc) {
            opts.gateway = argv[ix];
        }

        if ((strcmp(argv[ix], "--listener-address") == 0) && ++ix < argc) {
            opts.listener = argv[ix];
        }

        if ((strcmp(argv[ix], "--card") == 0) && ++ix < argc) {
            if ((lval = strtol(argv[ix], NULL, 10)) > 0) {
                opts.card = lval;
            }
        }

        if ((strcmp(argv[ix], "--card-index") == 0) && ++ix < argc) {
            if ((lval = strtol(argv[ix], NULL, 10)) > 0) {
                opts.card_index = lval;
            }
        }

        if ((strcmp(argv[ix], "--door") == 0) && ++ix < argc) {
            lval = strtol(argv[ix], NULL, 10);
            if (lval >= 1 && lval <= 4) {
                opts.door = (uint8_t)lval;
            }
        }

        if ((strcmp(argv[ix], "--event-index") == 0) && ++ix < argc) {
            if ((lval = strtol(argv[ix], NULL, 10)) > 0) {
                opts.event_index = lval;
            }
        }

        if ((strcmp(argv[ix], "--time-profile") == 0) && ++ix < argc) {
            lval = strtol(argv[ix], NULL, 10);
            if (lval >= 2 && lval <= 255) {
                opts.time_profile_id = (uint8_t)lval;
            }
        }

        ix++;
    }

    return opts;
}

void display(const char *tag, int N, field fields[]) {
    int w = 0;
    for (int i = 0; i < N; i++) {
        if (strlen(fields[i].field) > w) {
            w = strlen(fields[i].field);
        }
    }

    printf("\n%s\n", tag);
    for (int i = 0; i < N; i++) {
        field f = fields[i];
        if (strcmp(f.type, "uint8") == 0) {
            printf("  %-*s  %u\n", w, f.field, f.value.uint8);
        } else if (strcmp(f.type, "uint32") == 0) {
            printf("  %-*s  %u\n", w, f.field, f.value.uint32);
        } else if (strcmp(f.type, "bool") == 0) {
            printf("  %-*s  %s\n", w, f.field, f.value.boolean ? "Y" : "N");
        } else if (strcmp(f.type, "string") == 0) {
            printf("  %-*s  %s\n", w, f.field, f.value.string);
        } else {
            printf("  **** ERROR  unhandled field type (%s)\n", f.type);
        }
    }
    printf("\n");
}
