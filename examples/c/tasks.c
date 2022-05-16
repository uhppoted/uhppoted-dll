#include <stdio.h>
#include <stdlib.h>

#include "examples.h"
#include "uhppoted.h"

int addTask(int argc, char **argv) {
    uint32_t deviceID = parse(argc, argv).device_id;

    task task = {
        .task = 6,
        .door = 4,
        .from = "2022-02-01",
        .to = "2022-06-30",
        .monday = true,
        .tuesday = false,
        .wednesday = true,
        .thursday = true,
        .friday = false,
        .saturday = false,
        .sunday = true,
        .at = "08:30",
        .cards = 11,
    };

    if (add_task(deviceID, &task) < 0) {
        printf("ERROR %s\n", errmsg());
        return -1;
    }

    field fields[] = {
        {.field = "ID", .type = "uint32", .value.uint32 = deviceID},
        {.field = "task", .type = "uint8", .value.uint8 = task.task},
        {.field = "door", .type = "uint8", .value.uint8 = task.door},
        {.field = "enabled from", .type = "string", .value.string = task.from},
        {.field = "        to", .type = "string", .value.string = task.to},
        {.field = "enabled on Monday", .type = "bool", .value.boolean = task.monday},
        {.field = "           Tuesday", .type = "bool", .value.boolean = task.tuesday},
        {.field = "           Wednesday", .type = "bool", .value.boolean = task.wednesday},
        {.field = "           Thursday", .type = "bool", .value.boolean = task.thursday},
        {.field = "           Friday", .type = "bool", .value.boolean = task.friday},
        {.field = "           Saturday", .type = "bool", .value.boolean = task.saturday},
        {.field = "           Sunday", .type = "bool", .value.boolean = task.sunday},
        {.field = "at", .type = "string", .value.string = task.at},
        {.field = "cards", .type = "uint8", .value.uint8 = task.cards},
    };

    display("add-task", sizeof(fields) / sizeof(field), fields);

    return 0;
}

int refreshTaskList(int argc, char **argv) {
    uint32_t deviceID = parse(argc, argv).device_id;

    if (refresh_tasklist(deviceID) < 0) {
        printf("ERROR %s\n", errmsg());
        return -1;
    }

    field fields[] = {
        {.field = "ID", .type = "uint32", .value.uint32 = deviceID},
    };

    display("refresh_tasklist", sizeof(fields) / sizeof(field), fields);

    return 0;
}

int clearTaskList(int argc, char **argv) {
    uint32_t deviceID = parse(argc, argv).device_id;

    if (clear_tasklist(deviceID) < 0) {
        printf("ERROR %s\n", errmsg());
        return -1;
    }

    field fields[] = {
        {.field = "ID", .type = "uint32", .value.uint32 = deviceID},
    };

    display("clear-tasklist", sizeof(fields) / sizeof(field), fields);

    return 0;
}
