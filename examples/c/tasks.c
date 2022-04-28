#include <stdio.h>
#include <stdlib.h>

#include "uhppoted.h"

extern uint32_t DEVICE_ID;

int addTask(int argc, char **argv) {
    uint32_t deviceID = DEVICE_ID;

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

    printf("\nadd-task\n");
    printf("  ID:                   %u\n", deviceID);
    printf("  task:                 %u\n", task.task);
    printf("  door:                 %u\n", task.door);
    printf("  enabled from:         %s\n", task.from);
    printf("          to:           %s\n", task.to);
    printf("  enabled on Monday:    %d\n", task.monday);
    printf("             Tuesday:   %d\n", task.tuesday);
    printf("             Wednesday: %d\n", task.wednesday);
    printf("             Thursday:  %d\n", task.thursday);
    printf("             Friday:    %d\n", task.friday);
    printf("             Saturday:  %d\n", task.saturday);
    printf("             Sunday:    %d\n", task.sunday);
    printf("  at:                   %s\n", task.at);
    printf("  cards:                %u\n", task.cards);
    printf("\n");

    return 0;
}

int refreshTaskList(int argc, char **argv) {
    uint32_t deviceID = DEVICE_ID;

    if (refresh_tasklist(deviceID) < 0) {
        printf("ERROR %s\n", errmsg());
        return -1;
    }

    printf("\nrefresh-tasklist\n");
    printf("  ID: %u\n", deviceID);
    printf("\n");

    return 0;
}

int clearTaskList(int argc, char **argv) {
    uint32_t deviceID = DEVICE_ID;

    if (clear_tasklist(deviceID) < 0) {
        printf("ERROR %s\n", errmsg());
        return -1;
    }

    printf("\nclear-tasklist\n");
    printf("  ID: %u\n", deviceID);
    printf("\n");

    return 0;
}
