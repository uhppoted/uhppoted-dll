#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "tests.h"
#include "uhppoted.h"

bool addTask() {
    task task = {
        .task = 4,
        .door = 3,
        .from = "2022-02-01",
        .to = "2022-06-30",
        .monday = true,
        .tuesday = false,
        .wednesday = true,
        .thursday = true,
        .friday = false,
        .saturday = false,
        .sunday = true,
        .at = "09:45",
        .cards = 11,
    };

    if (add_task(DEVICE_ID, &task) < 0) {
        printf("ERROR %s\n", errmsg());
        return -1;
    }

    const result resultset[] = {};

    return evaluate("add-task", sizeof(resultset) / sizeof(result), resultset);
}

bool refreshTaskList() {
    if (refresh_tasklist(DEVICE_ID) < 0) {
        printf("ERROR %s\n", errmsg());
        return -1;
    }

    const result resultset[] = {};

    return evaluate("refresh-tasklist", sizeof(resultset) / sizeof(result), resultset);
}
