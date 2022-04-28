#include <iomanip>
#include <iostream>

#include "../include/uhppoted.hpp"
#include "tests.hpp"

using namespace std;

bool addTask(uhppoted &u) {
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

    u.add_task(DEVICE_ID, task);

    vector<result> rs = {};

    return evaluate("add-task", rs);
}

bool refreshTaskList(uhppoted &u) {
    u.refresh_tasklist(DEVICE_ID);

    vector<result> rs = {};

    return evaluate("refresh-tasklist", rs);
}

bool clearTaskList(uhppoted &u) {
    u.clear_tasklist(DEVICE_ID);

    vector<result> rs = {};

    return evaluate("clear-tasklist", rs);
}
