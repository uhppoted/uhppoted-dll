#include <iostream>

#include "../include/uhppoted.hpp"
#include "examples.hpp"

using namespace std;

extern const uint32_t DEVICE_ID;

void addTask(uhppoted &u, int argc, char **argv) {
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

    u.add_task(deviceID, task);

    vector<field> fields = {
        field("ID", deviceID),
        field("task", task.task),
        field("door", task.door),
        field("enabled from", task.from),
        field("        to", task.to),
        field("enabled on Monday", task.monday),
        field("           Tuesday", task.tuesday),
        field("           Wednesday", task.wednesday),
        field("           Thursday", task.thursday),
        field("           Friday", task.friday),
        field("           Saturday", task.saturday),
        field("           Sunday", task.sunday),
        field("           run at", task.at),
        field("cards", task.cards),
    };

    display("add-task", fields);
}

void refreshTaskList(uhppoted &u, int argc, char **argv) {
    uint32_t deviceID = DEVICE_ID;

    u.refresh_tasklist(deviceID);

    vector<field> fields = {
        field("ID", deviceID),
    };

    display("refresh-tasklist", fields);
}

void clearTaskList(uhppoted &u, int argc, char **argv) {
    uint32_t deviceID = DEVICE_ID;

    u.clear_tasklist(deviceID);

    vector<field> fields = {
        field("ID", deviceID),
    };

    display("clear-tasklist", fields);
}
