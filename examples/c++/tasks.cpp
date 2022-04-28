#include <iostream>

#include "../include/uhppoted.hpp"

using namespace std;

extern const uint32_t DEVICE_ID;

int addTask(uhppoted &u, int argc, char **argv) {
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

    try {
        u.add_task(deviceID, task);

        cout << endl
             << "add-task" << endl;
        cout << "  ID:                   " << deviceID << endl;
        cout << "  task:                 " << static_cast<int>(task.task) << endl;
        cout << "  door:                 " << static_cast<int>(task.door) << endl;
        cout << "  enabled from:         " << task.from << endl;
        cout << "          to:           " << task.to << endl;
        cout << "  enabled on Monday:    " << static_cast<int>(task.monday) << endl;
        cout << "             Tuesday:   " << static_cast<int>(task.tuesday) << endl;
        cout << "             Wednesday: " << static_cast<int>(task.wednesday) << endl;
        cout << "             Thursday:  " << static_cast<int>(task.thursday) << endl;
        cout << "             Friday:    " << static_cast<int>(task.friday) << endl;
        cout << "             Saturday:  " << static_cast<int>(task.saturday) << endl;
        cout << "             Sunday:    " << static_cast<int>(task.sunday) << endl;
        cout << "  run at:               " << task.at << endl;
        cout << "  cards:                " << static_cast<int>(task.cards) << endl;
        cout << endl;

        return 0;
    } catch (const exception &e) {
        cerr << endl
             << " *** ERROR " << e.what() << endl
             << endl;
    }

    return -1;
}

int refreshTaskList(uhppoted &u, int argc, char **argv) {
    uint32_t deviceID = DEVICE_ID;

    try {
        u.refresh_tasklist(deviceID);

        cout << endl
             << "refresh-tasklist" << endl;
        cout << "  ID: " << deviceID << endl;
        cout << endl;

        return 0;
    } catch (const exception &e) {
        cerr << endl
             << " *** ERROR " << e.what() << endl
             << endl;
    }

    return -1;
}

int clearTaskList(uhppoted &u, int argc, char **argv) {
    uint32_t deviceID = DEVICE_ID;

    try {
        u.clear_tasklist(deviceID);

        cout << endl
             << "clear-tasklist" << endl;
        cout << "  ID: " << deviceID << endl;
        cout << endl;

        return 0;
    } catch (const exception &e) {
        cerr << endl
             << " *** ERROR " << e.what() << endl
             << endl;
    }

    return -1;
}
