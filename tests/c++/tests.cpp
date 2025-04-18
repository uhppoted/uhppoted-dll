#include <iomanip>
#include <iostream>
#include <map>
#include <string>

#include "../include/uhppoted.hpp"
#include "tests.hpp"

using namespace std;

typedef bool (*f)(uhppoted &);

typedef struct test {
    string command;
    f fn;
} test;

vector<test> tests = {
    {"get-controllers", getControllers},
    {"get-controller", getController},
    {"set-address", setAddress},
    {"get-status", getStatus},
    {"get-status-no-event", getStatusNoEvent},
    {"get-time", getTime},
    {"set-time", setTime},
    {"get-listener", getListener},
    {"set-listener", setListener},
    {"get-door-control", getDoorControl},
    {"set-door-control", setDoorControl},
    {"open-door", openDoor},
    {"get-cards", getCards},
    {"get-card", getCard},
    {"get-card-by-index", getCardByIndex},
    {"put-card", putCard},
    {"delete-card", deleteCard},
    {"delete-cards", deleteCards},
    {"get-event-index", getEventIndex},
    {"set-event-index", setEventIndex},
    {"get-event", getEvent},
    {"record-special-events", recordSpecialEvents},
    {"get-time-profile", getTimeProfile},
    {"set-time-profile", setTimeProfile},
    {"clear-time-profiles", clearTimeProfiles},
    {"add-task", addTask},
    {"refresh-tasklist", refreshTaskList},
    {"clear-tasklist", clearTaskList},
    {"set-pc-control", setPCControl},
    {"set-interlock", setInterlock},
    {"activate-keypads", activateKeypads},
    {"set-door-passcodes", setDoorPasscodes},
    {"get-antipassback", getAntiPassback},
    {"set-antipassback", setAntiPassback},
    {"restore-default-parameters", restoreDefaultParameters},
    {"errors", errors},
    {"lookup", internationalisation},
    {"structs", structs},
    {"listen", listen},
};

const uint32_t DEVICE_ID = 405419896;
const uint32_t DEVICE_ID2 = 303986753;
const uint32_t CARD_ID = 8165538;
const uint32_t CARD_INDEX = 19;
const uint32_t EVENT_INDEX = 51;
const uint8_t DOOR = 4;
const uint8_t PROFILE_ID = 49;
const uint8_t ANTIPASSBACK = 2;

const controller ALPHA = {.id = 405419896, .address = "192.168.1.101", "tcp"};
const controller BETA = {.id = 303986753, .address = "192.168.1.102"};

void usage();
bool passed(const string &);
bool failed(const string &);

int main(int argc, char **argv) {
    string cmd;

    if (argc > 1) {
        cmd = argv[1];
    }

    vector<controller> controllers = {ALPHA, BETA};

    uhppoted u("0.0.0.0", "255.255.255.255", "0.0.0.0:60001", 2500, controllers, true);

    try {
        if (cmd == "help") {
            cout << endl;
            usage();
            return 0;
        }

        if (cmd == "" || cmd == "all") {
            bool ok = true;
            for (auto it = tests.begin(); it != tests.end(); it++) {
                ok = it->fn(u) ? ok : false;
            }

            return ok ? 0 : -1;
        }

        for (auto it = tests.begin(); it != tests.end(); it++) {
            if (it->command == cmd) {
                return it->fn(u) ? 0 : -1;
            }
        }

        cerr << endl
             << "   *** ERROR invalid command (" << cmd << ")" << endl
             << endl;
        usage();

    } catch (const exception &e) {
        cerr << endl
             << " *** ERROR " << e.what() << endl
             << endl;
        return -1;
    }
}

void usage() {
    cout << "   Usage: test <command>" << endl;
    cout << endl;
    cout << "   Supported commands:" << endl;

    for (auto it = tests.begin(); it != tests.end(); it++) {
        cout << "      " << it->command << endl;
    }
    cout << endl;
}

extern bool evaluate(const std::string &tag, const std::vector<result> &resultset) {
    bool ok = true;

    for (auto ix = resultset.begin(); ix != resultset.end(); ix++) {
        auto field = get<0>(*ix);
        auto expected = get<1>(*ix);
        auto value = get<2>(*ix);
        auto type = expected.type().name();

        if (type == typeid(uint8_t).name()) {
            auto p = any_cast<uint8_t>(expected);
            auto q = any_cast<uint8_t>(value);
            if (p != q) {
                cout << setw(26) << tag << " incorrect " << field << " (expected:" << static_cast<int>(p) << ", got:" << static_cast<int>(q) << ")" << endl;
                ok = false;
            }
        } else if (type == typeid(uint32_t).name()) {
            auto p = any_cast<uint32_t>(expected);
            auto q = any_cast<uint32_t>(value);

            if (p != q) {
                cout << setw(26) << tag << " incorrect " << field << " (expected:" << p << ", got:" << q << ")" << endl;
                ok = false;
            }
        } else if (type == typeid(bool).name()) {
            auto p = any_cast<bool>(expected);
            auto q = any_cast<bool>(value);

            if (p != q) {
                cout << setw(26) << tag << " incorrect " << field << " (expected:" << p << ", got:" << q << ")" << endl;
                ok = false;
            }
        } else if (type == typeid(string).name()) {
            auto p = any_cast<string>(expected);
            auto q = any_cast<string>(value);

            if (p != q) {
                cout << setw(26) << tag << " incorrect " << field << " (expected:" << p << ", got:" << q << ")" << endl;
                ok = false;
            }
        } else {
            cout << "invalid result type: field::" << field << ",type:" << type << endl;
            return false;
        }
    }

    if (!ok) {
        return failed(tag);
    }

    return passed(tag);
}

extern bool passed(const string &tag) {
    cout << setw(26) << left << tag << " ok" << endl;

    return true;
}

extern bool failed(const string &tag) {
    cout << setw(26) << left << tag << " failed" << endl;

    return false;
}
