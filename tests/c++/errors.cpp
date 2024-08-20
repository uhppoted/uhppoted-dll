#include "../include/uhppoted.hpp"
#include "tests.hpp"

using namespace std;

const uint32_t INVALID_DEVICE_ID = 987654321;

bool errGetController(uhppoted &);

bool errors(uhppoted &u) {
    struct {
        bool get_controller;
    } fn = {
        .get_controller = false,
    };

    fn.get_controller = errGetController(u);

    // clang-format off
    vector<result> rs = {
        result("get_controller", bool(true),  fn.get_controller ),
    };
    // clang-format on

    return evaluate("errors", rs);
}

bool errGetController(uhppoted &u) {
    try {
        u.get_device(INVALID_DEVICE_ID);
        return false;
    } catch (uhppoted_exception e) {
        return true;
    }
}
