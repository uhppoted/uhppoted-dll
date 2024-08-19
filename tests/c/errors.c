#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "tests.h"
#include "uhppoted.h"

const uint32_t INVALID_DEVICE_ID = 987654321;

bool errGetController();

bool errors() {
    const char *tag = "errors";

    struct {
        bool get_controller;
    } fn = {
        .get_controller = false,
    };

    fn.get_controller = errGetController();

    // clang-format off
    const result resultset[] = {
        { .field = "get_controller",       .type = "boolean", .value.boolean.expected = true,           .value.boolean.value = fn.get_controller },
    };
    // clang-format on

    return evaluate(tag, sizeof(resultset) / sizeof(result), resultset);
}

bool errGetController() {
    struct device d;

    return get_device(INVALID_DEVICE_ID, &d) != 0;
}
