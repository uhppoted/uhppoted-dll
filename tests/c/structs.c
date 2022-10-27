#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "tests.h"
#include "uhppoted.h"

bool structs() {
    // setup("0.0.0.0:0", "255.255.255.255", "0.0.0.0:60001", 2500, true, &alpha, &beta, NULL);

    const char *tag = "structs";
    char *uhppote;

    const uint32_t id = 0xffffffff;

    if (get_time(id, &uhppote) != 0) {
        printf("ERROR %s\n", errmsg());
        return false;
    }

    const result resultset[] = {
        {
            .field = "UHPPOTE",
            .type = "string",
            .value.string.expected = "{\"UHPPOTE\":{\"bind\":\"0.0.0.0:0\",\"broadcast\":\"255.255.255.255:60000\",\"listen\":\"0.0.0.0:60001\",\"debug\":true,\"controllers\":{}}}",
            .value.string.value = uhppote,
        },
    };

    bool ok = evaluate(tag, sizeof(resultset) / sizeof(result), resultset);

    free(uhppote);

    return ok;
}
