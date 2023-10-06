#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "tests.h"
#include "uhppoted.h"

bool structs() {
  const char *tag = "structs";
  struct device d;
  bool debug1 = true;
  bool debug2 = true;

  setup("0.0.0.0:0", "255.255.255.255", "0.0.0.0:60001", 2500, true, NULL);
  if (get_device(0xffffffff, &d) != 0) {
    debug1 = false;
  }

  setup("0.0.0.0:0", "255.255.255.255", "0.0.0.0:60001", 2500, false, NULL);
  if (get_device(0xfffffffe, &d) != 0) {
    debug2 = false;
  }

  const result resultset[] = {
      {
          .field = "UHPPOTE.debug",
          .type = "boolean",
          .value.boolean.expected = true,
          .value.boolean.value = debug1,
      },
      {
          .field = "UHPPOTE.debug",
          .type = "boolean",
          .value.boolean.expected = true,
          .value.boolean.value = debug2,
      },
  };

  bool ok = evaluate(tag, sizeof(resultset) / sizeof(result), resultset);

  return ok;
}
