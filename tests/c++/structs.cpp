#include <iomanip>
#include <iostream>

#include "../include/uhppoted.hpp"
#include "tests.hpp"

using namespace std;

bool structs(uhppoted &u) {
    vector<controller> controllers = {};

    uhppoted utrue("0.0.0.0", "255.255.255.255:60000", "0.0.0.0:60001", 2500, controllers, true);
    uhppoted ufalse("0.0.0.0", "255.255.255.255:60000", "0.0.0.0:60001", 2500, controllers, false);

    auto d1 = utrue.get_device(0xffffffff);
    auto d2 = ufalse.get_device(0xfffffffe);

    return passed("structs");
}
