#include <iostream>

#include "../include/uhppoted.hpp"
#include "examples.hpp"

using namespace std;

void getCards(uhppoted &u, int argc, char **argv) {
    auto options = parse(argc, argv);
    uint32_t deviceID = options.device_id;

    int N = u.get_cards(deviceID);

    vector<field> fields = {
        field("ID", deviceID),
        field("cards", uint32_t(N)),
    };

    display("get-cards", fields);
}

void getCard(uhppoted &u, int argc, char **argv) {
    auto options = parse(argc, argv);
    uint32_t deviceID = options.device_id;
    uint32_t cardID = options.card;

    card c = u.get_card(deviceID, cardID);

    vector<field> fields = {
        field("ID", deviceID),
        field("card number", c.card_number),
        field("     from", c.from),
        field("     to", c.to),
        field("     door[1]", c.doors[0]),
        field("     door[2]", c.doors[1]),
        field("     door[3]", c.doors[2]),
        field("     door[4]", c.doors[3]),
        field("     PIN", c.PIN),
    };

    display("get-card", fields);
}

void getCardByIndex(uhppoted &u, int argc, char **argv) {
    auto options = parse(argc, argv);
    uint32_t deviceID = options.device_id;
    uint32_t index = options.card_index;

    card c = u.get_card_by_index(deviceID, index);

    vector<field> fields = {
        field("ID", deviceID),
        field("index", index),
        field("card number", c.card_number),
        field("     from", c.from),
        field("     to", c.to),
        field("     door[1]", c.doors[0]),
        field("     door[2]", c.doors[1]),
        field("     door[3]", c.doors[2]),
        field("     door[4]", c.doors[3]),
        field("     PIN", c.PIN),
    };

    display("get-card-by-index", fields);
}

void putCard(uhppoted &u, int argc, char **argv) {
    auto options = parse(argc, argv);
    uint32_t deviceID = options.device_id;
    uint32_t card_number = options.card;
    string from = "2022-01-01";
    string to = "2022-12-31";
    uint8_t doors[4] = {0, 1, 31, 75};
    uint32_t PIN = 7531;

    u.put_card(deviceID, card_number, from, to, doors, PIN);

    vector<field> fields = {
        field("ID", deviceID),
        field("card number", card_number),
        field("     from", from),
        field("     to", to),
        field("     door[1]", doors[0]),
        field("     door[2]", doors[1]),
        field("     door[3]", doors[2]),
        field("     door[4]", doors[3]),
        field("     PIN", PIN),
    };

    display("put-card", fields);
}

void deleteCard(uhppoted &u, int argc, char **argv) {
    auto options = parse(argc, argv);
    uint32_t deviceID = options.device_id;
    uint32_t card_number = options.card;

    u.delete_card(deviceID, card_number);

    vector<field> fields = {
        field("ID", deviceID),
        field("card-number", card_number),
    };

    display("delete-card", fields);
}

void deleteCards(uhppoted &u, int argc, char **argv) {
    auto options = parse(argc, argv);
    uint32_t deviceID = options.device_id;

    u.delete_cards(deviceID);

    vector<field> fields = {
        field("ID", deviceID),
    };

    display("delete-cards", fields);
}
