#include <iomanip>
#include <iostream>

#include "../include/uhppoted.hpp"
#include "tests.hpp"

using namespace std;

bool getCards(uhppoted &u) {
    uint32_t N = u.get_cards(DEVICE_ID);

    vector<result> rs = {
        result("cards count", "uint32", {.uint32 = 39}, {.uint32 = N}),
    };

    return evaluate("get-cards", rs);
}

bool getCard(uhppoted &u) {
    auto card = u.get_card(DEVICE_ID, CARD_ID);

    vector<result> rs = {
        result("card number", "uint32", {.uint32 = 8165538}, {.uint32 = card.card_number}),
        result("card 'from' date", "string", {.string = "2022-01-01"}, {.string = card.from.c_str()}),
        result("card 'to' date", "string", {.string = "2022-12-31"}, {.string = card.to.c_str()}),
        result("card doors[1]", "uint8", {.uint8 = 0}, {.uint8 = card.doors[0]}),
        result("card doors[2]", "uint8", {.uint8 = 1}, {.uint8 = card.doors[1]}),
        result("card doors[3]", "uint8", {.uint8 = 31}, {.uint8 = card.doors[2]}),
        result("card doors[4]", "uint8", {.uint8 = 75}, {.uint8 = card.doors[3]}),
    };

    return evaluate("get-card", rs);
}

bool getCardByIndex(uhppoted &u) {
    auto card = u.get_card_by_index(DEVICE_ID, CARD_INDEX);

    vector<result> rs = {
        result("card number", "uint32", {.uint32 = 8165538}, {.uint32 = card.card_number}),
        result("card 'from' date", "string", {.string = "2022-01-01"}, {.string = card.from.c_str()}),
        result("card 'to' date", "string", {.string = "2022-12-31"}, {.string = card.to.c_str()}),
        result("card doors[1]", "uint8", {.uint8 = 0}, {.uint8 = card.doors[0]}),
        result("card doors[2]", "uint8", {.uint8 = 1}, {.uint8 = card.doors[1]}),
        result("card doors[3]", "uint8", {.uint8 = 31}, {.uint8 = card.doors[2]}),
        result("card doors[4]", "uint8", {.uint8 = 75}, {.uint8 = card.doors[3]}),
    };

    return evaluate("get-card-by-index", rs);
}

bool putCard(uhppoted &u) {
    uint8_t doors[4] = {0, 1, 31, 75};

    u.put_card(DEVICE_ID, CARD_ID, "2022-01-01", "2022-12-31", doors);

    vector<result> rs = {};

    return evaluate("put-card", rs);
}

bool deleteCard(uhppoted &u) {
    u.delete_card(DEVICE_ID, CARD_ID);

    vector<result> rs = {};

    return evaluate("delete-card", rs);
}

bool deleteCards(uhppoted &u) {
    u.delete_cards(DEVICE_ID);

    vector<result> rs = {};

    return evaluate("delete-cards", rs);
}
