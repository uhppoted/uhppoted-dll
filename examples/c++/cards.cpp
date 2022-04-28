#include <iostream>

#include "../include/uhppoted.hpp"

using namespace std;

extern const uint32_t DEVICE_ID;
extern const uint32_t CARD_NUMBER;
extern const uint32_t CARD_INDEX;
extern const uint8_t DOOR;

void getCards(uhppoted &u, int argc, char **argv) {
    uint32_t deviceID = DEVICE_ID;

    int N = u.get_cards(deviceID);

    cout << endl
         << "get-cards" << endl;
    cout << "  ID:    " << deviceID << endl;
    cout << "  cards: " << N << endl;
    cout << endl;
}

void getCard(uhppoted &u, int argc, char **argv) {
    uint32_t deviceID = DEVICE_ID;
    uint32_t cardID = CARD_NUMBER;

    card c = u.get_card(deviceID, cardID);

    cout << endl
         << "get-card" << endl;
    cout << "  ID:           " << deviceID << endl;
    cout << "  card number:  " << c.card_number << endl;
    cout << "       from:    " << c.from << endl;
    cout << "       to:      " << c.to << endl;
    cout << "       door[1]: " << static_cast<int>(c.doors[0]) << endl;
    cout << "       door[2]: " << static_cast<int>(c.doors[1]) << endl;
    cout << "       door[3]: " << static_cast<int>(c.doors[2]) << endl;
    cout << "       door[4]: " << static_cast<int>(c.doors[3]) << endl;
    cout << endl;
}

void getCardByIndex(uhppoted &u, int argc, char **argv) {
    uint32_t deviceID = DEVICE_ID;
    uint32_t index = CARD_INDEX;

    card c = u.get_card_by_index(deviceID, index);

    cout << endl
         << "get-card-by-index" << endl;
    cout << "  ID:           " << deviceID << endl;
    cout << "  index:        " << index << endl;
    cout << "  card number:  " << c.card_number << endl;
    cout << "       from:    " << c.from << endl;
    cout << "       to:      " << c.to << endl;
    cout << "       door[1]: " << static_cast<int>(c.doors[0]) << endl;
    cout << "       door[2]: " << static_cast<int>(c.doors[1]) << endl;
    cout << "       door[3]: " << static_cast<int>(c.doors[2]) << endl;
    cout << "       door[4]: " << static_cast<int>(c.doors[3]) << endl;
    cout << endl;
}

void putCard(uhppoted &u, int argc, char **argv) {
    uint32_t deviceID = DEVICE_ID;
    uint32_t card_number = CARD_NUMBER;
    string from = "2022-01-01";
    string to = "2022-12-31";
    uint8_t doors[4] = {0, 1, 31, 75};

    u.put_card(deviceID, card_number, from, to, doors);

    cout << endl
         << "put-card" << endl;
    cout << "  ID:           " << deviceID << endl;
    cout << "  card number:  " << card_number << endl;
    cout << "       from:    " << from << endl;
    cout << "       to:      " << to << endl;
    cout << "       door[1]: " << static_cast<int>(doors[0]) << endl;
    cout << "       door[2]: " << static_cast<int>(doors[1]) << endl;
    cout << "       door[3]: " << static_cast<int>(doors[2]) << endl;
    cout << "       door[4]: " << static_cast<int>(doors[3]) << endl;
    cout << endl;
}

void deleteCard(uhppoted &u, int argc, char **argv) {
    uint32_t deviceID = DEVICE_ID;
    uint32_t card_number = CARD_NUMBER;

    u.delete_card(deviceID, card_number);

    cout << endl
         << "delete-card" << endl;
    cout << "  ID:           " << deviceID << endl;
    cout << "  card number:  " << card_number << endl;
    cout << endl;
}

void deleteCards(uhppoted &u, int argc, char **argv) {
    uint32_t deviceID = DEVICE_ID;

    u.delete_cards(deviceID);

    cout << endl
         << "delete-cards" << endl;
    cout << "  ID: " << deviceID << endl;
    cout << endl;
}
