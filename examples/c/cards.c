#include <stdio.h>
#include <stdlib.h>

#include "examples.h"
#include "uhppoted.h"

int getCards(int argc, char **argv) {
    uint32_t deviceID = parse(argc, argv).device_id;
    int N;

    if (get_cards(deviceID, &N) < 0) {
        printf("ERROR %s\n", errmsg());
        return -1;
    }

    printf("\nget-cards\n");
    printf("  ID:    %u\n", deviceID);
    printf("  cards: %d\n", N);
    printf("\n");

    field fields[] = {
        {.field = "ID", .type = "uint32", .value.uint32 = deviceID},
        {.field = "cards", .type = "uint32", .value.uint32 = N},
    };

    display("get-cards", sizeof(fields) / sizeof(field), fields);

    return 0;
}

int getCard(int argc, char **argv) {
    options opts = parse(argc, argv);
    uint32_t deviceID = opts.device_id;
    uint32_t card_number = opts.card;
    card card;

    if (get_card(deviceID, card_number, &card) < 0) {
        printf("ERROR %s\n", errmsg());
        return -1;
    }

    field fields[] = {
        {.field = "ID", .type = "uint32", .value.uint32 = deviceID},
        {.field = "card number", .type = "uint32", .value.uint32 = card.card_number},
        {.field = "     from", .type = "string", .value.string = card.from},
        {.field = "     to", .type = "string", .value.string = card.to},
        {.field = "     door[1]", .type = "uint8", .value.uint8 = card.doors[0]},
        {.field = "     door[2]", .type = "uint8", .value.uint8 = card.doors[1]},
        {.field = "     door[3]", .type = "uint8", .value.uint8 = card.doors[2]},
        {.field = "     door[4]", .type = "uint8", .value.uint8 = card.doors[3]},
        {.field = "     PIN", .type = "uint32", .value.uint32 = card.PIN},
    };

    display("get-card", sizeof(fields) / sizeof(field), fields);

    return 0;
}

int getCardByIndex(int argc, char **argv) {
    options opts = parse(argc, argv);
    uint32_t deviceID = opts.device_id;
    uint32_t index = opts.card_index;
    card card;

    if (get_card_by_index(deviceID, index, &card) < 0) {
        printf("ERROR %s\n", errmsg());
        return -1;
    }

    field fields[] = {
        {.field = "ID", .type = "uint32", .value.uint32 = deviceID},
        {.field = "index", .type = "uint32", .value.uint32 = index},
        {.field = "card number", .type = "uint32", .value.uint32 = card.card_number},
        {.field = "     from", .type = "string", .value.string = card.from},
        {.field = "     to", .type = "string", .value.string = card.to},
        {.field = "     door[1]", .type = "uint8", .value.uint8 = card.doors[0]},
        {.field = "     door[2]", .type = "uint8", .value.uint8 = card.doors[1]},
        {.field = "     door[3]", .type = "uint8", .value.uint8 = card.doors[2]},
        {.field = "     door[4]", .type = "uint8", .value.uint8 = card.doors[3]},
        {.field = "     PIN", .type = "uint32", .value.uint32 = card.PIN},
    };

    display("get-card-by-index", sizeof(fields) / sizeof(field), fields);

    return 0;
}

int putCard(int argc, char **argv) {
    options opts = parse(argc, argv);
    uint32_t deviceID = opts.device_id;
    uint32_t card_number = opts.card;
    char *from = "2022-01-01";
    char *to = "2022-12-31";
    uint8_t doors[4] = {0, 1, 31, 75};
    uint32_t PIN = 7531;

    if (put_card(deviceID, card_number, from, to, doors, PIN) < 0) {
        printf("ERROR %s\n", errmsg());
        return -1;
    }

    field fields[] = {
        {.field = "ID", .type = "uint32", .value.uint32 = deviceID},
        {.field = "card number", .type = "uint32", .value.uint32 = card_number},
        {.field = "     from", .type = "string", .value.string = from},
        {.field = "     to", .type = "string", .value.string = to},
        {.field = "     door[1]", .type = "uint8", .value.uint8 = doors[0]},
        {.field = "     door[2]", .type = "uint8", .value.uint8 = doors[1]},
        {.field = "     door[3]", .type = "uint8", .value.uint8 = doors[2]},
        {.field = "     door[4]", .type = "uint8", .value.uint8 = doors[3]},
        {.field = "     PIN", .type = "uint32", .value.uint32 = PIN},
    };

    display("put-card", sizeof(fields) / sizeof(field), fields);

    return 0;
}

int deleteCard(int argc, char **argv) {
    options opts = parse(argc, argv);
    uint32_t deviceID = opts.device_id;
    uint32_t card_number = opts.card;

    if (delete_card(deviceID, card_number) < 0) {
        printf("ERROR %s\n", errmsg());
        return -1;
    }

    field fields[] = {
        {.field = "ID", .type = "uint32", .value.uint32 = deviceID},
        {.field = "card number", .type = "uint32", .value.uint32 = card_number},
    };

    display("delete-card", sizeof(fields) / sizeof(field), fields);

    return 0;
}

int deleteCards(int argc, char **argv) {
    uint32_t deviceID = parse(argc, argv).device_id;

    if (delete_cards(deviceID) < 0) {
        printf("ERROR %s\n", errmsg());
        return -1;
    }

    field fields[] = {
        {.field = "ID", .type = "uint32", .value.uint32 = deviceID},
    };

    display("delete-cards", sizeof(fields) / sizeof(field), fields);

    return 0;
}
