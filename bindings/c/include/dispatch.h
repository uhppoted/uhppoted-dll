#pragma once

#include <stdbool.h>
#include <stdint.h>

typedef struct ListenEvent {
    uint32_t controller;
    const char *timestamp;
    uint32_t index;
    uint8_t event;
    bool granted;
    uint8_t door;
    uint8_t direction;
    uint32_t card;
    uint8_t reason;
} ListenEvent;

typedef void (*onevent)(const struct ListenEvent *evt);

extern void dispatch(onevent f, const struct ListenEvent *evt);
