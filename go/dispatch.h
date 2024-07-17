#pragma once

#include <stdint.h>
#include <stdbool.h>

typedef struct ListenEvent {
    uint32_t  controller;
    char  *timestamp;
    uint32_t index;
    uint8_t event;
    bool granted;
    uint8_t door;
    uint8_t direction;
    uint32_t card;
    uint8_t reason;
} ListenEvent;

typedef void (*onevent) (const struct ListenEvent evt, void *userdata);
typedef void (*onerror) (const char *err);

extern void dispatch_event(onevent f, const struct ListenEvent evt, void * userdata);
extern void dispatch_error(onerror f, const char *err);
