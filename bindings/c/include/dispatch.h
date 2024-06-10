#pragma once

#include <stdint.h>

typedef void (*onevent)(
    uint32_t controller,
    uint32_t index,
    uint32_t date,
    uint32_t time,
    uint8_t event,
    uint32_t card,
    uint8_t door,
    uint8_t granted,
    uint8_t direction,
    uint8_t reason);

extern void dispatch(
    onevent f,
    uint32_t controller,
    uint32_t index,
    uint32_t date,
    uint32_t time,
    uint8_t event,
    uint32_t card,
    uint8_t door,
    uint8_t granted,
    uint8_t direction,
    uint8_t reason);
