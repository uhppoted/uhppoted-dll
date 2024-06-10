// Ref. https://pkg.go.dev/cmd/cgo#hdr-C_references_to_Go
// Ref. https://stackoverflow.com/questions/58606884/multiple-definition-when-using-cgo

#include <stdio.h>
#include <dispatch.h>

void dispatch(onevent f,
    uint32_t  controller, 
    uint32_t index,
    const char *timestamp, 
    uint8_t event,
    uint32_t card,
    uint8_t door,
    uint8_t granted,
    uint8_t direction,
    uint8_t reason) {
    f(controller, index, timestamp, event, card, door, granted, direction, reason);
}
