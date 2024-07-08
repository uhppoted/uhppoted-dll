// Ref. https://pkg.go.dev/cmd/cgo#hdr-C_references_to_Go
// Ref. https://stackoverflow.com/questions/58606884/multiple-definition-when-using-cgo

#include <stdio.h>
#include <dispatch.h>

// void dispatch_event(onevent f, const struct ListenEvent *evt) {
//     if (f != NULL) {
//         f(evt);
//         f();
//     }
// }

void dispatch_event(onevent f, uint32_t controller, uint32_t index,const char *timestamp) {
    if (f != NULL) {
        f(controller, index,timestamp);
    }
}

void dispatch_error(onerror f, const char *err) {
    if (f != NULL) {
        f(err);        
    }
}
