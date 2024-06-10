// Ref. https://pkg.go.dev/cmd/cgo#hdr-C_references_to_Go
// Ref. https://stackoverflow.com/questions/58606884/multiple-definition-when-using-cgo

#include <stdio.h>
#include <dispatch.h>

void dispatch(onevent f, const struct ListenEvent *evt) {
    f(evt);
}
