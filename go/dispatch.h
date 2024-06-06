#pragma once

typedef void (*yadda) (unsigned int index);

extern void dispatch(yadda f, unsigned int index);
