/* Code generated by cmd/cgo; DO NOT EDIT. */

/* package github.com/uhppoted/uhppoted-dll/go */


#line 1 "cgo-builtin-export-prolog"

#include <stddef.h>

#ifndef GO_CGO_EXPORT_PROLOGUE_H
#define GO_CGO_EXPORT_PROLOGUE_H

#ifndef GO_CGO_GOSTRING_TYPEDEF
typedef struct { const char *p; ptrdiff_t n; } _GoString_;
#endif

#endif

/* Start of preamble from import "C" comments.  */


#line 3 "main.go"

#include <stdlib.h>
#include <stdbool.h>
#include <stdint.h>
#include <string.h>

#include "dispatch.h"

typedef const char cchar_t;

typedef struct udevice {
	uint32_t    id;
	const char *address;
	const char *transport;
} udevice;

typedef struct udevices {
	uint32_t  N;        // number of devicess
	udevice  *devices;  // array non-local devices
} udevices;

typedef struct UHPPOTE {
	const char *bind;
	const char *broadcast;
	const char *listen;
	int         timeout;  // milliseconds
	udevices   *devices;  // (optional) list of non-local devices
	bool        debug;
} UHPPOTE;

typedef struct Device {
    uint32_t ID;
	const char *address;  // expects at least char[16]
	const char *subnet;   // expects at least char[16]
	const char *gateway;  // expects at least char[16]
	const char *MAC;      // expects at least char[18]
	const char *version;  // expects at least char[7]
	const char *date;     // expects at least char[11]
} Device;

typedef struct Event {
	const char *timestamp; // expects at least char[20]
    uint32_t index;
	uint8_t eventType;
	uint8_t granted;
	uint8_t door;
	uint8_t direction;
	uint32_t card;
	uint8_t reason;
} Event;

typedef struct Status {
    uint32_t ID;
	const char *sysdatetime; // expects at least char[20]
	uint8_t  *doors;         // expects uint_8[4]
	uint8_t  *buttons;       // expects uint_8[4]
	uint8_t relays;
	uint8_t inputs;
	uint8_t syserror;
	uint8_t info;
	uint32_t seqno;
	Event *event;
} Status;

typedef struct DoorControl {
    uint8_t mode;
    uint8_t delay;
} DoorControl;

typedef struct Card {
    uint32_t card_number;
    char* from;
    char* to;
	uint8_t *doors; // uint_8[4]
    uint32_t PIN;
} Card;

typedef struct TimeProfile {
    uint8_t ID;
    uint8_t linked;
    char *from;
    char *to;
    uint8_t monday;
    uint8_t tuesday;
    uint8_t wednesday;
    uint8_t thursday;
    uint8_t friday;
    uint8_t saturday;
    uint8_t sunday;
    char * segment1start;
    char * segment1end;
    char * segment2start;
    char * segment2end;
    char * segment3start;
    char * segment3end;
} TimeProfile;

typedef struct Task {
	uint8_t task;
	uint8_t door;
	const char *from;
	const char *to;
    uint8_t monday;
    uint8_t tuesday;
    uint8_t wednesday;
    uint8_t thursday;
    uint8_t friday;
    uint8_t saturday;
    uint8_t sunday;
	const char *at;
	uint8_t cards;
} Task;


#line 1 "cgo-generated-wrapper"


/* End of preamble from import "C" comments.  */


/* Start of boilerplate cgo prologue.  */
#line 1 "cgo-gcc-export-header-prolog"

#ifndef GO_CGO_PROLOGUE_H
#define GO_CGO_PROLOGUE_H

typedef signed char GoInt8;
typedef unsigned char GoUint8;
typedef short GoInt16;
typedef unsigned short GoUint16;
typedef int GoInt32;
typedef unsigned int GoUint32;
typedef long long GoInt64;
typedef unsigned long long GoUint64;
typedef GoInt64 GoInt;
typedef GoUint64 GoUint;
typedef size_t GoUintptr;
typedef float GoFloat32;
typedef double GoFloat64;
#ifdef _MSC_VER
#include <complex.h>
typedef _Fcomplex GoComplex64;
typedef _Dcomplex GoComplex128;
#else
typedef float _Complex GoComplex64;
typedef double _Complex GoComplex128;
#endif

/*
  static assertion to make sure the file is being used on architecture
  at least with matching size of GoInt.
*/
typedef char _check_for_64_bit_pointer_matching_GoInt[sizeof(void*)==64/8 ? 1:-1];

#ifndef GO_CGO_GOSTRING_TYPEDEF
typedef _GoString_ GoString;
#endif
typedef void *GoMap;
typedef void *GoChan;
typedef struct { void *t; void *v; } GoInterface;
typedef struct { void *data; GoInt len; GoInt cap; } GoSlice;

#endif

/* End of boilerplate cgo prologue.  */

#ifdef __cplusplus
extern "C" {
#endif

extern int GetDevices(struct UHPPOTE* u, unsigned int* list, int* N, cchar_t* errmsg, int* errN);
extern int GetDevice(struct UHPPOTE* u, struct Device* device, GoUint32 deviceID, cchar_t* errmsg, int* errN);
extern int SetAddress(struct UHPPOTE* u, GoUint32 deviceID, cchar_t* addr, cchar_t* subnet, cchar_t* gateway, cchar_t* errmsg, int* errN);
extern int GetStatus(struct UHPPOTE* u, struct Status* status, GoUint32 deviceID, cchar_t* errmsg, int* errN);
extern int GetTime(struct UHPPOTE* u, cchar_t* datetime, GoUint32 deviceID, cchar_t* errmsg, int* errN);
extern int SetTime(struct UHPPOTE* u, GoUint32 deviceID, cchar_t* datetime, cchar_t* errmsg, int* errN);
extern int GetListener(struct UHPPOTE* u, cchar_t* address, GoUint32 deviceID, cchar_t* errmsg, int* errN);
extern int SetListener(struct UHPPOTE* u, GoUint32 deviceID, cchar_t* listener, cchar_t* errmsg, int* errN);
extern char* GetDoorControl(struct UHPPOTE* u, struct DoorControl* control, GoUint32 deviceID, GoUint8 door);
extern char* SetDoorControl(struct UHPPOTE* u, GoUint32 deviceID, GoUint8 door, GoUint8 mode, GoUint8 delay);
extern char* OpenDoor(struct UHPPOTE* u, GoUint32 deviceID, GoUint8 door);
extern char* GetCards(struct UHPPOTE* u, int* N, GoUint32 deviceID);
extern char* GetCard(struct UHPPOTE* u, struct Card* card, GoUint32 deviceID, GoUint32 cardNumber);
extern char* GetCardByIndex(struct UHPPOTE* u, struct Card* card, GoUint32 deviceID, GoUint32 index);
extern char* PutCard(struct UHPPOTE* u, GoUint32 deviceID, GoUint32 cardNumber, char* from, char* to, GoUint8* doors, GoUint32 PIN);
extern char* DeleteCard(struct UHPPOTE* u, GoUint32 deviceID, GoUint32 cardNumber);
extern char* DeleteCards(struct UHPPOTE* u, GoUint32 deviceID);
extern char* GetEventIndex(struct UHPPOTE* u, GoUint32* index, GoUint32 deviceID);
extern char* SetEventIndex(struct UHPPOTE* u, GoUint32 deviceID, GoUint32 index);
extern char* GetEvent(struct UHPPOTE* u, Event* event, GoUint32 deviceID, GoUint32 index);
extern char* RecordSpecialEvents(struct UHPPOTE* u, GoUint32 deviceID, GoUint8 enabled);
extern char* GetTimeProfile(struct UHPPOTE* u, struct TimeProfile* profile, GoUint32 deviceID, GoUint8 profileID);
extern char* SetTimeProfile(struct UHPPOTE* u, GoUint32 deviceID, struct TimeProfile* profile);
extern char* ClearTimeProfiles(struct UHPPOTE* u, GoUint32 deviceID);
extern char* AddTask(struct UHPPOTE* u, GoUint32 deviceID, struct Task* task);
extern char* RefreshTaskList(struct UHPPOTE* u, GoUint32 deviceID);
extern char* ClearTaskList(struct UHPPOTE* u, GoUint32 deviceID);
extern char* SetPCControl(struct UHPPOTE* u, GoUint32 controller, GoUint8 enabled);
extern char* SetInterlock(struct UHPPOTE* u, GoUint32 controller, GoUint8 interlock);
extern char* ActivateKeypads(struct UHPPOTE* u, GoUint32 controller, GoUint8 reader1, GoUint8 reader2, GoUint8 reader3, GoUint8 reader4);

// Sets the supervisor passcodes for a door managed by the controller.
//
// Valid passcodes are in the range [1..999999] or 0 (no code) - invalid passcodes will be replaced by
// a 0 (no code).
//
extern char* SetDoorPasscodes(struct UHPPOTE* u, GoUint32 controller, GoUint8 door, GoUint32 passcode1, GoUint32 passcode2, GoUint32 passcode3, GoUint32 passcode4);

// Resets a controller to the manufacturer default configuration.
//
extern char* RestoreDefaultParameters(struct UHPPOTE* u, GoUint32 controller);

// Listens for events and invokes a callback function.
//
extern GoInt32 Listen(struct UHPPOTE* u, onevent f, GoUint8* listening, GoUint8* stop, onerror g, void* userdata);

#ifdef __cplusplus
}
#endif
