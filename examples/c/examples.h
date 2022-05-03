#pragma once

#include <stdbool.h>

typedef struct uint8 {
            uint8_t expected;
            uint8_t value;
        } uint8;

typedef struct uint32 {
            uint32_t expected;
            uint32_t value;
        } uint32;

typedef struct boolean {
            bool expected;
            bool value;
        } boolean;

typedef struct string {
            const char * expected;
            const char * value;
        } string;


typedef struct field {
    const char *field;
    const char *type;
    union {
        uint8_t uint8;
        uint32_t uint32;
        bool  boolean;
        char * string;
    } value;
} field;


extern int getDevices(int argc, char **argv);
extern int getDevice(int argc, char **argv);
extern int setAddress(int argc, char **argv);
extern int getStatus(int argc, char **argv);
extern int getTime(int argc, char **argv);
extern int setTime(int argc, char **argv);
extern int getListener(int argc, char **argv);
extern int setListener(int argc, char **argv);
extern int getDoorControl(int argc, char **argv);
extern int setDoorControl(int argc, char **argv);
extern int openDoor(int argc, char **argv);

extern int getCards(int argc, char **argv);
extern int getCard(int argc, char **argv);
extern int getCardByIndex(int argc, char **argv);
extern int putCard(int argc, char **argv);
extern int deleteCard(int argc, char **argv);
extern int deleteCards(int argc, char **argv);

extern int getEventIndex(int argc, char **argv);
extern int setEventIndex(int argc, char **argv);
extern int getEvent(int argc, char **argv);
extern int recordSpecialEvents(int argc, char **argv);

extern int getTimeProfile(int argc, char **argv);
extern int setTimeProfile(int argc, char **argv);
extern int clearTimeProfiles(int argc, char **argv);

extern int addTask(int argc, char **argv);
extern int refreshTaskList(int argc, char **argv);
extern int clearTaskList(int argc, char **argv);

extern void display(const char *tag, int N, field fields[]);

