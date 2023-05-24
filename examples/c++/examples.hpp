#pragma once

#include <any>
#include <tuple>
#include <vector>

#include "../include/uhppoted.hpp"

extern const std::string LOCALE;

typedef std::tuple<std::string, std::any> field;

typedef struct options {
    uint32_t device_id;
    std::string ip_address;
    std::string subnet_mask;
    std::string gateway;
    std::string listener;
    uint32_t card;
    uint32_t card_index;
    uint8_t door;
    uint32_t event_index;
    uint8_t time_profile_id;
} options;


void getDevices(uhppoted &, int argc, char **argv);
void getDevice(uhppoted &u, int argc, char **argv);
void setAddress(uhppoted &, int argc, char **argv);
void getStatus(uhppoted &, int argc, char **argv);
void getTime(uhppoted &, int argc, char **argv);
void setTime(uhppoted &u, int argc, char **argv);
void getListener(uhppoted &, int argc, char **argv);
void setListener(uhppoted &, int argc, char **argv);
void getDoorControl(uhppoted &, int argc, char **argv);
void setDoorControl(uhppoted &, int argc, char **argv);
void openDoor(uhppoted &, int argc, char **argv);

void getCards(uhppoted &u, int argc, char **argv);
void getCard(uhppoted &u, int argc, char **argv);
void getCardByIndex(uhppoted &u, int argc, char **argv);
void putCard(uhppoted &u, int argc, char **argv);
void deleteCard(uhppoted &u, int argc, char **argv);
void deleteCards(uhppoted &u, int argc, char **argv);

void getEventIndex(uhppoted &u, int argc, char **argv);
void setEventIndex(uhppoted &u, int argc, char **argv);
void getEvent(uhppoted &u, int argc, char **argv);
void recordSpecialEvents(uhppoted &u, int argc, char **argv);

void getTimeProfile(uhppoted &u, int argc, char **argv);
void setTimeProfile(uhppoted &u, int argc, char **argv);
void clearTimeProfiles(uhppoted &u, int argc, char **argv);

void addTask(uhppoted &u, int argc, char **argv);
void refreshTaskList(uhppoted &u, int argc, char **argv);
void clearTaskList(uhppoted &u, int argc, char **argv);

void setPCControl(uhppoted &u, int argc, char **argv);
void setInterlock(uhppoted &u, int argc, char **argv);

options parse(int argc, char **argv);
void display(const std::string &, const std::vector<field> &);
