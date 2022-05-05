#pragma once

#include <any>
#include <tuple>
#include <vector>

#include "../include/uhppoted.hpp"

typedef std::tuple<std::string, std::any> field;

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

void display(const std::string &, const std::vector<field> &);
