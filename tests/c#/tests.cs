using System;
using System.Collections.Generic;
using static System.Console;
using static System.String;

using uhppoted;

public class test {
    public string command;
    public Func<Uhppoted, bool> fn;

    public test(string command, Func<Uhppoted, bool> fn) {
        this.command = command;
        this.fn = fn;
    }
}

// Sadly, mono is only C#7 compatible. Positional records would make this a whole lot less clunky
public class Tests {
    const uint DEVICE_ID = 405419896;
    const uint CARD_NUMBER = 8165538;
    const uint CARD_INDEX = 19;
    const uint EVENT_INDEX = 51;
    const byte DOOR = 4;
    const byte PROFILE_ID = 49;

    static Controller[] controllers = { new Controller(405419896, "192.168.1.100"),
                                        new Controller(303986753, "192.168.1.100") };

    static test[] tests = {
        new test("get-devices", GetDevices),
        new test("get-device", GetDevice),
        new test("set-address", SetAddress),
        new test("get-status", GetStatus),
        new test("get-time", GetTime),
        new test("set-time", SetTime),
        new test("get-listener", GetListener),
        new test("set-listener", SetListener),
        new test("get-door-control", GetDoorControl),
        new test("set-door-control", SetDoorControl),
        new test("open-door", OpenDoor),
        new test("get-cards", GetCards),
        new test("get-card", GetCard),
        new test("get-card-by-index", GetCardByIndex),
        new test("put-card", PutCard),
        new test("delete-card", DeleteCard),
        new test("delete-cards", DeleteCards),
        new test("get-event-index", GetEventIndex),
        new test("set-event-index", SetEventIndex),
        new test("get-event", GetEvent),
        new test("record-special-events", RecordSpecialEvents),
        new test("get-time-profile", GetTimeProfile),
        new test("set-time-profile", SetTimeProfile),
        new test("clear-time-profiles", ClearTimeProfiles),
        new test("add-task", AddTask),
    };

    public static void Main(string[] args) {
        string cmd = "";
        if (args.Length > 0) {
            cmd = args[0];
        }

        try {
            Uhppoted u = new Uhppoted("192.168.1.100", "192.168.1.100:60000", "192.168.1.100:60001", 2500, controllers, true);

            // ... usage
            if (cmd == "help") {
                WriteLine();
                usage();
                return;
            }

            // ... all/default
            if (cmd == "" || cmd == "all") {
                if (!All(u)) {
                    Environment.Exit(-1);
                }

                return;
            }

            // ... named test
            test t = Array.Find(tests, v => v.command == cmd);
            if (t != null) {
                if (!t.fn(u)) {
                    Environment.Exit(-1);
                }

                return;
            }

            // ... invalid command
            WriteLine();
            WriteLine(String.Format("   *** ERROR: invalid command ({0})", cmd));
            WriteLine();

            usage();
            Environment.Exit(-1);

        } catch (Exception e) {
            WriteLine(String.Format("  *** ERROR: {0}", e.Message));
            Environment.Exit(-1);
        }
    }

    static bool All(Uhppoted u) {
        bool ok = true;

        foreach (var t in tests) {
            ok = t.fn(u) ? ok : false;
        }

        return ok;
    }

    static bool GetDevices(Uhppoted u) {
        uint[] devices = u.GetDevices();

        result[] resultset = {
            new uint32Result("device count", 3, (uint)devices.Length),
            new uint32Result("device[0]", 201020304, devices[0]),
            new uint32Result("device[1]", 303986753, devices[1]),
            new uint32Result("device[2]", 405419896, devices[2]),
        };

        return evaluate("get-devices", resultset);
    }

    static bool GetDevice(Uhppoted u) {
        Device device = u.GetDevice(DEVICE_ID);

        result[] resultset = {
            new uint32Result("device ID", 405419896, device.ID),
            new stringResult("IP address", "192.168.1.101", device.address),
            new stringResult("subnet mask", "255.255.255.0", device.subnet),
            new stringResult("gateway address", "192.168.1.1", device.gateway),
            new stringResult("MAC address", "00:12:23:34:45:56", device.MAC),
            new stringResult("version", "v8.92", device.version),
            new stringResult("date", "2018-11-05", device.date),
        };

        return evaluate("get-device", resultset);
    }

    static bool SetAddress(Uhppoted u) {
        u.SetAddress(DEVICE_ID, "192.168.1.125", "255.255.254.0", "192.168.1.0");

        result[] resultset = {};

        return evaluate("set-address", resultset);
    }

    static bool GetStatus(Uhppoted u) {
        Status status = u.GetStatus(DEVICE_ID);

        result[] resultset = {
            new uint32Result("device ID", 405419896, status.ID),
            new stringResult("system date/time", "2022-03-19 15:48:32", status.sysdatetime),
            new boolResult("doors[1]", true, status.doors[0]),
            new boolResult("doors[2]", false, status.doors[1]),
            new boolResult("doors[3]", false, status.doors[2]),
            new boolResult("doors[4]", true, status.doors[3]),
            new boolResult("buttons[1]", true, status.buttons[0]),
            new boolResult("buttons[2]", false, status.buttons[1]),
            new boolResult("buttons[3]", true, status.buttons[2]),
            new boolResult("buttons[4]", false, status.buttons[3]),
            new uint8Result("relays state", 0x12, status.relays),
            new uint8Result("inputs state", 0x34, status.inputs),
            new uint8Result("system error", 0x56, status.syserror),
            new uint8Result("special info", 253, status.info),
            new uint32Result("sequence number", 9876, status.seqno),
            new uint32Result("event index", 135, status.evt.index),
            new stringResult("event timestamp", "2022-01-02 12:34:56", status.evt.timestamp),
            new uint8Result("event type", 6, status.evt.eventType),
            new boolResult("event granted", true, status.evt.granted),
            new uint8Result("event door", 3, status.evt.door),
            new uint8Result("event direction", 1, status.evt.direction),
            new uint32Result("event card", 8100023, status.evt.card),
            new uint8Result("event reason", 21, status.evt.reason),
        };

        return evaluate("get-status", resultset);
    }

    static bool GetTime(Uhppoted u) {
        string datetime = u.GetTime(DEVICE_ID);

        result[] resultset = {
            new stringResult("date/time", "2022-01-02 12:34:56", datetime),
        };

        return evaluate("get-time", resultset);
    }

    static bool SetTime(Uhppoted u) {
        u.SetTime(DEVICE_ID, "2022-03-23 12:24:17");

        result[] resultset = {};

        return evaluate("set-time", resultset);
    }

    static bool GetListener(Uhppoted u) {
        string listener = u.GetListener(DEVICE_ID);

        result[] resultset = {
            new stringResult("event listener address", "192.168.1.100:60001", listener),
        };

        return evaluate("get-listener", resultset);
    }

    static bool SetListener(Uhppoted u) {
        u.SetListener(DEVICE_ID, "192.168.1.100:60001");

        result[] resultset = {};

        return evaluate("set-listener", resultset);
    }

    static bool GetDoorControl(Uhppoted u) {
        DoorControl control = u.GetDoorControl(DEVICE_ID, DOOR);

        result[] resultset = {
            new uint8Result("door control mode", ControlModes.Controlled, control.mode),
            new uint8Result("door open delay", 7, control.delay),
        };

        return evaluate("get-door-control", resultset);
    }

    static bool SetDoorControl(Uhppoted u) {
        u.SetDoorControl(DEVICE_ID, DOOR, ControlModes.NormallyClosed, 6);

        result[] resultset = {};

        return evaluate("set-door-control", resultset);
    }

    static bool OpenDoor(Uhppoted u) {
        u.OpenDoor(DEVICE_ID, DOOR);

        result[] resultset = {};

        return evaluate("open-door", resultset);
    }

    static bool GetCards(Uhppoted u) {
        uint cards = u.GetCards(DEVICE_ID);

        result[] resultset = {
            new uint32Result("card count", 39, cards),
        };

        return evaluate("get-cards", resultset);
    }

    static bool GetCard(Uhppoted u) {
        Card card = u.GetCard(DEVICE_ID, CARD_NUMBER);

        result[] resultset = {
            new uint32Result("card number", 8165538, card.cardNumber),
            new stringResult("'from' date", "2022-01-01", card.from),
            new stringResult("'to' date", "2022-12-31", card.to),
            new uint8Result("doors[1]", 0, card.doors[0]),
            new uint8Result("doors[2]", 1, card.doors[1]),
            new uint8Result("doors[3]", 31, card.doors[2]),
            new uint8Result("doors[4]", 75, card.doors[3]),
        };

        return evaluate("get-card", resultset);
    }

    static bool GetCardByIndex(Uhppoted u) {
        Card card = u.GetCardByIndex(DEVICE_ID, CARD_INDEX);

        result[] resultset = {
            new uint32Result("card number", 8165538, card.cardNumber),
            new stringResult("'from' date", "2022-01-01", card.from),
            new stringResult("'to' date", "2022-12-31", card.to),
            new uint8Result("doors[1]", 0, card.doors[0]),
            new uint8Result("doors[2]", 1, card.doors[1]),
            new uint8Result("doors[3]", 31, card.doors[2]),
            new uint8Result("doors[4]", 75, card.doors[3]),
        };

        return evaluate("get-card-by-index", resultset);
    }

    static bool PutCard(Uhppoted u) {
        byte[] doors = { 0, 1, 31, 75 };

        u.PutCard(DEVICE_ID, CARD_NUMBER, "2022-01-01", "2022-12-31", doors);

        result[] resultset = {};

        return evaluate("put-card", resultset);
    }

    static bool DeleteCard(Uhppoted u) {
        u.DeleteCard(DEVICE_ID, CARD_NUMBER);

        result[] resultset = {};

        return evaluate("delete-card", resultset);
    }

    static bool DeleteCards(Uhppoted u) {
        u.DeleteCards(DEVICE_ID);

        result[] resultset = {};

        return evaluate("delete-cards", resultset);
    }

    static bool GetEventIndex(Uhppoted u) {
        uint index = u.GetEventIndex(DEVICE_ID);

        result[] resultset = {
            new uint32Result("index", 47, index),
        };

        return evaluate("get-event-index", resultset);
    }

    static bool SetEventIndex(Uhppoted u) {
        u.SetEventIndex(DEVICE_ID, EVENT_INDEX);

        result[] resultset = {};

        return evaluate("set-event-index", resultset);
    }

    static bool GetEvent(Uhppoted u) {
        Event evt = u.GetEvent(DEVICE_ID, EVENT_INDEX);

        result[] resultset = {
            new uint32Result("event index", 51, evt.index),
            new stringResult("event timestamp", "2022-04-15 12:29:15", evt.timestamp),
            new uint8Result("event type", 6, evt.eventType),
            new boolResult("event granted", true, evt.granted),
            new uint8Result("event door", 3, evt.door),
            new uint8Result("event direction", 1, evt.direction),
            new uint32Result("event card", 8165538, evt.card),
            new uint8Result("event reason", 21, evt.reason),
        };

        return evaluate("get-event", resultset);
    }

    static bool RecordSpecialEvents(Uhppoted u) {
        u.RecordSpecialEvents(DEVICE_ID, true);

        result[] resultset = {};

        return evaluate("record-special-events", resultset);
    }

    static bool GetTimeProfile(Uhppoted u) {
        TimeProfile profile = u.GetTimeProfile(DEVICE_ID, PROFILE_ID);

        result[] resultset = {
            new uint8Result("profile ID", 49, profile.ID),
            new uint8Result("linked profile ID", 71, profile.linked),
            new stringResult("profile 'from' date", "2022-02-01", profile.from),
            new stringResult("profile 'to' date", "2022-06-30", profile.to),
            new boolResult("profile 'monday'", true, profile.monday),
            new boolResult("profile 'tuesday'", false, profile.tuesday),
            new boolResult("profile 'wednesday'", true, profile.wednesday),
            new boolResult("profile 'thursday'", true, profile.thursday),
            new boolResult("profile 'friday'", false, profile.friday),
            new boolResult("profile 'saturday'", false, profile.saturday),
            new boolResult("profile 'sunday'", true, profile.sunday),
            new stringResult("profile segment 1 start", "08:30", profile.segment1start),
            new stringResult("profile segment 1 end", "11:30", profile.segment1end),
            new stringResult("profile segment 2 start", "00:00", profile.segment2start),
            new stringResult("profile segment 2 end", "00:00", profile.segment2end),
            new stringResult("profile segment 3 start", "00:00", profile.segment3start),
            new stringResult("profile segment 3 end", "18:00", profile.segment3end),
        };

        return evaluate("get-time-profile", resultset);
    }

    static bool SetTimeProfile(Uhppoted u) {
        TimeProfile profile = new TimeProfile(PROFILE_ID, 71, "2022-02-01", "2022-06-30",
                                              true, false, true, true, false, false, true,
                                              "08:30", "11:30",
                                              "", "",
                                              "", "18:00");

        u.SetTimeProfile(DEVICE_ID, profile);

        result[] resultset = {};

        return evaluate("set-time-profile", resultset);
    }

    static bool ClearTimeProfiles(Uhppoted u) {
        u.ClearTimeProfiles(DEVICE_ID);

        result[] resultset = {};

        return evaluate("clear-time-profiles", resultset);
    }

    static bool AddTask(Uhppoted u) {
        Task task = new Task(4, 3, "2022-02-01", "2022-06-30",
                                              true, false, true, true, false, false, true,
                                              "09:45", 11);

        u.AddTask(DEVICE_ID, task);

        result[] resultset = {};

        return evaluate("add-task", resultset);
    }


    static bool evaluate(string tag, result[] resultset) {
        bool ok = true;

        foreach (var r in resultset) {
            switch (r) {
            case uint32Result v:
                if (v.expected != v.value) {
                    WriteLine(Format("{0, -21} incorrect {1} (expected:{2}, got:{3})", tag, v.field, v.expected, v.value));
                    ok = false;
                }
                break;

            case uint8Result v:
                if (v.expected != v.value) {
                    WriteLine(Format("{0, -21} incorrect {1} (expected:{2}, got:{3})", tag, v.field, v.expected, v.value));
                    ok = false;
                }
                break;

            case boolResult v:
                if (v.expected != v.value) {
                    WriteLine(Format("{0, -21} incorrect {1} (expected:{2}, got:{3})", tag, v.field, v.expected, v.value));
                    ok = false;
                }
                break;

            case stringResult v:
                if (v.expected != v.value) {
                    WriteLine(Format("{0, -21} incorrect {1} (expected:{2}, got:{3})", tag, v.field, v.expected, v.value));
                    ok = false;
                }
                break;

            default:
                throw new Exception(Format("unsupported type {0}", r));
            }
        }

        if (!ok) {
            return failed(tag);
        }

        return passed(tag);
    }

    static bool passed(string tag) {
        WriteLine(Format("{0, -21} {1}", tag, "ok"));

        return true;
    }

    static bool failed(string tag) {
        WriteLine(Format("{0, -21} {1}", tag, "failed"));

        return false;
    }

    static void usage() {
        WriteLine("   Usage: test <command>");
        WriteLine();
        WriteLine("   Supported commands:");

        foreach (var t in tests) {
            WriteLine(Format("      {0}", t.command));
        }

        WriteLine();
        WriteLine("   Defaults to 'all'");
        WriteLine();
    }
}

// *** Utility classes for test evaluation ***

public class result {}

public class uint32Result : result {
    public string field { get; }
    public uint expected { get; }
    public uint value { get; }

    public uint32Result(string field, uint expected, uint value) {
        this.field = field;
        this.expected = expected;
        this.value = value;
    }
}

public class uint8Result : result {
    public string field { get; }
    public byte expected { get; }
    public byte value { get; }

    public uint8Result(string field, byte expected, byte value) {
        this.field = field;
        this.expected = expected;
        this.value = value;
    }
}

public class boolResult : result {
    public string field { get; }
    public bool expected { get; }
    public bool value { get; }

    public boolResult(string field, bool expected, bool value) {
        this.field = field;
        this.expected = expected;
        this.value = value;
    }
}

public class stringResult : result {
    public string field { get; }
    public string expected { get; }
    public string value { get; }

    public stringResult(string field, string expected, string value) {
        this.field = field;
        this.expected = expected;
        this.value = value;
    }
}
