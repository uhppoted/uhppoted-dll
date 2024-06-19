using System;
using System.Threading;

using static System.Console;
using static System.String;

using uhppoted;

public class command {
    public string cmd;
    public string help;
    public Action<Uhppoted, string[]> fn;

    public command(string cmd, string help) {
        this.cmd = cmd;
        this.help = help;
    }

    public command(string cmd, string help, Action<Uhppoted, string[]> fn) {
        this.cmd = cmd;
        this.help = help;
        this.fn = fn;
    }
};

public class options {
    public uint deviceID;
    public string ipAddress;
    public string subnetMask;
    public string gateway;
    public string listener;
    public uint card;
    public uint cardIndex;
    public byte door;
    public uint eventIndex;
    public byte timeProfileID;

    public options(uint deviceID, string ipAddress, string subnetMask, string gateway, string listener, uint card, uint cardIndex, byte door, uint eventIndex, byte timeProfileID) {
        this.deviceID = deviceID;
        this.ipAddress = ipAddress;
        this.subnetMask = subnetMask;
        this.gateway = gateway;
        this.listener = listener;
        this.card = card;
        this.cardIndex = cardIndex;
        this.door = door;
        this.eventIndex = eventIndex;
        this.timeProfileID = timeProfileID;
    }
};

public class examples {
    const uint DEVICE_ID = 405419896;
    const uint CARD_NUMBER = 8000001;
    const uint CARD_INDEX = 7;
    const uint EVENT_INDEX = 43;
    const byte DOOR = 4;
    const byte PROFILE_ID = 29;
    const uint PIN = 7531;
    const string locale = "";

    static command[] commands = {
        new command("get-devices",
                    "Retrieves a list of UHPPOTE controller IDs findable on the local LAN.",
                    GetDevices),
        new command("get-device",
                    "Retrieves the basic device information for a single UHPPOTE controller.",
                    GetDevice),
        new command("set-address",
                    "Sets the controller IPv4 address, subnet mask and gateway address.",
                    SetAddress),
        new command("get-status",
                    "Retrieves a controller status.",
                    GetStatus),
        new command("get-time",
                    "Retrieves a controller current date/time (YYYY-MM-DD HH:mm:ss).",
                    GetTime),
        new command("set-time",
                    "Sets a controller current date/time (YYYY-MM-DD HH:mm:ss).",
                    SetTime),
        new command("get-listener",
                    "Retrieves a controller's configured event listener address.",
                    GetListener),
        new command("set-listener",
                    "Configures a controller's event listener address and port.",
                    SetListener),
        new command("get-door-control",
                    "Retrieves the control state and open delay for a controller door.",
                    GetDoorControl),
        new command("set-door-control",
                    "Sets the control mode and delay for a controller door.",
                    SetDoorControl),
        new command("open-door",
                    "Remotely opens a controller door.",
                    OpenDoor),
        new command("get-cards",
                    "Retrieves the number of cards stored on a controller.",
                    GetCards),
        new command("get-card",
                    "Retrieves the card detail for card number from a controller.",
                    GetCard),
        new command("get-card-by-index",
                    "Retrieves the card detail for the card stored at an index on a controller.",
                    GetCardByIndex),
        new command("put-card",
                    "Adds or updates the card detail for card number stored on a controller.",
                    PutCard),
        new command("delete-card",
                    "Deletes a card from a controller.",
                    DeleteCard),
        new command("delete-cards",
                    "Deletes all cards from a controller.",
                    DeleteCards),
        new command("get-event-index",
                    "Retrieves the current event index from a controller.",
                    GetEventIndex),
        new command("set-event-index",
                    "Sets the current event index on a controller.",
                    SetEventIndex),
        new command("get-event",
                    "Retrieves the event at the index from a controller.",
                    GetEvent),
        new command("record-special-events",
                    "Enables/disables recording additional events for a controller.",
                    RecordSpecialEvents),
        new command("get-time-profile",
                    "Retrieves a time profile from a controller.",
                    GetTimeProfile),
        new command("set-time-profile",
                    "Adds or updates a time profile on a controller.",
                    SetTimeProfile),
        new command("clear-time-profiles",
                    "Deletes all time profiles from a controller.",
                    ClearTimeProfiles),
        new command("add-task",
                    "Adds a scheduled task to a controller.",
                    AddTask),
        new command("refresh-tasklist",
                    "Refreshes a controller task list to activate added tasks.",
                    RefreshTaskList),
        new command("clear-tasklist",
                    "Clears a controller task list.",
                    ClearTaskList),
        new command("set-pc-control",
                    "Enables/disables controller remote access control.",
                    SetPCControl),
        new command("set-interlock",
                    "Set a controller interlock mode.",
                    SetInterlock),
        new command("activate-keypads",
                    "Activates and deactivates a controller reader access keypads.",
                    ActivateKeypads),
        new command("set-door-passcodes",
                    "Sets the supervisor passcodes for keypad only access to a door.",
                    SetDoorPasscodes),
        new command("restore-default-parameters",
                    "Resets a controller to the manufacturer default configuration.",
                    RestoreDefaultParameters),
        new command("listen",
                    "Listens for access events.",
                    Listen),
    };

    static Controller[] controllers = { new Controller(405419896, "192.168.1.100"),
                                        new Controller(303986753, "192.168.1.100") };

    public static void Main(string[] args) {
        if (args.Length < 1) {
            usage();
            return;
        }

        string cmd = args[0];

        try {
            if (cmd == "help") {
                help();
                return;
            }

            foreach (command c in commands) {
                if (c.cmd == cmd) {
                    Uhppoted u = new Uhppoted("0.0.0.0", "255.255.255.255", "0.0.0.0:60001", 2500, controllers, true);

                    c.fn(u, args);
                    return;
                }
            }

            WriteLine();
            WriteLine(Format("  *** ERROR: invalid command ({0})", cmd));
            usage();

        } catch (Exception e) {
            WriteLine(Format("  *** ERROR: {0}", e.Message));
        }
    }

    static void usage() {
        WriteLine();
        WriteLine("  Usage: mono example.exe <command>");
        WriteLine();
        WriteLine("  Supported commands");

        foreach (command c in commands) {
            WriteLine("    {0}", c.cmd);
        }

        WriteLine();
    }

    static void help() {
        WriteLine();
        WriteLine("  Usage: mono example.exe <command>");
        WriteLine();
        WriteLine("  Commands");

        foreach (command c in commands) {
            WriteLine("    {0,-17}  {1}", c.cmd, c.help);
        }

        WriteLine();
    }

    static void GetDevices(Uhppoted u, string[] args) {
        uint[] list = u.GetDevices();

        string tag = Format("get-devices ({0})", list.Length);
        field[] fields = new field[list.Length];

        for (int i = 0; i < list.Length; i++) {
            fields[i] = new uint32Field("", list[i]);
        }

        display("get-devices", fields);
    }

    static void GetDevice(Uhppoted u, string[] args) {
        options opts = parse(args);
        uint deviceID = opts.deviceID;

        Device device = u.GetDevice(deviceID);

        field[] fields = {
            new uint32Field("ID", deviceID),
            new stringField("address", device.address),
            new stringField("subnet mask", device.subnet),
            new stringField("gateway address", device.gateway),
            new stringField("MAC", device.MAC),
            new stringField("version", device.version),
            new stringField("released", device.date),
        };

        display("get-device", fields);
    }

    static void SetAddress(Uhppoted u, string[] args) {
        options opts = parse(args);
        uint deviceID = opts.deviceID;
        string address = opts.ipAddress;
        string subnet = opts.subnetMask;
        string gateway = opts.gateway;

        u.SetAddress(deviceID, address, subnet, gateway);

        field[] fields = {
            new uint32Field("ID", deviceID),
            new stringField("address", address),
            new stringField("subnet", subnet),
            new stringField("gateway", gateway),
        };

        display("set-address", fields);
    }

    static void GetStatus(Uhppoted u, string[] args) {
        options opts = parse(args);
        uint deviceID = opts.deviceID;

        Status status = u.GetStatus(deviceID);
        string timestamp = status.evt.timestamp;

        if (timestamp == "") {
            timestamp = "-";
        }

        field[] fields = {
            new uint32Field("ID", status.ID),
            new stringField("timestamp", status.sysdatetime),
            new boolField("doors[1]", status.doors[0]),
            new boolField("doors[2]", status.doors[1]),
            new boolField("doors[3]", status.doors[2]),
            new boolField("doors[4]", status.doors[3]),
            new boolField("buttons[1]", status.buttons[0]),
            new boolField("buttons[2]", status.buttons[1]),
            new boolField("buttons[3]", status.buttons[2]),
            new boolField("buttons[4]", status.buttons[3]),
            new uint8Field("relays", status.relays),
            new uint8Field("inputs", status.inputs),
            new uint8Field("syserror", status.syserror),
            new uint8Field("info", status.info),
            new uint32Field("seqno", status.seqno),
            new stringField("event timestamp", timestamp),
            new uint32Field("      index", status.evt.index),
            new stringField("      type", lookup.find(lookup.LOOKUP_EVENT_TYPE, status.evt.eventType, locale)),
            new boolField("      granted", status.evt.granted),
            new uint8Field("      door", status.evt.door),
            new stringField("      direction", lookup.find(lookup.LOOKUP_DIRECTION, status.evt.direction, locale)),
            new uint32Field("      card", status.evt.card),
            new stringField("      reason", lookup.find(lookup.LOOKUP_EVENT_REASON, status.evt.reason, locale)),
        };

        display("get-status", fields);
    }

    static void GetTime(Uhppoted u, string[] args) {
        options opts = parse(args);
        uint deviceID = opts.deviceID;

        string datetime = u.GetTime(deviceID);

        field[] fields = {
            new uint32Field("ID", deviceID),
            new stringField("date/time", datetime),
        };

        display("get-time", fields);
    }

    static void SetTime(Uhppoted u, string[] args) {
        options opts = parse(args);
        uint deviceID = opts.deviceID;
        string datetime = DateTime.Now.ToString("yyyy-MM-dd HH:mm:ss");

        u.SetTime(deviceID, datetime);

        field[] fields = {
            new uint32Field("ID", deviceID),
            new stringField("date/time", datetime),
        };

        display("set-time", fields);
    }

    static void GetListener(Uhppoted u, string[] args) {
        options opts = parse(args);
        uint deviceID = opts.deviceID;

        string listener = u.GetListener(deviceID);

        field[] fields = {
            new uint32Field("ID", deviceID),
            new stringField("event listener", listener),
        };

        display("get-listener", fields);
    }

    static void SetListener(Uhppoted u, string[] args) {
        options opts = parse(args);
        uint deviceID = opts.deviceID;
        string listener = opts.listener;

        u.SetListener(deviceID, listener);

        field[] fields = {
            new uint32Field("ID", deviceID),
            new stringField("event listener", listener),
        };

        display("set-listener", fields);
    }

    static void GetDoorControl(Uhppoted u, string[] args) {
        options opts = parse(args);
        uint deviceID = opts.deviceID;
        byte door = opts.door;

        DoorControl control = u.GetDoorControl(deviceID, door);

        field[] fields = {
            new uint32Field("ID", deviceID),
            new uint8Field("door", door),
            new stringField("mode", lookup.find(lookup.LOOKUP_MODE, control.mode, locale)),
            new uint8Field("delay", control.delay),
        };

        display("get-door-control", fields);
    }

    static void SetDoorControl(Uhppoted u, string[] args) {
        options opts = parse(args);
        uint deviceID = opts.deviceID;
        byte door = opts.door;
        byte mode = DoorMode.NormallyOpen;
        byte delay = 9;

        u.SetDoorControl(deviceID, door, mode, delay);

        field[] fields = {
            new uint32Field("ID", deviceID),
            new uint8Field("door", door),
            new stringField("mode", lookup.find(lookup.LOOKUP_MODE, mode, locale)),
            new uint8Field("delay", delay),
        };

        display("set-door-control", fields);
    }

    static void OpenDoor(Uhppoted u, string[] args) {
        options opts = parse(args);
        uint deviceID = opts.deviceID;
        byte door = opts.door;

        u.OpenDoor(deviceID, door);

        field[] fields = {
            new uint32Field("ID", deviceID),
            new uint8Field("door", door),
        };

        display("open-door", fields);
    }

    static void GetCards(Uhppoted u, string[] args) {
        options opts = parse(args);
        uint deviceID = opts.deviceID;

        uint cards = u.GetCards(deviceID);

        field[] fields = {
            new uint32Field("ID", deviceID),
            new uint32Field("cards", cards),
        };

        display("get-cards", fields);
    }

    static void GetCard(Uhppoted u, string[] args) {
        options opts = parse(args);
        uint deviceID = opts.deviceID;
        uint cardNumber = opts.card;
        Card card = u.GetCard(deviceID, cardNumber);

        field[] fields = {
            new uint32Field("ID", deviceID),
            new uint32Field("card number", card.cardNumber),
            new stringField("     from", card.from),
            new stringField("     to", card.to),
            new uint8Field("     door[1]", card.doors[0]),
            new uint8Field("     door[2]", card.doors[1]),
            new uint8Field("     door[3]", card.doors[2]),
            new uint8Field("     door[4]", card.doors[3]),
            new uint32Field("     PIN", card.PIN),
        };

        display("get-card", fields);
    }

    static void GetCardByIndex(Uhppoted u, string[] args) {
        options opts = parse(args);
        uint deviceID = opts.deviceID;
        uint index = opts.cardIndex;

        Card card = u.GetCardByIndex(deviceID, index);

        field[] fields = {
            new uint32Field("ID", deviceID),
            new uint32Field("index", index),
            new uint32Field("card number", card.cardNumber),
            new stringField("     from", card.from),
            new stringField("     to", card.to),
            new uint8Field("     door[1]", card.doors[0]),
            new uint8Field("     door[2]", card.doors[1]),
            new uint8Field("     door[3]", card.doors[2]),
            new uint8Field("     door[4]", card.doors[3]),
            new uint32Field("     PIN", card.PIN),
        };

        display("get-card-by-index", fields);
    }

    static void PutCard(Uhppoted u, string[] args) {
        options opts = parse(args);
        uint deviceID = opts.deviceID;
        uint cardNumber = opts.card;
        string from = "2023-01-01";
        string to = "2023-12-31";
        byte[] doors = { 0, 1, 31, 75 };
        uint PIN = 7531;

        u.PutCard(deviceID, cardNumber, from, to, doors, PIN);

        field[] fields = {
            new uint32Field("ID", deviceID),
            new uint32Field("card number", cardNumber),
            new stringField("     from", from),
            new stringField("     to", to),
            new uint8Field("     door[1]", doors[0]),
            new uint8Field("     door[2]", doors[1]),
            new uint8Field("     door[3]", doors[2]),
            new uint8Field("     door[4]", doors[3]),
            new uint32Field("     PIN", PIN),
        };

        display("put-card", fields);
    }

    static void DeleteCard(Uhppoted u, string[] args) {
        options opts = parse(args);
        uint deviceID = opts.deviceID;
        uint cardNumber = opts.card;

        u.DeleteCard(deviceID, cardNumber);

        field[] fields = {
            new uint32Field("ID", deviceID),
            new uint32Field("card number", cardNumber),
        };

        display("delete-card", fields);
    }

    static void DeleteCards(Uhppoted u, string[] args) {
        options opts = parse(args);
        uint deviceID = opts.deviceID;

        u.DeleteCards(deviceID);

        field[] fields = {
            new uint32Field("ID", deviceID),
        };

        display("delete-cards", fields);
    }

    static void GetEventIndex(Uhppoted u, string[] args) {
        options opts = parse(args);
        uint deviceID = opts.deviceID;

        uint index = u.GetEventIndex(deviceID);

        field[] fields = {
            new uint32Field("ID", deviceID),
            new uint32Field("index", index),
        };

        display("get-event-index", fields);
    }

    static void SetEventIndex(Uhppoted u, string[] args) {
        options opts = parse(args);
        uint deviceID = opts.deviceID;
        uint index = opts.eventIndex;

        u.SetEventIndex(deviceID, index);

        field[] fields = {
            new uint32Field("ID", deviceID),
            new uint32Field("index", index),
        };

        display("set-event-index", fields);
    }

    static void GetEvent(Uhppoted u, string[] args) {
        options opts = parse(args);
        uint deviceID = opts.deviceID;
        uint index = opts.eventIndex;

        Event evt = u.GetEvent(deviceID, index);

        field[] fields = {
            new uint32Field("ID", deviceID),
            new uint32Field("event index", evt.index),
            new stringField("      timestamp", evt.timestamp),
            new stringField("      type", lookup.find(lookup.LOOKUP_EVENT_TYPE, evt.eventType, locale)),
            new boolField("      granted", evt.granted),
            new uint8Field("      door", evt.door),
            new stringField("      direction", lookup.find(lookup.LOOKUP_DIRECTION, evt.direction, locale)),
            new uint32Field("      card number", evt.card),
            new stringField("      reason", lookup.find(lookup.LOOKUP_EVENT_REASON, evt.reason, locale)),
        };

        display("get-event", fields);
    }

    static void RecordSpecialEvents(Uhppoted u, string[] args) {
        options opts = parse(args);
        uint deviceID = opts.deviceID;
        bool enabled = true;

        u.RecordSpecialEvents(deviceID, enabled);

        field[] fields = {
            new uint32Field("ID", deviceID),
            new boolField("enabled", enabled),
        };

        display("record-special-events", fields);
    }

    static void GetTimeProfile(Uhppoted u, string[] args) {
        options opts = parse(args);
        uint deviceID = opts.deviceID;
        byte profileID = opts.timeProfileID;

        TimeProfile profile = u.GetTimeProfile(deviceID, profileID);

        field[] fields = {
            new uint32Field("ID", deviceID),
            new uint8Field("profile ID", profile.ID),
            new uint8Field("linked profile", profile.linked),
            new stringField("enabled from", profile.from),
            new stringField("        to", profile.to),
            new boolField("enabled on Monday", profile.monday),
            new boolField("           Tuesday", profile.tuesday),
            new boolField("           Wednesday", profile.wednesday),
            new boolField("           Thursday", profile.thursday),
            new boolField("           Friday", profile.friday),
            new boolField("           Saturday", profile.saturday),
            new boolField("           Sunday", profile.sunday),
            new stringField("segment 1 start", profile.segment1start),
            new stringField("          end", profile.segment1end),
            new stringField("segment 2 start", profile.segment2start),
            new stringField("          end", profile.segment2end),
            new stringField("segment 3 start", profile.segment3start),
            new stringField("          end", profile.segment3end),
        };

        display("get-time-profile", fields);
    }

    static void SetTimeProfile(Uhppoted u, string[] args) {
        options opts = parse(args);
        uint deviceID = opts.deviceID;
        TimeProfile profile = new TimeProfile(PROFILE_ID, 71, "2022-02-01", "2022-06-30",
                                              true, false, true, true, false, false, true,
                                              "08:30", "11:30",
                                              "", "",
                                              "", "18:00");

        u.SetTimeProfile(deviceID, profile);

        field[] fields = {
            new uint32Field("ID", deviceID),
            new uint8Field("profile ID", profile.ID),
            new uint8Field("linked profile", profile.linked),
            new stringField("enabled from", profile.from),
            new stringField("        to", profile.to),
            new boolField("enabled on Monday", profile.monday),
            new boolField("           Tuesday", profile.tuesday),
            new boolField("           Wednesday", profile.wednesday),
            new boolField("           Thursday", profile.thursday),
            new boolField("           Friday", profile.friday),
            new boolField("           Saturday", profile.saturday),
            new boolField("           Sunday", profile.sunday),
            new stringField("segment 1 start", profile.segment1start),
            new stringField("          end", profile.segment1end),
            new stringField("segment 2 start", profile.segment2start),
            new stringField("          end", profile.segment2end),
            new stringField("segment 3 start", profile.segment3start),
            new stringField("          end", profile.segment3end),
        };

        display("set-time-profile", fields);
    }

    static void ClearTimeProfiles(Uhppoted u, string[] args) {
        options opts = parse(args);
        uint deviceID = opts.deviceID;

        u.ClearTimeProfiles(deviceID);

        field[] fields = {
            new uint32Field("ID", deviceID),
        };

        display("clear-time-profiles", fields);
    }

    static void AddTask(Uhppoted u, string[] args) {
        options opts = parse(args);
        uint deviceID = opts.deviceID;
        Task task = new Task(6, 4, "2022-02-01", "2022-06-30",
                             true, false, true, true, false, false, true,
                             "08:30", 11);

        u.AddTask(deviceID, task);

        field[] fields = {
            new uint32Field("ID", deviceID),
            new uint8Field("task", task.task),
            new uint8Field("door", task.door),
            new stringField("enabled from", task.from),
            new stringField("        to", task.to),
            new boolField("enabled on Monday", task.monday),
            new boolField("           Tuesday", task.tuesday),
            new boolField("           Wednesday:", task.wednesday),
            new boolField("           Thursday", task.thursday),
            new boolField("           Friday", task.friday),
            new boolField("           Saturday", task.saturday),
            new boolField("           Sunday", task.sunday),
            new stringField("at", task.at),
            new uint32Field("cards", task.cards),
        };

        display("add-task", fields);
    }

    static void RefreshTaskList(Uhppoted u, string[] args) {
        options opts = parse(args);
        uint deviceID = opts.deviceID;

        u.RefreshTaskList(deviceID);

        field[] fields = {
            new uint32Field("ID", deviceID),
        };

        display("refresh-tasklist", fields);
    }

    static void ClearTaskList(Uhppoted u, string[] args) {
        options opts = parse(args);
        uint deviceID = opts.deviceID;

        u.ClearTaskList(deviceID);

        field[] fields = {
            new uint32Field("ID", deviceID),
        };

        display("clear-tasklist", fields);
    }

    static void SetPCControl(Uhppoted u, string[] args) {
        options opts = parse(args);
        uint controller = opts.deviceID;
        bool enabled = true;

        u.SetPCControl(controller, enabled);

        field[] fields = {
            new uint32Field("ID", controller),
            new boolField("enabled", enabled),
        };

        display("set-pc-control", fields);
    }

    static void SetInterlock(Uhppoted u, string[] args) {
        options opts = parse(args);
        uint controller = opts.deviceID;
        byte interlock = 1;

        u.SetInterlock(controller, interlock);

        field[] fields = {
            new uint32Field("ID", controller),
            new uint8Field("interlock", interlock),
        };

        display("set-interlock", fields);
    }

    static void ActivateKeypads(Uhppoted u, string[] args) {
        options opts = parse(args);
        uint controller = opts.deviceID;
        bool reader1 = true;
        bool reader2 = true;
        bool reader3 = false;
        bool reader4 = true;

        u.ActivateKeypads(controller, reader1, reader2, reader3, reader4);

        field[] fields = {
            new uint32Field("ID", controller),
            new boolField("reader 1", reader1),
            new boolField("reader 2", reader2),
            new boolField("reader 3", reader3),
            new boolField("reader 4", reader4),
        };

        display("activate-keypads", fields);
    }

    static void SetDoorPasscodes(Uhppoted u, string[] args) {
        options opts = parse(args);
        uint controller = opts.deviceID;
        byte door = opts.door;
        uint passcode1 = 12345;
        uint passcode2 = 999999;
        uint passcode3 = 0;
        uint passcode4 = 54321;

        u.SetDoorPasscodes(controller, door, passcode1, passcode2, passcode3, passcode4);

        field[] fields = {
            new uint32Field("ID", controller),
            new uint8Field("door", door),
            new uint32Field("passcode 1", passcode1),
            new uint32Field("passcode 2", passcode2),
            new uint32Field("passcode 3", passcode3),
            new uint32Field("passcode 4", passcode4),
        };

        display("set-door-passcodes", fields);
    }

    static void RestoreDefaultParameters(Uhppoted u, string[] args) {
        options opts = parse(args);
        uint controller = opts.deviceID;

        u.RestoreDefaultParameters(controller);

        field[] fields = {
            new uint32Field("ID", controller),
        };

        display("restore-default-parameters", fields);
    }

    static void Listen(Uhppoted u, string[] args) {
        var exitEvent = new ManualResetEvent(false);
        Console.CancelKeyPress += (sender, eventArgs) => {
            eventArgs.Cancel = true;
            exitEvent.Set();
        };

        var thread = new Thread(() => listen(u, exitEvent));
        var timeout = TimeSpan.FromMilliseconds(2500);

        thread.IsBackground = true;
        thread.Start();

        exitEvent.WaitOne();
        WriteLine("DEBUG ... waiting for thread");
        thread.Join(timeout);
        WriteLine("DEBUG ... refuses to exit after swipe");
    }

    // NTS: C# bool is not uint8_t
    static void listen(Uhppoted u, ManualResetEvent done) {
        Uhppoted.OnEvent onevent = (ListenEvent e) => {
            Console.WriteLine("-- EVENT");
            Console.WriteLine("   controller: {0}", e.controller);
            Console.WriteLine("   timestamp:  {0}", e.timestamp);
            Console.WriteLine("   index:      {0}", e.index);
            Console.WriteLine("   event:      {0}", lookup.find(lookup.LOOKUP_EVENT_TYPE, e.eventType, locale));
            Console.WriteLine("   granted:    {0}", e.granted ? "yes" : "no");
            Console.WriteLine("   door:       {0}", e.door);
            Console.WriteLine("   direction:  {0}", lookup.find(lookup.LOOKUP_DIRECTION, e.direction, locale));
            Console.WriteLine("   card:       {0}", e.card);
            Console.WriteLine("   reason:     {0}", lookup.find(lookup.LOOKUP_EVENT_REASON, e.reason, locale));
            Console.WriteLine();
        };

        byte running = 0;
        byte stop = 0;
        TimeSpan delay = TimeSpan.FromMilliseconds(1000);

        u.ListenEvents(onevent, ref running, ref stop);

        Thread.Sleep(delay);
        for (int count = 0; count < 5 && !cbool(running); count++) {
            WriteLine("DEBUG ... waiting {0} {1}", count, cbool(running) ? "running" : "pending");
            Thread.Sleep(delay);
        }

        if (!cbool(running)) {
            WriteLine("ERROR {0}", "failed to start event listener");
            return;
        }

        WriteLine("INFO  ... listening");
        done.WaitOne();
        WriteLine("DEBUG .. stopping");

        stop = 1;
        Thread.Sleep(delay);
        for (int count = 0; count < 5 && cbool(running); count++) {
            WriteLine("DEBUG ... stoppping event listener {0} {1}", count, cbool(running) ? "running" : "stopped");
            Thread.Sleep(delay);
        }

        WriteLine("DEBUG ... waited");

        if (cbool(running)) {
            WriteLine("ERROR {0}", "failed to stop event listener");
        }

        WriteLine("DEBUG ... thread exit");
    }

    static bool cbool(byte v) {
        return v == 1;
    }

    static options parse(string[] args) {
        options opts = new options(DEVICE_ID, "192.168.1.125", "255.255.255.0", "192.168.1.0", "192.168.1.100:60001", CARD_NUMBER,
                                   CARD_INDEX, DOOR, EVENT_INDEX, PROFILE_ID);

        int ix = 1;
        while (ix < args.Length) {
            string arg = args[ix++];

            switch (arg) {
            case "--controller":
                if (ix < args.Length) {
                    opts.deviceID = Convert.ToUInt32(args[ix++]);
                }
                break;

            case "--ip-address":
                if (ix < args.Length) {
                    opts.ipAddress = args[ix++];
                }
                break;

            case "--subnet-mask":
                if (ix < args.Length) {
                    opts.subnetMask = args[ix++];
                }
                break;

            case "--gateway-address":
                if (ix < args.Length) {
                    opts.gateway = args[ix++];
                }
                break;

            case "--listener-address":
                if (ix < args.Length) {
                    opts.listener = args[ix++];
                }
                break;

            case "--card":
                if (ix < args.Length) {
                    opts.card = Convert.ToUInt32(args[ix++]);
                }
                break;

            case "--card-index":
                if (ix < args.Length) {
                    opts.cardIndex = Convert.ToUInt32(args[ix++]);
                }
                break;

            case "--door":
                if (ix < args.Length) {
                    opts.door = Convert.ToByte(args[ix++]);
                }
                break;

            case "--event-index":
                if (ix < args.Length) {
                    opts.eventIndex = Convert.ToUInt32(args[ix++]);
                }
                break;

            case "--time-profile":
                if (ix < args.Length) {
                    opts.timeProfileID = Convert.ToByte(args[ix++]);
                }
                break;
            }
        }

        return opts;
    }

    static void display(string tag, field[] fields) {
        int w = 0;
        foreach (var f in fields) {
            switch (f) {
            case uint32Field v:
                w = System.Math.Max(v.field.Length, w);
                break;

            case uint8Field v:
                w = System.Math.Max(v.field.Length, w);
                break;

            case boolField v:
                w = System.Math.Max(v.field.Length, w);
                break;

            case stringField v:
                w = System.Math.Max(v.field.Length, w);
                break;

            default:
                throw new Exception(Format("unsupported type {0}", f));
            }
        }

        string format = Format("  {{0, {0}}} {{1}}", -w);

        WriteLine(Format("{0}", tag));

        foreach (var f in fields) {
            switch (f) {
            case uint32Field v:
                WriteLine(Format(format, v.field, v.value));
                break;

            case uint8Field v:
                WriteLine(Format(format, v.field, v.value));
                break;

            case boolField v:
                if (v.value) {
                    WriteLine(Format(format, v.field, "Y"));
                } else {
                    WriteLine(Format(format, v.field, "N"));
                }
                break;

            case stringField v:
                WriteLine(Format(format, v.field, v.value));
                break;

            default:
                throw new Exception(Format("unsupported type {0}", f));
            }
        }

        WriteLine();
    }
}

// *** Utility classes for example output ***

public class field {}

public class uint32Field : field {
    public string field { get; }
    public uint value { get; }

    public uint32Field(string field, uint value) {
        this.field = field;
        this.value = value;
    }
}

public class uint8Field : field {
    public string field { get; }
    public byte value { get; }

    public uint8Field(string field, byte value) {
        this.field = field;
        this.value = value;
    }
}

public class boolField : field {
    public string field { get; }
    public bool value { get; }

    public boolField(string field, bool value) {
        this.field = field;
        this.value = value;
    }
}

public class stringField : field {
    public string field { get; }
    public string value { get; }

    public stringField(string field, string value) {
        this.field = field;
        this.value = value;
    }
}
