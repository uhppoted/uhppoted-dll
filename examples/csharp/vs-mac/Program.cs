using static System.Console;
using static System.String;
using System.Text.RegularExpressions;

using uhppoted;

public class command
{
    public string cmd;
    public string help;
    public Action<Uhppoted, string[]> fn;

    public command(string cmd, string help, Action<Uhppoted, string[]> fn)
    {
        this.cmd = cmd;
        this.help = help;
        this.fn = fn;
    }
};

class UhppotedDLLCLI
{
    const uint CONTROLLER_ID = 405419896;
    const byte DOOR = 1;
    const string DOOR_MODE = "controlled";
    const byte DOOR_DELAY = 5;
    const uint CARD_NUMBER = 10058399;
    const uint CARD_INDEX = 1;
    const string CARD_FROM = "2023-01-01";
    const string CARD_TO = "2024-12-31";
    const uint CARD_PIN = 0;
    const uint EVENT_INDEX = 1;
    const byte TIME_PROFILE_ID = 2;
    const string TIME_PROFILE_START = "2023-01-01";
    const string TIME_PROFILE_END = "2023-12-31";
    const string TASK_AT = "00:00";
    const string LOCALE = "";

    static void Main(string[] args)
    {
        if (args.Length < 1)
        {
            usage();
        }
        else if (args[0] == "help")
        {
            help();
        }
        else
        {
            Controller[] controllers = { };
            Uhppoted u = new Uhppoted("0.0.0.0", "255.255.255.255", "0.0.0.0:60001", 2500, controllers, true);
            string cmd = args[0] ?? "";

            WriteLine("uhppoted-dll Visual Studio C# Example (MacOS)");

            try
            {
#pragma warning disable CS8600
                command c = Array.Find(commands, c => c.cmd == cmd);
#pragma warning restore CS8600

                if (c == null)
                {
                    WriteLine();
                    WriteLine(Format("  *** ERROR: invalid command ({0})", cmd));
                    usage();
                }
                else
                {
                    c.fn(u, args);
                }
            }
            catch (Exception e)
            {
                WriteLine(Format("  *** ERROR: {0}", e.Message));
            }
        }
    }

    static command[] commands = {
        new command("get-controllers",
                    "Retrieves a list of UHPPOTE controller IDs findable on the local LAN.",
                    GetControllers),

        new command("get-controller",
                    "Retrieves the basic information for a single UHPPOTE controller.",
                    GetController),

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

        new command("delete-all-cards",
                    "Deletes all cards from a controller.",
                    DeleteAllCards),

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
    };

    static void usage()
    {
        WriteLine();
        WriteLine("  Usage: uhppoted-dll-cli <command>");
        WriteLine();
        WriteLine("  Supported commands");

        foreach (command c in commands)
        {
            WriteLine("    {0}", c.cmd);
        }

        WriteLine();
    }

    static void help()
    {
        WriteLine();
        WriteLine("  Usage: uhppoted-dll-cli <command>");
        WriteLine();
        WriteLine("  Commands");

        foreach (command c in commands)
        {
            WriteLine("    {0,-17}  {1}", c.cmd, c.help);
        }

        WriteLine();
    }

    static void GetControllers(Uhppoted u, string[] args)
    {
        uint[] list = u.GetDevices();

        WriteLine(Format("get-controllers ({0})", list.Length));
        for (int i = 0; i < list.Length; i++)
        {
            WriteLine(Format("   {0}", list[i]));
        }
    }

    static void GetController(Uhppoted u, string[] args)
    {
        uint controller = ParseArgs(args, "--controller", CONTROLLER_ID);

        Device device = u.GetDevice(controller);

        WriteLine(Format("get-controller ({0})", controller));
        WriteLine(Format("   address     {0}", device.address));
        WriteLine(Format("   subnet mask {0}", device.subnet));
        WriteLine(Format("   gateway     {0}", device.gateway));
        WriteLine(Format("   MAC         {0}", device.MAC));
        WriteLine(Format("   version     {0}", device.version));
        WriteLine(Format("   released    {0}", device.date));
    }

    static void SetAddress(Uhppoted u, string[] args)
    {
        uint controller = ParseArgs(args, "--controller", CONTROLLER_ID);
        string address = ParseArgs(args, "--address", "");
        string netmask = ParseArgs(args, "--netmask", "");
        string gateway = ParseArgs(args, "--gateway", "");

        u.SetAddress(controller, address, netmask, gateway);

        WriteLine(Format("set-address ({0})", controller));
        WriteLine(Format("   address     {0}", address));
        WriteLine(Format("   subnet mask {0}", netmask));
        WriteLine(Format("   gateway     {0}", gateway));
    }

    static void GetStatus(Uhppoted u, string[] args)
    {
        uint controller = ParseArgs(args, "--controller", CONTROLLER_ID);

        Status status = u.GetStatus(controller);
        string timestamp = status.evt.timestamp;

        if (timestamp == "") {
           timestamp = "-";
        }

        WriteLine(Format("get-status ({0})", controller));
        WriteLine(Format("   ID              {0}", status.ID));
        WriteLine(Format("   timestamp       {0}", status.sysdatetime));
        WriteLine(Format("   doors[1]        {0}", status.doors[0]));
        WriteLine(Format("   doors[2]        {0}", status.doors[1]));
        WriteLine(Format("   doors[3]        {0}", status.doors[2]));
        WriteLine(Format("   doors[4]        {0}", status.doors[3]));
        WriteLine(Format("   buttons[1]      {0}", status.buttons[0]));
        WriteLine(Format("   buttons[2]      {0}", status.buttons[1]));
        WriteLine(Format("   buttons[3]      {0}", status.buttons[2]));
        WriteLine(Format("   buttons[4]      {0}", status.buttons[3]));
        WriteLine(Format("   relays          {0}", status.relays));
        WriteLine(Format("   inputs          {0}", status.inputs));
        WriteLine(Format("   syserror        {0}", status.syserror));
        WriteLine(Format("   info            {0}", status.info));
        WriteLine(Format("   seqno           {0}", status.seqno));
        WriteLine(Format("   event timestamp {0}", timestamp));
        WriteLine(Format("         index     {0}", status.evt.index));
        WriteLine(Format("         type      {0}", lookup.find(lookup.LOOKUP_EVENT_TYPE, status.evt.eventType, LOCALE)));
        WriteLine(Format("         granted   {0}", status.evt.granted));
        WriteLine(Format("         door      {0}", status.evt.door));
        WriteLine(Format("         direction {0}", lookup.find(lookup.LOOKUP_DIRECTION, status.evt.direction, LOCALE)));
        WriteLine(Format("         card      {0}", status.evt.card));
        WriteLine(Format("         reason    {0}", lookup.find(lookup.LOOKUP_EVENT_REASON, status.evt.reason, LOCALE)));
    }

    static void GetTime(Uhppoted u, string[] args)
    {
        uint controller = ParseArgs(args, "--controller", CONTROLLER_ID);
        string datetime = u.GetTime(controller);

        WriteLine(Format("get-time ({0})", controller));
        WriteLine(Format("   date/time {0}", datetime));
    }

    static void SetTime(Uhppoted u, string[] args)
    {
        uint controller = ParseArgs(args, "--controller", CONTROLLER_ID);
        string datetime = ParseArgs(args, "--time", DateTime.Now.ToString("yyyy-MM-dd HH:mm:ss"));

        u.SetTime(controller, datetime);

        WriteLine(Format("set-time ({0})", controller));
        WriteLine(Format("   date/time {0}", datetime));
    }

    static void GetListener(Uhppoted u, string[] args)
    {
        uint controller = ParseArgs(args, "--controller", CONTROLLER_ID);
        string listener = u.GetListener(controller);

        WriteLine(Format("get-listener ({0})", controller));
        WriteLine(Format("   event listener {0}", listener));
    }

    static void SetListener(Uhppoted u, string[] args)
    {
        uint controller = ParseArgs(args, "--controller", CONTROLLER_ID);
        string listener = ParseArgs(args, "--listener", "");

        u.SetListener(controller, listener);

        WriteLine(Format("set-listener ({0})", controller));
        WriteLine(Format("   event listener {0}", listener));
    }

    static void GetDoorControl(Uhppoted u, string[] args)
    {
        uint controller = ParseArgs(args, "--controller", CONTROLLER_ID);
        byte door = ParseArgs(args, "--door", DOOR);

        DoorControl control = u.GetDoorControl(controller, door);

        WriteLine(Format("get-door-control ({0})", controller));
        WriteLine(Format("   door  {0}", door));
        WriteLine(Format("   mode  {0}", lookup.find(lookup.LOOKUP_MODE, control.mode, LOCALE)));
        WriteLine(Format("   delay {0}", control.delay));
    }

    static void SetDoorControl(Uhppoted u, string[] args)
    {
        uint controller = ParseArgs(args, "--controller", CONTROLLER_ID);
        byte door = ParseArgs(args, "--door", DOOR);
        string mode = ParseArgs(args, "--mode", DOOR_MODE);
        byte delay = ParseArgs(args, "--delay", DOOR_DELAY);

        switch (mode)
        {
            case "controlled": u.SetDoorControl(controller, door, DoorMode.Controlled, delay); break;
            case "normally-open": u.SetDoorControl(controller, door, DoorMode.NormallyOpen, delay); break;
            case "normally-closed": u.SetDoorControl(controller, door, DoorMode.NormallyClosed, delay); break;
            default: throw new ArgumentException(Format("Unknown door mode {0}", mode));
        }

        WriteLine(Format("set-door-control ({0})", controller));
        WriteLine(Format("   door  {0}", door));
        WriteLine(Format("   mode  {0}", mode));
        WriteLine(Format("   delay {0}", delay));
    }

    static void OpenDoor(Uhppoted u, string[] args)
    {
        uint controller = ParseArgs(args, "--controller", CONTROLLER_ID);
        byte door = ParseArgs(args, "--door", DOOR);

        u.OpenDoor(controller, door);

        WriteLine(Format("open-door ({0})", controller));
        WriteLine(Format("   door  {0}", door));
    }

    static void GetCards(Uhppoted u, string[] args)
    {
        uint controller = ParseArgs(args, "--controller", CONTROLLER_ID);
        uint cards = u.GetCards(controller);

        WriteLine(Format("get-cards ({0})", controller));
        WriteLine(Format("   cards  {0}", cards));
    }

    static void GetCard(Uhppoted u, string[] args)
    {
        uint controller = ParseArgs(args, "--controller", CONTROLLER_ID);
        uint cardNumber = ParseArgs(args, "--card", CARD_NUMBER);
        Card card = u.GetCard(controller, cardNumber);

        WriteLine(Format("get-card ({0})", controller));
        WriteLine(Format("   card number {0}", card.cardNumber));
        WriteLine(Format("   from        {0}", card.from));
        WriteLine(Format("   to          {0}", card.to));
        WriteLine(Format("   door[1]     {0}", card.doors[0]));
        WriteLine(Format("   door[2]     {0}", card.doors[1]));
        WriteLine(Format("   door[3]     {0}", card.doors[2]));
        WriteLine(Format("   door[4]     {0}", card.doors[3]));
        WriteLine(Format("   PIN         {0}", card.PIN));
    }

    static void GetCardByIndex(Uhppoted u, string[] args)
    {
        uint controller = ParseArgs(args, "--controller", CONTROLLER_ID);
        uint index = ParseArgs(args, "--index", CARD_INDEX);
        Card card = u.GetCardByIndex(controller, index);

        WriteLine(Format("get-card-by-index ({0})", controller));
        WriteLine(Format("   card index  {0}", index));
        WriteLine(Format("   card number {0}", card.cardNumber));
        WriteLine(Format("   from        {0}", card.from));
        WriteLine(Format("   to          {0}", card.to));
        WriteLine(Format("   door[1]     {0}", card.doors[0]));
        WriteLine(Format("   door[2]     {0}", card.doors[1]));
        WriteLine(Format("   door[3]     {0}", card.doors[2]));
        WriteLine(Format("   door[4]     {0}", card.doors[3]));
        WriteLine(Format("   PIN         {0}", card.PIN));
    }

    static void PutCard(Uhppoted u, string[] args)
    {
        uint controller = ParseArgs(args, "--controller", CONTROLLER_ID);
        uint cardNumber = ParseArgs(args, "--card", CARD_NUMBER);
        string start = ParseArgs(args, "--start", CARD_FROM);
        string end = ParseArgs(args, "--end", CARD_TO);
        uint PIN = ParseArgs(args, "--PIN", CARD_PIN);
        byte[] doors = { 0, 0, 0, 0 };

        Regex re = new Regex(@"([1-4])(:([0-9]{1,3}))?", RegexOptions.ECMAScript);
        string v = ParseArgs(args, "--doors", "");
        MatchCollection matches = re.Matches(v);

        foreach (Match match in matches)
        {
            if (match.Groups[1].Success)
            {
                int door = Convert.ToInt16(match.Groups[1].Value);
                int ix = door - 1;

                if (match.Groups[3].Success)
                {
                    byte profile = Convert.ToByte(match.Groups[3].Value);
                    if (profile > 1 && profile < 255)
                    {
                        doors[ix] = profile;
                    }
                }
                else
                {
                    doors[ix] = 1;
                }
            }
        }

        u.PutCard(controller, cardNumber, start, end, doors, PIN);

        WriteLine(Format("put-card ({0})", controller));
        WriteLine(Format("   card number {0}", cardNumber));
        WriteLine(Format("   start date  {0}", start));
        WriteLine(Format("   end date    {0}", end));
        WriteLine(Format("   door[1]     {0}", doors[0]));
        WriteLine(Format("   door[2]     {0}", doors[1]));
        WriteLine(Format("   door[3]     {0}", doors[2]));
        WriteLine(Format("   door[4]     {0}", doors[3]));
        WriteLine(Format("   PIN         {0}", PIN));
    }

    static void DeleteCard(Uhppoted u, string[] args)
    {
        uint controller = ParseArgs(args, "--controller", CONTROLLER_ID);
        uint cardNumber = ParseArgs(args, "--card", CARD_NUMBER);

        u.DeleteCard(controller, cardNumber);

        WriteLine(Format("delete-card ({0})", controller));
        WriteLine(Format("   card number {0}", cardNumber));
    }

    static void DeleteAllCards(Uhppoted u, string[] args)
    {
        uint controller = ParseArgs(args, "--controller", CONTROLLER_ID);

        u.DeleteCards(controller);

        WriteLine(Format("delete-cards ({0})", controller));
        WriteLine(Format("   ok"));
    }

    static void GetEventIndex(Uhppoted u, string[] args)
    {
        uint controller = ParseArgs(args, "--controller", CONTROLLER_ID);
        uint index = u.GetEventIndex(controller);

        WriteLine(Format("get-event-index ({0})", controller));
        WriteLine(Format("   index {0}", index));
    }

    static void SetEventIndex(Uhppoted u, string[] args)
    {
        uint controller = ParseArgs(args, "--controller", CONTROLLER_ID);
        uint index = ParseArgs(args, "--index", EVENT_INDEX);

        u.SetEventIndex(controller, index);

        WriteLine(Format("set-event-index ({0})", controller));
        WriteLine(Format("   index {0}", index));
    }

    static void GetEvent(Uhppoted u, string[] args)
    {
        uint controller = ParseArgs(args, "--controller", CONTROLLER_ID);
        uint index = ParseArgs(args, "--index", EVENT_INDEX);

        Event evt = u.GetEvent(controller, index);

        WriteLine(Format("get-event ({0})", controller));
        WriteLine(Format("   event index {0}", evt.index));
        WriteLine(Format("   timestamp   {0}", evt.timestamp));
        WriteLine(Format("   type        {0}", lookup.find(lookup.LOOKUP_EVENT_TYPE, evt.eventType, LOCALE)));
        WriteLine(Format("   granted     {0}", evt.granted));
        WriteLine(Format("   door        {0}", evt.door));
        WriteLine(Format("   direction   {0}", lookup.find(lookup.LOOKUP_DIRECTION, evt.direction, LOCALE)));
        WriteLine(Format("   card number {0}", evt.card));
        WriteLine(Format("   reason      {0}", lookup.find(lookup.LOOKUP_EVENT_REASON, evt.reason, LOCALE)));
    }

    static void RecordSpecialEvents(Uhppoted u, string[] args)
    {
        uint controller = ParseArgs(args, "--controller", CONTROLLER_ID);
        bool disable = ParseArgs(args, "--disabled", false);

        u.RecordSpecialEvents(controller, !disable);

        WriteLine(Format("record-special-events ({0})", controller));
        WriteLine(Format("   ok"));
    }

    static void GetTimeProfile(Uhppoted u, string[] args)
    {
        uint controller = ParseArgs(args, "--controller", CONTROLLER_ID);
        byte profileID = ParseArgs(args, "--profile", TIME_PROFILE_ID);

        TimeProfile profile = u.GetTimeProfile(controller, profileID);

        WriteLine(Format("get-time-profile ({0})", controller));
        WriteLine(Format("   profile ID           {0}", profileID));
        WriteLine(Format("   linked profile       {0}", profile.linked));
        WriteLine(Format("   enabled from         {0}", profile.from));
        WriteLine(Format("           until        {0}", profile.to));
        WriteLine(Format("   enabled on Monday    {0}", profile.monday));
        WriteLine(Format("              Tuesday   {0}", profile.tuesday));
        WriteLine(Format("              Wednesday {0}", profile.wednesday));
        WriteLine(Format("              Thursday  {0}", profile.thursday));
        WriteLine(Format("              Friday    {0}", profile.friday));
        WriteLine(Format("              Saturday  {0}", profile.saturday));
        WriteLine(Format("              Sunday    {0}", profile.sunday));
        WriteLine(Format("   segment 1  start     {0}", profile.segment1start));
        WriteLine(Format("              end       {0}", profile.segment1end));
        WriteLine(Format("   segment 2  start     {0}", profile.segment2start));
        WriteLine(Format("              end       {0}", profile.segment2end));
        WriteLine(Format("   segment 3  start     {0}", profile.segment3start));
        WriteLine(Format("              end       {0}", profile.segment3end));
    }

    static void SetTimeProfile(Uhppoted u, string[] args)
    {
        uint controller = ParseArgs(args, "--controller", CONTROLLER_ID);
        byte profileID = ParseArgs(args, "--profile", TIME_PROFILE_ID);
        byte linkedProfile = ParseArgs(args, "--linked", 0);
        string startDate = ParseArgs(args, "--start", TIME_PROFILE_START);
        string endDate = ParseArgs(args, "--end", TIME_PROFILE_END);
        bool monday = false;
        bool tuesday = false;
        bool wednesday = false;
        bool thursday = false;
        bool friday = false;
        bool saturday = false;
        bool sunday = false;
        string[,] segments = { { "", "" }, { "", "" }, { "", "" } };

        Regex re = new Regex(@"(Mon|Tue|Wed|Thu|Fri|Sat|Sun)*", RegexOptions.ECMAScript);
        string v = ParseArgs(args, "--weekdays", "");
        MatchCollection matches = re.Matches(v);

        foreach (Match match in matches)
        {
            if (match.Groups[1].Success)
            {
                string weekday = match.Groups[1].Value;
                switch (weekday)
                {
                    case "Mon": monday = true; break;
                    case "Tue": tuesday = true; break;
                    case "Wed": wednesday = true; break;
                    case "Thu": thursday = true; break;
                    case "Fri": friday = true; break;
                    case "Sat": saturday = true; break;
                    case "Sun": sunday = true; break;
                }
            }
        }

        re = new Regex(@"([0-9]{1,2}:[0-9]{2})-([0-9]{1,2}:[0-9]{2})", RegexOptions.ECMAScript);
        v = ParseArgs(args, "--segments", "");
        matches = re.Matches(v);

        int ix = 0;
        foreach (Match match in matches)
        {
            if (ix < 3)
            {
                segments[ix, 0] = match.Groups[1].Value;
                segments[ix, 1] = match.Groups[2].Value;
                ix++;
            }
        }


        TimeProfile profile = new TimeProfile(profileID, linkedProfile, startDate, endDate,
                                          monday, tuesday, wednesday, thursday, friday, saturday, sunday,
                                          segments[0, 0], segments[0, 1],
                                          segments[1, 0], segments[1, 1],
                                          segments[2, 0], segments[2, 1]);

        u.SetTimeProfile(controller, profile);

        WriteLine(Format("set-time-profile ({0})", controller));
        WriteLine(Format("   profile ID           {0}", profileID));
        WriteLine(Format("   linked profile       {0}", profile.linked));
        WriteLine(Format("   enabled from         {0}", profile.from));
        WriteLine(Format("           until        {0}", profile.to));
        WriteLine(Format("   enabled on Monday    {0}", profile.monday));
        WriteLine(Format("              Tuesday   {0}", profile.tuesday));
        WriteLine(Format("              Wednesday {0}", profile.wednesday));
        WriteLine(Format("              Thursday  {0}", profile.thursday));
        WriteLine(Format("              Friday    {0}", profile.friday));
        WriteLine(Format("              Saturday  {0}", profile.saturday));
        WriteLine(Format("              Sunday    {0}", profile.sunday));
        WriteLine(Format("   segment 1  start     {0}", profile.segment1start));
        WriteLine(Format("              end       {0}", profile.segment1end));
        WriteLine(Format("   segment 2  start     {0}", profile.segment2start));
        WriteLine(Format("              end       {0}", profile.segment2end));
        WriteLine(Format("   segment 3  start     {0}", profile.segment3start));
        WriteLine(Format("              end       {0}", profile.segment3end));
    }

    static void ClearTimeProfiles(Uhppoted u, string[] args)
    {
        uint controller = ParseArgs(args, "--controller", CONTROLLER_ID);

        u.ClearTimeProfiles(controller);

        WriteLine(Format("clear-time-profiles ({0})", controller));
        WriteLine(Format("   ok"));
    }

    static void AddTask(Uhppoted u, string[] args)
    {
        uint controller = ParseArgs(args, "--controller", CONTROLLER_ID);
        byte door = ParseArgs(args, "--door", DOOR);
        string startDate = ParseArgs(args, "--start", TIME_PROFILE_START);
        string endDate = ParseArgs(args, "--end", TIME_PROFILE_END);
        bool monday = false;
        bool tuesday = false;
        bool wednesday = false;
        bool thursday = false;
        bool friday = false;
        bool saturday = false;
        bool sunday = false;
        string at = ParseArgs(args, "--at", TASK_AT);
        byte moreCards = ParseArgs(args, "--more-cards", 0);
        string taskName = ParseArgs(args, "--task", "");
        byte taskID = 0;

        switch (taskName)
        {
            case "control door": taskID = 0; break;
            case "unlock door": taskID = 1; break;
            case "lock door": taskID = 2; break;
            case "disable time profile": taskID = 3; break;
            case "enable time profile": taskID = 4; break;
            case "enable card, no password": taskID = 5; break;
            case "enable card+in password": taskID = 6; break;
            case "enable card+password": taskID = 7; break;
            case "enable more cards": taskID = 8; break;
            case "disable more cards": taskID = 9; break;
            case "trigger once": taskID = 10; break;
            case "disable push button": taskID = 11; break;
            case "enable push button": taskID = 12; break;
            default: throw new ArgumentException(Format("Unknown task {0}", taskName));
        }

        Regex re = new Regex(@"(Mon|Tue|Wed|Thu|Fri|Sat|Sun)*", RegexOptions.ECMAScript);
        string v = ParseArgs(args, "--weekdays", "");
        MatchCollection matches = re.Matches(v);

        foreach (Match match in matches)
        {
            if (match.Groups[1].Success)
            {
                string weekday = match.Groups[1].Value;
                switch (weekday)
                {
                    case "Mon": monday = true; break;
                    case "Tue": tuesday = true; break;
                    case "Wed": wednesday = true; break;
                    case "Thu": thursday = true; break;
                    case "Fri": friday = true; break;
                    case "Sat": saturday = true; break;
                    case "Sun": sunday = true; break;
                }
            }
        }

        uhppoted.Task task = new uhppoted.Task(taskID, door, startDate, endDate,
                             monday, tuesday, wednesday, thursday, friday, saturday, sunday,
                             at, moreCards);

        u.AddTask(controller, task);

        WriteLine(Format("add-task ({0})", controller));
        WriteLine(Format("   task                 {0}", task.task));
        WriteLine(Format("   door                 {0}", task.door));
        WriteLine(Format("   enabled from         {0}", task.from));
        WriteLine(Format("           until        {0}", task.to));
        WriteLine(Format("   enabled on Monday    {0}", task.monday));
        WriteLine(Format("              Tuesday   {0}", task.tuesday));
        WriteLine(Format("              Wednesday {0}", task.wednesday));
        WriteLine(Format("              Thursday  {0}", task.thursday));
        WriteLine(Format("              Friday    {0}", task.friday));
        WriteLine(Format("              Saturday  {0}", task.saturday));
        WriteLine(Format("              Sunday    {0}", task.sunday));
        WriteLine(Format("   at                   {0}", task.at));
        WriteLine(Format("   cards                {0}", task.cards));
    }

    static void RefreshTaskList(Uhppoted u, string[] args)
    {
        uint controller = ParseArgs(args, "--controller", CONTROLLER_ID);

        u.RefreshTaskList(controller);

        WriteLine(Format("refresh-tasklist ({0})", controller));
        WriteLine(Format("   ok"));
    }

    static void ClearTaskList(Uhppoted u, string[] args)
    {
        uint controller = ParseArgs(args, "--controller", CONTROLLER_ID);

        u.ClearTaskList(controller);

        WriteLine(Format("clear-tasklist ({0})", controller));
        WriteLine(Format("   ok"));
    }

    static void SetPCControl(Uhppoted u, string[] args)
    {
        uint controller = ParseArgs(args, "--controller", CONTROLLER_ID);
        bool disable = ParseArgs(args, "--disabled", false);

        u.SetPCControl(controller, !disable);

        WriteLine(Format("set-pc-control ({0})", controller));
        WriteLine(Format("   enabled {0}", !disable));
    }

    static void SetInterlock(Uhppoted u, string[] args)
    {
        uint controller = ParseArgs(args, "--controller", CONTROLLER_ID);
        string interlock = ParseArgs(args, "--interlock", "");

        switch (interlock)
        {
            case "none": u.SetInterlock(controller, 0); break;
            case "1&2": u.SetInterlock(controller, 1); break;
            case "3&4": u.SetInterlock(controller, 2); break;
            case "1&2,3&4": u.SetInterlock(controller, 3); break;
            case "1&2&3": u.SetInterlock(controller, 4); break;
            case "1&2&3&4": u.SetInterlock(controller, 8); break;
            default: throw new ArgumentException(Format("Invalid interlock {0}", interlock));
        }

        WriteLine(Format("set-interlock ({0})", controller));
        WriteLine(Format("   interlock {0}", interlock));
    }

    static void ActivateKeypads(Uhppoted u, string[] args)
    {
        uint controller = ParseArgs(args, "--controller", CONTROLLER_ID);
        bool reader1 = false;
        bool reader2 = false;
        bool reader3 = false;
        bool reader4 = false;

        Regex re = new Regex(@"([1-4])*", RegexOptions.ECMAScript);
        string keypads = ParseArgs(args, "--keypads", "");
        MatchCollection matches = re.Matches(keypads);

        foreach (Match match in matches)
        {
            if (match.Groups[1].Success)
            {
                string reader = match.Groups[1].Value;
                switch (reader)
                {
                    case "1": reader1 = true; break;
                    case "2": reader2 = true; break;
                    case "3": reader3 = true; break;
                    case "4": reader4 = true; break;
                }
            }
        }

        u.ActivateKeypads(controller, reader1, reader2, reader3, reader4);

        WriteLine(Format("activate-keypads ({0})", controller));
        WriteLine(Format("   reader 1 {0}", reader1));
        WriteLine(Format("   reader 2 {0}", reader2));
        WriteLine(Format("   reader 3 {0}", reader3));
        WriteLine(Format("   reader 4 {0}", reader4));
    }

    static void SetDoorPasscodes(Uhppoted u, string[] args)
    {
        uint controller = ParseArgs(args, "--controller", CONTROLLER_ID);
        byte door = ParseArgs(args, "--door", DOOR);
        string[] passcodes = ParseArgs(args, "--passcodes", "").Split(",");
        uint passcode1 = 0;
        uint passcode2 = 0;
        uint passcode3 = 0;
        uint passcode4 = 0;

        int ix = 1;
        foreach (string passcode in passcodes)
        {
            uint v = Convert.ToUInt32(passcode);

            if (v >= 1 && v <= 999999)
            {
                switch (ix++)
                {
                    case 1: passcode1 = v; break;
                    case 2: passcode2 = v; break;
                    case 3: passcode3 = v; break;
                    case 4: passcode4 = v; break;
                }
            }
        }

        u.SetDoorPasscodes(controller, door, passcode1, passcode2, passcode3, passcode4);

        WriteLine(Format("set-door-passcodes ({0})", controller));
        WriteLine(Format("   door     {0}", door));
        WriteLine(Format("   passcode 1 {0}", passcode1));
        WriteLine(Format("   passcode 2 {0}", passcode2));
        WriteLine(Format("   passcode 3 {0}", passcode3));
        WriteLine(Format("   passcode 4 {0}", passcode4));
    }

    static void RestoreDefaultParameters(Uhppoted u, string[] args)
    {
        uint controller = ParseArgs(args, "--controller", CONTROLLER_ID);

        u.RestoreDefaultParameters(controller);

        WriteLine(Format("restore-default-parameters ({0})", controller));
    }

    static UInt32 ParseArgs(string[] args, string option, UInt32 defval)
    {
        int ix = 1;
        while (ix < args.Length)
        {
            string arg = args[ix++];

            if (arg == option && ix < args.Length)
            {
                return Convert.ToUInt32(args[ix++]);
            }
        }

        return defval;
    }

    static byte ParseArgs(string[] args, string option, byte defval)
    {
        int ix = 1;
        while (ix < args.Length)
        {
            string arg = args[ix++];

            if (arg == option && ix < args.Length)
            {
                return Convert.ToByte(args[ix++]);
            }
        }

        return defval;
    }

    static string ParseArgs(string[] args, string option, string defval)
    {
        int ix = 1;
        while (ix < args.Length)
        {
            string arg = args[ix++];

            if (arg == option && ix < args.Length)
            {
                return args[ix++];
            }
        }

        return defval;
    }

    static bool ParseArgs(string[] args, string option, bool defval)
    {
        int ix = 1;
        while (ix < args.Length)
        {
            string arg = args[ix++];

            if (arg == option)
            {
                return true;
            }
        }

        return defval;
    }
}
