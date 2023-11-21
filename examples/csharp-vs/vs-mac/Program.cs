using static System.Console;
using static System.String;
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
        uint controllerID = ParseArgs(args, "--controller", CONTROLLER_ID);

        Device device = u.GetDevice(controllerID);

        WriteLine(Format("get-controller ({0})", controllerID));
        WriteLine(Format("   address {0}", device.address));
        WriteLine(Format("   subnet mask {0}", device.subnet));
        WriteLine(Format("   gateway address {0}", device.gateway));
        WriteLine(Format("   MAC {0}", device.MAC));
        WriteLine(Format("   version {0}", device.version));
        WriteLine(Format("   released {0}", device.date));
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
}
