using static System.Console;
using static System.String;
using uhppoted;

Controller[] controllers = { };
Uhppoted u = new Uhppoted("0.0.0.0", "255.255.255.255", "0.0.0.0:60001", 2500, controllers, true);

WriteLine("uhppoted-dll Visual Studio C# Example (MacOS)");
uint[] list = u.GetDevices();

WriteLine(Format("get-devices ({0})", list.Length));

for (int i = 0; i < list.Length; i++)
{
    WriteLine(Format("   {0}", list[i]));
}


