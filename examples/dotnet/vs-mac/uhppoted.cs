using System;
using System.Runtime.InteropServices;
using System.Collections.Generic;
using System.Text;

namespace uhppoted
{

    public class Uhppoted : IDisposable
    {
        private const string DLL = "libuhppoted.dylib";
        private UHPPOTE u = new UHPPOTE();

        public Uhppoted() { }

        public Uhppoted(string bind, string broadcast, string listen, int timeout, Controller[] controllers, bool debug)
        {
            this.u.bind = bind;
            this.u.broadcast = broadcast;
            this.u.listen = listen;
            this.u.timeout = timeout;
            this.u.devices = IntPtr.Zero;
            this.u.debug = debug;

            uint N = (uint)controllers.Length;
            udevice[] list = new udevice[N];

            for (int ix = 0; ix < controllers.Length; ix++)
            {
                Controller c = controllers[ix];

                list[ix].ID = c.ID;
                list[ix].address = c.address;
                list[ix].transport = c.transport;
            }

            int sz = Marshal.SizeOf(typeof(udevice));
            IntPtr p = Marshal.AllocHGlobal((int)N * sz);

            for (int ix = 0; ix < list.Length; ix++)
            {
                udevice d = list[ix];
                IntPtr q = p + ix * sz;

                Marshal.StructureToPtr(d, q, false);
            }

            udevices devices = new udevices();
            devices.N = N;
            devices.devices = p;

            IntPtr r = Marshal.AllocHGlobal(Marshal.SizeOf(devices));
            Marshal.StructureToPtr(devices, r, false);

            this.u.devices = r;
        }

        ~Uhppoted() { dispose(); }

        public void Dispose()
        {
            dispose();
            GC.SuppressFinalize(this);
        }

        private void dispose()
        {
            IntPtr p = this.u.devices;

            if (p != IntPtr.Zero)
            {
                udevices devices = (udevices)Marshal.PtrToStructure(p, typeof(udevices))!;
                IntPtr q = devices.devices;

                Marshal.FreeHGlobal(q);
                Marshal.FreeHGlobal(p);
            }
        }

        public uint[] GetDevices()
        {
            int N = 0;
            int count = N;
            uint[] slice;
            GoError err = new GoError();

            err.len = 256;
            err.message = Marshal.AllocHGlobal(256);

            try
            {
                do
                {
                    N += 16;
                    count = N;
                    slice = new uint[N];

                    if (GetDevices(ref this.u, slice, ref count, ref err) != 0)
                    {
                        raise(err);
                    }
                } while (N < count);

                uint[] list = new uint[count];

                Array.Copy(slice, list, list.Length);

                return list;
            }
            finally
            {
                Marshal.FreeHGlobal(err.message);
            }
        }

        public Device GetDevice(uint deviceID)
        {
            GoDevice device = new GoDevice();
            GoError err = new GoError();

            device.address = Marshal.AllocHGlobal(16);
            device.subnet = Marshal.AllocHGlobal(16);
            device.gateway = Marshal.AllocHGlobal(16);
            device.MAC = Marshal.AllocHGlobal(18);
            device.version = Marshal.AllocHGlobal(7);
            device.date = Marshal.AllocHGlobal(11);

            err.len = 256;
            err.message = Marshal.AllocHGlobal(256);

            try
            {
                if (GetDevice(ref this.u, deviceID, ref device, ref err) != 0)
                {
                    raise(err);
                }

                uint ID = device.ID;
                string address = Marshal.PtrToStringAnsi(device.address)!;
                string netmask = Marshal.PtrToStringAnsi(device.subnet)!;
                string gateway = Marshal.PtrToStringAnsi(device.gateway)!;
                string MAC = Marshal.PtrToStringAnsi(device.MAC)!;
                string version = Marshal.PtrToStringAnsi(device.version)!;
                string date = Marshal.PtrToStringAnsi(device.date)!;

                return new Device(ID, address, netmask, gateway, MAC, version, date);
            }
            finally
            {
                Marshal.FreeHGlobal(device.address);
                Marshal.FreeHGlobal(device.subnet);
                Marshal.FreeHGlobal(device.gateway);
                Marshal.FreeHGlobal(device.MAC);
                Marshal.FreeHGlobal(device.version);
                Marshal.FreeHGlobal(device.date);

                Marshal.FreeHGlobal(err.message);
            }
        }

        public void SetAddress(uint deviceID, string address, string subnet,
                               string gateway)
        {
            GoError err = new GoError();

            err.len = 256;
            err.message = Marshal.AllocHGlobal(256);

            try
            {
                if (SetAddress(ref this.u, deviceID, address, subnet, gateway, ref err) != 0) 
                {
                    raise(err);
                }
            }
            finally
            {
                Marshal.FreeHGlobal(err.message);
            }
        }

        public Status GetStatus(uint deviceID)
        {
            GoStatus status = new GoStatus();
            GoEvent _evt = new GoEvent();
            GoError err = new GoError();

            err.len = 256;
            err.message = Marshal.AllocHGlobal(256);

            _evt.timestamp = Marshal.AllocHGlobal(20);

            status.sysdatetime = Marshal.AllocHGlobal(20);
            status.doors = Marshal.AllocHGlobal(4);
            status.buttons = Marshal.AllocHGlobal(4);
            status.evt = Marshal.AllocHGlobal(Marshal.SizeOf(typeof(GoEvent)));

            Marshal.StructureToPtr(_evt, status.evt, true);

            try
            {
                if (GetStatus(ref this.u, deviceID, ref status, ref err) != 0)  
                {
                    raise(err);
                }

                string sysdatetime = Marshal.PtrToStringAnsi(status.sysdatetime)!;
                byte[] doors = new byte[4];
                byte[] buttons = new byte[4];
                Event e = new Event("", 0, 0, false, 0, 0, 0, 0);

                Marshal.Copy(status.doors, doors, 0, 4);
                Marshal.Copy(status.buttons, buttons, 0, 4);

                if (status.evt != IntPtr.Zero)
                {
                   GoEvent evt = (GoEvent)Marshal.PtrToStructure(status.evt, typeof(GoEvent))!;
                   string timestamp = Marshal.PtrToStringAnsi(evt.timestamp)!;

                    e = new Event(timestamp,
                                  evt.index,
                                  evt.eventType,
                                  evt.granted != 0,
                                  evt.door,
                                  evt.direction,
                                  evt.card,
                                  evt.reason);
                }

                return new Status(status.ID,
                                  sysdatetime,
                                  new bool[] {
                                      doors[0] == 1,
                                      doors[1] == 1,
                                      doors[2] == 1,
                                      doors[3] == 1,
                                  },
                                  new bool[] {
                                      buttons[0] == 1,
                                      buttons[1] == 1,
                                    buttons[2] == 1,
                                    buttons[3] == 1,
                                  },
                                  status.relays,
                                  status.inputs,
                                  status.syserror,
                                  status.info,
                                  status.seqno,
                                  e);
            }
            finally
            {
                Marshal.FreeHGlobal(status.sysdatetime);
                Marshal.FreeHGlobal(status.doors);
                Marshal.FreeHGlobal(status.buttons);
                Marshal.FreeHGlobal(status.evt);
                Marshal.FreeHGlobal(err.message);
            }
        }

        public string GetTime(uint deviceID)
        {
            IntPtr datetime = Marshal.AllocHGlobal(20);
            GoError err = new GoError();

            err.len = 256;
            err.message = Marshal.AllocHGlobal(256);

            try
            { 
                if (GetTime(ref this.u, deviceID, datetime, ref err) != 0)
                {
                    raise(err);
                }

                return Marshal.PtrToStringAnsi(datetime)!;
            }
            finally
            {
                Marshal.FreeHGlobal(datetime);
                Marshal.FreeHGlobal(err.message);
            }
        }

        public void SetTime(uint deviceID, string datetime)
        {
            GoError err = new GoError();

            err.len = 256;
            err.message = Marshal.AllocHGlobal(256);

            try
            { 
                if (SetTime(ref this.u, deviceID, datetime, ref err) != 0)
                {
                    raise(err);
                }
            }
            finally
            {
                Marshal.FreeHGlobal(err.message);
            }
        }

        public string GetListener(uint deviceID)
        {
            IntPtr listener = Marshal.AllocHGlobal(22);
            byte interval = 0;
            GoError err = new GoError();

            err.len = 256;
            err.message = Marshal.AllocHGlobal(256);

            try
            { 
                if (GetListener(ref this.u, deviceID, listener, ref interval, ref err) != 0)
                {
                    raise(err);
                }

                return Marshal.PtrToStringAnsi(listener)!;
            }
            finally
            {
                Marshal.FreeHGlobal(listener);
                Marshal.FreeHGlobal(err.message);
            }
        }

        public byte GetListenerInterval(uint deviceID)
        {
            IntPtr listener = Marshal.AllocHGlobal(22);
            byte interval = 0;
            GoError err = new GoError();

            err.len = 256;
            err.message = Marshal.AllocHGlobal(256);

            try
            { 
                if (GetListener(ref this.u, deviceID, listener, ref interval, ref err) != 0)
                {
                    raise(err);
                }

                return interval;
            }
            finally
            {
                Marshal.FreeHGlobal(listener);
                Marshal.FreeHGlobal(err.message);
            }
        }

        public void SetListener(uint deviceID, string listener, byte interval)
        {
            GoError err = new GoError();

            err.len = 256;
            err.message = Marshal.AllocHGlobal(256);

            try
            { 
                if (SetListener(ref this.u, deviceID, listener, interval, ref err) != 0)
                {
                    raise(err);
                }
            }
            finally
            {
                Marshal.FreeHGlobal(err.message);
            }
        }

        public DoorControl GetDoorControl(uint deviceID, byte door)
        {
            GoDoorControl control = new GoDoorControl();
            GoError err = new GoError();

            err.len = 256;
            err.message = Marshal.AllocHGlobal(256);

            try
            { 
                if (GetDoorControl(ref this.u, deviceID, door, ref control, ref err) != 0)
                {
                    raise(err);
                }

                return new DoorControl(control.control, control.delay);
            }
            finally
            {
                Marshal.FreeHGlobal(err.message);
            }

        }

        public void SetDoorControl(uint deviceID, byte door, byte mode, byte delay)
        {
            GoError err = new GoError();

            err.len = 256;
            err.message = Marshal.AllocHGlobal(256);

            try
            { 
                if (SetDoorControl(ref this.u, deviceID, door, mode, delay, ref err) != 0)
                {
                    raise(err);
                }
            }
            finally
            {
                Marshal.FreeHGlobal(err.message);
            }
        }

        public void OpenDoor(uint deviceID, byte door)
        {
            GoError err = new GoError();

            err.len = 256;
            err.message = Marshal.AllocHGlobal(256);

            try
            { 
                if (OpenDoor(ref this.u, deviceID, door, ref err) != 0)
                {
                    raise(err);
                }
            }
            finally
            {
                Marshal.FreeHGlobal(err.message);
            }
        }

        public uint GetCards(uint deviceID)
        {
            uint cards = 0;
            GoError err = new GoError();

            err.len = 256;
            err.message = Marshal.AllocHGlobal(256);

            try
            { 
                if (GetCards(ref this.u, deviceID, ref cards, ref err) != 0)
                {
                    raise(err);
                }

                return cards;
            }
            finally
            {
                Marshal.FreeHGlobal(err.message);
            }
        }

        public Card GetCard(uint deviceID, uint cardNumber)
        {
            GoCard card = new GoCard();
            GoError err = new GoError();

            card.from = Marshal.AllocHGlobal(11);
            card.to = Marshal.AllocHGlobal(11);
            card.doors = Marshal.AllocHGlobal(4);

            err.len = 256;
            err.message = Marshal.AllocHGlobal(256);

            try
            { 
                if (GetCard(ref this.u, deviceID, cardNumber, ref card, ref err) != 0)
                {
                    raise(err);
                }

                string from = Marshal.PtrToStringAnsi(card.from)!;
                string to = Marshal.PtrToStringAnsi(card.to)!;
                byte[] doors = new byte[4];

                Marshal.Copy(card.doors, doors, 0, 4);

                return new Card(card.cardNumber, from, to, doors, card.PIN);
            }
            finally
            {
                Marshal.FreeHGlobal(card.from);
                Marshal.FreeHGlobal(card.to);
                Marshal.FreeHGlobal(card.doors);
                Marshal.FreeHGlobal(err.message);
            }
        }

        public Card GetCardByIndex(uint deviceID, uint index)
        {
            GoCard card = new GoCard();
            GoError err = new GoError();

            card.from = Marshal.AllocHGlobal(11);
            card.to = Marshal.AllocHGlobal(11);
            card.doors = Marshal.AllocHGlobal(4);

            err.len = 256;
            err.message = Marshal.AllocHGlobal(256);

            try
            { 
                if (GetCardByIndex(ref this.u, deviceID, index, ref card, ref err) != 0)
                {
                    raise(err);
                }

                string from = Marshal.PtrToStringAnsi(card.from)!;
                string to = Marshal.PtrToStringAnsi(card.to)!;
                byte[] doors = new byte[4];

                Marshal.Copy(card.doors, doors, 0, 4);

                return new Card(card.cardNumber, from, to, doors, card.PIN);
            }
            finally
            {
                Marshal.FreeHGlobal(card.from);
                Marshal.FreeHGlobal(card.to);
                Marshal.FreeHGlobal(card.doors);
                Marshal.FreeHGlobal(err.message);
            }
        }

        public void PutCard(uint deviceID, uint cardNumber, string from, string to, byte[] doors, uint PIN)
        {
            GoError err = new GoError();

            err.len = 256;
            err.message = Marshal.AllocHGlobal(256);

            try
            { 
                if (PutCard(ref this.u, deviceID, cardNumber, from, to, doors, PIN, ref err) != 0)
                {
                    raise(err);
                }
            }
            finally
            {
                Marshal.FreeHGlobal(err.message);
            }
        }

        public void DeleteCard(uint deviceID, uint cardNumber)
        {
            GoError err = new GoError();

            err.len = 256;
            err.message = Marshal.AllocHGlobal(256);

            try
            { 
                if (DeleteCard(ref this.u, deviceID, cardNumber, ref err) != 0)
                {
                    raise(err);
                }
            }
            finally
            {
                Marshal.FreeHGlobal(err.message);
            }
        }

        public void DeleteCards(uint deviceID)
        {
            GoError err = new GoError();

            err.len = 256;
            err.message = Marshal.AllocHGlobal(256);

            try
            { 
                if (DeleteCards(ref this.u, deviceID, ref err) != 0)
                {
                    raise(err);
                }
            }
            finally
            {
                Marshal.FreeHGlobal(err.message);
            }
        }

        public uint GetEventIndex(uint deviceID)
        {
            uint index = 0;
            GoError err = new GoError();

            err.len = 256;
            err.message = Marshal.AllocHGlobal(256);

            try
            { 
                if (GetEventIndex(ref this.u, deviceID, ref index, ref err) != 0)
                {
                    raise(err);
                }

                return index;
            }
            finally
            {
                Marshal.FreeHGlobal(err.message);
            }
        }

        public void SetEventIndex(uint deviceID, uint index)
        {
            GoError err = new GoError();

            err.len = 256;
            err.message = Marshal.AllocHGlobal(256);

            try
            { 
                if (SetEventIndex(ref this.u, deviceID, index, ref err) != 0)
                {
                    raise(err);
                }
            }
            finally
            {
                Marshal.FreeHGlobal(err.message);
            }
        }

        public Event GetEvent(uint deviceID, uint index)
        {
            GoEvent evt = new GoEvent();
            GoError err = new GoError();

            evt.timestamp = Marshal.AllocHGlobal(20);

            err.len = 256;
            err.message = Marshal.AllocHGlobal(256);

            try
            { 
                if (GetEvent(ref this.u, deviceID, index, ref evt, ref err) != 0)
                {
                    raise(err);
                }

                string timestamp = Marshal.PtrToStringAnsi(evt.timestamp)!;
            
                return new Event(timestamp,
                                 evt.index,
                                 evt.eventType,
                                 evt.granted == 1,
                                 evt.door,
                                 evt.direction,
                                 evt.card,
                                 evt.reason);
            }
            finally
            {
                Marshal.FreeHGlobal(evt.timestamp);
                Marshal.FreeHGlobal(err.message);
            }
        }

        public void RecordSpecialEvents(uint deviceID, bool enabled)
        {
            GoError err = new GoError();

            err.len = 256;
            err.message = Marshal.AllocHGlobal(256);

            try
            { 
                if (RecordSpecialEvents(ref this.u, deviceID, enabled, ref err) != 0)
                {
                    raise(err);
                }
            }
            finally
            {
                Marshal.FreeHGlobal(err.message);
            }
        }

        public TimeProfile GetTimeProfile(uint deviceID, byte profileID)
        {
            GoTimeProfile profile = new GoTimeProfile();

            profile.from = Marshal.AllocHGlobal(11);
            profile.to = Marshal.AllocHGlobal(11);
            profile.segment1start = Marshal.AllocHGlobal(6);
            profile.segment1end = Marshal.AllocHGlobal(6);
            profile.segment2start = Marshal.AllocHGlobal(6);
            profile.segment2end = Marshal.AllocHGlobal(6);
            profile.segment3start = Marshal.AllocHGlobal(6);
            profile.segment3end = Marshal.AllocHGlobal(6);

            GoError err = new GoError();

            err.len = 256;
            err.message = Marshal.AllocHGlobal(256);

            try
            { 
                if (GetTimeProfile(ref this.u, deviceID, profileID, ref profile, ref err) != 0)
                {
                    raise(err);
                }

                string from = Marshal.PtrToStringAnsi(profile.from)!;
                string to = Marshal.PtrToStringAnsi(profile.to)!;
                string segment1start = Marshal.PtrToStringAnsi(profile.segment1start)!;
                string segment1end = Marshal.PtrToStringAnsi(profile.segment1end)!;
                string segment2start = Marshal.PtrToStringAnsi(profile.segment2start)!;
                string segment2end = Marshal.PtrToStringAnsi(profile.segment2end)!;
                string segment3start = Marshal.PtrToStringAnsi(profile.segment3start)!;
                string segment3end = Marshal.PtrToStringAnsi(profile.segment3end)!;

                return new TimeProfile(profile.ID,
                                       profile.linked,
                                       from,
                                       to,
                                       profile.monday != 0,
                                       profile.tuesday != 0,
                                       profile.wednesday != 0,
                                       profile.thursday != 0,
                                       profile.friday != 0,
                                       profile.saturday != 0,
                                       profile.sunday != 0,
                                       segment1start, segment1end,
                                       segment2start, segment2end,
                                       segment3start, segment3end);
            }
            finally
            {
                Marshal.FreeHGlobal(profile.from);
                Marshal.FreeHGlobal(profile.to);
                Marshal.FreeHGlobal(profile.segment1start);
                Marshal.FreeHGlobal(profile.segment1end);
                Marshal.FreeHGlobal(profile.segment2start);
                Marshal.FreeHGlobal(profile.segment2end);
                Marshal.FreeHGlobal(profile.segment3start);
                Marshal.FreeHGlobal(profile.segment3end);

                Marshal.FreeHGlobal(err.message);
            }
        }

        public void SetTimeProfile(uint deviceID, TimeProfile p)
        {
            GoTimeProfile profile = new GoTimeProfile();

            profile.ID = p.ID;
            profile.linked = p.linked;
            profile.from = Marshal.StringToHGlobalAnsi(p.from);
            profile.to = Marshal.StringToHGlobalAnsi(p.to);
            profile.monday = p.monday ? (byte)1 : (byte)0;
            profile.tuesday = p.tuesday ? (byte)1 : (byte)0;
            profile.wednesday = p.wednesday ? (byte)1 : (byte)0;
            profile.thursday = p.thursday ? (byte)1 : (byte)0;
            profile.friday = p.friday ? (byte)1 : (byte)0;
            profile.saturday = p.saturday ? (byte)1 : (byte)0;
            profile.sunday = p.sunday ? (byte)1 : (byte)0;
            profile.segment1start = Marshal.StringToHGlobalAnsi(p.segment1start);
            profile.segment1end = Marshal.StringToHGlobalAnsi(p.segment1end);
            profile.segment2start = Marshal.StringToHGlobalAnsi(p.segment2start);
            profile.segment2end = Marshal.StringToHGlobalAnsi(p.segment2end);
            profile.segment3start = Marshal.StringToHGlobalAnsi(p.segment3start);
            profile.segment3end = Marshal.StringToHGlobalAnsi(p.segment3end);

            GoError err = new GoError();

            err.len = 256;
            err.message = Marshal.AllocHGlobal(256);

            try
            { 
                if (SetTimeProfile(ref this.u, deviceID, ref profile, ref err) != 0)
                {
                    raise(err);
                }
            }
            finally
            {
                Marshal.FreeHGlobal(profile.from);
                Marshal.FreeHGlobal(profile.to);
                Marshal.FreeHGlobal(profile.segment1start);
                Marshal.FreeHGlobal(profile.segment1end);
                Marshal.FreeHGlobal(profile.segment2start);
                Marshal.FreeHGlobal(profile.segment2end);
                Marshal.FreeHGlobal(profile.segment3start);
                Marshal.FreeHGlobal(profile.segment3end);

                Marshal.FreeHGlobal(err.message);
            }
        }

        public void ClearTimeProfiles(uint deviceID)
        {
            GoError err = new GoError();

            err.len = 256;
            err.message = Marshal.AllocHGlobal(256);

            try
            { 
                if (ClearTimeProfiles(ref this.u, deviceID, ref err) != 0)
                {
                    raise(err);
                }
            }
            finally
            {
                Marshal.FreeHGlobal(err.message);
            }
        }

        public void AddTask(uint deviceID, Task t)
        {
            GoTask task = new GoTask();

            task.task = t.task;
            task.door = t.door;
            task.from = t.from;
            task.to = t.to;
            task.monday = t.monday ? (byte)1 : (byte)0;
            task.tuesday = t.tuesday ? (byte)1 : (byte)0;
            task.wednesday = t.wednesday ? (byte)1 : (byte)0;
            task.thursday = t.thursday ? (byte)1 : (byte)0;
            task.friday = t.friday ? (byte)1 : (byte)0;
            task.saturday = t.saturday ? (byte)1 : (byte)0;
            task.sunday = t.sunday ? (byte)1 : (byte)0;
            task.at = t.at;
            task.cards = t.cards;

            GoError err = new GoError();

            err.len = 256;
            err.message = Marshal.AllocHGlobal(256);

            try
            { 
                if (AddTask(ref this.u, deviceID, ref task, ref err) != 0)
                {
                    raise(err);
                }
            }
            finally
            {
                Marshal.FreeHGlobal(err.message);
            }
        }

        public void RefreshTaskList(uint deviceID)
        {
            GoError err = new GoError();

            err.len = 256;
            err.message = Marshal.AllocHGlobal(256);

            try
            { 
                if (RefreshTaskList(ref this.u, deviceID, ref err) != 0)
                {
                    raise(err);
                }
            }
            finally
            {
                Marshal.FreeHGlobal(err.message);
            }
        }

        public void ClearTaskList(uint deviceID)
        {
            GoError err = new GoError();

            err.len = 256;
            err.message = Marshal.AllocHGlobal(256);

            try
            { 
                if (ClearTaskList(ref this.u, deviceID, ref err) != 0)
                {
                    raise(err);
                }
            }
            finally
            {
                Marshal.FreeHGlobal(err.message);
            }
        }

        public void SetPCControl(uint controller, bool enabled)
        {
            GoError err = new GoError();

            err.len = 256;
            err.message = Marshal.AllocHGlobal(256);

            try
            { 
                if (SetPCControl(ref this.u, controller, enabled, ref err) != 0)
                {
                    raise(err);
                }
            }
            finally
            {
                Marshal.FreeHGlobal(err.message);
            }
        }

        public void SetInterlock(uint controller, byte interlock)
        {
            GoError err = new GoError();

            err.len = 256;
            err.message = Marshal.AllocHGlobal(256);

            try
            { 
                if (SetInterlock(ref this.u, controller, interlock, ref err) != 0)
                {
                    raise(err);
                }
            }
            finally
            {
                Marshal.FreeHGlobal(err.message);
            }
        }

        public void ActivateKeypads(uint controller, bool reader1, bool reader2, bool reader3, bool reader4)
        {
            GoError err = new GoError();

            err.len = 256;
            err.message = Marshal.AllocHGlobal(256);

            try
            { 
                if (ActivateKeypads(ref this.u, controller, reader1, reader2, reader3, reader4, ref err) != 0)
                {
                    raise(err);
                }
            }
            finally
            {
                Marshal.FreeHGlobal(err.message);
            }
        }

        public void SetDoorPasscodes(uint controller, byte door, uint passcode1, uint passcode2, uint passcode3, uint passcode4)
        {
            GoError err = new GoError();

            err.len = 256;
            err.message = Marshal.AllocHGlobal(256);

            try
            { 
                if (SetDoorPasscodes(ref this.u, controller, door, passcode1, passcode2, passcode3, passcode4, ref err) != 0)
                {
                    raise(err);
                }
            }
            finally
            {
                Marshal.FreeHGlobal(err.message);
            }
        }

        public byte GetAntiPassback(uint deviceID)
        {
            byte antipassback = 0;
            GoError err = new GoError();

            err.len = 256;
            err.message = Marshal.AllocHGlobal(256);

            try
            { 
                if (GetAntiPassback(ref this.u, deviceID, ref antipassback, ref err) != 0)
                {
                    raise(err);
                }

                return antipassback;
            }
            finally
            {
                Marshal.FreeHGlobal(err.message);
            }
        }

        public void SetAntiPassback(uint deviceID, byte antipassback)
        {
            GoError err = new GoError();

            err.len = 256;
            err.message = Marshal.AllocHGlobal(256);

            try
            { 
                if (SetAntiPassback(ref this.u, deviceID, antipassback, ref err) != 0)
                {
                    raise(err);
                }
            }
            finally
            {
                Marshal.FreeHGlobal(err.message);
            }
        }

        public void RestoreDefaultParameters(uint controller)
        {
            GoError err = new GoError();

            err.len = 256;
            err.message = Marshal.AllocHGlobal(256);

            try
            { 
                if (RestoreDefaultParameters(ref this.u, controller, ref err) != 0)
                {
                    raise(err);
                }
            }
            finally
            {
                Marshal.FreeHGlobal(err.message);
            }
        }

        public delegate void OnEvent(ListenEvent e, IntPtr userdata);
        public delegate void OnError(string err);

        delegate void OnListenEvent([In] GoListenEvent e, IntPtr userdata);
        delegate void OnListenError([In][MarshalAs(UnmanagedType.LPUTF8Str)] string err);

        public void ListenEvents(OnEvent on_event, OnError on_error, ref byte listening, ref byte stop, IntPtr userdata)
        {
            OnListenEvent onevent = ([In] GoListenEvent e, IntPtr userdata) =>
            {
                on_event(new ListenEvent(
                    e.controller,
                    e.timestamp,
                    e.index,
                    e.eventType,
                    e.granted == 1 ? true : false,
                    e.door,
                    e.direction,
                    e.card,
                    e.reason),
                    userdata);
            };

            OnListenError onerror = ([In][MarshalAs(UnmanagedType.LPUTF8Str)] string err) =>
            {
                on_error(err);
            };

            int err = Listen(ref this.u, onevent, ref listening, ref stop, onerror, IntPtr.Zero);
            if (err != 0)
            {
                throw new UhppotedException("error starting event listener");
            }
        }

        private void raise(IntPtr errmsg)
        {
            if (errmsg == IntPtr.Zero)
            {
                throw new UhppotedException("unknown error");
            }

            string? msg = Marshal.PtrToStringAnsi(errmsg);
            if (msg == null)
            {
                throw new UhppotedException("unknown error");
            }

            throw new UhppotedException(msg);
        }

        private void raise(IntPtr errmsg, int N)
        {
            if (errmsg == IntPtr.Zero)
            {
                throw new UhppotedException("unknown error");
            }

            string? msg = Marshal.PtrToStringAnsi(errmsg, N);
            if (msg == null)
            {
                throw new UhppotedException("unknown error");
            }

            throw new UhppotedException(msg);
        }

        private void raise(GoError err)
        {
            if (err.message == IntPtr.Zero)
            {
                throw new UhppotedException("unknown error");
            }

            string? msg = Marshal.PtrToStringAnsi(err.message, err.len);
            if (msg == null)
            {
                throw new UhppotedException("unknown error");
            }

            throw new UhppotedException(msg);
        }

        // Go FFI

        [DllImport(DLL, CallingConvention = CallingConvention.Cdecl, CharSet = CharSet.Ansi)]
        private static extern int GetDevices(ref UHPPOTE u, uint[] list, ref int N, ref GoError err);

        [DllImport(DLL, CallingConvention = CallingConvention.Cdecl, CharSet = CharSet.Ansi)]
        private static extern int GetDevice(ref UHPPOTE u, uint deviceID, ref GoDevice device, ref GoError err);

        [DllImport(DLL, CallingConvention = CallingConvention.Cdecl, CharSet = CharSet.Ansi)]
        private static extern int SetAddress(ref UHPPOTE u, uint deviceID, string address, string subnet, string gateway, ref GoError err);

        [DllImport(DLL, CallingConvention = CallingConvention.Cdecl, CharSet = CharSet.Ansi)]
        private static extern int GetStatus(ref UHPPOTE u, uint deviceID, ref GoStatus status, ref GoError err);

        [DllImport(DLL, CallingConvention = CallingConvention.Cdecl, CharSet = CharSet.Ansi)]
        private static extern int GetTime(ref UHPPOTE u, uint deviceID, IntPtr datetime, ref GoError err);

        [DllImport(DLL, CallingConvention = CallingConvention.Cdecl, CharSet = CharSet.Ansi)]
        private static extern int SetTime(ref UHPPOTE u, uint deviceID, string datetime, ref GoError err);

        [DllImport(DLL, CallingConvention = CallingConvention.Cdecl, CharSet = CharSet.Ansi)]
        private static extern int GetListener(ref UHPPOTE u, uint deviceID, IntPtr listener, ref byte interval, ref GoError err);

        [DllImport(DLL, CallingConvention = CallingConvention.Cdecl, CharSet = CharSet.Ansi)]
        private static extern int SetListener(ref UHPPOTE u, uint deviceID, string listener, byte interval, ref GoError err);

        [DllImport(DLL, CallingConvention = CallingConvention.Cdecl, CharSet = CharSet.Ansi)]
        private static extern int GetDoorControl(ref UHPPOTE u, uint deviceID, byte door, ref GoDoorControl control, ref GoError err);

        [DllImport(DLL, CallingConvention = CallingConvention.Cdecl, CharSet = CharSet.Ansi)]
        private static extern int SetDoorControl(ref UHPPOTE u, uint deviceID, byte door, byte mode, byte delay, ref GoError err);

        [DllImport(DLL, CallingConvention = CallingConvention.Cdecl, CharSet = CharSet.Ansi)]
        private static extern int OpenDoor(ref UHPPOTE u, uint deviceID, byte door, ref GoError err);

        [DllImport(DLL, CallingConvention = CallingConvention.Cdecl, CharSet = CharSet.Ansi)]
        private static extern int GetCards(ref UHPPOTE u, uint deviceID, ref uint N, ref GoError err);

        [DllImport(DLL, CallingConvention = CallingConvention.Cdecl, CharSet = CharSet.Ansi)]
        private static extern int GetCard(ref UHPPOTE u, uint deviceID, uint cardNumber, ref GoCard card, ref GoError err);

        [DllImport(DLL, CallingConvention = CallingConvention.Cdecl, CharSet = CharSet.Ansi)]
        private static extern int GetCardByIndex(ref UHPPOTE u, uint deviceID, uint index, ref GoCard card, ref GoError err);

        [DllImport(DLL, CallingConvention = CallingConvention.Cdecl, CharSet = CharSet.Ansi)]
        private static extern int PutCard(ref UHPPOTE u, uint deviceID, uint cardNumber, string from, string to, byte[] doors, uint PIN, ref GoError err);

        [DllImport(DLL, CallingConvention = CallingConvention.Cdecl, CharSet = CharSet.Ansi)]
        private static extern int DeleteCard(ref UHPPOTE u, uint deviceID, uint cardNumber, ref GoError err);

        [DllImport(DLL, CallingConvention = CallingConvention.Cdecl, CharSet = CharSet.Ansi)]
        private static extern int DeleteCards(ref UHPPOTE u, uint deviceID, ref GoError err);

        [DllImport(DLL, CallingConvention = CallingConvention.Cdecl, CharSet = CharSet.Ansi)]
        private static extern int GetEventIndex(ref UHPPOTE u, uint deviceID, ref uint index, ref GoError err);

        [DllImport(DLL, CallingConvention = CallingConvention.Cdecl, CharSet = CharSet.Ansi)]
        private static extern int SetEventIndex(ref UHPPOTE u, uint deviceID, uint index, ref GoError err);

        [DllImport(DLL, CallingConvention = CallingConvention.Cdecl, CharSet = CharSet.Ansi)]
        private static extern int GetEvent(ref UHPPOTE u, uint deviceID, uint index, ref GoEvent evt, ref GoError err);

        [DllImport(DLL, CallingConvention = CallingConvention.Cdecl, CharSet = CharSet.Ansi)]
        private static extern int RecordSpecialEvents(ref UHPPOTE u, uint deviceID, bool enabled, ref GoError err);

        [DllImport(DLL, CallingConvention = CallingConvention.Cdecl, CharSet = CharSet.Ansi)]
        private static extern int GetTimeProfile(ref UHPPOTE u, uint deviceID, byte profileID, ref GoTimeProfile profile, ref GoError err);

        [DllImport(DLL, CallingConvention = CallingConvention.Cdecl, CharSet = CharSet.Ansi)]
        private static extern int SetTimeProfile(ref UHPPOTE u, uint deviceID, ref GoTimeProfile profile, ref GoError err);

        [DllImport(DLL, CallingConvention = CallingConvention.Cdecl, CharSet = CharSet.Ansi)]
        private static extern int ClearTimeProfiles(ref UHPPOTE u, uint deviceID, ref GoError err);

        [DllImport(DLL, CallingConvention = CallingConvention.Cdecl, CharSet = CharSet.Ansi)]
        private static extern int AddTask(ref UHPPOTE u, uint deviceID, ref GoTask task, ref GoError err);

        [DllImport(DLL, CallingConvention = CallingConvention.Cdecl, CharSet = CharSet.Ansi)]
        private static extern int RefreshTaskList(ref UHPPOTE u, uint deviceID, ref GoError err);

        [DllImport(DLL, CallingConvention = CallingConvention.Cdecl, CharSet = CharSet.Ansi)]
        private static extern int ClearTaskList(ref UHPPOTE u, uint deviceID, ref GoError err);

        [DllImport(DLL, CallingConvention = CallingConvention.Cdecl, CharSet = CharSet.Ansi)]
        private static extern int SetPCControl(ref UHPPOTE u, uint deviceID, bool enabled, ref GoError err);

        [DllImport(DLL, CallingConvention = CallingConvention.Cdecl, CharSet = CharSet.Ansi)]
        private static extern int SetInterlock(ref UHPPOTE u, uint deviceID, byte interlock, ref GoError err);

        [DllImport(DLL, CallingConvention = CallingConvention.Cdecl, CharSet = CharSet.Ansi)]
        private static extern int ActivateKeypads(ref UHPPOTE u, uint deviceID, bool reader1, bool reader2, bool reader3, bool reader4, ref GoError err);

        [DllImport(DLL, CallingConvention = CallingConvention.Cdecl, CharSet = CharSet.Ansi)]
        private static extern int SetDoorPasscodes(ref UHPPOTE u, uint deviceID, byte door, uint passcode1, uint passcode2, uint passcode3, uint passcode4, ref GoError err);

        [DllImport(DLL, CallingConvention = CallingConvention.Cdecl, CharSet = CharSet.Ansi)]
        private static extern int GetAntiPassback(ref UHPPOTE u, uint deviceID, ref byte AntiPassback, ref GoError err);

        [DllImport(DLL, CallingConvention = CallingConvention.Cdecl, CharSet = CharSet.Ansi)]
        private static extern int SetAntiPassback(ref UHPPOTE u, uint deviceID, byte antipassback, ref GoError err);

        [DllImport(DLL, CallingConvention = CallingConvention.Cdecl, CharSet = CharSet.Ansi)]
        private static extern int RestoreDefaultParameters(ref UHPPOTE u, uint controller, ref GoError err);

        [DllImport(DLL, CallingConvention = CallingConvention.Cdecl, CharSet = CharSet.Ansi)]
        private static extern int Listen(ref UHPPOTE u, OnListenEvent handler, ref byte running, ref byte stop, OnListenError errx, IntPtr userdata);

        [StructLayout(LayoutKind.Sequential, CharSet = CharSet.Ansi)]
        struct udevice
        {
            public uint ID;
            public string address;
            public string transport;
        }

        [StructLayout(LayoutKind.Sequential, CharSet = CharSet.Ansi)]
        struct udevices
        {
            public uint N;
            public IntPtr devices; // array of udevice *
        }

        [StructLayout(LayoutKind.Sequential, CharSet = CharSet.Ansi)]
        struct UHPPOTE
        {
            public string bind;
            public string broadcast;
            public string listen;
            public int timeout;    // seconds, defaults to 5 if <= 0
            public IntPtr devices; // udevices * (optional list of non-local controller
                                   // ID + address pairs)
            public bool debug;
        }

        [StructLayout(LayoutKind.Sequential, CharSet = CharSet.Ansi)]
        struct GoError {
            public int len;
            public IntPtr message; // array of char
        }

        [StructLayout(LayoutKind.Sequential, CharSet = CharSet.Ansi)]
        struct GoDevice
        {
            public uint ID;
            public IntPtr address;
            public IntPtr subnet;
            public IntPtr gateway;
            public IntPtr MAC;
            public IntPtr version;
            public IntPtr date;
        }

        [StructLayout(LayoutKind.Sequential, CharSet = CharSet.Ansi)]
        struct GoEvent
        {
            public IntPtr timestamp;
            public uint index;
            public byte eventType;
            public byte granted;
            public byte door;
            public byte direction;
            public uint card;
            public byte reason;
        }

        [StructLayout(LayoutKind.Sequential, CharSet = CharSet.Ansi)]
        struct GoStatus
        {
            public uint ID;
            public IntPtr sysdatetime;
            public IntPtr doors;
            public IntPtr buttons;
            public byte relays;
            public byte inputs;
            public byte syserror;
            public byte info;
            public uint seqno;
            public IntPtr evt;
        }

        [StructLayout(LayoutKind.Sequential, CharSet = CharSet.Ansi)]
        struct GoDoorControl
        {
            public byte control;
            public byte delay;
        }

        [StructLayout(LayoutKind.Sequential, CharSet = CharSet.Ansi)]
        struct GoCard
        {
            public uint cardNumber;
            public IntPtr from;
            public IntPtr to;
            public IntPtr doors;
            public uint PIN;
        }

        [StructLayout(LayoutKind.Sequential, CharSet = CharSet.Ansi)]
        struct GoTimeProfile
        {
            public byte ID;
            public byte linked;
            public IntPtr from;
            public IntPtr to;
            public byte monday;
            public byte tuesday;
            public byte wednesday;
            public byte thursday;
            public byte friday;
            public byte saturday;
            public byte sunday;
            public IntPtr segment1start;
            public IntPtr segment1end;
            public IntPtr segment2start;
            public IntPtr segment2end;
            public IntPtr segment3start;
            public IntPtr segment3end;
        }

        [StructLayout(LayoutKind.Sequential, CharSet = CharSet.Ansi)]
        struct GoTask
        {
            public byte task;
            public byte door;
            public string from;
            public string to;
            public byte monday;
            public byte tuesday;
            public byte wednesday;
            public byte thursday;
            public byte friday;
            public byte saturday;
            public byte sunday;
            public string at;
            public byte cards;
        }

#pragma warning disable 649 // assigned in DLL
        [StructLayout(LayoutKind.Sequential, CharSet = CharSet.Ansi)]
        struct GoListenEvent
        {
            public uint controller;
            [MarshalAs(UnmanagedType.LPUTF8Str)] public string timestamp;
            public uint index;
            public byte eventType;
            public byte granted;
            public byte door;
            public byte direction;
            public uint card;
            public byte reason;
        }
#pragma warning restore 649
    }

    public class Controller
    {
        public uint ID;
        public string address;
        public string transport;

        public Controller(uint ID, string address)
        {
            this.ID = ID;
            this.address = address;
            this.transport = "udp";
        }

        public Controller(uint ID, string address, string transport)
        {
            this.ID = ID;
            this.address = address;
            this.transport = transport;
        }
    }

    public class UhppotedException : Exception
    {
        public UhppotedException(string message) : base(message) { }
    }

    public class Device
    {
        public uint ID;
        public string address;
        public string subnet;
        public string gateway;
        public string MAC;
        public string version;
        public string date;

        public Device(uint ID, string address, string subnet, string gateway, string MAC, string version, string date)
        {
            this.ID = ID;
            this.address = address;
            this.subnet = subnet;
            this.gateway = gateway;
            this.MAC = MAC;
            this.version = version;
            this.date = date;
        }
    }

    public class Event
    {
        public string timestamp;
        public uint index;
        public byte eventType;
        public bool granted;
        public byte door;
        public byte direction;
        public uint card;
        public byte reason;

        public Event(string timestamp, uint index, byte eventType, bool granted,
                 byte door, byte direction, uint card, byte reason)
        {
            this.timestamp = timestamp;
            this.index = index;
            this.eventType = eventType;
            this.granted = granted;
            this.door = door;
            this.direction = direction;
            this.card = card;
            this.reason = reason;
        }
    }

    public class ListenEvent
    {
        public uint controller;
        public string timestamp;
        public uint index;
        public byte eventType;
        public bool granted;
        public byte door;
        public byte direction;
        public uint card;
        public byte reason;

        public ListenEvent(uint controller,
                           string timestamp,
                           uint index,
                           byte eventType,
                           bool granted,
                           byte door,
                           byte direction,
                           uint card,
                           byte reason)
        {
            this.controller = controller;
            this.timestamp = timestamp;
            this.index = index;
            this.eventType = eventType;
            this.granted = granted;
            this.door = door;
            this.direction = direction;
            this.card = card;
            this.reason = reason;
        }
    }

    public class Status
    {
        public uint ID;
        public string sysdatetime;
        public bool[] doors;
        public bool[] buttons;
        public byte relays;
        public byte inputs;
        public byte syserror;
        public byte info;
        public uint seqno;
        public Event evt;

        public Status(uint ID,
                      string sysdatetime,
                      bool[] doors, bool[] buttons, byte relays, byte inputs,
                      byte syserror, byte info, uint seqno,
                      Event evt)
        {
            this.ID = ID;
            this.sysdatetime = sysdatetime;
            this.doors = doors;
            this.buttons = buttons;
            this.relays = relays;
            this.inputs = inputs;
            this.syserror = syserror;
            this.info = info;
            this.seqno = seqno;
            this.evt = evt;
        }
    }

    public class DoorControl
    {
        public byte mode;
        public byte delay;

        public DoorControl(byte mode, byte delay)
        {
            this.mode = mode;
            this.delay = delay;
        }
    }

    public class Card
    {
        public uint cardNumber;
        public string from;
        public string to;
        public byte[] doors;
        public uint PIN;

        public Card(uint cardNumber, string from, string to, byte[] doors, uint PIN)
        {
            this.cardNumber = cardNumber;
            this.from = from;
            this.to = to;
            this.doors = doors;
            this.PIN = PIN;
        }
    }

    public class TimeProfile
    {
        public byte ID;
        public byte linked;
        public string from;
        public string to;
        public bool monday;
        public bool tuesday;
        public bool wednesday;
        public bool thursday;
        public bool friday;
        public bool saturday;
        public bool sunday;
        public string segment1start;
        public string segment1end;
        public string segment2start;
        public string segment2end;
        public string segment3start;
        public string segment3end;

        public TimeProfile(byte ID, byte linked, string from, string to,
                           bool monday, bool tuesday, bool wednesday, bool thursday, bool friday, bool saturday, bool sunday,
                           string segment1start, string segment1end,
                           string segment2start, string segment2end,
                       string segment3start, string segment3end)
        {
            this.ID = ID;
            this.linked = linked;
            this.from = from;
            this.to = to;

            this.monday = monday;
            this.tuesday = tuesday;
            this.wednesday = wednesday;
            this.thursday = thursday;
            this.friday = friday;
            this.saturday = saturday;
            this.sunday = sunday;

            this.segment1start = segment1start;
            this.segment1end = segment1end;
            this.segment2start = segment2start;
            this.segment2end = segment2end;
            this.segment3start = segment3start;
            this.segment3end = segment3end;
        }
    }

    public class Task
    {
        public byte task;
        public byte door;
        public string from;
        public string to;
        public bool monday;
        public bool tuesday;
        public bool wednesday;
        public bool thursday;
        public bool friday;
        public bool saturday;
        public bool sunday;
        public string at;
        public byte cards;

        public Task(byte task, byte door, string from, string to,
                    bool monday, bool tuesday, bool wednesday, bool thursday, bool friday, bool saturday, bool sunday,
                    string at,
                byte cards)
        {
            this.task = task;
            this.door = door;
            this.from = from;
            this.to = to;

            this.monday = monday;
            this.tuesday = tuesday;
            this.wednesday = wednesday;
            this.thursday = thursday;
            this.friday = friday;
            this.saturday = saturday;
            this.sunday = sunday;

            this.at = at;
            this.cards = cards;
        }
    }

    public class lookup
    {
        public const string LOOKUP_MODE = "door.mode";
        public const string LOOKUP_DIRECTION = "event.direction";
        public const string LOOKUP_EVENT_TYPE = "event.type";
        public const string LOOKUP_EVENT_REASON = "event.reason";

        const string ModeNormallyOpen = "normally open";
        const string ModeNormallyClosed = "normally closed";
        const string ModeControlled = "controlled";
        const string ModeUnknown = "unknown";

        const string DirectionIn = "in";
        const string DirectionOut = "out";
        const string DirectionUnknown = "unknown";

        const string EventTypeNone = "none";
        const string EventTypeSwipe = "swipe";
        const string EventTypeDoor = "door";
        const string EventTypeAlarm = "alarm";
        const string EventTypeOverwritten = "overwritten";
        const string EventTypeUnknown = "unknown";

        const string EventReasonNone = "";
        const string EventReasonSwipe = "swipe";
        const string EventReasonSwipeOpen = "swipe open";
        const string EventReasonSwipeClose = "swipe close";
        const string EventReasonDenied = "swipe:denied (system)";
        const string EventReasonNoAccessRights = "no access rights";
        const string EventReasonIncorrectPassword = "incorrect password";
        const string EventReasonAntiPassback = "anti-passback";
        const string EventReasonMoreCards = "more cards";
        const string EventReasonFirstCardOpen = "first card open";
        const string EventReasonDoorIsNormallyClosed = "door is normally closed";
        const string EventReasonInterlock = "interlock";
        const string EventReasonNotInAllowedTimePeriod = "not in allowed time period";
        const string EventReasonInvalidTimezone = "invalid timezone";
        const string EventReasonAccessDenied = "access denied";
        const string EventReasonPushbuttonOk = "pushbutton ok";
        const string EventReasonDoorOpened = "door opened";
        const string EventReasonDoorClosed = "door closed";
        const string EventReasonDoorOpenedSupervisorPassword = "door opened (supervisor password)";
        const string EventReasonControllerPowerOn = "controller power on";
        const string EventReasonControllerReset = "controller reset";
        const string EventReasonPushbuttonInvalidDoorLocked = "pushbutton invalid (door locked)";
        const string EventReasonPushbuttonInvalidOffline = "pushbutton invalid (offline)";
        const string EventReasonPushbuttonInvalidInterlock = "pushbutton invalid (interlock)";
        const string EventReasonPushbuttonInvalidThreat = "pushbutton invalid (threat)";
        const string EventReasonDoorOpenTooLong = "door open too long";
        const string EventReasonForcedOpen = "forced open";
        const string EventReasonFire = "fire";
        const string EventReasonForcedClosed = "forced closed";
        const string EventReasonTheftPrevention = "theft prevention";
        const string EventReasonZone24x7 = "24x7 zone";
        const string EventReasonEmergency = "emergency";
        const string EventReasonRemoteOpenDoor = "remote open door";
        const string EventReasonRemoteOpenDoorUSBReader = "remote open door (USB reader)";
        const string EventReasonUnknown = "unknown";

        static Dictionary<uint, string> LookupMode = new Dictionary<uint, string>() {
        { DoorMode.NormallyOpen, ModeNormallyOpen },
        { DoorMode.NormallyClosed, ModeNormallyClosed },
        { DoorMode.Controlled, ModeControlled },
    };

        static Dictionary<uint, string> LookupDirection = new Dictionary<uint, string>() {
        { Direction.In, DirectionIn },
        { Direction.Out, DirectionOut },
    };

        static Dictionary<uint, string> LookupEventType = new Dictionary<uint, string>() {
        { EventType.None, EventTypeNone },
        { EventType.Swipe, EventTypeSwipe },
        { EventType.Door, EventTypeDoor },
        { EventType.Alarm, EventTypeAlarm },
        { EventType.Overwritten, EventTypeOverwritten },
    };

        static Dictionary<uint, string> LookupEventReason = new Dictionary<uint, string>() {
        { EventReason.None, EventReasonNone },
        { EventReason.Swipe, EventReasonSwipe },
        { EventReason.SwipeOpen, EventReasonSwipeOpen },
        { EventReason.SwipeClose, EventReasonSwipeClose },
        { EventReason.Denied, EventReasonDenied },
        { EventReason.NoAccessRights, EventReasonNoAccessRights },
        { EventReason.IncorrectPassword, EventReasonIncorrectPassword },
        { EventReason.AntiPassback, EventReasonAntiPassback },
        { EventReason.MoreCards, EventReasonMoreCards },
        { EventReason.FirstCardOpen, EventReasonFirstCardOpen },
        { EventReason.DoorIsNormallyClosed, EventReasonDoorIsNormallyClosed },
        { EventReason.Interlock, EventReasonInterlock },
        { EventReason.NotInAllowedTimePeriod, EventReasonNotInAllowedTimePeriod },
        { EventReason.InvalidTimezone, EventReasonInvalidTimezone },
        { EventReason.AccessDenied, EventReasonAccessDenied },
        { EventReason.PushbuttonOk, EventReasonPushbuttonOk },
        { EventReason.DoorOpened, EventReasonDoorOpened },
        { EventReason.DoorClosed, EventReasonDoorClosed },
        { EventReason.DoorOpenedSupervisorPassword, EventReasonDoorOpenedSupervisorPassword },
        { EventReason.ControllerPowerOn, EventReasonControllerPowerOn },
        { EventReason.ControllerReset, EventReasonControllerReset },
        { EventReason.PushbuttonInvalidDoorLocked, EventReasonPushbuttonInvalidDoorLocked },
        { EventReason.PushbuttonInvalidOffline, EventReasonPushbuttonInvalidOffline },
        { EventReason.PushbuttonInvalidInterlock, EventReasonPushbuttonInvalidInterlock },
        { EventReason.PushbuttonInvalidThreat, EventReasonPushbuttonInvalidThreat },
        { EventReason.DoorOpenTooLong, EventReasonDoorOpenTooLong },
        { EventReason.ForcedOpen, EventReasonForcedOpen },
        { EventReason.Fire, EventReasonFire },
        { EventReason.ForcedClosed, EventReasonForcedClosed },
        { EventReason.TheftPrevention, EventReasonTheftPrevention },
        { EventReason.Zone24x7, EventReasonZone24x7 },
        { EventReason.Emergency, EventReasonEmergency },
        { EventReason.RemoteOpenDoor, EventReasonRemoteOpenDoor },
        { EventReason.RemoteOpenDoorUSBReader, EventReasonRemoteOpenDoorUSBReader },
    };

        static Dictionary<string, Dictionary<uint, string>> dictionaries = new Dictionary<string, Dictionary<uint, string>>() {
        { LOOKUP_MODE, LookupMode },
        { LOOKUP_DIRECTION, LookupDirection },
        { LOOKUP_EVENT_TYPE, LookupEventType },
        { LOOKUP_EVENT_REASON, LookupEventReason },
    };

        static Dictionary<string, string> unknown = new Dictionary<string, string>() {
        { LOOKUP_MODE, ModeUnknown },
        { LOOKUP_DIRECTION, DirectionUnknown },
        { LOOKUP_EVENT_TYPE, EventTypeUnknown },
        { LOOKUP_EVENT_REASON, EventReasonUnknown },
    };

        public static string find(string category, uint code, string locale)
        {
            Dictionary<uint, string>? dictionary;
            string? s;

            if (dictionaries.TryGetValue(category, out dictionary))
            {
                if (dictionary.TryGetValue(code, out s))
                {
                    return s;
                }

                return unknown[category];
            }

            return "?";
        }
    }

}
