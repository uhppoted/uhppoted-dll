using System;
using System.Runtime.InteropServices;
using System.Collections.Generic;
using System.Text;

using static System.Console;
using static System.String;

namespace uhppoted {

    public class Uhppoted : IDisposable {
        private UHPPOTE u = new UHPPOTE();

        public Uhppoted() { 
        }

        public Uhppoted(string bind, string broadcast, string listen, int timeout, Controller[] controllers, bool debug) {
            this.u.bind = bind;
            this.u.broadcast = broadcast;
            this.u.listen = listen;
            this.u.timeout = timeout;
            this.u.devices = IntPtr.Zero;
            this.u.debug = debug;

            uint N = (uint)controllers.Length;
            udevice[] list = new udevice[N];

            for (int ix = 0; ix < controllers.Length; ix++) {
                Controller c = controllers[ix];

                list[ix].ID = c.ID;
                list[ix].address = c.address;
            }

            int sz = Marshal.SizeOf(typeof(udevice));
            IntPtr p = Marshal.AllocHGlobal((int)N * sz);

            for (int ix = 0; ix < list.Length; ix++) {
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

        ~Uhppoted() { 
            dispose(); 
        }

        public void Dispose() {
            dispose();
            GC.SuppressFinalize(this);
        }

        private void dispose() {
            IntPtr p = this.u.devices;

            if (p != IntPtr.Zero) {
                udevices devices = (udevices)Marshal.PtrToStructure(p, typeof(udevices))!;
                IntPtr q = devices.devices;

                Marshal.FreeHGlobal(q);
                Marshal.FreeHGlobal(p);
            }
        }

        public uint[] GetDevices() {
            int N = 0;
            int count = N;
            uint[] slice;

            IntPtr errmsg = Marshal.AllocHGlobal(256);

            try {
                do {
                    N += 16;
                    count = N;
                    slice = new uint[N];
                
                    if (GetDevices(ref this.u, ref count, slice, errmsg) != 0) {
                        raise(errmsg);
                    }
                } while (N < count);

                uint[] list = new uint[count];

                Array.Copy(slice, list, list.Length);

                return list;
            } finally {
                Marshal.FreeHGlobal(errmsg);                
            }
        }

        public Device GetDevice(uint deviceID) {
            IntPtr errmsg = Marshal.AllocHGlobal(256);
            GoDevice device = new GoDevice();

            device.address = Marshal.AllocHGlobal(16);
            device.subnet = Marshal.AllocHGlobal(16);
            device.gateway = Marshal.AllocHGlobal(16);
            device.MAC = Marshal.AllocHGlobal(18);
            device.version = Marshal.AllocHGlobal(5);
            device.date = Marshal.AllocHGlobal(11);

            try {
                if (GetDevice(ref this.u, ref device, deviceID, errmsg) != 0) {
                        raise(errmsg);
                }

                byte[] address = new byte[16];
                byte[] subnet = new byte[16];
                byte[] gateway = new byte[16];
                byte[] MAC = new byte[18];
                byte[] version = new byte[6];
                byte[] date = new byte[11];

                Marshal.Copy(device.address, address, 0, 16);
                Marshal.Copy(device.subnet, subnet, 0, 16);
                Marshal.Copy(device.gateway, gateway, 0, 16);
                Marshal.Copy(device.MAC, MAC, 0, 18);
                Marshal.Copy(device.version, version, 0, 6);
                Marshal.Copy(device.date, date, 0, 11);

                return new Device(
                    device.ID, 
                    System.Text.Encoding.UTF8.GetString(address, 0, address.Length),
                    System.Text.Encoding.UTF8.GetString(subnet, 0, subnet.Length),
                    System.Text.Encoding.UTF8.GetString(gateway, 0, gateway.Length),
                    System.Text.Encoding.UTF8.GetString(MAC, 0, MAC.Length),
                    System.Text.Encoding.UTF8.GetString(version, 0, version.Length),
                    System.Text.Encoding.UTF8.GetString(date, 0, date.Length));

            } finally {
                Marshal.FreeHGlobal(device.address);
                Marshal.FreeHGlobal(device.subnet);
                Marshal.FreeHGlobal(device.gateway);
                Marshal.FreeHGlobal(device.MAC);
                Marshal.FreeHGlobal(device.version);
                Marshal.FreeHGlobal(device.date);

                Marshal.FreeHGlobal(errmsg);                
            }
        }

        public void SetAddress(uint deviceID, string address, string subnet, string gateway) {
            IntPtr errmsg = Marshal.AllocHGlobal(256);

            try {
                if (SetAddress(ref this.u, deviceID, address, subnet, gateway, errmsg) != 0) {
                    raise(errmsg);
                }
            } finally {
                Marshal.FreeHGlobal(errmsg);                
            }
        }

        public Status GetStatus(uint deviceID) {
            IntPtr errmsg = Marshal.AllocHGlobal(256);
            GoStatus status = new GoStatus();

            status.sysdatetime = Marshal.AllocHGlobal(20);
            status.doors = Marshal.AllocHGlobal(4);
            status.buttons = Marshal.AllocHGlobal(4);
            status.eventTimestamp = Marshal.AllocHGlobal(20);

            try {
                if (GetStatus(ref this.u, ref status, deviceID, errmsg) != 0) {
                    raise(errmsg);
                }

                byte[] sysdatetime = new byte[20];
                byte[] doors = new byte[4];
                byte[] buttons = new byte[4];
                byte[] timestamp = new byte[20];

                Marshal.Copy(status.sysdatetime, sysdatetime, 0, 20);
                Marshal.Copy(status.doors, doors, 0, 4);
                Marshal.Copy(status.buttons, buttons, 0, 4);
                Marshal.Copy(status.eventTimestamp, timestamp, 0, 20);

                Event e = new Event(System.Text.Encoding.UTF8.GetString(timestamp, 0, timestamp.Length),
                                    status.eventIndex,
                                    status.eventType,
                                    status.eventGranted != 0,
                                    status.eventDoor,
                                    status.eventDirection,
                                    status.eventCard,
                                    status.eventReason);

                return new Status(status.ID, 
                                  System.Text.Encoding.UTF8.GetString(sysdatetime, 0, sysdatetime.Length),
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
            } finally {
                Marshal.FreeHGlobal(status.sysdatetime);
                Marshal.FreeHGlobal(status.doors);
                Marshal.FreeHGlobal(status.buttons);
                Marshal.FreeHGlobal(status.eventTimestamp);

                Marshal.FreeHGlobal(errmsg);                
            }
        }

        public string GetTime(uint deviceID) {
            IntPtr errmsg = Marshal.AllocHGlobal(256);
            IntPtr time = Marshal.AllocHGlobal(20);

            try {
                if (GetTime(ref this.u, time, deviceID, errmsg) != 0) {
                    raise(errmsg);
                }

                byte[] datetime = new byte[20];

                Marshal.Copy(time, datetime, 0, 20);
                
                return System.Text.Encoding.UTF8.GetString(datetime, 0, datetime.Length);
            } finally {
                Marshal.FreeHGlobal(time);
                Marshal.FreeHGlobal(errmsg);
            }
        }

        public void SetTime(uint deviceID, string datetime) {
            IntPtr errmsg = Marshal.AllocHGlobal(256);

            try {
                if (SetTime(ref this.u, deviceID, datetime, errmsg) != 0) {
                    raise(errmsg);
                }
            } finally {
                Marshal.FreeHGlobal(errmsg);                
            }
        }

        public string GetListener(uint deviceID) {
            IntPtr errmsg = Marshal.AllocHGlobal(256);
            IntPtr addr = Marshal.AllocHGlobal(22);

            try {
                if (GetListener(ref this.u, addr, deviceID, errmsg) != 0) {
                    raise(errmsg);
                }

                byte[] addrport = new byte[22];

                Marshal.Copy(addr, addrport, 0, 22);
                
                return System.Text.Encoding.UTF8.GetString(addrport, 0, addrport.Length);
            } finally {
                Marshal.FreeHGlobal(addr);
                Marshal.FreeHGlobal(errmsg);                
            }
        }

        public void SetListener(uint deviceID, string listener) {
            IntPtr errmsg = Marshal.AllocHGlobal(256);

            try {
                if (SetListener(ref this.u, deviceID, listener, errmsg) != 0) {
                    raise(errmsg);
                }
            } finally {
                Marshal.FreeHGlobal(errmsg);                
            }
        }

        public DoorControl GetDoorControl(uint deviceID, byte door) {
            IntPtr errmsg = Marshal.AllocHGlobal(256);

            try {
                GoDoorControl control = new GoDoorControl();

                if (GetDoorControl(ref this.u, ref control, deviceID, door, errmsg) != 0) {
                    raise(errmsg);
                }

                return new DoorControl(control.control, control.delay);
            } finally {
                Marshal.FreeHGlobal(errmsg);                
            }
        }

        public void SetDoorControl(uint deviceID, byte door, byte mode, byte delay) {
            IntPtr errmsg = Marshal.AllocHGlobal(256);

            try {
                if (SetDoorControl(ref this.u, deviceID, door, mode, delay, errmsg) != 0) {
                    raise(errmsg);
                }
            } finally {
                Marshal.FreeHGlobal(errmsg);                
            }
        }

        public void OpenDoor(uint deviceID, byte door) {
            IntPtr errmsg = Marshal.AllocHGlobal(256);

            try {
                if (OpenDoor(ref this.u, deviceID, door, errmsg) != 0) {
                    raise(errmsg);
                }
            } finally {
                Marshal.FreeHGlobal(errmsg);                
            }
        }

        public uint GetCards(uint deviceID) {
            IntPtr errmsg = Marshal.AllocHGlobal(256);
            uint N = 0;

            try {
                if (GetCards(ref this.u, ref N, deviceID, errmsg) != 0) {
                    raise(errmsg);
                }

                return N;
            } finally {
                Marshal.FreeHGlobal(errmsg);                
            }
        }

        public Card GetCard(uint deviceID, uint cardNumber) {
            IntPtr errmsg = Marshal.AllocHGlobal(256);
            GoCard card = new GoCard();

            card.doors = Marshal.AllocHGlobal(4);

            try {
                if (GetCard(ref this.u, ref card, deviceID, cardNumber, errmsg) != 0) {
                    raise(errmsg);
                }

                byte[] doors = new byte[4];

                Marshal.Copy(card.doors, doors, 0, 4);

                return new Card(card.cardNumber, card.from, card.to, doors, card.PIN);
            } finally {
                Marshal.FreeHGlobal(card.doors);
                Marshal.FreeHGlobal(errmsg);                
            }
        }

        public Card GetCardByIndex(uint deviceID, uint index) {
            IntPtr errmsg = Marshal.AllocHGlobal(256);
            GoCard card = new GoCard();

            card.doors = Marshal.AllocHGlobal(4);

            try {
                if (GetCardByIndex(ref this.u, ref card, deviceID, index, errmsg) != 0) {
                    raise(errmsg);
                }

                byte[] doors = new byte[4];

                Marshal.Copy(card.doors, doors, 0, 4);

                return new Card(card.cardNumber, card.from, card.to, doors, card.PIN);
            } finally {
                Marshal.FreeHGlobal(card.doors);
                Marshal.FreeHGlobal(errmsg);                
            }
        }

        public void PutCard(uint deviceID, uint cardNumber, string from, string to, byte[] doors, uint PIN) {
            IntPtr errmsg = Marshal.AllocHGlobal(256);

            try {
                if (PutCard(ref this.u, deviceID, cardNumber, from, to, doors, PIN, errmsg) != 0) {
                    raise(errmsg);
                }
            } finally {
                Marshal.FreeHGlobal(errmsg);                
            }
        }

        public void DeleteCard(uint deviceID, uint cardNumber) {
            IntPtr errmsg = Marshal.AllocHGlobal(256);

            try {
                if (DeleteCard(ref this.u, deviceID, cardNumber, errmsg) != 0) {
                    raise(errmsg);
                }
            } finally {
                Marshal.FreeHGlobal(errmsg);                
            }
        }

        public void DeleteCards(uint deviceID) {
            IntPtr errmsg = Marshal.AllocHGlobal(256);

            try {
                if (DeleteCards(ref this.u, deviceID, errmsg) != 0) {
                    raise(errmsg);
                }
            } finally {
                Marshal.FreeHGlobal(errmsg);                
            }
        }

        public uint GetEventIndex(uint deviceID) {
            IntPtr errmsg = Marshal.AllocHGlobal(256);
            uint index = 0;

            try {
                if (GetEventIndex(ref this.u, ref index, deviceID, errmsg) != 0) {
                    raise(errmsg);
                }

                return index;
            } finally {
                Marshal.FreeHGlobal(errmsg);                
            }
        }

        public void SetEventIndex(uint deviceID, uint index) {
            IntPtr errmsg = Marshal.AllocHGlobal(256);

            try {
                if (SetEventIndex(ref this.u, deviceID, index, errmsg) != 0) {
                    raise(errmsg);
                }
            } finally {
                Marshal.FreeHGlobal(errmsg);                
            }
        }

        public Event GetEvent(uint deviceID, uint index) {
            IntPtr errmsg = Marshal.AllocHGlobal(256);
            GoEvent evt = new GoEvent();

            try {
                if (GetEvent(ref this.u, ref evt, deviceID, index, errmsg) != 0) {
                     raise(errmsg);
               }

                return new Event(evt.timestamp,
                                 evt.index,
                                 evt.eventType,
                                 evt.granted == 1,
                                 evt.door,
                                 evt.direction,
                                 evt.card,
                                 evt.reason);
            } finally {
                Marshal.FreeHGlobal(errmsg);                
            }
        }

        public void RecordSpecialEvents(uint deviceID, bool enabled) {
            IntPtr errmsg = Marshal.AllocHGlobal(256);

            try {
                if (RecordSpecialEvents(ref this.u, deviceID, enabled, errmsg) != 0) {
                    raise(errmsg);
                }
            } finally {
                Marshal.FreeHGlobal(errmsg);                
            }
        }

        public TimeProfile GetTimeProfile(uint deviceID, byte profileID) {
            IntPtr errmsg = Marshal.AllocHGlobal(256);
            GoTimeProfile profile = new GoTimeProfile();

            try {
                if (GetTimeProfile(ref this.u, ref profile, deviceID, profileID, errmsg) != 0) {
                    raise(errmsg);
                }

                return new TimeProfile(profile.ID,
                                       profile.linked,
                                       profile.from,
                                       profile.to,
                                       profile.monday != 0,
                                       profile.tuesday != 0,
                                       profile.wednesday != 0,
                                       profile.thursday != 0,
                                       profile.friday != 0,
                                       profile.saturday != 0,
                                       profile.sunday != 0,
                                       profile.segment1start, profile.segment1end,
                                       profile.segment2start, profile.segment2end,
                                       profile.segment3start, profile.segment3end);
            } finally {
                Marshal.FreeHGlobal(errmsg);                
            }
        }

        public void SetTimeProfile(uint deviceID, TimeProfile p) {
            IntPtr errmsg = Marshal.AllocHGlobal(256);
            GoTimeProfile profile = new GoTimeProfile();

            profile.ID = p.ID;
            profile.linked = p.linked;
            profile.from = p.from;
            profile.to = p.to;
            profile.monday = p.monday ? (byte)1 : (byte)0;
            profile.tuesday = p.tuesday ? (byte)1 : (byte)0;
            profile.wednesday = p.wednesday ? (byte)1 : (byte)0;
            profile.thursday = p.thursday ? (byte)1 : (byte)0;
            profile.friday = p.friday ? (byte)1 : (byte)0;
            profile.saturday = p.saturday ? (byte)1 : (byte)0;
            profile.sunday = p.sunday ? (byte)1 : (byte)0;
            profile.segment1start = p.segment1start;
            profile.segment1end = p.segment1end;
            profile.segment2start = p.segment2start;
            profile.segment2end = p.segment2end;
            profile.segment3start = p.segment3start;
            profile.segment3end = p.segment3end;

            try {
                if (SetTimeProfile(ref this.u, deviceID, ref profile, errmsg) != 0) {
                    raise(errmsg);
                }
            } finally {
                Marshal.FreeHGlobal(errmsg);                
            }
        }

        public void ClearTimeProfiles(uint deviceID) {
            IntPtr errmsg = Marshal.AllocHGlobal(256);

            try {
                if (ClearTimeProfiles(ref this.u, deviceID, errmsg) != 0) {
                    raise(errmsg);
                }
            } finally {
                Marshal.FreeHGlobal(errmsg);                
            }
        }

        public void AddTask(uint deviceID, Task t) {
            IntPtr errmsg = Marshal.AllocHGlobal(256);
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

            try {
                if (AddTask(ref this.u, deviceID, ref task, errmsg) != 0) {
                    raise(errmsg);
                }
            } finally {
                Marshal.FreeHGlobal(errmsg);                
            }
        }

        public void RefreshTaskList(uint deviceID) {
            IntPtr errmsg = Marshal.AllocHGlobal(256);

            try {
                if (RefreshTaskList(ref this.u, deviceID, errmsg) != 0) {
                    raise(errmsg);
                }
            } finally {
                Marshal.FreeHGlobal(errmsg);                
            }
        }

        public void ClearTaskList(uint deviceID) {
            IntPtr errmsg = Marshal.AllocHGlobal(256);

            try {
                if (ClearTaskList(ref this.u, deviceID, errmsg) != 0) {
                    raise(errmsg);
                }
            } finally {
                Marshal.FreeHGlobal(errmsg);                
            }
        }

        public void SetPCControl(uint controller, bool enabled) {
            IntPtr errmsg = Marshal.AllocHGlobal(256);

            try {
                if (SetPCControl(ref this.u, controller, enabled, errmsg) != 0) {
                    raise(errmsg);
                }
            } finally {
                Marshal.FreeHGlobal(errmsg);                
            }
        }

        public void SetInterlock(uint controller, byte interlock) {
            IntPtr errmsg = Marshal.AllocHGlobal(256);

            try {
                if (SetInterlock(ref this.u, controller, interlock, errmsg) != 0) {
                    raise(errmsg);
                }
            } finally {
                Marshal.FreeHGlobal(errmsg);                
            }
        }

        public void ActivateKeypads(uint controller, bool reader1, bool reader2, bool reader3, bool reader4) {
            IntPtr errmsg = Marshal.AllocHGlobal(256);

            try {
                if (ActivateKeypads(ref this.u, controller, reader1, reader2, reader3, reader4, errmsg) != 0) {
                    raise(errmsg);
                }
            } finally {
                Marshal.FreeHGlobal(errmsg);                
            }
        }

        public void SetDoorPasscodes(uint controller, byte door, uint passcode1, uint passcode2, uint passcode3, uint passcode4) {
            IntPtr errmsg = Marshal.AllocHGlobal(256);

            try {
                if (SetDoorPasscodes(ref this.u, controller, door, passcode1, passcode2, passcode3, passcode4, errmsg) != 0) {
                    raise(errmsg);
                }
            } finally {
                Marshal.FreeHGlobal(errmsg);                
            }
        }

        public void RestoreDefaultParameters(uint controller) {
            IntPtr errmsg = Marshal.AllocHGlobal(256);

            try {
                if (RestoreDefaultParameters(ref this.u, controller, errmsg) != 0) {
                    raise(errmsg);
                }
            } finally {
                Marshal.FreeHGlobal(errmsg);                
            }
        }

        private void raise(IntPtr errmsg) {
            if (errmsg == IntPtr.Zero) {
                throw new UhppotedException("unknown error");
            } 
            
            string? msg = Marshal.PtrToStringAnsi(errmsg);
            if (msg == null){
                throw new UhppotedException("unknown error");
            } 
              
            throw new UhppotedException(msg);                    
        }

        // Go FFI

        [DllImport("uhppoted.dll")]
        private static extern int GetDevices(ref UHPPOTE u, ref int N, uint[] list, IntPtr errmsg);

        [DllImport("uhppoted.dll")]
        private static extern int GetDevice(ref UHPPOTE u, ref GoDevice device, uint deviceID, IntPtr errmsg);

        [DllImport("uhppoted.dll")]
        private static extern int SetAddress(ref UHPPOTE u, uint deviceID, string address, string subnet, string gateway, IntPtr errmsg);

        [DllImport("uhppoted.dll")]
        private static extern int GetStatus(ref UHPPOTE u, ref GoStatus status, uint deviceID, IntPtr err);

        [DllImport("uhppoted.dll")]
        private static extern int GetTime(ref UHPPOTE u, IntPtr datetime, uint deviceID, IntPtr errmsg);

        [DllImport("uhppoted.dll")]
        private static extern int SetTime(ref UHPPOTE u, uint deviceID, string datetime, IntPtr errmsg);

        [DllImport("uhppoted.dll")]
        private static extern int GetListener(ref UHPPOTE u, IntPtr listener, uint deviceID, IntPtr errmsg);

        [DllImport("uhppoted.dll")]
        private static extern int SetListener(ref UHPPOTE u, uint deviceID, string listener, IntPtr errmsg);

        [DllImport("uhppoted.dll")]
        private static extern int GetDoorControl(ref UHPPOTE u, ref GoDoorControl c, uint deviceID, byte door, IntPtr errmsg);

        [DllImport("uhppoted.dll")]
        private static extern int SetDoorControl(ref UHPPOTE u, uint deviceID, byte door, byte mode, byte delay, IntPtr errmsg);

        [DllImport("uhppoted.dll")]
        private static extern int OpenDoor(ref UHPPOTE u, uint deviceID, byte door, IntPtr errmsg);

        [DllImport("uhppoted.dll")]
        private static extern int GetCards(ref UHPPOTE u, ref uint N, uint deviceID, IntPtr errmsg);

        [DllImport("uhppoted.dll")]
        private static extern int GetCard(ref UHPPOTE u, ref GoCard card, uint deviceID, uint cardNumber, IntPtr errmsg);

        [DllImport("uhppoted.dll")]
        private static extern int GetCardByIndex(ref UHPPOTE u, ref GoCard card, uint deviceID, uint index, IntPtr errmsg);

        [DllImport("uhppoted.dll")]
        private static extern int PutCard(ref UHPPOTE u, uint deviceID, uint cardNumber, string from, string to, byte[] doors, uint PIN, IntPtr errmsg);

        [DllImport("uhppoted.dll")]
        private static extern int DeleteCard(ref UHPPOTE u, uint deviceID, uint cardNumber, IntPtr errmsg);

        [DllImport("uhppoted.dll")]
        private static extern int DeleteCards(ref UHPPOTE u, uint deviceID, IntPtr errmsg);

        [DllImport("uhppoted.dll")]
        private static extern int GetEventIndex(ref UHPPOTE u, ref uint index, uint deviceID, IntPtr errmsg);

        [DllImport("uhppoted.dll")]
        private static extern int SetEventIndex(ref UHPPOTE u, uint deviceID, uint index, IntPtr errmsg);

        [DllImport("uhppoted.dll")]
        private static extern int GetEvent(ref UHPPOTE u, ref GoEvent evt, uint deviceID, uint index, IntPtr errmsg);

        [DllImport("uhppoted.dll")]
        private static extern int RecordSpecialEvents(ref UHPPOTE u, uint deviceID, bool enabled, IntPtr errmsg);

        [DllImport("uhppoted.dll")]
        private static extern int GetTimeProfile(ref UHPPOTE u, ref GoTimeProfile profile, uint deviceID, byte profileID, IntPtr errmsg);

        [DllImport("uhppoted.dll")]
        private static extern int SetTimeProfile(ref UHPPOTE u, uint deviceID, ref GoTimeProfile profile, IntPtr errmsg);

        [DllImport("uhppoted.dll")]
        private static extern int ClearTimeProfiles(ref UHPPOTE u, uint deviceID, IntPtr errmsg);

        [DllImport("uhppoted.dll")]
        private static extern int AddTask(ref UHPPOTE u, uint deviceID, ref GoTask task, IntPtr errmsg);

        [DllImport("uhppoted.dll")]
        private static extern int RefreshTaskList(ref UHPPOTE u, uint deviceID, IntPtr errmsg);

        [DllImport("uhppoted.dll")]
        private static extern int ClearTaskList(ref UHPPOTE u, uint deviceID, IntPtr errmsg);

        [DllImport("uhppoted.dll")]
        private static extern int SetPCControl(ref UHPPOTE u, uint deviceID, bool enabled, IntPtr errmsg);

        [DllImport("uhppoted.dll")]
        private static extern int SetInterlock(ref UHPPOTE u, uint deviceID, byte interlock, IntPtr errmsg);

        [DllImport("uhppoted.dll")]
        private static extern int ActivateKeypads(ref UHPPOTE u, uint deviceID, bool reader1, bool reader2, bool reader3, bool reader4, IntPtr errmsg);

        [DllImport("uhppoted.dll")]
        private static extern int SetDoorPasscodes(ref UHPPOTE u, uint deviceID, byte door, uint passcode1, uint passcode2, uint passcode3, uint passcode4, IntPtr errmsg);

        [DllImport("uhppoted.dll")]
        private static extern int RestoreDefaultParameters(ref UHPPOTE u, uint controller, IntPtr errmsg);

    struct udevice {
            public uint ID;
            public string address;
        }

    struct udevices {
            public uint N;
            public IntPtr devices; // array of udevice *
        }

    struct UHPPOTE {
            public string bind;
            public string broadcast;
            public string listen;
            public int timeout;    // seconds, defaults to 5 if <= 0
            public IntPtr devices; // udevices * (optional list of non-local controller
                                   // ID + address pairs)
            public bool debug;
        }

    struct GoDevice {
            public uint ID;
            public IntPtr address;
            public IntPtr subnet;
            public IntPtr gateway;
            public IntPtr MAC;
            public IntPtr version;
            public IntPtr date;
        }

    struct GoEvent {
            public string timestamp;
            public uint index;
            public byte eventType;
            public byte granted;
            public byte door;
            public byte direction;
            public uint card;
            public byte reason;
        }

        struct GoStatus {
            public uint ID;
            public IntPtr sysdatetime;
            public IntPtr doors;
            public IntPtr buttons;
            public byte   relays;
            public byte   inputs;
            public byte   syserror;
            public byte   info;
            public uint   seqno;
            public IntPtr eventTimestamp;
            public uint   eventIndex;
            public byte   eventType;
            public byte   eventGranted;
            public byte   eventDoor;
            public byte   eventDirection;
            public uint   eventCard;
            public byte   eventReason;
      }
    
    struct GoDoorControl {
            public byte control;
            public byte delay;
        }

    struct GoCard {
            public uint cardNumber;
            public string from;
            public string to;
            public IntPtr doors;
            public uint PIN;
        }

    struct GoTimeProfile {
            public byte ID;
            public byte linked;
            public string from;
            public string to;
            public byte monday;
            public byte tuesday;
            public byte wednesday;
            public byte thursday;
            public byte friday;
            public byte saturday;
            public byte sunday;
            public string segment1start;
            public string segment1end;
            public string segment2start;
            public string segment2end;
            public string segment3start;
            public string segment3end;
        }

    struct GoTask {
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
    }

public class Controller {
        public uint ID;
        public string address;

    public Controller(uint ID, string address) {
            this.ID = ID;
            this.address = address;
        }
    }

public class UhppotedException : Exception {
        public UhppotedException(string message) : base(message) { }
    }

public class Device {
        public uint ID;
        public string address;
        public string subnet;
        public string gateway;
        public string MAC;
        public string version;
        public string date;

        public Device(uint ID, string address, string subnet, string gateway,
                  string MAC, string version, string date) {
            this.ID = ID;
            this.address = address;
            this.subnet = subnet;
            this.gateway = gateway;
            this.MAC = MAC;
            this.version = version;
            this.date = date;
        }
    }

public class Event {
        public string timestamp;
        public uint index;
        public byte eventType;
        public bool granted;
        public byte door;
        public byte direction;
        public uint card;
        public byte reason;

        public Event(string timestamp, uint index, byte eventType, bool granted,
                 byte door, byte direction, uint card, byte reason) {
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

    public class Status {
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
                      Event evt) {
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

public class DoorControl {
        public byte mode;
        public byte delay;

    public DoorControl(byte mode, byte delay) {
            this.mode = mode;
            this.delay = delay;
        }
    }

public class Card {
        public uint cardNumber;
        public string from;
        public string to;
        public byte[] doors;
        public uint PIN;

    public Card(uint cardNumber, string from, string to, byte[] doors, uint PIN) {
            this.cardNumber = cardNumber;
            this.from = from;
            this.to = to;
            this.doors = doors;
            this.PIN = PIN;
        }
    }

public class TimeProfile {
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
                       string segment3start, string segment3end) {
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

public class Task {
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
                byte cards) {
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

public class lookup {
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

    public static string find(string category, uint code, string locale) {
        Dictionary<uint, string>? dictionary;
        string? s;

        if (dictionaries.TryGetValue(category, out dictionary)) {
            if (dictionary.TryGetValue(code, out s)) {
                    return s;
                }

                return unknown[category];
            }

            return "?";
        }
    }

}
