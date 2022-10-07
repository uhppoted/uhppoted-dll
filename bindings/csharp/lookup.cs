using System;
using System.Runtime.InteropServices;
using System.Collections.Generic;
using System.Text;

public class DoorMode {
    public const byte NormallyOpen = 1;
    public const byte NormallyClosed = 2;
    public const byte Controlled = 3;
}

public class Direction {
    public const byte In = 1;
    public const byte Out = 2;
}

public class EventType {
    public const byte None = 0;
    public const byte Swipe = 1;
    public const byte Door = 2;
    public const byte Alarm = 3;
    public const byte Overwritten = 0xff;
}

public class EventReason {
    public const byte None = 0;
    public const byte Swipe = 1;
    public const byte SwipeOpen = 2;
    public const byte SwipeClose = 3;
    public const byte Denied = 5;
    public const byte NoAccessRights = 6;
    public const byte IncorrectPassword = 7;
    public const byte AntiPassback = 8;
    public const byte MoreCards = 9;
    public const byte FirstCardOpen = 10;
    public const byte DoorIsNormallyClosed = 11;
    public const byte Interlock = 12;
    public const byte NotInAllowedTimePeriod = 13;
    public const byte InvalidTimezone = 15;
    public const byte AccessDenied = 18;
    public const byte PushButtonOk = 20;
    public const byte DoorOpened = 23;
    public const byte DoorClosed = 24;
    public const byte DoorOpenedSupervisorPassword = 25;
    public const byte ControllerPowerOn = 28;
    public const byte ControllerReset = 29;
    public const byte PushButtonInvalidDoorLocked = 31;
    public const byte PushButtonInvalidOffline = 32;
    public const byte PushButtonInvalidInterlock = 33;
    public const byte PushButtonInvalidThreat = 34;
    public const byte DoorOpenTooLong = 37;
    public const byte ForcedOpen = 38;
    public const byte Fire = 39;
    public const byte ForcedClosed = 40;
    public const byte TheftPrevention = 41;
    public const byte Zone24x7 = 42;
    public const byte Emergency = 43;
    public const byte RemoteOpenDoor = 44;
    public const byte RemoteOpenDoorUSBReader = 45;
}
