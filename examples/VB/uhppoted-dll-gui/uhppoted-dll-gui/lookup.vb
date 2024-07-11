Public Class DoorMode
    Public Const NormallyOpen As Byte = 1
    Public Const NormallyClosed As Byte = 2
    Public Const Controlled As Byte = 3
End Class

Public Class Direction
    Public Const [In] As Byte = 1
    Public Const Out As Byte = 2
End Class

Public Class EventType
    Public Const None As Byte = 0
    Public Const Swipe As Byte = 1
    Public Const Door As Byte = 2
    Public Const Alarm As Byte = 3
    Public Const Overwritten As Byte = 255
End Class

Public Class EventReason
    Public Const None As Byte = 0
    Public Const Swipe As Byte = 1
    Public Const SwipeOpen As Byte = 2
    Public Const SwipeClose As Byte = 3
    Public Const Denied As Byte = 5
    Public Const NoAccessRights As Byte = 6
    Public Const IncorrectPassword As Byte = 7
    Public Const AntiPassback As Byte = 8
    Public Const MoreCards As Byte = 9
    Public Const FirstCardOpen As Byte = 10
    Public Const DoorIsNormallyClosed As Byte = 11
    Public Const Interlock As Byte = 12
    Public Const NotInAllowedTimePeriod As Byte = 13
    Public Const InvalidTimezone As Byte = 15
    Public Const AccessDenied As Byte = 18
    Public Const PushbuttonOk As Byte = 20
    Public Const DoorOpened As Byte = 23
    Public Const DoorClosed As Byte = 24
    Public Const DoorOpenedSupervisorPassword As Byte = 25
    Public Const ControllerPowerOn As Byte = 28
    Public Const ControllerReset As Byte = 29
    Public Const PushbuttonInvalidDoorLocked As Byte = 31
    Public Const PushbuttonInvalidOffline As Byte = 32
    Public Const PushbuttonInvalidInterlock As Byte = 33
    Public Const PushbuttonInvalidThreat As Byte = 34
    Public Const DoorOpenTooLong As Byte = 37
    Public Const ForcedOpen As Byte = 38
    Public Const Fire As Byte = 39
    Public Const ForcedClosed As Byte = 40
    Public Const TheftPrevention As Byte = 41
    Public Const Zone24x7 As Byte = 42
    Public Const Emergency As Byte = 43
    Public Const RemoteOpenDoor As Byte = 44
    Public Const RemoteOpenDoorUSBReader As Byte = 45
End Class