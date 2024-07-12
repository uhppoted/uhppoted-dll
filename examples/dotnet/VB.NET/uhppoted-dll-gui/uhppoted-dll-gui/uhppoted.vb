Imports System.Runtime.InteropServices
Imports System.Text

Namespace uhppoted
    Public Class Uhppoted
        Implements IDisposable

        Private u As New UHPPOTE()
        Private disposedValue As Boolean

        Public Sub New()
        End Sub

        Public Sub New(bind As String, broadcast As String, listen As String, timeout As Integer, controllers As Controller(), debug As Boolean)
            u.bind = bind
            u.broadcast = broadcast
            u.listen = listen
            u.timeout = timeout
            u.devices = IntPtr.Zero
            u.debug = debug
            Dim N As UInteger = CUInt(controllers.Length)
            Dim list As Udevice() = New Udevice(N - 1) {}

            For ix As Integer = 0 To controllers.Length - 1
                Dim c As Controller = controllers(ix)
                list(ix).ID = c.ID
                list(ix).address = c.address
            Next

            Dim sz As Integer = Marshal.SizeOf(GetType(Udevice))
            Dim p As IntPtr = Marshal.AllocHGlobal(CInt(N) * sz)

            For ix As Integer = 0 To list.Length - 1
                Dim d As Udevice = list(ix)
                Dim q As IntPtr = p + ix * sz
                Marshal.StructureToPtr(d, q, False)
            Next

            Dim devices As New Udevices With {.N = N, .devices = p}
            Dim r As IntPtr = Marshal.AllocHGlobal(Marshal.SizeOf(devices))
            Marshal.StructureToPtr(devices, r, False)
            u.devices = r
        End Sub

        Public Function GetDevices() As UInteger()
            Dim N As Integer = 0
            Dim count As Integer = N
            Dim slice As UInteger()
            Dim errmsg As IntPtr = Marshal.AllocHGlobal(256)

            Try

                Do
                    N += 16
                    count = N
                    slice = New UInteger(N - 1) {}

                    If GetDevices(Me.u, count, slice, errmsg) <> 0 Then
                        Raise(errmsg)
                    End If
                Loop While N < count

                Dim list As UInteger() = New UInteger(count - 1) {}
                Array.Copy(slice, list, list.Length)
                Return list
            Finally
                Marshal.FreeHGlobal(errmsg)
            End Try
        End Function

        Public Function GetDevice(deviceID As UInteger) As Device
            Dim errmsg = Marshal.AllocHGlobal(256)
            Dim device As New GoDevice With {
                .address = Marshal.AllocHGlobal(16),
                .subnet = Marshal.AllocHGlobal(16),
                .gateway = Marshal.AllocHGlobal(16),
                .MAC = Marshal.AllocHGlobal(18),
                .version = Marshal.AllocHGlobal(5),
                .date = Marshal.AllocHGlobal(11)
            }

            Try
                If GetDevice(u, device, deviceID, errmsg) <> 0 Then
                    Raise(errmsg)
                End If

                Dim ID As UInteger = device.ID
                Dim address = Marshal.PtrToStringAnsi(device.address)
                Dim netmask = Marshal.PtrToStringAnsi(device.subnet)
                Dim gateway = Marshal.PtrToStringAnsi(device.gateway)
                Dim MAC = Marshal.PtrToStringAnsi(device.MAC)
                Dim version = Marshal.PtrToStringAnsi(device.version)
                Dim [date] = Marshal.PtrToStringAnsi(device.[date])

                Return New Device(ID, address, netmask, gateway, MAC, version, [date])
            Finally
                Marshal.FreeHGlobal(device.address)
                Marshal.FreeHGlobal(device.subnet)
                Marshal.FreeHGlobal(device.gateway)
                Marshal.FreeHGlobal(device.MAC)
                Marshal.FreeHGlobal(device.version)
                Marshal.FreeHGlobal(device.[date])

                Marshal.FreeHGlobal(errmsg)
            End Try
        End Function

        Public Sub SetAddress(deviceID As UInteger, address As String, subnet As String, gateway As String)
            Dim errmsg As IntPtr = Marshal.AllocHGlobal(256)

            Try

                If SetAddress(Me.u, deviceID, address, subnet, gateway, errmsg) <> 0 Then
                    Raise(errmsg)
                End If

            Finally
                Marshal.FreeHGlobal(errmsg)
            End Try
        End Sub

        Public Function GetStatus(deviceID As UInteger) As Status
            Dim errmsg As IntPtr = Marshal.AllocHGlobal(256)
            Dim status As New GoStatus With {
                .sysdatetime = Marshal.AllocHGlobal(20),
                .doors = Marshal.AllocHGlobal(4),
                .buttons = Marshal.AllocHGlobal(4),
                .eventTimestamp = Marshal.AllocHGlobal(20)
            }

            Try

                If GetStatus(u, status, deviceID, errmsg) <> 0 Then
                    Raise(errmsg)
                End If

                Dim sysdatetime As Byte() = New Byte(19) {}
                Dim doors As Byte() = New Byte(3) {}
                Dim buttons As Byte() = New Byte(3) {}
                Dim timestamp As Byte() = New Byte(19) {}
                Marshal.Copy(status.sysdatetime, sysdatetime, 0, 20)
                Marshal.Copy(status.doors, doors, 0, 4)
                Marshal.Copy(status.buttons, buttons, 0, 4)
                Marshal.Copy(status.eventTimestamp, timestamp, 0, 20)
                Dim e As New [Event](Encoding.UTF8.GetString(timestamp, 0, timestamp.Length - 1), status.eventIndex, status.eventType, status.eventGranted <> 0, status.eventDoor, status.eventDirection, status.eventCard, status.eventReason)
                Return New Status(status.ID, Encoding.UTF8.GetString(sysdatetime, 0, sysdatetime.Length - 1), New Boolean() {doors(0) = 1, doors(1) = 1, doors(2) = 1, doors(3) = 1}, New Boolean() {buttons(0) = 1, buttons(1) = 1, buttons(2) = 1, buttons(3) = 1}, status.relays, status.inputs, status.syserror, status.info, status.seqno, e)
            Finally
                Marshal.FreeHGlobal(status.sysdatetime)
                Marshal.FreeHGlobal(status.doors)
                Marshal.FreeHGlobal(status.buttons)
                Marshal.FreeHGlobal(status.eventTimestamp)
                Marshal.FreeHGlobal(errmsg)
            End Try
        End Function

        Public Function GetTime(deviceID As UInteger) As String
            Dim errmsg As IntPtr = Marshal.AllocHGlobal(256)
            Dim time As IntPtr = Marshal.AllocHGlobal(20)

            Try

                If GetTime(Me.u, time, deviceID, errmsg) <> 0 Then
                    Raise(errmsg)
                End If

                Dim datetime As Byte() = New Byte(19) {}
                Marshal.Copy(time, datetime, 0, 20)
                Return Encoding.UTF8.GetString(datetime, 0, datetime.Length)
            Finally
                Marshal.FreeHGlobal(time)
                Marshal.FreeHGlobal(errmsg)
            End Try
        End Function

        Public Sub SetTime(deviceID As UInteger, datetime As String)
            Dim errmsg As IntPtr = Marshal.AllocHGlobal(256)

            Try
                If SetTime(Me.u, deviceID, datetime, errmsg) <> 0 Then
                    Raise(errmsg)
                End If
            Finally
                Marshal.FreeHGlobal(errmsg)
            End Try
        End Sub

        Public Function GetListener(deviceID As UInteger) As String
            Dim errmsg As IntPtr = Marshal.AllocHGlobal(256)
            Dim addr As IntPtr = Marshal.AllocHGlobal(22)

            Try
                If GetListener(Me.u, addr, deviceID, errmsg) <> 0 Then
                    Raise(errmsg)
                End If

                Dim addrport As Byte() = New Byte(21) {}
                Marshal.Copy(addr, addrport, 0, 22)
                Return Encoding.UTF8.GetString(addrport, 0, addrport.Length)
            Finally
                Marshal.FreeHGlobal(addr)
                Marshal.FreeHGlobal(errmsg)
            End Try
        End Function

        Public Sub SetListener(deviceID As UInteger, listener As String)
            Dim errmsg As IntPtr = Marshal.AllocHGlobal(256)

            Try
                If SetListener(Me.u, deviceID, listener, errmsg) <> 0 Then
                    Raise(errmsg)
                End If
            Finally
                Marshal.FreeHGlobal(errmsg)
            End Try
        End Sub

        Public Function GetDoorControl(deviceID As UInteger, door As Byte) As DoorControl
            Dim errmsg As IntPtr = Marshal.AllocHGlobal(256)

            Try
                Dim control As New GoDoorControl()
                If GetDoorControl(Me.u, control, deviceID, door, errmsg) <> 0 Then
                    Raise(errmsg)
                End If
                Return New DoorControl(control.control, control.delay)
            Finally
                Marshal.FreeHGlobal(errmsg)
            End Try
        End Function

        Public Sub SetDoorControl(deviceID As UInteger, door As Byte, mode As Byte, delay As Byte)
            Dim errmsg As IntPtr = Marshal.AllocHGlobal(256)

            Try
                If SetDoorControl(Me.u, deviceID, door, mode, delay, errmsg) <> 0 Then
                    Raise(errmsg)
                End If
            Finally
                Marshal.FreeHGlobal(errmsg)
            End Try
        End Sub

        Public Sub OpenDoor(deviceID As UInteger, door As Byte)
            Dim errmsg As IntPtr = Marshal.AllocHGlobal(256)

            Try
                If OpenDoor(Me.u, deviceID, door, errmsg) <> 0 Then
                    Raise(errmsg)
                End If
            Finally
                Marshal.FreeHGlobal(errmsg)
            End Try
        End Sub

        Public Function GetCards(deviceID As UInteger) As UInteger
            Dim errmsg As IntPtr = Marshal.AllocHGlobal(256)
            Dim N As UInteger = 0

            Try
                If GetCards(Me.u, N, deviceID, errmsg) <> 0 Then
                    Raise(errmsg)
                End If

                Return N
            Finally
                Marshal.FreeHGlobal(errmsg)
            End Try
        End Function

        Public Function GetCard(deviceID As UInteger, cardNumber As UInteger) As Card
            Dim errmsg As IntPtr = Marshal.AllocHGlobal(256)
            Dim card As New GoCard With {
                .from = Marshal.AllocHGlobal(11),
                .to = Marshal.AllocHGlobal(11),
                .doors = Marshal.AllocHGlobal(4)
            }

            Try
                If GetCard(Me.u, card, deviceID, cardNumber, errmsg) <> 0 Then
                    Raise(errmsg)
                End If

                Dim from As Byte() = New Byte(10) {}
                Dim [to] As Byte() = New Byte(10) {}
                Dim doors As Byte() = New Byte(3) {}
                Marshal.Copy(card.from, from, 0, 11)
                Marshal.Copy(card.[to], [to], 0, 11)
                Marshal.Copy(card.doors, doors, 0, 4)
                Return New Card(card.cardNumber, Encoding.UTF8.GetString(from, 0, from.Length), Encoding.UTF8.GetString([to], 0, [to].Length), doors, card.PIN)
            Finally
                Marshal.FreeHGlobal(card.from)
                Marshal.FreeHGlobal(card.[to])
                Marshal.FreeHGlobal(card.doors)
                Marshal.FreeHGlobal(errmsg)
            End Try
        End Function

        Public Function GetCardByIndex(deviceID As UInteger, index As UInteger) As Card
            Dim errmsg As IntPtr = Marshal.AllocHGlobal(256)
            Dim card As New GoCard With {
                .from = Marshal.AllocHGlobal(11),
                .to = Marshal.AllocHGlobal(11),
                .doors = Marshal.AllocHGlobal(4)
            }

            Try
                If GetCardByIndex(Me.u, card, deviceID, index, errmsg) <> 0 Then
                    Raise(errmsg)
                End If

                Dim from As Byte() = New Byte(10) {}
                Dim [to] As Byte() = New Byte(10) {}
                Dim doors As Byte() = New Byte(3) {}
                Marshal.Copy(card.from, from, 0, 11)
                Marshal.Copy(card.[to], [to], 0, 11)
                Marshal.Copy(card.doors, doors, 0, 4)
                Return New Card(card.cardNumber, Encoding.UTF8.GetString(from, 0, from.Length), Encoding.UTF8.GetString([to], 0, [to].Length), doors, card.PIN)
            Finally
                Marshal.FreeHGlobal(card.from)
                Marshal.FreeHGlobal(card.[to])
                Marshal.FreeHGlobal(card.doors)
                Marshal.FreeHGlobal(errmsg)
            End Try
        End Function

        Public Sub PutCard(deviceID As UInteger, cardNumber As UInteger, from As String, [to] As String, doors As Byte(), PIN As UInteger)
            Dim errmsg = Marshal.AllocHGlobal(256)

            Try
                If PutCard(Me.u, deviceID, cardNumber, from, [to], doors, PIN, errmsg) <> 0 Then
                    Raise(errmsg)
                End If
            Finally
                Marshal.FreeHGlobal(errmsg)
            End Try
        End Sub

        Public Sub DeleteCard(deviceID As UInteger, cardNumber As UInteger)
            Dim errmsg As IntPtr = Marshal.AllocHGlobal(256)

            Try
                If DeleteCard(Me.u, deviceID, cardNumber, errmsg) <> 0 Then
                    Raise(errmsg)
                End If
            Finally
                Marshal.FreeHGlobal(errmsg)
            End Try
        End Sub

        Public Sub DeleteCards(deviceID As UInteger)
            Dim errmsg As IntPtr = Marshal.AllocHGlobal(256)

            Try
                If DeleteCards(Me.u, deviceID, errmsg) <> 0 Then
                    Raise(errmsg)
                End If

            Finally
                Marshal.FreeHGlobal(errmsg)
            End Try
        End Sub

        Public Function GetEventIndex(deviceID As UInteger) As UInteger
            Dim errmsg As IntPtr = Marshal.AllocHGlobal(256)
            Dim index As UInteger = 0

            Try
                If GetEventIndex(Me.u, index, deviceID, errmsg) <> 0 Then
                    Raise(errmsg)
                End If

                Return index
            Finally
                Marshal.FreeHGlobal(errmsg)
            End Try
        End Function

        Public Sub SetEventIndex(deviceID As UInteger, index As UInteger)
            Dim errmsg As IntPtr = Marshal.AllocHGlobal(256)

            Try
                If SetEventIndex(Me.u, deviceID, index, errmsg) <> 0 Then
                    Raise(errmsg)
                End If

            Finally
                Marshal.FreeHGlobal(errmsg)
            End Try
        End Sub

        Public Function GetEvent(deviceID As UInteger, index As UInteger) As [Event]
            Dim errmsg As IntPtr = Marshal.AllocHGlobal(256)
            Dim evt As New GoEvent With {.timestamp = Marshal.AllocHGlobal(20)}

            Try
                If GetEvent(Me.u, evt, deviceID, index, errmsg) <> 0 Then
                    Raise(errmsg)
                End If

                Dim timestamp As Byte() = New Byte(19) {}
                Marshal.Copy(evt.timestamp, timestamp, 0, 20)
                Return New [Event](Encoding.UTF8.GetString(timestamp, 0, timestamp.Length), evt.index, evt.eventType, evt.granted = 1, evt.door, evt.direction, evt.card, evt.reason)
            Finally
                Marshal.FreeHGlobal(evt.timestamp)
                Marshal.FreeHGlobal(errmsg)
            End Try
        End Function

        Public Sub RecordSpecialEvents(deviceID As UInteger, enabled As Boolean)
            Dim errmsg As IntPtr = Marshal.AllocHGlobal(256)

            Try
                If RecordSpecialEvents(Me.u, deviceID, enabled, errmsg) <> 0 Then
                    Raise(errmsg)
                End If

            Finally
                Marshal.FreeHGlobal(errmsg)
            End Try
        End Sub

        Public Function GetTimeProfile(deviceID As UInteger, profileID As Byte) As TimeProfile
            Dim errmsg As IntPtr = Marshal.AllocHGlobal(256)
            Dim profile As New GoGetTimeProfile With {
                .from = Marshal.AllocHGlobal(11),
                .to = Marshal.AllocHGlobal(11),
                .segment1start = Marshal.AllocHGlobal(6),
                .segment1end = Marshal.AllocHGlobal(6),
                .segment2start = Marshal.AllocHGlobal(6),
                .segment2end = Marshal.AllocHGlobal(6),
                .segment3start = Marshal.AllocHGlobal(6),
                .segment3end = Marshal.AllocHGlobal(6)
            }

            Try
                If GetTimeProfile(Me.u, profile, deviceID, profileID, errmsg) <> 0 Then
                    Raise(errmsg)
                End If

                Dim from As Byte() = New Byte(10) {}
                Dim [to] As Byte() = New Byte(10) {}
                Dim segment1start As Byte() = New Byte(5) {}
                Dim segment1end As Byte() = New Byte(5) {}
                Dim segment2start As Byte() = New Byte(5) {}
                Dim segment2end As Byte() = New Byte(5) {}
                Dim segment3start As Byte() = New Byte(5) {}
                Dim segment3end As Byte() = New Byte(5) {}
                Marshal.Copy(profile.from, from, 0, 11)
                Marshal.Copy(profile.[to], [to], 0, 11)
                Marshal.Copy(profile.segment1start, segment1start, 0, 6)
                Marshal.Copy(profile.segment1end, segment1end, 0, 6)
                Marshal.Copy(profile.segment2start, segment2start, 0, 6)
                Marshal.Copy(profile.segment2end, segment2end, 0, 6)
                Marshal.Copy(profile.segment3start, segment3start, 0, 6)
                Marshal.Copy(profile.segment3end, segment3end, 0, 6)
                Return New TimeProfile(profile.ID, profile.linked, Encoding.UTF8.GetString(from, 0, from.Length), Encoding.UTF8.GetString([to], 0, [to].Length), profile.monday <> 0, profile.tuesday <> 0, profile.wednesday <> 0, profile.thursday <> 0, profile.friday <> 0, profile.saturday <> 0, profile.sunday <> 0, Encoding.UTF8.GetString(segment1start, 0, segment1start.Length), Encoding.UTF8.GetString(segment1end, 0, segment1end.Length), Encoding.UTF8.GetString(segment2start, 0, segment2start.Length), Encoding.UTF8.GetString(segment2end, 0, segment2end.Length), Encoding.UTF8.GetString(segment3start, 0, segment3start.Length), Encoding.UTF8.GetString(segment3end, 0, segment3end.Length))
            Finally
                Marshal.FreeHGlobal(profile.from)
                Marshal.FreeHGlobal(profile.[to])
                Marshal.FreeHGlobal(profile.segment1start)
                Marshal.FreeHGlobal(profile.segment1end)
                Marshal.FreeHGlobal(profile.segment2start)
                Marshal.FreeHGlobal(profile.segment2end)
                Marshal.FreeHGlobal(profile.segment3start)
                Marshal.FreeHGlobal(profile.segment3end)
                Marshal.FreeHGlobal(errmsg)
            End Try
        End Function

        Public Sub SetTimeProfile(deviceID As UInteger, p As TimeProfile)
            Dim errmsg As IntPtr = Marshal.AllocHGlobal(256)
            Dim profile As New GoSetTimeProfile With {
                .ID = p.ID,
                .linked = p.linked,
                .from = p.from,
                .to = p.[to],
                .monday = If(p.monday, CByte(1), CByte(0)),
                .tuesday = If(p.tuesday, CByte(1), CByte(0)),
                .wednesday = If(p.wednesday, CByte(1), CByte(0)),
                .thursday = If(p.thursday, CByte(1), CByte(0)),
                .friday = If(p.friday, CByte(1), CByte(0)),
                .saturday = If(p.saturday, CByte(1), CByte(0)),
                .sunday = If(p.sunday, CByte(1), CByte(0)),
                .segment1start = p.segment1start,
                .segment1end = p.segment1end,
                .segment2start = p.segment2start,
                .segment2end = p.segment2end,
                .segment3start = p.segment3start,
                .segment3end = p.segment3end
            }

            Try
                If SetTimeProfile(Me.u, deviceID, profile, errmsg) <> 0 Then
                    Raise(errmsg)
                End If
            Finally
                Marshal.FreeHGlobal(errmsg)
            End Try
        End Sub

        Public Sub ClearTimeProfiles(deviceID As UInteger)
            Dim errmsg As IntPtr = Marshal.AllocHGlobal(256)

            Try
                If ClearTimeProfiles(Me.u, deviceID, errmsg) <> 0 Then
                    Raise(errmsg)
                End If
            Finally
                Marshal.FreeHGlobal(errmsg)
            End Try
        End Sub

        Public Sub AddTask(deviceID As UInteger, t As Task)
            Dim errmsg As IntPtr = Marshal.AllocHGlobal(256)
            Dim task As New GoTask With {
                .task = t.task,
                .door = t.door,
                .from = t.from,
                .to = t.[to],
                .monday = If(t.monday, CByte(1), CByte(0)),
                .tuesday = If(t.tuesday, CByte(1), CByte(0)),
                .wednesday = If(t.wednesday, CByte(1), CByte(0)),
                .thursday = If(t.thursday, CByte(1), CByte(0)),
                .friday = If(t.friday, CByte(1), CByte(0)),
                .saturday = If(t.saturday, CByte(1), CByte(0)),
                .sunday = If(t.sunday, CByte(1), CByte(0)),
                .at = t.at,
                .cards = t.cards
            }

            Try
                If AddTask(u, deviceID, task, errmsg) <> 0 Then
                    Raise(errmsg)
                End If
            Finally
                Marshal.FreeHGlobal(errmsg)
            End Try
        End Sub

        Public Sub RefreshTaskList(deviceID As UInteger)
            Dim errmsg As IntPtr = Marshal.AllocHGlobal(256)

            Try
                If RefreshTaskList(u, deviceID, errmsg) <> 0 Then
                    Raise(errmsg)
                End If
            Finally
                Marshal.FreeHGlobal(errmsg)
            End Try
        End Sub

        Public Sub ClearTaskList(deviceID As UInteger)
            Dim errmsg As IntPtr = Marshal.AllocHGlobal(256)

            Try
                If ClearTaskList(u, deviceID, errmsg) <> 0 Then
                    Raise(errmsg)
                End If
            Finally
                Marshal.FreeHGlobal(errmsg)
            End Try
        End Sub

        Public Sub SetPCControl(controller As UInteger, enabled As Boolean)
            Dim errmsg As IntPtr = Marshal.AllocHGlobal(256)

            Try
                If SetPCControl(Me.u, controller, enabled, errmsg) <> 0 Then
                    Raise(errmsg)
                End If
            Finally
                Marshal.FreeHGlobal(errmsg)
            End Try
        End Sub

        Public Sub SetInterlock(controller As UInteger, interlock As Byte)
            Dim errmsg As IntPtr = Marshal.AllocHGlobal(256)

            Try
                If SetInterlock(Me.u, controller, interlock, errmsg) <> 0 Then
                    Raise(errmsg)
                End If
            Finally
                Marshal.FreeHGlobal(errmsg)
            End Try
        End Sub

        Public Sub ActivateKeypads(controller As UInteger, reader1 As Boolean, reader2 As Boolean, reader3 As Boolean, reader4 As Boolean)
            Dim errmsg As IntPtr = Marshal.AllocHGlobal(256)

            Try
                If ActivateKeypads(Me.u, controller, reader1, reader2, reader3, reader4, errmsg) <> 0 Then
                    Raise(errmsg)
                End If
            Finally
                Marshal.FreeHGlobal(errmsg)
            End Try
        End Sub

        Public Sub SetDoorPasscodes(controller As UInteger, door As Byte, passcode1 As UInteger, passcode2 As UInteger, passcode3 As UInteger, passcode4 As UInteger)
            Dim errmsg As IntPtr = Marshal.AllocHGlobal(256)

            Try
                If SetDoorPasscodes(Me.u, controller, door, passcode1, passcode2, passcode3, passcode4, errmsg) <> 0 Then
                    Raise(errmsg)
                End If
            Finally
                Marshal.FreeHGlobal(errmsg)
            End Try
        End Sub

        Public Sub RestoreDefaultParameters(controller As UInteger)
            Dim errmsg As IntPtr = Marshal.AllocHGlobal(256)

            Try
                If RestoreDefaultParameters(u, controller, errmsg) <> 0 Then
                    Raise(errmsg)
                End If
            Finally
                Marshal.FreeHGlobal(errmsg)
            End Try
        End Sub

        Public Delegate Sub OnEvent(e As ListenEvent)
        Public Delegate Sub OnError(err As String)

        Private Delegate Sub OnListenEvent(<[In]> ByRef evt As GoListenEvent)
        Private Delegate Sub OnListenError(<[In]> <MarshalAs(UnmanagedType.LPUTF8Str)> err As String)

        Public Sub ListenEvents(on_event As OnEvent, on_error As OnError, ByRef running As Byte, ByRef [stop] As Byte)
            Dim onevent As OnListenEvent = Sub(ByRef e) on_event(
                New ListenEvent(e.controller, e.timestamp, e.index, e.eventType, e.granted = 1,
                                e.door, e.direction, e.card, e.reason))

            Dim onerror As OnListenError = Sub(err1) on_error(err1)

            Dim err As Integer = Listen(Me.u, onevent, running, [stop], onerror)
            If err <> 0 Then
                Throw New UhppotedException("error listening for events")
            End If
        End Sub

        Private Shared Sub Raise(errmsg As IntPtr)
            If errmsg = IntPtr.Zero Then
                Throw New UhppotedException("unknown error")
            End If

            Dim msg As String = Marshal.PtrToStringAnsi(errmsg)

            If msg Is Nothing Then
                Throw New UhppotedException("unknown error")
            End If

            Throw New UhppotedException(msg)
        End Sub

#Disable Warning CA2101
        <DllImport("uhppoted.dll")>
        Private Shared Function GetDevices(ByRef u As UHPPOTE, ByRef N As Integer, list As UInteger(), errmsg As IntPtr) As Integer
        End Function
        <DllImport("uhppoted.dll")>
        Private Shared Function GetDevice(ByRef u As UHPPOTE, ByRef device As GoDevice, deviceID As UInteger, errmsg As IntPtr) As Integer
        End Function
        <DllImport("uhppoted.dll")>
        Private Shared Function SetAddress(ByRef u As UHPPOTE, deviceID As UInteger, address As String, subnet As String, gateway As String, errmsg As IntPtr) As Integer
        End Function
        <DllImport("uhppoted.dll")>
        Private Shared Function GetStatus(ByRef u As UHPPOTE, ByRef status As GoStatus, deviceID As UInteger, err As IntPtr) As Integer
        End Function
        <DllImport("uhppoted.dll")>
        Private Shared Function GetTime(ByRef u As UHPPOTE, datetime As IntPtr, deviceID As UInteger, errmsg As IntPtr) As Integer
        End Function
        <DllImport("uhppoted.dll")>
        Private Shared Function SetTime(ByRef u As UHPPOTE, deviceID As UInteger, datetime As String, errmsg As IntPtr) As Integer
        End Function
        <DllImport("uhppoted.dll")>
        Private Shared Function GetListener(ByRef u As UHPPOTE, listener As IntPtr, deviceID As UInteger, errmsg As IntPtr) As Integer
        End Function
        <DllImport("uhppoted.dll")>
        Private Shared Function SetListener(ByRef u As UHPPOTE, deviceID As UInteger, listener As String, errmsg As IntPtr) As Integer
        End Function
        <DllImport("uhppoted.dll")>
        Private Shared Function GetDoorControl(ByRef u As UHPPOTE, ByRef c As GoDoorControl, deviceID As UInteger, door As Byte, errmsg As IntPtr) As Integer
        End Function
        <DllImport("uhppoted.dll")>
        Private Shared Function SetDoorControl(ByRef u As UHPPOTE, deviceID As UInteger, door As Byte, mode As Byte, delay As Byte, errmsg As IntPtr) As Integer
        End Function
        <DllImport("uhppoted.dll")>
        Private Shared Function OpenDoor(ByRef u As UHPPOTE, deviceID As UInteger, door As Byte, errmsg As IntPtr) As Integer
        End Function
        <DllImport("uhppoted.dll")>
        Private Shared Function GetCards(ByRef u As UHPPOTE, ByRef N As UInteger, deviceID As UInteger, errmsg As IntPtr) As Integer
        End Function
        <DllImport("uhppoted.dll")>
        Private Shared Function GetCard(ByRef u As UHPPOTE, ByRef card As GoCard, deviceID As UInteger, cardNumber As UInteger, errmsg As IntPtr) As Integer
        End Function
        <DllImport("uhppoted.dll")>
        Private Shared Function GetCardByIndex(ByRef u As UHPPOTE, ByRef card As GoCard, deviceID As UInteger, index As UInteger, errmsg As IntPtr) As Integer
        End Function
        <DllImport("uhppoted.dll")>
        Private Shared Function PutCard(ByRef u As UHPPOTE, deviceID As UInteger, cardNumber As UInteger, from As String, [to] As String, doors As Byte(), PIN As UInteger, errmsg As IntPtr) As Integer
        End Function
        <DllImport("uhppoted.dll")>
        Private Shared Function DeleteCard(ByRef u As UHPPOTE, deviceID As UInteger, cardNumber As UInteger, errmsg As IntPtr) As Integer
        End Function
        <DllImport("uhppoted.dll")>
        Private Shared Function DeleteCards(ByRef u As UHPPOTE, deviceID As UInteger, errmsg As IntPtr) As Integer
        End Function
        <DllImport("uhppoted.dll")>
        Private Shared Function GetEventIndex(ByRef u As UHPPOTE, ByRef index As UInteger, deviceID As UInteger, errmsg As IntPtr) As Integer
        End Function
        <DllImport("uhppoted.dll")>
        Private Shared Function SetEventIndex(ByRef u As UHPPOTE, deviceID As UInteger, index As UInteger, errmsg As IntPtr) As Integer
        End Function
        <DllImport("uhppoted.dll")>
        Private Shared Function GetEvent(ByRef u As UHPPOTE, ByRef evt As GoEvent, deviceID As UInteger, index As UInteger, errmsg As IntPtr) As Integer
        End Function
        <DllImport("uhppoted.dll")>
        Private Shared Function RecordSpecialEvents(ByRef u As UHPPOTE, deviceID As UInteger, enabled As Boolean, errmsg As IntPtr) As Integer
        End Function
        <DllImport("uhppoted.dll")>
        Private Shared Function GetTimeProfile(ByRef u As UHPPOTE, ByRef profile As GoGetTimeProfile, deviceID As UInteger, profileID As Byte, errmsg As IntPtr) As Integer
        End Function
        <DllImport("uhppoted.dll")>
        Private Shared Function SetTimeProfile(ByRef u As UHPPOTE, deviceID As UInteger, ByRef profile As GoSetTimeProfile, errmsg As IntPtr) As Integer
        End Function
        <DllImport("uhppoted.dll")>
        Private Shared Function ClearTimeProfiles(ByRef u As UHPPOTE, deviceID As UInteger, errmsg As IntPtr) As Integer
        End Function
        <DllImport("uhppoted.dll")>
        Private Shared Function AddTask(ByRef u As UHPPOTE, deviceID As UInteger, ByRef task As GoTask, errmsg As IntPtr) As Integer
        End Function
        <DllImport("uhppoted.dll")>
        Private Shared Function RefreshTaskList(ByRef u As UHPPOTE, deviceID As UInteger, errmsg As IntPtr) As Integer
        End Function
        <DllImport("uhppoted.dll")>
        Private Shared Function ClearTaskList(ByRef u As UHPPOTE, deviceID As UInteger, errmsg As IntPtr) As Integer
        End Function
        <DllImport("uhppoted.dll")>
        Private Shared Function SetPCControl(ByRef u As UHPPOTE, deviceID As UInteger, enabled As Boolean, errmsg As IntPtr) As Integer
        End Function
        <DllImport("uhppoted.dll")>
        Private Shared Function SetInterlock(ByRef u As UHPPOTE, deviceID As UInteger, interlock As Byte, errmsg As IntPtr) As Integer
        End Function
        <DllImport("uhppoted.dll")>
        Private Shared Function ActivateKeypads(ByRef u As UHPPOTE, deviceID As UInteger, reader1 As Boolean, reader2 As Boolean, reader3 As Boolean, reader4 As Boolean, errmsg As IntPtr) As Integer
        End Function
        <DllImport("uhppoted.dll")>
        Private Shared Function SetDoorPasscodes(ByRef u As UHPPOTE, deviceID As UInteger, door As Byte, passcode1 As UInteger, passcode2 As UInteger, passcode3 As UInteger, passcode4 As UInteger, errmsg As IntPtr) As Integer
        End Function
        <DllImport("uhppoted.dll")>
        Private Shared Function RestoreDefaultParameters(ByRef u As UHPPOTE, controller As UInteger, errmsg As IntPtr) As Integer
        End Function
        <DllImport("uhppoted.dll")>
        Private Shared Function Listen(ByRef u As UHPPOTE, handler As OnListenEvent, ByRef running As Byte, ByRef [stop] As Byte, errx As OnListenError) As Integer
        End Function
#Enable Warning CA2101

        Structure Udevice
            Public ID As UInteger
            Public address As String
        End Structure

        Structure Udevices
            Public N As UInteger
            Public devices As IntPtr
        End Structure

        Structure UHPPOTE
            Public bind As String
            Public broadcast As String
            Public listen As String
            Public timeout As Integer
            Public devices As IntPtr
            Public debug As Boolean
        End Structure

        <StructLayout(LayoutKind.Sequential, CharSet:=CharSet.Ansi)>
        Friend Structure GoDevice
            Public ID As UInteger
            Public address As IntPtr
            Public subnet As IntPtr
            Public gateway As IntPtr
            Public MAC As IntPtr
            Public version As IntPtr
            Public [date] As IntPtr
        End Structure

        Structure GoEvent
            Public timestamp As IntPtr
            Public index As UInteger
            Public eventType As Byte
            Public granted As Byte
            Public door As Byte
            Public direction As Byte
            Public card As UInteger
            Public reason As Byte
        End Structure

        Structure GoStatus
            Public ID As UInteger
            Public sysdatetime As IntPtr
            Public doors As IntPtr
            Public buttons As IntPtr
            Public relays As Byte
            Public inputs As Byte
            Public syserror As Byte
            Public info As Byte
            Public seqno As UInteger
            Public eventTimestamp As IntPtr
            Public eventIndex As UInteger
            Public eventType As Byte
            Public eventGranted As Byte
            Public eventDoor As Byte
            Public eventDirection As Byte
            Public eventCard As UInteger
            Public eventReason As Byte
        End Structure

        Structure GoDoorControl
            Public control As Byte
            Public delay As Byte
        End Structure

        Structure GoCard
            Public cardNumber As UInteger
            Public [from] As IntPtr
            Public [to] As IntPtr
            Public doors As IntPtr
            Public PIN As UInteger
        End Structure

        Structure GoGetTimeProfile
            Public ID As Byte
            Public linked As Byte
            Public from As IntPtr
            Public [to] As IntPtr
            Public monday As Byte
            Public tuesday As Byte
            Public wednesday As Byte
            Public thursday As Byte
            Public friday As Byte
            Public saturday As Byte
            Public sunday As Byte
            Public segment1start As IntPtr
            Public segment1end As IntPtr
            Public segment2start As IntPtr
            Public segment2end As IntPtr
            Public segment3start As IntPtr
            Public segment3end As IntPtr
        End Structure

        Structure GoSetTimeProfile
            Public ID As Byte
            Public linked As Byte
            Public from As String
            Public [to] As String
            Public monday As Byte
            Public tuesday As Byte
            Public wednesday As Byte
            Public thursday As Byte
            Public friday As Byte
            Public saturday As Byte
            Public sunday As Byte
            Public segment1start As String
            Public segment1end As String
            Public segment2start As String
            Public segment2end As String
            Public segment3start As String
            Public segment3end As String
        End Structure

        Structure GoTask
            Public task As Byte
            Public door As Byte
            Public from As String
            Public [to] As String
            Public monday As Byte
            Public tuesday As Byte
            Public wednesday As Byte
            Public thursday As Byte
            Public friday As Byte
            Public saturday As Byte
            Public sunday As Byte
            Public at As String
            Public cards As Byte
        End Structure

        Friend Structure GoListenEvent
            Public controller As UInteger
            <MarshalAs(UnmanagedType.LPUTF8Str)>
            Public timestamp As String
            Public index As UInteger
            Public eventType As Byte
            Public granted As Byte
            Public door As Byte
            Public direction As Byte
            Public card As UInteger
            Public reason As Byte
        End Structure

        Protected Overridable Sub Dispose(disposing As Boolean)
            If Not disposedValue Then
                If disposing Then

                    Dim p As IntPtr = Me.u.devices

                    If p <> IntPtr.Zero Then
                        Dim devices As Udevices = CType(Marshal.PtrToStructure(p, GetType(Udevices)), Udevices)
                        Dim q As IntPtr = devices.devices
                        Marshal.FreeHGlobal(q)
                        Marshal.FreeHGlobal(p)
                    End If
                End If

                disposedValue = True
            End If
        End Sub

        Public Sub Dispose() Implements IDisposable.Dispose
            ' Do not change this code. Put cleanup code in 'Dispose(disposing As Boolean)' method
            Dispose(disposing:=True)
            GC.SuppressFinalize(Me)
        End Sub
    End Class

    Public Class Controller
        Public ID As UInteger
        Public address As String

        Public Sub New(ID As UInteger, address As String)
            Me.ID = ID
            Me.address = address
        End Sub
    End Class

    Public Class UhppotedException
        Inherits Exception

        Public Sub New(message As String)
            MyBase.New(message)
        End Sub
    End Class

    Public Class Device
        Public ID As UInteger
        Public address As String
        Public subnet As String
        Public gateway As String
        Public MAC As String
        Public version As String
        Public [date] As String

        Public Sub New(ID As UInteger, address As String, subnet As String, gateway As String, MAC As String, version As String, [date] As String)
            Me.ID = ID
            Me.address = address
            Me.subnet = subnet
            Me.gateway = gateway
            Me.MAC = MAC
            Me.version = version
            Me.date = [date]
        End Sub

        Friend Sub New(ByRef device As Uhppoted.GoDevice)
            Dim address = New Byte(15) {}
            Dim subnet = New Byte(15) {}
            Dim gateway = New Byte(15) {}
            Dim MAC = New Byte(17) {}
            Dim version = New Byte(5) {}
            Dim [date] = New Byte(10) {}

            Marshal.Copy(device.address, address, 0, 16)
            Marshal.Copy(device.subnet, subnet, 0, 16)
            Marshal.Copy(device.gateway, gateway, 0, 16)
            Marshal.Copy(device.MAC, MAC, 0, 18)
            Marshal.Copy(device.version, version, 0, 6)
            Marshal.Copy(device.Date, [date], 0, 11)

            ID = device.ID
            Me.address = Encoding.UTF8.GetString(address, 0, address.Length - 1)
            Me.subnet = Encoding.UTF8.GetString(subnet, 0, subnet.Length - 1)
            Me.gateway = Encoding.UTF8.GetString(gateway, 0, gateway.Length - 1)
            Me.MAC = Encoding.UTF8.GetString(MAC, 0, MAC.Length - 1)
            Me.version = Encoding.UTF8.GetString(version, 0, version.Length - 1)
            Me.date = Encoding.UTF8.GetString([date], 0, [date].Length - 1)
        End Sub
    End Class

    Public Class [Event]
        Public timestamp As String
        Public index As UInteger
        Public eventType As Byte
        Public granted As Boolean
        Public door As Byte
        Public direction As Byte
        Public card As UInteger
        Public reason As Byte

        Public Sub New(timestamp As String, index As UInteger, eventType As Byte, granted As Boolean, door As Byte, direction As Byte, card As UInteger, reason As Byte)
            Me.timestamp = timestamp
            Me.index = index
            Me.eventType = eventType
            Me.granted = granted
            Me.door = door
            Me.direction = direction
            Me.card = card
            Me.reason = reason
        End Sub
    End Class

    Public Class ListenEvent
        Public controller As UInteger
        Public timestamp As String
        Public index As UInteger
        Public eventType As Byte
        Public granted As Boolean
        Public door As Byte
        Public direction As Byte
        Public card As UInteger
        Public reason As Byte

        Public Sub New(controller As UInteger, timestamp As String, index As UInteger, eventType As Byte, granted As Boolean, door As Byte, direction As Byte, card As UInteger, reason As Byte)
            Me.controller = controller
            Me.timestamp = timestamp
            Me.index = index
            Me.eventType = eventType
            Me.granted = granted
            Me.door = door
            Me.direction = direction
            Me.card = card
            Me.reason = reason
        End Sub
    End Class

    Public Class Status
        Public ID As UInteger
        Public sysdatetime As String
        Public doors As Boolean()
        Public buttons As Boolean()
        Public relays As Byte
        Public inputs As Byte
        Public syserror As Byte
        Public info As Byte
        Public seqno As UInteger
        Public evt As [Event]

        Public Sub New(ID As UInteger, sysdatetime As String, doors As Boolean(), buttons As Boolean(), relays As Byte, inputs As Byte, syserror As Byte, info As Byte, seqno As UInteger, evt As [Event])
            Me.ID = ID
            Me.sysdatetime = sysdatetime
            Me.doors = doors
            Me.buttons = buttons
            Me.relays = relays
            Me.inputs = inputs
            Me.syserror = syserror
            Me.info = info
            Me.seqno = seqno
            Me.evt = evt
        End Sub
    End Class

    Public Class DoorControl
        Public mode As Byte
        Public delay As Byte

        Public Sub New(mode As Byte, delay As Byte)
            Me.mode = mode
            Me.delay = delay
        End Sub
    End Class

    Public Class Card
        Public cardNumber As UInteger
        Public [from] As String
        Public [to] As String
        Public doors As Byte()
        Public PIN As UInteger

        Public Sub New(cardNumber As UInteger, [from] As String, [to] As String, doors As Byte(), PIN As UInteger)
            Me.cardNumber = cardNumber
            Me.[from] = [from]
            Me.[to] = [to]
            Me.doors = doors
            Me.PIN = PIN
        End Sub
    End Class

    Public Class TimeProfile
        Public ID As Byte
        Public linked As Byte
        Public from As String
        Public [to] As String
        Public monday As Boolean
        Public tuesday As Boolean
        Public wednesday As Boolean
        Public thursday As Boolean
        Public friday As Boolean
        Public saturday As Boolean
        Public sunday As Boolean
        Public segment1start As String
        Public segment1end As String
        Public segment2start As String
        Public segment2end As String
        Public segment3start As String
        Public segment3end As String

        Public Sub New(ID As Byte, linked As Byte, from As String, [to] As String, monday As Boolean, tuesday As Boolean, wednesday As Boolean, thursday As Boolean, friday As Boolean, saturday As Boolean, sunday As Boolean, segment1start As String, segment1end As String, segment2start As String, segment2end As String, segment3start As String, segment3end As String)
            Me.ID = ID
            Me.linked = linked
            Me.from = from
            Me.[to] = [to]
            Me.monday = monday
            Me.tuesday = tuesday
            Me.wednesday = wednesday
            Me.thursday = thursday
            Me.friday = friday
            Me.saturday = saturday
            Me.sunday = sunday
            Me.segment1start = segment1start
            Me.segment1end = segment1end
            Me.segment2start = segment2start
            Me.segment2end = segment2end
            Me.segment3start = segment3start
            Me.segment3end = segment3end
        End Sub
    End Class

    Public Class Task
        Public task As Byte
        Public door As Byte
        Public from As String
        Public [to] As String
        Public monday As Boolean
        Public tuesday As Boolean
        Public wednesday As Boolean
        Public thursday As Boolean
        Public friday As Boolean
        Public saturday As Boolean
        Public sunday As Boolean
        Public at As String
        Public cards As Byte

        Public Sub New(task As Byte, door As Byte, from As String, [to] As String, monday As Boolean, tuesday As Boolean, wednesday As Boolean, thursday As Boolean, friday As Boolean, saturday As Boolean, sunday As Boolean, at As String, cards As Byte)
            Me.task = task
            Me.door = door
            Me.from = from
            Me.[to] = [to]
            Me.monday = monday
            Me.tuesday = tuesday
            Me.wednesday = wednesday
            Me.thursday = thursday
            Me.friday = friday
            Me.saturday = saturday
            Me.sunday = sunday
            Me.at = at
            Me.cards = cards
        End Sub
    End Class

    Public Class Lookup
        Public Const LOOKUP_MODE As String = "door.mode"
        Public Const LOOKUP_DIRECTION As String = "event.direction"
        Public Const LOOKUP_EVENT_TYPE As String = "event.type"
        Public Const LOOKUP_EVENT_REASON As String = "event.reason"
        Const ModeNormallyOpen As String = "normally open"
        Const ModeNormallyClosed As String = "normally closed"
        Const ModeControlled As String = "controlled"
        Const ModeUnknown As String = "unknown"
        Const DirectionIn As String = "in"
        Const DirectionOut As String = "out"
        Const DirectionUnknown As String = "unknown"
        Const EventTypeNone As String = "none"
        Const EventTypeSwipe As String = "swipe"
        Const EventTypeDoor As String = "door"
        Const EventTypeAlarm As String = "alarm"
        Const EventTypeOverwritten As String = "overwritten"
        Const EventTypeUnknown As String = "unknown"
        Const EventReasonNone As String = ""
        Const EventReasonSwipe As String = "swipe"
        Const EventReasonSwipeOpen As String = "swipe open"
        Const EventReasonSwipeClose As String = "swipe close"
        Const EventReasonDenied As String = "swipe:denied (system)"
        Const EventReasonNoAccessRights As String = "no access rights"
        Const EventReasonIncorrectPassword As String = "incorrect password"
        Const EventReasonAntiPassback As String = "anti-passback"
        Const EventReasonMoreCards As String = "more cards"
        Const EventReasonFirstCardOpen As String = "first card open"
        Const EventReasonDoorIsNormallyClosed As String = "door is normally closed"
        Const EventReasonInterlock As String = "interlock"
        Const EventReasonNotInAllowedTimePeriod As String = "not in allowed time period"
        Const EventReasonInvalidTimezone As String = "invalid timezone"
        Const EventReasonAccessDenied As String = "access denied"
        Const EventReasonPushbuttonOk As String = "pushbutton ok"
        Const EventReasonDoorOpened As String = "door opened"
        Const EventReasonDoorClosed As String = "door closed"
        Const EventReasonDoorOpenedSupervisorPassword As String = "door opened (supervisor password)"
        Const EventReasonControllerPowerOn As String = "controller power on"
        Const EventReasonControllerReset As String = "controller reset"
        Const EventReasonPushbuttonInvalidDoorLocked As String = "pushbutton invalid (door locked)"
        Const EventReasonPushbuttonInvalidOffline As String = "pushbutton invalid (offline)"
        Const EventReasonPushbuttonInvalidInterlock As String = "pushbutton invalid (interlock)"
        Const EventReasonPushbuttonInvalidThreat As String = "pushbutton invalid (threat)"
        Const EventReasonDoorOpenTooLong As String = "door open too long"
        Const EventReasonForcedOpen As String = "forced open"
        Const EventReasonFire As String = "fire"
        Const EventReasonForcedClosed As String = "forced closed"
        Const EventReasonTheftPrevention As String = "theft prevention"
        Const EventReasonZone24x7 As String = "24x7 zone"
        Const EventReasonEmergency As String = "emergency"
        Const EventReasonRemoteOpenDoor As String = "remote open door"
        Const EventReasonRemoteOpenDoorUSBReader As String = "remote open door (USB reader)"
        Const EventReasonUnknown As String = "unknown"
        Shared ReadOnly LookupMode As New Dictionary(Of UInteger, String)() From {
            {DoorMode.NormallyOpen, ModeNormallyOpen},
            {DoorMode.NormallyClosed, ModeNormallyClosed},
            {DoorMode.Controlled, ModeControlled}
        }
        Shared ReadOnly LookupDirection As New Dictionary(Of UInteger, String)() From {
            {Direction.[In], DirectionIn},
            {Direction.Out, DirectionOut}
        }
        Shared ReadOnly LookupEventType As New Dictionary(Of UInteger, String)() From {
            {EventType.None, EventTypeNone},
            {EventType.Swipe, EventTypeSwipe},
            {EventType.Door, EventTypeDoor},
            {EventType.Alarm, EventTypeAlarm},
            {EventType.Overwritten, EventTypeOverwritten}
        }
        Shared ReadOnly LookupEventReason As New Dictionary(Of UInteger, String)() From {
            {EventReason.None, EventReasonNone},
            {EventReason.Swipe, EventReasonSwipe},
            {EventReason.SwipeOpen, EventReasonSwipeOpen},
            {EventReason.SwipeClose, EventReasonSwipeClose},
            {EventReason.Denied, EventReasonDenied},
            {EventReason.NoAccessRights, EventReasonNoAccessRights},
            {EventReason.IncorrectPassword, EventReasonIncorrectPassword},
            {EventReason.AntiPassback, EventReasonAntiPassback},
            {EventReason.MoreCards, EventReasonMoreCards},
            {EventReason.FirstCardOpen, EventReasonFirstCardOpen},
            {EventReason.DoorIsNormallyClosed, EventReasonDoorIsNormallyClosed},
            {EventReason.Interlock, EventReasonInterlock},
            {EventReason.NotInAllowedTimePeriod, EventReasonNotInAllowedTimePeriod},
            {EventReason.InvalidTimezone, EventReasonInvalidTimezone},
            {EventReason.AccessDenied, EventReasonAccessDenied},
            {EventReason.PushbuttonOk, EventReasonPushbuttonOk},
            {EventReason.DoorOpened, EventReasonDoorOpened},
            {EventReason.DoorClosed, EventReasonDoorClosed},
            {EventReason.DoorOpenedSupervisorPassword, EventReasonDoorOpenedSupervisorPassword},
            {EventReason.ControllerPowerOn, EventReasonControllerPowerOn},
            {EventReason.ControllerReset, EventReasonControllerReset},
            {EventReason.PushbuttonInvalidDoorLocked, EventReasonPushbuttonInvalidDoorLocked},
            {EventReason.PushbuttonInvalidOffline, EventReasonPushbuttonInvalidOffline},
            {EventReason.PushbuttonInvalidInterlock, EventReasonPushbuttonInvalidInterlock},
            {EventReason.PushbuttonInvalidThreat, EventReasonPushbuttonInvalidThreat},
            {EventReason.DoorOpenTooLong, EventReasonDoorOpenTooLong},
            {EventReason.ForcedOpen, EventReasonForcedOpen},
            {EventReason.Fire, EventReasonFire},
            {EventReason.ForcedClosed, EventReasonForcedClosed},
            {EventReason.TheftPrevention, EventReasonTheftPrevention},
            {EventReason.Zone24x7, EventReasonZone24x7},
            {EventReason.Emergency, EventReasonEmergency},
            {EventReason.RemoteOpenDoor, EventReasonRemoteOpenDoor},
            {EventReason.RemoteOpenDoorUSBReader, EventReasonRemoteOpenDoorUSBReader}
        }
        Private Shared ReadOnly dictionaries As New Dictionary(Of String, Dictionary(Of UInteger, String))() From {
            {LOOKUP_MODE, LookupMode},
            {LOOKUP_DIRECTION, LookupDirection},
            {LOOKUP_EVENT_TYPE, LookupEventType},
            {LOOKUP_EVENT_REASON, LookupEventReason}
        }
        Private Shared ReadOnly unknown As New Dictionary(Of String, String)() From {
            {LOOKUP_MODE, ModeUnknown},
            {LOOKUP_DIRECTION, DirectionUnknown},
            {LOOKUP_EVENT_TYPE, EventTypeUnknown},
            {LOOKUP_EVENT_REASON, EventReasonUnknown}
        }

        Public Shared Function Find(category As String, code As UInteger) As String
            Dim dictionary As New Dictionary(Of UInteger, String)
            Dim s As String = Nothing

            If dictionaries.TryGetValue(category, dictionary) Then

                If dictionary.TryGetValue(code, s) Then
                    Return s
                End If

                Return unknown(category)
            End If

            Return "?"
        End Function
    End Class
End Namespace