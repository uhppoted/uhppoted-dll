Imports System.Net
Imports System.Threading
Imports uhppoted_dll_gui.uhppoted
Imports uhppoted_dll_gui.uhppoted.Uhppoted

Public Class Uhppotegui
    Dim u As uhppoted.Uhppoted

#Region "uhppote commands"
    Private Sub InitUhppote()
        Dim Bind As String = "192.168.1.121" 'IPv4 address:port to which to bind the UDP socket. Defaults to 0.0.0.0:0
        Dim Broadcast As String = "255.255.255.255:60000" 'IPv4 address:port for broadcast UDP packets. Defaults to 255.255.255.255:60000
        Dim listen As String = "0.0.0.0:60001" 'IPv4 address:port for events from controller (unused). Defaults to 0.0.0.0:60001
        Dim timeout As Integer = 2500 'milliseconds to wait for a reply. Defaults to 5 seconds.

        Try
            u = New uhppoted.Uhppoted(Bind, Broadcast, listen, timeout, Array.Empty(Of Controller)(), False)
        Catch ex As Exception
            MessageBox.Show(ex.Message)
        End Try
    End Sub

    Public Function GetControllers()
        Return u.GetDevices()
    End Function

    Private Function GetController(Controller As UInteger)
        Dim output As String

        Try
            Dim device As Device = u.GetDevice(Controller)
            output = "Get Controller: " & Controller & Environment.NewLine &
                                   "address: " & device.address & Environment.NewLine &
                                   "subnet mask: " & device.subnet & Environment.NewLine &
                                   "gateway: " & device.gateway & Environment.NewLine &
                                   "MAC: " & device.MAC & Environment.NewLine &
                                   "version: " & device.version & Environment.NewLine &
                                   "released: " & device.date
        Catch ex As Exception
            output = ex.Message
        End Try

        Return output
    End Function

    Private Shared Sub GetStatus(u As uhppoted.Uhppoted, Controller As UInteger)
        Dim status As Status = u.GetStatus(Controller)
        Dim timestamp As String = status.evt.timestamp

        If timestamp = "" Then
            timestamp = "-"
        End If

        Dim message As String = "get-status (" & Controller & ")" & Environment.NewLine &
                       "ID              : " & status.ID & Environment.NewLine &
                       "timestamp       : " & status.sysdatetime & Environment.NewLine &
                       "doors[1]        : " & status.doors(0) & Environment.NewLine &
                       "doors[2]        : " & status.doors(1) & Environment.NewLine &
                       "doors[3]        : " & status.doors(2) & Environment.NewLine &
                       "doors[4]        : " & status.doors(3) & Environment.NewLine &
                       "buttons[1]      : " & status.buttons(0) & Environment.NewLine &
                       "buttons[2]      : " & status.buttons(1) & Environment.NewLine &
                       "buttons[3]      : " & status.buttons(2) & Environment.NewLine &
                       "buttons[4]      : " & status.buttons(3) & Environment.NewLine &
                       "relays          : " & status.relays & Environment.NewLine &
                       "inputs          : " & status.inputs & Environment.NewLine &
                       "syserror        : " & status.syserror & Environment.NewLine &
                       "info            : " & status.info & Environment.NewLine &
                       "seqno           : " & status.seqno & Environment.NewLine &
                       "event timestamp : " & timestamp & Environment.NewLine &
                       "      index     : " & status.evt.index & Environment.NewLine &
                       "      type      : " & Lookup.Find(Lookup.LOOKUP_EVENT_TYPE, status.evt.eventType) & Environment.NewLine &
                       "      granted   : " & status.evt.granted & Environment.NewLine &
                       "      door      : " & status.evt.door & Environment.NewLine &
                       "      direction : " & Lookup.Find(Lookup.LOOKUP_DIRECTION, status.evt.direction) & Environment.NewLine &
                       "      card      : " & status.evt.card & Environment.NewLine &
                       "      reason    : " & Lookup.Find(Lookup.LOOKUP_EVENT_REASON, status.evt.reason)

        MsgBox(message)
    End Sub

    Private Sub OpenDoor(controller As UInteger, Door As Byte)
        u.OpenDoor(controller, Door)
    End Sub

    Private Function GetListener(Controller As UInteger)
        Return u.GetListener(Controller)
    End Function

    Private Sub SetListener(Controller As UInteger, IP As String)
        u.SetListener(Controller, IP)
    End Sub

#Region "Listener"
    Dim ListenThread As Thread
    Dim exitEvent As ManualResetEvent

    Private Sub Listen(u As uhppoted.Uhppoted, done As ManualResetEvent)
        Dim onevent As OnEvent = Sub(e As ListenEvent)
                                     ''Send incoming data to textbox
                                     ''I'm leaving this here as an option
                                     'Dim message As String = String.Format("-- EVENT" & Environment.NewLine &
                                     '                 "   controller: " & e.controller & Environment.NewLine &
                                     '                 "   timestamp:  " & e.timestamp & Environment.NewLine &
                                     '                 "   index:      " & e.index & Environment.NewLine &
                                     '                 "   event:      " & Lookup.Find(Lookup.LOOKUP_EVENT_TYPE, e.eventType) & Environment.NewLine &
                                     '                 "   granted:    " & If(e.granted, "yes", "no") & Environment.NewLine &
                                     '                 "   door:       " & e.door & Environment.NewLine &
                                     '                 "   direction:  " & Lookup.Find(Lookup.LOOKUP_DIRECTION, e.direction) & Environment.NewLine &
                                     '                 "   card:       " & e.card & Environment.NewLine &
                                     '                 "   reason:     " & Lookup.Find(Lookup.LOOKUP_EVENT_REASON, e.reason))
                                     'WTT(message)

                                     ''Sent incoming data to DataGridView
                                     AddRow(e.controller, e.timestamp, e.index, Lookup.Find(Lookup.LOOKUP_EVENT_TYPE, e.eventType),
                                            If(e.granted, "yes", "no"), e.door, Lookup.Find(Lookup.LOOKUP_DIRECTION, e.direction),
                                            e.card, Lookup.Find(Lookup.LOOKUP_EVENT_REASON, e.reason))
                                 End Sub

        Dim onerror As OnError = Sub(err As String) WTT("ERROR: " & err)

        Dim delay = TimeSpan.FromMilliseconds(1000)
        Dim running As Byte = 0 ' NTS because C# bool is not uint8_t
        Dim [stop] As Byte = 0    ' NTS because C# bool is not uint8_t

        u.ListenEvents(onevent, onerror, running, [stop])

        Thread.Sleep(delay)
        Dim count = 0

        While count < 5 AndAlso Not [CBool](running)
            WTT("DEBUG ... waiting " & count & " " & If([CBool](running), "running", "pending"))
            Thread.Sleep(delay)
            count += 1
        End While

        If Not [CBool](running) Then
            WTT("ERROR failed to start event listener")
            Return
        End If

        WTT("INFO  ... listening")
        done.WaitOne()
        WTT("DEBUG .. stopping")

        [stop] = 1
        Thread.Sleep(delay)
        Dim count1 = 0

        While count1 < 5 AndAlso [CBool](running)
            WTT("DEBUG ... stoppping event listener " & count1 & " " & If([CBool](running), "running", "stopped"))
            Thread.Sleep(delay)
            count1 += 1
        End While

        WTT("DEBUG ... waited")

        If [CBool](running) Then
            WTT("ERROR failed to stop event listener")
        End If

        WTT("DEBUG ... thread exit")
    End Sub

    Shared Function [CBool](v As Byte) As Boolean
        Return v = 1
    End Function

    Private Sub WTT(str As String)
        Invoke(Sub()
                   RichTextBox1.AppendText(str & Environment.NewLine)
                   RichTextBox1.ScrollToCaret()
               End Sub)
    End Sub

    Private Sub AddRow(controller As UInteger, timestamp As String, index As String, [event] As String, granted As String,
                       door As Integer, direction As String, card As String, reason As String)
        Invoke(Sub()
                   ListenDGV.Rows.Add(controller, timestamp, index, [event], granted, door, direction, card, reason)
               End Sub)
    End Sub
#End Region

#Region "Card"
    Public Function GetAllCards() As DataTable
        Dim dt As New DataTable

        ' Define the columns
        dt.Columns.Add("Controller", GetType(UInteger))
        dt.Columns.Add("Card number", GetType(UInteger))
        dt.Columns.Add("Valid from", GetType(Date))
        dt.Columns.Add("Valid to", GetType(Date))
        dt.Columns.Add("Doors", GetType(String))
        dt.Columns.Add("PIN", GetType(UInteger))

        Dim devices = u.GetDevices()
        For Each con As UInteger In devices
            Dim cardCount = u.GetCards(con)
            For n As Integer = 1 To cardCount
                Dim c As Card = u.GetCardByIndex(con, n)
                Dim r As DataRow = dt.NewRow()
                r("Controller") = con
                r("Card number") = c.cardNumber
                r("Valid from") = c.from
                r("Valid to") = c.to
                r("Doors") = ByteToDoorString(c.doors)
                r("PIN") = c.PIN

                dt.Rows.Add(r)
            Next
        Next

        Return dt
    End Function

    Private Function ByteToDoorString(doors As Byte()) As String
        If doors Is Nothing OrElse doors.Length = 0 Then
            Return "[0,0,0,0]"
        End If

        Dim doorStates As New List(Of String)

        ' Check each bit and convert to string representation
        For i As Integer = 0 To 3
            If (doors(0) And (1 << i)) <> 0 Then
                doorStates.Add("1")
            Else
                doorStates.Add("0")
            End If
        Next

        Return "[" & String.Join(",", doorStates) & "]"
    End Function

    Public Sub AddCard(Controller As UInteger, CardNumber As UInteger, [From] As String, [To] As String, Doors As Byte(), PIN As Integer)
        u.PutCard(Controller, CardNumber, [From], [To], Doors, PIN)
    End Sub

    Public Sub DeleteCard(Controller As UInteger, CardNumber As UInteger)
        Try
            u.DeleteCard(Controller, CardNumber)
        Catch ex As Exception
            MessageBox.Show(ex.Message)
        End Try
    End Sub

    Public Sub DeleteAllCardsFromController(controller As UInteger)
        Try
            u.DeleteCards(controller)
        Catch ex As Exception
            MessageBox.Show(ex.Message)
        End Try
    End Sub
#End Region
#End Region

    Private Sub Uhppotegui_Load(sender As Object, e As EventArgs) Handles MyBase.Load
        'Initialize uhppote
        InitUhppote()
    End Sub

    Private Sub GetControllerBTN_Click(sender As Object, e As EventArgs) Handles GetControllerBTN.Click
        If ControllerListBox.SelectedItem = Nothing Then
            Dim controller As Integer
            Dim str As String = InputBox("Enter Controller ID")
            If Not String.IsNullOrWhiteSpace(str) Then
                If Integer.TryParse(str, controller) Then
                    MsgBox(GetController(controller))
                Else
                    MessageBox.Show("Please enter a valid Controller ID number.", "Invalid Input")
                End If
            End If
        Else
            MsgBox(GetController(ControllerListBox.SelectedItem))
        End If
    End Sub

    Private Sub GetControllersBTN_Click(sender As Object, e As EventArgs) Handles GetControllersBTN.Click
        Dim list As UInteger() = GetControllers()
        For i As Integer = 0 To list.Length - 1
            ControllerListBox.Items.Add(list(i))
        Next
    End Sub

    Private Sub GetStatusBTN_Click(sender As Object, e As EventArgs) Handles GetStatusBTN.Click
        If ControllerListBox.SelectedItem = Nothing Then
            Dim controller As Integer
            Dim str As String = InputBox("Enter Controller ID")
            If Not String.IsNullOrWhiteSpace(str) Then
                If Integer.TryParse(str, controller) Then
                    GetStatus(u, controller)
                End If
            End If
        Else
            GetStatus(u, ControllerListBox.SelectedItem)
        End If
    End Sub

    Private Sub OpenDoorBTN_Click(sender As Object, e As EventArgs) Handles OpenDoorBTN.Click
        Dim Door As Integer

        If ControllerListBox.SelectedItem = Nothing Then
            Dim controller As UInteger
            Dim str As String = InputBox("Enter Controller ID")
            If Not String.IsNullOrWhiteSpace(str) Then
                If UInteger.TryParse(str, controller) Then
                    Dim DoorSTR As String = InputBox("Enter Door number (between 1 and 4)")
                    If Not String.IsNullOrWhiteSpace(DoorSTR) Then
                        If Integer.TryParse(DoorSTR, Door) Then
                            OpenDoor(controller, Door)
                        Else
                            MessageBox.Show("Please enter a valid door number")
                            Exit Sub
                        End If
                    End If
                Else
                    MessageBox.Show("Please enter a valid Controller ID")
                    Exit Sub
                End If
            End If
        Else
            Dim DoorSTR As String = InputBox("Enter Door number (between 1 and 4)")
            If Not String.IsNullOrWhiteSpace(DoorSTR) Then
                If Integer.TryParse(DoorSTR, Door) Then OpenDoor(ControllerListBox.SelectedItem, Door)
            Else
                MessageBox.Show("Please enter a valid door number")
                Exit Sub
            End If
        End If
    End Sub

    Private Sub StartListenBTN_Click(sender As Object, e As EventArgs) Handles StartListenBTN.Click
        RichTextBox1.Clear()

        exitEvent = New ManualResetEvent(False)
        ListenThread = New Thread(Sub() Listen(u, exitEvent)) With {.IsBackground = True}
        ListenThread.Start()
    End Sub

    Private Sub StopListenBTN_Click(sender As Object, e As EventArgs) Handles StopListenBTN.Click
        exitEvent.Set()
    End Sub

    Private Sub CardMgmtBTN_Click(sender As Object, e As EventArgs) Handles CardMgmtBTN.Click
        Dim f As New CardMgmtFRM
        f.ShowDialog()
    End Sub

    Private Sub SetPCControlBTN_Click(sender As Object, e As EventArgs) Handles SetPCControlBTN.Click
        MsgBox("Still working on this...")
    End Sub

    Private Sub GetLisenerBTN_Click(sender As Object, e As EventArgs) Handles GetLisenerBTN.Click
        If ControllerListBox.SelectedItem = Nothing Then
            Dim controller As UInteger
            Dim str As String = InputBox("Enter Controller ID")

            If Not String.IsNullOrWhiteSpace(str) Then
                If UInteger.TryParse(str, controller) Then
                    MsgBox(GetListener(controller))
                Else
                    MessageBox.Show("Please enter a valid Controller ID")
                    Exit Sub
                End If
            End If
        Else
            MsgBox(GetListener(ControllerListBox.SelectedItem))
        End If
    End Sub

    Private Sub SetLisenerBTN_Click(sender As Object, e As EventArgs) Handles SetLisenerBTN.Click
        ' I set here the port, but in uhppote-dll-cli (not the vb version) it's "IP:PORT" (eg. 192.168.1.121:60001)
        Dim Port As String = ":60001"

        If ControllerListBox.SelectedItem = Nothing Then
            Dim controller As UInteger
            Dim str As String = InputBox("Enter Controller ID")
            If UInteger.TryParse(str, controller) Then
                Dim IPInput As String = InputBox("Enter Lisener IP address (eg. 192.168.1.121)")
                If IsValidIPAddress(IPInput) Then
                    SetListener(controller, IPInput & Port)
                Else
                    MsgBox("Please enter a valid IP address")
                    Exit Sub
                End If
            Else
                MessageBox.Show("Please enter a valid Controller ID")
                Exit Sub
            End If
        Else
            Dim IPInput As String = InputBox("Enter Lisener IP address (eg. 192.168.1.121)")
            If IsValidIPAddress(IPInput) Then
                SetListener(ControllerListBox.SelectedItem, IPInput & Port)
            Else
                MsgBox("Please enter a valid IP address")
                Exit Sub
            End If
        End If
    End Sub

    Private Function IsValidIPAddress(ipString As String) As Boolean
        Dim address As IPAddress = Nothing
        Return IPAddress.TryParse(ipString, address)
    End Function
End Class