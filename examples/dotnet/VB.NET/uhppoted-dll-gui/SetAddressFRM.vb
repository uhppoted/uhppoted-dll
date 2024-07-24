Imports System.Net

Public Class SetAddressFRM
    Private Sub SetAddressFRM_Load(sender As Object, e As EventArgs) Handles MyBase.Load
        Dim list As UInteger() = Uhppotegui.GetControllers()
        For i As Integer = 0 To list.Length - 1
            ControllerListBox.Items.Add(list(i))
        Next
    End Sub

    Private Sub SetBTN_Click(sender As Object, e As EventArgs) Handles SetBTN.Click
        If ControllerListBox.SelectedItem = Nothing Then
            MsgBox("Please select a controller")
            Exit Sub
        Else
            If IsValidIPAddress(IPBox.Text) AndAlso IsValidIPAddress(MaskBox.Text) AndAlso IsValidIPAddress(GWBox.Text) Then
                Uhppotegui.SetAddress(ControllerListBox.SelectedItem, IPBox.Text, MaskBox.Text, GWBox.Text)

                Close()
            Else
                MsgBox("Please enter a valid IP, Subnet and/or Gateway")
                Exit Sub
            End If
        End If
    End Sub

    Private Function IsValidIPAddress(ipString As String) As Boolean
        Dim address As IPAddress = Nothing
        Return IPAddress.TryParse(ipString, address)
    End Function

    Private Sub CancelBTN_Click(sender As Object, e As EventArgs) Handles CancelBTN.Click
        Close()
    End Sub
End Class