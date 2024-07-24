Public Class DoorControlFRM
    Private Sub DoorControlFRM_Load(sender As Object, e As EventArgs) Handles MyBase.Load
        RadioButton1.Select()

        Dim list As UInteger() = Uhppotegui.GetControllers()
        For i As Integer = 0 To list.Length - 1
            ControllerListBox.Items.Add(list(i))
        Next
    End Sub

    Private Sub SetBTN_Click(sender As Object, e As EventArgs) Handles SetBTN.Click
        Dim DoorList As New List(Of String)

        Dim delay As Integer
        Dim parsedValue As Integer

        If Not String.IsNullOrEmpty(TextBox1.Text) AndAlso Integer.TryParse(TextBox1.Text, parsedValue) Then
            delay = parsedValue
        Else
            delay = 3
        End If

        If ControllerListBox.SelectedItem = Nothing Then
            MsgBox("Please select a controller")
            Exit Sub
        Else
            If CheckBox1.Checked Or CheckBox2.Checked Or CheckBox3.Checked Or CheckBox4.Checked Then
                For Each ctrl As Control In Controls
                    If TypeOf ctrl Is CheckBox Then
                        Dim chk As CheckBox = CType(ctrl, CheckBox)
                        If chk.Checked Then
                            Dim con As UInteger = ControllerListBox.SelectedItem
                            Dim doornum As Integer = chk.Text.Split(" "c)(1)
                            Dim mode As String = GetMode()

                            DoorList.Add("Controller: " & con & ", Door " & doornum & ", mode: " & mode & ", Delay: " & delay)
                            Uhppotegui.SetDoorControl(ControllerListBox.SelectedItem, chk.Text.Split(" "c)(1), GetMode(), delay)
                        End If
                    End If
                Next
            Else
                MsgBox("Please select at least 1 door")
                Exit Sub
            End If
        End If

        Dim result As DialogResult = MessageBox.Show("Door control updated for:" & Environment.NewLine &
                                                     String.Join(Environment.NewLine, DoorList) & Environment.NewLine &
                                                     "Do you want to update more doors?",
                                                     "CSetDoorControl", MessageBoxButtons.YesNo, MessageBoxIcon.Question)
        If (result = DialogResult.Yes) Then
            RadioButton1.Select()
            TextBox1.Text = Nothing
            For Each ctrl As Control In Controls
                If TypeOf ctrl Is CheckBox Then
                    Dim chk As CheckBox = CType(ctrl, CheckBox)
                    chk.Checked = False
                End If
            Next
        Else
            Close()
        End If
    End Sub

    Private Function GetMode() As String
        Dim mode As String = Nothing
        For Each ctrl As Control In Controls
            If TypeOf ctrl Is RadioButton Then
                Dim rb As RadioButton = CType(ctrl, RadioButton)
                If rb.Checked Then
                    mode = rb.Text
                    Exit For
                End If
            End If
        Next

        Return mode
    End Function

    Private Sub CancelBTN_Click(sender As Object, e As EventArgs) Handles CancelBTN.Click
        Close()
    End Sub
End Class