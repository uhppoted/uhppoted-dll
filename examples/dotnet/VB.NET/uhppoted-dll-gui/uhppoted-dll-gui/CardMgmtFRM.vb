Public Class CardMgmtFRM
    Private Sub CardMgmtFRM_Load(sender As Object, e As EventArgs) Handles MyBase.Load
        RefreshCardList()
    End Sub

    Private Sub RefreshCardList()
        DataGridView1.DataSource = Nothing
        DataGridView1.DataSource = Uhppotegui.GetAllCards()
    End Sub

    Private Sub AddCardBTN_Click(sender As Object, e As EventArgs) Handles AddCardBTN.Click
        Panel1.Visible = True
        MaskedTextBox1.Text = Date.Now.ToString("yyyy-MM-dd")


        Dim list As UInteger() = Uhppotegui.GetControllers()
        For i As Integer = 0 To list.Length - 1
            ListBox1.Items.Add(list(i))
        Next
    End Sub

    Private Sub SaveBTN_Click(sender As Object, e As EventArgs) Handles SaveBTN.Click
        If Not String.IsNullOrWhiteSpace(ListBox1.SelectedItem) AndAlso Not String.IsNullOrWhiteSpace(TextBox1.Text) AndAlso
                                                        (CheckBox1.Checked Or CheckBox2.Checked Or CheckBox3.Checked Or CheckBox4.Checked) Then

            Dim controller As UInteger = ListBox1.SelectedItem

            Dim cardnumber As Integer
            If Not Integer.TryParse(TextBox1.Text, cardnumber) Then
                MsgBox("Please use numbers only for the card number")
                Exit Sub
            End If

            Dim vFrom As String = MaskedTextBox1.Text
            Dim vTo As String = If(MaskedTextBox2.MaskCompleted, MaskedTextBox2.Text, Now.AddYears(1).ToString("yyyy-MM-dd"))

            Dim doors As Byte() = Nothing
            FillDoorsArray(doors, CheckBox1, CheckBox2, CheckBox3, CheckBox4)

            Dim PIN As Integer
            If Not Integer.TryParse(If(TextBox2.Text = Nothing, 0, TextBox2.Text), PIN) Then
                MsgBox("Please use numbers only for the PIN")
                Exit Sub
            End If

            Dim result As DialogResult
            Try
                Uhppotegui.AddCard(controller, cardnumber, vFrom, vTo, doors, PIN)
                result = MessageBox.Show($"Card '{cardnumber}' added.{Environment.NewLine}Do you want to add another card?",
                                      "Card added", MessageBoxButtons.YesNo, MessageBoxIcon.Question)
            Catch ex As Exception
                MessageBox.Show(ex.Message)
            End Try

            TextBox1.Text = Nothing
            TextBox2.Text = Nothing
            MaskedTextBox2.Text = Nothing
            CheckBox1.Checked = False
            CheckBox2.Checked = False
            CheckBox3.Checked = False
            CheckBox4.Checked = False

            If (result = DialogResult.Yes) Then
                ListBox1.ClearSelected()
            Else
                MaskedTextBox1.Text = Nothing
                ListBox1.Items.Clear()

                Panel1.Visible = False
            End If

            DataGridView1.DataSource = Nothing
            DataGridView1.DataSource = Uhppotegui.GetAllCards()

            RefreshCardList()
        Else
            MsgBox("Please select a controller from the list and/or enter a valid card number and/or select at least 1 door.")
        End If
    End Sub

    Private Sub FillDoorsArray(ByRef doors As Byte(), door1 As CheckBox, door2 As CheckBox, door3 As CheckBox, door4 As CheckBox)
        If doors Is Nothing OrElse doors.Length = 0 Then
            doors = New Byte(0) {}
        End If

        doors(0) = 0 ' Ensure the byte is reset to 0 before setting bits
        If door1.Checked Then doors(0) = doors(0) Or &H1 ' Set bit 0
        If door2.Checked Then doors(0) = doors(0) Or &H2 ' Set bit 1
        If door3.Checked Then doors(0) = doors(0) Or &H4 ' Set bit 2
        If door4.Checked Then doors(0) = doors(0) Or &H8 ' Set bit 3
    End Sub

    Private Sub CloseBTN_Click(sender As Object, e As EventArgs) Handles CloseBTN.Click
        Close()
    End Sub

    Private Sub DelCardBTN_Click(sender As Object, e As EventArgs) Handles DelCardBTN.Click
        If DataGridView1.SelectedRows.Count > 0 Then
            Dim selectedRow As DataGridViewRow = DataGridView1.SelectedRows(0)

            Dim controller As UInteger = Convert.ToUInt32(selectedRow.Cells("Controller").Value)
            Dim cardNumber As UInteger = Convert.ToUInt32(selectedRow.Cells("Card number").Value)

            Dim result As DialogResult
            result = MessageBox.Show($"Please confirm the deletion of Card Number: '{cardNumber}' from Controller: '{controller}'",
                                      "Card deletion", MessageBoxButtons.YesNo, MessageBoxIcon.Question)

            If (result = DialogResult.Yes) Then
                Uhppotegui.DeleteCard(controller, cardNumber)
                RefreshCardList()
            Else
                Exit Sub
            End If
        Else
            MessageBox.Show("Please select a card in the list.")
        End If
    End Sub

    Private Sub DACFCBTN_Click(sender As Object, e As EventArgs) Handles DACFCBTN.Click
        Dim controller As Integer
        Dim str As String = InputBox("Enter Controller ID")
        If Not String.IsNullOrWhiteSpace(str) Then
            If Integer.TryParse(str, controller) Then
                Dim result As DialogResult
                result = MessageBox.Show($"Are you sure you want to delete all cards from Controller: '{controller}' ?",
                                      "Cards deletion", MessageBoxButtons.YesNo, MessageBoxIcon.Question)

                If (result = DialogResult.Yes) Then
                    Uhppotegui.DeleteAllCardsFromController(controller)
                    RefreshCardList()
                Else
                    Exit Sub
                End If
            Else
                MessageBox.Show("Please enter a valid Controller ID")
                Exit Sub
            End If
        End If
    End Sub
End Class