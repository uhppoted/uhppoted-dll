<Global.Microsoft.VisualBasic.CompilerServices.DesignerGenerated()> _
Partial Class DoorControlFRM
    Inherits System.Windows.Forms.Form

    'Form overrides dispose to clean up the component list.
    <System.Diagnostics.DebuggerNonUserCode()> _
    Protected Overrides Sub Dispose(ByVal disposing As Boolean)
        Try
            If disposing AndAlso components IsNot Nothing Then
                components.Dispose()
            End If
        Finally
            MyBase.Dispose(disposing)
        End Try
    End Sub

    'Required by the Windows Form Designer
    Private components As System.ComponentModel.IContainer

    'NOTE: The following procedure is required by the Windows Form Designer
    'It can be modified using the Windows Form Designer.  
    'Do not modify it using the code editor.
    <System.Diagnostics.DebuggerStepThrough()> _
    Private Sub InitializeComponent()
        ControllerListBox = New ListBox()
        SetBTN = New Button()
        CancelBTN = New Button()
        CheckBox1 = New CheckBox()
        CheckBox2 = New CheckBox()
        CheckBox3 = New CheckBox()
        CheckBox4 = New CheckBox()
        RadioButton1 = New RadioButton()
        RadioButton2 = New RadioButton()
        RadioButton3 = New RadioButton()
        Label1 = New Label()
        TextBox1 = New TextBox()
        Label2 = New Label()
        SuspendLayout()
        ' 
        ' ControllerListBox
        ' 
        ControllerListBox.FormattingEnabled = True
        ControllerListBox.ItemHeight = 15
        ControllerListBox.Location = New Point(12, 12)
        ControllerListBox.Name = "ControllerListBox"
        ControllerListBox.Size = New Size(75, 214)
        ControllerListBox.TabIndex = 1
        ' 
        ' SetBTN
        ' 
        SetBTN.Location = New Point(12, 249)
        SetBTN.Name = "SetBTN"
        SetBTN.Size = New Size(75, 23)
        SetBTN.TabIndex = 2
        SetBTN.Text = "Set"
        SetBTN.UseVisualStyleBackColor = True
        ' 
        ' CancelBTN
        ' 
        CancelBTN.Location = New Point(129, 249)
        CancelBTN.Name = "CancelBTN"
        CancelBTN.Size = New Size(75, 23)
        CancelBTN.TabIndex = 3
        CancelBTN.Text = "Cancel"
        CancelBTN.UseVisualStyleBackColor = True
        ' 
        ' CheckBox1
        ' 
        CheckBox1.AutoSize = True
        CheckBox1.Location = New Point(93, 12)
        CheckBox1.Name = "CheckBox1"
        CheckBox1.Size = New Size(61, 19)
        CheckBox1.TabIndex = 4
        CheckBox1.Text = "Door 1"
        CheckBox1.UseVisualStyleBackColor = True
        ' 
        ' CheckBox2
        ' 
        CheckBox2.AutoSize = True
        CheckBox2.Location = New Point(93, 37)
        CheckBox2.Name = "CheckBox2"
        CheckBox2.Size = New Size(61, 19)
        CheckBox2.TabIndex = 5
        CheckBox2.Text = "Door 2"
        CheckBox2.UseVisualStyleBackColor = True
        ' 
        ' CheckBox3
        ' 
        CheckBox3.AutoSize = True
        CheckBox3.Location = New Point(93, 62)
        CheckBox3.Name = "CheckBox3"
        CheckBox3.Size = New Size(61, 19)
        CheckBox3.TabIndex = 6
        CheckBox3.Text = "Door 3"
        CheckBox3.UseVisualStyleBackColor = True
        ' 
        ' CheckBox4
        ' 
        CheckBox4.AutoSize = True
        CheckBox4.Location = New Point(93, 87)
        CheckBox4.Name = "CheckBox4"
        CheckBox4.Size = New Size(61, 19)
        CheckBox4.TabIndex = 7
        CheckBox4.Text = "Door 4"
        CheckBox4.UseVisualStyleBackColor = True
        ' 
        ' RadioButton1
        ' 
        RadioButton1.AutoSize = True
        RadioButton1.Location = New Point(93, 115)
        RadioButton1.Name = "RadioButton1"
        RadioButton1.Size = New Size(79, 19)
        RadioButton1.TabIndex = 8
        RadioButton1.TabStop = True
        RadioButton1.Text = "controlled"
        RadioButton1.UseVisualStyleBackColor = True
        ' 
        ' RadioButton2
        ' 
        RadioButton2.AutoSize = True
        RadioButton2.Location = New Point(93, 140)
        RadioButton2.Name = "RadioButton2"
        RadioButton2.Size = New Size(104, 19)
        RadioButton2.TabIndex = 9
        RadioButton2.TabStop = True
        RadioButton2.Text = "normally-open"
        RadioButton2.UseVisualStyleBackColor = True
        ' 
        ' RadioButton3
        ' 
        RadioButton3.AutoSize = True
        RadioButton3.Location = New Point(93, 162)
        RadioButton3.Name = "RadioButton3"
        RadioButton3.Size = New Size(111, 19)
        RadioButton3.TabIndex = 10
        RadioButton3.TabStop = True
        RadioButton3.Text = "normally-closed"
        RadioButton3.UseVisualStyleBackColor = True
        ' 
        ' Label1
        ' 
        Label1.AutoSize = True
        Label1.Location = New Point(93, 193)
        Label1.Name = "Label1"
        Label1.Size = New Size(77, 15)
        Label1.TabIndex = 11
        Label1.Text = "Delay (in sec)"
        ' 
        ' TextBox1
        ' 
        TextBox1.Location = New Point(176, 185)
        TextBox1.Name = "TextBox1"
        TextBox1.Size = New Size(28, 23)
        TextBox1.TabIndex = 12
        ' 
        ' Label2
        ' 
        Label2.Font = New Font("Segoe UI", 7F)
        Label2.Location = New Point(93, 210)
        Label2.Name = "Label2"
        Label2.Size = New Size(111, 28)
        Label2.TabIndex = 13
        Label2.Text = "Leave blank for default (3 sec)"
        ' 
        ' DoorControlFRM
        ' 
        AutoScaleDimensions = New SizeF(7F, 15F)
        AutoScaleMode = AutoScaleMode.Font
        ClientSize = New Size(222, 284)
        Controls.Add(Label2)
        Controls.Add(TextBox1)
        Controls.Add(Label1)
        Controls.Add(RadioButton3)
        Controls.Add(RadioButton2)
        Controls.Add(RadioButton1)
        Controls.Add(CheckBox4)
        Controls.Add(CheckBox3)
        Controls.Add(CheckBox2)
        Controls.Add(CheckBox1)
        Controls.Add(CancelBTN)
        Controls.Add(SetBTN)
        Controls.Add(ControllerListBox)
        Name = "DoorControlFRM"
        StartPosition = FormStartPosition.CenterScreen
        Text = "Door Control"
        ResumeLayout(False)
        PerformLayout()
    End Sub

    Friend WithEvents ControllerListBox As ListBox
    Friend WithEvents SetBTN As Button
    Friend WithEvents CancelBTN As Button
    Friend WithEvents CheckBox1 As CheckBox
    Friend WithEvents CheckBox2 As CheckBox
    Friend WithEvents CheckBox3 As CheckBox
    Friend WithEvents CheckBox4 As CheckBox
    Friend WithEvents RadioButton1 As RadioButton
    Friend WithEvents RadioButton2 As RadioButton
    Friend WithEvents RadioButton3 As RadioButton
    Friend WithEvents Label1 As Label
    Friend WithEvents TextBox1 As TextBox
    Friend WithEvents Label2 As Label
End Class
