<Global.Microsoft.VisualBasic.CompilerServices.DesignerGenerated()> _
Partial Class SetAddressFRM
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
        Label1 = New Label()
        Label2 = New Label()
        Label3 = New Label()
        GWBox = New TextBox()
        MaskBox = New TextBox()
        IPBox = New TextBox()
        SuspendLayout()
        ' 
        ' ControllerListBox
        ' 
        ControllerListBox.FormattingEnabled = True
        ControllerListBox.ItemHeight = 15
        ControllerListBox.Location = New Point(12, 12)
        ControllerListBox.Name = "ControllerListBox"
        ControllerListBox.Size = New Size(75, 214)
        ControllerListBox.TabIndex = 0
        ' 
        ' SetBTN
        ' 
        SetBTN.Location = New Point(93, 186)
        SetBTN.Name = "SetBTN"
        SetBTN.Size = New Size(75, 40)
        SetBTN.TabIndex = 1
        SetBTN.Text = "Set Address"
        SetBTN.UseVisualStyleBackColor = True
        ' 
        ' CancelBTN
        ' 
        CancelBTN.Location = New Point(193, 186)
        CancelBTN.Name = "CancelBTN"
        CancelBTN.Size = New Size(75, 40)
        CancelBTN.TabIndex = 2
        CancelBTN.Text = "Cancel"
        CancelBTN.UseVisualStyleBackColor = True
        ' 
        ' Label1
        ' 
        Label1.Location = New Point(93, 12)
        Label1.Name = "Label1"
        Label1.Size = New Size(73, 23)
        Label1.TabIndex = 6
        Label1.Text = "IP address:"
        Label1.TextAlign = ContentAlignment.MiddleLeft
        ' 
        ' Label2
        ' 
        Label2.Location = New Point(93, 51)
        Label2.Name = "Label2"
        Label2.Size = New Size(86, 23)
        Label2.TabIndex = 7
        Label2.Text = "Subnet Mask:"
        Label2.TextAlign = ContentAlignment.MiddleLeft
        ' 
        ' Label3
        ' 
        Label3.Location = New Point(93, 85)
        Label3.Name = "Label3"
        Label3.Size = New Size(61, 23)
        Label3.TabIndex = 8
        Label3.Text = "Gateway:"
        Label3.TextAlign = ContentAlignment.MiddleLeft
        ' 
        ' GWBox
        ' 
        GWBox.Location = New Point(172, 85)
        GWBox.Name = "GWBox"
        GWBox.Size = New Size(100, 23)
        GWBox.TabIndex = 9
        ' 
        ' MaskBox
        ' 
        MaskBox.Location = New Point(172, 51)
        MaskBox.Name = "MaskBox"
        MaskBox.Size = New Size(100, 23)
        MaskBox.TabIndex = 10
        ' 
        ' IPBox
        ' 
        IPBox.Location = New Point(172, 12)
        IPBox.Name = "IPBox"
        IPBox.Size = New Size(100, 23)
        IPBox.TabIndex = 11
        ' 
        ' SetAddressFRM
        ' 
        AutoScaleDimensions = New SizeF(7F, 15F)
        AutoScaleMode = AutoScaleMode.Font
        ClientSize = New Size(299, 237)
        Controls.Add(IPBox)
        Controls.Add(MaskBox)
        Controls.Add(GWBox)
        Controls.Add(Label3)
        Controls.Add(Label2)
        Controls.Add(Label1)
        Controls.Add(CancelBTN)
        Controls.Add(SetBTN)
        Controls.Add(ControllerListBox)
        Name = "SetAddressFRM"
        StartPosition = FormStartPosition.CenterScreen
        Text = "Set Address"
        ResumeLayout(False)
        PerformLayout()
    End Sub

    Friend WithEvents ControllerListBox As ListBox
    Friend WithEvents SetBTN As Button
    Friend WithEvents CancelBTN As Button
    Friend WithEvents Label1 As Label
    Friend WithEvents Label2 As Label
    Friend WithEvents Label3 As Label
    Friend WithEvents GWBox As TextBox
    Friend WithEvents MaskBox As TextBox
    Friend WithEvents IPBox As TextBox
End Class
