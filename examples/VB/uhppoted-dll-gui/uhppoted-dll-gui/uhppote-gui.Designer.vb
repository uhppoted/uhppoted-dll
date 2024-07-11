<Global.Microsoft.VisualBasic.CompilerServices.DesignerGenerated()>
Partial Class Uhppotegui
    Inherits System.Windows.Forms.Form

    'Form overrides dispose to clean up the component list.
    <System.Diagnostics.DebuggerNonUserCode()>
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
    <System.Diagnostics.DebuggerStepThrough()>
    Private Sub InitializeComponent()
        GetControllersBTN = New Button()
        ControllerListBox = New ListBox()
        GetControllerBTN = New Button()
        GetStatusBTN = New Button()
        OpenDoorBTN = New Button()
        StartListenBTN = New Button()
        StopListenBTN = New Button()
        ListenDGV = New DataGridView()
        Controller = New DataGridViewTextBoxColumn()
        timestamp = New DataGridViewTextBoxColumn()
        index = New DataGridViewTextBoxColumn()
        eventclnm = New DataGridViewTextBoxColumn()
        granted = New DataGridViewTextBoxColumn()
        door = New DataGridViewTextBoxColumn()
        direction = New DataGridViewTextBoxColumn()
        card = New DataGridViewTextBoxColumn()
        reason = New DataGridViewTextBoxColumn()
        RichTextBox1 = New RichTextBox()
        CardMgmtBTN = New Button()
        CType(ListenDGV, ComponentModel.ISupportInitialize).BeginInit()
        SuspendLayout()
        ' 
        ' GetControllersBTN
        ' 
        GetControllersBTN.Location = New Point(12, 58)
        GetControllersBTN.Name = "GetControllersBTN"
        GetControllersBTN.Size = New Size(75, 42)
        GetControllersBTN.TabIndex = 0
        GetControllersBTN.Text = "Get Controllers"
        GetControllersBTN.UseVisualStyleBackColor = True
        ' 
        ' ControllerListBox
        ' 
        ControllerListBox.FormattingEnabled = True
        ControllerListBox.ItemHeight = 15
        ControllerListBox.Location = New Point(12, 106)
        ControllerListBox.Name = "ControllerListBox"
        ControllerListBox.Size = New Size(75, 214)
        ControllerListBox.TabIndex = 1
        ' 
        ' GetControllerBTN
        ' 
        GetControllerBTN.Location = New Point(12, 12)
        GetControllerBTN.Name = "GetControllerBTN"
        GetControllerBTN.Size = New Size(75, 40)
        GetControllerBTN.TabIndex = 2
        GetControllerBTN.Text = "Get Controller"
        GetControllerBTN.UseVisualStyleBackColor = True
        ' 
        ' GetStatusBTN
        ' 
        GetStatusBTN.Location = New Point(108, 12)
        GetStatusBTN.Name = "GetStatusBTN"
        GetStatusBTN.Size = New Size(75, 40)
        GetStatusBTN.TabIndex = 3
        GetStatusBTN.Text = "Get Status"
        GetStatusBTN.UseVisualStyleBackColor = True
        ' 
        ' OpenDoorBTN
        ' 
        OpenDoorBTN.Location = New Point(108, 58)
        OpenDoorBTN.Name = "OpenDoorBTN"
        OpenDoorBTN.Size = New Size(75, 42)
        OpenDoorBTN.TabIndex = 4
        OpenDoorBTN.Text = "Open Door"
        OpenDoorBTN.UseVisualStyleBackColor = True
        ' 
        ' StartListenBTN
        ' 
        StartListenBTN.Location = New Point(205, 12)
        StartListenBTN.Name = "StartListenBTN"
        StartListenBTN.Size = New Size(75, 40)
        StartListenBTN.TabIndex = 5
        StartListenBTN.Text = "Start Listen"
        StartListenBTN.UseVisualStyleBackColor = True
        ' 
        ' StopListenBTN
        ' 
        StopListenBTN.Location = New Point(205, 58)
        StopListenBTN.Name = "StopListenBTN"
        StopListenBTN.Size = New Size(75, 42)
        StopListenBTN.TabIndex = 6
        StopListenBTN.Text = "Stop Listen"
        StopListenBTN.UseVisualStyleBackColor = True
        ' 
        ' ListenDGV
        ' 
        ListenDGV.AllowUserToAddRows = False
        ListenDGV.AllowUserToDeleteRows = False
        ListenDGV.AutoSizeColumnsMode = DataGridViewAutoSizeColumnsMode.Fill
        ListenDGV.ColumnHeadersHeightSizeMode = DataGridViewColumnHeadersHeightSizeMode.AutoSize
        ListenDGV.Columns.AddRange(New DataGridViewColumn() {Controller, timestamp, index, eventclnm, granted, door, direction, card, reason})
        ListenDGV.Location = New Point(205, 106)
        ListenDGV.Name = "ListenDGV"
        ListenDGV.ReadOnly = True
        ListenDGV.RowHeadersVisible = False
        ListenDGV.SelectionMode = DataGridViewSelectionMode.FullRowSelect
        ListenDGV.Size = New Size(950, 214)
        ListenDGV.TabIndex = 7
        ' 
        ' Controller
        ' 
        Controller.HeaderText = "Controller"
        Controller.Name = "Controller"
        Controller.ReadOnly = True
        ' 
        ' timestamp
        ' 
        timestamp.HeaderText = "timestamp"
        timestamp.Name = "timestamp"
        timestamp.ReadOnly = True
        ' 
        ' index
        ' 
        index.HeaderText = "index"
        index.Name = "index"
        index.ReadOnly = True
        ' 
        ' eventclnm
        ' 
        eventclnm.HeaderText = "event"
        eventclnm.Name = "eventclnm"
        eventclnm.ReadOnly = True
        ' 
        ' granted
        ' 
        granted.HeaderText = "granted"
        granted.Name = "granted"
        granted.ReadOnly = True
        ' 
        ' door
        ' 
        door.HeaderText = "door"
        door.Name = "door"
        door.ReadOnly = True
        ' 
        ' direction
        ' 
        direction.HeaderText = "direction"
        direction.Name = "direction"
        direction.ReadOnly = True
        ' 
        ' card
        ' 
        card.HeaderText = "card"
        card.Name = "card"
        card.ReadOnly = True
        ' 
        ' reason
        ' 
        reason.HeaderText = "reason"
        reason.Name = "reason"
        reason.ReadOnly = True
        ' 
        ' RichTextBox1
        ' 
        RichTextBox1.Location = New Point(295, 12)
        RichTextBox1.Name = "RichTextBox1"
        RichTextBox1.Size = New Size(168, 88)
        RichTextBox1.TabIndex = 8
        RichTextBox1.Text = ""
        ' 
        ' CardMgmtBTN
        ' 
        CardMgmtBTN.Location = New Point(503, 11)
        CardMgmtBTN.Name = "CardMgmtBTN"
        CardMgmtBTN.Size = New Size(88, 41)
        CardMgmtBTN.TabIndex = 9
        CardMgmtBTN.Text = "Card Management"
        CardMgmtBTN.UseVisualStyleBackColor = True
        ' 
        ' Uhppotegui
        ' 
        AutoScaleDimensions = New SizeF(7F, 15F)
        AutoScaleMode = AutoScaleMode.Font
        ClientSize = New Size(1167, 647)
        Controls.Add(CardMgmtBTN)
        Controls.Add(RichTextBox1)
        Controls.Add(ListenDGV)
        Controls.Add(StopListenBTN)
        Controls.Add(StartListenBTN)
        Controls.Add(OpenDoorBTN)
        Controls.Add(GetStatusBTN)
        Controls.Add(GetControllerBTN)
        Controls.Add(ControllerListBox)
        Controls.Add(GetControllersBTN)
        Name = "Uhppotegui"
        Text = "uhppote-GUI"
        CType(ListenDGV, ComponentModel.ISupportInitialize).EndInit()
        ResumeLayout(False)
    End Sub

    Friend WithEvents GetControllersBTN As Button
    Friend WithEvents ControllerListBox As ListBox
    Friend WithEvents GetControllerBTN As Button
    Friend WithEvents GetStatusBTN As Button
    Friend WithEvents OpenDoorBTN As Button
    Friend WithEvents StartListenBTN As Button
    Friend WithEvents StopListenBTN As Button
    Friend WithEvents ListenDGV As DataGridView
    Friend WithEvents RichTextBox1 As RichTextBox
    Friend WithEvents Controller As DataGridViewTextBoxColumn
    Friend WithEvents timestamp As DataGridViewTextBoxColumn
    Friend WithEvents index As DataGridViewTextBoxColumn
    Friend WithEvents eventclnm As DataGridViewTextBoxColumn
    Friend WithEvents granted As DataGridViewTextBoxColumn
    Friend WithEvents door As DataGridViewTextBoxColumn
    Friend WithEvents direction As DataGridViewTextBoxColumn
    Friend WithEvents card As DataGridViewTextBoxColumn
    Friend WithEvents reason As DataGridViewTextBoxColumn
    Friend WithEvents CardMgmtBTN As Button

End Class
