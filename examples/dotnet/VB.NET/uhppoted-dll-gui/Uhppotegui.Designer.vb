﻿<Global.Microsoft.VisualBasic.CompilerServices.DesignerGenerated()>
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
        GetControllerBTN = New Button()
        GetControllersBTN = New Button()
        ControllerListBox = New ListBox()
        GetStatusBTN = New Button()
        StartListenBTN = New Button()
        StopListenBTN = New Button()
        RichTextBox1 = New RichTextBox()
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
        CardMgmtBTN = New Button()
        OpenDoorBTN = New Button()
        GetLisenerBTN = New Button()
        SetLisenerBTN = New Button()
        SetAddressBTN = New Button()
        GetDoorControlBTN = New Button()
        SetDoorControlBTN = New Button()
        GetTimeBTN = New Button()
        SetTimeBTN = New Button()
        CType(ListenDGV, ComponentModel.ISupportInitialize).BeginInit()
        SuspendLayout()
        ' 
        ' GetControllerBTN
        ' 
        GetControllerBTN.Location = New Point(12, 12)
        GetControllerBTN.Name = "GetControllerBTN"
        GetControllerBTN.Size = New Size(75, 41)
        GetControllerBTN.TabIndex = 0
        GetControllerBTN.Text = "Get Controller"
        GetControllerBTN.UseVisualStyleBackColor = True
        ' 
        ' GetControllersBTN
        ' 
        GetControllersBTN.Location = New Point(12, 59)
        GetControllersBTN.Name = "GetControllersBTN"
        GetControllersBTN.Size = New Size(75, 40)
        GetControllersBTN.TabIndex = 1
        GetControllersBTN.Text = "Get Controllers"
        GetControllersBTN.UseVisualStyleBackColor = True
        ' 
        ' ControllerListBox
        ' 
        ControllerListBox.FormattingEnabled = True
        ControllerListBox.ItemHeight = 15
        ControllerListBox.Location = New Point(12, 105)
        ControllerListBox.Name = "ControllerListBox"
        ControllerListBox.Size = New Size(75, 214)
        ControllerListBox.TabIndex = 2
        ' 
        ' GetStatusBTN
        ' 
        GetStatusBTN.Location = New Point(124, 12)
        GetStatusBTN.Name = "GetStatusBTN"
        GetStatusBTN.Size = New Size(75, 41)
        GetStatusBTN.TabIndex = 3
        GetStatusBTN.Text = "Get Status"
        GetStatusBTN.UseVisualStyleBackColor = True
        ' 
        ' StartListenBTN
        ' 
        StartListenBTN.Location = New Point(93, 231)
        StartListenBTN.Name = "StartListenBTN"
        StartListenBTN.Size = New Size(75, 41)
        StartListenBTN.TabIndex = 4
        StartListenBTN.Text = "Start Listen"
        StartListenBTN.UseVisualStyleBackColor = True
        ' 
        ' StopListenBTN
        ' 
        StopListenBTN.Location = New Point(93, 278)
        StopListenBTN.Name = "StopListenBTN"
        StopListenBTN.Size = New Size(75, 41)
        StopListenBTN.TabIndex = 5
        StopListenBTN.Text = "Stop Listen"
        StopListenBTN.UseVisualStyleBackColor = True
        ' 
        ' RichTextBox1
        ' 
        RichTextBox1.Location = New Point(188, 231)
        RichTextBox1.Name = "RichTextBox1"
        RichTextBox1.Size = New Size(168, 88)
        RichTextBox1.TabIndex = 6
        RichTextBox1.Text = ""
        ' 
        ' ListenDGV
        ' 
        ListenDGV.AllowUserToAddRows = False
        ListenDGV.AllowUserToDeleteRows = False
        ListenDGV.AutoSizeColumnsMode = DataGridViewAutoSizeColumnsMode.Fill
        ListenDGV.ColumnHeadersHeightSizeMode = DataGridViewColumnHeadersHeightSizeMode.AutoSize
        ListenDGV.Columns.AddRange(New DataGridViewColumn() {Controller, timestamp, index, eventclnm, granted, door, direction, card, reason})
        ListenDGV.Location = New Point(12, 325)
        ListenDGV.Name = "ListenDGV"
        ListenDGV.ReadOnly = True
        ListenDGV.RowHeadersVisible = False
        ListenDGV.Size = New Size(917, 198)
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
        ' CardMgmtBTN
        ' 
        CardMgmtBTN.Location = New Point(658, 12)
        CardMgmtBTN.Name = "CardMgmtBTN"
        CardMgmtBTN.Size = New Size(88, 41)
        CardMgmtBTN.TabIndex = 8
        CardMgmtBTN.Text = "Card Management"
        CardMgmtBTN.UseVisualStyleBackColor = True
        ' 
        ' OpenDoorBTN
        ' 
        OpenDoorBTN.Location = New Point(334, 12)
        OpenDoorBTN.Name = "OpenDoorBTN"
        OpenDoorBTN.Size = New Size(75, 41)
        OpenDoorBTN.TabIndex = 10
        OpenDoorBTN.Text = "Open Door"
        OpenDoorBTN.UseVisualStyleBackColor = True
        ' 
        ' GetLisenerBTN
        ' 
        GetLisenerBTN.Location = New Point(124, 86)
        GetLisenerBTN.Name = "GetLisenerBTN"
        GetLisenerBTN.Size = New Size(75, 23)
        GetLisenerBTN.TabIndex = 11
        GetLisenerBTN.Text = "Get Lisener"
        GetLisenerBTN.UseVisualStyleBackColor = True
        ' 
        ' SetLisenerBTN
        ' 
        SetLisenerBTN.Location = New Point(124, 115)
        SetLisenerBTN.Name = "SetLisenerBTN"
        SetLisenerBTN.Size = New Size(75, 23)
        SetLisenerBTN.TabIndex = 12
        SetLisenerBTN.Text = "Set Lisener"
        SetLisenerBTN.UseVisualStyleBackColor = True
        ' 
        ' SetAddressBTN
        ' 
        SetAddressBTN.Location = New Point(205, 13)
        SetAddressBTN.Name = "SetAddressBTN"
        SetAddressBTN.Size = New Size(75, 40)
        SetAddressBTN.TabIndex = 13
        SetAddressBTN.Text = "Set Address"
        SetAddressBTN.UseVisualStyleBackColor = True
        ' 
        ' GetDoorControlBTN
        ' 
        GetDoorControlBTN.Location = New Point(415, 12)
        GetDoorControlBTN.Name = "GetDoorControlBTN"
        GetDoorControlBTN.Size = New Size(75, 41)
        GetDoorControlBTN.TabIndex = 14
        GetDoorControlBTN.Text = "Get Door Control"
        GetDoorControlBTN.UseVisualStyleBackColor = True
        ' 
        ' SetDoorControlBTN
        ' 
        SetDoorControlBTN.Location = New Point(496, 12)
        SetDoorControlBTN.Name = "SetDoorControlBTN"
        SetDoorControlBTN.Size = New Size(75, 41)
        SetDoorControlBTN.TabIndex = 15
        SetDoorControlBTN.Text = "Set Door Control"
        SetDoorControlBTN.UseVisualStyleBackColor = True
        ' 
        ' GetTimeBTN
        ' 
        GetTimeBTN.Location = New Point(205, 86)
        GetTimeBTN.Name = "GetTimeBTN"
        GetTimeBTN.Size = New Size(75, 23)
        GetTimeBTN.TabIndex = 16
        GetTimeBTN.Text = "Get Time"
        GetTimeBTN.UseVisualStyleBackColor = True
        ' 
        ' SetTimeBTN
        ' 
        SetTimeBTN.Location = New Point(205, 115)
        SetTimeBTN.Name = "SetTimeBTN"
        SetTimeBTN.Size = New Size(75, 23)
        SetTimeBTN.TabIndex = 17
        SetTimeBTN.Text = "Set Time"
        SetTimeBTN.UseVisualStyleBackColor = True
        ' 
        ' Uhppotegui
        ' 
        AutoScaleDimensions = New SizeF(7F, 15F)
        AutoScaleMode = AutoScaleMode.Font
        ClientSize = New Size(944, 534)
        Controls.Add(SetTimeBTN)
        Controls.Add(GetTimeBTN)
        Controls.Add(SetDoorControlBTN)
        Controls.Add(GetDoorControlBTN)
        Controls.Add(SetAddressBTN)
        Controls.Add(SetLisenerBTN)
        Controls.Add(GetLisenerBTN)
        Controls.Add(OpenDoorBTN)
        Controls.Add(CardMgmtBTN)
        Controls.Add(ListenDGV)
        Controls.Add(RichTextBox1)
        Controls.Add(StopListenBTN)
        Controls.Add(StartListenBTN)
        Controls.Add(GetStatusBTN)
        Controls.Add(ControllerListBox)
        Controls.Add(GetControllersBTN)
        Controls.Add(GetControllerBTN)
        Name = "Uhppotegui"
        StartPosition = FormStartPosition.CenterScreen
        Text = "uhppote-GUI"
        CType(ListenDGV, ComponentModel.ISupportInitialize).EndInit()
        ResumeLayout(False)
    End Sub

    Friend WithEvents GetControllerBTN As Button
    Friend WithEvents GetControllersBTN As Button
    Friend WithEvents ControllerListBox As ListBox
    Friend WithEvents GetStatusBTN As Button
    Friend WithEvents StartListenBTN As Button
    Friend WithEvents StopListenBTN As Button
    Friend WithEvents RichTextBox1 As RichTextBox
    Friend WithEvents ListenDGV As DataGridView
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
    Friend WithEvents OpenDoorBTN As Button
    Friend WithEvents GetLisenerBTN As Button
    Friend WithEvents SetLisenerBTN As Button
    Friend WithEvents SetAddressBTN As Button
    Friend WithEvents GetDoorControlBTN As Button
    Friend WithEvents SetDoorControlBTN As Button
    Friend WithEvents GetTimeBTN As Button
    Friend WithEvents SetTimeBTN As Button

End Class
