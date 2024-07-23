<Global.Microsoft.VisualBasic.CompilerServices.DesignerGenerated()> _
Partial Class CardMgmtFRM
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
        DataGridView1 = New DataGridView()
        AddCardBTN = New Button()
        DelCardBTN = New Button()
        TextBox1 = New TextBox()
        Panel1 = New Panel()
        Label5 = New Label()
        SaveBTN = New Button()
        Label4 = New Label()
        Label3 = New Label()
        Label2 = New Label()
        TextBox2 = New TextBox()
        MaskedTextBox2 = New MaskedTextBox()
        MaskedTextBox1 = New MaskedTextBox()
        CheckBox4 = New CheckBox()
        CheckBox3 = New CheckBox()
        CheckBox2 = New CheckBox()
        CheckBox1 = New CheckBox()
        ListBox1 = New ListBox()
        Label1 = New Label()
        CloseBTN = New Button()
        DACFCBTN = New Button()
        CType(DataGridView1, ComponentModel.ISupportInitialize).BeginInit()
        Panel1.SuspendLayout()
        SuspendLayout()
        ' 
        ' DataGridView1
        ' 
        DataGridView1.AllowUserToAddRows = False
        DataGridView1.AllowUserToDeleteRows = False
        DataGridView1.AutoSizeColumnsMode = DataGridViewAutoSizeColumnsMode.Fill
        DataGridView1.ColumnHeadersHeightSizeMode = DataGridViewColumnHeadersHeightSizeMode.AutoSize
        DataGridView1.Location = New Point(12, 234)
        DataGridView1.MultiSelect = False
        DataGridView1.Name = "DataGridView1"
        DataGridView1.ReadOnly = True
        DataGridView1.RowHeadersVisible = False
        DataGridView1.SelectionMode = DataGridViewSelectionMode.FullRowSelect
        DataGridView1.Size = New Size(776, 204)
        DataGridView1.TabIndex = 0
        ' 
        ' AddCardBTN
        ' 
        AddCardBTN.Location = New Point(21, 37)
        AddCardBTN.Name = "AddCardBTN"
        AddCardBTN.Size = New Size(75, 23)
        AddCardBTN.TabIndex = 1
        AddCardBTN.Text = "Add card"
        AddCardBTN.UseVisualStyleBackColor = True
        ' 
        ' DelCardBTN
        ' 
        DelCardBTN.Location = New Point(114, 37)
        DelCardBTN.Name = "DelCardBTN"
        DelCardBTN.Size = New Size(75, 23)
        DelCardBTN.TabIndex = 2
        DelCardBTN.Text = "Dalete card"
        DelCardBTN.UseVisualStyleBackColor = True
        ' 
        ' TextBox1
        ' 
        TextBox1.Location = New Point(102, 108)
        TextBox1.Name = "TextBox1"
        TextBox1.Size = New Size(100, 23)
        TextBox1.TabIndex = 3
        ' 
        ' Panel1
        ' 
        Panel1.Controls.Add(Label5)
        Panel1.Controls.Add(SaveBTN)
        Panel1.Controls.Add(Label4)
        Panel1.Controls.Add(Label3)
        Panel1.Controls.Add(Label2)
        Panel1.Controls.Add(TextBox2)
        Panel1.Controls.Add(MaskedTextBox2)
        Panel1.Controls.Add(MaskedTextBox1)
        Panel1.Controls.Add(CheckBox4)
        Panel1.Controls.Add(CheckBox3)
        Panel1.Controls.Add(CheckBox2)
        Panel1.Controls.Add(CheckBox1)
        Panel1.Controls.Add(ListBox1)
        Panel1.Controls.Add(Label1)
        Panel1.Controls.Add(TextBox1)
        Panel1.Location = New Point(12, 66)
        Panel1.Name = "Panel1"
        Panel1.Size = New Size(776, 140)
        Panel1.TabIndex = 7
        Panel1.Visible = False
        ' 
        ' Label5
        ' 
        Label5.Location = New Point(327, 54)
        Label5.Name = "Label5"
        Label5.Size = New Size(58, 51)
        Label5.TabIndex = 18
        Label5.Text = "(Leave blank for 1 year)"
        ' 
        ' SaveBTN
        ' 
        SaveBTN.Location = New Point(688, 17)
        SaveBTN.Name = "SaveBTN"
        SaveBTN.Size = New Size(75, 23)
        SaveBTN.TabIndex = 17
        SaveBTN.Text = "Save"
        SaveBTN.UseVisualStyleBackColor = True
        ' 
        ' Label4
        ' 
        Label4.AutoSize = True
        Label4.Location = New Point(538, 81)
        Label4.Name = "Label4"
        Label4.Size = New Size(153, 15)
        Label4.TabIndex = 16
        Label4.Text = "PIN (leave blank for no PIN)"
        ' 
        ' Label3
        ' 
        Label3.AutoSize = True
        Label3.Location = New Point(322, 39)
        Label3.Name = "Label3"
        Label3.Size = New Size(47, 15)
        Label3.TabIndex = 15
        Label3.Text = "Valid To"
        ' 
        ' Label2
        ' 
        Label2.AutoSize = True
        Label2.Location = New Point(223, 81)
        Label2.Name = "Label2"
        Label2.Size = New Size(63, 15)
        Label2.TabIndex = 14
        Label2.Text = "Valid From"
        ' 
        ' TextBox2
        ' 
        TextBox2.Location = New Point(571, 108)
        TextBox2.Name = "TextBox2"
        TextBox2.Size = New Size(100, 23)
        TextBox2.TabIndex = 13
        ' 
        ' MaskedTextBox2
        ' 
        MaskedTextBox2.Location = New Point(322, 108)
        MaskedTextBox2.Mask = "0000/00/00"
        MaskedTextBox2.Name = "MaskedTextBox2"
        MaskedTextBox2.Size = New Size(62, 23)
        MaskedTextBox2.TabIndex = 12
        ' 
        ' MaskedTextBox1
        ' 
        MaskedTextBox1.Location = New Point(223, 108)
        MaskedTextBox1.Mask = "0000-00-00"
        MaskedTextBox1.Name = "MaskedTextBox1"
        MaskedTextBox1.Size = New Size(63, 23)
        MaskedTextBox1.TabIndex = 11
        ' 
        ' CheckBox4
        ' 
        CheckBox4.AutoSize = True
        CheckBox4.Location = New Point(428, 112)
        CheckBox4.Name = "CheckBox4"
        CheckBox4.Size = New Size(61, 19)
        CheckBox4.TabIndex = 10
        CheckBox4.Text = "Door 4"
        CheckBox4.UseVisualStyleBackColor = True
        ' 
        ' CheckBox3
        ' 
        CheckBox3.AutoSize = True
        CheckBox3.Location = New Point(428, 91)
        CheckBox3.Name = "CheckBox3"
        CheckBox3.Size = New Size(61, 19)
        CheckBox3.TabIndex = 9
        CheckBox3.Text = "Door 3"
        CheckBox3.UseVisualStyleBackColor = True
        ' 
        ' CheckBox2
        ' 
        CheckBox2.AutoSize = True
        CheckBox2.Location = New Point(428, 66)
        CheckBox2.Name = "CheckBox2"
        CheckBox2.Size = New Size(61, 19)
        CheckBox2.TabIndex = 8
        CheckBox2.Text = "Door 2"
        CheckBox2.UseVisualStyleBackColor = True
        ' 
        ' CheckBox1
        ' 
        CheckBox1.AutoSize = True
        CheckBox1.Location = New Point(428, 41)
        CheckBox1.Name = "CheckBox1"
        CheckBox1.Size = New Size(61, 19)
        CheckBox1.TabIndex = 7
        CheckBox1.Text = "Door 1"
        CheckBox1.UseVisualStyleBackColor = True
        ' 
        ' ListBox1
        ' 
        ListBox1.FormattingEnabled = True
        ListBox1.ItemHeight = 15
        ListBox1.Location = New Point(5, 7)
        ListBox1.Name = "ListBox1"
        ListBox1.Size = New Size(79, 124)
        ListBox1.TabIndex = 6
        ' 
        ' Label1
        ' 
        Label1.AutoSize = True
        Label1.Location = New Point(102, 81)
        Label1.Name = "Label1"
        Label1.Size = New Size(79, 15)
        Label1.TabIndex = 5
        Label1.Text = "Card Number"
        ' 
        ' CloseBTN
        ' 
        CloseBTN.Location = New Point(713, 37)
        CloseBTN.Name = "CloseBTN"
        CloseBTN.Size = New Size(75, 23)
        CloseBTN.TabIndex = 8
        CloseBTN.Text = "Close"
        CloseBTN.UseVisualStyleBackColor = True
        ' 
        ' DACFCBTN
        ' 
        DACFCBTN.Location = New Point(210, 37)
        DACFCBTN.Name = "DACFCBTN"
        DACFCBTN.Size = New Size(186, 23)
        DACFCBTN.TabIndex = 9
        DACFCBTN.Text = "Delete all cards from controller"
        DACFCBTN.UseVisualStyleBackColor = True
        ' 
        ' CardMgmtFRM
        ' 
        AutoScaleDimensions = New SizeF(7F, 15F)
        AutoScaleMode = AutoScaleMode.Font
        ClientSize = New Size(800, 450)
        Controls.Add(DACFCBTN)
        Controls.Add(CloseBTN)
        Controls.Add(Panel1)
        Controls.Add(DelCardBTN)
        Controls.Add(AddCardBTN)
        Controls.Add(DataGridView1)
        Name = "CardMgmtFRM"
        StartPosition = FormStartPosition.CenterScreen
        Text = "Card Management"
        CType(DataGridView1, ComponentModel.ISupportInitialize).EndInit()
        Panel1.ResumeLayout(False)
        Panel1.PerformLayout()
        ResumeLayout(False)
    End Sub

    Friend WithEvents DataGridView1 As DataGridView
    Friend WithEvents AddCardBTN As Button
    Friend WithEvents DelCardBTN As Button
    Friend WithEvents TextBox1 As TextBox
    Friend WithEvents Panel1 As Panel
    Friend WithEvents Label1 As Label
    Friend WithEvents ListBox1 As ListBox
    Friend WithEvents TextBox2 As TextBox
    Friend WithEvents MaskedTextBox2 As MaskedTextBox
    Friend WithEvents MaskedTextBox1 As MaskedTextBox
    Friend WithEvents CheckBox4 As CheckBox
    Friend WithEvents CheckBox3 As CheckBox
    Friend WithEvents CheckBox2 As CheckBox
    Friend WithEvents CheckBox1 As CheckBox
    Friend WithEvents Label4 As Label
    Friend WithEvents Label3 As Label
    Friend WithEvents Label2 As Label
    Friend WithEvents SaveBTN As Button
    Friend WithEvents Label5 As Label
    Friend WithEvents CloseBTN As Button
    Friend WithEvents DACFCBTN As Button
End Class
