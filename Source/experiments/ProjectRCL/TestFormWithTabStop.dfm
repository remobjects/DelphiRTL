object Form6: TForm6
  Left = 0
  Top = 0
  Caption = 'The Test Application!'
  ClientHeight = 343
  ClientWidth = 634
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 24
    Top = 64
    Width = 31
    Height = 13
    Caption = 'Name:'
  end
  object Label2: TLabel
    Left = 24
    Top = 96
    Width = 46
    Height = 13
    Caption = 'Surname:'
  end
  object Label3: TLabel
    Left = 24
    Top = 128
    Width = 28
    Height = 13
    Caption = 'Type:'
  end
  object Label5: TLabel
    Left = 24
    Top = 168
    Width = 44
    Height = 13
    Caption = 'Location:'
  end
  object Label6: TLabel
    Left = 272
    Top = 215
    Width = 54
    Height = 13
    Caption = 'Comments:'
  end
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 634
    Height = 41
    Align = alTop
    TabOrder = 0
    ExplicitLeft = -8
    ExplicitTop = 64
    ExplicitWidth = 635
    object Label4: TLabel
      Left = 176
      Top = 4
      Width = 227
      Height = 25
      Caption = 'Edit Customer Record'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -21
      Font.Name = 'Tahoma'
      Font.Style = [fsBold]
      ParentFont = False
    end
  end
  object Panel2: TPanel
    Left = 0
    Top = 292
    Width = 634
    Height = 51
    Align = alBottom
    TabOrder = 7
    ExplicitTop = 248
    ExplicitWidth = 635
    object Button1: TButton
      Left = 448
      Top = 16
      Width = 75
      Height = 25
      Caption = 'OK'
      TabOrder = 0
    end
    object Button2: TButton
      Left = 544
      Top = 16
      Width = 75
      Height = 25
      Caption = 'Cancel'
      TabOrder = 1
    end
  end
  object Edit1: TEdit
    Left = 80
    Top = 61
    Width = 153
    Height = 21
    TabOrder = 1
  end
  object Edit2: TEdit
    Left = 80
    Top = 93
    Width = 153
    Height = 21
    TabOrder = 2
  end
  object ComboBox1: TComboBox
    Left = 80
    Top = 125
    Width = 153
    Height = 21
    TabOrder = 3
    Items.Strings = (
      'Food'
      'Drinks'
      'Insurance')
  end
  object ListBox1: TListBox
    Left = 80
    Top = 168
    Width = 153
    Height = 97
    ItemHeight = 13
    Items.Strings = (
      'Nevada'
      'California'
      'Kentucky'
      'North Carolina'
      'South Carolina'
      'Alaska'
      'Canada (yes!!)')
    TabOrder = 4
  end
  object GroupBox1: TGroupBox
    Left = 272
    Top = 61
    Width = 147
    Height = 148
    Caption = 'Special Chars'
    TabOrder = 5
    object CheckBox1: TCheckBox
      Left = 24
      Top = 34
      Width = 97
      Height = 17
      Caption = 'Risk'
      TabOrder = 0
    end
    object CheckBox2: TCheckBox
      Left = 24
      Top = 57
      Width = 97
      Height = 17
      Caption = 'Pay on site'
      TabOrder = 1
    end
    object CheckBox3: TCheckBox
      Left = 24
      Top = 80
      Width = 97
      Height = 17
      Caption = 'One day delivery'
      TabOrder = 2
    end
    object CheckBox4: TCheckBox
      Left = 24
      Top = 106
      Width = 97
      Height = 17
      Caption = 'Priority check'
      TabOrder = 3
    end
  end
  object Edit3: TEdit
    Left = 272
    Top = 244
    Width = 147
    Height = 21
    TabOrder = 6
  end
end
