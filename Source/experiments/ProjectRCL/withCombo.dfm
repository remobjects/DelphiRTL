object Form6: TForm6
  Left = 0
  Top = 0
  Caption = 'Form2'
  ClientHeight = 299
  ClientWidth = 635
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object ListBox1: TListBox
    Left = 368
    Top = 40
    Width = 121
    Height = 97
    ItemHeight = 13
    TabOrder = 0
  end
  object ComboBox1: TComboBox
    Left = 368
    Top = 184
    Width = 145
    Height = 21
    TabOrder = 1
    Text = 'ComboBox1'
    OnSelect = ComboBox1Select    
  end
  object Edit1: TEdit
    Left = 152
    Top = 96
    Width = 121
    Height = 21
    TabOrder = 2
    Text = 'Edit1'
  end
  object Button1: TButton
    Left = 112
    Top = 152
    Width = 75
    Height = 25
    Caption = 'Button1'
    TabOrder = 3
    OnClick = Button1Click
  end
end
