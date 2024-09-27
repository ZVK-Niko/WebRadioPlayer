object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'Form1'
  ClientHeight = 433
  ClientWidth = 622
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  OnCreate = FormCreate
  TextHeight = 15
  object Label1: TLabel
    Left = 120
    Top = 32
    Width = 34
    Height = 15
    Caption = 'Label1'
  end
  object Button1: TButton
    Left = 96
    Top = 160
    Width = 75
    Height = 25
    Caption = 'ButtonPlay'
    TabOrder = 0
    OnClick = Button1Click
  end
  object Button2: TButton
    Left = 216
    Top = 160
    Width = 75
    Height = 25
    Caption = 'ButtonStop'
    TabOrder = 1
    OnClick = Button2Click
  end
  object ComboBox1: TComboBox
    Left = 128
    Top = 80
    Width = 145
    Height = 23
    TabOrder = 2
    Text = 'ComboBox1'
  end
end
