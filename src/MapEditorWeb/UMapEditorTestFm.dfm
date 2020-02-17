object Form3: TForm3
  Left = 0
  Top = 0
  Caption = 'Form3'
  ClientHeight = 320
  ClientWidth = 769
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object lbl1: TLabel
    Left = 0
    Top = 11
    Width = 43
    Height = 13
    Caption = 'Route ID'
  end
  object lbl2: TLabel
    Left = 0
    Top = 38
    Width = 48
    Height = 13
    Caption = 'Rectangle'
  end
  object edtID: TEdit
    Left = 56
    Top = 8
    Width = 329
    Height = 21
    TabOrder = 0
    Text = '309703813 949655908094091588 949654444947879977'
  end
  object btn1: TButton
    Left = 408
    Top = 8
    Width = 75
    Height = 25
    Caption = 'get way'
    TabOrder = 1
    OnClick = btn1Click
  end
  object mmo1: TMemo
    Left = 0
    Top = 136
    Width = 769
    Height = 184
    Align = alBottom
    Anchors = [akLeft, akTop, akRight, akBottom]
    Lines.Strings = (
      'mmo1')
    ScrollBars = ssVertical
    TabOrder = 2
  end
  object edtRectangle: TEdit
    Left = 56
    Top = 35
    Width = 329
    Height = 21
    TabOrder = 3
    Text = '55.699175 37.735642 55.670468 37.836236 15'
  end
  object btn2: TButton
    Left = 408
    Top = 36
    Width = 75
    Height = 25
    Caption = 'get restangle'
    TabOrder = 4
    OnClick = btn2Click
  end
  object edtRes: TEdit
    Left = 56
    Top = 62
    Width = 329
    Height = 21
    TabOrder = 5
  end
  object btn3: TButton
    Left = 496
    Top = 8
    Width = 75
    Height = 25
    Caption = 'set roadtype'
    TabOrder = 6
    OnClick = btn3Click
  end
  object btnStartWeb: TButton
    Left = 592
    Top = 8
    Width = 75
    Height = 25
    Caption = 'Start Server'
    TabOrder = 7
    OnClick = btnStartWebClick
  end
  object btn4: TButton
    Left = 673
    Top = 8
    Width = 75
    Height = 25
    Caption = 'Stop Server'
    TabOrder = 8
    OnClick = btn4Click
  end
  object btn5: TButton
    Left = 408
    Top = 67
    Width = 75
    Height = 25
    Caption = 'Json'
    TabOrder = 9
    OnClick = btn5Click
  end
end
