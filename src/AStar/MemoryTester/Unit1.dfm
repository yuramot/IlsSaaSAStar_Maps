object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'Form2'
  ClientHeight = 299
  ClientWidth = 1076
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
  object mmo1: TMemo
    Left = 0
    Top = 41
    Width = 1076
    Height = 258
    Align = alClient
    TabOrder = 0
  end
  object pnl1: TPanel
    Left = 0
    Top = 0
    Width = 1076
    Height = 41
    Align = alTop
    TabOrder = 1
    object btn1: TButton
      Left = 16
      Top = 8
      Width = 75
      Height = 25
      Caption = 'Run Test'
      TabOrder = 0
      OnClick = btn1Click
    end
  end
end
