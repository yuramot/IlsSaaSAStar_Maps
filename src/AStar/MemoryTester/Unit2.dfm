object Form2: TForm2
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
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object mmo1: TMemo
    Left = 0
    Top = 41
    Width = 635
    Height = 258
    Align = alClient
    Lines.Strings = (
      'mmo1')
    TabOrder = 0
  end
  object pnl1: TPanel
    Left = 0
    Top = 0
    Width = 635
    Height = 41
    Align = alTop
    Caption = 'pnl1'
    TabOrder = 1
    object btn1: TButton
      Left = 16
      Top = 8
      Width = 75
      Height = 25
      Caption = 'btn1'
      TabOrder = 0
      OnClick = btn1Click
    end
  end
end
