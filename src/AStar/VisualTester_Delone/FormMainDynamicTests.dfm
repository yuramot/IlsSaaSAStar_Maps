object MainForm: TMainForm
  Left = 0
  Top = 0
  Caption = 'AStar.dll '#1087#1088#1086#1074#1077#1088#1103#1083#1100#1097#1080#1082
  ClientHeight = 572
  ClientWidth = 972
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -16
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  WindowState = wsMaximized
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 19
  object pMap: TPanel
    Left = 185
    Top = 41
    Width = 787
    Height = 512
    Align = alClient
    BevelOuter = bvNone
    Constraints.MinHeight = 256
    TabOrder = 0
  end
  object sbMain: TStatusBar
    Left = 0
    Top = 553
    Width = 972
    Height = 19
    Panels = <
      item
        Width = 256
      end
      item
        Width = 320
      end
      item
        Width = 50
      end>
  end
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 972
    Height = 41
    Align = alTop
    TabOrder = 2
    object Edit1: TEdit
      Left = 8
      Top = 6
      Width = 457
      Height = 27
      TabOrder = 0
      OnChange = Edit1Change
    end
    object Button1: TButton
      Left = 665
      Top = 6
      Width = 100
      Height = 28
      Caption = #1056#1040#1057#1063#1025#1058
      TabOrder = 1
      OnClick = bMakePathClick
    end
    object Button2: TButton
      Left = 471
      Top = 6
      Width = 188
      Height = 27
      Caption = #1086#1073#1084#1077#1085#1103#1090#1100' '#1080#1079'/'#1074
      TabOrder = 2
      OnClick = bChangeClick
    end
    object Button3: TButton
      Left = 808
      Top = 6
      Width = 139
      Height = 27
      Caption = #1047#1072#1075#1088#1091#1079#1080#1090#1100' '#1079#1086#1085#1099
      TabOrder = 3
      OnClick = Button3Click
    end
  end
  object pnl1: TPanel
    Left = 0
    Top = 41
    Width = 185
    Height = 512
    Align = alLeft
    Caption = 'pPoints'
    TabOrder = 3
    object mmoPoints: TMemo
      Left = 1
      Top = 26
      Width = 183
      Height = 485
      Align = alClient
      TabOrder = 0
    end
    object btn1: TButton
      Left = 1
      Top = 1
      Width = 183
      Height = 25
      Align = alTop
      Caption = #1053#1072#1088#1080#1089#1086#1074#1072#1090#1100' '#1086#1090#1088#1077#1079#1082#1080
      TabOrder = 1
      OnClick = btn1Click
    end
  end
end
