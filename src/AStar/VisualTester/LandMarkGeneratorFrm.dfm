object MainForm: TMainForm
  Left = 0
  Top = 0
  Caption = #1056#1077#1076#1072#1082#1090#1086#1088' '#1084#1072#1103#1082#1086#1074
  ClientHeight = 572
  ClientWidth = 1208
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -16
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 19
  object Splitter1: TSplitter
    Left = 973
    Top = 65
    Height = 471
    Align = alRight
  end
  object pMap: TPanel
    Left = 0
    Top = 65
    Width = 973
    Height = 471
    Align = alClient
    BevelOuter = bvNone
    Constraints.MinHeight = 256
    TabOrder = 0
  end
  object sbMain: TStatusBar
    Left = 0
    Top = 553
    Width = 1208
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
    Width = 1208
    Height = 65
    Align = alTop
    TabOrder = 2
    object Edit1: TEdit
      Left = 8
      Top = 32
      Width = 493
      Height = 27
      TabOrder = 0
      OnChange = Edit1Change
    end
    object Button1: TButton
      Left = 729
      Top = 32
      Width = 154
      Height = 27
      Caption = #1056#1040#1057#1063#1025#1058' '#1052#1040#1071#1050#1054#1042
      TabOrder = 1
      OnClick = Button1Click
    end
    object Button3: TButton
      Left = 929
      Top = 32
      Width = 139
      Height = 27
      Caption = #1047#1072#1075#1088#1091#1079#1080#1090#1100' '#1079#1086#1085#1099
      TabOrder = 2
      OnClick = Button3Click
    end
    object Button4: TButton
      Left = 1112
      Top = 7
      Width = 49
      Height = 25
      Caption = 'Pin'
      TabOrder = 3
      OnClick = Button4Click
    end
    object edtPin: TEdit
      Left = 976
      Top = 6
      Width = 121
      Height = 27
      TabOrder = 4
    end
    object Button5: TButton
      Left = 1167
      Top = 7
      Width = 26
      Height = 25
      Caption = 'X'
      TabOrder = 5
      OnClick = Button5Click
    end
    object CheckBox1: TCheckBox
      Left = 16
      Top = 9
      Width = 131
      Height = 17
      Caption = #1058#1077#1089#1090#1080#1088#1086#1074#1072#1085#1080#1077
      TabOrder = 6
    end
    object Button2: TButton
      Left = 313
      Top = 4
      Width = 188
      Height = 27
      Caption = #1086#1073#1084#1077#1085#1103#1090#1100' '#1080#1079'/'#1074
      TabOrder = 7
      OnClick = bChangeClick
    end
    object Button6: TButton
      Left = 153
      Top = 4
      Width = 154
      Height = 27
      Caption = 'AStar'
      TabOrder = 8
      OnClick = bMakePathClick
    end
  end
  object ProgressBar1: TProgressBar
    Left = 0
    Top = 536
    Width = 1208
    Height = 17
    Align = alBottom
    TabOrder = 3
  end
  object Memo1: TMemo
    Left = 976
    Top = 65
    Width = 232
    Height = 471
    Align = alRight
    Lines.Strings = (
      'Memo1')
    TabOrder = 4
  end
end
