object FormFolderSelect: TFormFolderSelect
  Left = 0
  Top = 0
  BorderStyle = bsDialog
  Caption = #1042#1099#1073#1086#1088' '#1087#1072#1087#1082#1080
  ClientHeight = 384
  ClientWidth = 320
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  PixelsPerInch = 96
  TextHeight = 13
  object bOk: TcxButton
    Left = 0
    Top = 304
    Width = 320
    Height = 40
    Align = alBottom
    Caption = #1044#1072
    TabOrder = 1
    OnClick = bOkClick
  end
  object bCancel: TcxButton
    Left = 0
    Top = 344
    Width = 320
    Height = 40
    Align = alBottom
    Caption = #1054#1090#1084#1077#1085#1072
    TabOrder = 2
    OnClick = bCancelClick
  end
  object shtwFolder: TcxShellTreeView
    Left = 0
    Top = 0
    Width = 320
    Height = 304
    Align = alClient
    Indent = 19
    Options.ShowNonFolders = False
    Options.ContextMenus = False
    Options.TrackShellChanges = False
    Options.ShowToolTip = False
    RightClickSelect = True
    TabOrder = 0
  end
end
