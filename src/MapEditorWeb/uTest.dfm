object Form1: TForm1
  Left = 271
  Top = 114
  Caption = 'ver 0.133'
  ClientHeight = 339
  ClientWidth = 781
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 24
    Top = 39
    Width = 20
    Height = 13
    Caption = 'Port'
  end
  object ButtonStart: TButton
    Left = 24
    Top = 8
    Width = 75
    Height = 25
    Caption = 'Start'
    TabOrder = 0
    OnClick = ButtonStartClick
  end
  object ButtonStop: TButton
    Left = 105
    Top = 8
    Width = 75
    Height = 25
    Caption = 'Stop'
    TabOrder = 1
    OnClick = ButtonStopClick
  end
  object EditPort: TEdit
    Left = 96
    Top = 39
    Width = 121
    Height = 21
    TabOrder = 2
    Text = '7653'
  end
  object ButtonOpenBrowser: TButton
    Left = 192
    Top = 8
    Width = 107
    Height = 25
    Caption = 'Open Browser'
    TabOrder = 3
    OnClick = ButtonOpenBrowserClick
  end
  object edt1: TEdit
    Left = 312
    Top = 8
    Width = 368
    Height = 21
    TabOrder = 4
  end
  object btn1: TButton
    Left = 686
    Top = 5
    Width = 75
    Height = 25
    Caption = 'Decode'
    TabOrder = 5
    OnClick = btn1Click
  end
  object mmo1: TMemo
    Left = 0
    Top = 152
    Width = 781
    Height = 187
    Align = alBottom
    Anchors = [akLeft, akTop, akRight, akBottom]
    TabOrder = 6
  end
  object edtRectangle: TEdit
    Left = 310
    Top = 35
    Width = 329
    Height = 21
    TabOrder = 7
    Text = '55.699175 37.735642 55.620468 37.896236'
    Visible = False
  end
  object btn2: TButton
    Left = 706
    Top = 90
    Width = 75
    Height = 25
    Caption = 'get restangle'
    TabOrder = 8
    Visible = False
    OnClick = btn2Click
  end
  object btn3: TButton
    Left = 24
    Top = 62
    Width = 75
    Height = 25
    Caption = 'Save Names'
    TabOrder = 9
    Visible = False
    OnClick = btn3Click
  end
  object btn4: TButton
    Left = 112
    Top = 62
    Width = 75
    Height = 25
    Caption = 'Load Dict'
    TabOrder = 10
    Visible = False
    OnClick = btn4Click
  end
  object btn5: TButton
    Left = 193
    Top = 62
    Width = 75
    Height = 25
    Caption = 'Save name'
    TabOrder = 11
    Visible = False
  end
  object btn6: TButton
    Left = 274
    Top = 62
    Width = 75
    Height = 25
    Caption = 'Save sign'
    TabOrder = 12
    Visible = False
    OnClick = btn6Click
  end
  object btn7: TButton
    Left = 355
    Top = 62
    Width = 75
    Height = 25
    Caption = 'Save Acc sign'
    TabOrder = 13
    Visible = False
    OnClick = btn7Click
  end
  object btn8: TButton
    Left = 436
    Top = 62
    Width = 75
    Height = 25
    Caption = 'Save Bak'
    TabOrder = 14
    Visible = False
  end
  object Button1: TButton
    Left = 517
    Top = 62
    Width = 75
    Height = 25
    Caption = 'check way from edt'
    TabOrder = 15
    Visible = False
    OnClick = Button1Click
  end
  object Button2: TButton
    Left = 355
    Top = 89
    Width = 75
    Height = 25
    Caption = 'Delete road'
    TabOrder = 16
    Visible = False
  end
  object edDelete: TEdit
    Left = 8
    Top = 93
    Width = 341
    Height = 21
    TabOrder = 17
    Text = '5169713-949655913173837490-949655910406009122'
    Visible = False
  end
  object Button3: TButton
    Left = 436
    Top = 89
    Width = 75
    Height = 25
    Caption = 'Parse JSON'
    TabOrder = 18
    Visible = False
    OnClick = Button3Click
  end
  object Button4: TButton
    Left = 517
    Top = 89
    Width = 75
    Height = 25
    Caption = 'Delete 3 road'
    TabOrder = 19
    Visible = False
    OnClick = Button4Click
  end
  object Button5: TButton
    Left = 598
    Top = 89
    Width = 75
    Height = 25
    Caption = 'check way'
    TabOrder = 20
    Visible = False
    OnClick = Button5Click
  end
  object Button6: TButton
    Left = 679
    Top = 89
    Width = 75
    Height = 25
    Caption = 'Get from redis'
    TabOrder = 21
    Visible = False
    OnClick = Button6Click
  end
  object Button7: TButton
    Left = 598
    Top = 62
    Width = 75
    Height = 25
    Caption = 'Check way2'
    TabOrder = 22
    Visible = False
    OnClick = Button7Click
  end
  object Button8: TButton
    Left = 679
    Top = 62
    Width = 75
    Height = 25
    Caption = 'way to list file'
    TabOrder = 23
    Visible = False
    OnClick = Button8Click
  end
  object chkDll: TCheckBox
    Left = 224
    Top = 40
    Width = 75
    Height = 17
    Caption = 'AStar64.dll'
    Checked = True
    State = cbChecked
    TabOrder = 24
  end
  object btn9: TButton
    Left = 686
    Top = 31
    Width = 75
    Height = 25
    Caption = 'Points -> GW'
    TabOrder = 25
    OnClick = btn9Click
  end
  object btn10: TButton
    Left = 310
    Top = 120
    Width = 75
    Height = 25
    Caption = 'Read'
    TabOrder = 26
    Visible = False
    OnClick = btn10Click
  end
  object btn11: TButton
    Left = 686
    Top = 58
    Width = 75
    Height = 25
    Caption = 'Hash ->Points'
    TabOrder = 27
    OnClick = btn11Click
  end
  object btn13: TButton
    Left = 605
    Top = 121
    Width = 75
    Height = 25
    Caption = 'GW->points'
    TabOrder = 28
    OnClick = btn13Click
  end
  object btn12: TButton
    Left = 517
    Top = 120
    Width = 85
    Height = 25
    Caption = 'Save Areas.txt'
    TabOrder = 29
    OnClick = btn12Click
  end
  object btn14: TButton
    Left = 681
    Top = 120
    Width = 75
    Height = 25
    Caption = 'Load idx'
    TabOrder = 30
    OnClick = btn14Click
  end
  object btnSeparateRoute: TButton
    Left = 408
    Top = 120
    Width = 103
    Height = 25
    Caption = #1056#1072#1079#1076#1077#1083#1080#1090#1100' '#1076#1086#1088#1086#1075#1091
    TabOrder = 31
    OnClick = btnSeparateRouteClick
  end
  object btn15: TButton
    Left = 8
    Top = 120
    Width = 75
    Height = 25
    Caption = 'LM->Web'
    TabOrder = 32
    OnClick = btn15Click
  end
  object btn16: TButton
    Left = 89
    Top = 120
    Width = 75
    Height = 25
    Caption = #1040#1085#1072#1083#1080#1079' idx'
    TabOrder = 33
    OnClick = btn16Click
  end
  object btn17: TButton
    Left = 170
    Top = 120
    Width = 75
    Height = 25
    Caption = 'Test Astar'
    TabOrder = 34
    OnClick = btn17Click
  end
  object ApplicationEvents1: TApplicationEvents
    OnIdle = ApplicationEvents1Idle
    Left = 680
    Top = 208
  end
  object tmr1: TTimer
    OnTimer = tmr1Timer
    Left = 656
    Top = 152
  end
  object dlgOpen: TOpenDialog
    InitialDir = '.'
    Options = [ofHideReadOnly, ofAllowMultiSelect, ofEnableSizing]
    Left = 608
    Top = 152
  end
  object q1: TADQuery
    Connection = ADConnection1
    SQL.Strings = (
      'select latitude, longitude'
      'from track'
      'where imei=:im and dt>:d1 and dt<:d2'
      'order by dt')
    Left = 168
    Top = 176
    ParamData = <
      item
        Name = 'IM'
        ParamType = ptInput
      end
      item
        Name = 'D1'
        ParamType = ptInput
      end
      item
        Name = 'D2'
        ParamType = ptInput
      end>
  end
  object ADConnection1: TADConnection
    Params.Strings = (
      'Server=localhost'
      'Database=addr1'
      'User_Name=root2'
      'Password=SAsql123'
      'DriverID=MySQL')
    LoginPrompt = False
    Left = 64
    Top = 176
  end
  object ADGUIxWaitCursor1: TADGUIxWaitCursor
    Left = 328
    Top = 160
  end
  object ADPhysMySQLDriverLink1: TADPhysMySQLDriverLink
    VendorLib = 'C:\ILSSAAS\IlsSaasMonitoringBE\bin\Win32\Debug\igf\libmysql.dll'
    Left = 392
    Top = 160
  end
  object q2: TADQuery
    Connection = ADConnection1
    SQL.Strings = (
      'select distinct imei'
      'from track')
    Left = 168
    Top = 232
  end
end
