object ILS_CommonSender: TILS_CommonSender
  OldCreateOrder = False
  AllowPause = False
  DisplayName = 'ILS Common Sender'
  OnStart = ServiceStart
  OnStop = ServiceStop
  Height = 256
  Width = 512
  object tCheck: TTimer
    Interval = 5000
    OnTimer = tCheckTimer
    Left = 48
    Top = 48
  end
end
