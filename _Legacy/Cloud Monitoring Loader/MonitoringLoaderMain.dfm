object ILSLoader: TILSLoader
  OldCreateOrder = False
  AllowPause = False
  DisplayName = 'ILS Loading System'
  OnStart = ServiceStart
  OnStop = ServiceStop
  Height = 160
  Width = 320
end
