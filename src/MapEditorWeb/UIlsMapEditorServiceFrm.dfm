object ILSSaaSMapEditorWebService: TILSSaaSMapEditorWebService
  OldCreateOrder = False
  AllowPause = False
  DisplayName = 'ILS SaaS MapEditor Web'
  BeforeInstall = ServiceBeforeInstall
  AfterInstall = ServiceAfterInstall
  BeforeUninstall = ServiceBeforeInstall
  OnShutdown = ServiceShutdown
  OnStart = ServiceStart
  OnStop = ServiceStop
  Height = 348
  Width = 500
end
