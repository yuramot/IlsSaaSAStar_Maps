object wmMapEditor: TwmMapEditor
  OldCreateOrder = False
  OnCreate = WebModuleCreate
  OnDestroy = WebModuleDestroy
  Actions = <
    item
      Default = True
      MethodType = mtPost
      Name = 'DefaultHandler'
      PathInfo = '/'
      OnAction = wmMapEditorDefaultHandlerAction
    end
    item
      Name = 'view'
      PathInfo = '/view'
    end
    item
      Name = 'PatchRoute'
      PathInfo = '/PatchRoute'
    end>
  BeforeDispatch = WebModuleBeforeDispatch
  AfterDispatch = WebModuleAfterDispatch
  OnException = WebModuleException
  Height = 230
  Width = 415
end
