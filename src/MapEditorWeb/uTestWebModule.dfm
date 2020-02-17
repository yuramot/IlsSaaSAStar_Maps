object WebModule1: TWebModule1
  OldCreateOrder = False
  Actions = <
    item
      Default = True
      MethodType = mtPost
      Name = 'DefaultHandler'
      PathInfo = '/'
      OnAction = WebModule1DefaultHandlerAction
    end
    item
      Name = 'view'
      PathInfo = '/view'
    end
    item
      Name = 'newroad'
      PathInfo = '/newroad'
    end
    item
      Name = 'deleteroad'
      PathInfo = '/deleteroad'
    end>
  BeforeDispatch = WebModuleBeforeDispatch
  AfterDispatch = WebModuleAfterDispatch
  OnException = WebModuleException
  Height = 230
  Width = 415
end
