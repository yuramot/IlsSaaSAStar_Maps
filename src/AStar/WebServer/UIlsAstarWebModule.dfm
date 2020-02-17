object wmAstar: TwmAstar
  OldCreateOrder = False
  Actions = <
    item
      Default = True
      Name = 'DefaultHandler'
      PathInfo = '/'
      OnAction = WebModule2DefaultHandlerAction
    end
    item
      Name = 'GetRoute'
      PathInfo = '/GetRoute'
      OnAction = wmAstarGetRouteAction
    end
    item
      MethodType = mtPost
      Name = 'GetRoute2'
      PathInfo = '/GetRoute2'
      OnAction = wmAstarGetRoute2Action
    end
    item
      Name = 'GetSpeedDefault'
      PathInfo = '/GetSpeedDefault'
      OnAction = wmAstarGetSpeedDefaultAction
    end
    item
      MethodType = mtPost
      Name = 'GetRouteNearestEdges'
      PathInfo = '/GetRouteNearestEdges'
      OnAction = wmAstarGetRouteNearestEdgesAction
    end
    item
      MethodType = mtPost
      Name = 'GetRouteTZ'
      PathInfo = '/GetRouteTZ'
      OnAction = wmAstarGetRouteTZAction
    end>
  Height = 230
  Width = 415
end
