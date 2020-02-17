program ILSAstarWebServerStandalone;

{$APPTYPE GUI}

uses
  Vcl.SvcMgr,
  Web.WebReq,
  IdHTTPWebBrokerBridge,
  Windows,
  UIlsAstarServiceFrm in 'UIlsAstarServiceFrm.pas' {ILSAstarWebService},
  UIlsAstarWebModule in 'UIlsAstarWebModule.pas' {wmAstar: TWebModule},
  AStar64.DynImport in '..\AStar64.DynImport.pas',
  AStar64.Typ in '..\AStar64.Typ.pas',
  AStar64.Common in '..\AStar64.Common.pas',
  Ils.Utils.Debug in '..\..\_Common\Ils.Utils.Debug.pas',
  Ils.Logger in '..\..\_Common\Ils.Logger.pas',
  JsonDataObjects in '..\..\..\lib\Json\JsonDataObjects.pas',
  Geo.Pos in '..\..\_Common\Geo.Pos.pas',
  Geo.Hash in '..\..\_Common\Geo.Hash.pas',
  Geo.Calcs in '..\..\_Common\Geo.Calcs.pas',
  Geo.Hash.Search in '..\..\Processor\TrackGeoLinkProcessor\Common\Geo.Hash.Search.pas';

{$R *.res}

{$SetPEFlags IMAGE_FILE_LARGE_ADDRESS_AWARE}

begin
  if WebRequestHandler <> nil then
    WebRequestHandler.WebModuleClass := WebModuleClass;
  if not Application.DelayInitialize or Application.Installing then
    Application.Initialize;
  Application.CreateForm(TILSAstarWebService, ILSAstarWebService);
  Application.Run;
end.

