program MapEditorWeb64;
{$APPTYPE GUI}

{$R 'Product_Version.res' 'Product_Version.rc'}

uses
//  Vcl.Forms,
  SysUtils,
  Vcl.SvcMgr,
  Web.WebReq,
  Ils.Utils.Svc in '..\_Common\Ils.Utils.Svc.pas',
  Ils.Logger in '..\_Common\Ils.Logger.pas',
  IdHTTPWebBrokerBridge,
  IlsMapEditorWebModule in 'IlsMapEditorWebModule.pas' {getcover: TWebModule},
  UIlsMapEditorServiceFrm in 'UIlsMapEditorServiceFrm.pas' {ILSSaaSMapEditorWebService: TService},
  AStar64.DynImport in '..\AStar\AStar64.DynImport.pas',
  UMapEditor in 'UMapEditor.pas',
  uZC in 'uZC.pas',
  uSign in 'uSign.pas',
  uGeneral in 'uGeneral.pas',
  uSpeed in 'uSpeed.pas',
  uLandMark in 'uLandMark.pas',
  LandmarkPathCalculator in '..\AStar\LandmarkPathCalculator.pas';

const
IMAGE_FILE_LARGE_ADDRESS_AWARE = $0020;

{$SetPEFlags IMAGE_FILE_LARGE_ADDRESS_AWARE}

//{$SetPEFlags IMAGE_FILE_LARGE_ADDRESS_AWARE}
var
  ConfigParam: string;

begin
  ConfigParam := GetParam('config');
  StartLog(ConfigParam);

  if WebRequestHandler <> nil then
    WebRequestHandler.WebModuleClass := WebModuleClass;

  if not Application.DelayInitialize or Application.Installing then
    Application.Initialize;
  Application.CreateForm(TILSSaaSMapEditorWebService, ILSSaaSMapEditorWebService);
  if (not FindCmdLineSwitch('install')) or (not FindCmdLineSwitch('uninstall')) then
  begin
    if ConfigParam <> '' then
      ILSSaaSMapEditorWebService.Name := CServiceName + '_' + ConfigParam;
  end;
  Application.Run;
end.
