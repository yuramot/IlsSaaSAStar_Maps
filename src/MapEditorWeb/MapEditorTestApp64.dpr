program MapEditorTestApp64;
{$APPTYPE GUI}


uses
  Vcl.Forms,
  Web.WebReq,
  IdHTTPWebBrokerBridge,
  uTest in 'uTest.pas' {Form1},
  IlsMapEditorWebModule in 'IlsMapEditorWebModule.pas' {getcover: TWebModule},
  UMapEditor in 'UMapEditor.pas',
  uZC in 'uZC.pas',
  uSign in 'uSign.pas',
  uSearchEdge in 'uSearchEdge.pas',
  uGeneral in 'uGeneral.pas',
  uSpeed in 'uSpeed.pas',
  uLandMark in 'uLandMark.pas',
  clMMTimer in 'clMMTimer.pas',
  AStar64.Files in '..\..\IlsSaaSCommon\_Common\AStar64.Files.pas',
  LandmarkPathCalculator in '..\AStar\LandmarkPathCalculator.pas',
  AStar64.DynImport in '..\AStar\AStar64.DynImport.pas',
  AStar64.LandMark in '..\AStar\AStar64.LandMark.pas';

const
IMAGE_FILE_LARGE_ADDRESS_AWARE = $0020;

{$R *.res}
{$SetPEFlags IMAGE_FILE_LARGE_ADDRESS_AWARE}

begin
  if WebRequestHandler <> nil then
    WebRequestHandler.WebModuleClass := WebModuleClass;
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
