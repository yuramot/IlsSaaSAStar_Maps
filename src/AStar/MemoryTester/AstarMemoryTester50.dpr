program AstarMemoryTester50;
{.$define FullDebugMode}
{.$define LogMemoryLeakDetailToFile}
{.$define EnableMemoryLeakReporting}

uses
  Vcl.Forms,
  Unit2 in 'Unit2.pas' {Form2},
  AStar64.Common in '..\AStar64.Common.pas',
  AStar64.Core in '..\AStar64.Core.pas',
  AStar64.Extra in '..\AStar64.Extra.pas',
  AStar64.Areas in '..\..\_Common\AStar64.Areas.pas',
  AStar64.FileStructs in '..\..\_Common\AStar64.FileStructs.pas',
  Geo.Hash in '..\..\_Common\Geo.Hash.pas',
  UStructArray in '..\..\_Common\UStructArray.pas',
  Geo.Pos in '..\..\_Common\Geo.Pos.pas',
  AStar64.Typ in '..\AStar64.Typ.pas',
  AStar64.Intf in '..\AStar64.Intf.pas',
  Geo.Calcs in '..\..\_Common\Geo.Calcs.pas',
  Ils.Logger in '..\..\_Common\Ils.Logger.pas',
  AStar64.Files in '..\..\_Common\AStar64.Files.pas',
  AStar64.LandMark in '..\AStar64.LandMark.pas' {$R *.res},
  Ils.Json.Names in '..\..\_Common\Ils.Json.Names.pas',
  Ils.Json.Utils in '..\..\_Common\Ils.Json.Utils.pas',
  JsonDataObjects in '..\..\..\lib\Json\JsonDataObjects.pas',
  Geo.Hash.Search in '..\..\_Common\Geo.Hash.Search.pas';

{$R *.res}

begin
  ReportMemoryLeaksOnShutdown := True;

//FullDebugMode
//LogMemoryLeakDetailToFile
//EnableMemoryLeakReporting



  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm2, Form2);
  Application.Run;
end.





