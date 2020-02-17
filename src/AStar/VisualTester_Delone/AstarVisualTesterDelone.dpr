program AstarVisualTesterDelone;

uses
  Vcl.Forms,
  FormMainDynamicTests in 'FormMainDynamicTests.pas' {MainForm},
  ILSMapControlEx_TLB in 'ILSMapControlEx_TLB.pas',
  AStar64.Common in '..\AStar64.Common.pas',
  AStar64.DynImport in 'AStar64.DynImport.pas',
  UGeoHash in '..\..\_Common\UGeoHash.pas',
  ULogger in '..\..\_Common\ULogger.pas',
  Geo.Pos in '..\..\_Common\Geo.Pos.pas',
  Delaunay in 'Delaunay.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.

