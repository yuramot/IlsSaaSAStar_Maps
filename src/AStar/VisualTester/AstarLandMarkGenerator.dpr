program AstarLandMarkGenerator;

uses
  Vcl.Forms,
  LandMarkGeneratorFrm in 'LandMarkGeneratorFrm.pas' {MainForm},
  ILSMapControlEx_TLB in 'ILSMapControlEx_TLB.pas',
  AStar64.DynImport in '..\AStar64.DynImport.pas',
  AStar64.Common in '..\AStar64.Common.pas',
  AStar64.Core in '..\AStar64.Core.pas',
  AStar64.Extra in '..\AStar64.Extra.pas',
  AStar64.Import in '..\AStar64.Import.pas',
  AStar64.Intf in '..\AStar64.Intf.pas',
  AStar64.Typ in '..\AStar64.Typ.pas',
  AStar64.LandMark in '..\AStar64.LandMark.pas',
  AStar64.FileStructs in '..\..\_Common\AStar64.FileStructs.pas',
  AStar64.Areas in '..\..\_Common\AStar64.Areas.pas',
  AStar64.Files in '..\..\_Common\AStar64.Files.pas',
  Geo.Pos in '..\..\_Common\Geo.Pos.pas',
  Geo.Calcs in '..\..\_Common\Geo.Calcs.pas',
  Geo.Hash in '..\..\_Common\Geo.Hash.pas',
  Geo.Hash.Search in '..\..\_Common\Geo.Hash.Search.pas',
  UStructArray in '..\..\_Common\UStructArray.pas',
  Ils.Utils in '..\..\_Common\Ils.Utils.pas',
  Ils.Logger in '..\..\_Common\Ils.Logger.pas',
  Ils.Json.Names in '..\..\_Common\Ils.Json.Names.pas',
  Ils.Json.Utils in '..\..\_Common\Ils.Json.Utils.pas',
  JsonDataObjects in '..\..\..\lib\Json\JsonDataObjects.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.

