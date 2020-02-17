library AStar64x64;

{$R 'Product_Version_x64.res' 'Product_Version_x64.rc'}
{$R *.res}

uses
  SysUtils,
  AStar64.Intf in 'AStar64.Intf.pas',
  AStar64.Extra in 'AStar64.Extra.pas',
  AStar64.Core in 'AStar64.Core.pas',
  AStar64.FileStructs in '..\_Common\AStar64.FileStructs.pas',
  UStructArray in '..\_Common\UStructArray.pas',
  UFiles in '..\_Common\UFiles.pas',
  AStar64.Common in 'AStar64.Common.pas',
  AStar64.Areas in '..\_Common\AStar64.Areas.pas',
  Geo.Pos in '..\_Common\Geo.Pos.pas',
  Geo.Calcs in '..\_Common\Geo.Calcs.pas',
  Geo.Hash in '..\_Common\Geo.Hash.pas',
  Geo.Hash.Search in '..\_Common\Geo.Hash.Search.pas',
  AStar64.Typ in 'AStar64.Typ.pas',
  AStar64.Files in '..\_Common\AStar64.Files.pas',
  AStar64.LandMark in 'AStar64.LandMark.pas',
  Ils.Utils in '..\_Common\Ils.Utils.pas',
  Ils.Logger in '..\_Common\Ils.Logger.pas',
  Ils.Json.Names in '..\_Common\Ils.Json.Names.pas',
  Ils.Json.Utils in '..\_Common\Ils.Json.Utils.pas',
  JsonDataObjects in '..\..\lib\Json\JsonDataObjects.pas';

//------------------------------------------------------------------------------
exports
  CreateRoute,
  CreateRouteAcc,
  CreateRouteWithPath,
  CreateRouteWithPathAcc,
  CreateRouteWithSpeedline,
  CreateRouteWithSpeedlineAcc,
  CreateRouteWithPath2,
  CreateRouteWithPath2Acc,
  CreateRouteWithSpeedline2,
  CreateRouteWithSpeedline2Acc,
  CreateRouteWithPath3,
  CreateRouteWithPath3Acc,
  AStarCalc,
  AStarCalcAcc,
  AStarCalcLandmarks,
  AStarCalcLandmarksAcc,
  AStarCalcSign,
  AStarCalcSignAcc,
  AStarCalc4,
  AStarCalc4Acc,
  CleanupMem,
  GetRoadSpeedsDefault,
  GetRoadSpeedsKToDef;

//------------------------------------------------------------------------------
begin
  IsMultiThread := True;
{$WARN SYMBOL_PLATFORM OFF}
  SetMinimumBlockAlignment(mba16Byte);
{$WARN SYMBOL_PLATFORM DEFAULT}
end.

