uses
  SysUtils,
  AStar64.Intf in 'AStar64.Intf.pas',
  AStar64.Extra in 'AStar64.Extra.pas',
  AStar64.Core in 'AStar64.Core.pas',
  AStar64.FileStructs in '..\_Common\AStar64.FileStructs.pas',
  UGeoHash in '..\_Common\UGeoHash.pas',
  UGeoSimple in '..\_Common\UGeoSimple.pas',
  UStructArray in '..\_Common\UStructArray.pas',
  UFiles in '..\_Common\UFiles.pas',
  AStar64.Common in 'AStar64.Common.pas',
  AStar64.Areas in '..\_Common\AStar64.Areas.pas',
  Geo.Pos in '..\_Common\Geo.Pos.pas';

//  ,ULogger in '..\_Common\ULogger.pas';


//------------------------------------------------------------------------------
exports
  CreateRoute,
  CreateRouteWithPath,
  CreateRouteWithSpeedline,
  CreateRouteWithPath2,
  CreateRouteWithSpeedline2,
  CleanupMem,
  GetRoadSpeedsDefault,
  GetRoadSpeedsKToDef,
  CreateRouteWithPath3;

//------------------------------------------------------------------------------
begin
  IsMultiThread := True;
{$WARN SYMBOL_PLATFORM OFF}
  SetMinimumBlockAlignment(mba16Byte);
{$WARN SYMBOL_PLATFORM DEFAULT}
end.
