unit AStar64.DynImport;
//------------------------------------------------------------------------------
// модуль динамического импорта функций AStar.dll
//------------------------------------------------------------------------------

//------------------------------------------------------------------------------
interface

uses
  SysUtils, StrUtils, Windows,
  AStar64.Typ, AStar64.Common;

//------------------------------------------------------------------------------
var

//------------------------------------------------------------------------------
//! экспортируемые ф-ции расчёта
//------------------------------------------------------------------------------

CreateRoute: function(
  const AHandle: Pointer;
  const ASpeedCB: TSpeedCallbackFunc;
  const AFromLatitude: Double;
  const AFromLongitude: Double;
  const AToLatitude: Double;
  const AToLongitude: Double;
  var RDistance: Double;
  var RDuration: Double
): Integer; stdcall;

CreateRouteAcc: function(
  const AHandle: Pointer;
  const ASpeedCB: TSpeedCallbackFunc;
  const AFromLatitude: Double;
  const AFromLongitude: Double;
  const AToLatitude: Double;
  const AToLongitude: Double;
  var RDistance: Double;
  var RDuration: Double;
  const AAccs: PInteger
): Integer; stdcall;

CreateRouteWithPath: function(
  const AHandle: Pointer;
  const ASpeedCB: TSpeedCallbackFunc;
  const AFromLatitude: Double;
  const AFromLongitude: Double;
  const AToLatitude: Double;
  const AToLongitude: Double;
  var RDistance: Double;
  var RDuration: Double;
  var RHashString: PAnsiChar
): Integer; stdcall;

CreateRouteWithPathAcc: function(
  const AHandle: Pointer;
  const ASpeedCB: TSpeedCallbackFunc;
  const AFromLatitude: Double;
  const AFromLongitude: Double;
  const AToLatitude: Double;
  const AToLongitude: Double;
  var RDistance: Double;
  var RDuration: Double;
  var RHashString: PAnsiChar;
  const AAccs: PInteger
): Integer; stdcall;

CreateRouteWithSpeedline: function(
  const AHandle: Pointer;
  const ASpeedCB: TSpeedCallbackFunc;
  const AFromLatitude: Double;
  const AFromLongitude: Double;
  const AToLatitude: Double;
  const AToLongitude: Double;
  var RDistance: Double;
  var RDuration: Double;
  var RSLString: PAnsiChar
): Integer; stdcall;

CreateRouteWithSpeedlineAcc: function(
  const AHandle: Pointer;
  const ASpeedCB: TSpeedCallbackFunc;
  const AFromLatitude: Double;
  const AFromLongitude: Double;
  const AToLatitude: Double;
  const AToLongitude: Double;
  var RDistance: Double;
  var RDuration: Double;
  var RSLString: PAnsiChar;
  const AAccs: PInteger
): Integer; stdcall;

CreateRouteWithPath2: function(
  const AHandle: Pointer;
  const ASpeedCB: TSpeedCallbackFunc;
  const AFromLatitude: Double;
  const AFromLongitude: Double;
  const AToLatitude: Double;
  const AToLongitude: Double;
  const ABufferLen: Integer;
  const RHashString: PAnsiChar;
  var RBufferLen: Integer;
  var RDistance: Double;
  var RDuration: Double
): Integer; stdcall;

CreateRouteWithPath2Acc: function(
  const AHandle: Pointer;
  const ASpeedCB: TSpeedCallbackFunc;
  const AFromLatitude: Double;
  const AFromLongitude: Double;
  const AToLatitude: Double;
  const AToLongitude: Double;
  const ABufferLen: Integer;
  const RHashString: PAnsiChar;
  var RBufferLen: Integer;
  var RDistance: Double;
  var RDuration: Double;
  const AAccs: PInteger
): Integer; stdcall;

CreateRouteWithSpeedline2: function(
  const AHandle: Pointer;
  const ASpeedCB: TSpeedCallbackFunc;
  const AFromLatitude: Double;
  const AFromLongitude: Double;
  const AToLatitude: Double;
  const AToLongitude: Double;
  const ABufferLen: Integer;
  const RSLString: PAnsiChar;
  var RBufferLen: Integer;
  var RDistance: Double;
  var RDuration: Double
): Integer; stdcall;

CreateRouteWithSpeedline2Acc: function(
  const AHandle: Pointer;
  const ASpeedCB: TSpeedCallbackFunc;
  const AFromLatitude: Double;
  const AFromLongitude: Double;
  const AToLatitude: Double;
  const AToLongitude: Double;
  const ABufferLen: Integer;
  const RSLString: PAnsiChar;
  var RBufferLen: Integer;
  var RDistance: Double;
  var RDuration: Double;
  const AAccs: PInteger
): Integer; stdcall;

CreateRouteWithPath3: function(
  const AReq: PAstarRequest
): Integer; stdcall;

CreateRouteWithPath3Acc: function(
  const AReq: PAstarRequest;
  const AAccs: PInteger
): Integer; stdcall;

AStarCalc: function(
  const AReq: PAstarRequest3
): Integer; stdcall;

AStarCalcAcc: function(
  const AReq: PAstarRequest3;
  const AAccs: PInteger
): Integer; stdcall;

AStarCalcLandmarks: function(
  const AReq: PAstarRequest3;
  var RNWHash: Int64;
  var RSEHash: Int64
): Integer; stdcall;

AStarCalcLandmarksAcc: function(
  const AReq: PAstarRequest3;
  var RNWHash: Int64;
  var RSEHash: Int64;
  const AAccs: PInteger
): Integer; stdcall;

//------------------------------------------------------------------------------
//! то же, что AStarCalc и AStarCalcAcc
//!   но с поддержкой поля SignsLimit в структуре TAstarRequest3
//! (AStarCalc и AStarCalcAcc будут игнорировать это поле)
//------------------------------------------------------------------------------
AStarCalcSign: function(
  const AReq: PAstarRequest3
): Integer; stdcall;

//------------------------------------------------------------------------------
//! то же, что AStarCalc и AStarCalcAcc
//!   но с поддержкой поля SignsLimit в структуре TAstarRequest3
//! (AStarCalc и AStarCalcAcc будут игнорировать это поле)
//------------------------------------------------------------------------------
AStarCalcSignAcc: function(
  const AReq: PAstarRequest3;
  const AAccs: PInteger
): Integer; stdcall;

AStarCalc4: function(
  const AReq: PAstarRequest4
): Integer; stdcall;

AStarCalc4Acc: function(
  const AReq: PAstarRequest4;
  const AAccs: PInteger
): Integer; stdcall;

CleanupMem: procedure(
  const AHashString: PAnsiChar
); stdcall;

GetRoadSpeedsDefault: procedure(
  const RRoadSpeedsRecord: PRoadSpeedsRecord
); stdcall;

GetRoadSpeedsKToDef: procedure(
  const ARoadSpeedsRecord: PRoadSpeedsRecord;
  const ADefSpeed: Integer;
  const ADefCitySpeed: Integer
); stdcall;

//------------------------------------------------------------------------------
//! экспортируемые ф-ции загрузка/выгрузки
//------------------------------------------------------------------------------

function LoadAStar64(const AAstarPath: string = ''): Boolean;
procedure UnloadAStar64;

//------------------------------------------------------------------------------
implementation

var
  HAStar64: THandle;

procedure CrearAddrs;
begin
  HAStar64 := 0;
  CreateRoute := nil;
  CreateRouteAcc := nil;
  CreateRouteWithPath := nil;
  CreateRouteWithPathAcc := nil;
  CreateRouteWithSpeedline := nil;
  CreateRouteWithSpeedlineAcc := nil;
  CreateRouteWithPath2 := nil;
  CreateRouteWithPath2Acc := nil;
  CreateRouteWithSpeedline2 := nil;
  CreateRouteWithSpeedline2Acc := nil;
  CreateRouteWithPath3 := nil;
  CreateRouteWithPath3Acc := nil;
  AStarCalc := nil;
  AStarCalcAcc := nil;
  AStarCalcLandmarks := nil;
  AStarCalcLandmarksAcc := nil;
  AStarCalcSign := nil;
  AStarCalcSignAcc := nil;
  AStarCalc4 := nil;
  AStarCalc4Acc := nil;
  CleanupMem := nil;
  GetRoadSpeedsDefault := nil;
  GetRoadSpeedsKToDef := nil;
end;

function LoadAStar64(const AAstarPath: string = ''): Boolean;
var
  AstarImage: string;
begin
  Result := False;
{$IFDEF CPUX64}
  AstarImage := IfThen(AAstarPath = '', '', IncludeTrailingPathDelimiter(AAstarPath)) + 'AStar64x64.dll';
{$ELSE}
  AstarImage := IfThen(AAstarPath = '', '', IncludeTrailingPathDelimiter(AAstarPath)) + 'AStar64.dll';
{$ENDIF}
  HAStar64 := LoadLibrary(PChar(AstarImage));
  if HAStar64 > 0 then
  begin
    CreateRoute := GetProcAddress(HAStar64, 'CreateRoute');
    CreateRouteAcc := GetProcAddress(HAStar64, 'CreateRouteAcc');
    CreateRouteWithPath := GetProcAddress(HAStar64, 'CreateRouteWithPath');
    CreateRouteWithPathAcc := GetProcAddress(HAStar64, 'CreateRouteWithPathAcc');
    CreateRouteWithSpeedline := GetProcAddress(HAStar64, 'CreateRouteWithSpeedline');
    CreateRouteWithSpeedlineAcc := GetProcAddress(HAStar64, 'CreateRouteWithSpeedlineAcc');
    CreateRouteWithPath2 := GetProcAddress(HAStar64, 'CreateRouteWithPath2');
    CreateRouteWithPath2Acc := GetProcAddress(HAStar64, 'CreateRouteWithPath2Acc');
    CreateRouteWithSpeedline2 := GetProcAddress(HAStar64, 'CreateRouteWithSpeedline2');
    CreateRouteWithSpeedline2Acc := GetProcAddress(HAStar64, 'CreateRouteWithSpeedline2Acc');
    CreateRouteWithPath3 := GetProcAddress(HAStar64, 'CreateRouteWithPath3');
    CreateRouteWithPath3Acc := GetProcAddress(HAStar64, 'CreateRouteWithPath3Acc');
    AStarCalc := GetProcAddress(HAStar64, 'AStarCalc');
    AStarCalcAcc := GetProcAddress(HAStar64, 'AStarCalcAcc');
    AStarCalcLandmarks := GetProcAddress(HAStar64, 'AStarCalcLandmarks');
    AStarCalcLandmarksAcc := GetProcAddress(HAStar64, 'AStarCalcLandmarksAcc');
    AStarCalcSign := GetProcAddress(HAStar64, 'AStarCalcSign');
    AStarCalcSignAcc := GetProcAddress(HAStar64, 'AStarCalcSignAcc');
    AStarCalc4 := GetProcAddress(HAStar64, 'AStarCalc4');
    AStarCalc4Acc := GetProcAddress(HAStar64, 'AStarCalc4Acc');
    CleanupMem := GetProcAddress(HAStar64, 'CleanupMem');
    GetRoadSpeedsDefault := GetProcAddress(HAStar64, 'GetRoadSpeedsDefault');
    GetRoadSpeedsKToDef := GetProcAddress(HAStar64, 'GetRoadSpeedsKToDef');
    Result := True;
  end;
end;

procedure UnloadAStar64;
begin
  FreeLibrary(HAStar64);
  CrearAddrs;
end;

//------------------------------------------------------------------------------
initialization
  CrearAddrs;

end.

