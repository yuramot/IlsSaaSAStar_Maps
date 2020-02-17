unit AStar64.DynImport;
//------------------------------------------------------------------------------
// модуль импорта функций AStar.dll
//
// содержит описание прототипов функций AStar.dll, а также описание структур
//------------------------------------------------------------------------------

//------------------------------------------------------------------------------
interface

uses
  System.Sysutils, StrUtils;


//------------------------------------------------------------------------------
type

//------------------------------------------------------------------------------
//!
//------------------------------------------------------------------------------
  TAStarParam = record
    Penalty: Double; // m_fPenalty
    DijkstraRate: Double; // m_fDijkstraRatio
    DijkstraOn: Int32; // m_bImDijkstra (BOOL)
    UseMapTypes: Int32; // m_bUseType (BOOL)
    PathType1: Int32; // m_iPathType1
    PathType2: Int32; // m_iPathType2
    PathType3: Int32; // m_iPathType3
    PathPart1: Int32; // m_iPathPart1
    PathPart2: Int32; // m_iPathPart2
    PathPart3: Int32; // m_iPathPart3
    AverageSpeedFlag: Int32; // m_bAverageSpeed (BOOL)
  end;

//------------------------------------------------------------------------------
//!
//------------------------------------------------------------------------------
  PCoords = ^TCoords;

  TCoords = packed record
    //! Широта
    Latitude: Double;
    //! Долгота
    Longitude: Double;
    //! Дальше
    Next: PCoords;
  end;


  PRoadSpeedsRecord = ^TRoadSpeedsRecord;
  TRoadSpeedsRecord = packed record
    Motorway      : UInt8;
    MotorwayLink  : UInt8;
    Trunk         : UInt8;
    TrunkLink     : UInt8;
    Primary       : UInt8;
    PrimaryLink   : UInt8;
    Secondary     : UInt8;
    SecondaryLink : UInt8;
  	Tertiary      : UInt8;
    TertiaryLink  : UInt8;
	  Residential   : UInt8;
    Road          : UInt8;
	  Unclassified  : UInt8;
    Service       : UInt8;
    LivingStreet  : UInt8;
    reverse       : UInt8;
  end;


  PAstarRequest = ^TAstarRequest;

  TAstarRequest = packed record
    Version: UInt64;
    FromLatitude: Double;
    FromLongitude: Double;
    ToLatitude: Double;
    ToLongitude: Double;
    ZonesLimit: UInt64;
    RoadSpeedsRecord: TRoadSpeedsRecord;
    FormatVariant: Integer;
    LenTreshold: Double;
    Timeout: Integer;
    Distance: Double;
    Duration: Double;
    BufferLen: Integer;
    HashString: PAnsiChar;
  end;


//------------------------------------------------------------------------------
//! прототип процедуры обратного вызова
//------------------------------------------------------------------------------
  TSpeedCallbackFunc = function(
    //! ссылка на внешние данные
    const AHandle: Pointer;
    //! ссылка на передаваемые данные
    const AMetaData: Pointer
  ): Integer; stdcall;

//------------------------------------------------------------------------------



var

CreateRoute: function(
  const AHandle: Pointer;
  const ASpeedCB: Pointer;
  const AFromLatitude: Double;
  const AFromLongitude: Double;
  const AToLatitude: Double;
  const AToLongitude: Double;
  var RDistance: Double;
  var RDuration: Double
): Integer; stdcall;

//------------------------------------------------------------------------------
//! рассчитать + вернуть путь
//------------------------------------------------------------------------------
CreateRouteWithPath: function (
  const AHandle: Pointer;
  const ASpeedCB: Pointer;
  const AFromLatitude: Double;
  const AFromLongitude: Double;
  const AToLatitude: Double;
  const AToLongitude: Double;
  var RDistance: Double;
  var RDuration: Double;
  var RHashString: PAnsiChar
): Integer; stdcall;

//------------------------------------------------------------------------------
//! рассчитать + вернуть путь и скорость
//------------------------------------------------------------------------------
CreateRouteWithSpeedline: function (
  const AHandle: Pointer;
  const ASpeedCB: Pointer;
  const AFromLatitude: Double;
  const AFromLongitude: Double;
  const AToLatitude: Double;
  const AToLongitude: Double;
  var RDistance: Double;
  var RDuration: Double;
  var RSLString: PAnsiChar
): Integer; stdcall;

//------------------------------------------------------------------------------
//! освободить память результирующих строк
//------------------------------------------------------------------------------
CleanupMem: procedure (
  const AHashString: PAnsiChar
); stdcall;

//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
//! рассчитать + вернуть путь
//------------------------------------------------------------------------------
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

//------------------------------------------------------------------------------
//! рассчитать + вернуть путь и скорость
//------------------------------------------------------------------------------
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


//------------------------------------------------------------------------------
//! рассчитать + вернуть путь + <скорость и т.п.>
//------------------------------------------------------------------------------
CreateRouteWithPath3: function(
  const AAStarRequest: PAstarRequest
): Integer; stdcall;


GetRoadSpeedsDefault: procedure(const ARoadSpeedsRecord: PRoadSpeedsRecord); stdcall;
GetRoadSpeedsKToDef: procedure(const ARoadSpeedsRecord: PRoadSpeedsRecord; const ADefSpeed: Integer; const ADefCitySpeed: Integer); stdcall;

function LoadAStar64(const AAstarPath: string = ''): Boolean;
procedure UnloadAStar64;

implementation

uses
  Windows;

var
  HAStar64: THandle;

function LoadAStar64(const AAstarPath: string = ''): Boolean;
var
  AstarImage: string;
begin
//  HAStar64 := LoadLibrary('igf\AStar64.dll');
  Result := False;

  AstarImage := IfThen(AAstarPath = '', '', IncludeTrailingPathDelimiter(AAstarPath)) + 'AStar64.dll';
  HAStar64 := LoadLibrary(PChar(AstarImage));
  if HAStar64 > 0 then
  begin
    CreateRoute := GetProcAddress(HAStar64, 'CreateRoute');
    CreateRouteWithPath := GetProcAddress(HAStar64, 'CreateRouteWithPath');
    CreateRouteWithSpeedline := GetProcAddress(HAStar64, 'CreateRouteWithSpeedline');
    CleanupMem := GetProcAddress(HAStar64, 'CleanupMem');
    CreateRouteWithPath2 := GetProcAddress(HAStar64, 'CreateRouteWithPath2');
    CreateRouteWithSpeedline2 := GetProcAddress(HAStar64, 'CreateRouteWithSpeedline2');
    CreateRouteWithPath3 := GetProcAddress(HAStar64, 'CreateRouteWithPath3');
    GetRoadSpeedsDefault := GetProcAddress(HAStar64, 'GetRoadSpeedsDefault');
    GetRoadSpeedsKToDef := GetProcAddress(HAStar64, 'GetRoadSpeedsKToDef');
    Result := True;
  end;
end;

procedure UnloadAStar64;
begin
  FreeLibrary(HAStar64);
  CreateRoute := nil;
  CreateRouteWithPath := nil;
  CreateRouteWithSpeedline := nil;
  CleanupMem := nil;
  CreateRouteWithPath2 := nil;
  CreateRouteWithSpeedline2 := nil;
  CreateRouteWithPath3 := nil;
  GetRoadSpeedsDefault := nil;
  GetRoadSpeedsKToDef := nil;
end;

//function CalcPlanRoute; external CAStarDLLName;
//procedure CleanUpMemory; external CAStarDLLName;
//procedure SetParamsByCriterion; external CAStarDLLName;
//procedure SetParams; external CAStarDLLName;
//function SetFilePath; external CAStarDLLName;
initialization

  HAStar64 := 0;

  CreateRoute := nil;
  CreateRouteWithPath := nil;
  CreateRouteWithSpeedline := nil;
  CleanupMem := nil;
  CreateRouteWithPath2 := nil;
  CreateRouteWithSpeedline2 := nil;
  CreateRouteWithPath3 := nil;
  GetRoadSpeedsDefault := nil;
  GetRoadSpeedsKToDef := nil;

end.

