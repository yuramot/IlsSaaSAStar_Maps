unit AStar64.Import;
//------------------------------------------------------------------------------
// модуль импорта AStar64.dll
//------------------------------------------------------------------------------

//------------------------------------------------------------------------------
interface

uses
  AStar64.Common;

//------------------------------------------------------------------------------
const

//------------------------------------------------------------------------------
//! название файла dll для линковки
//------------------------------------------------------------------------------
  CAStarDLLName = 'igf\AStar64.dll';

function CreateRoute(
  const AHandle: Pointer;
  const ASpeedCB: TSpeedCallbackFunc;
  const AFromLatitude: Double;
  const AFromLongitude: Double;
  const AToLatitude: Double;
  const AToLongitude: Double;
  var RDistance: Double;
  var RDuration: Double
): Integer; stdcall;

function CreateRouteWithPath(
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

function CreateRouteWithSpeedline(
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

procedure CleanupMem(
  const AHashString: PAnsiChar
); stdcall;

//------------------------------------------------------------------------------
implementation

function CreateRoute; external CAStarDLLName;
function CreateRouteWithPath; external CAStarDLLName;
function CreateRouteWithSpeedline; external CAStarDLLName;
procedure CleanupMem; external CAStarDLLName;

end.

