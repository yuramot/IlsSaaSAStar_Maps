unit AStar64.Intf;
//------------------------------------------------------------------------------
// модуль экспортируемых функций
//------------------------------------------------------------------------------

//------------------------------------------------------------------------------
interface

uses
  SysUtils, Windows, StrUtils, Geo.Pos,
  AStar64.Common, AStar64.Core, AStar64.Typ,
  slogsend;

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

function CreateRouteAcc(
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

function CreateRouteWithPathAcc(
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

function CreateRouteWithSpeedlineAcc(
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

function CreateRouteWithPath2(
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

function CreateRouteWithPath2Acc(
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

function CreateRouteWithSpeedline2(
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

function CreateRouteWithSpeedline2Acc(
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

function CreateRouteWithPath3(
  const AReq: PAstarRequest
): Integer; stdcall;

function CreateRouteWithPath3Acc(
  const AReq: PAstarRequest;
  const AAccs: PInteger
): Integer; stdcall;

function AStarCalc(
  const AReq: PAstarRequest3
): Integer; stdcall;

function AStarCalcAcc(
  const AReq: PAstarRequest3;
  const AAccs: PInteger
): Integer; stdcall;

function AStarCalcLandmarks(
  const AReq: PAstarRequest3;
  var RNWHash: Int64;
  var RSEHash: Int64
): Integer; stdcall;

function AStarCalcLandmarksAcc(
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
function AStarCalcSign(
  const AReq: PAstarRequest3
): Integer; stdcall;

//------------------------------------------------------------------------------
//! то же, что AStarCalc и AStarCalcAcc
//!   но с поддержкой поля SignsLimit в структуре TAstarRequest3
//! (AStarCalc и AStarCalcAcc будут игнорировать это поле)
//------------------------------------------------------------------------------
function AStarCalcSignAcc(
  const AReq: PAstarRequest3;
  const AAccs: PInteger
): Integer; stdcall;

function AStarCalc4(
  const AReq: PAstarRequest4
): Integer; stdcall;

function AStarCalc4Acc(
  const AReq: PAstarRequest4;
  const AAccs: PInteger
): Integer; stdcall;

procedure CleanupMem(
  const AHashString: PAnsiChar
); stdcall;

procedure GetRoadSpeedsDefault(
  const RRoadSpeedsRecord: PRoadSpeedsRecord
); stdcall;

procedure GetRoadSpeedsKToDef(
  const ARoadSpeedsRecord: PRoadSpeedsRecord;
  const ADefSpeed: Integer;
  const ADefCitySpeed: Integer
); stdcall;

//------------------------------------------------------------------------------
implementation

uses
  Geo.Hash;

const
  CAcc: array[0..1] of Integer = (1, 0);

//------------------------------------------------------------------------------
//! ф-ции логирования
//------------------------------------------------------------------------------

procedure ErrorLog(const AErrorMessage: string; const AErrorClass: string);
begin
  VLog('ERROR=' + AErrorMessage + ';class=' + AErrorClass, True);
end;

procedure CallLog(const APreamble: string; const AAStarRequest: PAstarRequest); overload;
begin
  VLog(APreamble + AAStarRequest^.ToString, True);
end;

procedure CallLog(const APreamble: string; const AAStarRequest: PAstarRequest3); overload;
begin
  VLog(APreamble + AAStarRequest^.ToString, True);
end;

procedure CallLog(const APreamble: string; const AAStarRequest: PAstarRequest4); overload;
begin
  VLog(APreamble + AAStarRequest^.ToString, True);
end;

procedure CallLog(const APreamble: string; const AFromLatitude, AFromLongitude, AToLatitude, AToLongitude: Double); overload;
var
  fs: TFormatSettings;
begin
  fs := TFormatSettings.Create('ru-ru');
  fs.DecimalSeparator := '.';
  VLog(
    APreamble + 'REQUEST=(0,['
      + FloatToStrF(AFromLatitude, ffFixed, 12, 8, fs) + ',' + FloatToStrF(AFromLongitude, ffFixed, 12, 8, fs)
      + ']-['
      + FloatToStrF(AToLatitude, ffFixed, 12, 8, fs) + ',' + FloatToStrF(AToLongitude, ffFixed, 12, 8, fs)
      + '],0)',
    True
  );
end;

procedure LMLog(const APreamble: string; const ANW, ASE: Int64);
begin
  VLog(APreamble + 'landmarks=(' + IntToHex(ANW, 16) + ',' + IntToHex(ASE, 16) + ')');
end;

procedure ResultLog(const APreamble: string; const AResult: Integer);
begin
  VLog(APreamble + 'RESULT=' + IntToStr(AResult), True);
end;

function Accs(AAccs: PInteger): string;
begin
  Result := '';
  while (AAccs^ <> 0) do
  begin
    Result := Result + IntToStr(AAccs^) + '\';
    Inc(AAccs);
  end;
  Result := 'accs=' + Result + ';';
end;

//------------------------------------------------------------------------------
//! экспортируемые ф-ции расчёта
//------------------------------------------------------------------------------

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
begin
  Result := CreateRouteAcc(AHandle, ASpeedCB, AFromLatitude, AFromLongitude, AToLatitude, AToLongitude, RDistance, RDuration, @CAcc[0]);
end;

function CreateRouteAcc(
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
var
  Star: TAstar;
begin
  Result := ERROR_UNKNOWN_ERROR;
  Star := nil;
  try
    try
      CallLog('"CreateRoute"' + Accs(AAccs), AFromLatitude, AFromLongitude, AToLatitude, AToLongitude);
      Star := TAstar.Create();
      Star.SetCalcParams(AAccs, AFromLatitude, AFromLongitude, AToLatitude, AToLongitude, CRoadSpeedRecordDef);
      Result := Star.Calculate();
      if (Result <> ERROR_ASTAR_SUCCESS) then
        Exit;
      Star.CountDD(RDistance, RDuration, ERROR_ASTAR_SUCCESS, 0, 0);
    except
      on E: EOutOfMemory do
      begin
        ErrorLog(E.Message, E.ClassName);
        Exit(ERROR_ASTAR_NOT_ENOUGH_MEMORY);
      end;
      on E: Exception do
      begin
        ErrorLog(E.Message, E.ClassName);
        Exit(ERROR_UNKNOWN_ERROR);
      end;
    end;
  finally
    Star.Free();
    ResultLog('"CreateRoute"', Result);
  end;
end;

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
begin
  Result := CreateRouteWithPathAcc(AHandle, ASpeedCB, AFromLatitude, AFromLongitude, AToLatitude, AToLongitude, RDistance, RDuration, RHashString, @CAcc[0]);
end;

function CreateRouteWithPathAcc(
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
var
  Star: TAstar;
begin
  Result := ERROR_UNKNOWN_ERROR;
  Star := nil;
  try
    try
      CallLog('"CreateRouteWithPath"' + Accs(AAccs), AFromLatitude, AFromLongitude, AToLatitude, AToLongitude);
      Star := TAstar.Create();
      Star.SetCalcParams(AAccs, AFromLatitude, AFromLongitude, AToLatitude, AToLongitude, CRoadSpeedRecordDef);
      Result := Star.Calculate();
      if (Result <> ERROR_ASTAR_SUCCESS) then
        Exit;
      Star.CountDD(RDistance, RDuration, ERROR_ASTAR_SUCCESS, 0, 0);
      Star.MakeHash(RHashString, ERROR_ASTAR_SUCCESS);
    except
      on E: EOutOfMemory do
      begin
        ErrorLog(E.Message, E.ClassName);
        Exit(ERROR_ASTAR_NOT_ENOUGH_MEMORY);
      end;
      on E: Exception do
      begin
        ErrorLog(E.Message, E.ClassName);
        Exit(ERROR_UNKNOWN_ERROR);
      end;
    end;
  finally
    Star.Free();
    ResultLog('"CreateRouteWithPath"', Result);
  end;
end;

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
begin
  Result := CreateRouteWithSpeedlineAcc(AHandle, ASpeedCB, AFromLatitude, AFromLongitude, AToLatitude, AToLongitude, RDistance, RDuration, RSLString, @CAcc[0]);
end;

function CreateRouteWithSpeedlineAcc(
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
var
  Star: TAstar;
begin
  Result := ERROR_UNKNOWN_ERROR;
  Star := nil;
  try
    try
      CallLog('"CreateRouteWithSpeedline"' + Accs(AAccs), AFromLatitude, AFromLongitude, AToLatitude, AToLongitude);
      Star := TAstar.Create();
      Star.SetCalcParams(AAccs, AFromLatitude, AFromLongitude, AToLatitude, AToLongitude, CRoadSpeedRecordDef);
      Result := Star.Calculate();
      if (Result <> ERROR_ASTAR_SUCCESS) then
        Exit;
      Star.CountDD(RDistance, RDuration, ERROR_ASTAR_SUCCESS, 0, 0);
      Star.MakeSpeedline(RSLString);
    except
      on E: EOutOfMemory do
      begin
        ErrorLog(E.Message, E.ClassName);
        Exit(ERROR_ASTAR_NOT_ENOUGH_MEMORY);
      end;
      on E: Exception do
      begin
        ErrorLog(E.Message, E.ClassName);
        Exit(ERROR_UNKNOWN_ERROR);
      end;
    end;
  finally
    Star.Free();
    ResultLog('"CreateRouteWithSpeedline"', Result);
  end;
end;

function CreateRouteWithPath2(
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
begin
  Result := CreateRouteWithPath2Acc(AHandle, ASpeedCB, AFromLatitude, AFromLongitude, AToLatitude, AToLongitude, ABufferLen, RHashString, RBufferLen, RDistance, RDuration, @CAcc[0]);
end;

function CreateRouteWithPath2Acc(
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
var
  Star: TAstar;
  HashString: PAnsiChar;
begin
  Result := ERROR_UNKNOWN_ERROR;
  Star := nil;
  HashString := nil;
  try
    try
      CallLog('"CreateRouteWithPath2"' + Accs(AAccs), AFromLatitude, AFromLongitude, AToLatitude, AToLongitude);
      Star := TAstar.Create();
      Star.SetCalcParams(AAccs, AFromLatitude, AFromLongitude, AToLatitude, AToLongitude, CRoadSpeedRecordDef);
      Result := Star.Calculate();
      if (Result <> ERROR_ASTAR_SUCCESS) then
        Exit;
      Star.CountDD(RDistance, RDuration, ERROR_ASTAR_SUCCESS, 0, 0);
      Star.MakeHash(HashString, ERROR_ASTAR_SUCCESS);
      RBufferLen := StrLen(HashString) + 1;
      if (RBufferLen > ABufferLen) then
        Exit(ERROR_ASTAR_REQUEST_TOO_SHORT_BUFFER);
      MoveMemory(RHashString, HashString, RBufferLen);
    except
      on E: EOutOfMemory do
      begin
        ErrorLog(E.Message, E.ClassName);
        Exit(ERROR_ASTAR_NOT_ENOUGH_MEMORY);
      end;
      on E: Exception do
      begin
        ErrorLog(E.Message, E.ClassName);
        Exit(ERROR_UNKNOWN_ERROR);
      end;
    end;
  finally
    CleanupMem(HashString);
    Star.Free();
    ResultLog('"CreateRouteWithPath2"', Result);
  end;
end;

function CreateRouteWithSpeedline2(
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
begin
  Result := CreateRouteWithSpeedline2Acc(AHandle, ASpeedCB, AFromLatitude, AFromLongitude, AToLatitude, AToLongitude, ABufferLen, RSLString, RBufferLen, RDistance, RDuration, @CAcc[0]);
end;

function CreateRouteWithSpeedline2Acc(
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
var
  Star: TAstar;
  SLString: PAnsiChar;
begin
  Result := ERROR_UNKNOWN_ERROR;
  Star := nil;
  SLString := nil;
  try
    try
      CallLog('"CreateRouteWithSpeedline2"' + Accs(AAccs), AFromLatitude, AFromLongitude, AToLatitude, AToLongitude);
      Star := TAstar.Create();
      Star.SetCalcParams(AAccs, AFromLatitude, AFromLongitude, AToLatitude, AToLongitude, CRoadSpeedRecordDef);
      Result := Star.Calculate();
      if (Result <> ERROR_ASTAR_SUCCESS) then
        Exit;
      Star.CountDD(RDistance, RDuration, ERROR_ASTAR_SUCCESS, 0, 0);
      Star.MakeSpeedline(SLString);
      RBufferLen := StrLen(SLString) + 1;
      if (RBufferLen > ABufferLen) then
        Exit(ERROR_ASTAR_REQUEST_TOO_SHORT_BUFFER);
      MoveMemory(RSLString, SLString, RBufferLen);
    except
      on E: EOutOfMemory do
      begin
        ErrorLog(E.Message, E.ClassName);
        Exit(ERROR_ASTAR_NOT_ENOUGH_MEMORY);
      end;
      on E: Exception do
      begin
        ErrorLog(E.Message, E.ClassName);
        Exit(ERROR_UNKNOWN_ERROR);
      end;
    end;
  finally
    CleanupMem(SLString);
    Star.Free();
    ResultLog('"CreateRouteWithSpeedline2"', Result);
  end;
end;

function CreateRouteWithPath3(
  const AReq: PAstarRequest
): Integer; stdcall;
begin
  Result := CreateRouteWithPath3Acc(AReq, @CAcc[0]);
end;

function CreateRouteWithPath3Acc(
  const AReq: PAstarRequest;
  const AAccs: PInteger
): Integer; stdcall;
var
  Star: TAstar;
  HashString: PAnsiChar;
  BufferLen: Integer;
  Duration: Double;
begin
  Result := ERROR_UNKNOWN_ERROR;
  Star := nil;
  HashString := nil;
  try
    try
      CallLog('"CreateRouteWithPath3"' + Accs(AAccs), AReq);
      if not Assigned(AReq) then
        Exit(ERROR_ASTAR_REQUEST_NOT_ASSIGNED);
      if (AReq.Version <> 1) then
        Exit(ERROR_ASTAR_REQUEST_WRONG_VERSION);
      Star := TAstar.Create();
      Star.SetCalcParams(AAccs, AReq);
      if not Star.CheckGraphVersion then
        Exit(ERROR_ASTAR_HAS_WRONG_FILE_VERSION);
      Result := Star.Calculate();
      if (Result <> ERROR_ASTAR_SUCCESS) then
        Exit;
      Star.CountDD(AReq.Distance, Duration, Result, 0, 0);
      AReq.Duration := Duration / 24;
      if (AReq.FormatVariant < Ord(Low(TGeoHashStringFormat))) or (AReq.FormatVariant > Ord(High(TGeoHashStringFormat)))
      or (AReq.BufferSize <= 0) then
        Exit;
      Star.MakeHash(HashString, Result, AReq.FormatVariant);
      BufferLen := StrLen(HashString) + 1;
      if (BufferLen > AReq.BufferSize) then
        Exit(ERROR_ASTAR_REQUEST_TOO_SHORT_BUFFER);
      MoveMemory(AReq.HashString, HashString, BufferLen);
    except
      on E: EOutOfMemory do
      begin
        ErrorLog(E.Message, E.ClassName);
        Exit(ERROR_ASTAR_NOT_ENOUGH_MEMORY);
      end;
      on E: Exception do
      begin
        ErrorLog(E.Message, E.ClassName);
        Exit(ERROR_UNKNOWN_ERROR);
      end;
    end;
  finally
    CleanupMem(HashString);
    Star.Free();
    ResultLog('"CreateRouteWithPath3"', Result);
  end;
end;

function AStarCalc(
  const AReq: PAstarRequest3
): Integer; stdcall;
begin
  Result := AStarCalcAcc(AReq, @CAcc[0]);
end;

function AStarCalcAcc(
  const AReq: PAstarRequest3;
  const AAccs: PInteger
): Integer; stdcall;
var
  Star: TAstar;
  HashString: PAnsiChar;
  BufferLen: Integer;
  Duration: Double;
begin
  Result := ERROR_UNKNOWN_ERROR;
  Star := nil;
  HashString := nil;
  try
    try
      CallLog('"AStarCalc"' + Accs(AAccs), AReq);
      if not Assigned(AReq) then
        Exit(ERROR_ASTAR_REQUEST_NOT_ASSIGNED);
      Star := TAstar.Create();
      Star.SetCalcParams(AAccs, AReq);
      Result := Star.Calculate();
      AReq.RoadLengthCount := Star.CountDD(AReq.Distance, Duration, Result, AReq.RoadLengthBufferSize, 0, @(AReq.Stat), AReq.RoadLengthByZoneByType, nil);
      if (AReq.FormatVariant < Ord(Low(TGeoHashStringFormat))) or (AReq.FormatVariant > Ord(High(TGeoHashStringFormat)))
      or (AReq.BufferSize <= 0) then
        Exit;
      Star.MakeHash(HashString, Result, AReq.FormatVariant);
      BufferLen := StrLen(HashString) + 1;
      if (BufferLen > AReq.BufferSize) then
        Exit(ERROR_ASTAR_REQUEST_TOO_SHORT_BUFFER);
      MoveMemory(AReq.HashString, HashString, BufferLen);
    except
      on E: EOutOfMemory do
      begin
        ErrorLog(E.Message, E.ClassName);
        Exit(ERROR_ASTAR_NOT_ENOUGH_MEMORY);
      end;
      on E: Exception do
      begin
        ErrorLog(E.Message, E.ClassName);
        Exit(ERROR_UNKNOWN_ERROR);
      end;
    end;
  finally
    CleanupMem(HashString);
    Star.Free();
    ResultLog('"AStarCalc"', Result);
  end;
end;

function AStarCalcLandmarks(
  const AReq: PAstarRequest3;
  var RNWHash: Int64;
  var RSEHash: Int64
): Integer; stdcall;
begin
  Result := AStarCalcLandmarksAcc(AReq, RNWHash, RSEHash, @CAcc[0]);
end;

function AStarCalcLandmarksAcc(
  const AReq: PAstarRequest3;
  var RNWHash: Int64;
  var RSEHash: Int64;
  const AAccs: PInteger
): Integer; stdcall;
var
  Star: TAstar;
  HashString: PAnsiChar;
  BufferLen: Integer;
  Duration: Double;
begin
  Result := ERROR_UNKNOWN_ERROR;
  Star := nil;
  HashString := nil;
  try
    try
      CallLog('"AStarCalcLandmarks"' + Accs(AAccs), AReq);
      if not Assigned(AReq) then
        Exit(ERROR_ASTAR_REQUEST_NOT_ASSIGNED);
      RNWHash := 0;
      RSEHash := 0;
      Star := TAstar.Create();
      Star.SetCalcParams(AAccs, AReq);
      Result := Star.Calculate();
      AReq.RoadLengthCount := Star.CountDD(AReq.Distance, Duration, Result, AReq.RoadLengthBufferSize, 0, @(AReq.Stat), AReq.RoadLengthByZoneByType, nil, AReq.FormatVariant = Ord(gfLandMark));
      if (AReq.FormatVariant < Ord(Low(TGeoHashStringFormat))) or (AReq.FormatVariant > Ord(High(TGeoHashStringFormat)))
      or (AReq.BufferSize <= 0) then
        Exit;
      Star.MakeHash(HashString, Result, AReq.FormatVariant);
      BufferLen := StrLen(HashString) + 1;
      if (BufferLen > AReq.BufferSize) then
        Exit(ERROR_ASTAR_REQUEST_TOO_SHORT_BUFFER);
      MoveMemory(AReq.HashString, HashString, BufferLen);
      Star.ReadLMRectangle(RNWHash, RSEHash);
      LMLog('"AStarCalcLandmarks"', RNWHash, RSEHash);
    except
      on E: EOutOfMemory do
      begin
        ErrorLog(E.Message, E.ClassName);
        Exit(ERROR_ASTAR_NOT_ENOUGH_MEMORY);
      end;
      on E: Exception do
      begin
        ErrorLog(E.Message, E.ClassName);
        Exit(ERROR_UNKNOWN_ERROR);
      end;
    end;
  finally
    CleanupMem(HashString);
    Star.Free();
    ResultLog('"AStarCalcLandmarks"', Result);
  end;
end;

function AStarCalcSign(
  const AReq: PAstarRequest3
): Integer; stdcall;
begin
  Result := AStarCalcSignAcc(AReq, @CAcc[0]);
end;

function AStarCalcSignAcc(
  const AReq: PAstarRequest3;
  const AAccs: PInteger
): Integer; stdcall;
var
  Star: TAstar;
  HashString: PAnsiChar;
  BufferLen: Integer;
  Duration: Double;
begin
  Result := ERROR_UNKNOWN_ERROR;
  Star := nil;
  HashString := nil;
  try
    try
      CallLog('"AStarCalcSign"' + Accs(AAccs), AReq);
      if not Assigned(AReq) then
        Exit(ERROR_ASTAR_REQUEST_NOT_ASSIGNED);
      Star := TAstar.Create();
      Star.SetCalcParamsSign(AAccs, AReq);
      Result := Star.Calculate();
      AReq.RoadLengthCount := Star.CountDD(AReq.Distance, Duration, Result, AReq.RoadLengthBufferSize, 0, @(AReq.Stat), AReq.RoadLengthByZoneByType, nil);
      if (AReq.FormatVariant < Ord(Low(TGeoHashStringFormat))) or (AReq.FormatVariant > Ord(High(TGeoHashStringFormat)))
      or (AReq.BufferSize <= 0) then
        Exit;
      Star.MakeHash(HashString, Result, AReq.FormatVariant);
      BufferLen := StrLen(HashString) + 1;
      if (BufferLen > AReq.BufferSize) then
        Exit(ERROR_ASTAR_REQUEST_TOO_SHORT_BUFFER);
      MoveMemory(AReq.HashString, HashString, BufferLen);
    except
      on E: EOutOfMemory do
      begin
        ErrorLog(E.Message, E.ClassName);
        Exit(ERROR_ASTAR_NOT_ENOUGH_MEMORY);
      end;
      on E: Exception do
      begin
        ErrorLog(E.Message, E.ClassName);
        Exit(ERROR_UNKNOWN_ERROR);
      end;
    end;
  finally
    CleanupMem(HashString);
    Star.Free();
    ResultLog('"AStarCalcSign"', Result);
  end;
end;

function AStarCalc4(
  const AReq: PAstarRequest4
): Integer; stdcall;
begin
  Result := AStarCalc4Acc(AReq, @CAcc[0]);
end;

function AStarCalc4Acc(
  const AReq: PAstarRequest4;
  const AAccs: PInteger
): Integer; stdcall;
var
  Star: TAstar;
  HashString: PAnsiChar;
  BufferLen: Integer;
  Duration: Double;
begin
  Result := ERROR_UNKNOWN_ERROR;
  Star := nil;
  HashString := nil;
  try
    try
      CallLog('"AStarCalc4"' + Accs(AAccs), AReq);
      if not Assigned(AReq) then
        Exit(ERROR_ASTAR_REQUEST_NOT_ASSIGNED);
      Star := TAstar.Create();
      Star.SetCalcParams(AAccs, AReq);
      Result := Star.Calculate();
      AReq.RoadLengthCount := Star.CountDD(AReq.Distance, Duration, Result, 0, AReq.RoadLengthAggregateBufferSize, @(AReq.Stat), nil, AReq.RoadLengthAggregate, AReq.FormatVariant = Ord(gfLandMark));
      if (AReq.FormatVariant < Ord(Low(TGeoHashStringFormat))) or (AReq.FormatVariant > Ord(High(TGeoHashStringFormat)))
      or (AReq.BufferSize <= 0) then
        Exit;
      Star.MakeHash(HashString, Result, AReq.FormatVariant);
      BufferLen := StrLen(HashString) + 1;
      if (BufferLen > AReq.BufferSize) then
        Exit(ERROR_ASTAR_REQUEST_TOO_SHORT_BUFFER);
      MoveMemory(AReq.HashString, HashString, BufferLen);
    except
      on E: EOutOfMemory do
      begin
        ErrorLog(E.Message, E.ClassName);
        Exit(ERROR_ASTAR_NOT_ENOUGH_MEMORY);
      end;
      on E: Exception do
      begin
        ErrorLog(E.Message, E.ClassName);
        Exit(ERROR_UNKNOWN_ERROR);
      end;
    end;
  finally
    CleanupMem(HashString);
    Star.Free();
    ResultLog('"AStarCalc4"', Result);
  end;
end;

procedure CleanupMem(
  const AHashString: PAnsiChar
); stdcall;
begin
  if Assigned(AHashString) then
    FreeMem(AHashString);
end;

procedure GetRoadSpeedsDefault(
  const RRoadSpeedsRecord: PRoadSpeedsRecord
); stdcall;
begin
  if Assigned(RRoadSpeedsRecord) then
    RRoadSpeedsRecord^ := CRoadSpeedRecordDef;
end;

procedure GetRoadSpeedsKToDef(
  const ARoadSpeedsRecord: PRoadSpeedsRecord;
  const ADefSpeed: Integer;
  const ADefCitySpeed: Integer
); stdcall;
var
  kt: Double;
  kc: Double;
begin
  if not Assigned(ARoadSpeedsRecord) then
    Exit;
  GetRoadSpeedsDefault(ARoadSpeedsRecord);
  kt:= ADefSpeed/ARoadSpeedsRecord.Trunk;
  if (ADefCitySpeed <= 0) then
    kc := kt
  else
    kc := ADefCitySpeed / ARoadSpeedsRecord.Primary;
  with ARoadSpeedsRecord^ do
  begin
    Motorway      := Round(Motorway       * kt);
    MotorwayLink  := Round(MotorwayLink   * kt);
    Trunk         := Round(Trunk          * kt);
    TrunkLink     := Round(TrunkLink      * kt);
    Primary       := Round(Primary        * kc);
    PrimaryLink   := Round(PrimaryLink    * kc);
    Secondary     := Round(Secondary      * kc);
    SecondaryLink := Round(SecondaryLink  * kc);
    Tertiary      := Round(Tertiary       * kc);
    TertiaryLink  := Round(TertiaryLink   * kc);
    Residential   := Round(Residential    * kc);
    Road          := Round(Road           * kc);
    Unclassified  := Round(Unclassified   * kc);
    Service       := Round(Service        * kc);
    LivingStreet  := Round(LivingStreet   * kc);
    reverse       := Round(reverse        * kc);
  end;
end;

end.

