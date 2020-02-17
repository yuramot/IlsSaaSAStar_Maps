program AStarTester;

{$APPTYPE CONSOLE}

{$R *.res}

uses
  SysUtils,
  Geo.Pos in '..\_Common\Geo.Pos.pas',
  AStar64.Import in 'AStar64.Import.pas',
  AStar64.Common in 'AStar64.Common.pas';

//------------------------------------------------------------------------------
var
  Rez: Integer;
  FromLatitude: Double;
  FromLongitude: Double;
  ToLatitude: Double;
  ToLongitude: Double;
  Distance: Double;
  Duration: Double;
  HashChar: PAnsiChar;
  HashStr: string;
  Cnt: Integer;
  GFile: text;

//------------------------------------------------------------------------------
//! выдаём скорость на основе "уровня" дороги
//------------------------------------------------------------------------------
function RoadSpeedByType(
  const AType: Integer
): Integer;
begin
  case AType of
    11, 12: Result := 110;
    13, 14: Result := 90;
    15, 16: Result := 70; // 70?
    21, 22: Result := 60;
    31, 32: Result := 60;
    41, 42, 43: Result := 40;
    else Result := 60;
  end;
end;

type
  PCar = ^TCar;
  TCar = packed record
    Speed: Integer;
    p: int64;
  end;
//------------------------------------------------------------------------------
//! процедура обратного вызова для рассчёта скорости
//------------------------------------------------------------------------------
function CallBack(
  const AHandle: Pointer;
  const AMetaData: Pointer
): Integer; stdcall;
begin
  Result := RoadSpeedByType(PMetaDataV1(AMetaData)^.RoadType);
//  Result := pcar(ahandle).Speed;
//  Result := 50;
end;

//------------------------------------------------------------------------------
// MAIN
//------------------------------------------------------------------------------
var
  car: TCar;
begin
  car.speed := 50;

  AssignFile(GFile, ExtractFilePath(ParamStr(0)) + 'proba.txt');
  Reset(GFile);
  while not Eof(GFile) do
  begin
    Readln(GFile, Cnt, FromLatitude, FromLongitude, ToLatitude, ToLongitude);
    Writeln(Cnt, FromLatitude, FromLongitude, ToLatitude, ToLongitude);
    Rez := CreateRouteWithPath(
//    Rez := CreateRouteWithSpeedline(
      @car,
      @CallBack,
      FromLatitude,
      FromLongitude,
      ToLatitude,
      ToLongitude,
      Distance,
      Duration,
      HashChar
    );
    Writeln(Rez);
    if (Rez = 0) then
    begin
      HashStr := string(AnsiString(HashChar));
      Writeln(HashStr);
      Writeln('Length = ', Distance:1:3);
      Writeln('Time = ', FormatDateTime('hh:nn:ss', Duration / 24));
      CleanupMem(HashChar);
    end;
  end;
  CloseFile(GFile);
  Halt;
//
  if (ParamCount < 4) then
  begin
    Writeln('Вызов: AStarTester.exe широта_из долгота_из широта_в долгота_в');
    Halt(1);
  end;
  FromLatitude := StrToFloat(ParamStr(1));
  FromLongitude := StrToFloat(ParamStr(2));
  ToLatitude := StrToFloat(ParamStr(3));
  ToLongitude := StrToFloat(ParamStr(4));
  Rez := CreateRouteWithPath(
    nil,
    @CallBack,
    FromLatitude,
    FromLongitude,
    ToLatitude,
    ToLongitude,
    Distance,
    Duration,
    HashChar
  );
  Writeln(Rez);
  if (Rez = 0) then
  begin
    HashStr := string(AnsiString(HashChar));
    Writeln(HashStr);
    Writeln('Length = ', Distance:1:3);
    Writeln('Time = ', FormatDateTime('hh:nn:ss', Duration / 24));
    CleanupMem(HashChar);
  end;
  Rez := CreateRouteWithSpeedline(
    nil,
    @CallBack,
    FromLatitude,
    FromLongitude,
    ToLatitude,
    ToLongitude,
    Distance,
    Duration,
    HashChar
  );
  Writeln(Rez);
  if (Rez = 0) then
  begin
    HashStr := string(AnsiString(HashChar));
    Writeln(HashStr);
    Writeln('Length = ', Distance:1:3);
    Writeln('Time = ', FormatDateTime('hh:nn:ss', Duration / 24));
    CleanupMem(HashChar);
  end;
{$IFDEF DEBUG}
  Readln;
{$ENDIF}
end.

