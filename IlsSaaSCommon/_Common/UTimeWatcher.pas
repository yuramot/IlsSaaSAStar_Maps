unit UTimeWatcher;

interface

//------------------------------------------------------------------------------
uses
  SysUtils, Windows;

procedure StartCount(
  const ID: Integer
);

procedure EndAndWriteCount(
  const ID: Integer
);

//------------------------------------------------------------------------------
implementation

var
  GCounterPerSec: TLargeInteger;
  GFile: text;
  GTimeStorage: array[0..999] of TLargeInteger;

procedure StartCount(
  const ID: Integer
);
begin
  QueryPerformanceCounter(GTimeStorage[ID]);
end;

procedure EndAndWriteCount(
  const ID: Integer
);
var
  TickEnd: TLargeInteger;
  ElapsedTime: Double;
begin
  QueryPerformanceCounter(TickEnd);
  ElapsedTime := (TickEnd - GTimeStorage[ID]) / GCounterPerSec;
  if (ElapsedTime < 1) then
    Writeln(GFile, ID:3, ' => ', (ElapsedTime * 1000):10:6, ' ms')
  else
    Writeln(GFile, ID:3, ' => ', ElapsedTime:11:6, ' m');
end;

//------------------------------------------------------------------------------
initialization
  AssignFile(GFile, ExtractFileDir(ParamStr(0)) + '\TimeLog.txt');
  Rewrite(GFile);
  QueryPerformanceFrequency(GCounterPerSec);

//------------------------------------------------------------------------------
finalization
  CloseFile(GFile);

end.

