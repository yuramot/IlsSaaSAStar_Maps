unit Ils.Utils.Debug;

interface

uses
  SysUtils, Registry, PsAPI, TlHelp32, Windows, TypInfo, StrUtils;

type
  TEnumConverter = class
  public
    class function EnumToInt<T>(const EnumValue: T): Integer;
    class function EnumToString<T>(EnumValue: T): string;
    class function StringToEnum<T>(strValue: String): T;
  end;


function IsAppRunning(const AFileName: string): boolean;
function GetDelphiXE2LocationExeName: string;
procedure SleepIfIdeRunning(milliseconds: Integer);
function MemoryUsed: Int64;
function MemoryUsedDelphi: Int64;
function MemoryUsedWindows: Int64;
function FormatMemoryUsed(const AMessage: string): string;

implementation

procedure SleepIfIdeRunning(milliseconds: Integer);
begin
//  if DebugHook <> 0 then
  if IsAppRunning('bds.exe') then
    Sleep(milliseconds);
end;

function ProcessFileName(dwProcessId: DWORD): string;
var
  hModule: Cardinal;
begin
  Result := '';
  hModule := OpenProcess(PROCESS_QUERY_INFORMATION or PROCESS_VM_READ, False, dwProcessId);
  if hModule <> 0 then
    try
      SetLength(Result, MAX_PATH);
      if GetModuleFileNameEx(hModule, 0, PChar(Result), MAX_PATH) > 0 then
        SetLength(Result, StrLen(PChar(Result)))
      else
        Result := '';
    finally
      CloseHandle(hModule);
    end;
end;

function IsAppRunning(const AFileName: string): boolean;
var
  hSnapshot      : Cardinal;
  EntryParentProc: TProcessEntry32;
begin
  Result := False;
  hSnapshot := CreateToolhelp32Snapshot(TH32CS_SNAPPROCESS, 0);
  if hSnapshot = INVALID_HANDLE_VALUE then
    exit;
  try
    EntryParentProc.dwSize := SizeOf(EntryParentProc);
    if Process32First(hSnapshot, EntryParentProc) then
      repeat
        if CompareText(ExtractFileName(AFileName), EntryParentProc.szExeFile) = 0 then
//          if CompareText(ProcessFileName(EntryParentProc.th32ProcessID),  AFileName) = 0 then
        begin
          Result := True;
          break;
        end;
      until not Process32Next(hSnapshot, EntryParentProc);
  finally
    CloseHandle(hSnapshot);
  end;
end;


function RegReadStr(const RegPath, RegValue: string; var Str: string;
  const RootKey: HKEY): boolean;
var
  Reg: TRegistry;
begin
  try
    Reg := TRegistry.Create;
    try
      Reg.RootKey := RootKey;
      Result      := Reg.OpenKey(RegPath, True);
      if Result then
        Str := Reg.ReadString(RegValue);
    finally
      Reg.Free;
    end;
  except
    Result := False;
  end;
end;

function RegKeyExists(const RegPath: string; const RootKey: HKEY): boolean;
var
  Reg: TRegistry;
begin
  try
    Reg := TRegistry.Create;
    try
      Reg.RootKey := RootKey;
      Result      := Reg.KeyExists(RegPath);
    finally
      Reg.Free;
    end;
  except
    Result := False;
  end;
end;

function GetDelphiXE2LocationExeName: string;
const
 Key = '\Software\Embarcadero\BDS\9.0';
begin
  Result:='';
    if RegKeyExists(Key, HKEY_CURRENT_USER) then
    begin
      RegReadStr(Key, 'App', Result, HKEY_CURRENT_USER);
      exit;
    end;

    if RegKeyExists(Key, HKEY_LOCAL_MACHINE) then
      RegReadStr(Key, 'App', Result, HKEY_LOCAL_MACHINE);
end;


{

example app:

Var
 Bds : String;

begin
  try
     Bds:=GetDelphiXE2LocationExeName;
     if Bds<>'' then
     begin
       if  IsAppRunning(Bds) then
        Writeln('The Delphi XE2 IDE Is running')
       else
        Writeln('The Delphi XE2 IDE Is not running')
     end
     else
     Writeln('The Delphi XE2 IDE Is was not found');
  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;
  Readln;
end.

}

function MemoryUsed: Int64;
begin
//{$ifdef FASTMM4}
  Result := MemoryUsedWindows;
//{$else}
//  Result := MemoryUsedDelphi;
//{$endif}
end;

function MemoryUsedDelphi: Int64;
var
  st: TMemoryManagerState;
  sb: TSmallBlockTypeState;
begin
  GetMemoryManagerState(st);

  Result :=
    st.TotalAllocatedMediumBlockSize * st.AllocatedMediumBlockCount +
    st.TotalAllocatedLargeBlockSize * st.AllocatedLargeBlockCount;

  for sb in st.SmallBlockTypeStates do
    Result := Result + sb.UseableBlockSize * sb.AllocatedBlockCount;
end;


function MemoryUsedWindows: Int64;
var
  mc: TProcessMemoryCounters;
  sz: Integer;
begin
  Result := 0;
  sz := SizeOf(mc);
  FillChar(mc, sz, 0);
  if GetProcessMemoryInfo(GetCurrentProcess, @mc, sz) then
//    Result := mc.PageFileUsage;
    Result := mc.WorkingSetSize;
end;

function FormatMemoryUsed(const AMessage: string): string;
var
  fs: TFormatSettings;
  mu: Double;
begin
  fs := TFormatSettings.Create;
  fs.ThousandSeparator := '''';
  mu := MemoryUsed;// ProcessMemoryUsed;
  Result := Format('%.0n', [mu], fs) + IfThen(AMessage<>'', ' ', '') + AMessage;
end;

class function TEnumConverter.EnumToInt<T>(const EnumValue: T): Integer;
begin
  Result := 0;
  Move(EnumValue, Result, sizeOf(EnumValue));
end;

class function TEnumConverter.EnumToString<T>(EnumValue: T): string;
begin
  Result := GetEnumName(TypeInfo(T), EnumToInt(EnumValue));
end;

class function TEnumConverter.StringToEnum<T>(strValue: String): T;
var
  TypInfo : PTypeInfo;
  Temp: Integer;
  PTemp : Pointer;
begin
  TypInfo := TypeInfo(T);
  Temp := GetEnumValue(TypInfo, strValue);
  PTemp := @Temp;
  Result := T(PTemp^);
end;

end.
