unit E_FileVersionUtils;

interface

uses
  Windows, System.SysUtils;

type
  TVersionInfo = record
    Major, Minor, Release, Build: Integer;
    FileVersion,
    ProductVersion: string;
    ProductName: string;
  end;

function GetApplicationVersion: string;
function GetFileTextVersion(const aExeFileName: TFileName): string;
function GetFileVersion(const aExeFileName: TFileName): string;

implementation

function GetApplicationVersion: string;
begin
  result := GetFileTextVersion(GetModuleName(HInstance));
end;

function GetVersionInfo(aExeFilePath: string; out aVersionInfo: TVersionInfo): Boolean;
var
  VerInfoSize: Cardinal;
  Dummy: Cardinal;
  PVerInfo, Translation: PChar;
  Value: PChar;

  p: integer;
  LangCharSet: string;

begin
  Result := False;
  aVersionInfo.Major   := 0;
  aVersionInfo.Minor   := 0;
  aVersionInfo.Release := 0;
  aVersionInfo.Build   := 0;

  aVersionInfo.ProductVersion:='0.0.0.0';
  aVersionInfo.ProductName:='';
  aVersionInfo.FileVersion := '';

  VerInfoSize := GetFileVersionInfoSize(PChar(aExeFilePath), Dummy);

  if VerInfoSize <= 0 then
    Exit;

  PVerInfo := AllocMem(VerInfoSize);

  try
    // определим язык CalcLangCharSet
    // CalcLangCharSet:='040904E3';
    if not GetFileVersionInfo(PChar(aExeFilePath), 0, VerInfoSize, PVerInfo) then
      Exit;

    if not VerQueryValue(PVerInfo, '\VarFileInfo\Translation', Pointer(Translation), VerInfoSize) then
      Exit;

    if VerInfoSize < 4 then
      Exit;

    p := 0;
    StrLCopy(@p, Translation, 1);
    LangCharSet := IntToHex(p, 4);
    StrLCopy(@p, Translation + 1, 1);
    LangCharSet := LangCharSet + IntToHex(p, 4);

    aVersionInfo.ProductName := '';
    if VerQueryValue(PVerInfo, PChar('StringFileInfo\'+LangCharSet+'\ProductName'), Pointer(Value), VerInfoSize) then
      aVersionInfo.ProductName := Value;

    aVersionInfo.ProductVersion := '';
    if VerQueryValue(PVerInfo, PChar('StringFileInfo\'+LangCharSet+'\ProductVersion'), Pointer(Value), VerInfoSize) then
      aVersionInfo.ProductVersion := Value;

    aVersionInfo.FileVersion := '';
    if VerQueryValue(PVerInfo, PChar('StringFileInfo\'+LangCharSet+'\FileVersion'), Pointer(Value), VerInfoSize) then
      aVersionInfo.FileVersion := Value;

    if not VerQueryValue(PVerInfo, '\', Pointer(Value), VerInfoSize) then
    begin
      aVersionInfo.Major   := HiWord(PVSFixedFileInfo(Value).dwFileVersionMS);
      aVersionInfo.Minor   := LoWord(PVSFixedFileInfo(Value).dwFileVersionMS);
      aVersionInfo.Release := HiWord(PVSFixedFileInfo(Value).dwFileVersionLS);
      aVersionInfo.Build   := LoWord(PVSFixedFileInfo(Value).dwFileVersionLS);
    end;


    Result := True;
  finally
    FreeMem(PVerInfo, VerInfoSize);
  end;
end;

function GetFileTextVersion(const aExeFileName: TFileName): string;
var
  VerInfo: TVersionInfo;
begin
  if GetVersionInfo(aExeFileName, VerInfo) then
    Result := VerInfo.FileVersion;
end;

function GetFileVersion(const aExeFileName: TFileName): string;
var
  VerInfoSize: Cardinal;
  VerValueSize: Cardinal;
  Dummy: Cardinal;
  PVerInfo: Pointer;
  PVerValue: PVSFixedFileInfo;
begin
  Result := '';
  VerInfoSize := GetFileVersionInfoSize(PChar(aExeFileName), Dummy);
  GetMem(PVerInfo, VerInfoSize);
  try
    if GetFileVersionInfo(PChar(aExeFileName), 0, VerInfoSize, PVerInfo) then
      if VerQueryValue(PVerInfo, '\', Pointer(PVerValue), VerValueSize) then
        with PVerValue^ do
          Result := Format('v%d.%d.%d.%d', [
            HiWord(dwFileVersionMS), //Major
            LoWord(dwFileVersionMS), //Minor
            HiWord(dwFileVersionLS), //Release
            LoWord(dwFileVersionLS)]); //Build
  finally
    FreeMem(PVerInfo, VerInfoSize);
  end;
end;


end.
