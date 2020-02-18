unit UStat;

interface

uses
  SysUtils, StrUtils, IniFiles, Ils.Stat.Sender;

type
  TFacility = Byte;

  TStatActivityType = (atConfig, atRecieve, atSend, atSort, atSave, atFilter, atHeartbeat);
  TStatActivitySubType = (astNone, astNormal, astError, astDebug);
//  TStatActivitySubType = (astEmergency, astAlert, astCritical, astError, astWarning, astNotice, astInfo,
//    astDebug)

const
  CStatActivityType: array[TStatActivityType] of string = (
    'Config', 'Recieve', 'Send', 'Sort', 'Save', 'Filter', 'Heartbeat'
  );

  CStatActivitySubType: array[TStatActivitySubType] of string = (
    'None', 'Normal', 'Error', 'Debug'
  );
// примеры дескрипторов статистики
// WialonRep.Send.Normal.Data
// WialonRep.Send.Normal.Photo
// WialonRecv.Recieve.Normal.Data
// MintransRecv.Save.Error.Data
// NavtelecomRecv.Heartbeat.Normal
// NavtelecomRecv.Save.Normal.Data

procedure StatActivity(AOrigin: string; AType: TStatActivityType; ASubType: TStatActivitySubType; AId: string; ADT: TDateTime; ASubSubType: string = ''; AAdditionalData: string = '');

implementation

var
  ILSStat: TIlsStatSender;

function GetStatFileName: string;
var
  ModuleName: string;
begin
  ModuleName := GetModuleName(HInstance);

  Result := ExtractFilePath(ModuleName) + 'stat\' + ChangeFileExt(ExtractFileName(ModuleName), '.stat');
end;

function GetModuleNameShort: string;
begin
  Result := ChangeFileExt(ExtractFileName(GetModuleName(HInstance)), '');
end;

procedure WriteIniString(const ASection: string; const AId: string; AAdditionalData: string);
var
  ini: TIniFile;
begin
  if (ASection = '') or (AId = '') then
    Exit;

  try
    ForceDirectories(ExtractFilePath(GetStatFileName));
    ini := TIniFile.Create(GetStatFileName);
    try
      ini.WriteString(ASection, AId, FormatDateTime('yyyy-mm-dd hh:nn:ss.zzz', Now) + IfThen(AAdditionalData <> '', ';' + AAdditionalData, ''));
    finally
      ini.Free;
    end;
  except
  end;
end;

procedure StatActivityToIni(AType: TStatActivityType; ASubType: TStatActivitySubType; AId: string; ASubSubType: string = ''; AAdditionalData: string = '');
begin
  WriteIniString(CStatActivityType[AType] + '.' + CStatActivitySubType[ASubType] + IfThen(ASubSubType = '', '', '.' + ASubSubType), AId, AAdditionalData);
end;

procedure StatActivityToLocalSystat(AOrigin: string; AType: TStatActivityType; ASubType: TStatActivitySubType; AId: string; ADT: TDateTime; ASubSubType: string = ''; AAdditionalData: string = '');
var
  s: string;
begin
  if not Assigned(ILSStat) then
    Exit;

  s :=
    AOrigin
    + '.' + CStatActivityType[AType]
    + '.' + CStatActivitySubType[ASubType]
    + IfThen(ASubSubType = '', '', '.' + ASubSubType)
    + ';' + AId
    + '=' + FormatDateTime('yyyy-dd-mm hh:nn:ss.zzz', ADT)
    + IfThen(AAdditionalData <> '', ';' + AAdditionalData, '');
  ILSStat.Send(s);
end;

procedure StatActivity(AOrigin: string; AType: TStatActivityType; ASubType: TStatActivitySubType; AId: string; ADT: TDateTime; ASubSubType: string = ''; AAdditionalData: string = '');
begin
//  StatActivityToIni(AType, ASubType, AId, ASubSubType, AAdditionalData);
//  StatActivityToLocalSystat(AOrigin, AType, ASubType, AId, ADT, ASubSubType);
end;



initialization
  ILSStat := nil;
  try
    ILSStat := TIlsStatSender.Create('127.0.0.1');
  except

  end;

finalization
  try
    ILSStat.Free;
  except

  end;

end.
