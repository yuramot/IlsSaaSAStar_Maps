unit Utils.Collector;
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------

//------------------------------------------------------------------------------
interface

uses
  SysUtils, DateUtils, IniFiles,
  JsonDataObjects,
  Ils.Logger, Ils.Utils,
  UMemStream,
  CINIFilesData;

type
//------------------------------------------------------------------------------
//!
//------------------------------------------------------------------------------
  TCollectorParams = record
    DumpFolder: string;
    KafkaBootstrap: string;
    KafkaTopic: string;
    PortNumber: Integer;
  end;

function LoadCollectorParams(
  out RParams: TCollectorParams
): Boolean;

procedure DumpAnswer(
  const AOutputData: TMyMemStream;
  const AIMEI: string
);

procedure DumpDevice(
  const AInputData: TMyMemStream;
  const AIMEI: string
);

procedure JSON2File(
  const AJSON: TJsonObject;
  const APath: string
);

//------------------------------------------------------------------------------
implementation

function LoadCollectorParams(
  out RParams: TCollectorParams
): Boolean;
var
  INIFile: TIniFile;
//------------------------------------------------------------------------------
begin
  Result := False;
  try
    INIFile := TIniFile.Create(ChangeFileExt(ParamStr(0), CIniDefExtDotted));
  except
    on Ex: Exception do
    begin
      ToLog('Ошибка открытия INI-файла:'#13#10 + Ex.Message);
      Exit;
    end;
  end;
  try
    GLogLevel := INIFile.ReadInteger(CIniMainParamsSection, 'LogLevel', 0);
    ToLog(Format('Уровень лога => "%d"', [GLogLevel]));
    RParams.PortNumber := INIFile.ReadInteger(CIniMainParamsSection, 'Port', 0);
    ToLog(Format('Порт прослушивания => "%d"', [RParams.PortNumber]));
    RParams.DumpFolder := INIFile.ReadString(CIniMainParamsSection, 'DumpFolder', '');
    if (RParams.DumpFolder = '') then
      RParams.DumpFolder := ExtractFilePath(ParamStr(0)) + 'Data\'
    else
      RParams.DumpFolder := IncludeTrailingPathDelimiter(RParams.DumpFolder);
    ForceDirectories(RParams.DumpFolder);
    ToLog(Format('Папка сброса файлов данных => "%s"', [RParams.DumpFolder]));
    RParams.KafkaBootstrap := INIFile.ReadString(CIniKafkaSection, 'bootstrap', '');
    ToLog(Format('Kafka bootstrap => "%s"', [RParams.KafkaBootstrap]));
    RParams.KafkaTopic := INIFile.ReadString(CIniKafkaSection, 'topic', '');
    ToLog(Format('Kafka topic => "%s"', [RParams.KafkaTopic]));
  except
    on Ex: Exception do
    begin
      ToLog('Ошибка чтения параметров из INI-файла:'#13#10 + Ex.Message);
      INIFile.Free();
      Exit;
    end;
  end;
  INIFile.Free();
  Result := True;
end;

procedure DumpAnswer(
  const AOutputData: TMyMemStream;
  const AIMEI: string
);
var
  LogData: TBytes;
  LogLength: Integer;
//------------------------------------------------------------------------------
begin
  AOutputData.UnreadReaded();
  LogLength := AOutputData.Remaining;
  if (LogLength = 0) then
    Exit;
  SetLength(LogData, LogLength);
  AOutputData.ReadData(LogLength, @LogData[0]);
  ToLog(LogDataHex('Ответ прибору с IMEI "' + AIMEI + '"', @LogData[0], LogLength));
  AOutputData.UnreadReaded();
end;

procedure DumpDevice(
  const AInputData: TMyMemStream;
  const AIMEI: string
);
var
  LogData: TBytes;
  LogLength: Integer;
//------------------------------------------------------------------------------
begin
  AInputData.UnreadReaded();
  LogLength := AInputData.Remaining;
  if (LogLength = 0) then
    Exit;
  SetLength(LogData, LogLength);
  AInputData.ReadData(LogLength, @LogData[0]);
  ToLog(LogDataHex('Данные прибора с IMEI "' + AIMEI + '"', @LogData[0], LogLength));
  AInputData.UnreadReaded();
end;

procedure JSON2File(
  const AJSON: TJsonObject;
  const APath: string
);
var
  NowTime: TDateTime;
  FilePath: string;
//------------------------------------------------------------------------------
begin
  NowTime := Now();
  repeat
    FilePath := APath + FormatDateTime('yyyymmdd"_"hhnnss"_"zzz".json"', NowTime);
    if not FileExists(FilePath) then
    begin
      AJSON.SaveToFile(FilePath, False);
      Break;
    end;
    NowTime := IncMilliSecond(NowTime, 10);
  until False;
end;

end.

