unit Ils.Utils.Svc;

interface

uses
  SysUtils, StrUtils, DateUtils, SvcMgr, WinSvc, SyncObjs, IniFiles,
  JsonDataObjects,
  CINIFilesData, Ils.Logger, Ils.Utils.SaaS;

type
  TKafkaProcessFunc = function(
    const AJSON: TJsonObject;
    const AMes: string;
    const AOffset: Int64;
    out OMessageDT: TDateTime
  ): Boolean of object;
  TCheckRepairDBFunc = function(): Boolean of object;

function GetParam(AParam: string): string;
function GetConfigSuffix(AParam: string = 'config'): string;
procedure SvcBeforeInstallUninstall(const AService: TService; const AServiceName: string; const AServiceDisplayName: string; const AParamName: string = 'config');
procedure SvcAfterInstall(const AServiceName: string; const AParamName: string = 'config');
function GetInstanceName: string;

function KafkaMPWrapper(
  const AMessage: AnsiString;
  const AOffset: Int64;
  const ATopic: AnsiString;
  const AProcessFunc: TKafkaProcessFunc;
  const ACheckDBFunc: TCheckRepairDBFunc;
  const ASyncDB: TCriticalSection;
  const ASyncINI: TCriticalSection;
  const ARAR: TRedisActivityReporter;
  var RPrevOffset: Int64;
  var RPrevDT: TDateTime
): Boolean;

implementation

function GetParam(AParam: string): string;
var
  i: Integer;
  s: string;
  p1: Integer;
  p2: Integer;
begin
  Result :='';
  for i := 1 to ParamCount do
  begin
    s := ParamStr(i);
    p1 := Pos(LowerCase(AParam), LowerCase(s));
    if p1 = 2 then
    begin
      p2 := Pos('=', s);
      if p2 > p1 then
        Result := Copy(s, p2 + 1, Length(s) - p2);
      Break;
    end;
  end;
end;

function GetConfigSuffix(AParam: string = 'config'): string;
var
  ConfigParam: string;
begin
  ConfigParam := GetParam(AParam);
  Result := IfThen(ConfigParam <> '', '_' + ConfigParam, '');
end;

procedure SvcBeforeInstallUninstall(const AService: TService; const AServiceName: string; const AServiceDisplayName: string; const AParamName: string = 'config');
var
  ConfigParam: string;
begin
  ConfigParam := GetParam(AParamName);
  if ( ConfigParam <> '' ) then
  begin
    AService.Name := AServiceName + '_' + ConfigParam;
    AService.DisplayName := AServiceDisplayName + ' ' + ConfigParam;
  end
end;

procedure SvcAfterInstall(const AServiceName: string; const AParamName: string = 'config');
var
  ConfigParam: string;
  SCManager: SC_HANDLE;
  OurService: SC_HANDLE;
begin
  ConfigParam := GetParam(AParamName);
  if ( ConfigParam <> '' ) then
  begin
    SCManager := OpenSCManager( nil, nil, SC_MANAGER_ALL_ACCESS );
    if ( SCManager <> 0 ) then
    begin
      OurService := OpenService( SCManager, PChar( AServiceName + '_' + ConfigParam ), SERVICE_ALL_ACCESS );
      if ( OurService <> 0 ) then
      begin
        if not ChangeServiceConfig(
          OurService,
          SERVICE_WIN32_OWN_PROCESS,
          SERVICE_AUTO_START,
          SERVICE_ERROR_NORMAL,
          PChar( ParamStr( 0 ) + ' /' + AParamName + '=' + ConfigParam ),
          nil,
          nil,
          nil,
          nil,
          nil,
          nil
        )
        then
          raise Exception.Create( 'Не удалось изменение статуса службы' );
      end
      else
        raise Exception.Create( 'Не удалось подключение к службе' );
    end
    else
      raise Exception.Create( 'Не удалось подключение к менеджеру служб' );
  end;
end;

function GetInstanceName: string;
begin
  Result := IfThen(GetParam('config') = '', 'saas', GetParam('config'));
end;

function KafkaMPWrapper(
  const AMessage: AnsiString;
  const AOffset: Int64;
  const ATopic: AnsiString;
  const AProcessFunc: TKafkaProcessFunc;
  const ACheckDBFunc: TCheckRepairDBFunc;
  const ASyncDB: TCriticalSection;
  const ASyncINI: TCriticalSection;
  const ARAR: TRedisActivityReporter;
  var RPrevOffset: Int64;
  var RPrevDT: TDateTime
): Boolean;
var
  MessageU, TopicU: string;
  NewDT: TDateTime;
  MessageDT: TDateTime;
  Speed: Double;
  JSONObj: TJsonObject;
  INIFile: TIniFile;
//------------------------------------------------------------------------------
begin
  // инициализация
  MessageU := string(AMessage);
  TopicU := string(ATopic);
  Assert(Assigned(AProcessFunc), TopicU + ': параметр AProcessFunc должен присутствовать');
  Assert(Assigned(ACheckDBFunc), TopicU + ': параметр ACheckDBFunc должен присутствовать');
  Result := True;
  if (MessageU = '') then
    Exit;
  try
    // загрузка json
    JSONObj := nil; // затыкаем компилятор
    try
      JSONObj := TJsonObject.Parse(MessageU) as TJsonObject;
    except
      Exception.RaiseOuterException(Exception.Create(TopicU + ': ошибка разбора JSON сообщения:'#13#10 + MessageU));
    end;
    // обработка
    try
      if Assigned(ASyncDB) then
        ASyncDB.Acquire();
      try
//        if not ACheckDBFunc() then
//          Exit(False); // при потере связи с БД нужен перезапуск обработки
        try
          Result := AProcessFunc(JSONObj, MessageU, AOffset, MessageDT);
        except
          Result := False;
          Exception.RaiseOuterException(Exception.Create(TopicU + ': ошибка обработки JSON сообщения:'#13#10 + MessageU));
        end;
      finally
        if Assigned(ASyncDB) then
          ASyncDB.Release();
      end;
    finally
      JSONObj.Free();
    end;
  except
    on Ex: Exception do
    begin
      ExceptionToLog(Ex, TopicU + '//' + IntToStr(AOffset));
    end;
  end;
  // если сообщение обработано - сохраняем смещение сообщения
  if Result then
  try
    NewDT := Now();
    if (SecondsBetween(NewDT, RPrevDT) >= 5) then
    begin
      if Assigned(ASyncINI) then
        ASyncINI.Acquire();
      try
        INIFile := TIniFile.Create(ChangeFileExt(ParamStr(0), GetConfigSuffix() + CKafkaIdxDefExtDotted));
        try
          Speed := (AOffset - RPrevOffset) / (NewDT - RPrevDT) / SecsPerDay;
          INIFile.WriteString(CIniKafkaOffsetSection, CIniKafkaOffset + '_' + TopicU, IntToStr(AOffset));
          INIFile.WriteInteger(CIniStatSection, CIniStatPPS + '_' + TopicU, Round(Speed));
          ToLog('Offset = ' + IntToStr(AOffset) + ', PPS = ' + FloatToStr(Speed) + ', time = ' + FormatDateTime('yyyy-mm-dd hh:nn:ss.zzz', MessageDT));
          if Assigned(ARAR) then
            ARAR.SaveLastActivity((AOffset - RPrevOffset), OneSecond);
          RPrevDT := NewDT;
          RPrevOffset := AOffset;
        finally
          INIFile.Free();
        end;
      finally
        if Assigned(ASyncINI) then
          ASyncINI.Release();
      end;
    end;
  except
    ToLog(TopicU + ': ошибка сохранения позиции ' + IntToStr(AOffset) + ' в ini-файл');
    Result := False; // при ошибке сохранения позиции нужен перезапуск обработки
  end;
end;

end.

