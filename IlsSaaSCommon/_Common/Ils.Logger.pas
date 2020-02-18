unit Ils.Logger;
//------------------------------------------------------------------------------
// модуль реализует класс логирования
//------------------------------------------------------------------------------
// логирование осуществляется в файл, расположение и имя которого определяется
// по следующему принципу:
// если путь к исполняемому файлу выглядит как
//  диск:\путь\ИмяИсполняемогоФайла.exe
// то путь к файлу лога будет
//  диск:\путь\log\ИмяИсполняемогоФайла.log
//
// после смены суток файл лога переименовывается в
//  диск:\путь\log\ИмяИсполняемогоФайла__yyyymmdd.log
// где yyyymmdd - текущие год, месяц и день предыдущего дня
//------------------------------------------------------------------------------

//------------------------------------------------------------------------------
interface

uses
  SysUtils, StrUtils, Windows, Math, Classes;

var
//------------------------------------------------------------------------------
// переменная уровня лога
//------------------------------------------------------------------------------
// *** ошибки должны логироваться всегда ***
//  рекомендуемые уровни:
//    0 - только базовая информация (=начало/конец работы)
//    1 - информация о нестандартном поведении (например таймаутах событий)
//    2 - информация о событиях (например новых TCP подключениях)
//    3 - полный лог + дамп всех данных
//    4* - для сервисов приёма происходит дамп итоговых JSON'ов в файлы
//------------------------------------------------------------------------------
  GLogLevel: Integer;

type
  TLogFunction = function(const AMessage: string; const ASkipTimeStamp: Boolean = False): string;

//------------------------------------------------------------------------------
//! записать сообщение в лог
//------------------------------------------------------------------------------
function ToLog(
  const AMessage: string;
  const ASkipTimeStamp: Boolean = False
): string;

function ToLogHex(
  const AMessage: string
): string;

//------------------------------------------------------------------------------
//! записать сообщение в лог
//------------------------------------------------------------------------------
function ExceptionToLog(
  const AExcept: Exception;
  const AHeader: string = ''
): string;

function ExceptionToLogNew(
  const AExcept: Exception;
  const AHeader: string = ''
): string;

function StartLog(const ASuffix: string; const AMultiProcess: Boolean = False): Boolean;

//------------------------------------------------------------------------------
implementation

const

//------------------------------------------------------------------------------
//! строка формата даты-времени
//------------------------------------------------------------------------------
  CDateTimeFormat       : string = 'yyyy"."mm"."dd" "hh":"nn":"ss"."zzz';
  CDateTimeFormatNoDate : string = '                       ';

//------------------------------------------------------------------------------
//! шаблон формата строки лога
//------------------------------------------------------------------------------
  CLogStrFormat: string = '%s %s'#13#10;

//------------------------------------------------------------------------------
type

//------------------------------------------------------------------------------
//! TNewLogger
//------------------------------------------------------------------------------
  TNewLogger = class
  private
    FLogFileName: string;
    FPrevDate: TDate;
    FHandle: THandle;
    FMultiProcess: Boolean;
    procedure CheckTransition();
    procedure LogOpen();
    procedure LogClose();
  public
    constructor Create(const ALogName: string = ''; const AMultiProcess: Boolean = False);
    destructor Destroy(); override;
    procedure ToLog(const AMessage: string; const ASkipTimeStamp: Boolean = False);
  end;

//------------------------------------------------------------------------------
var
  GLogger: TNewLogger;
  GLoggerSuffix: string;

function StartLog(const ASuffix: string; const AMultiProcess: Boolean = False): Boolean;
begin
  FreeAndNil(GLogger);
  try
    GLoggerSuffix := ASuffix;
    GLogger := TNewLogger.Create(ChangeFileExt(ExtractFileName(GetModuleName(HInstance)), IfThen(ASuffix <> '', '_' + ASuffix, '')), AMultiProcess);
  except
    // подавляю специально - такие ситуации могут возникнуть в сервисах
  end;
  Result := Assigned(GLogger);
end;

function ToLog(
  const AMessage: string;
  const ASkipTimeStamp: Boolean = False
): string;
begin
  if not Assigned(GLogger) then
    StartLog(GLoggerSuffix);
  GLogger.ToLog(AMessage, ASkipTimeStamp);
  Result := AMessage;
end;

function ToLogHex(
  const AMessage: string
): string;

  function StrToHexStr(const S: AnsiString): AnsiString;
  var
    Hex: PAnsiChar;
  begin
    Hex := AllocMem(Length(S) * 2);
    try
      BinToHex(PAnsiChar(S), Hex, Length(S));
      Result := Copy(Hex, 0, Length(S) * 2);
    finally
      FreeMem(Hex,Length(S) * 2);
    end;
  end;

var
  i: Integer;
  LineNo: Integer;
  LogStr1: AnsiString;
  LogStr2: AnsiString;
  Hex: AnsiString;
  s: AnsiString;
  stc: AnsiString;
begin
  s := AnsiString(AMessage);
  LineNo := 0;
  while (Length(s) > 0) do
  begin
    stc := Copy(s, 1, 16);
    Delete(s, 1, 16);
    Hex := StrToHexStr(stc);
    LogStr1 := '';
    for i := 1 to Length(Hex) do
    begin
      if ((i mod 2) > 0) and (i > 1) then
        LogStr1 := LogStr1 + '  ';
      LogStr1 := LogStr1 + Hex[i];
    end;
    LogStr1 := LogStr1 + AnsiString(StringOfChar(' ', (4 * 16) - Length(LogStr1)));
    LogStr2 := '';
    for i := 1 to Length(stc) do
    begin
      if ((stc[i] >= #32) and (stc[i] <= #127)) then
        LogStr2 := LogStr2 + '   ' + stc[i]
      else
        LogStr2 := LogStr2 + ' #' + AnsiString(StrToHexStr(stc[i]));
    end;
    if (LineNo = 0) then
      ToLog(string(LogStr1 + '|' + LogStr2))
    else
      ToLog(string(LogStr1 + '|' + LogStr2), True);
    Inc(LineNo);
  end;
end;

function ExceptionToLog(
  const AExcept: Exception;
  const AHeader: string
): string;
var
  CurrExcept: Exception;
  Level: Integer;
begin
  if (AHeader <> '') then
    Result := 'Возникло исключение (' + AHeader + '):'
  else
    Result := 'Возникло исключение:';
  Level := 1;
  CurrExcept := AExcept;
  repeat
    Result := Format('%s'#13#10'уровень %d:'#13#10'%s', [Result, Level, CurrExcept.Message]);
    Inc(Level);
    CurrExcept := CurrExcept.InnerException;
  until not Assigned(CurrExcept);
  if (AExcept.StackTrace <> '') then
    Result := Format('%s'#13#10'дамп стека:'#13#10'%s', [Result, AExcept.StackTrace]);
  GLogger.ToLog(Result);
end;

function ExceptionToLogNew(
  const AExcept: Exception;
  const AHeader: string
): string;
begin
  Result := 'Возникло исключение';
  if (AHeader <> '') then
    Result := Result + ' (' + AHeader + ')';
  Result := Result + ' <' + AExcept.ClassName + '>'#13#10 + AExcept.Message;
  if (AExcept.StackTrace <> '') then
    Result := Result + #13#10'дамп стека:'#13#10 + AExcept.StackTrace;
  GLogger.ToLog(Result);
end;

//------------------------------------------------------------------------------
// TNewLogger
//------------------------------------------------------------------------------

constructor TNewLogger.Create(const ALogName: string = ''; const AMultiProcess: Boolean = False);
var
  FileDT: TFileTime;
  SystemDT: TSystemTime;
begin
  inherited Create();
  //
  FLogFileName := ALogName;
  FMultiProcess := AMultiProcess;
  if (FLogFileName = '') then
    FLogFileName := ChangeFileExt(ExtractFileName(GetModuleName(HInstance)), '');
  ForceDirectories(ExtractFilePath(GetModuleName(HInstance)) + 'log\');
  LogOpen();
  if GetFileTime(FHandle, nil, nil, @FileDT) then
  begin
    if FileTimeToSystemTime(@FileDT, SystemDT) then
    begin
      FPrevDate := EncodeDate(SystemDT.wYear, SystemDT.wMonth, SystemDT.wDay);
      if (FPrevDate < Date()) then
        Exit;
    end;
  end;
  FPrevDate := Date();
end;

destructor TNewLogger.Destroy();
begin
  LogClose();
  //
  inherited Destroy();
end;

procedure TNewLogger.ToLog(
  const AMessage: string;
  const ASkipTimeStamp: Boolean = False
);
var
  MesFormatted: string;
  MesRef: PChar;
  RetPosition: Int64;
  ActualWrite: DWORD;
begin
  if (AMessage = '') then
    Exit;
  try
    CheckTransition();
    if ASkipTimeStamp then
      MesFormatted := Format(CLogStrFormat, [CDateTimeFormatNoDate, AMessage])
    else
      MesFormatted := Format(CLogStrFormat, [FormatDateTime(CDateTimeFormat, Now()), AMessage]);
    MesRef := PChar(MesFormatted);
    if FMultiProcess then
      SetFilePointerEx(FHandle, 0, @RetPosition, FILE_END);
    Windows.WriteFile(FHandle, MesRef^, Length(MesFormatted) * SizeOf(Char), ActualWrite, nil);
  except
    // игнорируем все ошибки
  end;
end;

procedure TNewLogger.CheckTransition();
var
  LogPath, ArchPath, PathPath, TempStr: string;
begin
  if (FPrevDate = Date()) then
    Exit;
  LogClose();
  PathPath := ExtractFilePath(GetModuleName(HInstance)) + 'log\';
  TempStr := FLogFileName;
  LogPath := ChangeFileExt(PathPath + TempStr, '.log');
  ArchPath := PathPath + TempStr + FormatDateTime('"__"yyyymmdd', Date() - 1) + '.log';
  RenameFile(LogPath, ArchPath);
  FPrevDate := Date();
  LogOpen();
end;

procedure TNewLogger.LogOpen();
var
  XPath: string;
  RetPosition: Int64;
  ErrorCode: DWORD;
begin
  XPath := ExtractFilePath(GetModuleName(HInstance)) + 'log\' + FLogFileName + '.log';
  FHandle := CreateFile(
    PChar(XPath),
    GENERIC_WRITE,
    IfThen(
      FMultiProcess,
      FILE_SHARE_READ or FILE_SHARE_DELETE or FILE_SHARE_WRITE,
      FILE_SHARE_READ or FILE_SHARE_DELETE
    ),
    nil,
    OPEN_ALWAYS,
    FILE_ATTRIBUTE_NORMAL,
    0
  );
  ErrorCode := GetLastError();
  if (FHandle = INVALID_HANDLE_VALUE) then
    RaiseLastOSError(ErrorCode);
  if not SetFilePointerEx(FHandle, 0, @RetPosition, FILE_END) then
    RaiseLastOSError();
end;

procedure TNewLogger.LogClose();
begin
  if (FHandle <> INVALID_HANDLE_VALUE) and (FHandle <> 0) then
    CloseHandle(FHandle);
  FHandle := 0;
end;

//------------------------------------------------------------------------------
initialization
  GLogger := nil;
  GLoggerSuffix := '';
{$ifndef NO_AUTOCREATE_LOGGER_OBJECT}
{$ifdef MULTIPROCESS_LOGGER_OBJECT}
  StartLog(GLoggerSuffix, True);
{$else}
  StartLog(GLoggerSuffix);
{$endif}
{$endif}

//------------------------------------------------------------------------------
finalization
  GLogger.Free();

end.

