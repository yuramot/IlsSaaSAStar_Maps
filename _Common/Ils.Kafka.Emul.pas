unit Ils.Kafka.Emul;
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------

//------------------------------------------------------------------------------
interface

uses
  SysUtils, StrUtils, Classes, Windows, SyncObjs;

//------------------------------------------------------------------------------
const

//------------------------------------------------------------------------------
//! копия из ULibKafka
//------------------------------------------------------------------------------
  RD_KAFKA_OFFSET_BEGINNING = -2; (* *< Start consuming from beginning of kafka partition queue:oldest msg *)
  RD_KAFKA_OFFSET_END = -1; (* *< Start consuming from end of kafka partition queue:next msg *)
  RD_KAFKA_OFFSET_STORED = -1000; (* *< Start consuming from offset retrieved from offset store *)
  RD_KAFKA_OFFSET_INVALID = -1001; (* *< Invalid offset *)
// !!! *** внимание !!! интерпретируем любое из этих значений, как RD_KAFKA_OFFSET_BEGINNING *** !!!

//------------------------------------------------------------------------------
type

//------------------------------------------------------------------------------
//! callback-функция логирования
//------------------------------------------------------------------------------
  TLogFunc = function(
    const AMessage: string;
    const ASkipTimeStamp: Boolean = False
  ): string;

//------------------------------------------------------------------------------
//! callback-функция приёма сообщения kafka-читателя
//------------------------------------------------------------------------------
  TOnMessageFunc = function(
    const AMessage: AnsiString;
    const AOffset: Int64;
    const ATopic: AnsiString
  ): Boolean of object;

//------------------------------------------------------------------------------
//! callback-функция (внутреннее использование)
//------------------------------------------------------------------------------
  TCBThreadFunc = procedure of object;

//------------------------------------------------------------------------------
//! поток обработки (внутреннее использование)
//------------------------------------------------------------------------------
  TKafkaThread = class(TThread)
  protected
    procedure Execute(); override;
  private
    FOnCB: TCBThreadFunc;
  public
    constructor Create(
      const AOnCB: TCBThreadFunc
    );
  end;

//------------------------------------------------------------------------------
//! эмулятор kafka-писателя
//------------------------------------------------------------------------------
  TKafkaProducerEmul = class
  private
    FOnLog: TLogFunc;
    FSync: TCriticalSection;
    FFile: TFileStream;
    FMsgNum: Int64;
  public
    constructor Create(
      const AFileName: string;
      const AOnLog: TLogFunc
    );
    destructor Destroy(); override;
    //!
    function Produce(
      APayload: AnsiString
    ): Integer;
    //!
    property OnLog: TLogFunc read FOnLog write FOnLog;
  end;

//------------------------------------------------------------------------------
//! эмулятор kafka-читателя
//------------------------------------------------------------------------------
  TKafkaConsumerEmul = class
  private
    FOnLog: TLogFunc;
    FOnMessage: TOnMessageFunc;
    FOffset: Int64;
    FTopic: AnsiString;
    FFile: TFileStream;
    FKafkaThread: TKafkaThread;
    procedure ConsumeCallback();
  public
    constructor Create(
      const AFileName: string;
      const AOnMessage: TOnMessageFunc;
      const AOnLog: TLogFunc
    );
    destructor Destroy(); override;
    //!
    function Start(
      const AOffset: Int64 = RD_KAFKA_OFFSET_INVALID;
      const ACreateThread: Boolean = True
    ): Boolean;
    //!
    function Stop(): Boolean;
    //!
    function CheckTryRestart(
      var ACheckedOffset: Int64
    ): Boolean;
    //!
    function GetMessage(
      out RMessage: AnsiString
    ): Boolean;
    //!
    property OnLog: TLogFunc read FOnLog write FOnLog;
    property NextOffset: Int64 read FOffset;
  end;

//------------------------------------------------------------------------------
implementation

//------------------------------------------------------------------------------
// TKafkaProducerEmul
//------------------------------------------------------------------------------

constructor TKafkaProducerEmul.Create(
  const AFileName: string;
  const AOnLog: TLogFunc
);
begin
  inherited Create();
  //
  FOnLog := AOnLog;
  FSync := TCriticalSection.Create();
  ForceDirectories(ExtractFilePath(AFileName));
  FFile := TFileStream.Create(AFileName, fmCreate or fmShareDenyWrite);
end;

destructor TKafkaProducerEmul.Destroy();
begin
  FFile.Free();
  FSync.Free();
  //
  inherited Destroy();
end;

function TKafkaProducerEmul.Produce(
  APayload: AnsiString
): Integer;
var
  RS: AnsiString;
//------------------------------------------------------------------------------
begin
  FSync.Acquire();
  try
    Result := 0;
    Inc(FMsgNum);
    RS := AnsiString(IntToStr(FMsgNum)) + AnsiString(':') + APayload + AnsiString(#13#10);
    try
      FFile.Write(RS[1], Length(RS));
    except
      on Ex: Exception do
      begin
        Dec(FMsgNum);
        if Assigned(FOnLog) then
          FOnLog('Ошибка при попытке вставки сообщения "' + string(APayload) + '" ' + Ex.ClassName + ': ' + Ex.Message);
        Result := -1;
      end;
    end;
  finally
    FSync.Release();
  end;
end;

//------------------------------------------------------------------------------
// TKafkaConsumerEmul
//------------------------------------------------------------------------------

procedure TKafkaConsumerEmul.ConsumeCallback();
var
  RS: AnsiString;
  R: Boolean;
//------------------------------------------------------------------------------
begin
  R := False;
  if Assigned(FOnMessage) and GetMessage(RS) then
  begin
    FKafkaThread.Synchronize(
      procedure
      begin
        try
          R := FOnMessage(RS, FOffset, FTopic);
        except
        end;
      end
    );
  end;
  if not R then
    Stop();
end;

constructor TKafkaConsumerEmul.Create(
  const AFileName: string;
  const AOnMessage: TOnMessageFunc;
  const AOnLog: TLogFunc
);
begin
  inherited Create();
  //
  FOnLog := AOnLog;
  FOnMessage := AOnMessage;
  FTopic := AnsiString(ExtractFileName(AFileName));
  FOffset := RD_KAFKA_OFFSET_BEGINNING;
  FFile := TFileStream.Create(AFileName, fmOpenRead or fmShareDenyNone);
end;

destructor TKafkaConsumerEmul.Destroy();
begin
  FKafkaThread.Free();
  //
  inherited Destroy();
end;

function TKafkaConsumerEmul.Start(
  const AOffset: Int64;
  const ACreateThread: Boolean
): Boolean;
begin
  FOffset := AOffset;
  if ACreateThread and not Assigned(FKafkaThread) then
    FKafkaThread := TKafkaThread.Create(ConsumeCallback);
  Result := True;
end;

function TKafkaConsumerEmul.Stop(): Boolean;
begin
  Result := True;
  if not Assigned(FKafkaThread) then
    Exit;
  FKafkaThread.FreeOnTerminate := True;
  FKafkaThread.Terminate();
  FKafkaThread := nil;
end;

function TKafkaConsumerEmul.CheckTryRestart(
  var ACheckedOffset: Int64
): Boolean;
begin
  Result := True;
end;

function TKafkaConsumerEmul.GetMessage(
  out RMessage: AnsiString
): Boolean;

  function ColonPos(
    const AStr: AnsiString
  ): Integer;
  begin
    for Result := 1 to Length(AStr) do
    begin
      if (AStr[Result] = ':') then
        Exit;
    end;
    Result := -1;
  end;

  function ReadStr(): AnsiString;
  var
    RCh: AnsiChar;
  begin
    Result := '';
    while (FFile.Read(RCh, 1) = 1) do
    begin
      if (RCh = #13) then
      begin
        FFile.Read(RCh, 1); // skip #10
        Break;
      end;
      Result := Result + RCh;
    end;
  end;

var
  RS: AnsiString;
  SP: Integer;
  ReadOffset: Int64;
label R;
//------------------------------------------------------------------------------
begin
  Result := False;
  try
    if (FOffset <= 0) then
      FOffset := 1;
R:
    RS := ReadStr();
    SP := ColonPos(RS);
    if (SP = -1) then
      Exit;
    ReadOffset := StrToInt64Def(string(Copy(RS, 1, SP - 1)), RD_KAFKA_OFFSET_INVALID);
    RMessage := Copy(RS, SP + 1, MaxInt);
    if (ReadOffset = FOffset) then
    begin
      Inc(FOffset);
      Exit(True);
    end
    else if (ReadOffset < FOffset) then
    begin
      goto R;
    end
    else
    begin
      FFile.Position := 0;
      goto R;
    end;
    Result := True;
  except
    on Ex: Exception do
    begin
      if Assigned(FOnLog) then
        FOnLog('Ошибка при попытке получения сообщения "' + string(RS) + '" ' + Ex.ClassName + ': ' + Ex.Message);
    end;
  end;
end;

//------------------------------------------------------------------------------
// TKafkaThread
//------------------------------------------------------------------------------

constructor TKafkaThread.Create(
  const AOnCB: TCBThreadFunc
);
begin
  FreeOnTerminate := False;
  FOnCB := AOnCB;
  //
  inherited Create(False);
end;

procedure TKafkaThread.Execute();
begin
  while not Terminated do
  begin
    if Assigned(FOnCB) then
      FOnCB();
    Windows.Sleep(0);
  end;
end;

end.

