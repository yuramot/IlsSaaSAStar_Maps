unit Ils.Kafka.Disp;
//------------------------------------------------------------------------------
// модуль класса-диспетчеризатора данных от kafka
//------------------------------------------------------------------------------
// работает по заданному критерию
//------------------------------------------------------------------------------

//------------------------------------------------------------------------------
interface

uses
  SysUtils, IniFiles, SyncObjs,
  Ils.Kafka, Ils.Logger,
  CINIFilesData,
  ULibKafka, JsonDataObjects;

//------------------------------------------------------------------------------
type

//------------------------------------------------------------------------------
//! запись инициализации kafka
//------------------------------------------------------------------------------
  TKafkaConfig = record
    BootStrap: string;
    Group: string;
    Topic: string;
    constructor Create(
      const ABootStrap: string;
      const AGroup: string;
      const ATopic: string
    );
  end;

//------------------------------------------------------------------------------
//! формат процедуры обратного вызова
//------------------------------------------------------------------------------
  TKafkaDispCallback = procedure(
    const AJSONObj: TJsonObject
  ) of object;

//------------------------------------------------------------------------------
//! запись с информаций об обратном вызове
//------------------------------------------------------------------------------
  TKafkaDispCallbackInfo = record
    CB: TKafkaDispCallback;
    Ref: string;
  end;

  TKafkaDispCallbackInfoArray = TArray<TKafkaDispCallbackInfo>;

//------------------------------------------------------------------------------
//! класс диспетчеризатор
//------------------------------------------------------------------------------
  TKafkaDisp = class
  private
    FKafka: TKafkaConsumer;
    FRecIndex: Int64;
    FCallbacks: TKafkaDispCallbackInfoArray;
    FLock: TCriticalSection;
    //!
    function FindHallMark(
      const AHallmark: string
    ): Integer;
    //! callback от kafka
    function OnKafkaData(
      const AMessage: AnsiString;
      const AOffset: Int64;
      const ATopic: AnsiString
    ): Boolean;
  public
    constructor Create(
      const Config: TKafkaConfig;
      const RecIndexIniFileName: string;
      const RecIndexRecord: string
    );
    destructor Destroy(); override;
    //! начать чтение из kafka
    procedure StartUp();
    //! подписатся
    procedure Subscribe(
      const AHallmark: string;
      const ACallback: TKafkaDispCallback
    );
    //! отписаться
    procedure UnSubscribe(
      const AHallmark: string
    );
  end;

//------------------------------------------------------------------------------
implementation

//------------------------------------------------------------------------------
// TKafkaConfig
//------------------------------------------------------------------------------

constructor TKafkaConfig.Create(
  const ABootStrap: string;
  const AGroup: string;
  const ATopic: string
);
begin
  BootStrap := ABootStrap;
  Group := AGroup;
  Topic := ATopic;
end;

//------------------------------------------------------------------------------
// TAPF
//------------------------------------------------------------------------------

constructor TKafkaDisp.Create(
  const Config: TKafkaConfig;
  const RecIndexIniFileName: string;
  const RecIndexRecord: string
);
var
  INIFile: TIniFile;
  RecIndexStr: string;
//------------------------------------------------------------------------------
begin
  inherited Create();
  //
  FLock := TCriticalSection.Create();
  INIFile := TIniFile.Create(RecIndexIniFileName);
  try
    RecIndexStr := INIFile.ReadString(CIniKafkaIndexSection, RecIndexRecord, '');
  finally
    INIFile.Free();
  end;
  ToLog(Format('KAFKA прочитанное смещение => "%s"', [RecIndexStr]));
  FRecIndex := StrToInt64Def(RecIndexStr, RD_KAFKA_OFFSET_BEGINNING);
  ToLog(Format('KAFKA установленное смещение => "%d"', [FRecIndex]));
  FKafka := TKafkaConsumer.Create(Config.BootStrap, Config.Group, Config.Topic, OnKafkaData, ToLog);
end;

destructor TKafkaDisp.Destroy();
begin
  FKafka.Free();
  FLock.Free();
  //
  inherited Destroy();
end;

procedure TKafkaDisp.StartUp();
begin
  FKafka.Start(FRecIndex);
end;

procedure TKafkaDisp.Subscribe(
  const AHallmark: string;
  const ACallback: TKafkaDispCallback
);
var
  Index: Integer;
  Len: Integer;
//------------------------------------------------------------------------------
begin
  FLock.Acquire();
  try
    Index := FindHallMark(AHallmark);
    if (Index = -1) then
    begin
      Len := Length(FCallbacks);
      SetLength(FCallbacks, Len + 1);
      FCallbacks[Len].CB := ACallback;
      FCallbacks[Len].Ref := AHallmark;
    end
    else
      FCallbacks[Index].CB := ACallback;
  finally
    FLock.Release();
  end;
end;

procedure TKafkaDisp.UnSubscribe(
  const AHallmark: string
);
var
  Index: Integer;
  NewLen: Integer;
//------------------------------------------------------------------------------
begin
  FLock.Acquire();
  try
    Index := FindHallMark(AHallmark);
    if (Index = -1) then
      Exit;
    NewLen := High(FCallbacks);
    FCallbacks[Index] := FCallbacks[NewLen];
    SetLength(FCallbacks, NewLen);    
  finally
    FLock.Release();
  end;
end;

function TKafkaDisp.FindHallMark(
  const AHallmark: string
): Integer;
begin
  for Result := Low(FCallbacks) to High(FCallbacks) do
  begin
    if (FCallbacks[Result].Ref = AHallmark) then
      Exit;
  end;
  Result := -1;
end;

function TKafkaDisp.OnKafkaData(
  const AMessage: AnsiString;
  const AOffset: Int64;
  const ATopic: AnsiString
): Boolean;
var
  JSONObj: TJsonObject;
//------------------------------------------------------------------------------
begin
//
end;

end.

