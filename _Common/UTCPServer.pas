unit UTCPServer;
//------------------------------------------------------------------------------
// модуль реализации TCP-сервера
//------------------------------------------------------------------------------

//------------------------------------------------------------------------------
interface

uses
  SysUtils, ExtCtrls, SyncObjs, Windows,
  blcksock, synsock,
  Ils.Logger, UThreads, UThreadManager, UMemStream;

type

//------------------------------------------------------------------------------
// forward declarations
//------------------------------------------------------------------------------
  TTCPServer = class;
  TTCPProcessor = class;

//------------------------------------------------------------------------------
//! массив классов обработчика входящих соединений
//------------------------------------------------------------------------------
  TTCPProcessorArray = array of TTCPProcessor;

//------------------------------------------------------------------------------
//! TCP-сервер
//------------------------------------------------------------------------------
  TTCPServer = class sealed
  private
    //! номер прослушиваемого порта
    FPortNumber: Integer;
    //! поток прослушивания
    FListenerThread: TThreadInfinite;
    //! поток обработки
    FWorkerThread: TThreadInfinite;
    //! сокет прослушивателя входящих соединений
    FListenerSocket: TTCPBlockSocket;
    //! таймер проверки ошибок
    FErrorTimer: TTimer;
    //! синхронизация
    FStopper: TCriticalSection;
    //! пул открытых входящих соединений
    FRecievePool: TTCPProcessorArray;
    //! флаг возникшей ошибки
    FError: Boolean;
    //! создание рабочей системы
    procedure CoreCreate();
    //! удаление рабочей системы
    procedure CoreDestroy();
    //! процедура потока прослушивателя
    procedure Listen();
    //! процедура потока обработки
    procedure Work();
    //! процедура проверки ошибки (событие таймера)
    procedure CheckError(
      Sender: TObject
    );
  public
    //!
    constructor Create(
      const APortNumber: Integer
    );
    //!
    destructor Destroy(); override;
  end;

//------------------------------------------------------------------------------
//! обработчик входящего соединения
//------------------------------------------------------------------------------
  TTCPProcessor = class abstract
  private
    //! ссылка на сервер
    FServer: TTCPServer;
    //! время создания
    FDTCreated: TDateTime;
    //! время начала выполнения работы
    FDTStartWork: TDateTime;
    //! флаг выполнения работы
    FInWork: Boolean;
    //! флаг необходимости закрытия
    FMustClose: Boolean;
    //! TCP-сокет соединения
    FTCPSocket: TTCPBlockSocket;
    //! хранилище принятых данных
    FInputData: TMyMemStream;
    //! хранилище отправляемых данных
    FOutputData: TMyMemStream;
    //! приём данных
    procedure Acceptor();
  protected
    //!
    function DataAnalyzer(): Boolean; virtual; abstract;
    //!
    property InputData: TMyMemStream read FInputData;
    //!
    property OutputData: TMyMemStream read FOutputData;
  public
    //!
    constructor Create(
      const AServer: TTCPServer;
      const ASocket: TSocket
    ); virtual;
    //!
    destructor Destroy(); override;
  end;

//------------------------------------------------------------------------------
//! класс обработчика входящего соединения
//------------------------------------------------------------------------------
  TTCPProcessorClass = class of TTCPProcessor;

//------------------------------------------------------------------------------
var

//------------------------------------------------------------------------------
//! переменная для типа обработчика
//------------------------------------------------------------------------------
  GTCPProcessor: TTCPProcessorClass;

//------------------------------------------------------------------------------
implementation

const

//------------------------------------------------------------------------------
//! интервал проверки ошибок в двух основных потоках
//!   в случае ошибки непрерывно будут выполнятся попытки перезапуска сервера
//------------------------------------------------------------------------------
  CSSCheckInterval = 5000; // 5 секунд

//------------------------------------------------------------------------------
//! тайм-аут на закрытие сокета
//!   после этого промежутка TCP-соединение с прибором рвётся принудительно
//------------------------------------------------------------------------------
  CCloseTime = 10.0 / 1440; // 10 минут

//------------------------------------------------------------------------------
//! тайм-аут работы задачи до сброса
//!   после этого времени работающему соединению посылается AbortSocket
//------------------------------------------------------------------------------
  CTCPTimeOut = 2.0 / 1440; // 2 минуты

//------------------------------------------------------------------------------
//! рабочая процедура обработки данных
//------------------------------------------------------------------------------
procedure ProcessMe(
  const AData: Pointer
);
begin
  TTCPProcessor(AData).Acceptor();
end;

//------------------------------------------------------------------------------
//! рабочая процедура закрытия сокета
//------------------------------------------------------------------------------
procedure CloseMe(
  const AData: Pointer
);
begin
  TTCPProcessor(AData).FTCPSocket.AbortSocket();
  TTCPProcessor(AData).FMustClose := True;
end;

//------------------------------------------------------------------------------
//! рабочая процедура прослушивания
//------------------------------------------------------------------------------
procedure ListenerDo(
  const AData: Pointer
);
begin
  TTCPServer(AData).Listen();
end;

//------------------------------------------------------------------------------
//! процедура обработки ошибки прослушивания
//------------------------------------------------------------------------------
procedure ListenerError(
  const AData: Pointer;
  var AException: Exception;
  const AThreadID: TThreadID
);
begin
  ToLog('Ошибка TCP:'#13#10'ошибка потока прослушивателя:'#13#10 + AException.Message);
  TTCPServer(AData).FError := True;
end;

//------------------------------------------------------------------------------
//! рабочая процедура приёма данных
//------------------------------------------------------------------------------
procedure WorkerDo(
  const AData: Pointer
);
begin
  TTCPServer(AData).Work();
end;

//------------------------------------------------------------------------------
//! процедура обработки ошибки приёма данных
//------------------------------------------------------------------------------
procedure WorkerError(
  const AData: Pointer;
  var AException: Exception;
  const AThreadID: TThreadID
);
begin
  ToLog('Ошибка TCP:'#13#10'ошибка потока приёма данных:'#13#10 + AException.Message);
  TTCPServer(AData).FError := True;
end;

//------------------------------------------------------------------------------
// TTCPServer
//------------------------------------------------------------------------------

constructor TTCPServer.Create(
  const APortNumber: Integer
);
begin
  inherited Create();
  //
  FPortNumber := APortNumber;
  //
  FStopper := TCriticalSection.Create();
  //
  CoreCreate();
  //
  FErrorTimer := TTimer.Create(nil);
  FErrorTimer.Interval := CSSCheckInterval;
  FErrorTimer.Enabled := True;
  FErrorTimer.OnTimer := CheckError;
end;

destructor TTCPServer.Destroy();
begin
  FErrorTimer.Free();
  CoreDestroy();
  FStopper.Free();
  //
  inherited Destroy();
end;

procedure TTCPServer.CoreCreate();
begin
  FListenerSocket := TTCPBlockSocket.Create();
  FListenerSocket.CreateSocket();
  FListenerSocket.Family := SF_IP4;
  FListenerSocket.Bind('0.0.0.0', IntToStr(FPortNumber));
  FListenerSocket.SetLinger( False, 0 );
  FListenerSocket.Listen();
  //
  FListenerThread := TThreadInfinite.Create(ListenerDo, ListenerError, Self);
  FWorkerThread := TThreadInfinite.Create(WorkerDo, WorkerError, Self);
  //
  FError := False;
end;

procedure TTCPServer.CoreDestroy();
var
  //!
  I: Integer;
//------------------------------------------------------------------------------
begin
  // заглушка на выключение
  FError := True;
  //
  FreeAndNil(FListenerThread);
  FreeAndNil(FWorkerThread);
  // закрываем сокеты обработки
  for I := Low(FRecievePool) to High(FRecievePool) do
  begin
    if Assigned(FRecievePool[I]) then
      FRecievePool[I].FTCPSocket.CloseSocket();
  end;
  // удаляем классы обработки
  for I := Low(FRecievePool) to High(FRecievePool) do
  begin
    if Assigned(FRecievePool[I]) then
    begin
      while FRecievePool[I].FInWork do
      begin
        Windows.Sleep(0);
      end;
      FreeAndNil(FRecievePool[I]);
    end;
  end;
  //
  SetLength(FRecievePool, 0);
  // итоговое закрытие сокета приёма
  FreeAndNil(FListenerSocket);
end;

procedure TTCPServer.Listen();
var
  //!
  Processor: TTCPProcessor;
  //!
  I: Integer;
//------------------------------------------------------------------------------
begin
  if FError then Exit;
  // проверим есть ли новое подключение
  if not FListenerSocket.CanRead(1000) then Exit;
  // ... есть
  // создать класс = принять подключение
  // *** фабрика ***
  Processor := GTCPProcessor.Create(Self, FListenerSocket.Accept());
  try
    if (GLogLevel >= 2) then
      ToLog(Format('Входящее: %s %d', [FListenerSocket.GetRemoteSinIP, FListenerSocket.GetRemoteSinPort]));
    FStopper.Acquire();
    try
      // добавить класс в массив
      //  проверяем есть ли дырка в массиве
      for I := Low(FRecievePool) to High(FRecievePool) do
      begin
        if not Assigned(FRecievePool[I]) then
        begin
          // ... есть - сохраним в неё
          FRecievePool[I] := Processor;
          Exit;
        end;
      end;
      // ... нету - расширим массив
      SetLength(FRecievePool, Length(FRecievePool) + 1);
      FRecievePool[High(FRecievePool)] := Processor;
    finally
      FStopper.Release();
    end;
  except
    Processor.Free();
    raise;
  end;
end;

procedure TTCPServer.Work();
var
  //! для скорости
  Processor: TTCPProcessor;
  //!
  I: Integer;
//------------------------------------------------------------------------------
begin
  if FError then Exit;
  FStopper.Acquire();
  try
    for I := Low(FRecievePool) to High(FRecievePool) do
    begin
      // если совсем ошибка - то прервёмся
      if FError then Exit;
      // копия ссылки для скорости
      Processor := FRecievePool[I];
      // всё - только если у нас вообще есть объект работы
      if not Assigned(Processor) then Continue;
      // если тайм-аут - сразу флаг
      if (Now() - Processor.FDTCreated > CCloseTime) then
        Processor.FMustClose := True;
      //
      if not Processor.FInWork then
      begin // сервис не работает сейчас
        // если надо - закрыть соединение
        if Processor.FMustClose then
        begin
          FreeAndNil(FRecievePool[I]); // за'nil'ить - обязательно
          Continue;
        end;
        // если надо - работать (если есть с чем = новые данных ИЛИ данные в буфере)
        if Processor.FTCPSocket.CanRead(0)
        or (Processor.FInputData.Remaining <> 0) then
        begin
          Processor.FDTStartWork := Now();
          Processor.FInWork := True; // сбросим в конце ProcessMe
          //
          if not TryExecuteThis(ProcessMe, nil, FRecievePool[I]) then
          begin
            Processor.FMustClose := True; // должно идти до FMode
            Processor.FInWork := False; // сбросим флаг работы сами, ибо мы не вызвали ProcessMe
          end;
        end;
      end
      else
      begin // сервис работает сейчас
        // если мы заклинились на приёме - закрыть
        if (Now() - Processor.FDTStartWork > CTCPTimeOut) then
        begin
          if (GLogLevel >= 1) then
            ToLog(Format('Тайм-аут соединения: %s %d', [Processor.FTCPSocket.GetRemoteSinIP, Processor.FTCPSocket.GetRemoteSinPort]));
          // зашэдулим по любому
          while not TryExecuteThis(CloseMe, nil, FRecievePool[I]) do ;
          //
          if (GLogLevel >= 1) then
            ToLog(Format('Послан AbortSocket соединению: %s %d', [Processor.FTCPSocket.GetRemoteSinIP, Processor.FTCPSocket.GetRemoteSinPort]));
        end;
      end;
    end;
  finally
    FStopper.Release();
  end;
  // ждём один цикл - *** иначе без активных коннектов будет 100% загрузка потока
  Windows.Sleep(1);
end;

procedure TTCPServer.CheckError(
  Sender: TObject
);
begin
  if not FError then Exit;
  FErrorTimer.Enabled := False;
  try
    try
      ToLog('Ошибка TCP:'#13#10'зафиксирован факт глобальной ошибки'#13#10'выполняем попытку остановки рабочей подсистемы');
      CoreDestroy();
    except
      on Ex: Exception do
      begin
        ToLog('Ошибка TCP:'#13#10'ошибка при попытке остановки рабочей подсистемы:'#13#10 + Ex.Message);
        Exit;
      end;
    end;
    try
      ToLog('Ошибка TCP:'#13#10'зафиксирован факт глобальной ошибки'#13#10'выполняем попытку запуска рабочей подсистемы');
      CoreCreate();
    except
      on Ex: Exception do
      begin
        ToLog('Ошибка TCP:'#13#10'ошибка при попытке запуска рабочей подсистемы:'#13#10 + Ex.Message);
      end;
    end;
    ToLog('Ошибка TCP:'#13#10'рабочая подсистема успешно перезапущена после глобальной ошибки!');
  finally
    FErrorTimer.Enabled := True;
  end;
end;

//------------------------------------------------------------------------------
// TTCPProcessor
//------------------------------------------------------------------------------

constructor TTCPProcessor.Create(
  const AServer: TTCPServer;
  const ASocket: TSocket
);
begin
  inherited Create();
  //
  FDTCreated := Now();
  FDTStartWork := 54789; //*** 2050.01.01
  FServer := AServer;
  //
  FInputData := TMyMemStream.Create();
  FOutputData := TMyMemStream.Create();
  FTCPSocket := TTCPBlockSocket.Create();
  FTCPSocket.Socket := ASocket;
end;

destructor TTCPProcessor.Destroy();
begin
  FTCPSocket.Free();
  FOutputData.Free();
  FInputData.Free();
  //
  inherited Destroy();
end;

procedure TTCPProcessor.Acceptor();
var
  //! прочитанный байт
  ByteRead: Byte;
  //! промежуточный маччив данных ответа
  BytesSend: TBytes;
  //! длина пакета ответа
  SendLength: Integer;
//------------------------------------------------------------------------------
begin
  try
    // читаем данные
    try
      while FTCPSocket.CanRead(0) do
      begin
        // прочитаем один байт
        FTCPSocket.RecvBuffer(@ByteRead, 1);
        // сохраним его
        FInputData.AddData(1, @ByteRead);
      end;
    except
      on Ex: ESynapseError do
      begin
        FMustClose := True;
        if (GLogLevel >= 2)
        or ((Ex.ErrorCode <> 10053) and (Ex.ErrorCode <> 10054)) then
          ToLog(Format('Ошибка TCP:'#13#10'ошибка приёма:'#13#10'"%d" %s', [Ex.ErrorCode, Ex.Message]));
        Exit;
      end;
      on Ex: Exception do
      begin
        FMustClose := True;
        ToLog(Format('Ошибка TCP:'#13#10'общая ошибка приёма:'#13#10'%s', [Ex.Message]));
        Exit;
      end;
    end;
    // вызываем внешний обработчик
    FMustClose := not DataAnalyzer();
    // отправляем ответ
    FOutputData.UnreadReaded();
    SendLength := FOutputData.Remaining;
    if (SendLength = 0) then Exit;
    try
      SetLength(BytesSend, SendLength);
      FOutputData.ReadData(SendLength, @BytesSend[0]);
      if (FTCPSocket.SendBuffer(@BytesSend[0], SendLength) <> SendLength) then
        raise Exception.Create('ответ отправлен не полностью');
      FOutputData.ClearAll();
    except
      on Ex: ESynapseError do
      begin
        FMustClose := True;
        if (GLogLevel >= 2)
        or ((Ex.ErrorCode <> 10053) and (Ex.ErrorCode <> 10054)) then
          ToLog(Format('Ошибка TCP:'#13#10'ошибка отправки ответа:'#13#10'"%d" %s', [Ex.ErrorCode, Ex.Message]));
        Exit;
      end;
      on Ex: Exception do
      begin
        FMustClose := True;
        ToLog(Format('Ошибка TCP:'#13#10'общая ошибка отправки ответа:'#13#10'%s', [Ex.Message]));
        Exit;
      end;
    end;
  finally
    FInWork := False; // мы должны проставить флаг, иначе класс не сможет быть уничтожен
  end;
end;

end.

