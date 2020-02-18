unit ReceiverMain;
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------

//------------------------------------------------------------------------------
interface

uses
  SysUtils, SvcMgr, Classes, Windows, Messages, Graphics, Controls, Dialogs,
  ExtCtrls, SyncObjs, DBClient, ActiveX, IniFiles, StrUtils, DateUtils, Math,
  blcksock, synsock,
  C_FileNames, T_Common, T_Points,
  E_Utils, E_UtilsStr, E_UtilsFiles, E_Logger, E_Files,
  E_Threads, E_ThreadManager, E_MemStream, E_Pairs;

//------------------------------------------------------------------------------
type

//------------------------------------------------------------------------------
// forward declarations
//------------------------------------------------------------------------------
  TTCPServer = class;
  TTCPProcessor = class;

//------------------------------------------------------------------------------
//! массив классов обработчика данных
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
    //! синхронизация
    FStopper: TCriticalSection;
    //! сокет прослушивателя входящих соединений
    FListenerSocket: TTCPBlockSocket;
    //! таймер проверки ошибок
    FErrorTimer: TTimer;
    //! пул открытых входящих соединений
    FRecievePool: TTCPProcessorArray;
    //! флаг возникшей ошибки
    FError: Boolean;
    //! счётчик ID TCP-соединений
    //! используется для присваивания очередному соединению уникального номера
    FChannelID: Integer;
    //!
    FReplaceFile: Boolean;
    //! создание рабочей системы
    procedure CoreCreate();
    //! удаление рабочей системы
    procedure CoreDestroy();
    //! процедура проверки ошибки (по таймеру)
    procedure CheckError(
      Sender: TObject
    );
    //! процедура прослушивания
    procedure CoreListener();
    //! процедура приёма данных
    procedure CoreWorker();
  public
    //!
    constructor Create(
      const APortNumber: Integer;
      const AReplaceFile: Boolean
    );
    //!
    destructor Destroy(); override;
  end;

//------------------------------------------------------------------------------
//! класс обработчика данных
//------------------------------------------------------------------------------
  TTCPProcessor = class sealed
  private
    //! буфер приёма
    FRBuffer: Pointer;
    //! время создания
    FDTCreated: TDateTime;
    //! время сброса дампа на диск
    FDTSaved: TDateTime;
    //! время начала выполнения работы
    FDTStartWork: TDateTime;
    //! TCP-сокет соединения
    FTCPSocket: TTCPBlockSocket;
    //! флаг выполнения работы
    {!
      0 - ничего
      1 - приём
      2 - внешняя обработка по таймеру
    }
    FMode: Integer;
    //! флаг необходимости закрытия
    FMustClose: Boolean;
    //! флаг процесса закрытия
    FCloseing: Boolean;
    //! глобальный ID TCP-соединения
    FID: Integer;

    FReplaceFile: Boolean;
  public
    //!
    constructor Create(
      const ASocket: TSocket;
      const AID: Integer;
      const AReplaceFile: Boolean
    );
    //!
    destructor Destroy(); override;
    //! процедура обработки данных
    procedure Analyze();
  end;

//------------------------------------------------------------------------------
//! форма
//------------------------------------------------------------------------------
  TILS_CommonReceiver = class(TService)
    procedure ServiceStart(Sender: TService; var Started: Boolean);
    procedure ServiceStop(Sender: TService; var Stopped: Boolean);
  private
    //! лог
    FLogger: TLogger;
    //! корневая рабочая папка
    FWorkFolder: string;
    //! порт TCP-сервера
    FPortNumber: Integer;
    //! TCP-сервер
    FTCPServer: TTCPServer;
    //!
    FReplaceFile: Boolean;
  public
    function GetServiceController: TServiceController; override;
  end;

//------------------------------------------------------------------------------
var

//------------------------------------------------------------------------------
//! переменная главного окна
//------------------------------------------------------------------------------
  ILS_CommonReceiver: TILS_CommonReceiver;

//------------------------------------------------------------------------------
//! копия глобальной переменной главного окна выше
//! для обращений без переименования по всему тексту программы
//------------------------------------------------------------------------------
  GService: TILS_CommonReceiver;

//------------------------------------------------------------------------------
implementation

{$R *.DFM}

//------------------------------------------------------------------------------
const

//------------------------------------------------------------------------------
//! сообщение об ошибке
//------------------------------------------------------------------------------
  CProtocolError: string = 'Ошибка в принятых данных:'#13#10'%s';

//------------------------------------------------------------------------------
//! количество попыток сохранения
//------------------------------------------------------------------------------
  CMaxSaveTry = 100;

//------------------------------------------------------------------------------
//! тайм-аут на закрытие сокета
//! после этого промежутка TCP-соединение с прибором рвётся принудительно
//------------------------------------------------------------------------------
  CCloseTime = 5.0 / 1440; // 5 минут

//------------------------------------------------------------------------------
//! интервал проверки ошибок
//------------------------------------------------------------------------------
  CErrorsCheckInterval = 5000; // 5 секунд

//------------------------------------------------------------------------------
//! рабочая процедура обработки данных
//------------------------------------------------------------------------------
procedure ProcessMe(
  const AData: Pointer
);
begin
  TTCPProcessor( AData ).Analyze();
end;

//------------------------------------------------------------------------------
//! рабочая процедура закрытия сокета
//------------------------------------------------------------------------------
procedure CloseMe(
  const AData: Pointer
);
begin
  TTCPProcessor( AData ).FTCPSocket.AbortSocket();
  TTCPProcessor( AData ).FMustClose := True;
end;

//------------------------------------------------------------------------------
//! внешняя процедура приёма данных
//------------------------------------------------------------------------------
procedure WorkerDo(
  const AData: Pointer
);
begin
  TTCPServer( AData ).CoreWorker();
end;

//------------------------------------------------------------------------------
//! внешняя процедура обработки ошибки приёма данных
//------------------------------------------------------------------------------
procedure WorkerError(
  const AData: Pointer;
  var AException: Exception;
  const AThreadID: TThreadID
);
begin
  GService.FLogger.ToLog( 'Ошибка TCP: ошибка потока приёма данных:'#13#10 + AException.Message );
  TTCPServer( AData ).FError := True;
end;

//------------------------------------------------------------------------------
//! внешняя процедура прослушивания
//------------------------------------------------------------------------------
procedure ListenerDo(
  const AData: Pointer
);
begin
  TTCPServer( AData ).CoreListener();
end;

//------------------------------------------------------------------------------
//! внешняя процедура обработки ошибки прослушивания
//------------------------------------------------------------------------------
procedure ListenerError(
  const AData: Pointer;
  var AException: Exception;
  const AThreadID: TThreadID
);
begin
  GService.FLogger.ToLog( 'Ошибка TCP: ошибка потока прослушивателя:'#13#10 + AException.Message );
  TTCPServer( AData ).FError := True;
end;

//------------------------------------------------------------------------------
// TTCPServer
//------------------------------------------------------------------------------

constructor TTCPServer.Create(
  const APortNumber: Integer;
  const AReplaceFile: Boolean
);
begin
  inherited Create();
  //
  FPortNumber := APortNumber;
  FReplaceFile := AReplaceFile;
  //
  FStopper := TCriticalSection.Create();
  //
  CoreCreate();
  //
  FErrorTimer := TTimer.Create( nil );
  FErrorTimer.Interval := CErrorsCheckInterval;
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
  FListenerSocket.Bind( '0.0.0.0', IntToStr( FPortNumber ) );
  FListenerSocket.SetLinger( False, 0 );
  FListenerSocket.Listen();
  //
  FListenerThread := TThreadInfinite.Create( ListenerDo, ListenerError, Self );
  FWorkerThread := TThreadInfinite.Create( WorkerDo, WorkerError, Self );
  //
  FError := False;
end;

procedure TTCPServer.CoreDestroy();
var
  //! счётчик
  I: Integer;
//------------------------------------------------------------------------------
begin
  // заглушка на выключение
  FError := True;
  // потоки
  FreeAndNil( FWorkerThread );
  FreeAndNil( FListenerThread );
  // сокеты обработки
  for I := Low( FRecievePool ) to High( FRecievePool ) do
  begin
    if Assigned( FRecievePool[I] ) then
    begin
      // останавливаем приём
      FRecievePool[I].FTCPSocket.CloseSocket();
      // ждём завершения
      while ( FRecievePool[I].FMode <> 0 ) do
      begin
        Windows.Sleep( 0 );
      end;
      // уничтожаем класс
      FreeAndNil( FRecievePool[I] );
    end;
  end;
  //
  SetLength( FRecievePool, 0 );
  // сокет приёма
  FreeAndNil( FListenerSocket );
end;

procedure TTCPServer.CheckError(
  Sender: TObject
);
begin
  if not FError then Exit;
  FErrorTimer.Enabled := False;
  try
    try
      GService.FLogger.ToLog( 'Ошибка TCP: зафиксирован факт общей ошибки; попытка удаления рабочей подсистемы' );
      CoreDestroy();
    except
      on Ex: Exception do
      begin
        GService.FLogger.ToLog( Format( 'Ошибка TCP: ошибка при удалении рабочей подсистемы:'#13#10'%s', [Ex.Message] ) );
        Exit;
      end;
    end;
    try
      GService.FLogger.ToLog( 'Ошибка TCP: зафиксирован факт общей ошибки; попытка создания рабочей подсистемы' );
      CoreCreate();
    except
      on Ex: Exception do
      begin
        GService.FLogger.ToLog( Format( 'Ошибка TCP: ошибка при создании рабочей подсистемы:'#13#10'%s', [Ex.Message] ) );
      end;
    end;
    GService.FLogger.ToLog( 'Ошибка TCP: подсистема сервера успешно перезапущена; статус - ОК' );
  finally
    FErrorTimer.Enabled := True;
  end;
end;

procedure TTCPServer.CoreListener();
var
  //! временная переменная
  Processor: TTCPProcessor;
  //!
  I: Integer;
//------------------------------------------------------------------------------
begin
  if FError then Exit;
  // проверим есть ли новое подключение
  if not FListenerSocket.CanRead( 1000 ) then Exit;
  FStopper.Acquire();
  try
    // новое подключение мы принимаем всегда
    // ID
    Inc( FChannelID );
//    if ( FChannelID = 0 ) then
//      FChannelID := 1;
    // создать класс
    Processor := TTCPProcessor.Create( FListenerSocket.Accept(), FChannelID, FReplaceFile );
    // добавить его в массив
    // проверяем есть ли дырка в массиве
    for I := Low( FRecievePool ) to High( FRecievePool ) do
    begin
      if not Assigned( FRecievePool[I] ) then
      begin
        // ... есть - сохраним в неё
        FRecievePool[I] := Processor;
        Exit;
      end;
    end;
    // ... нету - расширим массив
    SetLength( FRecievePool, Length( FRecievePool ) + 1 );
    FRecievePool[High( FRecievePool )] := Processor;
  finally
    FStopper.Release();
  end;
end;

procedure TTCPServer.CoreWorker();
var
  //! счётчик
  I: Integer;
//------------------------------------------------------------------------------
begin
  if FError then Exit;
  FStopper.Acquire();
  try
    for I := Low( FRecievePool ) to High( FRecievePool ) do
    begin
      if FError then Exit;
      // всё - только если у нас вообще есть объект работы
      if not Assigned( FRecievePool[I] ) then Continue;
      // если тайм-аут - сразу флаг
      if ( Now() - FRecievePool[I].FDTCreated > CCloseTime ) then
        FRecievePool[I].FMustClose := True;
      //
      if ( FRecievePool[I].FMode <> 0 ) then
      begin // сервис работает сейчас
        // если мы заклинились на приёме - закрыть
        if ( FRecievePool[I].FMode = 1 )
        and ( Now() - FRecievePool[I].FDTStartWork > CCloseTime ) then
        begin
          GService.FLogger.ToLog( 'Поток завис' );
          // зашэдулим по любому
          while not TryExecuteThis( CloseMe, nil, FRecievePool[I], GService.FLogger ) do ;
          //
          GService.FLogger.ToLog( 'Потоку послали AbortSocket' );
        end;
      end
      else
      begin // сервис не работает сейчас
        // если надо - закрыть соединение
        if FRecievePool[I].FMustClose then
        begin
          FreeAndNil( FRecievePool[I] ); // за'nil'ить - обязательно
          Continue;
        end;
        // если надо - работать (если есть с чем - CanRead)
        if ( FRecievePool[I].FTCPSocket.CanRead( 0 ) ) then
        begin
          FRecievePool[I].FDTStartWork := Now();
          FRecievePool[I].FMode := 1; // сбросим в конце ProcessMe
          //
          if not TryExecuteThis( ProcessMe, nil, FRecievePool[I], GService.FLogger ) then
          begin
            FRecievePool[I].FMustClose := True; // должно идти до FMode
            FRecievePool[I].FMode := 0; // сбросим флаг работы сами, т.к. мы не вызвали ProcessMe
          end;
        end;
      end;
    end;
  finally
    FStopper.Release();
  end;
  // ждём
  Windows.Sleep( 1 ); // один цикл
end;

//------------------------------------------------------------------------------
// TTCPProcessor
//------------------------------------------------------------------------------

constructor TTCPProcessor.Create(
  const ASocket: TSocket;
  const AID: Integer;
  const AReplaceFile: Boolean
);
begin
  inherited Create();
  //
  GetMem( FRBuffer, 8192 ); // !!! не менять
  //
  FID := AID;
  FReplaceFile := AReplaceFile;
  FDTCreated := Now();
  FDTSaved := FDTCreated;
  FDTStartWork := 54789; // *** 2050.01.01
  //
  FTCPSocket := TTCPBlockSocket.Create();
  FTCPSocket.Socket := ASocket;
end;

destructor TTCPProcessor.Destroy();
begin
  // флаг
  FCloseing := True;
  // закрыть сокет
  FTCPSocket.Free();
  // освободить ресурсы
  FreeMem( FRBuffer );
  //
  inherited Destroy();
end;

procedure TTCPProcessor.Analyze();
var
  //! остаток данных для приёма
  DataRemaining: Int64;
  //! временная для приёма
  InputData32: Integer;
  //! кол-во данных для приёма
  DataToRead: Integer;
  //! кол-во принятых данных
  DataReaded: Integer;
  //! папка файла
  OutputFileFolder: string;
  //! принятое имя файла
  RecFileName: string;
  //! рабочее имя файла (с цифровым суффиксом - если есть копия)
  WorkFileName: string;
  //! для посимвольного приёма имени
  OneChar: Char;
  //!
  I: Integer;
  //! байт подтверждения ответа
  OkByte: Byte;
  //! файл
  FileH: TFileHandler;
//------------------------------------------------------------------------------
begin
  OkByte := 6; // "ACK"
  try
    try
      // приём ID клиента
      if ( FTCPSocket.RecvBuffer( @InputData32, 4 ) <> 4 ) then
        raise Exception.CreateFmt( CProtocolError, ['ID клиента пришло не полностью'] );
      OutputFileFolder := GService.FWorkFolder + IntToStr( InputData32 ) + CPathDelim;
      // подтверждение ID клиента
      FTCPSocket.SendBuffer( @OkByte, 1 );
      // приём длины имени файла
      if ( FTCPSocket.RecvBuffer( @InputData32, 4 ) <> 4 ) then
        raise Exception.CreateFmt( CProtocolError, ['длина имени файла пришла не полностью'] );
      // приём имени файла
      SetLength( RecFileName, InputData32);
      for I := 1 to InputData32 do
      begin
        if ( FTCPSocket.RecvBuffer( @OneChar, 2 ) <> 2 ) then
          raise Exception.CreateFmt( CProtocolError, ['имя файла пришло не полностью'] );
        RecFileName[I] := OneChar;
      end;
      // подтверждение приёма имени файла
      FTCPSocket.SendBuffer( @OkByte, 1 );
      // подпапка
      ForceDirectories( OutputFileFolder );
      // тут мы делаем уникальное имя файла, чтобы не затирать уже принятые
      WorkFileName := RecFileName;
      I := 2;


      if FReplaceFile then
        DeleteFile(PWideChar(OutputFileFolder + WorkFileName));

      while FileExists( OutputFileFolder + WorkFileName ) do
      begin
        WorkFileName := Format( '%s (%d).%s', [QExcludeFileExt( RecFileName ), I, QExtractFileExt( RecFileName )] );
        Inc( I );
      end;
      // приём самого файла
      FileH := TFileHandler.Create( OutputFileFolder + WorkFileName, famWriteStrict );
      try
        FileH.Size := 0;
        // размер файла
        if ( FTCPSocket.RecvBuffer( @DataRemaining, 8 ) <> 8 ) then
          raise Exception.CreateFmt( CProtocolError, ['размер файла пришёл не полностью'] );
        // файл
        DataToRead := 8192;
        repeat
          if ( DataRemaining < 8192 ) then
            DataToRead := Integer( DataRemaining );
          if ( DataToRead = 0 ) then Break;
          DataReaded := FTCPSocket.RecvBuffer( FRBuffer, DataToRead );
          if ( DataReaded <> DataToRead ) then
          begin // очередной пакет принят частично
            if ( DataReaded = 0 ) then
              raise Exception.CreateFmt( CProtocolError, ['файл пришёл не полностью'] );
          end;
          DataRemaining := DataRemaining - Int64( DataReaded );
          FileH.ReadFromMem( FRBuffer, DataReaded );
        until ( DataRemaining = 0 );
      finally
        FileH.Free();
      end;
      // подтверждение приёма файла
      FTCPSocket.SendBuffer( @OkByte, 1 );
      // теперь можно смело закрыть соединение
      FMustClose := True;
    except
      on Ex: ESynapseError do
      begin
        FMustClose := True;
        if ( Ex.ErrorCode <> 10054 ) then
          GService.FLogger.ErrorToLog( 'SynapseError №' + IntToStr( Ex.ErrorCode ) + ':'#13#10 + Ex.ErrorMessage );
      end;
      on Ex: Exception do
      begin
        FMustClose := True;
        GService.FLogger.ErrorToLog( 'Ошибка приёма файлов:'#13#10 + Ex.Message );
      end;
    end;
  finally
    FMode := 0; // мы должны проставить флаг, иначе класс не сможет быть уничтожен
  end;
end;

//------------------------------------------------------------------------------
// TILS_ServNovacom
//------------------------------------------------------------------------------

procedure ServiceController(CtrlCode: DWord); stdcall;
begin
  GService.Controller(CtrlCode);
end;

function TILS_CommonReceiver.GetServiceController: TServiceController;
begin
  Result := ServiceController;
end;

procedure TILS_CommonReceiver.ServiceStart(
  Sender: TService;
  var Started: Boolean
);
var
  //! файл настроек
  INIFile: TIniFile;
//------------------------------------------------------------------------------
begin
{$IFDEF DEBUG}
  Windows.Sleep( 10000 );
{$ENDIF}
  Started := False;
  // логгер
  FLogger := TLogger.Create( ParamStr( 0 ), 5 * 1024 * 1024, 5 );
  FLogger.ToLog( '============ СЛУЖБА ПРИЁМА ФАЙЛОВ ============' );
  FLogger.ToLog( ParamStr( 0 ) );
  FLogger.ToLog( '=== запускается ===' );
  // загрузка параметров
  try
    INIFile := TIniFile.Create( ChangeFileExt( ParamStr( 0 ), CIniDefExtDotted ) );
  except
    on Ex: Exception do
    begin
      FLogger.ErrorToLog( 'Ошибка чтения параметров из INI-файла:'#13#10 + Ex.Message );
      Exit;
    end;
  end;
  try
    FWorkFolder := IncludeLastDelimiter( INIFile.ReadString( 'Main Params', 'WorkFolder', '' ), CPathDelim );
    FReplaceFile := INIFile.ReadBool( 'Main Params', 'ReplaceFile', False );
    FLogger.ToLog( Format( 'Корневая папка обработки данных => "%s"', [FWorkFolder] ) );
    FPortNumber := INIFile.ReadInteger( 'Main Params', 'Port', 0 );
    FLogger.ToLog( Format( 'Порт прослушивания => "%d"', [FPortNumber] ) );
    FLogger.ToLog( Format( 'Перезаписывать входящие файлы => "%s"', [IfThen(FReplaceFile, 'да', 'нет')] ) );
  except
    on Ex: Exception do
    begin
      FLogger.ErrorToLog( 'Ошибка чтения параметров из INI-файла:'#13#10 + Ex.Message );
      INIFile.Free();
      Exit;
    end;
  end;
  INIFile.Free();
  // создаём сервер
  FTCPServer := TTCPServer.Create( FPortNumber , FReplaceFile);
  // всё готово
  FLogger.ToLog( '============ запущена ============' );
  Started := True;
end;

procedure TILS_CommonReceiver.ServiceStop(
  Sender: TService;
  var Stopped: Boolean
);
begin
  Stopped := True;
  FLogger.ToLog( '=== останавливается ===' );
  FTCPServer.Free();
  FinalizeTMM(); //*** необходимость
  FLogger.ToLog( '============ остановлена ============' );
  FLogger.Free();
end;

//------------------------------------------------------------------------------
initialization
  IsMultiThread := True;
  SetMinimumBlockAlignment( mba16Byte );

end.

