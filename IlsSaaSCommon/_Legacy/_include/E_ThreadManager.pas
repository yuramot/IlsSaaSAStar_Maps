unit E_ThreadManager;
//------------------------------------------------------------------------------
// Модуль реализует класс менеджера потоков
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------

//------------------------------------------------------------------------------
interface

uses
  SysUtils, SyncObjs,
  ExtCtrls, // TTimer
  E_Threads, E_Logger;

//------------------------------------------------------------------------------
//! запрос выполнения задачи
//------------------------------------------------------------------------------
function TryExecuteThis(
  const AProc: TThCallback;
  const AError: TThError;
  const AData: Pointer;
  const ALogger: TLogger
): Boolean;

//------------------------------------------------------------------------------
//! завершение модуля
//! должно вызываться из внешего источника !
//------------------------------------------------------------------------------
procedure FinalizeTMM();

//------------------------------------------------------------------------------
implementation

type

//------------------------------------------------------------------------------
//! информация о потоке
//------------------------------------------------------------------------------
  PMyThreadRec = ^TMyThreadRec;

  TMyThreadRec = record
    //! время последнего выполнения работы потоком
    LastTime: TDateTime;
    //! ссылка на логгер
    LogRef: TLogger;
    //! ссылка на класс потока
    Th: TThreadActivated;
    //! что выполняем
    CBProc: TThCallback;
    //! с чем выполняем
    CBData: Pointer;
    //! что выполняем если ошибка
    CBError: TThError;
    //! выполняет ли поток полезную работу
    InWork: Boolean;
    //! возникла ли ошибка выполнения
    WasError: Boolean;
  end;

  TMyThreadRecArray = array of TMyThreadRec;

//------------------------------------------------------------------------------
//! фиктивный класс для процедуры таймера очистки холостых потоков
//------------------------------------------------------------------------------
  TTMMTimerDummy = class
  public
    class procedure IdleTimerTick(
      Sender: TObject
    );
  end;

//------------------------------------------------------------------------------
const

//------------------------------------------------------------------------------
//! максимальное количество создаваемых потоков
//------------------------------------------------------------------------------
  cTMMaxThreads = 1500;

//------------------------------------------------------------------------------
//! интервал очистки холостых потоков
//------------------------------------------------------------------------------
  cTMCleanUpInterval = 30000; // 30 секунд

//------------------------------------------------------------------------------
//! сообщения
//------------------------------------------------------------------------------
  cTMCreateError: string = 'Не удалось создать новый поток:'#13#10'%s';
  cTMLimitError: string = 'Достигнут лимит потоков, создание нового невозможно. (Max=%d)';
//  cTMProcessError: string = 'Зафиксирована следующая ошибка во время выполнения потока:'#13#10'%s';

//------------------------------------------------------------------------------
var

//------------------------------------------------------------------------------
//! список потоков
//------------------------------------------------------------------------------
  GTMMThreadsPool: TMyThreadRecArray;

//------------------------------------------------------------------------------
//! блокировщик работы со списком
//------------------------------------------------------------------------------
  GTMMCritical: TCriticalSection;

//------------------------------------------------------------------------------
//! таймер очистки холостых потоков
//------------------------------------------------------------------------------
  GTMMIdleTimer: TTimer;

//------------------------------------------------------------------------------
//! флаг остановки модуля
//------------------------------------------------------------------------------
  GTMMEnded: Boolean; // = False

//------------------------------------------------------------------------------
//! рабочая процедура потока
//! выполняется в отдельном потоке
//------------------------------------------------------------------------------
procedure ThWork(
  const AData: Pointer
);
var
  //! типизированный AData
  LThis: PMyThreadRec;
//------------------------------------------------------------------------------
begin
  LThis := AData;
  LThis^.CBProc( LThis^.CBData );
  LThis^.LastTime := Now();
  LThis^.InWork := False;
end;

//------------------------------------------------------------------------------
//! процедура ошибки потока
//! выполняется в отдельном потоке
//------------------------------------------------------------------------------
procedure ThError(
  const AData: Pointer;
  var AException: Exception;
  const AThreadID: TThreadID
);
var
  //! типизированный AData
  LThis: PMyThreadRec;
//------------------------------------------------------------------------------
begin
  LThis := AData;
//  LThis^.LogRef.ErrorToLog( Format( cTMProcessError, [AException.Message] ) );
  LThis^.CBError( LThis^.CBData, AException, AThreadID );
  LThis^.WasError := True;
end;

//------------------------------------------------------------------------------
//! поиск свободного потока в списке
//------------------------------------------------------------------------------
function FindIdleThread(): Integer;
begin
  for Result := Low( GTMMThreadsPool ) to High( GTMMThreadsPool )
  do begin
    if Assigned( GTMMThreadsPool[Result].Th ) and not GTMMThreadsPool[Result].InWork
    then Exit;
  end;
  Result := -1;
end;

//------------------------------------------------------------------------------
//! поиск пустого места в списке для нового потока
//------------------------------------------------------------------------------
function FindEmptySlot(): Integer;
begin
  for Result := Low( GTMMThreadsPool ) to High( GTMMThreadsPool )
  do begin
    if not Assigned( GTMMThreadsPool[Result].Th )
    then Exit;
  end;
  Result := -1;
end;

//------------------------------------------------------------------------------
//! запрос выполнения задачи
//------------------------------------------------------------------------------
function TryExecuteThis(
  const AProc: TThCallback;
  const AError: TThError;
  const AData: Pointer;
  const ALogger: TLogger
): Boolean;
label A;
var
  //!
  LRef: PMyThreadRec;
  //!
  LIndex: Integer;
//------------------------------------------------------------------------------
begin
  Result := False;
  if GTMMEnded
  then Exit;
  // работаем
  GTMMCritical.Acquire();
  try
    // ищем простаивающий поток
    LIndex := FindIdleThread();
    if ( LIndex <> -1 )
    then begin // нашли простаивающий поток
      LRef := @GTMMThreadsPool[LIndex]; // простая ссылка
A:
      LRef^.WasError := False; // нет ошибки
      LRef^.InWork := True; // в работе
      LRef^.CBProc := AProc; // что
      LRef^.CBData := AData; // с чем
      LRef^.CBError := AError; // если всё плохо
      LRef^.LogRef := ALogger; // лог
      LRef^.Th.Work( LRef ); // вызов выполнения
    end
    else begin // не нашли простаивающий поток
      // ищем место для нового потока
      LIndex := FindEmptySlot();
      if ( LIndex <> -1 )
      then begin // нашли место для нового потока
        LRef := @GTMMThreadsPool[LIndex]; // простая ссылка
        // создаём новый поток
        try
          LRef^.Th := TThreadActivated.Create( ThWork, ThError );
        except
          on Ex: Exception
          do begin
            ALogger.ErrorToLog( Format( cTMCreateError, [Ex.Message] ) );
            Exit;
          end;
        end;
        // всё остальное
        goto A;
      end
      else begin // не нашли место для нового потока
        ALogger.ErrorToLog( Format( cTMLimitError, [cTMMaxThreads] ) );
        Exit;
      end;
    end;
  finally
    GTMMCritical.Release();
  end;
  Result := True;
end;

//------------------------------------------------------------------------------
//! процедура таймера очистки холостых потоков
//------------------------------------------------------------------------------
class procedure TTMMTimerDummy.IdleTimerTick(
  Sender: TObject
);
var
  //!
  LI: Integer;
//------------------------------------------------------------------------------
begin
  if GTMMEnded
  then Exit;
  GTMMCritical.Acquire();
  try
    for LI := Low( GTMMThreadsPool ) to High( GTMMThreadsPool )
    do begin
      // очищаем остановившиеся при ошибках потоки
      if Assigned( GTMMThreadsPool[LI].Th )
      and GTMMThreadsPool[LI].Th.IsTerminated()
      then FreeAndNil( GTMMThreadsPool[LI].Th );
      // завершаем потоки с возникшей внешней ошибкой
      if Assigned( GTMMThreadsPool[LI].Th )
      and GTMMThreadsPool[LI].WasError
      then FreeAndNil( GTMMThreadsPool[LI].Th );
      // завершаем лишние не занятые работой потоки
      if Assigned( GTMMThreadsPool[LI].Th )
      and ( not GTMMThreadsPool[LI].InWork )
      and ( Now() - GTMMThreadsPool[LI].LastTime > ( cTMCleanUpInterval / 86400000 ) ) // миллисекунд в сутках
      then FreeAndNil( GTMMThreadsPool[LI].Th );
    end;
  finally
    GTMMCritical.Release();
  end;
end;

//------------------------------------------------------------------------------
//! начальная инициализация модуля
//------------------------------------------------------------------------------
procedure InitTMM();
begin
  IsMultiThread := True;
  GTMMCritical := TCriticalSection.Create();
  SetLength( GTMMThreadsPool, cTMMaxThreads );
  GTMMIdleTimer := TTimer.Create( nil );
  GTMMIdleTimer.Interval := cTMCleanUpInterval;
  GTMMIdleTimer.Enabled := True;
  GTMMIdleTimer.OnTimer := TTMMTimerDummy.IdleTimerTick;
end;

//------------------------------------------------------------------------------
//! завершение модуля
//! должно вызываться из внешего источника !!!
//------------------------------------------------------------------------------
procedure FinalizeTMM();
var
  //!
  LI: Integer;
//------------------------------------------------------------------------------
begin
  if GTMMEnded
  then Exit;
  GTMMEnded := True;
  FreeAndNil( GTMMIdleTimer );
  for LI := Low( GTMMThreadsPool ) to High( GTMMThreadsPool )
  do FreeAndNil( GTMMThreadsPool[LI].Th );
  SetLength( GTMMThreadsPool, 0 );
  FreeAndNil( GTMMCritical );
end;

//------------------------------------------------------------------------------
initialization
  InitTMM();

//------------------------------------------------------------------------------
finalization
  FinalizeTMM();

end.

