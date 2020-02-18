unit E_Threads;
//------------------------------------------------------------------------------
// Модуль реализует классы потоков
//------------------------------------------------------------------------------
//
// Поток непрерывной работы - TThreadInfinite
//  - вызывает заданную процедуру непрерывно
// Поток запускаемой работы - TThreadActivated
//  - вызывает заданную процедуру по требованию (метод Work)
//
// Есть 2 процедуры, вызываемые при работе потока:
//  - TThCallbackFunc - собственно процедура работы, вызывается при каждом цикле
//  - TThErrorFunc - процедура ошибки, вызывается при возникновении исключительной ситуации (может быть опущена)
//  ! обе процедуры вызываются в контексте потока !
//
// !ВАЖНЫЕ ЗАМЕЧАНИЯ!
//  1) классы потоков работают не так, как наследник TThread;
//  они не имеют процедуры Terminate(), поэтому вручную завершить выполнение потока
//  можно только уничтожив экземпляр класса, т.е. вызвав Free();
//  ОДНАКО при возникновении исключительной ситуации поток будет автоматически завершён,
//  причём это произойдёт после вызова процедуры TThErrorFunc (если она назначена);
//  - ! уничтожать экземпляр класса потока всё равно надо вручную (впрочем, не обязательно делать это немедленно)
//
//  2) потоки не могут быть приостановлены (Suspended)
//
//  3) все вызываемые процедуры TThreadActivated имеют параметр AData;
//  его значение каждый раз передаётся при вызове Work, т.е. при запросе на отработку
//  этот параметр может быть равен nil;
//------------------------------------------------------------------------------

//------------------------------------------------------------------------------
interface

uses
  SysUtils, Classes, RTLConsts, Windows;

//------------------------------------------------------------------------------
type

//------------------------------------------------------------------------------
//! рабочая процедура потока
//------------------------------------------------------------------------------
  TThCallback = procedure(
    //! ссылка на данные
    const AData: Pointer
  );

//------------------------------------------------------------------------------
//! ошибка потока
//------------------------------------------------------------------------------
  TThError = procedure(
    //! ссылка на данные
    const AData: Pointer;
    //! ссылка на класс исключения
    var AException: Exception;
    //! windows ID потока
    const AThreadID: TThreadID
  );

//------------------------------------------------------------------------------
//! внутренний класс потока непрерывной работы (!не предназначен для использования напрямую!)
//------------------------------------------------------------------------------
  TInternalInfiniteThread = class sealed( TThread )
  strict private
    //! ссылка на процедуру обратного вызова
    FCallbackFunc: TThCallback;
    //! ссылка на процедуру ошибки
    FErrorFunc: TThError;
    //! ссылка на данные
    FData: Pointer;
  protected
    //!
    procedure Execute(); override;
  public
    //!
    constructor Create(
      const ACallbackFunc: TThCallback;
      const AErrorFunc: TThError;
      const AData: Pointer
    );
    //!
    destructor Destroy(); override;
  end;

//------------------------------------------------------------------------------
//! внутренний класс потока запускаемой работы (!не предназначен для использования напрямую!)
//------------------------------------------------------------------------------
  TInternalActivatedThread = class sealed( TThread )
  strict private
    //! ссылка на процедуру обратного вызова
    FCallbackFunc: TThCallback;
    //! ссылка на процедуру ошибки
    FErrorFunc: TThError;
    //! ссылка на данные
    FData: Pointer;
    //! событие прокрутки цикла
    FCicleEvent: THandle;
  protected
    //!
    procedure Execute(); override;
  public
    //! ссылка на данные (для записи)
    property DataRef: Pointer write FData;
    //!
    constructor Create(
      const ACallbackFunc: TThCallback;
      const AErrorFunc: TThError
    );
    //!
    destructor Destroy(); override;
    //! выполнить один цикл
    procedure Process();
  end;

//------------------------------------------------------------------------------
//! класс потока непрерывной работы
//------------------------------------------------------------------------------
  TThreadInfinite = class sealed
  strict private
    //!
    FThread: TInternalInfiniteThread;
  public
    //!
    constructor Create(
      const ACallbackFunc: TThCallback;
      const AErrorFunc: TThError;
      const AData: Pointer
    );
    //!
    destructor Destroy(); override;
    //!
    function IsTerminated(): Boolean;
  end;

//------------------------------------------------------------------------------
//! класс потока запускаемой работы
//------------------------------------------------------------------------------
  TThreadActivated = class sealed
  strict private
    //!
    FThread: TInternalActivatedThread;
  public
    //!
    constructor Create(
      const ACallbackFunc: TThCallback;
      const AErrorFunc: TThError
    );
    //!
    destructor Destroy(); override;
    //!
    procedure Work(
      const AData: Pointer
    );
    //!
    function IsTerminated(): Boolean;
  end;

//------------------------------------------------------------------------------
implementation

const

//------------------------------------------------------------------------------
//! сообщения
//------------------------------------------------------------------------------
  cTNoWorkProc: string = 'Не назначена рабочая процедура потока';

//------------------------------------------------------------------------------
// TInternalInfiniteThread
//------------------------------------------------------------------------------

constructor TInternalInfiniteThread.Create(
  const ACallbackFunc: TThCallback;
  const AErrorFunc: TThError;
  const AData: Pointer
);
begin
  inherited Create( True );
  //
  if not Assigned( ACallbackFunc )
  then raise EThread.CreateResFmt( @SThreadCreateError, [cTNoWorkProc] );
  FreeOnTerminate := False;
  FCallbackFunc := ACallbackFunc;
  FErrorFunc := AErrorFunc;
  FData := AData;
end;

destructor TInternalInfiniteThread.Destroy();
begin
  inherited Destroy(); // Terminate и WaitFor внутри
end;

procedure TInternalInfiniteThread.Execute();
begin
  while not Terminated
  do begin
    Windows.Sleep( 0 ); // не забываем давать работать другим
    try
      FCallbackFunc( FData );
    except
      on Ex: Exception
      do begin
        if Assigned( FErrorFunc )
        then FErrorFunc( FData, Ex, ThreadID );
        Terminate();
      end;
    end;
  end;
end;

//------------------------------------------------------------------------------
// TInternalActivatedThread
//------------------------------------------------------------------------------

constructor TInternalActivatedThread.Create(
  const ACallbackFunc: TThCallback;
  const AErrorFunc: TThError
);
begin
  inherited Create( True );
  //
  if not Assigned( ACallbackFunc )
  then raise EThread.CreateResFmt( @SThreadCreateError, [cTNoWorkProc] );
  FCicleEvent := CreateEvent( nil, True, False, nil );
  if ( FCicleEvent = 0 )
  then raise EThread.CreateResFmt( @SThreadCreateError, [SysErrorMessage( GetLastError )] );
  FreeOnTerminate := False;
  FCallbackFunc := ACallbackFunc;
  FErrorFunc := AErrorFunc;
end;

destructor TInternalActivatedThread.Destroy();
begin
  Terminate(); // ставим флаг завершения
  SetEvent( FCicleEvent ); // запускаем с точки ожидания (если поток не в работе)
  WaitFor(); // ожидаем завершения потока
  CloseHandle( FCicleEvent ); // освобождаем ресурсы
  //
  inherited Destroy();
end;

procedure TInternalActivatedThread.Process();
begin
  SetEvent( FCicleEvent );
end;

procedure TInternalActivatedThread.Execute();
begin
  repeat
    if Terminated
    then Break;
    WaitForSingleObject( FCicleEvent, INFINITE ); // в этой точке мы ждём
    if Terminated
    then Break;
    ResetEvent( FCicleEvent );
    try
      FCallbackFunc( FData );
    except
      on Ex: Exception
      do begin
        if Assigned( FErrorFunc )
        then FErrorFunc( FData, Ex, ThreadID );
        Terminate();
      end;
    end;
  until False;
end;

//------------------------------------------------------------------------------
// TThreadInfinite
//------------------------------------------------------------------------------

constructor TThreadInfinite.Create(
  const ACallbackFunc: TThCallback;
  const AErrorFunc: TThError;
  const AData: Pointer
);
begin
  inherited Create();
  //
  FThread := TInternalInfiniteThread.Create( ACallbackFunc, AErrorFunc, AData );
  FThread.Start(); // автозапуск
end;

destructor TThreadInfinite.Destroy();
begin
  FThread.Free();
  //
  inherited Destroy();
end;

function TThreadInfinite.IsTerminated(): Boolean;
begin
  Result := FThread.Terminated;
end;

//------------------------------------------------------------------------------
// TThreadActivated
//------------------------------------------------------------------------------

constructor TThreadActivated.Create(
  const ACallbackFunc: TThCallback;
  const AErrorFunc: TThError
);
begin
  inherited Create();
  //
  FThread := TInternalActivatedThread.Create( ACallbackFunc, AErrorFunc );
  FThread.Start();
end;

destructor TThreadActivated.Destroy();
begin
  FThread.Free();
  //
  inherited Destroy();
end;

procedure TThreadActivated.Work(
  const AData: Pointer
);
begin
  if FThread.Terminated
  then Exit;
  FThread.DataRef := AData;
  FThread.Process();
end;

function TThreadActivated.IsTerminated(): Boolean;
begin
  Result := FThread.Terminated;
end;

end.

