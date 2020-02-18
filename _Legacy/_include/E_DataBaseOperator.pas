unit E_DataBaseOperator;
//------------------------------------------------------------------------------

//------------------------------------------------------------------------------
interface

uses
  MidasLib{static link for midas.lib},
  SysUtils, Variants, Classes, DB, ADODB, Provider, DBClient,
  E_Logger;

//------------------------------------------------------------------------------
//------------------------------------------------------------------------------
//------------ record TConnectionParams ----------------------------------------
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------
//! Запись с параметрами соединения с БД
(*!
  Содержит строку подключения к БД и таймаут
*)
//------------------------------------------------------------------------------
{type
  TConnectionParams = record
    //! Строка соединения с БД
    ConnectionString: string[255];
    //! Таймаут подключения к БД
    TimeOut: Integer;
  end;}

//------------------------------------------------------------------------------
//------------------------------------------------------------------------------
//------------ class TDataBaseOperator -----------------------------------------
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------
//! Класс для работы с БД
(*!
  Осуществляет подключение к базе данных, выполнение запросов,
  при необходимости возвращает набор данных (DataSet)
*)
type
  TDataBaseOperator = class sealed
  private
    //! Объект для выполнения запросов к БД, возвращающий датасет
    FADOQuery: TADOQuery;
    //! Объект для выполнения команд к БД, не возвращающий датасет
    FADOCommand: TADOCommand;
    //! Признак удержания соединения (если True, то используется постоянное соединение с БД)
    FKeepConnection: Boolean;
    //! Объект для соединения с БД
    FADOConnection: TADOConnection;
    //! Ссылка на логгер
    FLogger: TLogger;
    //! Функция подключения к БД
    {!
      @return
      Вернет True при удачном подключении
    }
    function Connect(): Boolean;
    //! Процедура отключения от БД
    procedure Disconnect();
  public
    //! Функция начала транзакции
    {!
      @return
      Вернет True в случае успешного выполнения; иначе - False
    }
    function BeginTransaction(): Boolean;
    //! Функция завершения транзакции
    {!
      @return
      Вернет True в случае успешного выполнения; иначе - False
    }
    function CommitTransaction(): Boolean;
    //! Функция отката транзакции
    {!
      @return
      Вернет True в случае успешного выполнения; иначе - False
    }
    function RollbackTransaction(): Boolean;
    //! Функция выполнения команд в БД (например, запросы на UPDATE и INSERT)
    {!
      @param in
      ASQLText - Текст SQL-запроса
      @return
      Вернет True в случае успешного выполнения; иначе - False
    }
    function CommandExecute(
      const ASQLText: string
    ): Boolean; overload;
    //! Функция выполнения запросов в БД, возвращающих наборы данных (например, запросы на SELECT)
    {!
      @param in
      ASQLText - Текст SQL-запроса
      @param  out
      RDataSet - Возвращаемый набор данных
      @return
      Вернет количество записей в случае успешного выполнения; либо -1 в случае ошибки
    }
    function FillDataSet(
      const ASQLText: string;
      var RDataSet: TClientDataSet
    ): Integer;
    //! Функция получения значения из запроса
    //! Рекомендуется использовать при запросах, возвращающих только 1 запись, вместо GetDataSet
    {!
      @param in
      ASQLText - текст запроса
      @param in out
      RValue - значение пераого поля первой записи
      @return
      Вернет кол-во записей в запросе; либо -1 в случае ошибки
    }
    function GetValue(
      const ASQLText: string;
      var RValue: Variant
    ): Integer;
    //! Функция записи потока в blob-поле
    {!
      @param in
      ATableName - имя таблицы в базе данных
      @param in
      AIDFieldName - имя поля идентификатора
      @param in
      ABlobFieldName - имя блоб-поля для записи
      @param in
      AID - Идентификатор записи, для которой осуществлять запись
      @param in
      AStream - поток для записи в блоб-поле
      @return
      Вернет количество записанных байт; либо -1 в случае ошибки
    }
    function WriteStreamToBlobField(
      const ATableName: string;
      const AIDFieldName: string;
      const ABlobFieldName: string;
      const AID: Integer;
      const AStream: TMemoryStream
    ): Integer; overload;
    function WriteStreamToBlobField(
      const ATableName: string;
      const AIDFieldName: string;
      const ABlobFieldName: string;
      const AID: string;
      const AStream: TMemoryStream
    ): Integer; overload;
    //! Функция чтения потока из blob-поля
    {!
      @param in
      ATableName - имя таблицы в базе данных
      @param in
      AIDFieldName - имя поля идентификатора
      @param in
      ABlobFieldName - имя блоб-поля для чтения
      @param in
      AID - Идентификатор записи, из которой осуществлять чтение
      @param in out
      RStream - поток для чтения из блоб-поля
      @return
      Вернет количество прочитанных байт; либо -1 в случае ошибки
    }
    function ReadStreamFromBlobField(
      const ATableName: string;
      const AIDFieldName: string;
      const ABlobFieldName: string;
      const AID: Integer;
      var RStream: TMemoryStream
    ): Integer; overload;
    function ReadStreamFromBlobField(
      const ATableName: string;
      const AIDFieldName: string;
      const ABlobFieldName: string;
      const AID: string;
      var RStream: TMemoryStream
    ): Integer; overload;
    //! Функция проверки соединения с БД
    {!
       @param in
       ATimeout - Таймаут для соединения с БД (0 - отключить таймаут)
       @return
       При удачном выполнении верент True; иначе - False
    }
    function TestDataBaseConnection(
      const ATimeout: Integer
    ): Boolean;
    //! Конструктор
    {!
      @param in
      AConnectionParams - параметры подключения к БД
      @param in
      ALogger - логер (передается по ссылке)
      @param in
      AKeepConnection - Признак удержания соединения
    }
    constructor Create(
      const AConnectionString: string;
      const ATimeOut: Integer;
      const ALogger: TLogger;
      const AKeepConnection: Boolean
    );
    //! Деструктор
    destructor Destroy(); override;
  end;

//------------------------------------------------------------------------------
implementation

//------------------------------------------------------------------------------
// Реализация конструктора
//------------------------------------------------------------------------------
constructor TDataBaseOperator.Create(
  const AConnectionString: string;
  const ATimeOut: Integer;
  const ALogger: TLogger;
  const AKeepConnection: Boolean
);
begin
  // инициализируем объект подключения к БД
  FADOConnection := TADOConnection.Create( nil );
  FADOConnection.ConnectionString := AConnectionString;
  FADOConnection.ConnectionTimeout := ATimeOut;
  FADOConnection.CommandTimeout := ATimeOut;
  FADOConnection.LoginPrompt := False;
  FADOConnection.Attributes := [];
  // инициализируем объект для запросов
  FADOQuery := TADOQuery.Create( nil );
  FADOQuery.Connection := FADOConnection;
  FADOQuery.CommandTimeout := ATimeOut;
  // для ускорения ставим только один проход
  FADOQuery.CursorType := ctStatic;
  FADOQuery.CursorLocation := clUseClient;
  // инициализируем объект для команд
  FADOCommand := TADOCommand.Create( nil );
  FADOCommand.Connection := FADOConnection;
  FADOCommand.CommandTimeout := ATimeOut;
  FADOCommand.ExecuteOptions := [eoExecuteNoRecords];
  // остальное
  FLogger := ALogger;
  FKeepConnection := AKeepConnection;
  if FKeepConnection
  then Connect();
end;

//------------------------------------------------------------------------------
// Реализация деструктора
//------------------------------------------------------------------------------
destructor TDataBaseOperator.Destroy();
begin
  if FKeepConnection
  then Disconnect();
  FADOCommand.Free();
  FADOQuery.Free();
  FADOConnection.Free();
  inherited Destroy();
end;

//------------------------------------------------------------------------------
// Реализация подключения к БД
//------------------------------------------------------------------------------
function TDataBaseOperator.Connect(): Boolean;
begin
  Result := True;
  try
    Disconnect();
    FADOConnection.Connected := True;
  except
    on Ex: Exception
    do begin
      Result := False;
      if Assigned( FLogger )
      then FLogger.ErrorToLog( 'Ошибка соединения с БД:'#13#10 + Ex.Message );
    end;
  end;
end;

//------------------------------------------------------------------------------
// Реализация сброса подключения к БД
//------------------------------------------------------------------------------
procedure TDataBaseOperator.Disconnect();
begin
  FADOConnection.Connected := False;
end;

//------------------------------------------------------------------------------
// Реализация выполнения команд в БД
//------------------------------------------------------------------------------
function TDataBaseOperator.CommandExecute(
  const ASQLText: string
): Boolean;
var
  //! Признак наличия запущенной транзакции
  LTransctionInProcess: Boolean;
//------------------------------------------------------------------------------
begin
  Result := False;
  if not FKeepConnection
  then begin
    if not Connect()
    then Exit;
  end;
  LTransctionInProcess := FADOConnection.InTransaction;
  if not LTransctionInProcess
  then begin // если нет запущенной транзакции, то начинаем ее
    if not BeginTransaction()
    then Exit;
  end;
  FADOCommand.CommandText := ASQLText;
  try
    FADOCommand.Execute();
    Result := True;
  except
    on Ex: Exception
    do begin
      if Assigned( FLogger )
      then FLogger.ErrorToLog(
        'Ошибка выполнения команды:'
          + #13#10 + Ex.Message
          + #13#10'^ Текст команды:'
          + #13#10 + ASQLText
      );
    end;
  end;
  if not LTransctionInProcess
  then begin // если до начала транзакция не была запущена, то завершаем или откатываем текущую
    if Result
    then CommitTransaction()
    else RollBackTransaction();
  end;
  if not FKeepConnection
  then Disconnect();
end;

//------------------------------------------------------------------------------
// Реализация запросов в БД
//------------------------------------------------------------------------------
function TDataBaseOperator.FillDataSet(
  const ASQLText: string;
  var RDataSet: TClientDataSet
): Integer;
var
  //! Провайдер для работы с DataSet
  LDataSetProvider: TDatasetProvider;
  //! Флаг ошибки
  LErr: Boolean;
//------------------------------------------------------------------------------
begin
  Result := -1;
  if not FKeepConnection
  then begin
    if not Connect()
    then Exit;
  end;
  LDataSetProvider := TDataSetProvider.Create( nil );
  try
    FADOQuery.SQL.Text := ASQLText;
    LErr := False;
    try
      FADOQuery.Open();
    except
      on Ex: Exception
      do begin
        LErr := True;
        if Assigned( FLogger )
        then FLogger.ErrorToLog(
          'Ошибка выполнения запроса:'
            + #13#10 + Ex.Message
            + #13#10'^ Текст запроса:'
            + #13#10 + ASQLText
        );
      end;
    end;
    if not LErr
    then begin
      FADOQuery.DisableControls();
      try
        if FADOQuery.Active
        then FADOQuery.First();
        LDataSetProvider.DataSet := FADOQuery;
        RDataSet.Data := LDataSetProvider.Data;
        Result := RDataSet.RecordCount;
      except
        on Ex: Exception
        do begin
          if Assigned( FLogger )
          then FLogger.ErrorToLog(
            'Ошибка выполнения запроса:'
              + #13#10'Ошибка формирования набора данных:'
              + #13#10 + Ex.Message
              + #13#10'^ Текст запроса:'
              + #13#10 + ASQLText
          );
        end;
      end;
      FADOQuery.EnableControls();
      FADOQuery.Close();
    end;
    FADOQuery.SQL.Text := '';
    if not FKeepConnection
    then Disconnect();
  finally
    LDataSetProvider.Free();
  end;
end;

//------------------------------------------------------------------------------
// Реализация получения значения из запроса
//------------------------------------------------------------------------------
function TDataBaseOperator.GetValue(
  const ASQLText: string;
  var RValue: Variant
): Integer;
var
  //! Получаем данные сюда
  LDataSet: TClientDataSet;
//------------------------------------------------------------------------------
begin
  RValue := Null;
  LDataSet := TClientDataSet.Create( nil );
  try
    Result := FillDataSet( ASQLText, LDataSet );
    if ( Result < 0 )
    then Exit;
    try
      LDataSet.First();
      RValue := LDataSet.Fields[0].Value;
      LDataSet.Close();
    except
      on Ex: Exception
      do begin
        Result := -1;
        if Assigned( FLogger )
        then FLogger.ErrorToLog(
          'Ошибка получения значения:'
            + #13#10 + Ex.Message
            + #13#10'^ Текст запроса:'
            + #13#10 + ASQLText
        );
      end;
    end;
  finally
    LDataSet.Free();
  end;
end;

//------------------------------------------------------------------------------
// Реализация записи потока в блоб-поле
//------------------------------------------------------------------------------
function TDataBaseOperator.WriteStreamToBlobField(
  const ATableName: string;
  const AIDFieldName: string;
  const ABlobFieldName: string;
  const AID: Integer;
  const AStream: TMemoryStream
): Integer;
begin
  Result := WriteStreamToBlobField( ATableName, AIDFieldName, ABlobFieldName, IntToStr( AID ), AStream );
end;

//------------------------------------------------------------------------------
// Реализация записи потока в блоб-поле
//------------------------------------------------------------------------------
function TDataBaseOperator.WriteStreamToBlobField(
  const ATableName: string;
  const AIDFieldName: string;
  const ABlobFieldName: string;
  const AID: string;
  const AStream: TMemoryStream
): Integer;
var
  //! Текст запроса
  LSQLText: string;
  //! Признак наличия запущенной транзакции
  LTransctionInProcess: Boolean;
//------------------------------------------------------------------------------
begin
  Result := -1;
  if not FKeepConnection
  then begin
    if not Connect()
    then Exit;
  end;
  LTransctionInProcess := FADOConnection.InTransaction;
  if not LTransctionInProcess
  then begin // если нет запущенной транзакции, то начинаем ее
    if not BeginTransaction()
    then Exit;
  end;
  LSQLText := 'UPDATE ' + ATableName
            + ' SET ' + ABlobFieldName + ' = :blobdata'
            + ' WHERE ' + AIDFieldName + ' = ''' + AID + '''';
  FADOCommand.CommandText := LSQLText;
  try
    FADOCommand.Parameters.ParamByName( 'blobdata' ).LoadFromStream( AStream, ftBlob );
    FADOCommand.Execute();
    Result := AStream.Size;
  except
    on Ex: Exception
    do begin
      if Assigned( FLogger )
      then FLogger.ErrorToLog(
        'Ошибка записи потока в blob-поле:'
          + #13#10 + Ex.Message
          + #13#10'^ Текст запроса:'
          + #13#10 + LSQLText
      );
    end;
  end;
  if not LTransctionInProcess
  then begin // если до начала транзакция не была запущена, то завершаем или откатываем текущую
    if ( Result >= 0 )
    then CommitTransaction()
    else RollBackTransaction();
  end;
  if not FKeepConnection
  then Disconnect();
end;

//------------------------------------------------------------------------------
// Реализация чтения потока из блоб-поля
//------------------------------------------------------------------------------
function TDataBaseOperator.ReadStreamFromBlobField(
  const ATableName: string;
  const AIDFieldName: string;
  const ABlobFieldName: string;
  const AID: Integer;
  var RStream: TMemoryStream
): Integer;
begin
  Result := ReadStreamFromBlobField( ATableName, AIDFieldName, ABlobFieldName, IntToStr( AID ), RStream );
end;

//------------------------------------------------------------------------------
// Реализация чтения потока из блоб-поля
//------------------------------------------------------------------------------
function TDataBaseOperator.ReadStreamFromBlobField(
  const ATableName: string;
  const AIDFieldName: string;
  const ABlobFieldName: string;
  const AID: string;
  var RStream: TMemoryStream
): Integer;
//------------------------------------------------------------------------------
var
  //! Поток для чтения блоба
  LADOBlobStream: TADOBlobStream;
  //! Текст запроса
  LSQLText: string;
  //! Признак ошибки
  LErr: Boolean;
//------------------------------------------------------------------------------
begin
  Result := -1;
  if not FKeepConnection
  then begin
    if not Connect()
    then Exit;
  end;
  RStream.Clear();
  LSQLText := 'SELECT * FROM ' + ATableName + ' WHERE ' + AIDFieldName + ' = ''' + AID + '''';
  FADOQuery.SQL.Text := LSQLText;
  LErr := False;
  try
    FADOQuery.Open();
  except
    on Ex: Exception
    do begin
      LErr := True;
      if Assigned( FLogger )
      then FLogger.ErrorToLog(
        'Ошибка чтения потока из blob-поля:'
          + #13#10'Ошибка получения данных:'
          + #13#10 + Ex.Message
          + #13#10'^ Текст запроса:'
          + #13#10 + LSQLText
      );
    end;
  end;
  if not LErr
  then try
    if ( FADOQuery.RecordCount > 0 )
    then begin
      LADOBlobStream := TADOBlobStream.Create( TBlobField( FADOQuery.FieldByName( ABlobFieldName ) ), bmRead );
      try
        RStream.Seek( 0, soBeginning );
        RStream.LoadFromStream( LADOBlobStream );
      finally
        LADOBlobStream.Free();
      end;
    end;
    Result := RStream.Size;
  except
    on Ex: Exception
    do begin
      if Assigned( FLogger )
      then FLogger.ErrorToLog(
        'Ошибка чтения потока из blob-поля:'
          + #13#10 + Ex.Message
          + #13#10'^ Текст запроса:'
          + #13#10 + LSQLText
      );
    end;
  end;
  if not FKeepConnection
  then Disconnect();
end;

//------------------------------------------------------------------------------
// Реализация проверки соедиения с БД
//------------------------------------------------------------------------------
function TDataBaseOperator.TestDataBaseConnection(
  const ATimeout: Integer
): Boolean;
//------------------------------------------------------------------------------
var
  //! Текущий таймаут
  LCurrentTimeout: Integer;
//------------------------------------------------------------------------------
begin
  LCurrentTimeout := FADOConnection.ConnectionTimeout;
  Disconnect();
  if ( ATimeout > 0 )
  then FADOConnection.ConnectionTimeout := ATimeout;
  Result := Connect();
  if Result and ( ATimeout > 0 )
  then begin
    Disconnect();
    FADOConnection.ConnectionTimeout := LCurrentTimeout;
    Result := Connect();
  end;
  if FADOConnection.Connected and not FKeepConnection
  then Disconnect();
end;

//------------------------------------------------------------------------------
// Реализация начала транзакции
//------------------------------------------------------------------------------
function TDataBaseOperator.BeginTransaction(): Boolean;
begin
  Result := True;
  try
    if not FADOConnection.InTransaction
    then FADOConnection.BeginTrans()
    else begin
      if Assigned( FLogger )
      then FLogger.ErrorToLog( 'Попытка начала транзакции при уже запущенной' );
    end;
  except
    on Ex: Exception
    do begin
      Result := False;
      if Assigned( FLogger )
      then FLogger.ErrorToLog( 'Ошибка начала транзакции:'#13#10 + Ex.Message );
    end;
  end;
end;

//------------------------------------------------------------------------------
// Реализация завершения транзакции
//------------------------------------------------------------------------------
function TDataBaseOperator.CommitTransaction(): Boolean;
begin
  Result := True;
  try
    if FADOConnection.InTransaction
    then FADOConnection.CommitTrans()
    else begin
      if Assigned( FLogger )
      then FLogger.ErrorToLog( 'Попытка завершения незапущенной транзакции' );
    end;
  except
    on Ex: Exception
    do begin
      Result := False;
      if Assigned( FLogger )
      then FLogger.ErrorToLog( 'Ошибка завершения транзакции:'#13#10 + Ex.Message );
    end;
  end;
end;

//------------------------------------------------------------------------------
// Реализация отката транзакции
//------------------------------------------------------------------------------
function TDataBaseOperator.RollbackTransaction(): Boolean;
begin
  Result := True;
  try
    if FADOConnection.InTransaction
    then FADOConnection.RollbackTrans()
    else begin
      if Assigned( FLogger )
      then FLogger.ErrorToLog( 'Попытка отката незапущенной транзакции' );
    end;
  except
    on Ex: Exception
    do begin
      Result := False;
      if Assigned( FLogger )
      then begin
        FLogger.ErrorToLog( 'Ошибка отката транзакции:'#13#10 + Ex.Message );
      end;
    end;
  end;
end;

end.

