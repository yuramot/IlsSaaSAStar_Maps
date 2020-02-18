unit E_SDACDataBaseOperator;
//------------------------------------------------------------------------------

//------------------------------------------------------------------------------
interface

uses
  MidasLib{static link for midas.lib},
  SysUtils, Variants, Classes, Data.DB,
  Datasnap.Provider, Datasnap.DBClient,
  MSAccess, OLEDBAccess, DBAccess, MemDS,
  E_Logger;

//------------------------------------------------------------------------------
//------------------------------------------------------------------------------
//------------ record TConnectionParams ----------------------------------------
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------
//! ������ � ����������� ���������� � ��
(*!
  �������� ������ ����������� � �� � �������
*)
//------------------------------------------------------------------------------
{type
  TConnectionParams = record
    //! ������ ���������� � ��
    ConnectionString: string[255];
    //! ������� ����������� � ��
    TimeOut: Integer;
  end;}

//------------------------------------------------------------------------------
//------------------------------------------------------------------------------
//------------ class TDataBaseOperator -----------------------------------------
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------
//! ����� ��� ������ � ��
(*!
  ������������ ����������� � ���� ������, ���������� ��������,
  ��� ������������� ���������� ����� ������ (DataSet)
*)
type
  TDataBaseOperator = class sealed
  private
    //! ������ ��� ���������� � ��
    FADOConnection: TMSConnection;
    //! ������ ��� ���������� �������� � ��, ������������ �������
    FADOQuery: TMSQuery;
    //! ������ ��� ���������� ������ � ��, �� ������������ �������
    FADOCommand: TMSSQL;
    //! ������� ��������� ���������� (���� True, �� ������������ ���������� ���������� � ��)
    FKeepConnection: Boolean;
    //! ������ �� ������
    FLogger: TLogger;
    //! ������� ����������� � ��
    {!
      @return
      ������ True ��� ������� �����������
    }
    function Connect(): Boolean;
    //! ��������� ���������� �� ��
    procedure Disconnect();
  public
    //! ������� ������ ����������
    {!
      @return
      ������ True � ������ ��������� ����������; ����� - False
    }
    function BeginTransaction(): Boolean;
    //! ������� ���������� ����������
    {!
      @return
      ������ True � ������ ��������� ����������; ����� - False
    }
    function CommitTransaction(): Boolean;
    //! ������� ������ ����������
    {!
      @return
      ������ True � ������ ��������� ����������; ����� - False
    }
    function RollbackTransaction(): Boolean;
    //! ������� ���������� ������ � �� (��������, ������� �� UPDATE � INSERT)
    {!
      @param in
      ASQLText - ����� SQL-�������
      @return
      ������ True � ������ ��������� ����������; ����� - False
    }
    function CommandExecute(
      const ASQLText: string
    ): Boolean; overload;
    //! ������� ���������� �������� � ��, ������������ ������ ������ (��������, ������� �� SELECT)
    {!
      @param in
      ASQLText - ����� SQL-�������
      @param  out
      RDataSet - ������������ ����� ������
      @return
      ������ ���������� ������� � ������ ��������� ����������; ���� -1 � ������ ������
    }
    function FillDataSet(
      const ASQLText: string;
      var RDataSet: TClientDataSet
    ): Integer;
    //! ������� ��������� �������� �� �������
    //! ������������� ������������ ��� ��������, ������������ ������ 1 ������, ������ GetDataSet
    {!
      @param in
      ASQLText - ����� �������
      @param in out
      RValue - �������� ������� ���� ������ ������
      @return
      ������ ���-�� ������� � �������; ���� -1 � ������ ������
    }
    function GetValue(
      const ASQLText: string;
      var RValue: Variant
    ): Integer;
    //! ������� ������ ������ � blob-����
    {!
      @param in
      ATableName - ��� ������� � ���� ������
      @param in
      AIDFieldName - ��� ���� ��������������
      @param in
      ABlobFieldName - ��� ����-���� ��� ������
      @param in
      AID - ������������� ������, ��� ������� ������������ ������
      @param in
      AStream - ����� ��� ������ � ����-����
      @return
      ������ ���������� ���������� ����; ���� -1 � ������ ������
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
    //! ������� ������ ������ �� blob-����
    {!
      @param in
      ATableName - ��� ������� � ���� ������
      @param in
      AIDFieldName - ��� ���� ��������������
      @param in
      ABlobFieldName - ��� ����-���� ��� ������
      @param in
      AID - ������������� ������, �� ������� ������������ ������
      @param in out
      RStream - ����� ��� ������ �� ����-����
      @return
      ������ ���������� ����������� ����; ���� -1 � ������ ������
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
    //! ������� �������� ���������� � ��
    {!
       @param in
       ATimeout - ������� ��� ���������� � �� (0 - ��������� �������)
       @return
       ��� ������� ���������� ������ True; ����� - False
    }
    function TestDataBaseConnection(
      const ATimeout: Integer
    ): Boolean;
    //! �����������
    {!
      @param in
      AConnectionParams - ��������� ����������� � ��
      @param in
      ALogger - ����� (���������� �� ������)
      @param in
      AKeepConnection - ������� ��������� ����������
    }
    constructor Create(
      const AConnectionString: string;
      const ATimeOut: Integer;
      const ALogger: TLogger;
      const AKeepConnection: Boolean
    );
    //! ����������
    destructor Destroy(); override;
  end;

//------------------------------------------------------------------------------
implementation

//------------------------------------------------------------------------------
// ���������� ������������
//------------------------------------------------------------------------------
constructor TDataBaseOperator.Create(
  const AConnectionString: string;
  const ATimeOut: Integer;
  const ALogger: TLogger;
  const AKeepConnection: Boolean
);
begin
  // �������������� ������ ����������� � ��
  FADOConnection := TMSConnection.Create( nil );
  FADOConnection.ConnectString := AConnectionString;
  FADOConnection.ConnectionTimeout := ATimeOut;
  FADOConnection.LoginPrompt := False;
  FADOConnection.Options.DefaultLockTimeout := -1;
  // �������������� ������ ��� ��������
  FADOQuery := TMSQuery.Create( nil );
  FADOQuery.Connection := FADOConnection;
  FADOQuery.CommandTimeout := ATimeOut;
  FADOQuery.ReadOnly := False;
//  FADOQuery.LockMode := lmNone; // default?
  // ��� ��������� ������ ������ ���� ������
//  FADOQuery.CursorType := ctDefaultResultSet;
  FADOQuery.CursorType := ctStatic;
//  FADOQuery.CursorLocation := clUseClient;
  // �������������� ������ ��� ������
  FADOCommand := TMSSQL.Create( nil );
  FADOCommand.Connection := FADOConnection;
  FADOCommand.CommandTimeout := ATimeOut;
  // ���������
  FLogger := ALogger;
  FKeepConnection := AKeepConnection;
  if FKeepConnection
  then Connect();
end;

//------------------------------------------------------------------------------
// ���������� �����������
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
// ���������� ����������� � ��
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
      if ( FLogger <> nil )
      then begin
        FLogger.AddToLog( '������ ���������� � ��:', lhError );
        FLogger.AddToLog( Ex.Message, lhError );
      end;
    end;
  end;
end;

//------------------------------------------------------------------------------
// ���������� ������ ����������� � ��
//------------------------------------------------------------------------------
procedure TDataBaseOperator.Disconnect();
begin
  FADOConnection.Connected := False;
end;

//------------------------------------------------------------------------------
// ���������� ���������� ������ � ��
//------------------------------------------------------------------------------
function TDataBaseOperator.CommandExecute(
  const ASQLText: string
): Boolean;
var
  //! ������� ������� ���������� ����������
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
  then begin // ���� ��� ���������� ����������, �� �������� ��
    if not BeginTransaction()
    then Exit;
  end;
  FADOCommand.SQL.Text := ASQLText;
  try
    FADOCommand.Execute();
    Result := True;
  except
    on Ex: Exception
    do begin
      if ( FLogger <> nil )
      then begin
        FLogger.AddToLog( '������ ���������� �������:', lhError );
        FLogger.AddToLog( Ex.Message, lhError );
        FLogger.AddToLog( '����� �������:', lhError );
        FLogger.AddToLog( ASQLText, lhError );
      end;
    end;
  end;
  if not LTransctionInProcess
  then begin // ���� �� ������ ���������� �� ���� ��������, �� ��������� ��� ���������� �������
    if Result
    then CommitTransaction()
    else RollBackTransaction();
  end;
  if not FKeepConnection
  then Disconnect();
end;

//------------------------------------------------------------------------------
// ���������� �������� � ��
//------------------------------------------------------------------------------
function TDataBaseOperator.FillDataSet(
  const ASQLText: string;
  var RDataSet: TClientDataSet
): Integer;
var
  //! ��������� ��� ������ � DataSet
  LDataSetProvider: TDatasetProvider;
  //! ���� ������
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
        if ( FLogger <> nil )
        then begin
          FLogger.AddToLog( '������ ���������� �������:', lhError );
          FLogger.AddToLog( Ex.Message, lhError );
          FLogger.AddToLog( '����� �������:', lhError );
          FLogger.AddToLog( ASQLText, lhError );
        end;
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
          if ( FLogger <> nil )
          then begin
//            LErr := True;
            FLogger.AddToLog( '������ ���������� �������:', lhError );
            FLogger.AddToLog( '������ ������������ ������ ������:', lhError );
            FLogger.AddToLog( Ex.Message, lhError );
            FLogger.AddToLog( '����� �������:', lhError );
            FLogger.AddToLog( ASQLText, lhError );
          end;
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
// ���������� ��������� �������� �� �������
//------------------------------------------------------------------------------
function TDataBaseOperator.GetValue(
  const ASQLText: string;
  var RValue: Variant
): Integer;
var
  //! �������� ������ ����
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
        if ( FLogger <> nil )
        then begin
          FLogger.AddToLog( '������ ��������� ��������:', lhError );
          FLogger.AddToLog( Ex.Message, lhError );
          FLogger.AddToLog( '����� �������:', lhError );
          FLogger.AddToLog( ASQLText, lhError );
        end;
      end;
    end;
  finally
    LDataSet.Free();
  end;
end;

//------------------------------------------------------------------------------
// ���������� ������ ������ � ����-����
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
// ���������� ������ ������ � ����-����
//------------------------------------------------------------------------------
function TDataBaseOperator.WriteStreamToBlobField(
  const ATableName: string;
  const AIDFieldName: string;
  const ABlobFieldName: string;
  const AID: string;
  const AStream: TMemoryStream
): Integer;
var
  //! ����� �������
  LSQLText: string;
  //! ������� ������� ���������� ����������
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
  then begin // ���� ��� ���������� ����������, �� �������� ��
    if not BeginTransaction()
    then Exit;
  end;
  LSQLText := 'UPDATE ' + ATableName
            + ' SET ' + ABlobFieldName + ' = :blobdata'
            + ' WHERE ' + AIDFieldName + ' = ''' + AID + '''';
  FADOCommand.SQL.Text := LSQLText;
  try
    FADOCommand.Params.ParamByName( 'blobdata' ).LoadFromStream( AStream, ftBlob );
    FADOCommand.Execute();
    Result := AStream.Size;
  except
    on Ex: Exception
    do begin
      if ( FLogger <> nil )
      then begin
        FLogger.AddToLog( '������ ������ ������ � blob-����:', lhError );
        FLogger.AddToLog( Ex.Message, lhError );
        FLogger.AddToLog( '����� �������:', lhError );
        FLogger.AddToLog( LSQLText, lhError );
      end;
    end;
  end;
  if not LTransctionInProcess
  then begin // ���� �� ������ ���������� �� ���� ��������, �� ��������� ��� ���������� �������
    if ( Result >= 0 )
    then CommitTransaction()
    else RollBackTransaction();
  end;
  if not FKeepConnection
  then Disconnect();
end;

//------------------------------------------------------------------------------
// ���������� ������ ������ �� ����-����
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
// ���������� ������ ������ �� ����-����
//------------------------------------------------------------------------------
function TDataBaseOperator.ReadStreamFromBlobField(
  const ATableName: string;
  const AIDFieldName: string;
  const ABlobFieldName: string;
  const AID: string;
  var RStream: TMemoryStream
): Integer;
var
  // ����� ��� ������ �����
  LBlobStream: TStream;
  //! ����� �������
  LSQLText: string;
  //! ������� ������
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
      if ( FLogger <> nil )
      then begin
        FLogger.AddToLog( '������ ������ ������ �� blob-����:', lhError );
        FLogger.AddToLog( '������ ��������� ������:', lhError );
        FLogger.AddToLog( Ex.Message, lhError );
        FLogger.AddToLog( '����� �������:', lhError );
        FLogger.AddToLog( LSQLText, lhError );
      end;
    end;
  end;
  if not LErr
  then try
    if ( FADOQuery.RecordCount > 0 )
    then begin
      LBlobStream := FADOQuery.CreateBlobStream( FADOQuery.FieldByName( ABlobFieldName ), bmRead );
      try
        RStream.Seek( 0, 0 );
        RStream.LoadFromStream( LBlobStream );
      finally
        LBlobStream.Free();
      end;
    end;
    Result := RStream.Size;
  except
    on Ex: Exception
    do begin
      FLogger.AddToLog( '������ ������ ������ �� blob-����:', lhError );
      FLogger.AddToLog( Ex.Message, lhError );
      FLogger.AddToLog( '����� �������:', lhError );
      FLogger.AddToLog( LSQLText, lhError );
    end;
  end;
  if not FKeepConnection
  then Disconnect();
end;

//------------------------------------------------------------------------------
// ���������� �������� ��������� � ��
//------------------------------------------------------------------------------
function TDataBaseOperator.TestDataBaseConnection(
  const ATimeout: Integer
): Boolean;
var
  //! ������� �������
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
// ���������� ������ ����������
//------------------------------------------------------------------------------
function TDataBaseOperator.BeginTransaction(): Boolean;
begin
  Result := True;
  try
    if not FADOConnection.InTransaction
    then FADOConnection.StartTransaction()
    else begin
      if ( FLogger <> nil )
      then FLogger.AddToLog( '������� ������ ���������� ��� ��� ����������', lhHint );
    end;
  except
    on Ex: Exception
    do begin
      Result := False;
      if ( FLogger <> nil )
      then begin
        FLogger.AddToLog( '������ ������ ����������:', lhError );
        FLogger.AddToLog( Ex.Message, lhError );
      end;
    end;
  end;
end;

//------------------------------------------------------------------------------
// ���������� ���������� ����������
//------------------------------------------------------------------------------
function TDataBaseOperator.CommitTransaction(): Boolean;
begin
  Result := True;
  try
    if FADOConnection.InTransaction
    then FADOConnection.Commit()
    else begin
      if ( FLogger <> nil )
      then FLogger.AddToLog( '������� ���������� ������������ ����������', lhHint );
    end;
  except
    on Ex: Exception
    do begin
      Result := False;
      if ( FLogger <> nil )
      then begin
        FLogger.AddToLog( '������ ���������� ����������:', lhError );
        FLogger.AddToLog( Ex.Message, lhError );
      end;
    end;
  end;
end;

//------------------------------------------------------------------------------
// ���������� ������ ����������
//------------------------------------------------------------------------------
function TDataBaseOperator.RollbackTransaction(): Boolean;
begin
  Result := True;
  try
    if FADOConnection.InTransaction
    then FADOConnection.Rollback()
    else begin
      if ( FLogger <> nil )
      then FLogger.AddToLog( '������� ������ ������������ ����������', lhHint );
    end;
  except
    on Ex: Exception
    do begin
      Result := False;
      if ( FLogger <> nil )
      then begin
        FLogger.AddToLog( '������ ������ ����������:', lhError );
        FLogger.AddToLog( Ex.Message, lhError );
      end;
    end;
  end;
end;

end.

