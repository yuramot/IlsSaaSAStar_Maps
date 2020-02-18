unit UMySQLCheck;

interface

uses
  SysUtils, Ils.Logger, ZConnection, uADCompClient, Ils.MySql.Conf;

function CheckTryRepairMySQL(
  const AConnection: TZConnection
): Boolean; overload;

function CheckTryRepairMySQL(
  const AConnection: TADConnection
): Boolean; overload;

implementation

function CheckTryRepairMySQL(
  const AConnection: TZConnection
): Boolean;
var
  DBInfo: string;
begin
  DBInfo := 'СУБД ' + AConnection.HostName + ':' + IntToStr(AConnection.Port) + '@' + AConnection.Database + ' => ';
  Result := AConnection.Connected and AConnection.Ping();
  if not Result then
  begin
    ToLog(DBInfo + 'обрыв связи, попытка восстановления');
    try
      AConnection.Disconnect();
      AConnection.Connect();
      ToLog(DBInfo + 'восстановление связи успешно');
      Result := True;
    except
      on Ex: Exception do
        ToLog(DBInfo + 'ошибка восстановления связи "' + Ex.ClassName + '":' + Ex.Message);
    end;
  end;
end;

function CheckTryRepairMySQL(
  const AConnection: TADConnection
): Boolean;
var
  DBInfo: string;
begin
  DBInfo := 'СУБД ' + AConnection.Params.Values['Server'] + ':' + AConnection.Params.Values['Port'] + '@' + AConnection.Params.Values['Database'] + ' => ';
  try
    Result := AConnection.Connected and AConnection.Ping();
  except
    on Ex: Exception do
    begin
      ToLog(DBInfo + 'ошибка проверки связи "' + Ex.ClassName + '":' + Ex.Message);
      Result := False;
    end;
  end;
  if not Result then
  begin
    ToLog(DBInfo + 'обрыв связи, попытка восстановления');
    try
      AConnection.Connected := False;
      AConnection.Connected := True;
      ToLog(DBInfo + 'восстановление связи успешно');
      Result := True;
    except
      on Ex: Exception do
        ToLog(DBInfo + 'ошибка восстановления связи "' + Ex.ClassName + '":' + Ex.Message);
    end;
  end;
end;

end.

