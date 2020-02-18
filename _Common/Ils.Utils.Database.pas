unit Ils.Utils.Database;

interface

uses
  ZConnection, Ils.Logger, SysUtils;

function CheckConnection(AConnection: TZConnection): Boolean;

implementation

function CheckConnection(AConnection: TZConnection): Boolean;
begin
  try
    if not AConnection.Connected then
      AConnection.Connect;
    if not AConnection.PingServer then
      AConnection.Reconnect;
  except
    on E: Exception do
      ToLog(E.ClassName + ':' + E.Message);
  end;
  Result := AConnection.Connected;
end;

end.
