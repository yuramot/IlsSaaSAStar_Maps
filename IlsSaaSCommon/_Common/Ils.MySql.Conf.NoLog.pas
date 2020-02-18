unit Ils.MySql.Conf.NoLog;

interface

uses
  SysUtils, IniFiles,
  ZDataSet, ZConnection, ZAbstractRODataset,
  uADCompClient;

const

//------------------------------------------------------------------------------
//! строка установки нулевой таймзоны
//------------------------------------------------------------------------------
  CMySQLSetZeroTZ = 'SET time_zone=''+0:00''';

  CMySQLUtf8PropertiesText =
    'character_set_client=utf8'#13#10+
    'character_set_connection=utf8'#13#10+
    'character_set_database=utf8'#13#10+
    'character_set_results=utf8'#13#10+
    'character_set_server=utf8'#13#10+
    'character_set_system=utf8'#13#10+
    'collation_connection=utf8_general_ci'#13#10+
    'collation_database=utf8_general_ci'#13#10+
    'collation_server=utf8_general_ci'#13#10+
    'Codepage=utf8'#13#10+
    'datewriteformat=YYYY-MM-DD'#13#10+
    'datereadformat=YYYY-MM-DD'#13#10+
    'datedisplayformat=YYYY-MM-DD'#13#10+
    'timewriteformat=HH:NN:SS.ZZZ'#13#10+
    'timereadformat=HH:NN:SS.ZZZ'#13#10+
    'timedisplayformat=HH:NN:SS.ZZZ'#13#10+
    'datetimewriteformat=YYYY-MM-DD HH:NN:SS.ZZZ'#13#10+
    'datetimereadformat=YYYY-MM-DD HH:NN:SS.ZZZ'#13#10+
    'datetimedisplayformat=YYYY-MM-DD HH:NN:SS.ZZZ'#13#10+
    'AutoEncodeStrings=ON'#13#10+
    'controls_cp=CP_UTF16'#13#10;

type

  TMySQLDatabaseConfig = record
    Host: string;
    Port: Integer;
    Login: string;
    Password: string;
    Database: string;
    Protocol: string;
    constructor Create(AHost: string; APort: Integer; ALogin, APassword, ADatabase, AProtocol: string); overload;
    constructor Create(AIni: TIniFile); overload;
    function ConfigConnection(out zc: TZConnection; const ASetZeroTZ: Boolean): TMySQLDatabaseConfig; overload;
    function ConfigConnection(out adc: TADConnection; const ASetZeroTZ: Boolean): TMySQLDatabaseConfig; overload;
    function InitQuery(out AQuery: TZQuery; const AConnection: TZConnection; ASQL: string): TMySQLDatabaseConfig; overload;
    function InitQuery(out AQuery: TZReadOnlyQuery; const AConnection: TZConnection; ASQL: string): TMySQLDatabaseConfig; overload;
    function InitQuery(out AQuery: TADQuery; const AConnection: TADConnection; ASQL: string): TMySQLDatabaseConfig; overload;
    function InitQuery(out AQuery: TADStoredProc; const AConnection: TADConnection; AProcName: string): TMySQLDatabaseConfig; overload;
  end;

  TMySQLConnectionHandler = class
    class procedure AfterConnectSetZeroTZ(ASender: TObject);
  end;

implementation

uses
  CINIFilesData;

{ TMySQlDatabaseConfig }

constructor TMySQLDatabaseConfig.Create(AHost: string; APort: Integer; ALogin, APassword, ADatabase, AProtocol: string);
begin
  Host := AHost;
  Port := APort;
  Login := ALogin;
  Password := APassword;
  Database := ADatabase;
  Protocol := AProtocol;
end;

constructor TMySQLDatabaseConfig.Create(AIni: TIniFile);
begin
  Create(
    AIni.ReadString(CIniMySqlSection, CIniDBAddr, CIniDBAddrDef),
    AIni.ReadInteger(CIniMySqlSection, CIniDBPort, CIniDBPortDef),
    AIni.ReadString(CIniMySqlSection, CIniDBLogin, CIniDBLoginDef),
    AIni.ReadString(CIniMySqlSection, CIniDBPass, CIniDBPassDef),
    AIni.ReadString(CIniMySqlSection, CIniDBName, CIniDBNameDef),
    AIni.ReadString(CIniMySqlSection, CIniDBProtocol, CIniDBProtocolDef)
  );
end;

function TMySQLDatabaseConfig.ConfigConnection(out zc: TZConnection; const ASetZeroTZ: Boolean): TMySQLDatabaseConfig;
begin
  Result := Self;
  if not Assigned(zc) then
    zc := TZConnection.Create(nil);
  zc.HostName := Host;
  zc.Port := Port;
  zc.User := Login;
  zc.Password := Password;
  zc.Database := Database;
  zc.Catalog := Database;
  zc.Protocol := Protocol;
  zc.Properties.Text := CMySQLUtf8PropertiesText;
  zc.Properties.Values['timeout'] := '10';
  if ASetZeroTZ then
  begin
    zc.AfterConnect := TMySQLConnectionHandler.AfterConnectSetZeroTZ;
    zc.AfterReconnect := TMySQLConnectionHandler.AfterConnectSetZeroTZ;
  end;
end;

function TMySQLDatabaseConfig.ConfigConnection(out adc: TADConnection; const ASetZeroTZ: Boolean): TMySQLDatabaseConfig;
begin
  Result := Self;
  if not Assigned(adc) then
    adc := TADConnection.Create(nil);
  adc.Params.Values['DriverID'] := 'MySQL';
  adc.Params.Values['Database'] := Database;
  adc.Params.Values['User_Name'] := Login;
  adc.Params.Values['Password'] := Password;
  adc.Params.Values['Server'] := Host;
  adc.Params.Values['Port'] := IntToStr(Port);
  adc.Params.Values['CharacterSet'] := 'utf8';
  adc.Params.Values['TinyIntFormat'] := 'Integer';
  adc.FormatOptions.FmtDisplayDateTime := 'yyyy-mm-dd hh:nn:ss.zzz';
  if ASetZeroTZ then
  begin
    adc.AfterConnect := TMySQLConnectionHandler.AfterConnectSetZeroTZ;
//    adc.AfterReconnect := TMySQLConnectionHandler.AfterConnectSetZeroTZ;
  end;
end;

function TMySQLDatabaseConfig.InitQuery(out AQuery: TZQuery; const AConnection: TZConnection; ASQL: string): TMySQLDatabaseConfig;
begin
  Result := Self;
  AQuery := TZQuery.Create(nil);
  AQuery.Connection := AConnection;
  AQuery.SQL.Text := ASQL;
end;

function TMySQLDatabaseConfig.InitQuery(out AQuery: TZReadOnlyQuery; const AConnection: TZConnection; ASQL: string): TMySQLDatabaseConfig;
begin
  Result := Self;
  AQuery := TZReadOnlyQuery.Create(nil);
  AQuery.Connection := AConnection;
  AQuery.SQL.Text := ASQL
end;

function TMySQLDatabaseConfig.InitQuery(out AQuery: TADQuery; const AConnection: TADConnection; ASQL: string): TMySQLDatabaseConfig;
begin
  Result := Self;
  AQuery := TADQuery.Create(nil);
  AQuery.Connection := AConnection;
  AQuery.SQL.Text := ASQL;
end;

function TMySQLDatabaseConfig.InitQuery(out AQuery: TADStoredProc; const AConnection: TADConnection; AProcName: string): TMySQLDatabaseConfig;
begin
  Result := Self;
  AQuery := TADStoredProc.Create(nil);
  AQuery.Connection := AConnection;
  AQuery.StoredProcName := AProcName;
end;

{ TMySQLConnectionHandler }

class procedure TMySQLConnectionHandler.AfterConnectSetZeroTZ(ASender: TObject);
begin
  if ASender is TZConnection then
    TZConnection(ASender).ExecuteDirect(CMySQLSetZeroTZ)
  else if ASender is TADConnection then
    TADConnection(ASender).ExecSQL(CMySQLSetZeroTZ)
  ;
end;

end.

