unit UConfigMySql;

interface

uses
  UConfigJson, UConfigBase, JsonDataObjects, System.SysUtils,
  ZAbstractConnection, ZConnection, ZAbstractRODataset, ZAbstractDataset, ZDataset,
  Ils.MySQL.Conf, Ils.Kafka;

type
  TConfigMySqlAbstract = class(TConfigJson)
  protected
    FMySqlConnection: TZConnection;
    FMySqlQuery: TZQuery;

  protected
    function Reload: Boolean; override;
    function CheckConnect: Boolean; virtual; abstract;
    function GetModifiedDateTime: TDateTime; override;
    function GetModifiedCount: Integer; override;
    function GetDBConfigModifiedDateTime: TDateTime; virtual; abstract;
    function GetDBConfigModifiedCount: Integer; virtual; abstract;
    function ReloadDBConfig: Boolean; virtual; abstract;
  public

    constructor Create(const ADBConf: TMySQlDatabaseConfig; const AConfigChangeCB: TConfigEvent; const ALogCB: TLogFunc = nil; const ACheckInterval: Integer = 1000); reintroduce; virtual;
    destructor Destroy; override;
  end;

  TConfigMySql = class(TConfigMySqlAbstract)
  protected
    FFormatSettings: TFormatSettings;

    function CheckConnect: Boolean; override;
    function MakeConfigJson: Boolean; virtual; abstract;
    function ReloadDBConfig: Boolean; override;
  public
    constructor Create(const ADBConf: TMySQlDatabaseConfig; const AConfigChangeCB: TConfigEvent; const ALogCB: TLogFunc = nil; const ACheckInterval: Integer = 1000); reintroduce; virtual;
    destructor Destroy; override;
  end;

implementation

{ TConfigMySqlAbstract }

constructor TConfigMySqlAbstract.Create(const ADBConf: TMySQlDatabaseConfig;  const AConfigChangeCB: TConfigEvent; const ALogCB: TLogFunc; const ACheckInterval: Integer);
begin
  FLogCB := ALogCB;

  FMySqlConnection := TZConnection.Create(nil);
  ADBConf.ConfigConnection(FMySqlConnection, False);

  FMySqlQuery := TZQuery.Create(nil);
  FMySqlQuery.Connection := FMySqlConnection;
  try
    FMySqlConnection.Connect;
  except
    on E: Exception do
      Log('Ошибка соединения с базой данных: ' + E.Message);
  end;

  inherited Create('', AConfigChangeCB, ALogCB, ACheckInterval);
end;

destructor TConfigMySqlAbstract.Destroy;
begin
  FMySqlQuery.Free;
  FMySqlConnection.Free;

  inherited;
end;

function TConfigMySqlAbstract.GetModifiedDateTime: TDateTime;
begin
  if not CheckConnect then
    Exit(0);

  Result := GetDBConfigModifiedDateTime;
end;

function TConfigMySqlAbstract.GetModifiedCount: Integer;
begin
  if not CheckConnect then
    Exit(0);

  Result := GetDBConfigModifiedCount;
end;

function TConfigMySqlAbstract.Reload: Boolean;
begin
  Result := ReloadDBConfig;
end;

{ TConfigMySql }

function TConfigMySql.CheckConnect: Boolean;
begin
  Result := False;

  try
    if not FMySqlConnection.Connected then
      FMySqlConnection.Connect;

    Result := FMySqlConnection.PingServer;
  except

  end;
end;

constructor TConfigMySql.Create(const ADBConf: TMySQlDatabaseConfig; const AConfigChangeCB: TConfigEvent;
  const ALogCB: TLogFunc; const ACheckInterval: Integer);
begin
  FFormatSettings.DecimalSeparator := '.';
  inherited Create(ADBConf, AConfigChangeCB, ALogCB, ACheckInterval);
end;

destructor TConfigMySql.Destroy;
begin
  inherited;
end;

function TConfigMySql.ReloadDBConfig: Boolean;
begin
  Result := MakeConfigJson;
end;

end.
