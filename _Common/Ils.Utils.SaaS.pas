unit Ils.Utils.SaaS;

interface

uses
  Redis.Client, Ils.Utils, Windows, JsonDataObjects, DateUtils, SysUtils,
  Ils.Logger, IniFiles, Ils.Utils.Debug, Redis.NetLib.INDY, Ils.Redis.Conf;

type

  TRedisActivityReporter = class
  private
    FRedisConfig: TRedisConf;
    FInstance, FName, FVersion, FConnectionHostName, FConnectionDatabase: string;
    FStarted: TDateTime;
    FTimeOut: Integer;
    FRedisClient: TRedisClient;
    FConnected: Boolean;
  public
    function Connect: Boolean;
    procedure SaveLastActivity(const AOperations: Integer; const ATime: TTime);

    constructor Create(
      const ARedisConfig: TRedisConf;
      const AInstance, AName, AVersion, AConnectionHostName, AConnectionDatabase: string;
      const ATimeOut: Integer = 600
    );
    destructor Destroy; override;
  end;



implementation

{ TRedisActivity }

function TRedisActivityReporter.Connect: Boolean;
begin
  if not FConnected then
    try
      FRedisClient.Connect;
      FRedisClient.AUTH(FRedisConfig.Password);
      FConnected := True;
    except
      on E: Exception do
        ToLog('TRedisActivity.Create - ' + E.ClassName + ':' + E.Message);
    end;
  Result := FConnected;
end;

constructor TRedisActivityReporter.Create(
  const ARedisConfig: TRedisConf;
  const AInstance, AName, AVersion, AConnectionHostName, AConnectionDatabase: string;
  const ATimeOut: Integer = 600
);
begin
  FRedisConfig := ARedisConfig;
  FInstance := AInstance;
  FName := AName;
  FVersion := AVersion;
  FConnectionHostName := AConnectionHostName;
  FConnectionDatabase := AConnectionDatabase;
  FTimeOut := ATimeOut;
  FStarted := NowUTC;
  FRedisClient :=  TRedisClient.Create(FRedisConfig.Host, FRedisConfig.Port);
  FConnected := False;
  if FRedisConfig.Enabled then
    Connect;
end;

destructor TRedisActivityReporter.Destroy;
begin
  if FConnected then
    FRedisClient.Disconnect;
  FRedisClient.Free;

  inherited;
end;

procedure TRedisActivityReporter.SaveLastActivity(const AOperations: Integer; const ATime: TTime);

  function ComputerName: string;
  const
    MAX_COMPUTERNAME = MAX_PATH;
  var
    b: array[0 .. MAX_COMPUTERNAME - 1] of Char;
    l: dword;
  begin
    l := MAX_COMPUTERNAME;
    if GetComputerName(b, l) then
      Result := b
    else
      Result := ''
  end;

var
  jo: TJsonObject;
  Key: string;
begin
  if not FRedisConfig.Enabled then
    Exit;

  if not FConnected then
    Exit;
  try
    try
      jo := TJsonObject.Create;
      try
        jo.S['Instance']          := LowerCase(FInstance);
        jo.S['Name']              := LowerCase(FName);
        jo.I['Process']           := GetCurrentProcessId;
        jo.S['Version']           := FVersion;
        jo.S['Host']              := ComputerName;
        jo.S['DB']                := FConnectionHostName + ':' + FConnectionDatabase;
        jo.I['Memory']            := MemoryUsed;// FormatMemoryUsed('bytes');// '';
        jo.I['StartTime']         := DateTimeToUnix(FStarted);
        jo.I['LastTime']          := DateTimeToUnix(NowUTC);
        jo.I['TimeOut']           := FTimeOut;
        jo.I['Operations']        := AOperations;
        jo.F['TimeOfOperations']  := ATime * SecsPerDay;

        Key := LowerCase(FInstance + '_' + FName + '_' + IntToStr(GetCurrentProcessId));
        FRedisClient.&SET(Key, jo.ToJSON());
        FRedisClient.EXPIRE(Key, FTimeOut);
      finally
        jo.Free;
      end;
    except
      on E: Exception do begin
        ToLog('TRedisActivity.SaveLastActivity - ' + E.ClassName + ':' + E.Message);
        FConnected := False;
        FRedisClient.Disconnect;
      end;
    end;
  except
    on E: Exception do begin
      ToLog('TRedisActivity.SaveLastActivity (L2) - ' + E.ClassName + ':' + E.Message);
    end;
  end;
end;


end.
