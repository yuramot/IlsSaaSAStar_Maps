unit Ils.Redis.Publisher;

interface

uses
  Redis.Client, System.Classes, Ils.Redis.Conf, SysUtils;

type

  TRedisPublisher = class
  private
    FRCli: TRedisClient;
    FChan: string;
    FHost: string;
    FPort: Integer;
    FSSL: Boolean;
    FPassword: string;
    function TryConnect: Boolean;
  public
    procedure Publish(AMessage: string);
    constructor Create(const AChan: string; const AConf: TRedisConf);
    destructor Destroy; override;
  end;

implementation

{ TRedisPublisher }

constructor TRedisPublisher.Create(const AChan: string;
  const AConf: TRedisConf);
begin
  FChan := AChan;

  FHost := AConf.Host;
  FPort := AConf.Port;
  FSSL := AConf.SSL;
  FPassword := AConf.Password;
  TryConnect;
end;

destructor TRedisPublisher.Destroy;
begin
  FRCli.Free;
  inherited;
end;

function TRedisPublisher.TryConnect: Boolean;
begin
  Result := False;
  FRCli := TRedisClient.Create(FHost, FPort, FSSL);

  try
    FRCli.Connect;
    if FPassword <> '' then
      FRCli.AUTH(FPassword);
    Result := True;
  except
    FreeAndNil(FRCli);
  end;
end;


procedure TRedisPublisher.Publish(AMessage: string);
begin
  try
    if Assigned(FRCli) or TryConnect then
      FRCli.PUBLISH(FChan, AMessage);
  except

  end;
end;

end.
