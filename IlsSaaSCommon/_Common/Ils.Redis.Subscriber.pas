unit Ils.Redis.Subscriber;

interface

uses
  Redis.Client, System.Classes, Ils.Redis.Conf, SysUtils, Ils.Logger;

type

  TArrayOfString = array of string;

  TOnRedisSubscribeMessage = procedure (const AChan, AMessage: string) of object;

  TRedisSubscriber = class(TThread)
  private
    FRCli: TRedisClient;
    FChan, FMessage: string;
    FHost: string;
    FPort: Integer;
    FSSL: Boolean;
    FPassword: string;
    FSubscribeChans: TArrayOfString;
    FOnMessage: TOnRedisSubscribeMessage;
    FOnLog: TLogFunction;
    function OnTimeOut: boolean;
    procedure OnRedisMessage(AChan, AMessage: string);
    procedure DoMessage;
    function TryConnect: Boolean;
  protected
    procedure Execute; override;
    procedure ToLog(const AMessage: string);

  public
    procedure Start;
    property OnMessage: TOnRedisSubscribeMessage read FOnMessage write FOnMessage;
    constructor Create(const AChans: TArrayOfString; const AConf: TRedisConf; const AOnMessage: TOnRedisSubscribeMessage; const AOnLog: TLogFunction = nil; const APaused: Boolean = False);
    destructor Destroy; override;
  end;

implementation

{ TRedisSubscribe }

constructor TRedisSubscriber.Create(const AChans: TArrayOfString; const AConf: TRedisConf; const AOnMessage: TOnRedisSubscribeMessage; const AOnLog: TLogFunction = nil; const APaused: Boolean = False);
begin
  inherited Create(True);
  FOnMessage := AOnMessage;
  FOnLog := AOnLog;
  FSubscribeChans := AChans;
  FreeOnTerminate := True;
  FHost := AConf.Host;
  FPort := AConf.Port;
  FSSL := AConf.SSL;
  FPassword := AConf.Password;
  TryConnect;

  Suspended := APaused;
end;

destructor TRedisSubscriber.Destroy;
begin
  FRCli.Free;
end;

function TRedisSubscriber.TryConnect: Boolean;
begin
  FRCli := TRedisClient.Create(FHost, FPort, FSSL);

  try
    FRCli.Connect;
    if FPassword <> '' then
      FRCli.AUTH(FPassword);
    Result := True;
  except
    FreeAndNil(FRCli);
    raise;
  end;
end;

procedure TRedisSubscriber.Execute;
begin
  while not Terminated do
    try
      if Assigned(FRCli) or TryConnect then
        FRCli.SUBSCRIBE(FSubscribeChans, OnRedisMessage, OnTimeOut)
    except
      on E: Exception do begin
        ToLog('Error on redis subscribe: ' + E.ClassName + ':' + E.Message);
        FreeAndNil(FRCli);
        Sleep(2500);
      end;
    end;
end;

procedure TRedisSubscriber.DoMessage;
begin
  if Assigned(FOnMessage) then
    try
      FOnMessage(FChan, FMessage);
    except
      //TODO:!!!
    end;
end;

procedure TRedisSubscriber.ToLog(const AMessage: string);
begin
  if Assigned(FOnLog) then
    FOnLog(AMessage);
end;

procedure TRedisSubscriber.OnRedisMessage(AChan, AMessage: string);
begin
  FChan := AChan;
  FMessage := AMessage;
  Synchronize(Self, DoMessage);
end;

function TRedisSubscriber.OnTimeOut: boolean;
begin
  Result := not Terminated;
end;

procedure TRedisSubscriber.Start;
begin
  Suspended := False;
end;

end.
