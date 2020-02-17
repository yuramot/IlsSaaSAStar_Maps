unit clMMTimer;

interface

uses
  Winapi.Windows,
  System.Classes,
  Winapi.MMSystem;


type
  TMMTimer = class
  private
    FInterval: integer;
    fId: UINT;
    FOnTimer: TNotifyEvent;
    function GetEnabled: boolean;
    procedure SetEnabled(Value: boolean);
    procedure SetInterval(Value: integer);
  protected
    procedure DoTimer; virtual;
  public
    property Interval: integer read FInterval write SetInterval;
    property Enabled: boolean read GetEnabled write SetEnabled;
    property OnTimer: TNotifyEvent read FOnTimer write FOnTimer;
    destructor Destroy; override;
  end;

implementation

{ TMMTimer }

procedure MMCallBack(uTimerID, uMsg: UINT; dwUser, dw1, dw2: DWORD); stdcall;
begin
  if dwUser <> 0 then
    TMMTimer(dwUser).DoTimer;
end;

destructor TMMTimer.Destroy;
begin
  if Enabled then
  begin
    if (fId <> 0) then
    begin
      timeKillEvent(fId);
      fId := 0;
    end
  end;
  inherited;
end;

procedure TMMTimer.DoTimer;
begin
  if Assigned(FOnTimer) then
    FOnTimer(Self);
end;

function TMMTimer.GetEnabled: boolean;
begin
  try
    Result := fId <> 0
  except
    Result := False;
  end;
end;

procedure TMMTimer.SetEnabled(Value: boolean);
begin
  if Enabled <> Value then
  begin
    if fId <> 0 then
    begin
      timeKillEvent(fId);
      fId := 0;
    end
    else
    begin
      fId := timeSetEvent(FInterval, 0, @MMCallBack, DWORD(Self),
        TIME_CALLBACK_FUNCTION or TIME_PERIODIC or TIME_KILL_SYNCHRONOUS);
    end;
  end;
end;

procedure TMMTimer.SetInterval(Value: integer);
var
  oldEnabled: boolean;
begin
  if FInterval <> Value then
  begin
    oldEnabled := Enabled;
    Enabled := false;
    FInterval := Value;
    Enabled := oldEnabled;
  end;
end;

end.
