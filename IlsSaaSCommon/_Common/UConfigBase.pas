unit UConfigBase;

interface

uses
  Vcl.ExtCtrls, System.SysUtils, Ils.Kafka;

type

  TConfigBase = class;

  TConfigEvent = procedure(ASender: TConfigBase) of object;

  TConfigBaseAbstract = class
  protected
    function Reload: Boolean; virtual; abstract;
    function CheckNeedReload: Boolean; virtual; abstract;
    function GetModifiedDateTime: TDateTime; virtual; abstract;
    function GetModifiedCount: Integer; virtual; abstract;
  end;

  TConfigBase = class(TConfigBaseAbstract)
  protected
    FCheckTimer: TTimer;
    FLastLoadedVersionDT: TDateTime;
    FLastLoadedVersionCount: Integer;
    FCurrentVersionDT: TDateTime;
    FCurrentVersionCount: Integer;
    FLogCB: TLogFunc;
    FOnConfigChange: TConfigEvent;

    procedure ChangedNotify;
    procedure OnCheckTimer(Sender: TObject);
    procedure SetInterval(const Value: Integer);
    function GetInterval: Integer;
    procedure Load;
    function CheckNeedReload: Boolean; override;

  public
    constructor Create(
      const AConfigChangeCB: TConfigEvent;
      const ALogCB: TLogFunc = nil;
      const ACheckInterval: Integer = 1000); virtual;
    destructor Destroy; override;
  end;


implementation

{ TConfigBase }

procedure TConfigBase.ChangedNotify;
begin
  if Assigned(FOnConfigChange) then
    FOnConfigChange(Self);
end;

function TConfigBase.CheckNeedReload: Boolean;
begin
  FCurrentVersionDT := GetModifiedDateTime;
  FCurrentVersionCount := GetModifiedCount;

  Result := (FCurrentVersionDT <> FLastLoadedVersionDT) or (FCurrentVersionCount <> FLastLoadedVersionCount);
end;

constructor TConfigBase.Create(const AConfigChangeCB: TConfigEvent; const ALogCB: TLogFunc; const ACheckInterval: Integer);
begin
  FCheckTimer := TTimer.Create(nil);
  FCheckTimer.OnTimer := OnCheckTimer;
  FCheckTimer.Interval := ACheckInterval;
  FOnConfigChange := AConfigChangeCB;
  FLogCB := ALogCB;
  FCheckTimer.Enabled := True;
end;

destructor TConfigBase.Destroy;
begin
  FCheckTimer.Free;

  inherited;
end;

procedure TConfigBase.Load;
begin
  if CheckNeedReload then
  begin
    if ((FLastLoadedVersionDT < FCurrentVersionDT) or (FLastLoadedVersionCount <> FCurrentVersionCount)) and Reload then
    begin
      FLastLoadedVersionDT := FCurrentVersionDT;
      FLastLoadedVersionCount := FCurrentVersionCount;
      ChangedNotify;
    end;
  end;
end;

procedure TConfigBase.OnCheckTimer(Sender: TObject);
begin
  FCheckTimer.Enabled := False;
  try
    Load;
  finally
    FCheckTimer.Enabled := True;
  end;
end;

function TConfigBase.GetInterval: Integer;
begin
  Result := FCheckTimer.Interval;
end;

procedure TConfigBase.SetInterval(const Value: Integer);
begin
  FCheckTimer.Interval := Value;
end;

end.
