unit Cache.Elements;
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------

//------------------------------------------------------------------------------
interface

uses
  SysUtils, Math,
  Cache.Root, Geo.Pos, Event.Cnst{, PlanFact.Types};

//------------------------------------------------------------------------------
type

//------------------------------------------------------------------------------
//!
//------------------------------------------------------------------------------
  TIMEI = Int64;

//------------------------------------------------------------------------------
//! источник записи
//------------------------------------------------------------------------------
  TObjOrigin = (ooNew, ooLoaded);

//------------------------------------------------------------------------------
//------------------------------------------------------------------------------
//! мегаблок гео-событий !
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------

//------------------------------------------------------------------------------
//! внешний ключ гео-событий
//------------------------------------------------------------------------------
  TEventGeoDictKey = packed record
    IMEI: TIMEI;
    ZoneID: Integer;
  end;

//------------------------------------------------------------------------------
//! список кэшей гео-событий
//------------------------------------------------------------------------------
  TEventGeoDictionary = class(TCacheDictionary<TEventGeoDictKey>);

//------------------------------------------------------------------------------
//! гео-событие
//------------------------------------------------------------------------------
  TEventGeo = class(TCacheDataObjectAbstract)
  private
    FChanged: TDateTime;
    FStored: TDateTime;
    FOrigin: TObjOrigin;
    FKey: TEventGeoDictKey;
    FDTMark: TDateTime;
    FDuration: Double;
    FTill: TDateTime;
    FTriggerID: Integer;
    procedure SetDuration(const Value: Double);
    procedure SetTill(const Value: TDateTime);
  protected
    function GetDTMark(): TDateTime; override;
  public
    constructor Create(
      ASource: TEventGeo
    ); overload;
    constructor Create(
      const AOrigin: TObjOrigin;
      const AKey: TEventGeoDictKey;
      const ADTMark: TDateTime;
      const ATriggerID: Integer;
      const ADuration: Double
    ); overload;
    constructor Create(
      const AOrigin: TObjOrigin;
      const AIMEI: TIMEI;
      const AZoneID: Integer;
      const ADTMark: TDateTime;
      const ATriggerID: Integer;
      const ADuration: Double
    ); overload;
    function Clone(): TCacheDataObjectAbstract; override;
    property Key: TEventGeoDictKey read FKey;
    property Duration: Double read FDuration write SetDuration;
    property Till: TDateTime read FTill write SetTill;
    property TriggerID: Integer read FTriggerID;
    property Changed: TDateTime read FChanged;
    property Stored: TDateTime read FStored write FStored;
    property Origin: TObjOrigin read FOrigin write FOrigin;
  end;

//------------------------------------------------------------------------------
//------------------------------------------------------------------------------
//! мегаблок событий движения !
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------

//------------------------------------------------------------------------------
//! список кэшей событий движения
//------------------------------------------------------------------------------
  TEventMoveDictionary = class(TCacheDictionary<TIMEI>);

//------------------------------------------------------------------------------
//! событие движения
//------------------------------------------------------------------------------
  TEventMove = class(TCacheDataObjectAbstract)
  private
    FChanged: TDateTime;
    FStored: TDateTime;
    FOrigin: TObjOrigin;
    FKey: TIMEI;
    FDTMark: TDateTime;
    FDuration: Double;
    FTill: TDateTime;
    FEventType: TEventSystemType;
    FGeoPos: TGeoPos;
    procedure SetDuration(const Value: Double);
    procedure SetTill(const Value: TDateTime);
    procedure SetEventType(const Value: TEventSystemType);
    procedure SetGeoPos(const Value: TGeoPos);
  protected
    function GetDTMark(): TDateTime; override;
  public
    constructor Create(
      ASource: TEventMove
    ); overload;
    constructor Create(
      const AOrigin: TObjOrigin;
      const AKey: TIMEI;
      const ADTMark: TDateTime;
      const ADuration: Double;
      const AEventType: TEventSystemType;
      const AGeoPos: TGeoPos
    ); overload;
    function Clone(): TCacheDataObjectAbstract; override;
    property Key: TIMEI read FKey;
    property Duration: Double read FDuration write SetDuration;
    property Till: TDateTime read FTill write SetTill;
    property EventType: TEventSystemType read FEventType write SetEventType;
    property GeoPos: TGeoPos read FGeoPos write SetGeoPos;
    property Changed: TDateTime read FChanged;
    property Stored: TDateTime read FStored write FStored;
    property Origin: TObjOrigin read FOrigin write FOrigin;
  end;

//------------------------------------------------------------------------------
//------------------------------------------------------------------------------
//! мегаблок событий гаража !
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------

//------------------------------------------------------------------------------
//! ключ списка кэшей событий гаража
//------------------------------------------------------------------------------
  TEventDepotDictKey = packed record
    IMEI: TIMEI;
    DepotID: Integer;
  end;

//------------------------------------------------------------------------------
//! список кэшей событий гаража
//------------------------------------------------------------------------------
  TEventDepotDictionary = class(TCacheDictionary<TEventDepotDictKey>);

//------------------------------------------------------------------------------
//! событие гаража
//------------------------------------------------------------------------------
  TEventDepot = class(TCacheDataObjectAbstract)
  private
    FChanged: TDateTime;
    FStored: TDateTime;
    FOrigin: TObjOrigin;
    FKey: TEventDepotDictKey;
    FDTMark: TDateTime;
    FDuration: Double;
    FTill: TDateTime;
    FGUID: string;
    procedure SetDuration(const Value: Double);
    procedure SetTill(const Value: TDateTime);
  protected
    function GetDTMark(): TDateTime; override;
  public
    constructor Create(
      ASource: TEventDepot
    ); overload;
    constructor Create(
      const AOrigin: TObjOrigin;
      const AKey: TEventDepotDictKey;
      const ADTMark: TDateTime;
      const ADuration: Double;
      const AGUID: string
    ); overload;
    constructor Create(
      const AOrigin: TObjOrigin;
      const AIMEI: TIMEI;
      const ADepotID: Integer;
      const ADTMark: TDateTime;
      const ADuration: Double;
      const AGUID: string
    ); overload;
    function Clone(): TCacheDataObjectAbstract; override;
    property Key: TEventDepotDictKey read FKey;
    property Duration: Double read FDuration write SetDuration;
    property Till: TDateTime read FTill write SetTill;
    property GUID: string read FGUID;
    property Changed: TDateTime read FChanged;
    property Stored: TDateTime read FStored write FStored;
    property Origin: TObjOrigin read FOrigin write FOrigin;
  end;

//------------------------------------------------------------------------------
//------------------------------------------------------------------------------
//! мегаблок событий waypoint'а !
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------

//------------------------------------------------------------------------------
//! ключ списка кэшей событий waypoint'а
//------------------------------------------------------------------------------
  TEventWaypointDictKey = packed record
    VehicleID: Integer;
    WaypointID: Integer;
  end;

//------------------------------------------------------------------------------
//! ключ списка кэшей событий waypoint'а
//------------------------------------------------------------------------------
  TEventWaypointDictionary = class(TCacheDictionary<TEventWaypointDictKey>);

//------------------------------------------------------------------------------
//! событие waypoint'а
//------------------------------------------------------------------------------
  TEventWaypoint = class(TCacheDataObjectAbstract)
  private
    FChanged: TDateTime;
    FStored: TDateTime;
    FOrigin: TObjOrigin;
    FKey: TEventWaypointDictKey;
    FDTMark: TDateTime;
    FDuration: Double;
    FTill: TDateTime;
    FPlanHit: Boolean;
    FWorktimeHit: Boolean;
    FFeasibleHit: Boolean;
    FGUID: string;
    FDepotGUID: string;
    procedure SetDuration(const Value: Double);
    procedure SetTill(const Value: TDateTime);
    procedure SetPlanHit(const Value: Boolean);
    procedure SetWorktimeHit(const Value: Boolean);
    procedure SetFeasibleHit(const Value: Boolean);
  protected
    function GetDTMark(): TDateTime; override;
  public
    constructor Create(
      ASource: TEventWaypoint
    ); overload;
    constructor Create(
      const AOrigin: TObjOrigin;
      const AKey: TEventWaypointDictKey;
      const ADTMark: TDateTime;
      const ADuration: Double;
      const APlanHit: Boolean;
      const AWorktimeHit: Boolean;
      const AFeasibleHit: Boolean;
      const AGUID: string;
      const ADepotGUID: string
    ); overload;
    constructor Create(
      const AOrigin: TObjOrigin;
      const AVehicleID: Integer;
      const AWaypointID: Integer;
      const ADTMark: TDateTime;
      const ADuration: Double;
      const APlanHit: Boolean;
      const AWorktimeHit: Boolean;
      const AFeasibleHit: Boolean;
      const AGUID: string;
      const ADepotGUID: string
    ); overload;
    function Clone(): TCacheDataObjectAbstract; override;
    property Key: TEventWaypointDictKey read FKey;
    property Duration: Double read FDuration write SetDuration;
    property Till: TDateTime read FTill write SetTill;
    property PlanHit: Boolean read FPlanHit write SetPlanHit;
    property WorktimeHit: Boolean read FWorktimeHit write SetWorktimeHit;
    property FeasibleHit: Boolean read FFeasibleHit write SetFeasibleHit;
    property GUID: string read FGUID;
    property DepotGUID: string read FDepotGUID;
    property Changed: TDateTime read FChanged;
    property Stored: TDateTime read FStored write FStored;
    property Origin: TObjOrigin read FOrigin write FOrigin;
  end;

//------------------------------------------------------------------------------
implementation

//------------------------------------------------------------------------------
// TEventGeo
//------------------------------------------------------------------------------

constructor TEventGeo.Create(
  ASource: TEventGeo
);
begin
  inherited Create();
  //
  FChanged := ASource.FChanged;
  FStored := ASource.FStored;
  FOrigin := ASource.FOrigin;
  FKey := ASource.FKey;
  FDTMark := ASource.FDTMark;
  FTill := ASource.FTill;
  FDuration := ASource.FDuration;
  FTriggerID := ASource.FTriggerID;
end;

constructor TEventGeo.Create(
  const AOrigin: TObjOrigin;
  const AKey: TEventGeoDictKey;
  const ADTMark: TDateTime;
  const ATriggerID: Integer;
  const ADuration: Double
);
begin
  inherited Create();
  //
  FChanged := Now();
  FStored := FChanged;
  FOrigin := AOrigin;
  FKey := AKey;
  FDTMark := ADTMark;
  FTill := ADTMark + ADuration;
  FDuration := ADuration;
  FTriggerID := ATriggerID;
end;

constructor TEventGeo.Create(
  const AOrigin: TObjOrigin;
  const AIMEI: TIMEI;
  const AZoneID: Integer;
  const ADTMark: TDateTime;
  const ATriggerID: Integer;
  const ADuration: Double
);
begin
  inherited Create();
  //
  FChanged := Now();
  FStored := FChanged;
  FOrigin := AOrigin;
  FKey.IMEI := AIMEI;
  FKey.ZoneID := AZoneID;
  FDTMark := ADTMark;
  FTill := ADTMark + ADuration;
  FDuration := ADuration;
  FTriggerID := ATriggerID;
end;

function TEventGeo.Clone(): TCacheDataObjectAbstract;
begin
  Result := TEventGeo.Create(Self);
end;

function TEventGeo.GetDTMark(): TDateTime;
begin
  Result := FDTMark;
end;

procedure TEventGeo.SetDuration(
  const Value: Double
);
begin
  if (FDuration <> Value) then
  begin
    FDuration := Value;
    FTill := FDTMark + Value;
    FChanged := Now();
  end;
end;

procedure TEventGeo.SetTill(
  const Value: TDateTime
);
begin
  if (FTill <> Value) then
  begin
    FTill := Value;
    FDuration := FDTMark - Value;
    FChanged := Now();
  end;
end;

//------------------------------------------------------------------------------
// TEventMove
//------------------------------------------------------------------------------

constructor TEventMove.Create(
  ASource: TEventMove
);
begin
  inherited Create();
  //
  FChanged := ASource.FChanged;
  FStored := ASource.FStored;
  FOrigin := ASource.FOrigin;
  FKey := ASource.FKey;
  FDTMark := ASource.FDTMark;
  FTill := ASource.FTill;
  FDuration := ASource.FDuration;
  FEventType := ASource.FEventType;
  FGeoPos := ASource.FGeoPos;
end;

constructor TEventMove.Create(
  const AOrigin: TObjOrigin;
  const AKey: TIMEI;
  const ADTMark: TDateTime;
  const ADuration: Double;
  const AEventType: TEventSystemType;
  const AGeoPos: TGeoPos
);
begin
  inherited Create();
  //
  FChanged := Now();
  FStored := FChanged;
  FOrigin := AOrigin;
  FKey := AKey;
  FDTMark := ADTMark;
  FTill := ADTMark + ADuration;
  FDuration := ADuration;
  FEventType := AEventType;
  FGeoPos := AGeoPos;
end;

function TEventMove.Clone(): TCacheDataObjectAbstract;
begin
  Result := TEventMove.Create(Self);
end;

function TEventMove.GetDTMark(): TDateTime;
begin
  Result := FDTMark;
end;

procedure TEventMove.SetDuration(
  const Value: Double
);
begin
  if (FDuration <> Value) then
  begin
    FDuration := Value;
    FTill := FDTMark + Value;
    FChanged := Now();
  end;
end;

procedure TEventMove.SetTill(
  const Value: TDateTime
);
begin
  if (FTill <> Value) then
  begin
    FTill := Value;
    FDuration := FDTMark - Value;
    FChanged := Now();
  end;
end;

procedure TEventMove.SetEventType(
  const Value: TEventSystemType
);
begin
  if (FEventType <> Value) then
  begin
    FEventType := Value;
    FChanged := Now();
  end;
end;

procedure TEventMove.SetGeoPos(
  const Value: TGeoPos
);
begin
  if not (FGeoPos = Value) then
  begin
    FGeoPos := Value;
    FChanged := Now();
  end;
end;

//------------------------------------------------------------------------------
// TEventDepot
//------------------------------------------------------------------------------

constructor TEventDepot.Create(
  ASource: TEventDepot
);
begin
  inherited Create();
  //
  FChanged := ASource.FChanged;
  FStored := ASource.FStored;
  FOrigin := ASource.FOrigin;
  FKey := ASource.FKey;
  FDTMark := ASource.FDTMark;
  FTill := ASource.FTill;
  FDuration := ASource.FDuration;
  FGUID := ASource.FGUID;
end;

constructor TEventDepot.Create(
  const AOrigin: TObjOrigin;
  const AKey: TEventDepotDictKey;
  const ADTMark: TDateTime;
  const ADuration: Double;
  const AGUID: string
);
begin
  inherited Create();
  //
  FChanged := Now();
  FStored := FChanged;
  FOrigin := AOrigin;
  FKey := AKey;
  FDTMark := ADTMark;
  FTill := ADTMark + ADuration;
  FDuration := ADuration;
  FGUID := AGUID;
end;

constructor TEventDepot.Create(
  const AOrigin: TObjOrigin;
  const AIMEI: TIMEI;
  const ADepotID: Integer;
  const ADTMark: TDateTime;
  const ADuration: Double;
  const AGUID: string
);
begin
  inherited Create();
  //
  FChanged := Now();
  FStored := FChanged;
  FOrigin := AOrigin;
  FKey.IMEI := AIMEI;
  FKey.DepotID := ADepotID;
  FDTMark := ADTMark;
  FTill := ADTMark + ADuration;
  FDuration := ADuration;
  FGUID := AGUID;
end;

function TEventDepot.Clone(): TCacheDataObjectAbstract;
begin
  Result := TEventDepot.Create(Self);
end;

function TEventDepot.GetDTMark(): TDateTime;
begin
  Result := FDTMark;
end;

procedure TEventDepot.SetDuration(
  const Value: Double
);
begin
  if (FDuration <> Value) then
  begin
    FDuration := Value;
    FTill := FDTMark + Value;
    FChanged := Now();
  end;
end;

procedure TEventDepot.SetTill(
  const Value: TDateTime
);
begin
  if (FTill <> Value) then
  begin
    FTill := Value;
    FDuration := FDTMark - Value;
    FChanged := Now();
  end;
end;

//------------------------------------------------------------------------------
// TEventWaypoint
//------------------------------------------------------------------------------

constructor TEventWaypoint.Create(
  ASource: TEventWaypoint
);
begin
  inherited Create();
  //
  FChanged := ASource.FChanged;
  FStored := ASource.FStored;
  FOrigin := ASource.FOrigin;
  FKey := ASource.FKey;
  FDTMark := ASource.FDTMark;
  FDuration := ASource.FDuration;
  FTill := ASource.FTill;
  FPlanHit := ASource.FPlanHit;
  FWorktimeHit := ASource.FWorktimeHit;
  FFeasibleHit := ASource.FFeasibleHit;
  FGUID := ASource.FGUID;
  FDepotGUID := ASource.FDepotGUID;
end;

constructor TEventWaypoint.Create(
  const AOrigin: TObjOrigin;
  const AKey: TEventWaypointDictKey;
  const ADTMark: TDateTime;
  const ADuration: Double;
  const APlanHit: Boolean;
  const AWorktimeHit: Boolean;
  const AFeasibleHit: Boolean;
  const AGUID: string;
  const ADepotGUID: string
);
begin
  inherited Create();
  //
  FChanged := Now();
  FStored := FChanged;
  FOrigin := AOrigin;
  FKey := AKey;
  FDTMark := ADTMark;
  FDuration := ADuration;
  FTill := ADTMark + ADuration;
  FPlanHit := APlanHit;
  FWorktimeHit := AWorktimeHit;
  FFeasibleHit := AFeasibleHit;
  FGUID := AGUID;
  FDepotGUID := ADepotGUID;
end;

constructor TEventWaypoint.Create(
  const AOrigin: TObjOrigin;
  const AVehicleID: Integer;
  const AWaypointID: Integer;
  const ADTMark: TDateTime;
  const ADuration: Double;
  const APlanHit: Boolean;
  const AWorktimeHit: Boolean;
  const AFeasibleHit: Boolean;
  const AGUID: string;
  const ADepotGUID: string
);
begin
  inherited Create();
  //
  FChanged := Now();
  FStored := FChanged;
  FOrigin := AOrigin;
  FKey.VehicleID := AVehicleID;
  FKey.WaypointID := AWaypointID;
  FDTMark := ADTMark;
  FDuration := ADuration;
  FTill := ADTMark + ADuration;
  FPlanHit := APlanHit;
  FWorktimeHit := AWorktimeHit;
  FFeasibleHit := AFeasibleHit;
  FGUID := AGUID;
  FDepotGUID := ADepotGUID;
end;

function TEventWaypoint.Clone(): TCacheDataObjectAbstract;
begin
  Result := TEventWaypoint.Create(Self);
end;

function TEventWaypoint.GetDTMark(): TDateTime;
begin
  Result := FDTMark;
end;

procedure TEventWaypoint.SetDuration(
  const Value: Double
);
begin
  if (FDuration <> Value) then
  begin
    FDuration := Value;
    FTill := FDTMark + Value;
    FChanged := Now();
  end;
end;

procedure TEventWaypoint.SetTill(
  const Value: TDateTime
);
begin
  if (FTill <> Value) then
  begin
    FTill := Value;
    FDuration := FDTMark - Value;
    FChanged := Now();
  end;
end;

procedure TEventWaypoint.SetPlanHit(
  const Value: Boolean
);
begin
  if (FPlanHit <> Value) then
  begin
    FPlanHit := Value;
    FChanged := Now();
  end;
end;

procedure TEventWaypoint.SetWorktimeHit(
  const Value: Boolean
);
begin
  if (FWorktimeHit <> Value) then
  begin
    FWorktimeHit := Value;
    FChanged := Now();
  end;
end;

procedure TEventWaypoint.SetFeasibleHit(
  const Value: Boolean
);
begin
  if (FFeasibleHit <> Value) then
  begin
    FFeasibleHit := Value;
    FChanged := Now();
  end;
end;

end.

