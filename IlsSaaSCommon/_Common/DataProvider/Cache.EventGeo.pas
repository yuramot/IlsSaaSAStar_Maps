unit Cache.EventGeo;
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------

//------------------------------------------------------------------------------
interface

uses
  SysUtils,
  Cache.Root,
  Adapter.MySQL,
  ZConnection, ZDataset,
  Event.Cnst;

//------------------------------------------------------------------------------
type

  TGeoEventType = (etDepot, etZone, etTrip, etWaypoint);


//------------------------------------------------------------------------------
//! ключ списка кэшей гео-событий
//------------------------------------------------------------------------------
  TEventGeoKey = packed record
    IMEI: Int64;
    ID: Integer;
//    EventType: TGeoEventType;
  end;

//------------------------------------------------------------------------------
//! класс данных гео-событий
//------------------------------------------------------------------------------
  TEventGeo = class(TCacheDataObjectAbstract)
  private
    FKey: TEventGeoKey;
    FDTMark: TDateTime;
    FEventType: TEventUserType;
    FDuration: Double;
    FTill: TDateTime;
    FChanged: Boolean;
    procedure SetDuration(const Value: Double);
    procedure SetTill(const Value: TDateTime);
  protected
    //!
    function GetDTMark(): TDateTime; override;
  public
    constructor Create(
      const AIMEI: Int64;
      const ADTMark: TDateTime;
      const AEventType: TEventUserType;
      const AID: Integer;
      const ADuration: Double
    ); overload;
    constructor Create(
      const AKey: TEventGeoKey;
      const ADTMark: TDateTime;
      const AEventType: TEventUserType;
      const ADuration: Double
    ); overload;
    constructor Create(
      ASource: TEventGeo
    ); overload;
    function Clone(): TCacheDataObjectAbstract; override;
    procedure ClearChanged();
    property Key: TEventGeoKey read FKey;
    property IMEI: Int64 read FKey.IMEI;
    property ID: Integer read FKey.ID;
    property EventType: TEventUserType read FEventType;
    property Duration: Double read FDuration write SetDuration;
    property Till: TDateTime read FTill write SetTill;
    property Changed: Boolean read FChanged;
  end;

//------------------------------------------------------------------------------
//! класс кэша гео-событий
//------------------------------------------------------------------------------
  TEventGeoCacheDataAdapter = class(TCacheAdapterMySQL)
  private
    FTableName: string;
    FIdFieldName: string;
    FGeoCacheType: TGeoEventType;
    FCheckWaypointHasEventQuery: TZQuery;
  public
    procedure Flush(
      const ACache: TCache
    ); override;
    procedure Add(
      const ACache: TCache;
      const AObj: TCacheDataObjectAbstract
    ); override;
    procedure Change(
      const ACache: TCache;
      const AObj: TCacheDataObjectAbstract
    ); override;
    function MakeObjFromReadQuery(
      const AQuery: TZQuery
    ): TCacheDataObjectAbstract; override;
    function Exists(
      const AEventKey: TEventGeoKey;
      const AEventGeoType: TEventUserType
    ): Boolean;
    constructor Create(
      const AReadConnection, AWriteConnection: TZConnection;
      const AEventType: TGeoEventType
    );
  end;

//------------------------------------------------------------------------------
//! класс списка кэшей гео-событий
//------------------------------------------------------------------------------
  TEventGeoHash = TCacheDictionary<TEventGeoKey>;

//------------------------------------------------------------------------------
implementation

//------------------------------------------------------------------------------
// TEventGeo
//------------------------------------------------------------------------------

constructor TEventGeo.Create(
  const AIMEI: Int64;
  const ADTMark: TDateTime;
  const AEventType: TEventUserType;
  const AID: Integer;
  const ADuration: Double
);

  function MakeGeoEventType(AEventType: TEventUserType): TGeoEventType;
  begin
    Result := etDepot;
    case AEventType of
      egzEnterZone,
      egzLeaveZone: Result := etZone;
    end;
  end;

begin
  inherited Create();
  //
  FKey.IMEI := AIMEI;
  FKey.ID := AID;
  FDTMark := ADTMark;
  FEventType := AEventType;
  FDuration := ADuration;
  FTill := ADuration + ADTMark;
end;

constructor TEventGeo.Create(
  const AKey: TEventGeoKey;
  const ADTMark: TDateTime;
  const AEventType: TEventUserType;
  const ADuration: Double
);
begin
  inherited Create();
  //
  FKey := AKey;
  FDTMark := ADTMark;
  FEventType := AEventType;
  FDuration := ADuration;
  FTill := ADuration + ADTMark;
end;

constructor TEventGeo.Create(
  ASource: TEventGeo
);
begin
  inherited Create();
  //
  FKey := ASource.Key;
  FDTMark := ASource.DTMark;
  FEventType := ASource.EventType;
  FDuration := ASource.Duration;
  FTill := ASource.Till;
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
    FTill := Value + DTMark;
    FChanged := True;
  end;
end;

procedure TEventGeo.SetTill(
  const Value: TDateTime
);
begin
  if (FTill <> Value) then
  begin
    FTill := Value;
    FDuration := Value - DTMark;
    FChanged := True;
  end;
end;

procedure TEventGeo.ClearChanged();
begin
  FChanged := False;
end;


{ TEventGeoCacheDataAdapter }

constructor TEventGeoCacheDataAdapter.Create(const AReadConnection,
  AWriteConnection: TZConnection; const AEventType: TGeoEventType);
var
  InsertSQL: string;
  UpdateSQL: string;
  ReadBeforeSQL: string;
  ReadAfterSQL: string;
  ReadRangeSQL: string;
  DeleteOneSQL: string;
  CheckWaypointHasEventSQL: string;
begin
  case AEventType of
    etDepot: begin
      FTableName := 'event_geodepot';
      FIdFieldName := 'DepotID';
    end;
    etZone: begin
      FTableName := 'event_geozone';
      FIdFieldName := 'ZoneID';
    end;
    etTrip: begin
      FTableName := 'event_geotrip';
      FIdFieldName := 'TripID';
    end;
    etWaypoint: begin
      FTableName := 'event_geowaypoint';
      FIdFieldName := 'WaypointID';
    end;
    else
      raise Exception.Create('unknown adapter type!');
  end;

  FGeoCacheType := AEventType;

  ReadRangeSQL :=
    'SELECT *'
    + ' FROM ' + FTableName
    + ' WHERE IMEI = :imei'
    + ' AND ' + FIdFieldName + ' = :id'
    + ' AND DT >= :dt_from'
    + ' AND DT <= :dt_to';
  ReadBeforeSQL :=
    'SELECT MAX(DT) as DT'
    + ' FROM ' + FTableName
    + ' WHERE IMEI = :imei'
    + ' AND ' + FIdFieldName + ' = :id'
    + ' AND DT < :dt';
  ReadAfterSQL :=
    'SELECT MIN(DT) as DT'
    + ' FROM ' + FTableName
    + ' WHERE IMEI = :imei'
    + ' AND ' + FIdFieldName + ' = :id'
    + ' AND DT > :dt';
  InsertSQL :=
    'INSERT IGNORE INTO ' + FTableName
    + ' (IMEI, DT, ' + FIdFieldName + ', EventTypeID, Duration)'
    + ' VALUES (:imei, :dt, :id, :t_id, :dur)'
    + ' ON DUPLICATE KEY UPDATE Duration = :dur';
  UpdateSQL :=
    'UPDATE ' + FTableName
    + ' SET Duration = :dur'
    + ' ,EventTypeID = :t_id'
    + ' WHERE IMEI = :imei'
    + ' AND ' + FIdFieldName + ' = :id'
    + ' AND DT = :dt';
  DeleteOneSQL :=
    'DELETE FROM ' + FTableName
    + ' WHERE IMEI = :imei'
    + ' AND ' + FIdFieldName + ' = :id'
    + ' AND DT = :dt';
  inherited Create(
    AReadConnection, AWriteConnection,
    InsertSQL, UpdateSQL, ReadBeforeSQL, ReadAfterSQL, ReadRangeSQL, DeleteOneSQL);

  CheckWaypointHasEventSQL :=
    'SELECT count(1) as CNT'
    + ' FROM ' + FTableName
    + ' WHERE IMEI = :imei'
    + ' AND ' + FIdFieldName + ' = :id'
    + ' AND EventTypeID = :EventTypeID'
  ;
  FCheckWaypointHasEventQuery := TZQuery.Create(nil);
  FCheckWaypointHasEventQuery.Connection := AWriteConnection;
  FCheckWaypointHasEventQuery.SQL.Text := CheckWaypointHasEventSQL;
end;

procedure TEventGeoCacheDataAdapter.Flush(
  const ACache: TCache
);
begin
end;

procedure TEventGeoCacheDataAdapter.Add(
  const ACache: TCache;
  const AObj: TCacheDataObjectAbstract
);
begin
  with TEventGeo(AObj) do
  begin
    FillParamsFromKey(ACache.Key, FInsertQry);
    FInsertQry.ParamByName(CSQLParamDT).AsDateTime := DTMark;
    FInsertQry.ParamByName('dur').AsFloat := Duration;
    FInsertQry.ParamByName('t_id').AsInteger := Ord(EventType);
    FInsertQry.ExecSQL();
  end;
end;

procedure TEventGeoCacheDataAdapter.Change(
  const ACache: TCache;
  const AObj: TCacheDataObjectAbstract
);
begin
  with TEventGeo(AObj) do
  begin
    FillParamsFromKey(ACache.Key, FUpdateQry);
    FUpdateQry.ParamByName(CSQLParamDT).AsDateTime := DTMark;
    FUpdateQry.ParamByName('dur').AsFloat := Duration;
    FUpdateQry.ParamByName('t_id').AsInteger := Ord(EventType);
    FUpdateQry.ExecSQL();
  end;
end;

function TEventGeoCacheDataAdapter.Exists(
  const AEventKey: TEventGeoKey;
  const AEventGeoType: TEventUserType
): Boolean;
begin
  FCheckWaypointHasEventQuery.Close;
  FCheckWaypointHasEventQuery.ParamByName('imei').AsLargeInt := AEventKey.IMEI;
  FCheckWaypointHasEventQuery.ParamByName('id').AsInteger := AEventKey.ID;
  FCheckWaypointHasEventQuery.ParamByName('EventTypeID').AsInteger := Ord(AEventGeoType);
  FCheckWaypointHasEventQuery.Open;
  Result := FCheckWaypointHasEventQuery.FieldByName('CNT').AsInteger > 0;
  FCheckWaypointHasEventQuery.Close;
end;

function TEventGeoCacheDataAdapter.MakeObjFromReadQuery(
  const AQuery: TZQuery
): TCacheDataObjectAbstract;
begin
  with AQuery do
  begin
    Result := TEventGeo.Create(
      FieldByName('IMEI').AsLargeInt,
      FieldByName('DT').AsDateTime,
      TEventUserType(FieldByName('EventTypeID').AsInteger),
      FieldByName(FIdFieldName).AsInteger,
      FieldByName('Duration').AsFloat
    );
  end;
end;

end.

