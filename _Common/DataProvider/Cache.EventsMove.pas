unit Cache.EventsMove;
//------------------------------------------------------------------------------
//
//------------------------------------------------------------------------------

//------------------------------------------------------------------------------
interface

uses
  SysUtils,
  Cache.Root,
  Adapter.MySQL,
  Geo.Pos,
  ZConnection, ZDataset,
  Ils.MySql.Conf,
  Event.Cnst;
//  ,Ils.Logger,
//  Ils.Utils;

//------------------------------------------------------------------------------
type

  TProcessedEvent = procedure(const AObj: TCacheDataObjectAbstract; const AUpdate: Boolean) of object;

//------------------------------------------------------------------------------
//!
//------------------------------------------------------------------------------
  TSystemMoveEvent = class(TCacheDataObjectAbstract)
  private
    FIMEI: Int64;
    FDTMark: TDateTime;
    FEventType: TEventSystemType;
    FDuration: TTime;
    FLatitude: Double;
    FLongitude: Double;
    FRadius: Double;
    FDispersion: Double;
    FOffset: Int64;
  protected
    //!
    function GetDTMark(): TDateTime; override;
  public
    constructor Create(
      const IMEI: Int64;
      const DTMark: TDateTime;
      const EventType: TEventSystemType;
      const Duration: TTime;
      const Latitude: Double;
      const Longitude: Double;
      const Radius: Double;
      const Dispersion: Double;
      const Offset: Int64
    ); overload;
    constructor Create(
      Source: TSystemMoveEvent
    ); overload;
    function Clone(): TCacheDataObjectAbstract; override;

    property IMEI: Int64 read FIMEI;
    property DTMark: TDateTime read GetDTMark;
    property EventType: TEventSystemType read FEventType;
    property Duration: TTime read FDuration write FDuration;
    property Latitude: Double read FLatitude;
    property Longitude: Double read FLongitude;
    property Radius: Double read FRadius;
    property Dispersion: Double read FDispersion;
    property Offset: Int64 read FOffset;
  end;

//------------------------------------------------------------------------------
//!
//------------------------------------------------------------------------------
  TSystemMoveEventCacheDataAdapter = class(TCacheAdapterMySQL)
//  TTrackPointCacheDataAdapter = class(TCacheAdapterMySQL)
  private
    FProcessedEvent: TProcessedEvent;
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
    constructor Create(
      const AReadConnection, AWriteConnection: TZConnection;
      const AProcessedEvent: TProcessedEvent
    );
  end;


//------------------------------------------------------------------------------
//! класс списка кэшей точек трека
//------------------------------------------------------------------------------
  TTrackPointCacheDictionary = TCacheDictionary<Int64>;

//------------------------------------------------------------------------------
implementation

//------------------------------------------------------------------------------
//
//------------------------------------------------------------------------------

constructor TSystemMoveEvent.Create(
  const IMEI: Int64;
  const DTMark: TDateTime;
  const EventType: TEventSystemType;
  const Duration: TTime;
  const Latitude: Double;
  const Longitude: Double;
  const Radius: Double;
  const Dispersion: Double;
  const Offset: Int64
);
begin
  inherited Create();
  //
  FIMEI := IMEI;
  FDTMark := DTMark;
  FEventType := EventType;
  FDuration := Duration;
  FLatitude := Latitude;
  FLongitude := Longitude;
  FRadius := Radius;
  FDispersion := Dispersion;
  FOffset := Offset;
end;

constructor TSystemMoveEvent.Create(
  Source: TSystemMoveEvent
);
begin
  inherited Create();
  //
  FIMEI := Source.IMEI;
  FDTMark := Source.DTMark;
  FEventType := Source.EventType;
  FDuration := Source.Duration;
  FLatitude := Source.Latitude;
  FLongitude := Source.Longitude;
  FRadius := Source.Radius;
  FDispersion := Source.Dispersion;
  FOffset := Source.Offset;
end;

function TSystemMoveEvent.Clone(): TCacheDataObjectAbstract;
begin
  Result := TSystemMoveEvent.Create(Self);
end;

function TSystemMoveEvent.GetDTMark(): TDateTime;
begin
  Result := FDTMark;
end;


{ TSystemMoveEventCacheDataAdapter }

constructor TSystemMoveEventCacheDataAdapter.Create(
  const AReadConnection, AWriteConnection: TZConnection;
  const AProcessedEvent: TProcessedEvent
);
const
  SQLReadRange =
      'SELECT *'
    + ' FROM eventsystem_move'
    + ' WHERE IMEI = :imei'
    + ' AND DT >= :dt_from'
    + ' AND DT <= :dt_to'
    ;
  SQLReadBefore =
      'SELECT MAX(DT) as DT'
    + ' FROM eventsystem_move'
    + ' WHERE IMEI = :imei'
    + ' AND DT < :dt'
    ;
  SQLReadAfter =
      'SELECT MIN(DT) as DT'
    + ' FROM  eventsystem_move'
    + ' WHERE IMEI = :imei'
    + ' AND DT > :dt'
    ;
  SQLInsert =
      'insert ignore into eventsystem_move set'
    + ' IMEI = :IMEI,'
    + ' DT = :DT,'
    + ' EventTypeID = :EventTypeID,'
    + ' Duration = :Duration,'
    + ' Latitude = :Latitude,'
    + ' Longitude = :Longitude,'
    + ' Radius = :Radius,'
    + ' Dispersion = :Dispersion'
    ;
  SQLUpdate =
    'update eventsystem_move set'
    + ' EventTypeID = :EventTypeID,'
    + ' Duration = :Duration,'
    + ' Latitude = :Latitude,'
    + ' Longitude = :Longitude,'
    + ' Radius = :Radius,'
    + ' Dispersion = :Dispersion'
    + ' where'
    + ' IMEI = :IMEI'
    + ' and DT = :DT'
    ;
  SQLDeleteOne =
    'delete from eventsystem_move'
    + ' where'
    + ' IMEI = :IMEI'
    + ' and DT = :DT'
    ;

begin
  FProcessedEvent := AProcessedEvent;

  inherited Create(
    AReadConnection, AWriteConnection,
    SQLInsert, SQLUpdate, SQLReadBefore, SQLReadAfter, SQLReadRange, SQLDeleteOne);
end;

procedure TSystemMoveEventCacheDataAdapter.Flush(const ACache: TCache);
begin
  //Nothing!
end;

procedure TSystemMoveEventCacheDataAdapter.Add(
  const ACache: TCache;
  const AObj: TCacheDataObjectAbstract
);
begin
  FillParamsFromKey(ACache.Key, FInsertQry);
  with (AObj as TSystemMoveEvent), FInsertQry do
  begin
    ParamByName('DT').AsDateTime := FDTMark;
    ParamByName('EventTypeID').AsInteger := Ord(FEventType);
    ParamByName('Duration').AsFloat := FDuration;
    ParamByName('Latitude').AsFloat := FLatitude;
    ParamByName('Longitude').AsFloat := FLongitude;
    ParamByName('Radius').AsFloat := FRadius;
    ParamByName('Dispersion').AsFloat := FDispersion;

    ExecSQL;

    try
      if Assigned(FProcessedEvent) then
        FProcessedEvent(AObj, False);
    except
      on E: Exception do begin
      end;
    end;
  end;

end;

procedure TSystemMoveEventCacheDataAdapter.Change(
  const ACache: TCache;
  const AObj: TCacheDataObjectAbstract
);
begin
  FillParamsFromKey(ACache.Key, FUpdateQry);
  with (AObj as TSystemMoveEvent), FUpdateQry do
  begin
    ParamByName('DT').AsDateTime := FDTMark;
    ParamByName('EventTypeID').AsInteger := Ord(FEventType);
    ParamByName('Duration').AsFloat := FDuration;
    ParamByName('Latitude').AsFloat := FLatitude;
    ParamByName('Longitude').AsFloat := FLongitude;
    ParamByName('Radius').AsFloat := FRadius;
    ParamByName('Dispersion').AsFloat := FDispersion;

    ExecSQL;
    try
      if Assigned(FProcessedEvent) then
        FProcessedEvent(AObj, True);
    except
      on E: Exception do begin
      end;
    end;
  end;

end;

function TSystemMoveEventCacheDataAdapter.MakeObjFromReadQuery(
  const AQuery: TZQuery
): TCacheDataObjectAbstract;
begin
  with AQuery do
  begin
    Result := TSystemMoveEvent.Create(
      FieldByName('IMEI').AsLargeInt,
      FieldByName('DT').AsDateTime,
      TEventSystemType(FieldByName('EventTypeID').AsInteger),
      FieldByName('Duration').AsFloat,
      FieldByName('Latitude').AsFloat,
      FieldByName('Longitude').AsFloat,
      FieldByName('Radius').AsFloat,
      FieldByName('Dispersion').AsFloat,
      0
    );
  end;
end;

end.

