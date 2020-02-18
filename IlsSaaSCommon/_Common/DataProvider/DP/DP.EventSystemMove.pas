unit DP.EventSystemMove;
//------------------------------------------------------------------------------
// модуль кэша таблицы eventsystem_move
//------------------------------------------------------------------------------
// содержит:
//------------------------------------------------------------------------------

//------------------------------------------------------------------------------
interface

uses
  SysUtils,
  System.Generics.Collections,
  DP.Root,
  JsonDataObjects,
  ZConnection, ZDataset,
  Geo.Pos, Event.Cnst, Ils.MySql.Conf, Ils.Utils, Ils.Logger;

//------------------------------------------------------------------------------
type

//------------------------------------------------------------------------------
//! класс данных событий движения
//------------------------------------------------------------------------------
  TClassEventSystemMove = class(TDataObj)
  private
    //! свойства изменены
    FChanged: Boolean;
    //! IMEI
    FIMEI: Int64;
    //! с какого времени
    FDTMark: TDateTime;
    //! длительность
    FDuration: Double;
    //! до какого времени
    FTill: TDateTime;
    //! гео-координаты
    FGeoPos: TGeoPos;
    //! радиус чего-то там
    FRadius: Double;
    //! разброс чего-то там
    FDispertion: Double;
    //! тип события
    FEventType: TEventSystemType;
    //!
    procedure SetDuration(const Value: Double);
    procedure SetTill(const Value: TDateTime);
    procedure SetLatitude(const Value: Double);
    procedure SetLongitude(const Value: Double);
    procedure SetRadius(const Value: Double);
    procedure SetDispertion(const Value: Double);
    procedure SetEventType(const Value: TEventSystemType);
  protected
    //!
    function GetDTMark(): TDateTime; override;
  public
    class function CreateFromJSON(
      const AJSON: TJsonObject;
      out RObject: TClassEventSystemMove
    ): Boolean;
    constructor Create(
      const IMEI: Int64;
      const DTMark: TDateTime;
      const EventType: TEventSystemType;
      const Duration: Double;
      const Latitude: Double;
      const Longitude: Double;
      const Radius: Double;
      const Dispertion: Double
    );
    //!
    property IMEI: Int64 read FIMEI;
    property Duration: Double read FDuration write SetDuration;
    property Till: TDateTime read FTill write SetTill;
    property Latitude: Double read FGeoPos.Latitude write SetLatitude;
    property Longitude: Double read FGeoPos.Longitude write SetLongitude;
    property Radius: Double read FRadius write SetRadius;
    property Dispertion: Double read FDispertion write SetDispertion;
    property EventType: TEventSystemType read FEventType write SetEventType;
  end;

//------------------------------------------------------------------------------
//! класс кэша данных событий движения
//------------------------------------------------------------------------------
  TCacheEventSystemMove = class(TCacheRoot)
  protected
    //! вставить запись в БД
    procedure ExecDBInsert(
      const AObj: IDataObj
    ); override;
    //! обновить запись в БД
    procedure ExecDBUpdate(
      const AObj: IDataObj
    ); override;
    //! преобразователь из записи БД в объект
    function MakeObjFromReadReq(
      const AQuery: TZQuery
    ): IDataObj; override;
  public
    constructor Create(
      const IMEI: Int64;
      const ReadConnection: TZConnection;
      const WriteConnection: TZConnection;
      const DBWriteback: Boolean;
      const MaxKeepCount: Integer;
      const LoadDelta: Double
    );
  end;

//------------------------------------------------------------------------------
//! класс списка кэшей событий движения
//------------------------------------------------------------------------------
  TEventSystemMoveDictionary = class(TCacheDictionaryRoot<Int64>);

//------------------------------------------------------------------------------
implementation

const

//------------------------------------------------------------------------------
// запросы к БД
//------------------------------------------------------------------------------
  CSQLReadRange = 'SELECT *'
    + ' FROM eventsystem_move'
    + ' WHERE imei = :imei'
    + ' AND dt >= :dt_from'
    + ' AND dt <= :dt_to';
  CSQLReadBefore = 'SELECT *'
    + ' FROM eventsystem_move'
    + ' WHERE IMEI = :imei'
    + ' AND DT < :dt'
    + ' ORDER BY DT DESC'
    + ' LIMIT 1';
  CSQLReadAfter = 'SELECT *'
    + ' FROM eventsystem_move'
    + ' WHERE IMEI = :imei'
    + ' AND DT > :dt'
    + ' ORDER BY DT'
    + ' LIMIT 1';
  CSQLInsert = 'INSERT INTO eventsystem_move'
    + ' (IMEI, DT, EventTypeID, Duration, Latitude, Longitude, Radius, Dispersion)'
    + ' VALUES (:imei, :dt, :event_type, :dur, :la, :lo, :radius, :dispersion)';
  CSQLUpdate = 'UPDATE eventsystem_move SET'
    + ' EventTypeID = :event_type, Duration = :dur, Latitude = :la, Longitude = :lo, Radius = :radius, Dispersion = :dispersion'
    + ' WHERE IMEI = :imei'
    + ' AND DT = :dt';
  CSQLDeleteRange = 'DELETE FROM eventsystem_move'
    + ' WHERE imei = :imei'
    + ' AND dt >= :dt_from'
    + ' AND dt <= :dt_to';
  CSQLLastPresentDT = 'SELECT MAX(DT) AS DT'
    + ' FROM eventsystem_move'
    + ' WHERE imei = :imei';

//------------------------------------------------------------------------------
// TSystemMoveEventClass
//------------------------------------------------------------------------------

class function TClassEventSystemMove.CreateFromJSON(
  const AJSON: TJsonObject;
  out RObject: TClassEventSystemMove
): Boolean;
begin
  Result := False;
  try
    RObject := TClassEventSystemMove.Create(
      AJSON.L['IMEI'],
      IlsToDateTime(AJSON.S['DT']),
      TEventSystemType(AJSON.I['EventTypeID']),
      AJSON.F['Duration'],
      AJSON.F['la'],
      AJSON.F['lo'],
      AJSON.F['Radius'],
      AJSON.F['Dispersion']
    );
    Result := True;
  except
    on Ex: Exception do
    begin
      ToLog(Format(
        'При создании класса TClassEventSystemMove из JSON'#13#10'%s'#13#10'возникла ошибка:'#13#10'%s',
        [AJSON.ToJSON(True), Ex.Message]
      ));
    end;
  end;
end;

constructor TClassEventSystemMove.Create(
  const IMEI: Int64;
  const DTMark: TDateTime;
  const EventType: TEventSystemType;
  const Duration: Double;
  const Latitude: Double;
  const Longitude: Double;
  const Radius: Double;
  const Dispertion: Double
);
begin
  inherited Create();
  //
  FIMEI := IMEI;
  FDTMark := DTMark;
  FDuration := Duration;
  FTill := DTMark + Duration;
  FGeoPos.Latitude := Latitude;
  FGeoPos.Longitude := Longitude;
  FRadius := Radius;
  FDispertion := Dispertion;
  FEventType := EventType;
end;

function TClassEventSystemMove.GetDTMark(): TDateTime;
begin
  Result := FDTMark;
end;

procedure TClassEventSystemMove.SetDuration(
  const Value: Double
);
begin
  if (FDuration <> Value) then
  begin
    FDuration := Value;
    FTill := Value + FDTMark;
    FChanged := True;
  end;
end;

procedure TClassEventSystemMove.SetTill(
  const Value: TDateTime
);
begin
  if (FTill <> Value) then
  begin
    FTill := Value;
    FDuration := Value - FDTMark;
    FChanged := True;
  end;
end;

procedure TClassEventSystemMove.SetLatitude(
  const Value: Double
);
begin
  if (FGeoPos.Latitude <> Value) then
  begin
    FGeoPos.Latitude := Value;
    FChanged := True;
  end;
end;

procedure TClassEventSystemMove.SetLongitude(
  const Value: Double
);
begin
  if (FGeoPos.Longitude <> Value) then
  begin
    FGeoPos.Longitude := Value;
    FChanged := True;
  end;
end;

procedure TClassEventSystemMove.SetRadius(
  const Value: Double
);
begin
  if (FRadius <> Value) then
  begin
    FRadius := Value;
    FChanged := True;
  end;
end;

procedure TClassEventSystemMove.SetDispertion(
  const Value: Double
);
begin
  if (FDispertion <> Value) then
  begin
    FDispertion := Value;
    FChanged := True;
  end;
end;

procedure TClassEventSystemMove.SetEventType(
  const Value: TEventSystemType
);
begin
  if (FEventType <> Value) then
  begin
    FEventType := Value;
    FChanged := True;
  end;
end;

//------------------------------------------------------------------------------
// TCacheEventSystemMove
//------------------------------------------------------------------------------

constructor TCacheEventSystemMove.Create(
  const IMEI: Int64;
  const ReadConnection: TZConnection;
  const WriteConnection: TZConnection;
  const DBWriteback: Boolean;
  const MaxKeepCount: Integer;
  const LoadDelta: Double
);
begin
  inherited Create(
    ReadConnection, WriteConnection, DBWriteback, MaxKeepCount, LoadDelta,
    CSQLReadRange, CSQLReadBefore, CSQLReadAfter, CSQLInsert, CSQLUpdate, CSQLDeleteRange, CSQLLastPresentDT
  );
  //
  FQueryReadRange.ParamByName('imei').AsLargeInt := IMEI;
  FQueryReadBefore.ParamByName('imei').AsLargeInt := IMEI;
  FQueryReadAfter.ParamByName('imei').AsLargeInt := IMEI;
  FQueryInsert.ParamByName('imei').AsLargeInt := IMEI;
  FQueryUpdate.ParamByName('imei').AsLargeInt := IMEI;
  FQueryDeleteRange.ParamByName('imei').AsLargeInt := IMEI;
  FQueryLastPresentDT.ParamByName('imei').AsLargeInt := IMEI;
end;

procedure TCacheEventSystemMove.ExecDBInsert(
  const AObj: IDataObj
);
begin
  with (AObj as TClassEventSystemMove), FQueryInsert do
  begin
    Active := False;
    ParamByName('dt').AsFloat := DTMark;
    ParamByName('event_type').AsFloat := Ord(EventType);
    ParamByName('dur').AsFloat := Duration;
    ParamByName('la').AsFloat := Latitude;
    ParamByName('lo').AsFloat := Longitude;
    ParamByName('radius').AsFloat := Radius;
    ParamByName('dispersion').AsFloat := Dispertion;
    ExecSQL();
  end;
end;

procedure TCacheEventSystemMove.ExecDBUpdate(
  const AObj: IDataObj
);
begin
  with (AObj as TClassEventSystemMove), FQueryUpdate do
  begin
    Active := False;
    ParamByName('dt').AsFloat := DTMark;
    ParamByName('event_type').AsFloat := Ord(EventType);
    ParamByName('dur').AsFloat := Duration;
    ParamByName('la').AsFloat := Latitude;
    ParamByName('lo').AsFloat := Longitude;
    ParamByName('radius').AsFloat := Radius;
    ParamByName('dispersion').AsFloat := Dispertion;
    ExecSQL();
  end;
end;

function TCacheEventSystemMove.MakeObjFromReadReq(
  const AQuery: TZQuery
): IDataObj;
begin
  with AQuery do
  begin
    Result := TClassEventSystemMove.Create(
      FieldByName('IMEI').AsLargeInt,
      FieldByName('DT').AsDateTime,
      TEventSystemType(FieldByName('EventTypeID').AsInteger),
      FieldByName('Duration').AsFloat,
      FieldByName('Latitude').AsFloat,
      FieldByName('Longitude').AsFloat,
      FieldByName('Radius').AsFloat,
      FieldByName('Dispersion').AsFloat
    );
  end;
end;

end.

