unit DP.EventGeo;
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------

//------------------------------------------------------------------------------
interface

uses
  SysUtils,
  Cache.Root,
  ZConnection, ZDataset,
  Event.Cnst;

//------------------------------------------------------------------------------
type

  TGeoEventType = (etDepot, etZone);

//------------------------------------------------------------------------------
//! ключ списка кэшей гео-событий
//------------------------------------------------------------------------------
  TEventGeoKey = packed record
    IMEI: Int64;
    ID: Integer;
    EventType: TGeoEventType;
  end;

//------------------------------------------------------------------------------
//! класс данных гео-событий
//------------------------------------------------------------------------------
  TEventGeo = class(TDataObj)
  private
    FKey: TEventGeoKey;
    FDTMark: TDateTime;
    FEventType: Integer;
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
      const AEventType: Integer;
      const AID: Integer;
      const ADuration: Double
    ); overload;
    constructor Create(
      const AKey: TEventGeoKey;
      const ADTMark: TDateTime;
      const AEventType: Integer;
      const ADuration: Double
    ); overload;
    constructor Create(
      ASource: TEventGeo
    ); overload;
    function Clone(): IDataObj; override;
    procedure ClearChanged();
    property Key: TEventGeoKey read FKey;
    property IMEI: Int64 read FKey.IMEI;
    property ID: Integer read FKey.ID;
    property EventType: Integer read FEventType;
    property Duration: Double read FDuration write SetDuration;
    property Till: TDateTime read FTill write SetTill;
    property Changed: Boolean read FChanged;
  end;

//------------------------------------------------------------------------------
//! класс кэша гео-событий
//------------------------------------------------------------------------------
  TCacheEventGeo = class(TCacheRoot)
  protected
    FIdFieldName: string;
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
      const AKey: TEventGeoKey;
      const AReadConnection: TZConnection;
      const AWriteConnection: TZConnection;
      const ADBWriteback: Boolean;
      const AMaxKeepCount: Integer;
      const ALoadDelta: Double
    );
  end;

//------------------------------------------------------------------------------
//! класс списка кэшей гео-событий
//------------------------------------------------------------------------------
  TEventGeoHash = TCacheDictionaryRoot<TEventGeoKey>;

//------------------------------------------------------------------------------
implementation

//------------------------------------------------------------------------------
// TEventGeo
//------------------------------------------------------------------------------

constructor TEventGeo.Create(
  const AIMEI: Int64;
  const ADTMark: TDateTime;
  const AEventType: Integer;
  const AID: Integer;
//  const AZoneID: Integer;
//  const ADepotID: Integer;
  const ADuration: Double
);

  function MakeGeoEventType(AEventType: Integer): TGeoEventType;
  begin
    Result := etDepot;
    case AEventType of
      Ord(egzEnterZone),
      Ord(egzLeaveZone): Result := etZone;
    end;
  end;

begin
  inherited Create();
  //
  FKey.IMEI := AIMEI;
  FKey.ID := AID;
  FKey.EventType := MakeGeoEventType(AEventType);
  FDTMark := ADTMark;
  FEventType := AEventType;
  FDuration := ADuration;
  FTill := ADuration + ADTMark;
end;

constructor TEventGeo.Create(
  const AKey: TEventGeoKey;
  const ADTMark: TDateTime;
  const AEventType: Integer;
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

function TEventGeo.Clone(): IDataObj;
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

//------------------------------------------------------------------------------
// TCacheEventGeo
//------------------------------------------------------------------------------

constructor TCacheEventGeo.Create(
  const AKey: TEventGeoKey;
  const AReadConnection: TZConnection;
  const AWriteConnection: TZConnection;
  const ADBWriteback: Boolean;
  const AMaxKeepCount: Integer;
  const ALoadDelta: Double
);
var
  TableName: string;
  SQLReadRange: string;
  SQLReadBefore: string;
  SQLReadAfter: string;
  SQLInsert: string;
  SQLUpdate: string;
  SQLDeleteRange: string;
  SQLLastPresentDT: string;
begin
  case AKey.EventType of
    etDepot: begin
      TableName := 'event_geodepot';
      FIdFieldName := 'DepotID';
    end;
    etZone: begin
      TableName := 'event_geozone';
      FIdFieldName := 'ZoneID';
    end;
  end;

  SQLReadRange := 'SELECT *'
    + ' FROM ' + TableName
    + ' WHERE IMEI = :imei'
    + ' AND ' + FIdFieldName + ' = :id'
    + ' AND DT >= :dt_from'
    + ' AND DT <= :dt_to';
  SQLReadBefore := 'SELECT *'
    + ' FROM ' + TableName
    + ' WHERE IMEI = :imei'
    + ' AND ' + FIdFieldName + ' = :id'
    + ' AND DT < :dt'
    + ' ORDER BY DT DESC'
    + ' LIMIT 1';
  SQLReadAfter := 'SELECT *'
    + ' FROM ' + TableName
    + ' WHERE IMEI = :imei'
    + ' AND ' + FIdFieldName + ' = :id'
    + ' AND DT > :dt'
    + ' ORDER BY DT'
    + ' LIMIT 1';
  SQLInsert := 'INSERT INTO ' + TableName
    + ' (IMEI, DT, ' + FIdFieldName + ', EventGeoTypeID, Duration)'
    + ' VALUES (:imei, :dt, :id, :t_id, :dur)';
  SQLUpdate := 'UPDATE ' + TableName
    + ' SET Duration = :dur'
    + ' ,EventGeoTypeID = :t_id'
    + ' WHERE IMEI = :imei'
    + ' AND ' + FIdFieldName + ' = :id'
    + ' AND DT = :dt';
  SQLDeleteRange := 'DELETE FROM ' + TableName
    + ' WHERE IMEI = :imei'
    + ' AND ' + FIdFieldName + ' = :id'
    + ' AND DT >= :dt_from'
    + ' AND DT <= :dt_to';
  SQLLastPresentDT := 'SELECT MAX(DT) AS DT'
    + ' FROM ' + TableName
    + ' WHERE IMEI = :imei'
    + ' AND ' + FIdFieldName + ' = :id';

  inherited Create(
    AReadConnection, AWriteConnection, ADBWriteback, AMaxKeepCount, ALoadDelta,
    SQLReadRange, SQLReadBefore, SQLReadAfter, SQLInsert, SQLUpdate, SQLDeleteRange, SQLLastPresentDT
  );

  FQueryReadRange.ParamByName('imei').AsLargeInt := AKey.IMEI;
  FQueryReadBefore.ParamByName('imei').AsLargeInt := AKey.IMEI;
  FQueryReadAfter.ParamByName('imei').AsLargeInt := AKey.IMEI;
  FQueryInsert.ParamByName('imei').AsLargeInt := AKey.IMEI;
  FQueryUpdate.ParamByName('imei').AsLargeInt := AKey.IMEI;
  FQueryDeleteRange.ParamByName('imei').AsLargeInt := AKey.IMEI;
  FQueryLastPresentDT.ParamByName('imei').AsLargeInt := AKey.IMEI;
  FQueryReadRange.ParamByName('id').AsLargeInt := AKey.ID;
  FQueryReadBefore.ParamByName('id').AsLargeInt := AKey.ID;
  FQueryReadAfter.ParamByName('id').AsLargeInt := AKey.ID;
  FQueryInsert.ParamByName('id').AsLargeInt := AKey.ID;
  FQueryUpdate.ParamByName('id').AsLargeInt := AKey.ID;
  FQueryDeleteRange.ParamByName('id').AsLargeInt := AKey.ID;
  FQueryLastPresentDT.ParamByName('id').AsLargeInt := AKey.ID;
end;

procedure TCacheEventGeo.ExecDBInsert(
  const AObj: IDataObj
);
begin
  with (AObj as TEventGeo), FQueryInsert do
  begin
    Active := False;
    ParamByName('dt').AsDateTime := FDTMark;
    ParamByName('t_id').AsInteger := FEventType;
    ParamByName('dur').AsFloat := FDuration;
    ExecSQL();
  end;
end;

procedure TCacheEventGeo.ExecDBUpdate(
  const AObj: IDataObj
);
begin
  with (AObj as TEventGeo), FQueryUpdate do
  begin
    Active := False;
    ParamByName('dt').AsDateTime := FDTMark;
    ParamByName('t_id').AsInteger := FEventType;
    ParamByName('dur').AsFloat := FDuration;
    ExecSQL();
  end;
end;

function TCacheEventGeo.MakeObjFromReadReq(
  const AQuery: TZQuery
): IDataObj;
begin
  with AQuery do
  begin
    Result := TEventGeo.Create(
      FieldByName('IMEI').AsLargeInt,
      FieldByName('DT').AsDateTime,
      FieldByName('EventGeoTypeID').AsInteger,
      FieldByName(FIdFieldName).AsInteger,
      FieldByName('Duration').AsFloat
    );
  end;
end;

end.

