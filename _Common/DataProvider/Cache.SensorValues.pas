unit Cache.SensorValues;

interface

uses
  SysUtils,
  Cache.Root,
  Adapter.MySQL,
  Geo.Pos,
  ZConnection, ZDataset,
  Ils.MySql.Conf,
  uTrackPoints,
  Math,
  DateUtils,
  Ils.Utils;

type

  TSensorValueKey = packed record
    IMEI: Int64;
    SensorID: Integer;
    constructor Create(const AIMEI: Int64; const ASensorID: Integer);
  end;

  TSensorValue = class(TCacheDataObjectAbstract)
  protected
    function GetDTMark(): TDateTime; override;
  private
    FKey: TSensorValueKey;
    FDT: TDateTime;
    FValueRaw: Double;
    FValue: Double;
    FValueFiltered: Double;

  public
    property IMEI: Int64 read FKey.IMEI;
    property SensorID: Integer read FKey.SensorID;
    property DT: TDateTime read FDT;
    property ValueRaw: Double read FValueRaw;
    property Value: Double read FValue write FValue;
    property ValueFiltered: Double read FValueFiltered;
    procedure Assign(const ASource: TSensorValue);
    function Clone: TCacheDataObjectAbstract; override;
    constructor Create(const ASource: TSensorValue); overload;
    constructor Create(const AIMEI: Int64; const ASensorID: Integer; const ADT: TDateTime; const AValueRaw, AValue, AValueFiltered: Double); overload;
  end;

  TSensorValueCacheDataAdapter = class(TCacheAdapterMySQL)
  private
    FQueryUpdateSensorLastValue: TZQuery;
  public
    procedure Flush(const ACache: TCache); override;
    procedure Add(const ACache: TCache; const AObj: TCacheDataObjectAbstract); override;
    procedure Change(const ACache: TCache; const AObj: TCacheDataObjectAbstract); override;
    function MakeObjFromReadQuery(const AQuery: TZQuery): TCacheDataObjectAbstract; override;
    constructor Create(const AReadConnection, AWriteConnection: TZConnection);
    destructor Destroy; override;
  end;

  TSensorValueCacheHash = class(TCacheDictionary<TSensorValueKey>)
  private
    FSensorValueDataAdapter: TSensorValueCacheDataAdapter;
    FWriteBack: Boolean;
    FInterval: Double;
    FMaxCount: Integer;
  public
    function GetSensorCache(const ASensorKey: TSensorValueKey): TCache;
    constructor Create(const ASensorValueDataAdapter: TSensorValueCacheDataAdapter;
      const AWriteBack: Boolean; const AInterval: Double; const AMaxCount: Integer);
  end;


implementation

{ TSensorValueCacheDataAdapter }

constructor TSensorValueCacheDataAdapter.Create(
  const AReadConnection, AWriteConnection: TZConnection
);
var
  SQLInsert: string;
  SQLUpdate: string;
  SQLReadBefore: string;
  SQLReadAfter: string;
  SQLReadRange: string;
  SQLDeleteOne: string;
  SQLUpdateLastValue: string;
begin
  SQLReadRange := 'SELECT *'
    + ' FROM sensorvalue'
    + ' WHERE IMEI = :imei'
    + ' AND SensorID = :SensorID'
    + ' AND DT >= :dt_from'
    + ' AND DT <= :dt_to'
    ;
  SQLReadBefore := 'SELECT MAX(DT) as DT'
    + ' FROM sensorvalue'
    + ' WHERE IMEI = :imei'
    + ' AND SensorID = :SensorID'
    + ' AND DT < :dt'
    ;
  SQLReadAfter := 'SELECT MIN(DT) as DT'
    + ' FROM  sensorvalue'
    + ' WHERE IMEI = :imei'
    + ' AND SensorID = :SensorID'
    + ' AND DT > :dt'
    ;
  SQLInsert :=
    'insert ignore into sensorvalue set'
    + ' IMEI = :IMEI, DT = :DT, SensorID = :SensorID,'
    + ' Raw = :Raw, Value = :Value, Filtered = :Filtered'
    + ' on duplicate key update'
    + ' Raw = VALUES(Raw), Value = VALUES(Value), Filtered = VALUES(Filtered)'
    ;
  SQLUpdate :=
    'update sensorvalue set'
    + ' Raw = :Raw, Value = :Value, Filtered = :Filtered'
    + ' where'
    + ' IMEI = :IMEI'
    + ' AND SensorID = :SensorID'
    + ' and DT = :DT'
    ;
  SQLDeleteOne :=
    '*';
  SQLUpdateLastValue :=
    'insert ignore into sensorlastvalue set'
    + ' IMEI = :IMEI, DT = :DT, SensorID = :SensorID,'
    + ' Raw = :Raw, Value = :Value, Filtered = :Filtered'
    + ' on duplicate key update'
    + ' DT = VALUES(DT),'
    + ' Raw = VALUES(Raw), Value = VALUES(Value), Filtered = VALUES(Filtered)'
    ;

  inherited Create(
    AReadConnection, AWriteConnection,
    SQLInsert, SQLUpdate, SQLReadBefore, SQLReadAfter, SQLReadRange, SQLDeleteOne);

  FQueryUpdateSensorLastValue := TZQuery.Create(nil);
  FQueryUpdateSensorLastValue.Connection := AWriteConnection;
  FQueryUpdateSensorLastValue.SQL.Text := SQLUpdateLastValue;
end;


destructor TSensorValueCacheDataAdapter.Destroy;
begin
  FQueryUpdateSensorLastValue.Free;

  inherited;
end;

procedure TSensorValueCacheDataAdapter.Flush(const ACache: TCache);
begin
  //
end;

procedure TSensorValueCacheDataAdapter.Add(
  const ACache: TCache;
  const AObj: TCacheDataObjectAbstract
);
begin
  FillParamsFromKey(ACache.Key, FInsertQry);
  FillParamsFromKey(ACache.Key, FQueryUpdateSensorLastValue);


  with (AObj as TSensorValue), FInsertQry do
  begin
    Close;

    ParamByName('IMEI').AsLargeInt := IMEI;
    ParamByName('DT').AsDateTime := DT;
    ParamByName('SensorID').AsInteger := SensorID;
    ParamByName('Raw').AsFloat := ValueRaw;
    ParamByName('Value').AsFloat := Value;
    ParamByName('Filtered').AsFloat := ValueFiltered;

    ExecSQL;
  end;

  with (AObj as TSensorValue), FQueryUpdateSensorLastValue do
  begin
    Close;

    ParamByName('IMEI').AsLargeInt := IMEI;
    ParamByName('DT').AsDateTime := DT;
    ParamByName('SensorID').AsInteger := SensorID;
    ParamByName('Raw').AsFloat := ValueRaw;
    ParamByName('Value').AsFloat := Value;
    ParamByName('Filtered').AsFloat := ValueFiltered;

    ExecSQL;
  end;
end;

procedure TSensorValueCacheDataAdapter.Change(
  const ACache: TCache;
  const AObj: TCacheDataObjectAbstract
);
begin
  FillParamsFromKey(ACache.Key, FUpdateQry);

  with (AObj as TSensorValue), FUpdateQry do
  begin
    Close;

    ParamByName('IMEI').AsLargeInt := IMEI;
    ParamByName('DT').AsDateTime := DT;
    ParamByName('SensorID').AsInteger := SensorID;
    ParamByName('Raw').AsFloat := ValueRaw;
    ParamByName('Value').AsFloat := Value;
    ParamByName('Filtered').AsFloat := ValueFiltered; //TODO: проверить на infinity

    ExecSQL;
  end;
end;

function TSensorValueCacheDataAdapter.MakeObjFromReadQuery(
  const AQuery: TZQuery
): TCacheDataObjectAbstract;
begin
  with AQuery do
  begin
    Result :=
      TSensorValue.Create(
        FieldByName('IMEI').AsLargeInt,
        FieldByName('SensorID').AsInteger,
        FieldByName('DT').AsDateTime,
        FieldByName('Raw').AsFloat,
        FieldByName('Value').AsFloat,
        FieldByName('Filtered').AsFloat
      );
  end;
end;

{ TSensorValue }

procedure TSensorValue.Assign(const ASource: TSensorValue);
begin
  FKey.IMEI       := ASource.IMEI;
  FKey.SensorID   := ASource.SensorID;
  FDT             := ASource.DT;
  FValueRaw       := ASource.ValueRaw;
  FValue          := ASource.Value;
  FValueFiltered  := ASource.ValueFiltered;
end;

function TSensorValue.Clone: TCacheDataObjectAbstract;
begin
  Result := TSensorValue.Create(Self);
end;

constructor TSensorValue.Create(const AIMEI: Int64; const ASensorID: Integer;
  const ADT: TDateTime; const AValueRaw, AValue, AValueFiltered: Double);
begin
  FKey.IMEI       := AIMEI;
  FKey.SensorID   := ASensorID;
  FDT             := ADT;
  FValueRaw       := AValueRaw;
  FValue          := AValue;
  FValueFiltered  := AValueFiltered;
end;

constructor TSensorValue.Create(const ASource: TSensorValue);
begin
  Assign(ASource);
end;

function TSensorValue.GetDTMark: TDateTime;
begin
  Result := FDT;
end;

{ TSensorValueKey }

constructor TSensorValueKey.Create(const AIMEI: Int64; const ASensorID: Integer);
begin
  IMEI := AIMEI;
  SensorID := ASensorID;
end;

{ TSensorValueCacheHash }

constructor TSensorValueCacheHash.Create(
  const ASensorValueDataAdapter: TSensorValueCacheDataAdapter;
  const AWriteBack: Boolean; const AInterval: Double; const AMaxCount: Integer);
begin
  FSensorValueDataAdapter := ASensorValueDataAdapter;
  FWriteBack := AWriteBack;
  FInterval := AInterval;
  FMaxCount := AMaxCount;

  inherited Create;
end;

function TSensorValueCacheHash.GetSensorCache(
  const ASensorKey: TSensorValueKey): TCache;
begin
  if not ContainsKey(ASensorKey) then
  begin
    Add(ASensorKey, TCache.Create(
      FSensorValueDataAdapter, True, 15 * OneMinute, 1000, TConnKey.Create([
          'IMEI', IntToStr(ASensorKey.IMEI),
          'SensorID', IntToStr(ASensorKey.SensorID)
      ])
    ));
  end;
  Result := TCache(Self[ASensorKey]);
end;

end.
