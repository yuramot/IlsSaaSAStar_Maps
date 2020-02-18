unit DP.SensorsData;
//------------------------------------------------------------------------------
// !!! *** !!! ВНИМАНИЕ !!! реализовано только чтение !!! *** !!!
//------------------------------------------------------------------------------

//------------------------------------------------------------------------------
interface

uses
  SysUtils,
  System.Generics.Collections,
  DP.Root,
  ZConnection, ZDataset,
  JsonDataObjects,
  Ils.MySql.Conf;

//------------------------------------------------------------------------------
type

//------------------------------------------------------------------------------
//! класс данных данных датчиков (<- не описка)
//------------------------------------------------------------------------------
  TClassDataSensor = class(TDataObj)
  private
    FDTMark: TDateTime;
    FIMEI: Int64;
    FSensorID: Integer;
    FSensorValue: Double;
  protected
    //!
    function GetDTMark(): TDateTime; override;
  public
    constructor Create(
      const IMEI: Int64;
      const DTMark: TDateTime;
      const SensorID: Integer;
      const SensorValue: Double
    ); overload;
    constructor Create(
      Source: TClassDataSensor
    ); overload;
    function Clone(): IDataObj; override;
    property IMEI: Int64 read FIMEI;
    property SensorID: Integer read FSensorID;
    property SensorValue: Double read FSensorValue write FSensorValue;
  end;

//------------------------------------------------------------------------------
//! класс кэша данных датчиков
//------------------------------------------------------------------------------
  TCacheDataSensor = class(TCacheRoot)
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
      const SensorID: Integer;
      const ReadConnection: TZConnection;
      const WriteConnection: TZConnection;
//      const DBWriteback: Boolean;
      const MaxKeepCount: Integer;
      const LoadDelta: Double
    );
  end;

//------------------------------------------------------------------------------
//! ключ списка кэшей данных датчиков
//------------------------------------------------------------------------------
  TSensorsDictKey = packed record
    //!
    IMEI: Int64;
    //!
    SensorID: Integer;
  end;

//------------------------------------------------------------------------------
//! класс списка кэшей данных датчиков
//------------------------------------------------------------------------------
  TSensorsDictionary = class(TCacheDictionaryRoot<TSensorsDictKey>);

//------------------------------------------------------------------------------
implementation

//------------------------------------------------------------------------------
// запросы к БД
//------------------------------------------------------------------------------
const
  CSQLReadRange = 'SELECT *'
    + ' FROM sensorvalue'
    + ' WHERE IMEI = :imei'
    + ' AND SensorID = :sid'
    + ' AND DT >= :dt_from'
    + ' AND DT <= :dt_to';
  CSQLReadBefore = 'SELECT *'
    + ' FROM sensorvalue'
    + ' WHERE IMEI = :imei'
    + ' AND SensorID = :sid'
    + ' AND DT < :dt'
    + ' ORDER BY DT DESC'
    + ' LIMIT 1';
  CSQLReadAfter = 'SELECT *'
    + ' FROM sensorvalue'
    + ' WHERE IMEI = :imei'
    + ' AND SensorID = :sid'
    + ' AND DT > :dt'
    + ' ORDER BY DT'
    + ' LIMIT 1';
  CSQLInsert = 'NI';
  CSQLUpdate = 'NI';
  CSQLDeleteRange = 'NI';
  CSQLLastPresentDT = 'SELECT MAX(DT) AS DT'
    + ' FROM sensorvalue'
    + ' WHERE IMEI = :imei'
    + ' AND SensorID = :sid';

//------------------------------------------------------------------------------
// TClassDataSensor
//------------------------------------------------------------------------------

constructor TClassDataSensor.Create(
  const IMEI: Int64;
  const DTMark: TDateTime;
  const SensorID: Integer;
  const SensorValue: Double
);
begin
  inherited Create();
  //
  FDTMark := DTMark;
  FIMEI := IMEI;
  FSensorID := SensorID;
  FSensorValue := SensorValue;
end;

constructor TClassDataSensor.Create(
  Source: TClassDataSensor
);
begin
  inherited Create();
  //
  FDTMark := Source.DTMark;
  FIMEI := Source.IMEI;
  FSensorID := Source.SensorID;
  FSensorValue := Source.SensorValue;
end;

function TClassDataSensor.Clone(): IDataObj;
begin
  Result := TClassDataSensor.Create(Self);
end;

function TClassDataSensor.GetDTMark(): TDateTime;
begin
  Result := FDTMark;
end;

//------------------------------------------------------------------------------
// TCacheDataSensor
//------------------------------------------------------------------------------

constructor TCacheDataSensor.Create(
  const IMEI: Int64;
  const SensorID: Integer;
  const ReadConnection: TZConnection;
  const WriteConnection: TZConnection;
//  const DBWriteback: Boolean;
  const MaxKeepCount: Integer;
  const LoadDelta: Double
);
begin
  inherited Create(
    ReadConnection, WriteConnection, {DBWriteback}False, MaxKeepCount, LoadDelta,
    CSQLReadRange, CSQLReadBefore, CSQLReadAfter, CSQLInsert, CSQLUpdate, CSQLDeleteRange, CSQLLastPresentDT
  );
  //
  FQueryReadRange.ParamByName('imei').AsLargeInt := IMEI;
  FQueryReadBefore.ParamByName('imei').AsLargeInt := IMEI;
  FQueryReadAfter.ParamByName('imei').AsLargeInt := IMEI;
  FQueryLastPresentDT.ParamByName('imei').AsLargeInt := IMEI;
  FQueryReadRange.ParamByName('sid').AsInteger := SensorID;
  FQueryReadBefore.ParamByName('sid').AsInteger := SensorID;
  FQueryReadAfter.ParamByName('sid').AsInteger := SensorID;
  FQueryLastPresentDT.ParamByName('sid').AsInteger := SensorID;
end;

procedure TCacheDataSensor.ExecDBInsert(
  const AObj: IDataObj
);
begin
//  with (AObj as TClassDataSensor), FQueryInsert do
//  begin
//    Active := False;
//    ParamByName('dt').AsDateTime := DTMark();
////
//    ExecSQL();
//  end;
end;

procedure TCacheDataSensor.ExecDBUpdate(
  const AObj: IDataObj
);
begin
//  with (AObj as TClassDataSensor), FQueryUpdate do
//  begin
//    Active := False;
//    ParamByName('dt').AsDateTime := DTMark();
////
//    ExecSQL();
//  end;
end;

function TCacheDataSensor.MakeObjFromReadReq(
  const AQuery: TZQuery
): IDataObj;
begin
  with AQuery do
  begin
    Result := TClassDataSensor.Create(
      FieldByName('IMEI').AsLargeInt,
      FieldByName('DT').AsDateTime,
      FieldByName('SensorID').AsInteger,
      FieldByName('Value').AsFloat
    );
  end;
end;

end.

