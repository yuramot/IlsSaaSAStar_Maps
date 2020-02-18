unit DP.TrackPoints;
//------------------------------------------------------------------------------
// !!! *** это не подноценый класс данных, а очень ограниченный *** !!!
//------------------------------------------------------------------------------

//------------------------------------------------------------------------------
interface

uses
  SysUtils,
  DP.Root,
  Geo.Pos,
  ZConnection, ZDataset,
  Ils.MySql.Conf;

//------------------------------------------------------------------------------
type

//------------------------------------------------------------------------------
//! МИКРО-класс данных точек трека
//------------------------------------------------------------------------------
  TTrackPoint = class(TDataObj)
  private
    FIMEI: Int64;
    FDTMark: TDateTime;
    FLatitude: Double;
    FLongitude: Double;
  protected
    //!
    function GetDTMark(): TDateTime; override;
  public
    constructor Create(
      const IMEI: Int64;
      const DTMark: TDateTime;
      const Latitude: Double;
      const Longitude: Double
    ); overload;
    constructor Create(
      Source: TTrackPoint
    ); overload;
    function Clone(): IDataObj; override;
    property IMEI: Int64 read FIMEI;
    property Latitude: Double read FLatitude;
    property Longitude: Double read FLongitude;
  end;

//------------------------------------------------------------------------------
//! МИКРО-класс кэша точек трека
//------------------------------------------------------------------------------
  TCacheTrackPoint = class(TCacheRoot)
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
      const ReadConnection: TZConnection;
      const WriteConnection: TZConnection;
      const IMEI: Int64;
      const MaxKeepCount: Integer;
      const LoadDelta: Double
    );
  end;

//------------------------------------------------------------------------------
//! класс списка кэшей точек трека
//------------------------------------------------------------------------------
  TTrackPointDictionary = TCacheDictionaryRoot<Int64>;

//------------------------------------------------------------------------------
implementation

//------------------------------------------------------------------------------
// TClassTrackPoint
//------------------------------------------------------------------------------

constructor TTrackPoint.Create(
  const IMEI: Int64;
  const DTMark: TDateTime;
  const Latitude: Double;
  const Longitude: Double
);
begin
  inherited Create();
  //
  FIMEI := IMEI;
  FDTMark := DTMark;
  FLatitude := Latitude;
  FLongitude := Longitude;
end;

constructor TTrackPoint.Create(
  Source: TTrackPoint
);
begin
  inherited Create();
  //
  FIMEI := Source.IMEI;
  FDTMark := Source.DTMark;
  FLatitude := Source.Latitude;
  FLongitude := Source.Longitude;
end;

function TTrackPoint.Clone(): IDataObj;
begin
  Result := TTrackPoint.Create(Self);
end;

function TTrackPoint.GetDTMark(): TDateTime;
begin
  Result := FDTMark;
end;

//------------------------------------------------------------------------------
// TCacheTrackPoint
//------------------------------------------------------------------------------

constructor TCacheTrackPoint.Create(
  const ReadConnection: TZConnection;
  const WriteConnection: TZConnection;
  const IMEI: Int64;
  const MaxKeepCount: Integer;
  const LoadDelta: Double
);
const
  CSQLReadRange = 'SELECT IMEI, DT, Latitude, Longitude'
    + ' FROM track'
    + ' WHERE IMEI = :imei'
    + ' AND DT >= :dt_from'
    + ' AND DT <= :dt_to';
  CSQLReadBefore = 'SELECT IMEI, DT, Latitude, Longitude'
    + ' FROM track'
    + ' WHERE IMEI = :imei'
    + ' AND DT < :dt'
    + ' ORDER BY DT DESC'
    + ' LIMIT 1';
  CSQLReadAfter = 'SELECT IMEI, DT, Latitude, Longitude'
    + ' FROM track'
    + ' WHERE IMEI = :imei'
    + ' AND DT > :dt'
    + ' ORDER BY DT'
    + ' LIMIT 1';
  CSQLInsert = 'NI';
  CSQLUpdate = 'NI';
  CSQLDeleteRange = 'NI';
  CSQLLastPresentDT = 'SELECT MAX(DT) AS DT'
    + ' FROM track'
    + ' WHERE IMEI = :imei';
//------------------------------------------------------------------------------
begin
  inherited Create(
    ReadConnection, WriteConnection, False, MaxKeepCount, LoadDelta,
    CSQLReadRange, CSQLReadBefore, CSQLReadAfter, CSQLInsert, CSQLUpdate, CSQLDeleteRange, CSQLLastPresentDT
  );
  //
  FQueryReadRange.ParamByName('imei').AsLargeInt := IMEI;
  FQueryReadBefore.ParamByName('imei').AsLargeInt := IMEI;
  FQueryReadAfter.ParamByName('imei').AsLargeInt := IMEI;
  FQueryLastPresentDT.ParamByName('imei').AsLargeInt := IMEI;
end;

procedure TCacheTrackPoint.ExecDBInsert(
  const AObj: IDataObj
);
begin
//  with (AObj as TClassTrackPoint), FQueryInsert do
//  begin
//    Active := False;
//    ParamByName('dt').AsDateTime := DTMark;
//    ParamByName('Latitude').AsFloat := Latitude;
//    ParamByName('Longitude').AsFloat := Longitude;
//    ExecSQL();
//  end;
end;

procedure TCacheTrackPoint.ExecDBUpdate(
  const AObj: IDataObj
);
begin
//  with (AObj as TClassTrackPoint), FQueryUpdate do
//  begin
//    Active := False;
//    ParamByName('dt').AsDateTime := DTMark;
//    ParamByName('Latitude').AsFloat := Latitude;
//    ParamByName('Longitude').AsFloat := Longitude;
//    ExecSQL();
//  end;
end;

function TCacheTrackPoint.MakeObjFromReadReq(
  const AQuery: TZQuery
): IDataObj;
begin
  with AQuery do
  begin
    Result := TTrackPoint.Create(
      FieldByName('IMEI').AsLargeInt,
      FieldByName('DT').AsDateTime,
      FieldByName('Latitude').AsFloat,
      FieldByName('Longitude').AsFloat
    );
  end;
end;

end.

