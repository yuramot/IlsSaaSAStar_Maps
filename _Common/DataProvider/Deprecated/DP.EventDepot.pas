unit DP.EventDepot;
//------------------------------------------------------------------------------
// модуль кэша таблицы event_depot
//------------------------------------------------------------------------------
// содержит:
//------------------------------------------------------------------------------

//------------------------------------------------------------------------------
interface

uses
  SysUtils,
  System.Generics.Collections,
  DP.Root,
  Geo.Pos,
  ZConnection, ZDataset,
  Ils.MySql.Conf;

//------------------------------------------------------------------------------
type

//------------------------------------------------------------------------------
//! класс данных событий гаража
//------------------------------------------------------------------------------
  TClassEventDepot = class(TDataObj)
  private
    //!
    FID: Integer;
    //!
    FVehicleID: Integer;
    //!
    FDepotID: Integer;
    //!
    FDTMark: TDateTime;
    //!
    FDuration: Double;
    //!
    FTill: TDateTime;
    //!
    procedure SetTill(const Value: TDateTime);
    procedure SetDuration(const Value: Double);
    procedure SetID(const Value: Integer);
  protected
    //!
    function GetDTMark(): TDateTime; override;
  public
    constructor Create(
      const ID: Integer;
      const VehicleID: Integer;
      const DepotID: Integer;
      const DTMark: TDateTime;
      const Duration: Double
    );
    //!
    property ID: Integer read FID write SetID;
    property VehicleID: Integer read FVehicleID;
    property DepotID: Integer read FDepotID;
    property Duration: Double read FDuration write SetDuration;
    property Till: TDateTime read FTill write SetTill;
  end;

//------------------------------------------------------------------------------
//! класс кэша событий гаража
//------------------------------------------------------------------------------
  TCacheEventDepot = class(TCacheRoot)
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
      const DBWriteback: Boolean;
      const MaxKeepCount: Integer;
      const LoadDelta: Double
    );
  end;

//------------------------------------------------------------------------------
//! ключ списка кэшей событий гаража
//------------------------------------------------------------------------------
  TEventDepotDictKey = packed record
    //!
    VehicleID: Integer;
    //!
    DepotID: Integer;
  end;

//------------------------------------------------------------------------------
//! класс списка кэшей событий гаража
//------------------------------------------------------------------------------
  TEEventDepotDictionary = class(TCacheDictionaryRoot<TEventDepotDictKey>);

//------------------------------------------------------------------------------
implementation

const

//------------------------------------------------------------------------------
// запросы к БД
//------------------------------------------------------------------------------
  CSQLReadRange = 'SELECT *'
    + ' FROM event_depot'
    + ' WHERE VehicleID = :v_id'
    + ' AND DepotID = :d_id'
    + ' AND dt >= :dt_from'
    + ' AND dt <= :dt_to';
  CSQLReadBefore = 'SELECT *'
    + ' FROM event_depot'
    + ' WHERE VehicleID = :v_id'
    + ' AND DepotID = :d_id'
    + ' AND DT < :dt'
    + ' ORDER BY DT DESC'
    + ' LIMIT 1';
  CSQLReadAfter = 'SELECT *'
    + ' FROM event_depot'
    + ' WHERE VehicleID = :v_id'
    + ' AND DepotID = :d_id'
    + ' AND DT > :dt'
    + ' ORDER BY DT'
    + ' LIMIT 1';
  CSQLInsert = 'INSERT INTO event_depot'
    + ' (VehicleID, DepotID, DT, Duration)'
    + ' VALUES (:v_id, :d_id, :dt, :dur)';
  CSQLUpdate = 'UPDATE event_depot SET'
    + ' SET DT = :dt, Duration = :dur'
    + ' WHERE ??? = ???';
  CSQLDeleteRange = 'DELETE FROM event_depot'
    + ' WHERE VehicleID = :v_id'
    + ' AND DepotID = :d_id'
    + ' AND dt >= :dt_from'
    + ' AND dt <= :dt_to';
  CSQLLastPresentDT = 'SELECT MAX(DT) AS DT'
    + ' FROM event_depot'
    + ' WHERE VehicleID = :v_id'
    + ' AND DepotID = :d_id';

//------------------------------------------------------------------------------
// TSystemMoveEventClass
//------------------------------------------------------------------------------

constructor TClassEventDepot.Create(
  const ID: Integer;
  const VehicleID: Integer;
  const DepotID: Integer;
  const DTMark: TDateTime;
  const Duration: Double
);
begin
  inherited Create();
  //
  FID := ID;
  FVehicleID := VehicleID;
  FDepotID := DepotID;
  FDTMark := DTMark;
  FDuration := Duration;
  FTill := DTMark + Duration;
end;

function TClassEventDepot.GetDTMark(): TDateTime;
begin
  Result := FDTMark;
end;

procedure TClassEventDepot.SetID(
  const Value: Integer
);
begin
  FID := Value;
end;

procedure TClassEventDepot.SetDuration(
  const Value: Double
);
begin
  FDuration := Value;
  FTill := Value + FDTMark;
end;

procedure TClassEventDepot.SetTill(
  const Value: TDateTime
);
begin
  FTill := Value;
  FDuration := Value - FDTMark;
end;

//------------------------------------------------------------------------------
// TCacheEventDepot
//------------------------------------------------------------------------------

constructor TCacheEventDepot.Create(
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
//  FQueryReadRange.ParamByName('v_id').AsInteger := 0;
//  FQueryReadBefore.ParamByName('v_id').AsInteger := 0;
//  FQueryReadAfter.ParamByName('v_id').AsInteger := 0;
//  FQueryInsert.ParamByName('v_id').AsInteger := 0;
//  FQueryInsert.ParamByName('v_id').AsInteger := 0;
//  FQueryDeleteRange.ParamByName('v_id').AsInteger := 0;
//  FQueryLastPresentDT.ParamByName('v_id').AsInteger := 0;
//  FQueryReadRange.ParamByName('d_id').AsInteger := 0;
//  FQueryReadBefore.ParamByName('d_id').AsInteger := 0;
//  FQueryReadAfter.ParamByName('d_id').AsInteger := 0;
//  FQueryInsert.ParamByName('d_id').AsInteger := 0;
//  FQueryInsert.ParamByName('d_id').AsInteger := 0;
//  FQueryDeleteRange.ParamByName('d_id').AsInteger := 0;
//  FQueryLastPresentDT.ParamByName('d_id').AsInteger := 0;
end;

procedure TCacheEventDepot.ExecDBInsert(
  const AObj: IDataObj
);
begin
  with (AObj as TClassEventDepot), FQueryInsert do
  begin
    Active := False;
    ParamByName('v_id').AsFloat := FVehicleID;
    ParamByName('d_id').AsFloat := FDepotID;
    ParamByName('dt').AsDateTime := FDTMark;
    ParamByName('dur').AsDateTime := FDuration;
    ExecSQL();
  end;
end;

procedure TCacheEventDepot.ExecDBUpdate(
  const AObj: IDataObj
);
begin
  with (AObj as TClassEventDepot), FQueryUpdate do
  begin
    Active := False;
    ParamByName('v_id').AsFloat := FVehicleID;
    ParamByName('d_id').AsFloat := FDepotID;
    ParamByName('dt').AsDateTime := FDTMark;
    ParamByName('dur').AsDateTime := FDuration;
    ExecSQL();
  end;
end;

function TCacheEventDepot.MakeObjFromReadReq(
  const AQuery: TZQuery
): IDataObj;
begin
  with AQuery do
  begin
    Result := TClassEventDepot.Create(
      FieldByName('ID').AsInteger,
      FieldByName('VehicleID').AsInteger,
      FieldByName('DepotID').AsInteger,
      FieldByName('DT').AsDateTime,
      FieldByName('Duration').AsFloat
    );
  end;
end;

end.

