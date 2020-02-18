unit Adapt.EventDepot;
//------------------------------------------------------------------------------
// модуль адаптера кэша для TClassEventDepot
//------------------------------------------------------------------------------

//------------------------------------------------------------------------------
interface

uses
  SysUtils,
  ZConnection, ZDataset,
  Cache.Root, Cache.Elements, Adapter.MySQL, Event.Cnst;

//------------------------------------------------------------------------------
type

//------------------------------------------------------------------------------
//!
//------------------------------------------------------------------------------
  TEventDepotMySQL = class(TCacheAdapterMySQL)
  protected
    function MakeObjFromReadQuery(
      const AQuery: TZQuery
    ): TCacheDataObjectAbstract; override;
  public
    constructor Create(
      const AReadConnection: TZConnection;
      const AWriteConnection: TZConnection
    );
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
  end;

//------------------------------------------------------------------------------
implementation

//------------------------------------------------------------------------------
//! запросы
//------------------------------------------------------------------------------
const
  CSQLInsert = 'INSERT INTO event_depot'
    + ' (IMEI, DepotID, DT, Duration, GUID)'
    + ' VALUES (:imei, :d_id, :dt, :dur, UUID_TO_BIN(:guid))';
  CSQLUpdate = 'UPDATE event_depot'
    + ' SET Duration = :dur'
    + ' WHERE IMEI = :imei'
    + ' AND DepotID = :d_id'
    + ' AND DT = :dt';
  CSQLReadBefore = 'SELECT MAX(DT) AS DT'
    + ' FROM event_depot'
    + ' WHERE IMEI = :imei'
    + ' AND DepotID = :d_id'
    + ' AND DT < :dt';
  CSQLReadAfter = 'SELECT MIN(DT) AS DT'
    + ' FROM event_depot'
    + ' WHERE IMEI = :imei'
    + ' AND DepotID = :d_id'
    + ' AND DT > :dt';
  CSQLReadRange = 'SELECT'
    + ' IMEI, DepotID, DT, Duration, BIN_TO_UUID(GUID) AS GUID'
    + ' FROM event_depot'
    + ' WHERE IMEI = :imei'
    + ' AND DepotID = :d_id'
    + ' AND DT >= :dt_from'
    + ' AND DT <= :dt_to';
  CSQLDeleteRange = 'DELETE FROM event_depot'
    + ' WHERE IMEI = :imei'
    + ' AND DepotID = :d_id'
    + ' AND DT = :dt';

//------------------------------------------------------------------------------
// TEventDepotMySQL
//------------------------------------------------------------------------------

constructor TEventDepotMySQL.Create(
  const AReadConnection: TZConnection;
  const AWriteConnection: TZConnection
);
begin
  inherited Create(
    AReadConnection, AWriteConnection,
    CSQLInsert, CSQLUpdate, CSQLReadBefore, CSQLReadAfter, CSQLReadRange, CSQLDeleteRange
  );
end;

procedure TEventDepotMySQL.Flush(
  const ACache: TCache
);
//var
//  Iter: TCacheUnit;
//------------------------------------------------------------------------------
begin
//  FillParamsFromKey(ACache.Key, FQueryInsert);
//  FillParamsFromKey(ACache.Key, FQueryUpdate);
//  for Iter in ACache do
//  begin
//    with TClassEventDepot(Iter) do
//    begin
//      if (Origin = ooNew) then
//      begin
//        FQueryInsert.ParamByName(CSQLParamDT).AsDateTime := DTMark;
//        FQueryInsert.ParamByName('dur').AsFloat := Duration;
//        FQueryInsert.ParamByName('inv').AsInteger := Ord(Inverse);
//        FQueryInsert.ExecSQL();
//        Origin := ooLoaded;
//        Stored := Now();
//      end
//      else
//      begin
//        if (Changed > Stored) then
//        begin
//          FQueryUpdate.ParamByName(CSQLParamDT).AsDateTime := DTMark;
//          FQueryUpdate.ParamByName('dur').AsFloat := Duration;
//          FQueryUpdate.ParamByName('inv').AsInteger := Ord(Inverse);
//          FQueryUpdate.ExecSQL();
//          Stored := Now();
//        end;
//      end;
//    end;
//  end;
end;

procedure TEventDepotMySQL.Add(
  const ACache: TCache;
  const AObj: TCacheDataObjectAbstract
);
begin
  with TEventDepot(AObj) do
  begin
    FillParamsFromKey(ACache.Key, FInsertQry);
    FInsertQry.ParamByName(CSQLParamDT).AsDateTime := DTMark;
    FInsertQry.ParamByName('dur').AsFloat := Duration;
    FInsertQry.ParamByName('guid').AsString := GUID;
    FInsertQry.ExecSQL();
    Origin := ooLoaded;
    Stored := Now();
  end;
end;

procedure TEventDepotMySQL.Change(
  const ACache: TCache;
  const AObj: TCacheDataObjectAbstract
);
begin
  with TEventDepot(AObj) do
  begin
    FillParamsFromKey(ACache.Key, FUpdateQry);
//    if (Changed > Stored) then
//    begin
      FUpdateQry.ParamByName(CSQLParamDT).AsDateTime := DTMark;
      FUpdateQry.ParamByName('dur').AsFloat := Duration;
      FUpdateQry.ExecSQL();
      Stored := Now();
//    end;
  end;
end;

function TEventDepotMySQL.MakeObjFromReadQuery(
  const AQuery: TZQuery
): TCacheDataObjectAbstract;
begin
  with AQuery do
  begin
    Result := TEventDepot.Create(
      ooLoaded,
      FieldByName('IMEI').AsLargeInt,
      FieldByName('DepotID').AsInteger,
      FieldByName('DT').AsDateTime,
      FieldByName('Duration').AsFloat,
      '{' + FieldByName('GUID').AsString + '}'
    );
  end;
end;

end.

