unit Adapt.WaypointEvent;
//------------------------------------------------------------------------------
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
  TEventWaypointMySQL = class(TCacheAdapterMySQL)
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
  CSQLInsert = 'INSERT INTO waypointevent'
    + ' (VehicleID, WaypointID, DT, PlanHit, WorktimeHit, FeasibleHit, GUID, EventDepotGUID)'
    + ' VALUES (:v_id, :w_id, :dt, :h_plan, :h_worktime, :h_feasible, UUID_TO_BIN(:guid), UUID_TO_BIN(:ed_guid))';
  CSQLUpdate = 'UPDATE waypointevent'
    + ' SET PlanHit = :h_plan, WorktimeHit = :h_worktime, FeasibleHit = :h_feasible'
    + ' WHERE VehicleID = :v_id'
    + ' AND WaypointID = :w_id'
    + ' AND DT = :dt';
  CSQLReadBefore = 'SELECT MAX(DT) AS DT'
    + ' FROM waypointevent'
    + ' WHERE VehicleID = :v_id'
    + ' AND WaypointID = :w_id'
    + ' AND DT < :dt';
  CSQLReadAfter = 'SELECT MIN(DT) AS DT'
    + ' FROM waypointevent'
    + ' WHERE VehicleID = :v_id'
    + ' AND WaypointID = :w_id'
    + ' AND DT > :dt';
  CSQLReadRange = 'SELECT'
      + ' D.Duration, W.VehicleID, W.WaypointID, W.DT, W.PlanHit, W.WorktimeHit, W.FeasibleHit,'
      + ' BIN_TO_UUID(W.GUID) AS GUID, BIN_TO_UUID(W.EventDepotGUID) AS EventDepotGUID'
    + ' FROM waypointevent W, event_depot D'
    + ' WHERE W.EventDepotGUID = D.GUID'
    + ' AND W.VehicleID = :v_id'
    + ' AND W.WaypointID = :w_id'
    + ' AND W.DT >= :dt_from'
    + ' AND W.DT <= :dt_to';
  CSQLDeleteRange = 'DELETE FROM waypointevent'
    + ' WHERE VehicleID = :v_id'
    + ' AND WaypointID = :w_id'
    + ' AND DT = :dt';

//------------------------------------------------------------------------------
// TEventWaypointMySQL
//------------------------------------------------------------------------------

constructor TEventWaypointMySQL.Create(
  const AReadConnection: TZConnection;
  const AWriteConnection: TZConnection
);
begin
  inherited Create(
    AReadConnection, AWriteConnection,
    CSQLInsert, CSQLUpdate, CSQLReadBefore, CSQLReadAfter, CSQLReadRange, CSQLDeleteRange
  );
end;

procedure TEventWaypointMySQL.Flush(
  const ACache: TCache
);
begin
//
end;

procedure TEventWaypointMySQL.Add(
  const ACache: TCache;
  const AObj: TCacheDataObjectAbstract
);
begin
  with TEventWaypoint(AObj) do
  begin
    FillParamsFromKey(ACache.Key, FInsertQry);
    FInsertQry.ParamByName(CSQLParamDT).AsDateTime := DTMark;
    FInsertQry.ParamByName('h_plan').AsInteger := Ord(PlanHit);
    FInsertQry.ParamByName('h_worktime').AsInteger := Ord(WorktimeHit);
    FInsertQry.ParamByName('h_feasible').AsInteger := Ord(FeasibleHit);
    FInsertQry.ParamByName('guid').AsString := GUID;
    FInsertQry.ParamByName('ed_guid').AsString := DepotGUID;
    FInsertQry.ExecSQL();
    Origin := ooLoaded;
    Stored := Now();
  end;
end;

procedure TEventWaypointMySQL.Change(
  const ACache: TCache;
  const AObj: TCacheDataObjectAbstract
);
begin
  with TEventWaypoint(AObj) do
  begin
    FillParamsFromKey(ACache.Key, FUpdateQry);
//    if (Changed > Stored) then
//    begin
      FUpdateQry.ParamByName(CSQLParamDT).AsDateTime := DTMark;
      FUpdateQry.ParamByName('h_plan').AsInteger := Ord(PlanHit);
      FUpdateQry.ParamByName('h_worktime').AsInteger := Ord(WorktimeHit);
      FUpdateQry.ParamByName('h_feasible').AsInteger := Ord(FeasibleHit);
      FUpdateQry.ExecSQL();
      Stored := Now();
//    end;
  end;
end;

function TEventWaypointMySQL.MakeObjFromReadQuery(
  const AQuery: TZQuery
): TCacheDataObjectAbstract;
begin
  with AQuery do
  begin
    Result := TEventWaypoint.Create(
      ooLoaded,
      FieldByName('VehicleID').AsInteger,
      FieldByName('WaypointID').AsInteger,
      FieldByName('DT').AsDateTime,
      FieldByName('Duration').AsFloat,
      FieldByName('PlanHit').AsInteger <> 0,
      FieldByName('WorktimeHit').AsInteger <> 0,
      FieldByName('FeasibleHit').AsInteger <> 0,
      '{' + FieldByName('GUID').AsString + '}',
      '{' + FieldByName('EventDepotGUID').AsString + '}'
    );
  end;
end;

end.

