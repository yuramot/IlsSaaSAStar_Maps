unit DP.EventSensor;
//------------------------------------------------------------------------------
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
//! класс данных событий двухуровнего датчика
//------------------------------------------------------------------------------
  TClassEventBinary = class(TDataObj)
  private
    FDTMark: TDateTime;
    FIMEI: Int64;
    FTriggerID: Integer;
    FDuration: TDateTime;
    FInverse: Boolean;
  protected
    //!
    function GetDTMark(): TDateTime; override;
  public
    constructor Create(
      const IMEI: Int64;
      const TriggerID: Integer;
      const DTMark: TDateTime;
      const Duration: TDateTime;
      const Inverse: Boolean
    ); overload;
    constructor Create(
      Source: TClassEventBinary
    ); overload;
    function Clone(): IDataObj; override;
    property IMEI: Int64 read FIMEI;
    property TriggerID: Integer read FTriggerID;
    property Duration: TDateTime read FDuration write FDuration;
    property Inverse: Boolean read FInverse write FInverse;
  end;

//------------------------------------------------------------------------------
//! класс кэша событий двухуровнего датчика
//------------------------------------------------------------------------------
  TCacheEventBinary = class(TCacheRoot)
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
      const TriggerID: Integer;
      const ReadConnection: TZConnection;
      const WriteConnection: TZConnection;
      const DBWriteback: Boolean;
      const MaxKeepCount: Integer;
      const LoadDelta: Double
    );
  end;

//------------------------------------------------------------------------------
//! класс данных событий многоуровнего датчика
//------------------------------------------------------------------------------
  TClassEventMultilevel = class(TDataObj)
  private
    FDTMark: TDateTime;
    FIMEI: Int64;
    FTriggerID: Integer;
    FValue: Double;
    FDuration: TDateTime;
    FInverse: Boolean;
  protected
    //!
    function GetDTMark(): TDateTime; override;
  public
    constructor Create(
      const IMEI: Int64;
      const TriggerID: Integer;
      const DTMark: TDateTime;
      const Value: Double;
      const Duration: TDateTime;
      const Inverse: Boolean
    ); overload;
    constructor Create(
      Source: TClassEventMultilevel
    ); overload;
    function Clone(): IDataObj; override;
    property IMEI: Int64 read FIMEI;
    property TriggerID: Integer read FTriggerID;
    property Value: Double read FValue write FValue;
    property Duration: TDateTime read FDuration write FDuration;
    property Inverse: Boolean read FInverse write FInverse;
  end;

//------------------------------------------------------------------------------
//! класс кэша событий многоуровнего датчика
//------------------------------------------------------------------------------
  TCacheEventMultilevel = class(TCacheRoot)
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
      const TriggerID: Integer;
      const ReadConnection: TZConnection;
      const WriteConnection: TZConnection;
      const DBWriteback: Boolean;
      const MaxKeepCount: Integer;
      const LoadDelta: Double
    );
  end;

//------------------------------------------------------------------------------
//! ключ списка кэшей событий датчиков
//------------------------------------------------------------------------------
  TSensorEventDictKey = packed record
    //!
    IMEI: Int64;
    //!
    TriggerID: Integer;
  end;

//------------------------------------------------------------------------------
//! класс списка кэшей событий датчиков
//------------------------------------------------------------------------------
  TSensorEventDictionary = class(TCacheDictionaryRoot<TSensorEventDictKey>);

//------------------------------------------------------------------------------
implementation

//------------------------------------------------------------------------------
// TClassEventBinary
//------------------------------------------------------------------------------

constructor TClassEventBinary.Create(
  const IMEI: Int64;
  const TriggerID: Integer;
  const DTMark: TDateTime;
  const Duration: TDateTime;
  const Inverse: Boolean
);
begin
  inherited Create();
  //
  FDTMark := DTMark;
  FIMEI := IMEI;
  FTriggerID := TriggerID;
  FDuration := Duration;
  FInverse := Inverse;
end;

constructor TClassEventBinary.Create(
  Source: TClassEventBinary
);
begin
  inherited Create();
  //
  FDTMark := Source.DTMark;
  FIMEI := Source.IMEI;
  FTriggerID := Source.TriggerID;
  FDuration := Source.Duration;
  FInverse := Source.Inverse;
end;

function TClassEventBinary.Clone(): IDataObj;
begin
  Result := TClassEventBinary.Create(Self);
end;

function TClassEventBinary.GetDTMark(): TDateTime;
begin
  Result := FDTMark;
end;

//------------------------------------------------------------------------------
// TCacheEventBinary
//------------------------------------------------------------------------------

constructor TCacheEventBinary.Create(
  const IMEI: Int64;
  const TriggerID: Integer;
  const ReadConnection: TZConnection;
  const WriteConnection: TZConnection;
  const DBWriteback: Boolean;
  const MaxKeepCount: Integer;
  const LoadDelta: Double
);
const
  CSQLReadRange = 'SELECT *'
    + ' FROM event_binary'
    + ' WHERE IMEI = :imei'
    + ' AND TriggerID = :tid'
    + ' AND DT >= :dt_from'
    + ' AND DT <= :dt_to';
  CSQLReadBefore = 'SELECT *'
    + ' FROM event_binary'
    + ' WHERE IMEI = :imei'
    + ' AND TriggerID = :tid'
    + ' AND DT < :dt'
    + ' ORDER BY DT DESC'
    + ' LIMIT 1';
  CSQLReadAfter = 'SELECT *'
    + ' FROM event_binary'
    + ' WHERE IMEI = :imei'
    + ' AND TriggerID = :tid'
    + ' AND DT > :dt'
    + ' ORDER BY DT'
    + ' LIMIT 1';
  CSQLInsert = 'INSERT INTO event_binary'
    + ' (IMEI, DT, TriggerID, Duration, Inverse)'
    + ' VALUES (:imei, :dt, :tid, :dur, :inv)';
  CSQLUpdate = 'UPDATE event_binary'
    + ' SET Duration = :dur, Inverse = :inv'
    + ' WHERE IMEI = :imei'
    + ' AND TriggerID = :tid'
    + ' AND DT = :dt';
  CSQLDeleteRange = 'DELETE FROM event_binary'
    + ' WHERE IMEI = :imei'
    + ' AND TriggerID = :tid'
    + ' AND DT >= :dt_from'
    + ' AND DT <= :dt_to';
  CSQLLastPresentDT = 'SELECT MAX(DT) AS DT'
    + ' FROM event_binary'
    + ' WHERE IMEI = :imei'
    + ' AND TriggerID = :tid';
//------------------------------------------------------------------------------
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
  FQueryReadRange.ParamByName('tid').AsInteger := TriggerID;
  FQueryReadBefore.ParamByName('tid').AsInteger := TriggerID;
  FQueryReadAfter.ParamByName('tid').AsInteger := TriggerID;
  FQueryInsert.ParamByName('tid').AsInteger := TriggerID;
  FQueryUpdate.ParamByName('tid').AsInteger := TriggerID;
  FQueryDeleteRange.ParamByName('tid').AsInteger := TriggerID;
  FQueryLastPresentDT.ParamByName('tid').AsInteger := TriggerID;
end;

procedure TCacheEventBinary.ExecDBInsert(
  const AObj: IDataObj
);
begin
  with (AObj as TClassEventBinary), FQueryInsert do
  begin
    Active := False;
    ParamByName('dt').AsDateTime := DTMark;
    ParamByName('dur').AsDateTime := Duration;
    ParamByName('inv').AsBoolean := Inverse;
    ExecSQL();
  end;
end;

procedure TCacheEventBinary.ExecDBUpdate(
  const AObj: IDataObj
);
begin
  with (AObj as TClassEventBinary), FQueryUpdate do
  begin
    Active := False;
    ParamByName('dt').AsDateTime := DTMark;
    ParamByName('dur').AsDateTime := Duration;
    ParamByName('inv').AsBoolean := Inverse;
    ExecSQL();
  end;
end;

function TCacheEventBinary.MakeObjFromReadReq(
  const AQuery: TZQuery
): IDataObj;
begin
  with AQuery do
  begin
    Result := TClassEventBinary.Create(
      FieldByName('IMEI').AsLargeInt,
      FieldByName('TriggerID').AsInteger,
      FieldByName('DT').AsDateTime,
      FieldByName('Duration').AsDateTime,
      FieldByName('Inverse').AsBoolean
    );
  end;
end;

//------------------------------------------------------------------------------
// TClassEventMultilevel
//------------------------------------------------------------------------------

constructor TClassEventMultilevel.Create(
  const IMEI: Int64;
  const TriggerID: Integer;
  const DTMark: TDateTime;
  const Value: Double;
  const Duration: TDateTime;
  const Inverse: Boolean
);
begin
  inherited Create();
  //
  FDTMark := DTMark;
  FIMEI := IMEI;
  FTriggerID := TriggerID;
  FValue := Value;
  FDuration := Duration;
  FInverse := Inverse;
end;

constructor TClassEventMultilevel.Create(
  Source: TClassEventMultilevel
);
begin
  inherited Create();
  //
  FDTMark := Source.DTMark;
  FIMEI := Source.IMEI;
  FTriggerID := Source.TriggerID;
  FValue := Source.Value;
  FDuration := Source.Duration;
  FInverse := Source.Inverse;
end;

function TClassEventMultilevel.Clone(): IDataObj;
begin
  Result := TClassEventMultilevel.Create(Self);
end;

function TClassEventMultilevel.GetDTMark(): TDateTime;
begin
  Result := FDTMark;
end;

//------------------------------------------------------------------------------
// TCacheEventMultilevel
//------------------------------------------------------------------------------

constructor TCacheEventMultilevel.Create(
  const IMEI: Int64;
  const TriggerID: Integer;
  const ReadConnection: TZConnection;
  const WriteConnection: TZConnection;
  const DBWriteback: Boolean;
  const MaxKeepCount: Integer;
  const LoadDelta: Double
);
const
  CSQLReadRange = 'SELECT *'
    + ' FROM event_multilevel'
    + ' WHERE IMEI = :imei'
    + ' AND TriggerID = :tid'
    + ' AND DT >= :dt_from'
    + ' AND DT <= :dt_to';
  CSQLReadBefore = 'SELECT *'
    + ' FROM event_multilevel'
    + ' WHERE IMEI = :imei'
    + ' AND TriggerID = :tid'
    + ' AND DT < :dt'
    + ' ORDER BY DT DESC'
    + ' LIMIT 1';
  CSQLReadAfter = 'SELECT *'
    + ' FROM event_multilevel'
    + ' WHERE IMEI = :imei'
    + ' AND TriggerID = :tid'
    + ' AND DT > :dt'
    + ' ORDER BY DT'
    + ' LIMIT 1';
  CSQLInsert = 'INSERT INTO event_multilevel'
    + ' (IMEI, DT, TriggerID, Value, Duration, Inverse)'
    + ' VALUES (:imei, :dt, :tid, :value, :dur, :inv)';
  CSQLUpdate = 'UPDATE event_multilevel'
    + ' SET Value = :value, Duration = :dur, Inverse = :inv'
    + ' WHERE IMEI = :imei'
    + ' AND TriggerID = :tid'
    + ' AND DT = :dt';
  CSQLDeleteRange = 'DELETE FROM event_multilevel'
    + ' WHERE IMEI = :imei'
    + ' AND TriggerID = :tid'
    + ' AND DT >= :dt_from'
    + ' AND DT <= :dt_to';
  CSQLLastPresentDT = 'SELECT MAX(DT) AS DT'
    + ' FROM event_multilevel'
    + ' WHERE IMEI = :imei'
    + ' AND TriggerID = :tid';
//------------------------------------------------------------------------------
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
  FQueryReadRange.ParamByName('tid').AsInteger := TriggerID;
  FQueryReadBefore.ParamByName('tid').AsInteger := TriggerID;
  FQueryReadAfter.ParamByName('tid').AsInteger := TriggerID;
  FQueryInsert.ParamByName('tid').AsInteger := TriggerID;
  FQueryUpdate.ParamByName('tid').AsInteger := TriggerID;
  FQueryDeleteRange.ParamByName('tid').AsInteger := TriggerID;
  FQueryLastPresentDT.ParamByName('tid').AsInteger := TriggerID;
end;

procedure TCacheEventMultilevel.ExecDBInsert(
  const AObj: IDataObj
);
begin
  with (AObj as TClassEventMultilevel), FQueryInsert do
  begin
    Active := False;
    ParamByName('dt').AsDateTime := DTMark;
    ParamByName('value').AsFloat := Value;
    ParamByName('dur').AsDateTime := Duration;
    ParamByName('inv').AsInteger := Ord(Inverse);
    ExecSQL();
  end;
end;

procedure TCacheEventMultilevel.ExecDBUpdate(
  const AObj: IDataObj
);
begin
  with (AObj as TClassEventMultilevel), FQueryUpdate do
  begin
    Active := False;
    ParamByName('dt').AsDateTime := DTMark;
    ParamByName('value').AsFloat := Value;
    ParamByName('dur').AsDateTime := Duration;
    ParamByName('inv').AsInteger := Ord(Inverse);
    ExecSQL();
  end;
end;

function TCacheEventMultilevel.MakeObjFromReadReq(
  const AQuery: TZQuery
): IDataObj;
begin
  with AQuery do
  begin
    Result := TClassEventMultilevel.Create(
      FieldByName('IMEI').AsLargeInt,
      FieldByName('TriggerID').AsInteger,
      FieldByName('DT').AsDateTime,
      FieldByName('Value').AsFloat,
      FieldByName('Duration').AsDateTime,
      FieldByName('Inverse').AsInteger <> 0
    );
  end;
end;

end.

