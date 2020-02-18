unit Adapter.MySQL;
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------

//------------------------------------------------------------------------------
interface

uses
  SysUtils,
  ZConnection, ZDataset,
  Cache.Root, Ils.Logger, Ils.Utils;

//------------------------------------------------------------------------------
const

//------------------------------------------------------------------------------
//! параметры запросов в БД
//------------------------------------------------------------------------------
  CSQLParamFromDT = 'dt_from';
  CSQLParamToDT = 'dt_to';
  CSQLParamDT = 'dt';

//------------------------------------------------------------------------------
type
  TCacheAdapterMySQL = class abstract(TCacheDataAdapterAbstract)
  protected
    FReadConnection: TZConnection;
    FWriteConnection: TZConnection;
    FInsertQry: TZQuery;
    FUpdateQry: TZQuery;
    FReadBeforeQry: TZQuery;
    FReadAfterQry: TZQuery;
    FReadRangeQry: TZQuery;
    FDeleteOneQry: TZQuery;
    //! заполнить дополнительные параметры из ключа
    procedure FillParamsFromKey(
      const AKey: TConnKey;
      const RQuery: TZQuery
    );
    //! создать объект из данных БД
    function MakeObjFromReadQuery(
      const AQuery: TZQuery
    ): TCacheDataObjectAbstract; virtual; abstract;
  public
    constructor Create(
      const AReadConnection: TZConnection;
      const AWriteConnection: TZConnection;
      const AReqInsert: string;
      const AReqUpdate: string;
      const AReqReadBefore: string;
      const AReqReadAfter: string;
      const AReqReadRange: string;
      const AReqDeleteOne: string
    );
    destructor Destroy(); override;
    //!
    procedure LoadRange(
      const ACache: TCache;
      const AFrom: TDateTime;
      const ATo: TDateTime;
      var RObjects: TCacheDataObjectAbstractArray
    ); override;
    //!
    function GetDTBefore(
      const ACache: TCache;
      const ADT: TDateTime
    ): TDateTime; override;
    //!
    function GetDTAfter(
      const ACache: TCache;
      const ADT: TDateTime
    ): TDateTime; override;
    //!
    procedure Delete(
      const ACache: TCache;
      const AObj: TCacheDataObjectAbstract
    ); override;
  end;

//------------------------------------------------------------------------------
implementation

//------------------------------------------------------------------------------
// TCacheAdapterMySQL
//------------------------------------------------------------------------------

constructor TCacheAdapterMySQL.Create(
  const AReadConnection: TZConnection;
  const AWriteConnection: TZConnection;
  const AReqInsert: string;
  const AReqUpdate: string;
  const AReqReadBefore: string;
  const AReqReadAfter: string;
  const AReqReadRange: string;
  const AReqDeleteOne: string
);
begin
  inherited Create();
  //
  FReadConnection := AReadConnection;
  FWriteConnection := AWriteConnection;
  //
  FInsertQry := TZQuery.Create(nil);
  FInsertQry.Connection := AWriteConnection;
  FInsertQry.SQL.Text := AReqInsert;
  FUpdateQry := TZQuery.Create(nil);
  FUpdateQry.Connection := AWriteConnection;
  FUpdateQry.SQL.Text := AReqUpdate;
  FReadBeforeQry := TZQuery.Create(nil);
  FReadBeforeQry.Connection := AReadConnection;
  FReadBeforeQry.SQL.Text := AReqReadBefore;
  FReadAfterQry := TZQuery.Create(nil);
  FReadAfterQry.Connection := AReadConnection;
  FReadAfterQry.SQL.Text := AReqReadAfter;
  FReadRangeQry := TZQuery.Create(nil);
  FReadRangeQry.Connection := AReadConnection;
  FReadRangeQry.SQL.Text := AReqReadRange;
  FDeleteOneQry := TZQuery.Create(nil);
  FDeleteOneQry.Connection := AWriteConnection;
  FDeleteOneQry.SQL.Text := AReqDeleteOne;
end;

destructor TCacheAdapterMySQL.Destroy();
begin
  FInsertQry.Free();
  FUpdateQry.Free();
  FReadBeforeQry.Free();
  FReadAfterQry.Free();
  FReadRangeQry.Free();
  FDeleteOneQry.Free();
  //
  inherited Destroy();
end;

procedure TCacheAdapterMySQL.LoadRange(
  const ACache: TCache;
  const AFrom: TDateTime;
  const ATo: TDateTime;
  var RObjects: TCacheDataObjectAbstractArray
);
var
  I: Integer;
//------------------------------------------------------------------------------
begin
  FillParamsFromKey(ACache.Key, FReadRangeQry);
  FReadRangeQry.ParamByName(CSQLParamFromDT).AsDateTime := AFrom;
  FReadRangeQry.ParamByName(CSQLParamToDT).AsDateTime := ATo;
  FReadRangeQry.Active := True;
  try
    try
      I := 0;
      FReadRangeQry.FetchAll();
      SetLength(RObjects, FReadRangeQry.RecordCount);
      FReadRangeQry.First();
      while not FReadRangeQry.Eof do
      begin
        RObjects[I] := MakeObjFromReadQuery(FReadRangeQry);
        Inc(I);
        FReadRangeQry.Next();
      end;
    finally
      FReadRangeQry.Active := False;
    end;
  except
    for I := Low(RObjects) to High(RObjects) do
    begin
      RObjects[I].Free();
    end;
    raise;
  end;
end;

function TCacheAdapterMySQL.GetDTBefore(
  const ACache: TCache;
  const ADT: TDateTime
): TDateTime;
begin
  FillParamsFromKey(ACache.Key, FReadBeforeQry);
  FReadBeforeQry.ParamByName(CSQLParamDT).AsDateTime := ADT;
  FReadBeforeQry.Active := True;
  FReadBeforeQry.FetchAll;
  try
    if (FReadBeforeQry.RecordCount <> 0) and (not FReadBeforeQry.FieldByName(CSQLParamDT).IsNull) then
      Result := FReadBeforeQry.FieldByName(CSQLParamDT).AsDateTime
    else
//    begin
//      ToLog(ClassName + ' ' + ACache.Key['imei'] + ' Before ' + DateTimeToIls(ADT));
      Result := 0;
//    end;
  finally
    FReadBeforeQry.Active := False;
  end;
end;

function TCacheAdapterMySQL.GetDTAfter(
  const ACache: TCache;
  const ADT: TDateTime
): TDateTime;
begin
  FillParamsFromKey(ACache.Key, FReadAfterQry);
  FReadAfterQry.ParamByName(CSQLParamDT).AsDateTime := ADT;
  FReadAfterQry.Active := True;
  FReadAfterQry.FetchAll;
  try
    if (FReadAfterQry.RecordCount <> 0) and (not FReadAfterQry.FieldByName(CSQLParamDT).IsNull) then
      Result := FReadAfterQry.FieldByName(CSQLParamDT).AsDateTime
    else
//    begin
//      ToLog(ClassName + ' ' + ACache.Key['imei'] + ' After ' + DateTimeToIls(ADT));
      Result := 0;
//    end;
  finally
    FReadAfterQry.Active := False;
  end;
end;

procedure TCacheAdapterMySQL.Delete(
  const ACache: TCache;
  const AObj: TCacheDataObjectAbstract
);
begin
  FillParamsFromKey(ACache.Key, FDeleteOneQry);
  FDeleteOneQry.ParamByName(CSQLParamDT).AsDateTime := AObj.DTMark;
  FDeleteOneQry.ExecSQL();
end;

procedure TCacheAdapterMySQL.FillParamsFromKey(
  const AKey: TConnKey;
  const RQuery: TZQuery
);
var
  Iter: TConnKeyPair;
//------------------------------------------------------------------------------
begin
  for Iter in AKey do
  begin
    RQuery.ParamByName(Iter.Key).AsString := Iter.Value;
  end;
end;

end.

