unit Adapt.EventGeo;
//------------------------------------------------------------------------------
// модуль адаптера кэша для TClassEventGeo
//------------------------------------------------------------------------------

//------------------------------------------------------------------------------
interface

uses
  SysUtils,
  ZConnection, ZDataset,
  Cache.Root;

//------------------------------------------------------------------------------
type
  TEventGeoMySQL = class(TCacheDataAdapterAbstract)
  private
    FReadConnection: TZConnection;
    FWriteConnection: TZConnection;
  public
    constructor Create(
      const ReadConnection: TZConnection;
      const WriteConnection: TZConnection
    );
    destructor Destroy(); override;
    procedure Flush(); override;
    procedure Action(
      const AObj: TCacheDataObjectAbstract;
      const AIsAdded: Boolean
    ); override;
    procedure DeleteRange(
      const AFrom: TDateTime;
      const ATo: TDateTime
    ); override;
    function GetDTBefore(
      const ADT: TDateTime
    ): TDateTime; override;
    function GetDTAfter(
      const ADT: TDateTime
    ): TDateTime; override;
    procedure LoadRange(
      const AFrom: TDateTime;
      const ATo: TDateTime;
      out RObjects: TCacheDataObjectAbstractArray
    ); override;
  end;

//------------------------------------------------------------------------------
implementation

//------------------------------------------------------------------------------
// TEventGeoMySQL
//------------------------------------------------------------------------------

constructor TEventGeoMySQL.Create(
  const ReadConnection: TZConnection;
  const WriteConnection: TZConnection
);
begin
  inherited Create();
  //
  FReadConnection := ReadConnection;
  FWriteConnection := WriteConnection;
end;

destructor TEventGeoMySQL.Destroy();
begin
  //
  inherited Destroy();
end;

procedure TEventGeoMySQL.Flush();
begin
//
end;

procedure TEventGeoMySQL.Action(
  const AObj: TCacheDataObjectAbstract;
  const AIsAdded: Boolean
);
begin
//
end;

procedure TEventGeoMySQL.DeleteRange(
  const AFrom: TDateTime;
  const ATo: TDateTime
);
begin
//
end;

function TEventGeoMySQL.GetDTBefore(
  const ADT: TDateTime
): TDateTime;
begin
//
end;

function TEventGeoMySQL.GetDTAfter(
  const ADT: TDateTime
): TDateTime;
begin
//
end;

procedure TEventGeoMySQL.LoadRange(
  const AFrom: TDateTime;
  const ATo: TDateTime;
  out RObjects: TCacheDataObjectAbstractArray
);
begin
//
end;

end.

