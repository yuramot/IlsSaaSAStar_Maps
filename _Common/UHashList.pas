unit UHashList;
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------

//------------------------------------------------------------------------------
interface

uses
  SysUtils,
  System.Generics.Collections;

//------------------------------------------------------------------------------
type

//------------------------------------------------------------------------------
//! словарь списков
//------------------------------------------------------------------------------
  THashList<TDictionaryStruct, TListStruct> = class
  private
    FContainer: TDictionary<TDictionaryStruct, TList<TListStruct>>;
  public
    constructor Create();
    destructor Destroy(); override;
    procedure Clear();
    procedure Insert(
      const AKey: TDictionaryStruct;
      const AValue: TListStruct
    );
    procedure InsertIfNotPresent(
      const AKey: TDictionaryStruct;
      const AValue: TListStruct
    );
    function GetList(
      const AKey: TDictionaryStruct;
      out RList: TList<TListStruct>
    ): Boolean;
    function ContainsKey(
      const AKey: TDictionaryStruct
    ): Boolean;
    function ContainsValue(
      const AValue: TListStruct
    ): Boolean;
    procedure RemoveKey(
      const AKey: TDictionaryStruct
    );
    procedure RemoveValue(
      const AValue: TListStruct
    );
    function Count(): Integer; overload;
    function Count(
      const AKey: TDictionaryStruct
    ): Integer; overload;
  end;

//------------------------------------------------------------------------------
implementation

//------------------------------------------------------------------------------
// THashList<TDictionaryStruct, TListStruct>
//------------------------------------------------------------------------------

constructor THashList<TDictionaryStruct, TListStruct>.Create();
begin
  inherited Create();
  //
  FContainer := TDictionary<TDictionaryStruct, TList<TListStruct>>.Create();
end;

destructor THashList<TDictionaryStruct, TListStruct>.Destroy();
begin
  Clear();
  FContainer.Free();
  //
  inherited Destroy();
end;

procedure THashList<TDictionaryStruct, TListStruct>.Clear();
var
  Iter: TDictionaryStruct;
  Flag: Boolean;
begin
  repeat
    Flag := True;
    for Iter in FContainer.Keys do
    begin
      FContainer.ExtractPair(Iter).Value.Free();
      Flag := False;
      Break;
    end;
  until Flag;
end;

procedure THashList<TDictionaryStruct, TListStruct>.Insert(
  const AKey: TDictionaryStruct;
  const AValue: TListStruct
);
begin
  if not FContainer.ContainsKey(AKey) then
    FContainer.Add(AKey, TList<TListStruct>.Create());
  FContainer[AKey].Add(AValue);
end;

procedure THashList<TDictionaryStruct, TListStruct>.InsertIfNotPresent(
  const AKey: TDictionaryStruct;
  const AValue: TListStruct
);
begin
  if not ContainsValue(AValue) then
    Insert(AKey, AValue);
end;

function THashList<TDictionaryStruct, TListStruct>.GetList(
  const AKey: TDictionaryStruct;
  out RList: TList<TListStruct>
): Boolean;
begin
  Result := FContainer.ContainsKey(AKey);
  if Result then
    RList := FContainer[AKey];
end;

function THashList<TDictionaryStruct, TListStruct>.ContainsKey(
  const AKey: TDictionaryStruct
): Boolean;
begin
  Result := FContainer.ContainsKey(AKey);
end;

function THashList<TDictionaryStruct, TListStruct>.ContainsValue(
  const AValue: TListStruct
): Boolean;
var
  Iter: TList<TListStruct>;
begin
  for Iter in FContainer.Values do
  begin
    if Iter.Contains(AValue) then
      Exit(True);
  end;
  Result := False;
end;

procedure THashList<TDictionaryStruct, TListStruct>.RemoveKey(
  const AKey: TDictionaryStruct
);
begin
  if FContainer.ContainsKey(AKey) then
    FContainer.ExtractPair(AKey).Value.Free();
end;

procedure THashList<TDictionaryStruct, TListStruct>.RemoveValue(
  const AValue: TListStruct
);
var
  Iter: TList<TListStruct>;
begin
  for Iter in FContainer.Values do
  begin
    if Iter.Contains(AValue) then
    begin
      Iter.Remove(AValue);
      Exit;
    end;
  end;
end;

function THashList<TDictionaryStruct, TListStruct>.Count(): Integer;
var
  Iter: TList<TListStruct>;
begin
  Result := 0;
  for Iter in FContainer.Values do
  begin
    Result := Result + Iter.Count;
  end;
end;

function THashList<TDictionaryStruct, TListStruct>.Count(
  const AKey: TDictionaryStruct
): Integer;
begin
  if not FContainer.ContainsKey(AKey) then
    Exit(0);
  Result := FContainer[AKey].Count;
end;

end.

