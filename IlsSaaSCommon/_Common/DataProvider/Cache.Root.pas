unit Cache.Root;
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------

//------------------------------------------------------------------------------
interface

uses
  SysUtils, Math,
  System.Generics.Collections;

//------------------------------------------------------------------------------
const

//------------------------------------------------------------------------------
//! размер кэша при неуказании его
//------------------------------------------------------------------------------
  CCacheSizeMax = 10000;

//------------------------------------------------------------------------------
type

//------------------------------------------------------------------------------
//! результат функции AddOrReplace
//------------------------------------------------------------------------------
  TAORResult = (arAdded, arReplaced);

//------------------------------------------------------------------------------
//! forward
//------------------------------------------------------------------------------
  TCache = class;

//------------------------------------------------------------------------------
//! дополнительный ключ
//------------------------------------------------------------------------------
  TConnKey = class(TDictionary<string, string>)
  public
    constructor Create(
      const AKeys: array of string
    );
  end;

  TConnKeyPair = TPair<string, string>;

//------------------------------------------------------------------------------
//! корневой абстрактный класс данных
//------------------------------------------------------------------------------
  TCacheDataObjectAbstract = class abstract
  protected
    function GetDTMark(): TDateTime; virtual; abstract;
  public
    function Clone(): TCacheDataObjectAbstract; virtual; abstract;
    property DTMark: TDateTime read GetDTMark;
  end;

  TCacheDataObjectAbstractArray = TArray<TCacheDataObjectAbstract>;

//------------------------------------------------------------------------------
//! корневой абстрактный класс адаптера
//------------------------------------------------------------------------------
  TCacheDataAdapterAbstract = class abstract
  public
    //! обновить все изменённые объекты
    procedure Flush(
      const ACache: TCache
    ); virtual; abstract;
    //! добавление объекта
    procedure Add(
      const ACache: TCache;
      const AObj: TCacheDataObjectAbstract
    ); virtual; abstract;
    //! изменение объекта
    procedure Change(
      const ACache: TCache;
      const AObj: TCacheDataObjectAbstract
    ); virtual; abstract;
    //! удаление объекта
    procedure Delete(
      const ACache: TCache;
      const AObj: TCacheDataObjectAbstract
    ); virtual; abstract;
    //! получить ближайшее по времени время данных ДО заданного времени
    (*!
      return: 0 (30.12.1899) - если объект не существует
    *)
    function GetDTBefore(
      const ACache: TCache;
      const ADT: TDateTime
    ): TDateTime; virtual; abstract;
    //! получить ближайшее по времени время данных ПОСЛЕ заданного времени
    (*!
      return: 0 (30.12.1899) - если объект не существует
    *)
    function GetDTAfter(
      const ACache: TCache;
      const ADT: TDateTime
    ): TDateTime; virtual; abstract;
    //! подгрузить данные из источника
    procedure LoadRange(
      const ACache: TCache;
      const AFrom: TDateTime;
      const ATo: TDateTime;
      var RObjects: TCacheDataObjectAbstractArray
    ); virtual; abstract;
  end;

//------------------------------------------------------------------------------
//! кэш
//------------------------------------------------------------------------------
  TCache = class
  protected
    FData: TCacheDataObjectAbstractArray;
    FSize: Integer;
    FSizeMax: Integer;
    FAdapter: TCacheDataAdapterAbstract;
    FWriteback: Boolean;
    FLoadDelta: Double;
    FKey: TConnKey;
    FLoadedFrom: TDateTime;
    FLoadedTo: TDateTime;
    FMinAdapterDT: TDateTime;
    FMaxAdapterDT: TDateTime;
    FChanges: TDictionary<TDateTime, Integer>;
  private
    //! увеличить объём массива хранения
    procedure Grow();
    //! получить индекс элемента с запрошенным ИЛИ минимально большим временем
    //! *** может вернуть индекс больше последнего занятого (= FSize) ***
    function IndexOfEqualOrAbove(
      const ADT: TDateTime
    ): Integer;
    //! получить индекс элемента с запрошенным ИЛИ минимально меньшим временем
    //! *** может вернуть индекс меньше первого занятого (= -1) ***
    function IndexOfEqualOrLower(
      const ADT: TDateTime
    ): Integer;
    //! получить индекс элемента с запрошенным временем
    (*!
      return: -1 - если элемент не найден; иначе - индекс
    *)
    function IndexOf(
      const ADT: TDateTime
    ): Integer;
    //! внутренняя логика вставки нового элемента
    (*!
      return: True - если вставка успешна; False - в противном случае
    *)
    function InternalAdd(
      const AObj: TCacheDataObjectAbstract
    ): Boolean;
    //! внутренняя логика замены нового элемента
    (*!
      return: True - если замена успешна; False - в противном случае
    *)
    function InternalReplace(
      const AObj: TCacheDataObjectAbstract
    ): Boolean;
    //! внутренняя логика удаления элемента
    (*!
      return: ссылка на объект - если элемент был удалён; nil - если элемента не было в кэше
    *)
    function InternalDelete(
      const ADT: TDateTime
    ): TCacheDataObjectAbstract;
    //! проверка на нахождение даты/времени в границах загрузки из адаптера
    function InLoadedRange(
      const ADT: TDateTime
    ): Boolean;
    //! дозагрузить данные из адаптера
    procedure AdapterLoadRange(
      const AFrom: TDateTime;
      const ATo: TDateTime
    );
    function GetFirstDT: TDateTime;
    function GetLastDT: TDateTime;
  public
    //!
    (*
      Adapter - адаптер данных (не может быть nil)
      Writeback - отправлять ли изменения обратно в адаптер (если False - будет только чтение)
      LoadDelta - расширение диапазона загрузки (фактически - TDateTime)
      AKey - дополнительный ключ данных
    *)
    constructor Create(
      const AAdapter: TCacheDataAdapterAbstract;
      const AWriteback: Boolean;
      const ALoadDelta: Double;
      const ACacheSizeLimit: Integer;
      const AKey: TConnKey
    );
    //!
    destructor Destroy(); override;
    //! очистить кэш
    procedure Clear(); overload;
    //! очистить кэш до заданного времени
    procedure ClearTill(
      const ATill: TDateTime
    );
    //! очистить кэш после заданного времени
    procedure ClearFrom(
      const AFrom: TDateTime
    );
    procedure Clear(
      const AAround: TDateTime
    ); overload;

//    //! проверить нахождение времени в границах кэша; очистить кэш, если вне границ
//    procedure CheckAndClear(
//      const ADT: TDateTime
//    );
    //! добавить или заменить объект
    function AddOrReplace(
      const AObj: TCacheDataObjectAbstract
    ): TAORResult;
    //! заменить объект
    (*!
      return: True - объект заменён; False - объекта с таким временем нет в кэше
    *)
    function Replace(
      const AObj: TCacheDataObjectAbstract
    ): Boolean;
    //! добавить объект
    (*!
      return: True - объект добавлен; False - объект с таким временем уже есть в кэше
    *)
    function Add(
      const AObj: TCacheDataObjectAbstract
    ): Boolean;
    //! добавить объект; получить объект с заданным временем (переданный или из кэша)
    (*!
      return: объект
    *)
    function AddOrGet(
      const AObj: TCacheDataObjectAbstract
    ): TCacheDataObjectAbstract;
    //! сигнал о изменении объекта
    procedure WasChanged(
      const ADT: TDateTime
    );
    //!
    procedure MarkChanged(
      const ADT: TDateTime
    );
    //!
    procedure Flush(
    );
    //! удаление объекта
    function Delete(
      const ADT: TDateTime
    ): Boolean;
    //! удаление объекта, не затрагивающее БД - только из кэша
    procedure DeleteNotDB(
      const ADT: TDateTime
    );
    //! получить объект с запрошенным временем
    (*!
      return: nil - если объект не найден
    *)
    function Get(
      const ADT: TDateTime
    ): TCacheDataObjectAbstract;
    //! получить объект с максимальным временем меньше запрошенного
    (*!
      return: nil - если объект не найден
    *)
    function GetBefore(
      const ADT: TDateTime;
      const ACacheOnly: Boolean = False
    ): TCacheDataObjectAbstract;
    //! получить объект с минимальным временем больше запрошенного
    (*!
      return: nil - если объект не найден
    *)
    function GetAfter(
      const ADT: TDateTime;
      const ACacheOnly: Boolean = False
    ): TCacheDataObjectAbstract;
    //! количество объектов в кэше
    property Count: Integer read FSize;
    //! дополнительный ключ
    property Key: TConnKey read FKey;
  public // класс реализации for_in
    type
      TCacheEnumerator = class
      private
        FCache: TCache;
        FIdx: Integer;
        function GetCurrent(): TCacheDataObjectAbstract; inline;
      public
        constructor Create(ACache: TCache);
        property Current: TCacheDataObjectAbstract read GetCurrent;
        function MoveNext(): Boolean; inline;
      end;
  public // провайдер for_in
    function GetEnumerator(): TCacheEnumerator;

    function WillItBeCleaned(ADT: TDateTime): Boolean;
    property FirstDT: TDateTime read GetFirstDT;
    property LastDT: TDateTime read GetLastDT;
    property LoadedFrom: TDateTime read FLoadedFrom;
    property LoadedTo: TDateTime read FLoadedTo;
    property LoadDelta: Double read FLoadDelta;
    property Adapter: TCacheDataAdapterAbstract read FAdapter;
  end;

//------------------------------------------------------------------------------
//! словарь кэшей с автоматическим освобождением
//------------------------------------------------------------------------------
  TCacheDictionary<KeyType> = class(TDictionary<KeyType, TCache>)
  private
    procedure ValueEvent(
      Sender: TObject;
      const Item: TCache;
      Action: TCollectionNotification
    );
    function GetCountAll: Integer;
  public
    property CountAll: Integer read GetCountAll;
    constructor Create();
  end;

//------------------------------------------------------------------------------
implementation

resourcestring
  CNoAdapter = 'Отсутствует адаптер';

//------------------------------------------------------------------------------
const

//------------------------------------------------------------------------------
//! шаг увеличения буфера
//------------------------------------------------------------------------------
  CCacheChunkSize = 64;

//------------------------------------------------------------------------------
// TCache
//------------------------------------------------------------------------------

constructor TCache.Create(
  const AAdapter: TCacheDataAdapterAbstract;
  const AWriteback: Boolean;
  const ALoadDelta: Double;
  const ACacheSizeLimit: Integer;
  const AKey: TConnKey
);
begin
  inherited Create();
  //
  if not Assigned(AAdapter) then
    raise Exception.CreateRes(@CNoAdapter);
  FLoadedFrom := Infinity;
  FLoadedTo := NegInfinity;
  FMinAdapterDT := NegInfinity;
  FMaxAdapterDT := Infinity;
  FAdapter := AAdapter;
  FWriteback := AWriteback;
  FLoadDelta := ALoadDelta;
  FSizeMax := IfThen(ACacheSizeLimit <> 0, ACacheSizeLimit, CCacheSizeMax);
  FKey := AKey;
  FChanges := TDictionary<TDateTime, Integer>.Create;
end;

destructor TCache.Destroy();
begin
  Clear();
  FKey.Free();
  FChanges.Free;
  //
  inherited Destroy();
end;

procedure TCache.Clear();
var
  I: Integer;
//------------------------------------------------------------------------------
begin
  if Assigned(FAdapter) then // проверка на случай исключения в конструкторе
    FAdapter.Flush(Self);
  FLoadedFrom := Infinity;
  FLoadedTo := NegInfinity;
  FMinAdapterDT := NegInfinity;
  FMaxAdapterDT := Infinity;
  for I := 0 to FSize - 1 do
  begin
    FData[I].Free();
  end;
  FSize := 0;
  SetLength(FData, 0);
end;

procedure TCache.ClearTill(
  const ATill: TDateTime
);
var
  IdxLow: Integer;
  I: Integer;
//------------------------------------------------------------------------------
begin
  IdxLow := IndexOfEqualOrAbove(ATill - FLoadDelta);
  if (IdxLow = 0) then
    Exit;
  if (IdxLow = FSize) then
  begin
    Clear();
    Exit;
  end;
  if (FSize < FSizeMax) then
    Exit;
  FAdapter.Flush(Self);
  for I := 0 to IdxLow - 1 do
  begin
    FData[I].Free();
  end;
  FSize := FSize - IdxLow;
  Move(FData[IdxLow], FData[0], FSize * SizeOf(FData[0]));
  FLoadedFrom := FData[0].DTMark;
end;

procedure TCache.ClearFrom(
  const AFrom: TDateTime
);
var
  IdxHi: Integer;
  I: Integer;
//------------------------------------------------------------------------------
begin
  IdxHi := IndexOfEqualOrLower(AFrom + FLoadDelta);
  if (IdxHi = 0) then
    Exit;
  if (IdxHi < 0) then
  begin
    Clear();
    Exit;
  end;
  if (FSize < FSizeMax) then
    Exit;
  FAdapter.Flush(Self);
  for I := IdxHi + 1 to FSize - 1 do
  begin
    FData[I].Free();
  end;
  FSize := IdxHi;
  FLoadedTo := FData[FSize - 1].DTMark;
end;

procedure TCache.Clear(
  const AAround: TDateTime
);
begin
  ClearTill(AAround);
  ClearFrom(AAround);
end;

function TCache.AddOrReplace(
  const AObj: TCacheDataObjectAbstract
): TAORResult;
var
  ObjClone: TCacheDataObjectAbstract;
  TempDT: TDateTime;
//------------------------------------------------------------------------------
begin
  TempDT := AObj.DTMark;
  if not InLoadedRange(TempDT) then
    AdapterLoadRange(TempDT, TempDT);
  ObjClone := AObj.Clone();
  if InternalReplace(ObjClone) then
  begin
    if FWriteback then
      FAdapter.Change(Self, ObjClone);
    Result := arReplaced;
  end
  else
  begin
    InternalAdd(ObjClone);
    if FWriteback then
      FAdapter.Add(Self, ObjClone);
    Result := arAdded;
  end;
end;

function TCache.Replace(
  const AObj: TCacheDataObjectAbstract
): Boolean;
var
  ObjClone: TCacheDataObjectAbstract;
  TempDT: TDateTime;
//------------------------------------------------------------------------------
begin
  Result := False;
  ObjClone := AObj.Clone();
  try
    TempDT := ObjClone.DTMark;
    if not InLoadedRange(TempDT) then
      AdapterLoadRange(TempDT, TempDT);
    Result := InternalReplace(ObjClone);
    if Result and FWriteback then
      FAdapter.Change(Self, ObjClone);
  finally
    if not Result then
      ObjClone.Free();
  end;
end;

function TCache.Add(
  const AObj: TCacheDataObjectAbstract
): Boolean;
var
  ObjClone: TCacheDataObjectAbstract;
  TempDT: TDateTime;
//------------------------------------------------------------------------------
begin
  Result := False;
  ObjClone := AObj.Clone();
  try
    TempDT := ObjClone.DTMark;
    if not InLoadedRange(TempDT) then
      AdapterLoadRange(TempDT, TempDT);
    Result := InternalAdd(ObjClone);
    if Result and FWriteback then
      FAdapter.Add(Self, ObjClone);
  finally
    if not Result then
      ObjClone.Free();
  end;
end;

function TCache.AddOrGet(
  const AObj: TCacheDataObjectAbstract
): TCacheDataObjectAbstract;
begin
  Result := AObj;
  if not Add(AObj) then
    Result := Get(AObj.GetDTMark);
end;

procedure TCache.WasChanged(
  const ADT: TDateTime
);
var
  Poz: Integer;
//------------------------------------------------------------------------------
begin
  if not FWriteback then
    Exit;
  if not InLoadedRange(ADT) then
    AdapterLoadRange(ADT, ADT);
  Poz := IndexOf(ADT);
  if (Poz <> -1) then
    FAdapter.Change(Self, FData[Poz]);
end;

procedure TCache.MarkChanged(const ADT: TDateTime);
begin
  if not FChanges.ContainsKey(ADT) then
    FChanges.Add(ADT, 0);
  FChanges.Items[ADT] := FChanges.Items[ADT] + 1;
end;

procedure TCache.Flush;
var
  DT: TDateTime;
begin
  for DT in FChanges.Keys do
    WasChanged(DT);

  FChanges.Clear;
end;

function TCache.Delete(
  const ADT: TDateTime
): Boolean;
var
  Obj: TCacheDataObjectAbstract;
//------------------------------------------------------------------------------
begin
  Result := True;
  if not InLoadedRange(ADT) then
    AdapterLoadRange(ADT, ADT);
  Obj := InternalDelete(ADT);
  if not Assigned(Obj) then
    Exit(False);
  try
    if FWriteback then
      FAdapter.Delete(Self, Obj);
  finally
    Obj.Free();
  end;
end;

procedure TCache.DeleteNotDB(
  const ADT: TDateTime
);
begin
  if not InLoadedRange(ADT) then
    AdapterLoadRange(ADT, ADT);
  InternalDelete(ADT).Free();
end;

function TCache.Get(
  const ADT: TDateTime
): TCacheDataObjectAbstract;
var
  Index: Integer;
//------------------------------------------------------------------------------
begin
  Result := nil;
  if ADT = 0 then
    Exit;
  if not InLoadedRange(ADT) then
    AdapterLoadRange(ADT, ADT);
  Index := IndexOf(ADT);
  if (Index <> -1) then
    Result := FData[Index];
end;

function TCache.GetBefore(
  const ADT: TDateTime;
  const ACacheOnly: Boolean = False
): TCacheDataObjectAbstract;
var
  TempDT: TDateTime;
//------------------------------------------------------------------------------
begin
  Result := nil;
  if (FSize = 0) or (ADT <= FData[0].DTMark) or (ADT > FData[FSize - 1].DTMark) then
  begin
    if (FSize = 0) or (ADT <= FData[0].DTMark) or IsInfinite(FMaxAdapterDT) then
    begin
      if (ADT <= FMinAdapterDT) then // достигли предела данных в адаптере - читать не имеет смысла
        Exit;
      if ACacheOnly then
        Exit;
      TempDT := FAdapter.GetDTBefore(Self, ADT);
      if (TempDT = 0) then
      begin
        if (FSize <> 0) then
          FMinAdapterDT := FData[0].DTMark
        else
          FMinAdapterDT := ADT;
        Exit;
      end;
      AdapterLoadRange(TempDT, ADT);
      Result := FData[IndexOf(TempDT)]; // не может не быть в границах (только что загрузили)
    end
    else
      Result := FData[FSize - 1];
  end
  else
    Result := FData[IndexOfEqualOrAbove(ADT) - 1]; // тут IndexOfEqualOrAbove не может быть равным 0 (условие в if)
end;

function TCache.GetAfter(
  const ADT: TDateTime;
  const ACacheOnly: Boolean = False
): TCacheDataObjectAbstract;
var
  Index: Integer;
  TempDT: TDateTime;
//------------------------------------------------------------------------------
begin
  Result := nil;
  if (FSize = 0) or (ADT < FData[0].DTMark) or (ADT >= FData[FSize - 1].DTMark) then
  begin
    if ACacheOnly then
      Exit;

    if (ADT >= FMaxAdapterDT) then // достигли предела данных в адаптере - читать не имеет смысла
      Exit;
    TempDT := FAdapter.GetDTAfter(Self, ADT);
    if (TempDT = 0) then
    begin
      if (FSize <> 0) then
        FMaxAdapterDT := FData[FSize - 1].DTMark
      else
        FMaxAdapterDT := ADT;
      Exit;
    end;
    AdapterLoadRange(ADT, TempDT);
    Result := FData[IndexOf(TempDT)]; // не может не быть в границах (только что загрузили)
  end
  else
  begin
    Index := IndexOfEqualOrAbove(ADT); // Index не может быть равен FSize (условие в if)
    if (ADT = FData[Index].DTMark) then
      Inc(Index);
    Result := FData[Index]; // Index опять же не может быть равен FSize (условие в другом if)
  end;
end;

procedure TCache.Grow();
begin
  SetLength(FData, Length(FData) + CCacheChunkSize);
end;

function TCache.IndexOfEqualOrAbove(
  const ADT: TDateTime
): Integer;
var
  First, Last, Middle: Integer;
//------------------------------------------------------------------------------
begin
  if (FSize = 0) then
    Exit(0);
  if (ADT > FData[FSize - 1].DTMark) then
    Exit(FSize);
  First := 0;
  Last := FSize - 1;
  while (First < Last) do
  begin
    Middle := ((Last - First) shr 1) + First;
    if (ADT <= FData[Middle].DTMark) then
      Last := Middle
    else
      First := Middle + 1;
  end;
  Result := Last;
end;

function TCache.IndexOfEqualOrLower(
  const ADT: TDateTime
): Integer;
var
  First, Last, Middle: Integer;
//------------------------------------------------------------------------------
begin
  if (FSize = 0) then
    Exit(0);
  if (ADT < FData[0].DTMark) then
    Exit(-1);
  First := 0;
  Last := FSize - 1;
  while (First < Last) do
  begin
    Middle := ((Last - First) shr 1) + First;
    if (ADT >= FData[Middle].DTMark) then
      Last := Middle
    else
      First := Middle + 1;
  end;
  Result := Last;
end;

function TCache.IndexOf(
  const ADT: TDateTime
): Integer;
begin
  Result := IndexOfEqualOrAbove(ADT);
  if (Result = FSize) or (FData[Result].DTMark <> ADT) then
    Result := -1;
end;

function TCache.InternalAdd(
  const AObj: TCacheDataObjectAbstract
): Boolean;
var
  ObjDT: TDateTime;
  Poz: Integer;
//------------------------------------------------------------------------------
begin
  Result := True;
  if (FSize = Length(FData)) then
    Grow();
  ObjDT := AObj.DTMark;
  Poz := IndexOfEqualOrAbove(ObjDT);
  if (Poz = FSize) then // добавление в конец списка
    Inc(FSize)
  else if (FData[Poz].DTMark <> ObjDT) then // добавление в середину/начало списка
  begin
    Move(FData[Poz], FData[Poz + 1], (FSize - Poz) * SizeOf(FData[0]));
    Inc(FSize);
  end
  else // замена
    Result := False;
  if Result then
  begin
    FData[Poz] := AObj;
    FLoadedFrom := Min(FLoadedFrom, ObjDT);
    FLoadedTo := Max(FLoadedTo, ObjDT);
    FMinAdapterDT := Min(FMinAdapterDT, ObjDT);
    FMaxAdapterDT := Max(FMaxAdapterDT, ObjDT);
  end;
end;

function TCache.InternalReplace(
  const AObj: TCacheDataObjectAbstract
): Boolean;
var
  Poz: Integer;
//------------------------------------------------------------------------------
begin
  Poz := IndexOf(AObj.DTMark);
  Result := Poz <> -1;
  if Result then
  begin
    FData[Poz].Free();
    FData[Poz] := AObj;
  end;
end;

function TCache.WillItBeCleaned(ADT: TDateTime): Boolean;
var
  IdxLow,
  IdxHi: Integer;
begin
  IdxLow := IndexOfEqualOrAbove(ADT - FLoadDelta);
  IdxHi := IndexOfEqualOrLower(ADT + FLoadDelta);
  Result := (Count > 0) and ((IdxLow = FSize) or (IdxHi < 0));
end;

function TCache.InternalDelete(
  const ADT: TDateTime
): TCacheDataObjectAbstract;
var
  Index: Integer;
//------------------------------------------------------------------------------
begin
  Index := IndexOf(ADT);
  if (Index = -1) then
    Exit(nil);
  if (FMinAdapterDT = ADT) and (Index < FSize - 2) then
    FMinAdapterDT := FData[Index + 1].DTMark;
  if (FMaxAdapterDT = ADT) and (Index > 0) then
    FMaxAdapterDT := FData[Index - 1].DTMark;
  Result := FData[Index];
  Dec(FSize);
  if (FSize <> Index) then // исключительно для предотвращения Range Check Error
    Move(FData[Index + 1], FData[Index], (FSize - Index) * SizeOf(FData[0]));
end;

function TCache.InLoadedRange(
  const ADT: TDateTime
): Boolean;
begin
  Result := (FLoadedFrom <= ADT) and (ADT <= FLoadedTo);
end;

procedure TCache.AdapterLoadRange(
  const AFrom: TDateTime;
  const ATo: TDateTime
);
var
  NewFrom, NewTo: TDateTime;
  InCacheFromDT, InCacheToDT: TDateTime;
  NewData: TCacheDataObjectAbstractArray;
  I: Integer;
//------------------------------------------------------------------------------
begin
  NewFrom := AFrom - FLoadDelta;
  NewTo := ATo + FLoadDelta;
  if (FSize <> 0) then
  begin
    InCacheFromDT := FData[0].DTMark;
    InCacheToDT := FData[FSize - 1].DTMark;
    if (InCacheFromDT <= AFrom) and (ATo <= InCacheToDT) then
      Exit;
    if (NewFrom > InCacheToDT) then
      NewFrom := InCacheToDT;
    if (NewTo < InCacheFromDT) then
      NewTo := InCacheFromDT;
  end;
  FAdapter.LoadRange(Self, NewFrom, NewTo, NewData);
  for I := Low(NewData) to High(NewData) do
  begin
    if (IndexOf(NewData[I].DTMark) = -1) then
      InternalAdd(NewData[I])
    else
      NewData[I].Free();
  end;
  if (Length(NewData) <> 0) then
  begin
    FLoadedFrom := Min(FLoadedFrom, NewFrom);
    FLoadedTo := Max(FLoadedTo, NewTo);
  end;
end;

function TCache.GetEnumerator(): TCacheEnumerator;
begin
  Result := TCacheEnumerator.Create(Self);
end;

function TCache.GetFirstDT: TDateTime;
begin
  Result := 0;
  if FSize > 0 then
    Result := FData[0].DTMark;
end;

function TCache.GetLastDT: TDateTime;
begin
  Result := 0;
  if FSize > 0 then
    Result := FData[FSize - 1].DTMark;
end;

//------------------------------------------------------------------------------
// TCache.TCacheEnumerator
//------------------------------------------------------------------------------

constructor TCache.TCacheEnumerator.Create(
  ACache: TCache
);
begin
  inherited Create();
  //
  FCache := ACache;
  FIdx := -1;
end;

function TCache.TCacheEnumerator.GetCurrent(): TCacheDataObjectAbstract;
begin
  Result := FCache.FData[FIdx];
end;

function TCache.TCacheEnumerator.MoveNext(): Boolean;
begin
  Inc(FIdx);
  Result := FIdx < FCache.FSize;
end;

//------------------------------------------------------------------------------
// TCacheDictionaryRoot<KeyType>
//------------------------------------------------------------------------------

constructor TCacheDictionary<KeyType>.Create();
begin
  inherited Create();
  //
  OnValueNotify := ValueEvent;
end;

function TCacheDictionary<KeyType>.GetCountAll: Integer;
var
  k: KeyType;
begin
  Result := 0;

  for k in Keys do
    Result := Result + Items[k].Count;
end;

procedure TCacheDictionary<KeyType>.ValueEvent(
  Sender: TObject;
  const Item: TCache;
  Action: TCollectionNotification
);
begin
  if (Action = cnRemoved) then
    Item.Free();
end;

//------------------------------------------------------------------------------
// TConnKey
//------------------------------------------------------------------------------

constructor TConnKey.Create(
  const AKeys: array of string
);
var
  k, v: string;
  i: Integer;
//------------------------------------------------------------------------------
begin
  inherited Create();
  //
  i := 0;
  while (i + 1) < Length(AKeys) do
  begin
    k := AKeys[i];
    v := AKeys[i + 1];
    Add(k, v);
    Inc(i, 2);
  end;
end;

end.

