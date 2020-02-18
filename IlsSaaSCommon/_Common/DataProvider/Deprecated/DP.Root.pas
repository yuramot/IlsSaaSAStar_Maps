unit DP.Root;
//------------------------------------------------------------------------------
// модуль абстрактного корневого класса кэша
//------------------------------------------------------------------------------
// содержит:
//  <TBD>
//------------------------------------------------------------------------------
// ФУНКЦИИ КЭША-АДАПТЕРА К БД
//
// все функции изменения данных в БД контролируются флагом (DBWriteback), задаваемым при создании;
// то есть, если запись в БД запрещена, все изменения будут происходить с самим кэшем, из БД будет осуществляться только чтение
//
//  ClearCache - очистить кэш (не затрагивает БД)
//  Get - получить объект с запрошенным временем (загрузка из БД при необходимости)
//  GetBefore - получить объект с максимальным временем меньше запрошенного (загрузка из БД при необходимости)
//  GetAfter - получить объект с минимальным временем больше запрошенного (загрузка из БД при необходимости)
//  DeleteOne - удаление одного объекта (по времени)
//  DeleteRange - удаление группы объектов (по диапазону времени; удаление включает границы диапазона)
//  Insert - вставка нового объекта (отклоняется, если объект уже есть в кэше)
//    ^ именно тут происходит сокращение кэша при превышении количества объектов
//  WasChanged - информирует об обновлении объекта
//    ^ _необходимо_ вызвать после завершения редактирования объекта для сохранения его в БД
//------------------------------------------------------------------------------
// параметры конструктора:
//  DatabaseConfig: TMySQlDatabaseConfig - запись конфигурации БД
//  DBWriteback: Boolean - флаг разрешения обратной записи из кэша в БД
//  MaxKeepCount: Integer - максимальное число данных в кэше
//  LoadDelta: Double - расширение единоразво загружаемых данны, для минимизации количества запросов к БД (сутки = TDateTime)
//------------------------------------------------------------------------------

//------------------------------------------------------------------------------
interface

uses
  SysUtils, DateUtils, Math,
  System.Generics.Collections,
  Ils.Logger, Data.DB,
  ZConnection, ZDataset,
  Ils.MySql.Conf, Ils.Utils, Ils.Utils.Debug;

//------------------------------------------------------------------------------
type

//------------------------------------------------------------------------------
//! базовый абстрактный класс объекта данных (бывший интефейс)
//------------------------------------------------------------------------------
  IDataObj = class abstract //interface(IInterface)
  protected
    function GetDTMark(): TDateTime; virtual; abstract;
  public
    function Clone: IDataObj; virtual; abstract;
    property DTMark: TDateTime read GetDTMark;
  end;

//------------------------------------------------------------------------------
//! устаревшая прослойка
//------------------------------------------------------------------------------
  TDataObj = class(IDataObj)
  end;

//------------------------------------------------------------------------------
//! базовый абстрактный класс кэша данных
//------------------------------------------------------------------------------
  TCacheRoot = class abstract
  private
    //! увеличить объём массива хранения
    procedure Grow();
    //! получить индекс элемента с запрошенным или минимально большим временем
    //! *** может вернуть индекс больше последнего занятого
    function IndexOfEqualOrAbove(
      const ADT: TDateTime
    ): Integer;
    //! получить индекс элемента с запрошенным временем (вернёт -1, если элемент не найден)
    function IndexOf(
      const ADT: TDateTime
    ): Integer;
    //! проверка на нахождение в последних границах загрузки из БД (от вызов DBLoadRange)
    function OutOfLoadedBorders(
      const ADT: TDateTime
    ): Boolean;
    //! внутренняя реализация вставки; затрагивает только кэш, не БД
    function InternalAdd(
      const AObj: IDataObj
    ): Boolean;
    //! внутренняя реализация удаления одного элемента; затрагивает только кэш, не БД
    (*!
      return: True - если было удаление; False - если объекта с заданным временем не было в кэше
    *)
    function InternalDeleteOne(
      const ADT: TDateTime
    ): Boolean;
    //! внутренняя реализация удаления нескольких элементов; затрагивает только кэш, не БД
    (*!
      return: True - если осуществлена вставка; False - если объектов с заданным интервалом времён не было в кэше
    *)
    function InternalDeleteRange(
      const AFrom: TDateTime;
      const ATo: TDateTime
    ): Boolean;
    //! очистить часть кэша до времени
    procedure ClearCacheTill(
      const ADTTill: TDateTime
    );
    //! загрузить в кэш из БД промежуток данных
    //!  гарантирована загрузка в кэш данных из БД:
    //!   AFromDT - LoadDelta <= данные <= AToDT + LoadDelta
    //!  гарантирована непрерывность с уже имеющимися данными
    procedure DBLoadRange(
      const AFromDT: TDateTime;
      const AToDT: TDateTime
    );
    //! удалить данные из БД
    //! !!! из БД, не из кэша !!!
    //! !!! AFromDT <= удаляемые данные <= AToDT !!!
    procedure DBDeleteRange(
      const AFromDT: TDateTime;
      const AToDT: TDateTime
    );
    //! получить из БД ближайший по времени объект данных ДО заданного времени
    //!  не влияет на содержимое кэша
    function DBGetObjBefore(
      const ADT: TDateTime
    ): IDataObj;
    //! получить из БД ближайший по времени объект данных ПОСЛЕ заданного времени
    //!  не влияет на содержимое кэша
    function DBGetObjAfter(
      const ADT: TDateTime
    ): IDataObj;
  private
    FData: TArray<IDataObj>;
    FSize: Integer;
    FMaxKeepCount: Integer;
    FLoadDelta: Double;
    FDBWriteback: Boolean;
    FLastPresentDT: TDateTime;
    FDBLoadedFrom: TDateTime;
    FDBLoadedTo: TDateTime;
    FReadConnection: TZConnection;
    FWriteConnection: TZConnection;
  public
    type
      TProcessedEvent = procedure(const AObj: IDataObj; const ANew: Boolean) of object;
  protected
//    FQueryLastInsertID: TZQuery;
    FQueryReadRange: TZQuery;
    FQueryReadBefore: TZQuery;
    FQueryReadAfter: TZQuery;
    FQueryInsert: TZQuery;
    FQueryUpdate: TZQuery;
    FQueryDeleteRange: TZQuery;
    FQueryLastPresentDT: TZQuery;
    FOnProcessed: TProcessedEvent;
//    procedure OnOpenDS(
//      DataSet: TDataSet
//    );
  protected
    //! Перехватчики Событий
    procedure InternalExecDBInsert(
      const AObj: IDataObj
    ); virtual;
    //!
    procedure InternalExecDBUpdate(
      const AObj: IDataObj
    ); virtual;
    //!
    procedure DoProcessed(
      const AObj: IDataObj;
      const AUpdate: Boolean
    ); virtual;
    //! вставить запись в БД
    procedure ExecDBInsert(
      const AObj: IDataObj
    ); virtual; abstract;
    //! обновить запись в БД
    procedure ExecDBUpdate(
      const AObj: IDataObj
    ); virtual; abstract;
    //! преобразователь из записи БД в объект
    function MakeObjFromReadReq(
      const AQuery: TZQuery
    ): IDataObj; virtual; abstract;
  public
    constructor Create(
      const ReadConnection: TZConnection;
      const WriteConnection: TZConnection;
      const DBWriteback: Boolean;
      const MaxKeepCount: Integer;
      const LoadDelta: Double;
      const ReqReadRange: string;
      const ReqReadBefore: string;
      const ReqReadAfter: string;
      const ReqInsert: string;
      const ReqUpdate: string;
      const ReqDeleteRange: string;
      const ReqLastPresentDT: string;
      const AOnProcessed: TProcessedEvent = nil
    );
    destructor Destroy(); override;
    //
    procedure AfterConstruction(); override;
    //! очистить кэш
    procedure ClearCache();

    procedure CheckAndClearCache(ADT: TDateTime);
    //! команда изменения объекта
    procedure WasChanged(
      const AObj: IDataObj
    );
    //! вставка нового объекта
    (*!
      return: True - если осуществлена вставка; False - если объект уже существовал (вставка отклонена)
    *)
    function Add(
      const AObj: IDataObj
    ): Boolean;

    function AddOrGet(const AObj: IDataObj): IDataObj;

    //! удаление одного объекта
    procedure DeleteOne(
      const ADT: TDateTime
    );
    //! удаление группы объектов
    //! !!! пределы включены в удаляемый данные:
    //! !!! AFrom <= удаляемые данные <= ATo !!!
    procedure DeleteRange(
      const AFrom: TDateTime;
      const ATo: TDateTime
    );
    //! получить объект с запрошенным временем
    (*!
      return: nil - если объект не найден
    *)
    function Get(
      const ADT: TDateTime
    ): IDataObj;
    //! получить объект с максимальным временем меньше запрошенного
    (*!
      return: nil - если объект не найден
    *)
    function GetBefore(
      const ADT: TDateTime
    ): IDataObj;
    //! получить объект с минимальным временем больше запрошенного
    (*!
      return: nil - если объект не найден
    *)
    function GetAfter(
      const ADT: TDateTime
    ): IDataObj;
    //!
    property Count: Integer read FSize;
  end;

//------------------------------------------------------------------------------
//! словарь кэшей с автоматическим освобождением
//------------------------------------------------------------------------------
  TCacheDictionaryRoot<KeyType> = class(TDictionary<KeyType, TCacheRoot>)
  private
    procedure ValueEvent(
      Sender: TObject;
      const Item: TCacheRoot;
      Action: TCollectionNotification
    );
    function GetCountAll: Integer;
  public
    property CountAll: Integer read GetCountAll;
    constructor Create();
  end;

//------------------------------------------------------------------------------
implementation

const

//------------------------------------------------------------------------------
//! шаг увеличения буфера
//------------------------------------------------------------------------------
  CCacheChunkSize = 32;

//------------------------------------------------------------------------------
//! ID последней записи
//------------------------------------------------------------------------------
  CSQLLastInsertID =
    'select last_insert_id() as LID';

//------------------------------------------------------------------------------
//! параметры запросов в БД
//------------------------------------------------------------------------------
  CSQLParamFromDT = 'dt_from';
  CSQLParamToDT = 'dt_to';
  CSQLParamDT = 'dt';

//------------------------------------------------------------------------------
// TCacheRoot
//------------------------------------------------------------------------------

constructor TCacheRoot.Create(
  const ReadConnection: TZConnection;
  const WriteConnection: TZConnection;
  const DBWriteback: Boolean;
  const MaxKeepCount: Integer;
  const LoadDelta: Double;
  const ReqReadRange: string;
  const ReqReadBefore: string;
  const ReqReadAfter: string;
  const ReqInsert: string;
  const ReqUpdate: string;
  const ReqDeleteRange: string;
  const ReqLastPresentDT: string;
  const AOnProcessed: TProcessedEvent = nil
);
begin
  inherited Create();
  //
  FReadConnection := ReadConnection;
  FWriteConnection := WriteConnection;
  FDBWriteback := DBWriteback;
  FMaxKeepCount := MaxKeepCount;
  FLoadDelta := LoadDelta;
  FDBLoadedFrom := Infinity;
  FDBLoadedTo := NegInfinity;
  //
  FOnProcessed := AOnProcessed;
  //
  FQueryReadRange := TZQuery.Create(nil);
  FQueryReadRange.Connection := ReadConnection;
  FQueryReadRange.SQL.Text := ReqReadRange;
//  FQueryReadRange.BeforeOpen := OnOpenDS;
  FQueryReadBefore := TZQuery.Create(nil);
  FQueryReadBefore.Connection := ReadConnection;
  FQueryReadBefore.SQL.Text := ReqReadBefore;
//  FQueryReadBefore.BeforeOpen := OnOpenDS;
  FQueryReadAfter := TZQuery.Create(nil);
  FQueryReadAfter.Connection := ReadConnection;
  FQueryReadAfter.SQL.Text := ReqReadAfter;
//  FQueryReadAfter.BeforeOpen := OnOpenDS;
  FQueryInsert := TZQuery.Create(nil);
  FQueryInsert.Connection := WriteConnection;
  FQueryInsert.SQL.Text := ReqInsert;
//  FQueryInsert.BeforeOpen := OnOpenDS;
  FQueryUpdate := TZQuery.Create(nil);
  FQueryUpdate.Connection := WriteConnection;
  FQueryUpdate.SQL.Text := ReqUpdate;
//  FQueryUpdate.BeforeOpen := OnOpenDS;
  FQueryDeleteRange := TZQuery.Create(nil);
  FQueryDeleteRange.Connection := WriteConnection;
  FQueryDeleteRange.SQL.Text := ReqDeleteRange;
//  FQueryDeleteRange.BeforeOpen := OnOpenDS;
  FQueryLastPresentDT := TZQuery.Create(nil);
  FQueryLastPresentDT.Connection := ReadConnection;
  FQueryLastPresentDT.SQL.Text := ReqLastPresentDT;
//  FQueryLastPresentDT.BeforeOpen := OnOpenDS;
end;

destructor TCacheRoot.Destroy();
begin
  FQueryLastPresentDT.Free();
  FQueryDeleteRange.Free();
  FQueryUpdate.Free();
  FQueryInsert.Free();
  FQueryReadAfter.Free();
  FQueryReadBefore.Free();
  FQueryReadRange.Free();
  ClearCache();
  //
  inherited Destroy();
end;

//procedure TCacheRoot.OnOpenDS(
//  DataSet: TDataSet
//);
//var
//  I: Integer;
//  RS: string;
//begin
//// *** не забывать, что на ExecSQL не срабатывает; надо добавлять вызов вручную ***
////  (DataSet as TZQuery).Connection.PingServer;
//Exit;
//  RS := '';
//  for I := 0 to TZQuery(DataSet).Params.Count - 1 do
//  begin
//    RS := RS + TZQuery(DataSet).Params[I].Name + ' = ' + TZQuery(DataSet).Params[I].AsString + #13#10;
//  end;
//  RS := #13#10 + TZQuery(DataSet).SQL.Text + RS;
//  ToLog(RS);
//end;

function TCacheRoot.AddOrGet(const AObj: IDataObj): IDataObj;
begin
  Result := AObj;
  if not Add(AObj) then
    Result := Get(AObj.GetDTMark);
end;

procedure TCacheRoot.AfterConstruction();
begin
  inherited;
  //
  FLastPresentDT := NegInfinity;
  if not FDBWriteback then
    Exit;
  try
    FQueryLastPresentDT.Open;
    if (FQueryLastPresentDT.RecordCount > 0) then
      FLastPresentDT := FQueryLastPresentDT.FieldByName('DT').AsDateTime;
  finally
    FQueryLastPresentDT.Close;
  end;
end;

procedure TCacheRoot.ClearCache();
var
  I: Integer;
//------------------------------------------------------------------------------
begin
  FDBLoadedFrom := Infinity;
  FDBLoadedTo := NegInfinity;
  for I := 0 to FSize - 1 do
  begin
    FData[I].Free();
  end;
  FSize := 0;
  SetLength(FData, 0);
end;

procedure TCacheRoot.CheckAndClearCache(ADT: TDateTime);
var
  FromDT, ToDT: TDateTime;
  InCacheFromDT, InCacheToDT: TDateTime;
begin
  if (FSize = 0) then
    Exit;

  if (FSize >= FMaxKeepCount) then
    ClearCacheTill(ADT);

  FromDT := ADT - FLoadDelta;
  ToDT := ADT + FLoadDelta;

  InCacheToDT := FData[FSize - 1].DTMark;
  InCacheFromDT := FData[0].DTMark;

  if (FromDT > InCacheToDT) or
     (ToDT < InCacheFromDT) then
    ClearCache;
end;

procedure TCacheRoot.WasChanged(
  const AObj: IDataObj
);
var
  Poz: Integer;
  ObjDT: TDateTime;
//------------------------------------------------------------------------------
begin
  ObjDT := AObj.DTMark;
  Poz := IndexOf(ObjDT);
  // объект мог удалиться из кэша, например по превышении объёма хранения...
  if (Poz = -1) then
  begin
    // пытаемся перечитать данные из БД
    DBLoadRange(ObjDT, ObjDT);
    Poz := IndexOf(ObjDT);
    // ... и если его нет в БД, значит что-то не так
    if (Poz = -1) then
      raise Exception.CreateFmt('Объект с временной меткой "%s" был удалён из БД', [DateTimeToIls(ObjDT)]);
  end;
// обновление БД запускается сначала, чтобы не возникало расхождения кэша и БД, если возникнет ошибка запроса
  InternalExecDBUpdate(AObj);

  if (FData[Poz] = AObj) then
    Exit;

  FData[Poz].Free();
  FData[Poz] := AObj;
end;

function TCacheRoot.Add(
  const AObj: IDataObj
): Boolean;
var
  ObjDT: TDateTime;
  ObjClone: IDataObj;
//------------------------------------------------------------------------------
begin
// !!! ***
// на самом деле тут полная фигня, потому что нихрена не понятно - удалять ли объект при исключении
// *** !!!
  Result := False;
  ObjClone := nil;
  try
    ObjClone := AObj.Clone();
    ObjDT := ObjClone.DTMark;
    if OutOfLoadedBorders(ObjDT) then
      DBLoadRange(ObjDT, ObjDT);
    Result := InternalAdd(ObjClone);
    if Result and FDBWriteback then
    begin
      try
        InternalExecDBInsert(ObjClone);
      except
        // обеспечиваем согласованность при ошибке на вставке
        InternalDeleteOne(ObjDT);
        raise;
      end;
    end;
  finally
    if not Result then
      ObjClone.Free();
  end;
end;

procedure TCacheRoot.DeleteOne(
  const ADT: TDateTime
);
begin
  if InternalDeleteOne(ADT) and FDBWriteback then
    DBDeleteRange(ADT, ADT);
  //
//  InternalDeleteOne(ADT);
end;

procedure TCacheRoot.DeleteRange(
  const AFrom: TDateTime;
  const ATo: TDateTime
);
begin
  if InternalDeleteRange(AFrom, ATo) and FDBWriteback then
    DBDeleteRange(AFrom, ATo);
  //
//  InternalDeleteRange(AFrom, ATo);
end;

function TCacheRoot.Get(
  const ADT: TDateTime
): IDataObj;
var
  Index: Integer;
//------------------------------------------------------------------------------
begin
  Result := nil;
  if OutOfLoadedBorders(ADT) then
    DBLoadRange(ADT, ADT);
  Index := IndexOf(ADT);
  if (Index <> -1) then
    Result := FData[Index];
end;

function TCacheRoot.GetBefore(
  const ADT: TDateTime
): IDataObj;
var
  Index: Integer;
  TempDT: TDateTime;
  TempObj: IDataObj;
//------------------------------------------------------------------------------
begin
  Result := nil;
  if (FSize = 0) or (ADT <= FData[0].DTMark) or (ADT > FData[FSize - 1].DTMark) then
  begin
    if  (ADT >= FLastPresentDT) then
    begin // мы упёрлись в верхнюю границу данных - ищем только по кэшу
      Index := IndexOfEqualOrAbove(ADT) - 1;
      if (Index >= 0) then
        Result := FData[Index];
    end
    else
    begin
      TempObj := DBGetObjBefore(ADT);
      if not Assigned(TempObj) then
        Exit;
      TempDT := TempObj.DTMark;
      TempObj.Free();
      DBLoadRange(TempDT, ADT);
      Result := FData[IndexOf(TempDT)]; // не может не быть в границах (только что загрузили)
    end;
  end
  else
    Result := FData[IndexOfEqualOrAbove(ADT) - 1]; // тут IndexOfEqualOrAbove не может быть равным 0 (условие в if)
end;

function TCacheRoot.GetAfter(
  const ADT: TDateTime
): IDataObj;
var
  Index: Integer;
  TempDT: TDateTime;
  TempObj: IDataObj;
//------------------------------------------------------------------------------
begin
  Result := nil;
  if (FSize = 0) or (ADT < FData[0].DTMark) or (ADT >= FData[FSize - 1].DTMark) then
  begin
    if  (ADT >= FLastPresentDT) then
      Exit; // мы упёрлись в верхнюю границу данных, запрос будет бессмысленен
    TempObj := DBGetObjAfter(ADT);
    if not Assigned(TempObj) then
      Exit;
    TempDT := TempObj.DTMark;
    TempObj.Free();
    DBLoadRange(ADT, TempDT);
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

procedure TCacheRoot.Grow();
begin
  SetLength(FData, Length(FData) + CCacheChunkSize);
end;

function TCacheRoot.IndexOfEqualOrAbove(
  const ADT: TDateTime
): Integer;
var
  First, Last, Middle: Integer;
//------------------------------------------------------------------------------
begin
  if (FSize = 0) then
    Exit(0);
  if (FData[FSize - 1].DTMark < ADT) then
    Exit(FSize);
  First := 0;
  Last := FSize - 1;
  while (First < Last) do
  begin
    Middle := ((Last - First) shr 1) + First;
    if (FData[Middle].DTMark >= ADT) then
      Last := Middle
    else
      First := Middle + 1;
  end;
  Result := Last;
end;

function TCacheRoot.IndexOf(
  const ADT: TDateTime
): Integer;
begin
  Result := IndexOfEqualOrAbove(ADT);
  if (Result = FSize) or (FData[Result].DTMark <> ADT) then
    Result := -1;
end;

function TCacheRoot.OutOfLoadedBorders(
  const ADT: TDateTime
): Boolean;
begin
  Result := (ADT < FDBLoadedFrom) or (ADT > FDBLoadedTo);
end;

function TCacheRoot.InternalAdd(
  const AObj: IDataObj
): Boolean;
var
  Poz: Integer;
  ObjDT: TDateTime;
//------------------------------------------------------------------------------
begin
  if (FSize = Length(FData)) then
    Grow();
  ObjDT := AObj.DTMark;
  Poz := IndexOfEqualOrAbove(ObjDT);
  if (Poz <> FSize) then
  begin
    if (FData[Poz].DTMark <> ObjDT) then
    begin
      Move(FData[Poz], FData[Poz + 1], (FSize - Poz) * SizeOf(FData[0]));
//      FillChar(FData[Poz], SizeOf(FData[0]), 0);
    end
    else
      Exit(False);
  end;
  FData[Poz] := AObj;
  Inc(FSize);
  if FDBWriteback then
    FLastPresentDT := Max(FLastPresentDT, ObjDT);
  Result := True;
end;

function TCacheRoot.InternalDeleteOne(
  const ADT: TDateTime
): Boolean;
var
  Index: Integer;
//------------------------------------------------------------------------------
begin
  if (FSize = 0) or (ADT < FData[0].DTMark) or (ADT > FData[FSize - 1].DTMark) then
    Exit(True);
  Index := IndexOf(ADT);
  if (Index = -1) then
    Exit(False);
  Dec(FSize);
  FData[Index].Free();
  if (FSize <> Index) then // исключительно для предотвращения Range Check Error
    Move(FData[Index + 1], FData[Index], (FSize - Index) * SizeOf(FData[0]));
//  FillChar(FData[FSize], SizeOf(FData[0]), 0);
  Result := True;
end;

function TCacheRoot.InternalDeleteRange(
  const AFrom: TDateTime;
  const ATo: TDateTime
): Boolean;
var
  IndexF, IndexT: Integer;
  Idx: Integer;
//------------------------------------------------------------------------------
begin
  if (FSize = 0) or (ATo < FData[0].DTMark) or (AFrom > FData[FSize - 1].DTMark) then
    Exit(True);
  IndexF := IndexOfEqualOrAbove(AFrom);
  IndexT := IndexOfEqualOrAbove(ATo);
  if (IndexT <> FSize) and (FData[IndexT].DTMark = ATo) then
    Inc(IndexT); // следующий за последним элемент
  for Idx := IndexF to IndexT - 1 do
  begin
    FData[Idx].Free();
  end;
  if (FSize <> IndexT) then // исключительно для предотвращения Range Check Error
    Move(FData[IndexT], FData[IndexF], (FSize - IndexT) * SizeOf(FData[0]));
//  FillChar(FData[FSize - IndexT + IndexF], (FSize - IndexF) * SizeOf(FData[0]), 0);
  FSize := FSize - (IndexT - IndexF);
  Result := IndexT <> IndexF;
end;

procedure TCacheRoot.DoProcessed(const AObj: IDataObj; const AUpdate: Boolean);
begin
  if Assigned(FOnProcessed) then
    FOnProcessed(AObj, AUpdate);
end;

procedure TCacheRoot.InternalExecDBInsert(const AObj: IDataObj);
begin
  ExecDBInsert(AObj);
  DoProcessed(AObj, False);
end;

procedure TCacheRoot.InternalExecDBUpdate(const AObj: IDataObj);
begin
  ExecDBUpdate(AObj);
  DoProcessed(AObj, True);
end;

procedure TCacheRoot.ClearCacheTill(
  const ADTTill: TDateTime
);
var
  Index: Integer;
  I: Integer;
//------------------------------------------------------------------------------
begin
  Index := IndexOfEqualOrAbove(ADTTill - FLoadDelta);
  if (Index = 0) then
    Exit;
  if (Index = FSize) then
  begin
    ClearCache();
    Exit;
  end;
  for I := 0 to Index - 1 do
  begin
    FData[I].Free();
  end;
  FSize := FSize - Index;
  Move(FData[Index], FData[0], FSize * SizeOf(FData[0]));
//  FillChar(FData[FSize], Index * SizeOf(FData[0]), 0);
  FDBLoadedFrom := FData[0].DTMark;
end;

procedure TCacheRoot.DBLoadRange(
  const AFromDT: TDateTime;
  const AToDT: TDateTime
);
var
  FromDT, ToDT: TDateTime;
  InCacheFromDT, InCacheToDT: TDateTime;
  TempObj: IDataObj;
//------------------------------------------------------------------------------
begin
  FromDT := AFromDT - FLoadDelta;
  ToDT := AToDT + FLoadDelta;
  if (FSize <> 0) then
  begin
    InCacheToDT := FData[FSize - 1].DTMark;
    InCacheFromDT := FData[0].DTMark;
    if (AFromDT >= InCacheFromDT) and (AToDT <= InCacheToDT) then
      Exit; // уже закэшировано
    if (FromDT > InCacheToDT) then
      FromDT := InCacheToDT;
    if (ToDT < InCacheFromDT) then
      ToDT := InCacheFromDT;
  end;

//  if Abs(AToDT - AFromDT) > 1 then //(FLoadDelta * 2)
//    ToLog('загрузка диапазона больше суток: (' + FormatFloat('0.00', AToDT - AFromDT) + ') ' +
//      DateTimeToIls(AFromDT) + '-' + DateTimeToIls(AToDT) + ', imei: ' + FQueryReadRange.ParamByName('IMEI').AsString);

  FQueryReadRange.Close;
  FQueryReadRange.Params.ParamByName(CSQLParamFromDT).AsDateTime := FromDT;
  FQueryReadRange.Params.ParamByName(CSQLParamToDT).AsDateTime := ToDT;
  FQueryReadRange.Open;
  try
    FQueryReadRange.FetchAll();
    if (FQueryReadRange.RecordCount > 0) then
    begin
      FDBLoadedFrom := Min(FDBLoadedFrom, FromDT);
      FDBLoadedTo := Max(FDBLoadedTo, ToDT);
    end;
    FQueryReadRange.First();
    while not FQueryReadRange.Eof do
    begin
      TempObj := MakeObjFromReadReq(FQueryReadRange);
      if not InternalAdd(TempObj) then
        TempObj.Free();
      FQueryReadRange.Next();
    end;
  finally
    FQueryReadRange.Close;
  end;
end;

procedure TCacheRoot.DBDeleteRange(
  const AFromDT: TDateTime;
  const AToDT: TDateTime
);
begin
  FQueryDeleteRange.Close;
  FQueryDeleteRange.Params.ParamByName(CSQLParamFromDT).AsDateTime := AFromDT;
  FQueryDeleteRange.Params.ParamByName(CSQLParamToDT).AsDateTime := AToDT;
  FQueryDeleteRange.ExecSQL();
end;

function TCacheRoot.DBGetObjBefore(
  const ADT: TDateTime
): IDataObj;
begin
  Result := nil;
  FQueryReadBefore.Close;
  FQueryReadBefore.Params.ParamByName(CSQLParamDT).AsDateTime := ADT;
  FQueryReadBefore.Open;
  try
    if (FQueryReadBefore.RecordCount = 0) then
      Exit;
    Result := MakeObjFromReadReq(FQueryReadBefore);
  finally
    FQueryReadBefore.Close;
  end;
end;

function TCacheRoot.DBGetObjAfter(
  const ADT: TDateTime
): IDataObj;
begin
  Result := nil;
  FQueryReadAfter.Close;
  FQueryReadAfter.Params.ParamByName(CSQLParamDT).AsDateTime := ADT;
  FQueryReadAfter.Open;
  try
    if (FQueryReadAfter.RecordCount = 0) then
      Exit;
    Result := MakeObjFromReadReq(FQueryReadAfter);
  finally
    FQueryReadAfter.Close;
  end;
end;

//------------------------------------------------------------------------------
// TCacheDictionaryRoot<KeyType>
//------------------------------------------------------------------------------

constructor TCacheDictionaryRoot<KeyType>.Create();
begin
  inherited Create();
  //
  OnValueNotify := ValueEvent;
end;

function TCacheDictionaryRoot<KeyType>.GetCountAll: Integer;
var
  k: KeyType;
begin
  Result := 0;

  for k in Keys do
    Result := Result + Items[k].Count;
end;

procedure TCacheDictionaryRoot<KeyType>.ValueEvent(
  Sender: TObject;
  const Item: TCacheRoot;
  Action: TCollectionNotification
);
begin
  if (Action = cnRemoved) then
    Item.Free();
end;

end.

