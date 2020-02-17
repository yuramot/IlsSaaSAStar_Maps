unit AStar64.Extra;
//------------------------------------------------------------------------------
// модуль дополнительной логики AStar
//
// содержит дополнительные типы и классы для работы AStar
//------------------------------------------------------------------------------

//------------------------------------------------------------------------------
interface

uses
  SysUtils, StrUtils, Types, Math, Windows,
  System.Generics.Collections, System.Generics.Defaults,
  Geo.Hash, Geo.Pos, Geo.Calcs, AStar64.FileStructs, UStructArray,
  AStar64.Common, AStar64.Areas, AStar64.Typ, AStar64.Files;

//------------------------------------------------------------------------------
const

//------------------------------------------------------------------------------
//! предел итераций особых начальных действий:
//!   - повторный поиск при заходе в тупик
//!   - отсутствие блокировки разворотов
//------------------------------------------------------------------------------
  CFirstSpecialIterations = 25;

//------------------------------------------------------------------------------
//! обратная к скорости для компоненты H (ч/км)
//------------------------------------------------------------------------------
  CBackwardCoeff = 1.0 / RS_reverse;

//------------------------------------------------------------------------------
type

//------------------------------------------------------------------------------
//! forward
//------------------------------------------------------------------------------
  TFileAgglomeration = class;
  TArea = class;
  TNode = class;
  TAStarList = class;

//------------------------------------------------------------------------------
//! статус ребра для алгоритма AStar
//!   nsOpen      - в открытом списке
//!   nsClosed    - в закрытом списке
//!   nsExcluded  - исключено из анализа
//------------------------------------------------------------------------------
  TNodeStatus = (nsUndefined, nsOpen, nsClosed, nsExcluded);

//------------------------------------------------------------------------------
//! TBD
//------------------------------------------------------------------------------
  TADir = (adForward, adBackward);

//------------------------------------------------------------------------------
//! узел A*
//------------------------------------------------------------------------------
//  INode = interface
//    function GetG: Double;
//    procedure SetG(Value: Double);
//    function GetH: Double;
//    procedure SetH(Value: Double);
//    function GetF: Double;
//    procedure SetF(Value: Double);
//    function GetStatus: TNodeStatus;
//    procedure SetStatus(Value: TNodeStatus);
//
//    function GetParentNode: INode;
//
//    property G: Double read GetG write SetG;
//    property H: Double read GetH write SetH;
//    property F: Double read GetF write SetF;
//    property Status: TNodeStatus read GetStatus write SetStatus;
//    property ParentNode: INode read GetParentNode;
//  end;

//  IUniversalWayPoint = interface
//
//  end;
//
//  TUniversalWayPoint = class(TInterfacedObject, IUniversalWayPoint)
//
//  end;
//
//  TUniversalWayPointArray = TArray<TUniversalWayPoint>;

  TUniversalWayPoint = record
//    p         : TWayPoint;
    Latitude  : Double;
    Longitude : Double;
    RType     : Integer;
    RSpeed1   : Integer;
    Zone1     : UInt64;
    constructor Create(const wp: TGeoPos; const aRoadType: Integer; const aSpeed: Integer; const aZone: UInt64); overload;
    function AsGeoPos: TGeoPos;
  end;

  TNode = class
  strict private
    //! указатель на предыдущий узел
    FParentNode: TNode;
    //! указатель на область
    FArea: TArea;
    //! прямая ссылка на ребро
    FEdge: PEdge;
    //! статус узла
    FStatus: TNodeStatus;
    //!
    FZone: TZoneControl;
    //!
    FSign: TSignControl;
  { strict private }
  private
    function GetDuration: Double;
    function GetReportedSpeed: Integer;
    function GetWayIndex: Integer;
    function GetWayCount: Integer;
    function GetDistance: Double;
    function GetWayPoints(aIdx: Integer; aDir: TADir): TUniversalWayPoint;
    function GetCoordsFrom: TGeoPos;
    function GetCoordsTo: TGeoPos;
    function GetHashVector: PHashVector;
  { private }
  public
    //! функция стоимости достижения рассматриваемой вершины из начальной
    G: Double;
    //! эвристическая оценка расстояния от рассматриваемой вершины к конечной
    H: Double;
    //! эвристическая функция f(x) = g(x) + h(x)
    F: Double;
    //!
    ReportedRoadSpeedsRecord: TRoadSpeedsRecord;

    function HasZone(AZonesMask: UInt64): Boolean;
    function HasSign(ASignsMask: UInt64): Boolean;

    constructor Create(
      const AParent: TNode;
      const AArea: TArea;
      const ADir: TADir;
      const AEdgeIndex: Integer
    );
    //!
    procedure FillNodeHeuristics(
      const ASrcLatitude: Double;
      const ASrcLongitude: Double;
      const ADestLatitude: Double;
      const ADestLongitude: Double;
      const ARoadSpeeds: TRoadSpeedsRecord
    );
    //!
    property Status: TNodeStatus read FStatus write FStatus;
    property ParentNode: TNode read FParentNode;
    property Area: TArea read FArea;
    property SEdge: PEdge read FEdge;
    property Zone: TZoneControl read FZone;
    property Sign: TSignControl read FSign;
    property Duration: Double read GetDuration;
    property Distance: Double read GetDistance;
    property ReportedSpeed: Integer read GetReportedSpeed;
    property WayIndex: Integer read GetWayIndex;
    property WayCount: Integer read GetWayCount;
    property WayPoints[aIdx: Integer; aDir: TADir]: TUniversalWayPoint read GetWayPoints;
    property CoordsFrom: TGeoPos read GetCoordsFrom;
    property CoordsTo: TGeoPos read GetCoordsTo;
    property HashVector: PHashVector read GetHashVector;
  end;

//------------------------------------------------------------------------------
//! списки узлов
//------------------------------------------------------------------------------
  TNodeArray = TArray<TNode>;

  TAstarHash = class(TDictionary<THashVector, TList<Integer>>)
  protected
    procedure ValueNotify(const Value: TList<Integer>; Action: TCollectionNotification); override;
  public
    procedure Add(const AHash: THashVector; const AIdx: Integer); overload;
    procedure Delete(const AHash: THashVector; const AIdx: Integer); overload;
    function GetConnected(const AHash: THashVector): Integer;
  end;

  TAstarHashBack = class(TDictionary<Byte, TAstarHash>)
  protected
    procedure ValueNotify(const Value: TAstarHash; Action: TCollectionNotification); override;
  const
    CTruncateMask: Int64 = $000000000000000F;
  public
    procedure Add(const AHash: THashVector; const AIdx: Integer); overload;
    procedure Delete(const AHash: THashVector; const AIdx: Integer); overload;
    function GetConnected(const AHash: THashVector): Integer;
  end;

  TAstarIntList = class(TList<Integer>)
    procedure Add(const AIdx: Integer); overload;
    procedure Delete(const AIdx: Integer); overload;
    destructor Destroy; override;
  end;

//------------------------------------------------------------------------------
//! специализированный список узлов
//! содержит реализицию низкоуровневых вспомогательных логик A*
//------------------------------------------------------------------------------

  TAStarList = class(TList<TNode>)
  private
//    ClosedHash: TAstarHashBack;
    ClosedHash: TAstarHash;
    OpenedList: TAstarIntList;
    ValuableNodes: TDictionary<THashVector, Integer>;
    function GetClosedCount: Integer;
    function GetOpenedCount: Integer;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Clear;
    //! получить узел для обработки; сразу переместить его в закрытый список (логика AStar)
    function GetWorkNode: TNode;
    //! добавлять только при помощи этих метода
    procedure AddNew(const ANode: TNode);
    procedure AddNewWithPresentCheck(const ANode: TNode);
    procedure AddNewSkipDict(const ANode: TNode);
    //! проверка узла из конца списка на дубль путей в него = выбор лучшего из двух путей
    function CheckLastEdge(const ANode: TNode; const APassCounter: Integer; const AZones: TZoneControl): Boolean;
    //! проверка на совпадение закрытых узлов с другим списком
    function GetConnection(const ATestingNode: TNode; const ARefList: TAStarList): TNode;
    //! статус менять только при помощи этого метода
    procedure ChangeStatus(const AIdx: Integer; const ANewStatus: TNodeStatus; const ADictIns: Boolean = True);
    function IsHashVectorPresent(const AHashVector: THashVector): Boolean;
    property ClosedCount: Integer read GetClosedCount;
    property OpenedCount: Integer read GetOpenedCount;
  end;

//------------------------------------------------------------------------------
//! класс контроля файлов области
//------------------------------------------------------------------------------

  TArea = class
  strict private
    FController: TFileAgglomeration;
  private
    FDataRec: THoldRec;
    FEdgeHashForward, FEdgeHashBackward: TEdgeDictionary;
    FEdgeHashListForward, FEdgeHashListBackward: TEdgeDictionaryList;
    function GetEdges(aDir: TADir): TEdgeArray;
    function GetEdgesHash(aDir: TADir): TEdgeDictionary;
    function GetEdgesHashList(aDir: TADir): TEdgeDictionaryList;
    function GetLinks(aDir: TADir): THashVectorArray;
    function GetZoneCtrls(aDir: TADir): TZoneControlArray;
    function GetSignCtrls(aDir: TADir): TSIgnControlArray;
    function GetWays(): TGeoPosArray;
  public
    //! гео-хэш области
    AreaHash: Int64;
    //!
    constructor Create(
      const AController: TFileAgglomeration;
      const ARootPath: string;
      const AAreaHash: Int64;
      const AAccounts: TIntegerDynArray
    );
    //!
    destructor Destroy(); override;
    //!
    procedure ParseRef(
      const AParentNode: TNode;
      const AReductionFactor: Integer;
      const ADir: TADir;
      const ARoadWorkSet: TRoadTypeSet;
      const ARoadSpeeds: TRoadSpeedsRecord;
      var RNewNodesList: TNodeArray;
      const AAccounts: TIntegerDynArray
    );
    //!
    property Edges[aDir: TADir]: TEdgeArray read GetEdges;
    property EdgesHash[aDir: TADir]: TEdgeDictionary read GetEdgesHash;
    property EdgesHashList[aDir: TADir]: TEdgeDictionaryList read GetEdgesHashList;
    property Links[aDir: TADir]: THashVectorArray read GetLinks;
    property ZoneCtrls[aDir: TADir]: TZoneControlArray read GetZoneCtrls;
    property SignCtrls[aDir: TADir]: TSignControlArray read GetSignCtrls;
    property Ways: TGeoPosArray read GetWays;
  end;

//------------------------------------------------------------------------------
//! хранитель списка файлов
//------------------------------------------------------------------------------
  TFileAgglomeration = class
  strict private
    //! корневой путь файлов графа
    FRootPath: string;
    //! список классов контроля файлов области
    FAreasHash: TDictionary<Int64, TArea>;
  public
    //!
    constructor Create();
    //!
    destructor Destroy(); override;
    //! загрузить область для заданнй точки
    function LoadArea(
      const APointHash: Int64;
      const AAccounts: TIntegerDynArray
    ): TArea;

    property RootPath: string read FRootPath;

    procedure Clear;
  end;

function Reverse(aDir: TADir): TADir;

//------------------------------------------------------------------------------
implementation

uses
  AStar64.Core;

//------------------------------------------------------------------------------
const

//------------------------------------------------------------------------------
//! кол-во элементов списка вершин TAStarList, выделяемых за раз
//------------------------------------------------------------------------------
  CListGrowAmount = 256;

//------------------------------------------------------------------------------
//!
//------------------------------------------------------------------------------
function Reverse(aDir: TADir): TADir;
begin
  Result := adForward;
  case aDir of
    adForward: Result := adBackward;
    adBackward: Result := adForward;
  end;
end;

{ TNode }

constructor TNode.Create(
  const AParent: TNode;
  const AArea: TArea;
  const ADir: TADir;
  const AEdgeIndex: Integer
);
begin
  inherited Create();
  //
  FParentNode := AParent;
  FArea := AArea;
  FEdge := @(FArea.Edges[ADir][AEdgeIndex]);
  FZone := FArea.ZoneCtrls[ADir][AEdgeIndex];
  FSign := FArea.SignCtrls[ADir][AEdgeIndex];
  FStatus := nsUndefined;
end;

procedure TNode.FillNodeHeuristics(
  const ASrcLatitude: Double;
  const ASrcLongitude: Double;
  const ADestLatitude: Double;
  const ADestLongitude: Double;
  const ARoadSpeeds: TRoadSpeedsRecord
);
var
  LenToDst: Double;
begin
  ReportedRoadSpeedsRecord := ARoadSpeeds;
  LenToDst := TGeoCalcs.GeoLengthKmDeg(CoordsTo.Latitude, CoordsTo.Longitude, ADestLatitude, ADestLongitude);
  G := FParentNode.G + Duration;
  H := LenToDst / CRoadSpeedRecordDef.GetSpeedByRoadType(RT_BackwardDef);
  F := H + G;
end;

function TNode.GetCoordsFrom: TGeoPos;
begin
  Result := FEdge.CoordsFrom;
end;

function TNode.GetCoordsTo: TGeoPos;
begin
  Result := FEdge.CoordsTo;
end;

function TNode.GetHashVector: PHashVector;
begin
  Result := @(FEdge.HashVector);
end;

function TNode.GetDistance: Double;
begin
  Result := FEdge.Distance;
end;

function TNode.GetDuration: Double;
begin
  Result := SafeDiv(FEdge.Distance, ReportedRoadSpeedsRecord.GetSpeedByRoadType(FEdge.RoadType));
end;

function TNode.GetReportedSpeed: Integer;
begin
  Result := ReportedRoadSpeedsRecord.GetSpeedByRoadType(FEdge.RoadType);
end;

function TNode.GetWayIndex: Integer;
begin
  Result := FEdge.WayIndex;
end;

function TNode.GetWayCount: Integer;
begin
  Result := FEdge.WayCount;
end;

function TNode.GetWayPoints(aIdx: Integer; aDir: TADir): TUniversalWayPoint;
var
  idx: Integer;
begin
  idx := aIdx + WayIndex;
  if aIdx = 0 then
  begin
    case aDir of
      adForward: Result := TUniversalWayPoint.Create(CoordsFrom, FEdge.RoadType, ReportedSpeed, Zone);
      adBackward: Result := TUniversalWayPoint.Create(CoordsTo, FEdge.RoadType, ReportedSpeed, Zone);
    end;
  end
  else
    Result := TUniversalWayPoint.Create(FArea.Ways[idx - 1], FEdge.RoadType, ReportedSpeed, Zone);
end;

function TNode.HasZone(AZonesMask: UInt64): Boolean;
begin
  Result := (AZonesMask and Zone) <> 0;
end;

function TNode.HasSign(ASignsMask: UInt64): Boolean;
begin
  Result := (ASignsMask and Sign) <> 0;
end;

{ TAStarList }

constructor TAStarList.Create;
begin
  inherited Create;
  //
  Capacity := CListGrowAmount;
//  ClosedHash := TAstarHashBack.Create;
  ClosedHash := TAstarHash.Create;
  OpenedList := TAstarIntList.Create;
  ValuableNodes := TDictionary<THashVector, Integer>.Create;
end;

destructor TAStarList.Destroy;
begin
  Clear;
  ValuableNodes.Free;
  OpenedList.Free;
  ClosedHash.Free;
  //
  inherited Destroy;
end;

function TAStarList.GetClosedCount: Integer;
begin
  Result := ClosedHash.Count;
end;

function TAStarList.GetOpenedCount: Integer;
begin
  Result := OpenedList.Count;
end;

function TAStarList.GetWorkNode: TNode;
var
  MinF: Double;
  RezIndex: Integer;
  I: Integer;
begin
  Result := nil;
  RezIndex := -1;
  MinF := Infinity;
  for I in OpenedList do
  begin
    if (Items[I].F < MinF) then
    begin
      RezIndex := I;
      MinF := Items[I].F;
    end;
  end;
  if (RezIndex <> -1) then
  begin
    ChangeStatus(RezIndex, nsClosed); // сразу в закрытый список
    Result := Items[RezIndex];
  end;
end;

function TAStarList.IsHashVectorPresent(const AHashVector: THashVector): Boolean;
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
  begin
    if Items[I].HashVector^ = AHashVector then
      Exit(True);
  end;
  Result := False;
end;

procedure TAStarList.AddNew(const ANode: TNode);
begin
//  if IsHashVectorPresent(ANode.HashVector^) then
//    Exit;
  if (Count = Capacity) then
    Capacity := Capacity + CListGrowAmount;
  Add(ANode);
  ChangeStatus(Count - 1, nsOpen);
end;

procedure TAStarList.AddNewWithPresentCheck(const ANode: TNode);
begin
  if IsHashVectorPresent(ANode.HashVector^) then
    Exit;
  if (Count = Capacity) then
    Capacity := Capacity + CListGrowAmount;
  Add(ANode);
  ChangeStatus(Count - 1, nsOpen);
end;

procedure TAStarList.AddNewSkipDict(const ANode: TNode);
begin
  if (Count = Capacity) then
    Capacity := Capacity + CListGrowAmount;
  Add(ANode);
  ChangeStatus(Count - 1, nsOpen, False);
end;

procedure TAStarList.ChangeStatus(const AIdx: Integer; const ANewStatus: TNodeStatus; const ADictIns: Boolean = True);
var
  n: TNode;
begin
  n := Items[AIdx];
  if (n.Status <> ANewStatus) then
  begin
    n.Status := ANewStatus;
    case ANewStatus of
      nsOpen: begin
        if ADictIns then
          ValuableNodes.Add(n.HashVector^, AIdx);
        ClosedHash.Delete(n.HashVector^, AIdx);
        OpenedList.Add(AIdx);
      end;
      nsClosed: begin
        ClosedHash.Add(n.HashVector^, AIdx);
        OpenedList.Delete(AIdx);
      end;
      else begin
        ValuableNodes.Remove(n.HashVector^);
        ClosedHash.Delete(n.HashVector^, AIdx);
        OpenedList.Delete(AIdx);
      end;
    end;
  end;
end;

(*
function TAStarList.CheckLastEdge(const ANode: TNode; const APassCounter: Integer; const AZones: TZoneControl): Boolean;
var
  //!
  I, J: Integer;
  //!
  LastN, LastParentN, CheckedN: TNode;
  LastNIdx: Integer;
//------------------------------------------------------------------------------
begin
  Result := True;
  LastN := Last();
  LastNIdx := Count - 1;
  LastParentN := LastN.ParentNode;
  if not Assigned(LastParentN) then Exit;
  // проверка на дорогу обратно
  if (APassCounter > CFirstSpecialIterations) // не отклонять обратные пути в начале
  and (LastParentN.HashVector.PointFrom = LastN.HashVector.PointTo)
  and (LastParentN.HashVector.PointTo = LastN.HashVector.PointFrom) then
  begin
    ChangeStatus(LastNIdx, nsExcluded);
    Exit;
  end;
  //
  for I := 0 to Count - 2 do
  begin
    CheckedN := Items[I];
    // особые случаи
    if not Assigned(CheckedN.ParentNode) then Continue; // пропуск первичных рёбер
    if (CheckedN = LastParentN) then Continue; // пропуск родительского ребра
    if (CheckedN.Status <> nsClosed) then Continue; // пропуск нерассчитанных рёбер
    // выбор лучшего
    if (CheckedN.HashVector.PointFrom = LastN.HashVector.PointFrom)
    and (CheckedN.HashVector.PointTo = LastN.HashVector.PointTo) then
    begin
      // а что лучше?
      if (CheckedN.F <= LastN.F) then
      begin
        ChangeStatus(LastNIdx, nsExcluded);
      end
      else
      begin
        ChangeStatus(I, nsExcluded);
        // убиваем старый маршрут - со всеми ветками
        // мы можем начинать с (I+1), т.к. не может быть ссылок на вершины, добавленные позже
        // ^ по этой же причине (ссылки) нам достаточно прогнать цикл один раз
        for J := I + 1 to Count - 2 do
        begin
          if Assigned(Items[J].ParentNode) and (Items[J].ParentNode.Status = nsExcluded) then
            ChangeStatus(J, nsExcluded);
        end;
      end;
      // так или иначе - анализ завершён
      Exit;
    end;
  end;
end;
//*)

//(*
function TAStarList.CheckLastEdge(const ANode: TNode; const APassCounter: Integer; const AZones: TZoneControl): Boolean;
var
  J: Integer;
  LastParentN: TNode;
  CheckedNIdx: Integer;
begin
  Result := False;
  if ANode.HasZone(AZones) then
    Exit;
  LastParentN := ANode.ParentNode;
  if (APassCounter > CFirstSpecialIterations) // не отклонять обратные пути в начале
  and (LastParentN.HashVector.PointFrom = ANode.HashVector.PointTo)
  and (LastParentN.HashVector.PointTo = ANode.HashVector.PointFrom) then
    Exit;
  if not ValuableNodes.ContainsKey(ANode.HashVector^) then
  begin
    AddNew(ANode);
    Exit(True);
  end;
  CheckedNIdx := ValuableNodes.Items[ANode.HashVector^];
  if (Items[CheckedNIdx].F <= ANode.F) then
    Exit;
VLog('new better');
  ChangeStatus(CheckedNIdx, nsExcluded);
  // убиваем старый маршрут - со всеми ветками
  // мы можем начинать с (CheckedNIdx+1), т.к. не может быть ссылок на вершины, добавленные позже
  // ^ по этой же причине (ссылки) нам достаточно прогнать цикл один раз
  for J := CheckedNIdx + 1 to Count - 2 do
  begin
    if Assigned(Items[J].ParentNode) and (Items[J].ParentNode.Status = nsExcluded) then
      ChangeStatus(J, nsExcluded);
  end;
  AddNew(ANode);
  Result := True;
end;
//*)

procedure TAStarList.Clear;
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
    Items[I].Free();
  ClosedHash.Clear;
  OpenedList.Clear;
  ValuableNodes.Clear;
  //
  inherited;
end;

function TAStarList.GetConnection(
  const ATestingNode: TNode;
  const ARefList: TAStarList
): TNode;
var
  I: Integer;
begin
  I := ARefList.ClosedHash.GetConnected(ATestingNode.HashVector.Reverse);
  if I >= 0 then
    Result := ARefList.Items[I]
  else
    Result := nil;
end;

{ TArea }

constructor TArea.Create(
  const AController: TFileAgglomeration;
  const ARootPath: string;
  const AAreaHash: Int64;
  const AAccounts: TIntegerDynArray
);

  procedure LoadEdges(
    var AEdge: TEdgeArray;
    var REdgeHash: TEdgeDictionary;
    var REdgeHashArray: TEdgeDictionaryList
  );
  var
    i: Integer;
  begin
    if Length(AEdge) = 0 then
      Exit;

    for i := Low(AEdge) to High(AEdge) do
      if not (AEdge[i].RoadType in  [RT_service, RT_pedestrian]) then
      begin
        REdgeHash.AddOrSetValue(AEdge[i].HashVector, i);
        REdgeHashArray.Add(AEdge[i].HashVector.HashFrom, i);
      end;
  end;

begin
  inherited Create();
  //
  FController := AController;
  AreaHash := AAreaHash;
  //
//  TGFAdapter.Load('root', ['acc''s'], 'hash', FDataRec);
  TGFAdapter.Load(ARootPath, AAccounts, TGeoHash.ConvertBinToString(AreaHash, 5), FDataRec);
  //
  FEdgeHashForward := TEdgeDictionary.Create(Length(FDataRec.EdgeForward) * 2);
  FEdgeHashBackward := TEdgeDictionary.Create(Length(FDataRec.EdgeBackward) * 2);
  FEdgeHashListForward := TEdgeDictionaryList.Create(Length(FDataRec.EdgeForward) * 2);
  FEdgeHashListBackward := TEdgeDictionaryList.Create(Length(FDataRec.EdgeBackward) * 2);
  //
  LoadEdges(FDataRec.EdgeForward, FEdgeHashForward, FEdgeHashListForward);
  LoadEdges(FDataRec.EdgeBackward, FEdgeHashBackward, FEdgeHashListBackward);
end;

destructor TArea.Destroy();
begin
  SetLength(FDataRec.EdgeForward, 0);
  SetLength(FDataRec.EdgeBackward, 0);
  SetLength(FDataRec.ListForward, 0);
  SetLength(FDataRec.ListBackward, 0);
  SetLength(FDataRec.ZCForward, 0);
  SetLength(FDataRec.ZCBackward, 0);
  SetLength(FDataRec.SCForward, 0);
  SetLength(FDataRec.SCBackward, 0);
  SetLength(FDataRec.Ways, 0);
  //
  FEdgeHashForward.Free;
  FEdgeHashBackward.Free;
  FEdgeHashListForward.Free;
  FEdgeHashListBackward.Free;
  //
  inherited Destroy();
end;

function TArea.GetEdges(aDir: TADir): TEdgeArray;
begin
  case aDir of
    adForward: Result := FDataRec.EdgeForward;
    adBackward: Result := FDataRec.EdgeBackward;
  end;
end;

function TArea.GetEdgesHash(aDir: TADir): TEdgeDictionary;
begin
  Result :=  nil;
  case aDir of
    adForward: Result := FEdgeHashForward;
    adBackward: Result := FEdgeHashBackward;
  end;
end;

function TArea.GetEdgesHashList(aDir: TADir): TEdgeDictionaryList;
begin
  Result := nil;
  case aDir of
    adForward: Result := FEdgeHashListForward;
    adBackward: Result := FEdgeHashListBackward;
  end;
end;

function TArea.GetLinks(aDir: TADir): THashVectorArray;
begin
  case aDir of
    adForward: Result := FDataRec.ListForward;
    adBackward: Result := FDataRec.ListBackward;
  end;
end;

function TArea.GetZoneCtrls(aDir: TADir): TZoneControlArray;
begin
  case aDir of
    adForward: Result := FDataRec.ZCForward;
    adBackward: Result := FDataRec.ZCBackward;
  end;
end;

function TArea.GetSignCtrls(aDir: TADir): TSignControlArray;
begin
  case aDir of
    adForward: Result := FDataRec.SCForward;
    adBackward: Result := FDataRec.SCBackward;
  end;
end;

function TArea.GetWays(): TGeoPosArray;
begin
  Result := FDataRec.Ways;
end;

procedure TArea.ParseRef(
  const AParentNode: TNode;
  const AReductionFactor: Integer;
  const ADir: TADir;
  const ARoadWorkSet: TRoadTypeSet;
  const ARoadSpeeds: TRoadSpeedsRecord;
  var RNewNodesList: TNodeArray;
  const AAccounts: TIntegerDynArray
);

  function GetWorkArea(AHash: Int64): TArea;
  begin
    if (AHash and CAreaHashMask) = AreaHash then
      Result := Self
    else
      Result := FController.LoadArea(AHash, AAccounts);
  end;

var
  I, J: Integer;
  OutCounter: Integer;
  LinksKey: THashVector;
  WorkEdge: PEdge;
  WorkArea: TArea;
  EdgesArray: TEdgeArray;
  EdgesHash: TEdgeDictionary;
  RoadWorkSet: TRoadTypeSet;
  ReductionFactor: Integer;
begin
  OutCounter := 0;
  SetLength(RNewNodesList, 0);
  //
  Assert(Assigned(AParentNode));
  WorkEdge := AParentNode.SEdge;
  WorkArea := GetWorkArea(WorkEdge.HashVector.HashTo);
  if not Assigned(WorkArea) then
    Exit;
  for I := WorkEdge.LinkIndex to WorkEdge.LinkIndex + WorkEdge.LinkCount - 1 do
  begin
    //
    EdgesArray := WorkArea.Edges[ADir];
    EdgesHash := WorkArea.EdgesHash[ADir];
    LinksKey := Links[ADir][I];
    //
    ReductionFactor := AReductionFactor;
    repeat
      RoadWorkSet := GetReducedRoadWorkSet(ARoadWorkSet, ReductionFactor);
      if EdgesHash.ContainsKey(LinksKey) then
      begin
        J := EdgesHash.Items[LinksKey];
        if IsRoadFits(EdgesArray[J].RoadType, RoadWorkSet) then
        begin
          SetLength(RNewNodesList, OutCounter + 1);
          RNewNodesList[OutCounter] := TNode.Create(AParentNode, WorkArea, ADir, J);
          Inc(OutCounter);
        end;
      end;
      if OutCounter <= 0 then
        Dec(ReductionFactor);
    until (OutCounter > 0) or (ReductionFactor < CReductionFactorMin) ;
  end;
end;

{ TFileAgglomeration }

constructor TFileAgglomeration.Create();
begin
  inherited Create();
  //
  FRootPath := ExtractFilePath(GetModuleName(HInstance));
  FAreasHash := TDictionary<Int64, TArea>.Create(128);
end;

destructor TFileAgglomeration.Destroy();
var
  K: Int64;
begin
  for K in FAreasHash.Keys do
    FAreasHash.Items[K].Free;
  FAreasHash.Clear;
  FAreasHash.Free;
  //
  inherited Destroy();
end;

function TFileAgglomeration.LoadArea(
  const APointHash: Int64;
  const AAccounts: TIntegerDynArray
): TArea;
var
  AreaHash: Int64;
begin
  // в строку
  AreaHash := APointHash and CAreaHashMask;
  // а может уже загружено?
  if FAreasHash.ContainsKey(AreaHash) then
    Exit(FAreasHash.Items[AreaHash]);
  // ...нет
  Result := nil;
  try
    Result := TArea.Create(Self, FRootPath, AreaHash, AAccounts);
    FAreasHash.Add(AreaHash, Result);
  except
    on E: ENoGraphFile do
    begin
      // ничего - возвращаем nil
    end;
    on E: Exception do
    begin
      raise;
    end;
  end;
end;

procedure TFileAgglomeration.Clear;
begin
  FAreasHash.Clear;
end;

{ TAstarHashBack }

procedure TAstarHashBack.Add(const AHash: THashVector; const AIdx: Integer);
var
  Hash: Byte;
begin
  Hash := AHash.TruncateToByte;
  if not ContainsKey(Hash) then
    Add(Hash, TAstarHash.Create(64));

  Items[Hash].Add(AHash, AIdx);
end;

procedure TAstarHashBack.Delete(const AHash: THashVector; const AIdx: Integer);
var
  Hash: Byte;
begin
  Hash := AHash.TruncateToByte;
  if ContainsKey(Hash) then
    Items[Hash].Delete(AHash, AIdx);
end;

function TAstarHashBack.GetConnected(const AHash: THashVector): Integer;
var
  Hash: Byte;
begin
  Hash := AHash.TruncateToByte;
  Result := -1;
  if ContainsKey(Hash) then
    Result := Items[Hash].GetConnected(AHash);
end;

procedure TAstarHashBack.ValueNotify(const Value: TAstarHash; Action: TCollectionNotification);
begin
  if Action <> cnAdded then
    Value.Free;
end;

{ TAstarIntList }

procedure TAstarIntList.Add(const AIdx: Integer);
begin
  if not Contains(AIdx) then
    inherited Add(AIdx);
end;

procedure TAstarIntList.Delete(const AIdx: Integer);
begin
  if Contains(AIdx) then
    inherited Delete(IndexOf(AIdx));
end;

destructor TAstarIntList.Destroy;
begin
  Clear;
  inherited;
end;

{ TUniversalWayPoint }

constructor TUniversalWayPoint.Create(const wp: TGeoPos; const aRoadType, aSpeed: Integer; const aZone: UInt64);
begin
  Latitude  := wp.Latitude;
  Longitude := wp.Longitude;
  RType     := aRoadType;
  RSpeed1   := aSpeed;
  Zone1     := aZone;
end;

function TUniversalWayPoint.AsGeoPos: TGeoPos;
begin
  Result := TGeoPos.Create(Latitude, Longitude);
end;

{ TAstarHash }

procedure TAstarHash.Add(const AHash: THashVector; const AIdx: Integer);
var
  l: TList<Integer>;
begin
  if ContainsKey(AHash) then
  begin
    l := Items[AHash];
    if not l.Contains(AIdx) then
      l.Add(AIdx);
  end
  else
  begin
    Add(AHash, TList<Integer>.Create);
    Items[AHash].Add(AIdx);
  end;
end;

procedure TAstarHash.Delete(const AHash: THashVector; const AIdx: Integer);
var
  idx: Integer;
  l: TList<Integer>;
begin
  if ContainsKey(AHash) then
  begin
    l := Items[AHash];
    idx := l.IndexOf(AIdx);
    if idx >= 0 then
      l.Delete(idx);
  end
end;

function TAstarHash.GetConnected(const AHash: THashVector): Integer;
var
  l: TList<Integer>;
begin
  Result := -1;
  if ContainsKey(AHash) then
  begin
    l := Items[AHash];
    if l.Count > 0 then
      Result := l[0];
  end;
end;

procedure TAstarHash.ValueNotify(const Value: TList<Integer>;
  Action: TCollectionNotification);
begin
  if Action <> cnAdded then
    Value.Free;
end;

end.

