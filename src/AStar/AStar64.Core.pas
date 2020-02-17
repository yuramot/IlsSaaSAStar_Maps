unit AStar64.Core;
//------------------------------------------------------------------------------
// центральный модуль DLL
//
// содержит класс, реализующий расчёт
//------------------------------------------------------------------------------

//------------------------------------------------------------------------------
interface

uses
  SysUtils, Windows, Math, DateUtils, Types, SyncObjs,
  System.Generics.Collections, System.Generics.Defaults,
  Geo.Pos, AStar64.FileStructs, Geo.Hash, Geo.Calcs, AStar64.Areas,
  AStar64.Common, AStar64.Extra, AStar64.Typ, slogsend,
  IniFiles, StrUtils, AStar64.LandMark, Ils.Logger;

//------------------------------------------------------------------------------
const
  CTimeoutDef = 600;
  CZonesMaskDef = 0;
  CSignsMaskDef = 0;

//------------------------------------------------------------------------------
type

//------------------------------------------------------------------------------
//! класс расчёта
//------------------------------------------------------------------------------
  TAstar = class
  private
    FAccounts: TIntegerDynArray;
    FFromLatitude: Double;
    FFromLongitude: Double;
    FToLatitude: Double;
    FToLongitude: Double;
    FTimeOut: Integer;
    FLenTreshold: Double;
    FZones: TZoneControl;
    FSigns: TSignControl;
    FRoadSpeeds: TRoadSpeedsRecord;
    FRoadWorkSet: TRoadTypeSet;
    FFromList, FToList: TAStarList;
    FFilesHolder: TFileAgglomeration;
    FFromNode, FToNode: TNode;
    FLandMarkPicker: TLandMarkPicker;
    FCountFrom: Integer;
    FCountFromFirst: Integer;
    FCountTo: Integer;
    FCountToFirst: Integer;
    FRadiusTo: Double;
    FRadiusFrom: Double;
    FRadiusFromMax: Double;
    FRadiusToMax: Double;
    FReductionFactorTo: Integer;
    FReductionFactorFrom: Integer;
    FFeature: TAStarFeatureSet;
    FFormatSettings: TFormatSettings;
    //! поиск ближайшего ребра в массиве областей 3*3
    //! возвращает FALSE, если найти ребро не удалось
    function FindAndAddNearestEdge(
      const ALatitude: Double;
      const ALongitude: Double;
      const ADir: TADir;
      const ARoadWorkSet: TRoadTypeSet;
      const ARoadSpeeds: TRoadSpeedsRecord;
      const AZoneMask: TZoneControl
    ): Boolean;
    //!
    function GetList(aDir: TADir): TAStarList;
    function GetNode(aDir: TADir): TNode;
    procedure SetNode(aDir: TADir; const Value: TNode);
    function GetRadius(aDir: TADir): Double;
    procedure SetRadius(aDir: TADir; Value: Double);
    function GetRadiusMax(aDir: TADir): Double;
    procedure SetRadiusMax(aDir: TADir; Value: Double);
    function GetReductionFactor(aDir: TADir):Integer;
    procedure SetReductionFactor(aDir: TADir; Value:Integer);
    function GetLatitude(aDir: TADir): Double;
    procedure SetLatitude(aDir: TADir; Value: Double);
    function GetLongitude(aDir: TADir): Double;
    procedure SetLongitude(aDir: TADir; Value: Double);
    //!
    property List[aDir: TADir]: TAStarList read GetList;
    property Node[aDir: TADir]: TNode read GetNode write SetNode;
    property Radius[aDir: TADir]: Double read GetRadius write SetRadius;
    property RadiusMax[aDir: TADir]: Double read GetRadiusMax write SetRadiusMax;
    property ReductionFactor[aDir: TADir]: Integer read GetReductionFactor write SetReductionFactor;
    property Latitude[aDir: TADir]: Double read GetLatitude write SetLatitude;
    property Longitude[aDir: TADir]: Double read GetLongitude write SetLongitude;
  public
    constructor Create();
    destructor Destroy(); override;
    function Calculate(): Integer; overload;
    function Calculate(AReductionFactorDef: Integer): Integer; overload;
    procedure SetCalcParams(
      const AAccs: PInteger;
      const AAStarRequest: PAstarRequest
    ); overload;
    procedure SetCalcParams(
      const AAccs: PInteger;
      const AAStarRequest: PAstarRequest3
    ); overload;
    procedure SetCalcParams(
      const AAccs: PInteger;
      const AAStarRequest: PAstarRequest4
    ); overload;
    procedure SetCalcParams(
      const AAccs: PInteger;
      const AFromLatitude: Double;
      const AFromLongitude: Double;
      const AToLatitude: Double;
      const AToLongitude: Double;
      const ARoadSpeeds: TRoadSpeedsRecord;
      const ATimeOut: Integer = CTimeoutDef;
      const ALenTreshold: Double = CReductionTresholdDef;
      const AZones: TZoneControl = CZonesMaskDef;
      const ASigns: TSignControl = CSignsMaskDef;
      const AFeature: UInt64 = 0
    ); overload;
    procedure SetCalcParamsSign(
      const AAccs: PInteger;
      const AAStarRequest: PAstarRequest3
    );
    function CountDD(
      var RDistance: Double;
      var RDuration: Double;
      const AFaultReason: Integer;
      const ARLTLen: Integer;
      const ARLALen: Integer;
      const AStatRecord: PStatRecord = nil;
      const ARZTL: PRoadLengthByTypeRecord = nil;
      const ARLAR: PRoadLengthAggregateRecord = nil;
      const AEdgeOnly: Boolean = False
    ): Integer;
    procedure MakeHash(var RHashString: PAnsiChar; const AFaultReason: Integer; const AFormat: Integer = 0);
    procedure MakeSpeedline(var RSpeedlineString: PAnsiChar);
    procedure ReadLMRectangle(var RNWHash, RSEHash: Int64);
    function CheckGraphVersion: Boolean;
  end;

procedure TransformArr(
  const ASrc: PInteger;
  var RDest: TIntegerDynArray
);

procedure VLog(const AMessage: string; const AForceLog: Boolean = False);
procedure SetupLogMode();

//------------------------------------------------------------------------------
implementation

var
  GWriteTextLog: Boolean = False;
  GWriteUDPLog: Boolean = False;

var
  GMutex: TMutex;

type
//------------------------------------------------------------------------------
//! массив из хэшей областей (9 штук, включая центр)
//------------------------------------------------------------------------------
  THashSquareArray = array[0..8] of Int64;

procedure TransformArr(
  const ASrc: PInteger;
  var RDest: TIntegerDynArray
);
var
  AccRef: PInteger;
  Cnt: Integer;
begin
  AccRef := ASrc;
  Cnt := 0;
  repeat
    SetLength(RDest, Cnt + 1);
    RDest[Cnt] := AccRef^;
    Inc(Cnt);
    Inc(AccRef);
  until (AccRef^ = 0);
end;

//------------------------------------------------------------------------------
//! рассчитываем все девять областей вокруг заданной точки
//------------------------------------------------------------------------------
procedure SquaresAround(
  const AArea: Int64;
  var RAreas: THashSquareArray
);
begin
  RAreas[0] := AArea;
  RAreas[1] := AddLongi(AArea);
  RAreas[2] := AddLati(AArea);
  RAreas[3] := SubLongi(AArea);
  RAreas[4] := SubLati(AArea);
  RAreas[5] := SubLati(RAreas[1]);
  RAreas[6] := AddLongi(RAreas[2]);
  RAreas[7] := AddLati(RAreas[3]);
  RAreas[8] := SubLongi(RAreas[4]);
end;

//function CallBack(
//  const AHandle: Pointer;
//  const AMetaData: Pointer
//): Integer; stdcall;
//begin
//  Result := GetSpeedByOSM(PMetaDataV1(AMetaData)^.RoadType);
//end;

//------------------------------------------------------------------------------
// TAstar
//------------------------------------------------------------------------------

constructor TAstar.Create();
begin
  inherited Create();
  //
  SetupLogMode();
  FFilesHolder := TFileAgglomeration.Create();
  FFromList := TAStarList.Create();
  FToList := TAStarList.Create();
  FFormatSettings := TFormatSettings.Create('ru-ru');
  FFormatSettings.DecimalSeparator := '.';
end;

procedure TAstar.SetCalcParams(
  const AAccs: PInteger;
  const AAStarRequest: PAstarRequest
);
begin
  SetCalcParams(
    AAccs,
    AAStarRequest.FromLatitude,
    AAStarRequest.FromLongitude,
    AAStarRequest.ToLatitude,
    AAStarRequest.ToLongitude,
    AAStarRequest.RoadSpeedsRecord,
    AAStarRequest.TimeOut,
    AAStarRequest.LenTreshold,
    AAStarRequest.ZonesLimit
  );
end;

procedure TAstar.SetCalcParams(
  const AAccs: PInteger;
  const AAStarRequest: PAstarRequest3
);
begin
  SetCalcParams(
    AAccs,
    AAStarRequest.FromLatitude,
    AAStarRequest.FromLongitude,
    AAStarRequest.ToLatitude,
    AAStarRequest.ToLongitude,
    CRoadSpeedRecordDef,
    AAStarRequest.TimeOut,
    AAStarRequest.LenTreshold,
    AAStarRequest.ZonesLimit,
    AAStarRequest.SignsLimit,
    1
  );
end;

procedure TAstar.SetCalcParams(
  const AAccs: PInteger;
  const AAStarRequest: PAstarRequest4
);
begin
  SetCalcParams(
    AAccs,
    AAStarRequest.FromLatitude,
    AAStarRequest.FromLongitude,
    AAStarRequest.ToLatitude,
    AAStarRequest.ToLongitude,
    CRoadSpeedRecordDef,
    AAStarRequest.TimeOut,
    AAStarRequest.LenTreshold,
    AAStarRequest.ZonesLimit,
    AAStarRequest.SignsLimit,
    AAStarRequest.Feature
  );
end;

procedure TAstar.SetCalcParams(
  const AAccs: PInteger;
  const AFromLatitude: Double;
  const AFromLongitude: Double;
  const AToLatitude: Double;
  const AToLongitude: Double;
  const ARoadSpeeds: TRoadSpeedsRecord;
  const ATimeOut: Integer = CTimeoutDef;
  const ALenTreshold: Double = CReductionTresholdDef;
  const AZones: TZoneControl = CZonesMaskDef;
  const ASigns: TSignControl = CSignsMaskDef;
  const AFeature: UInt64 = 0
);
begin
  TransformArr(AAccs, FAccounts);
  //
  FFromLatitude := AFromLatitude;
  FFromLongitude := AFromLongitude;
  FToLatitude := AToLatitude;
  FToLongitude := AToLongitude;
  FTimeOut := Round(ATimeOut / (CReductionFactorStartDef + 1));
  FLenTreshold := ALenTreshOld;
  FZones := AZones;
  FSigns := ASigns;
  FRoadSpeeds := ARoadSpeeds;
  FRoadWorkSet := GetFullRoadWorkSet(FRoadSpeeds);
  FFeature := GetAStarFeatureSet(AFeature);
  //
  FLandMarkPicker := TLandMarkPicker.Create(FAccounts, AFromLatitude, AFromLongitude, AToLatitude, AToLongitude);
end;

procedure TAstar.SetCalcParamsSign(
  const AAccs: PInteger;
  const AAStarRequest: PAstarRequest3
);
begin
  SetCalcParams(
    AAccs,
    AAStarRequest.FromLatitude,
    AAStarRequest.FromLongitude,
    AAStarRequest.ToLatitude,
    AAStarRequest.ToLongitude,
    CRoadSpeedRecordDef,
    AAStarRequest.TimeOut,
    AAStarRequest.LenTreshold,
    AAStarRequest.ZonesLimit,
    AAStarRequest.SignsLimit
  );
end;

destructor TAstar.Destroy();
begin
  FFilesHolder.Free();
  FFromList.Free();
  FToList.Free();
  FLandMarkPicker.Free();
  //
  inherited Destroy();
end;

function TAstar.CheckGraphVersion: Boolean;
var
  VerStr: string;
  ini: TIniFile;
begin
  ini := TIniFile.Create(FFilesHolder.RootPath + 'Astar.info');
  try
    VerStr := ini.ReadString('Version', 'Files', '');
    Result :=
      VerStr =
        IntToStr(CAstarFileStructsVersionMajor) + '.' +
        IntToStr(CAstarFileStructsVersionMinor) + '.' +
        IntToStr(CAstarFileStructsVersionRevision);
  finally
    ini.Free;
  end;
end;

function TAstar.Calculate(): Integer;
var
  ReductionFactor: Integer;
begin
  ReductionFactor := CReductionFactorStartDef;
  repeat
    Result := Calculate(ReductionFactor);
    Dec(ReductionFactor);
  until (ReductionFactor < CReductionFactorDef) or (Result = 0);
end;

function TAstar.Calculate(AReductionFactorDef: Integer): Integer;

  function MakeLandMarkConnection(AKey: TLandMarkWayKey): Integer;
  var
    ForwardNode: TNode;
    BackwardNode: TNode;
    tmpNode: TNode;
    Area: TArea;
    NewNodes: TNodeArray;
    i: Integer;
    i1, i2: Integer;
    v: THashVector;
    ParentNode: TNode;
    n: TNode;
  begin
    ForwardNode := FLandMarkPicker.LandMarkMatrix.Items[AKey].ConnectedNode[adForward];
    i1 := FLandMarkPicker.LandMarkMatrix.Items[AKey].GeoWayVectorHash[ForwardNode.HashVector^];
    BackwardNode := FLandMarkPicker.LandMarkMatrix.Items[AKey].ConnectedNode[adBackward];
    i2 := FLandMarkPicker.LandMarkMatrix.Items[AKey].GeoWayVectorHash[BackwardNode.HashVector.Reverse];

    ParentNode := ForwardNode;
    for i := i1 + 1 to i2 - 1 do
    begin
      v := FLandMarkPicker.LandMarkMatrix.Items[AKey].HashVector[i];

      Area := FFilesHolder.LoadArea(ParentNode.SEdge.HashVector.HashFrom, FAccounts);
      if not Assigned(Area) then
        Exit(ERROR_ASTAR_FILE_NOT_FOUND);
      Area.ParseRef(ParentNode, 0, adForward, GetFullRoadWorkSet(FRoadSpeeds), FRoadSpeeds, NewNodes, FAccounts);

      tmpNode := nil;
      for n in NewNodes do
        if n.SEdge.HashVector.HashTo = v.HashTo then
          tmpNode := n
        else
          n.Free;

      if not Assigned(tmpNode) then
        Exit(ERROR_ASTAR_NO_WAY_FOUND);

      List[adForward].AddNewSkipDict(tmpNode);
      ParentNode := tmpNode;

      if i = (i2 - 1) then
        Node[adForward] := ParentNode;
    end;
    Node[adBackward] := BackwardNode;
    Result := ERROR_ASTAR_SUCCESS;
  end;

  function CountWS(AWS: TRoadTypeSet): string;
  var
    i: Integer;
    r: TRoadType;
  begin
    i := 0;
    for r in AWS do
      Inc(i);
    Result := IntToStr(i);
  end;

label S;
var
  PassCounter, FirstTryCounter: Integer;
  NeedRecalcFrom, NeedRecalcTo: Boolean;
  AreaWork: TArea;
  NewNodesList: TNodeArray;
  NodesIter: TNode;
  Started, StepStarted: TDateTime;
  StepSec: Integer;
  Dir: TADir;
  LandMarkWayKey: TLandMarkWayKey;

  procedure SetNeedToRecalc(const ADir: TADir; const AValue: Boolean);
  begin
    if ADir = adForward then
      NeedRecalcFrom := AValue
    else
      NeedRecalcTo := AValue;
  end;

//  procedure LogConnected(const ADir: TADir);
//  begin
//    VLog(' ' + IfThen(ADir = adForward, 'f', 'b') + ' CONNECTED forw: ' +
//      TGeoHash.ConvertBinToString(Node[ADir].HashVector.HashFrom, 12) + '-' +
//      TGeoHash.ConvertBinToString(Node[ADir].HashVector.HashTo, 12));
//    VLog(' ' + IfThen(ADir = adForward, 'f', 'b') + ' CONNECTED backw: ' +
//      TGeoHash.ConvertBinToString(Node[Reverse(ADir)].HashVector.HashFrom, 12) + '-' +
//      TGeoHash.ConvertBinToString(Node[Reverse(ADir)].HashVector.HashTo, 12));
//  end;

  function PassInfo(): string;
  begin
    Result :=
        'pass=' + IntToStr(PassCounter)
      + ', time=' + IntToStr(Trunc((Now - Started) * SecsPerDay)) + 's'
      + ', from=' + FormatFloat('#.##', FRadiusFromMax, FFormatSettings)
      + ', to=' + FormatFloat('#.##', FRadiusToMax, FFormatSettings)
      + ', rfrom=' + IntToStr(FReductionFactorFrom)
      + ', rto=' + IntToStr(FReductionFactorTo)
      + ', rdef=' + IntToStr(AReductionFactorDef)
      + ', cfrom=(' + IntToStr(FFromList.Count) + ';' + IntToStr(FFromList.OpenedCount) + ')'
      + ', cto=(' + IntToStr(FToList.Count)  + ';' + IntToStr(FToList.OpenedCount) + ')'
      + ', rsfrom=(' + CountWS(FRoadWorkSet) + ';' + CountWS(GetReducedRoadWorkSet(FRoadWorkSet, FReductionFactorFrom)) + ')'
      + ', rsto=(' + CountWS(FRoadWorkSet) + ';' + CountWS(GetReducedRoadWorkSet(FRoadWorkSet, FReductionFactorTo)) + ')'
      + ', stime=' + IntToStr(StepSec) + 's'
      + ''
  end;

const
  CMaxNodeListLimit = 10000;  //подобрано експериментально
  CReportedSpeedFactor = 1;//0.9;
begin
  VLog(
      '"Calculate"arf=' + IntToStr(AReductionFactorDef)
    + ',cfrom=(' + IntToStr(List[adForward].Count) + ';'+ IntToStr(List[adForward].OpenedCount) + ')'
    + ',cto=(' + IntToStr(List[adBackward].Count)  + ';'+ IntToStr(List[adBackward].OpenedCount) + ')'
  );
  Radius[adForward] := 0;
  Radius[adBackward] := 0;
  RadiusMax[adForward] := 0;
  RadiusMax[adBackward] := 0;
  ReductionFactor[adForward] := 0;
  ReductionFactor[adBackward] := 0;
  Started := Now;
  StepSec := 0;
  Result := ERROR_ASTAR_SUCCESS;
  try
    if (afLandMark in FFeature)
    and Assigned(FLandMarkPicker.LandMarkMatrix) then
    begin
      FLandMarkPicker.LandMarkMatrix.Clear;
      FLandMarkPicker.LandMarkMatrix.CheckAndLoadLandMarks(FFromLatitude, FFromLongitude, FToLatitude, FToLongitude, FZones);
    end;
    List[adForward].Clear;
    List[adBackward].Clear;
    StepStarted := Now;
    FirstTryCounter := 0;
    NeedRecalcFrom := True;
    NeedRecalcTo := True;
S:
    PassCounter := 0;
    // найдём и добавим в списки начальные рёбра графа
    // - для начальной координаты
    if NeedRecalcFrom then
    begin
      if not FindAndAddNearestEdge(FFromLatitude, FFromLongitude, adForward, FRoadWorkSet, FRoadSpeeds, FZones) then
        Exit(ERROR_ASTAR_NEAR_EDGE_NOT_FOUND);
      NeedRecalcFrom := False;
    end;
    // - для конечной координаты
    if NeedRecalcTo then
    begin
      if not FindAndAddNearestEdge(FToLatitude, FToLongitude, adBackward, FRoadWorkSet, FRoadSpeeds, FZones) then
        Exit(ERROR_ASTAR_NEAR_EDGE_NOT_FOUND);
      NeedRecalcTo := False;
    end;
    // главный цикл
    repeat
      for Dir := Low(TADir) to High(TADir) do
      begin
        SetLength(NewNodesList, 0);
        repeat
          // получаем рабочую ноду
          Node[Dir] := List[Dir].GetWorkNode;
          if not Assigned(Node[Dir]) then
          begin
            if FirstTryCounter >= CFirstSpecialIterations then // ^ если не получили, то пути не существует
              Exit(ERROR_ASTAR_NO_WAY_FOUND);
            SetNeedToRecalc(Dir, True);
            Inc(FirstTryCounter);
            goto S;
          end;
          // проверяем на соединение путей по маякам
          if (afLandMark in FFeature) then
            if Assigned(FLandMarkPicker.LandMarkMatrix) then
              if FLandMarkPicker.LandMarkMatrix.CheckLandmarkWayConnection(Node[Dir], Dir, LandMarkWayKey) then
                Exit(MakeLandMarkConnection(LandMarkWayKey));
          // проверяем на соединение прямого и обратного путей
          Node[Reverse(Dir)] := List[Dir].GetConnection(Node[Dir], List[Reverse(Dir)]);
          if Assigned(Node[Reverse(Dir)]) then
          begin
//            LogConnected(Dir);
            FLandMarkPicker.LMNW := 0;
            FLandMarkPicker.LMSE := 0;
            Exit(ERROR_ASTAR_SUCCESS);
          end;
          // делаем запрос к TArea на развёртку ребра
          AreaWork := FFilesHolder.LoadArea(Node[Dir].HashVector.HashFrom, FAccounts);
          if not Assigned(AreaWork) then
            Exit(ERROR_ASTAR_FILE_NOT_FOUND);
          Radius[Dir] := TGeoCalcs.GeoLengthKmDeg(Node[Dir].CoordsTo.Latitude, Node[Dir].CoordsTo.Longitude,  Latitude[Dir], Longitude[Dir]);
          if Radius[Dir] > RadiusMax[Dir] then
            RadiusMax[Dir] := Radius[Dir];
          ReductionFactor[Dir] := IfThen(
            (RadiusMax[Dir] < FLenTreshold) and (List[Dir].Count < CMaxNodeListLimit),
            0,
            AReductionFactorDef
          );
          AreaWork.ParseRef(Node[Dir], ReductionFactor[Dir], Dir, FRoadWorkSet, FRoadSpeeds, NewNodesList, FAccounts);
          // список рёбер нулевой длины - не обязательно фатальная ошибка
        until Length(NewNodesList) <> 0;
        // для полученного списка рёбер
        for NodesIter in NewNodesList do
        begin
          // рассчитать характеристики и добавить к списку
          NodesIter.FillNodeHeuristics(Latitude[Dir], Longitude[Dir], Latitude[Reverse(Dir)], Longitude[Reverse(Dir)], FRoadSpeeds);
//          List[Dir].AddNew(NodesIter);
          // проверим на дубль пути в эту точку
          if not List[Dir].CheckLastEdge(NodesIter, PassCounter, FZones) then
            NodesIter.Free;
        end;
      end;
      //
      Inc(PassCounter);
      if (PassCounter mod 1000) = 0 then
      begin
        StepSec := Trunc((Now - StepStarted) * SecsPerDay);
        VLog(PassInfo);
        StepStarted := Now;
      end;
      if ((Now - Started) * SecsPerDay) > FTimeOut then
        Exit(ERROR_ASTAR_TIMEOUT);
    until False;
  finally
    VLog('RESULT=' + IntToStr(Result) + ', ' + PassInfo);
  end;
end;

function TAstar.CountDD(
  var RDistance: Double;
  var RDuration: Double;
  const AFaultReason: Integer;
  const ARLTLen: Integer;
  const ARLALen: Integer;
  const AStatRecord: PStatRecord = nil;
  const ARZTL: PRoadLengthByTypeRecord = nil;
  const ARLAR: PRoadLengthAggregateRecord = nil;
  const AEdgeOnly: Boolean = False
): Integer;
var
  ZRLTHash: TDictionary<UInt64, TRoadLengthByTypeRecord>;
  ZRLAPos: Integer;

  procedure Clean;
  begin
    if Assigned(ARZTL) then
      FillMemory(ARZTL, ARLTLen, 0);
    if Assigned(ARLAR) then
      FillMemory(ARLAR, ARLALen, 0);
    ZRLAPos := 0;
  end;

  procedure AddRoadLen(az: UInt64; at: UInt32; al: Double);
  var
    t: TRoadLengthByTypeRecord;
  begin
    if Assigned(ARZTL) then
    begin
      if not ZRLTHash.ContainsKey(az) then
        ZRLTHash.Add(az, TRoadLengthByTypeRecord.Create(az));

      t := ZRLTHash.Items[az];
      t.AddRoadLen(at, al);
      ZRLTHash.Items[az] := t;
    end;

    if Assigned(ARLAR) then
      if (ZRLAPos * SizeOf(TRoadLengthAggregateRecord)) <= ARLALen then
        if Assigned(ARLAR) then
        begin
          if (ZRLAPos = 0) and
             (TRoadLengthAggregateRecordArray(ARLAR)[ZRLAPos].RoadType = 0) then
          begin
            TRoadLengthAggregateRecordArray(ARLAR)[ZRLAPos].RoadType := at;
            TRoadLengthAggregateRecordArray(ARLAR)[ZRLAPos].Zones := az;
          end;

          if (TRoadLengthAggregateRecordArray(ARLAR)[ZRLAPos].Zones <> az) or
             (TRoadLengthAggregateRecordArray(ARLAR)[ZRLAPos].RoadType <> at) then
          begin
            Inc(ZRLAPos);
            TRoadLengthAggregateRecordArray(ARLAR)[ZRLAPos].RoadType := at;
            TRoadLengthAggregateRecordArray(ARLAR)[ZRLAPos].Zones := az;
          end;

          TRoadLengthAggregateRecordArray(ARLAR)[ZRLAPos].RoadLength :=
            TRoadLengthAggregateRecordArray(ARLAR)[ZRLAPos].RoadLength + al;
        end;
  end;

  procedure CalcLength(ANodeWork: TNode; ADir: TADir);
  var
    l: Double;
    z: UInt64;
    t: Integer;
  begin
    if Assigned(ARZTL) or Assigned(ARLAR) then
    begin
      l := ANodeWork.SEdge.Distance;
      z := ANodeWork.Zone;
      t := ANodeWork.SEdge.RoadType;
      AddRoadLen(z, t, l);
      if Assigned(AStatRecord) then
        AStatRecord.OrdinaryLen := AStatRecord.OrdinaryLen + l;
    end;
  end;

  procedure FillLength;
  var
    z: UInt64;
    i: Integer;
  begin
    Result := 0;
    if ZRLTHash.Count * SizeOf(TRoadLengthByTypeRecord) > ARLTLen then
      Exit;
    i := 0;
    for z in ZRLTHash.Keys do
    begin
      TRoadLengthByTypeRecordArray(ARZTL)[i] := ZRLTHash[z];
      Inc(i);
    end;
    Result := ZRLTHash.Count;
  end;

var
  NodeWork: TNode;
begin
  RDistance := 0;
  RDuration := 0;
  if AFaultReason < ERROR_ASTAR_SUCCESS then
  begin
    RDistance := TGeoCalcs.GeoLengthKmDeg(FFromLatitude, FFromLongitude, FToLatitude, FToLongitude);
    RDuration := RDistance * CBackwardCoeff;
    Exit;
  end;

  Clean;

  ZRLTHash := TDictionary<UInt64, TRoadLengthByTypeRecord>.Create;

  try
    // счёт точек прямого пути
    NodeWork := Node[adForward];
    repeat
      if AEdgeOnly then
        Inc(FCountFrom)
      else begin
        FCountFromFirst := NodeWork.WayCount + 1;
        FCountFrom := FCountFrom + FCountFromFirst;
      end;
      RDistance := RDistance + NodeWork.Distance;
      RDuration := RDuration + NodeWork.Duration;
      CalcLength(NodeWork, adForward);
      NodeWork := NodeWork.ParentNode;
    until not Assigned(NodeWork);
    // счёт точек обратного пути
    NodeWork := Node[adBackward];
    repeat
      NodeWork := NodeWork.ParentNode; // сразу пропускаем первое ребро = оно - копия прямого пути
      if not Assigned(NodeWork) then
        Break;

      if AEdgeOnly then
        Inc(FCountTo)
      else begin
        FCountToFirst := NodeWork.WayCount + 1;
        FCountTo := FCountTo + FCountToFirst;
      end;
      RDistance := RDistance + NodeWork.Distance;
      RDuration := RDuration + NodeWork.Duration;
      CalcLength(NodeWork, adBackward);
    until False;
    FillLength;
  finally
    ZRLTHash.Free;
  end;
end;

procedure TAstar.MakeHash(var RHashString: PAnsiChar; const AFaultReason: Integer; const AFormat: Integer);

  function GetWayCount(const ANodeWork: TNode; AFormat: Integer): Integer;
  begin
    Result := 0;
    if AFormat <> Ord(gfLandMark) then
      Result := ANodeWork.WayCount;
  end;

var
  I: Integer;
  PointsCount: Integer;
  BytesCount: Integer;
  FoundIndex: Integer;
  PathCPos: Integer;
  DistCurrent: Double;
  DistMax: Double;
  NodeWork: TNode;
  OutputPosition: TGeoPathPointArray;
  RezStr: AnsiString;
  FoundFirst: Integer;
  FoundLast: Integer;
  WayCount: Integer;
begin
  if AFaultReason < ERROR_ASTAR_SUCCESS then
  begin
    SetLength(OutputPosition, 2);
    OutputPosition[0].p := TGeoPos.Create(FFromLatitude, FFromLongitude);
    OutputPosition[0].s := FRoadSpeeds.Residential;
    OutputPosition[0].t := RT_residential;
    OutputPosition[0].z := 0;
    OutputPosition[0].c := False;
    OutputPosition[1].p := TGeoPos.Create(FToLatitude, FToLongitude);
    OutputPosition[1].s := FRoadSpeeds.Residential;
    OutputPosition[1].t := RT_residential;
    OutputPosition[1].z := 0;
    OutputPosition[1].c := False;
  end
  else
  begin
    PointsCount := FCountFrom + FCountTo + 2; // (+2 точки начала и конца)
    SetLength(OutputPosition, PointsCount);
    // заполнение - прямой путь
    NodeWork := Node[adForward];

{$ifdef ASTAR_DEBUG}
            TAstar.Log(' Node[adForward]: ' +
              TGeoHash.ConvertBinToString(NodeWork.HashVector.PointFrom, 12) + '-' +
              TGeoHash.ConvertBinToString(NodeWork.HashVector.PointTo, 12));
{$endif}

    PathCPos := FCountFrom;
    repeat
      WayCount := GetWayCount(NodeWork, AFormat);

      PathCPos := PathCPos - WayCount;
      for I := 0 to WayCount do
      begin
        OutputPosition[PathCPos + I].p := NodeWork.WayPoints[I, adForward].AsGeoPos;
        OutputPosition[PathCPos + I].s := FRoadSpeeds.GetSpeedByRoadType(NodeWork.WayPoints[I, adForward].RType); //NodeWork.WayPoints[I, adForward].RSpeed;
        OutputPosition[PathCPos + I].t := NodeWork.WayPoints[I, adForward].RType;
        OutputPosition[PathCPos + I].z := NodeWork.Zone; //NodeWork.WayPoints[I, adForward].Zone;
        OutputPosition[PathCPos + I].c := I = 0;
      end;
      //
      NodeWork := NodeWork.ParentNode;
      Dec(PathCPos);
    until (PathCPos = 0);
{$ifdef ASTAR_DEBUG}
    TAstar.Log('---------');
{$endif}
    // заполнение - обратный путь
    NodeWork := Node[adBackward];

{$ifdef ASTAR_DEBUG}
    TAstar.Log(' Node[adBackward]: ' +
      TGeoHash.ConvertBinToString(NodeWork.HashVector.PointFrom, 12) + '-' +
      TGeoHash.ConvertBinToString(NodeWork.HashVector.PointTo, 12));
{$endif}
    PathCPos := FCountFrom + 1;
    repeat
      NodeWork := NodeWork.ParentNode; // сразу пропускаем первое ребро = оно - копия прямого пути

      if not Assigned(NodeWork) then
        Break;

      WayCount := GetWayCount(NodeWork, AFormat);

      for I := 0 to WayCount do
      begin
        OutputPosition[PathCPos].p := NodeWork.WayPoints[I, adBackward].AsGeoPos;
        OutputPosition[PathCPos].s := FRoadSpeeds.GetSpeedByRoadType(NodeWork.WayPoints[I, adBackward].RType); //NodeWork.WayPoints[I, adBackward].RSpeed;
        OutputPosition[PathCPos].t := NodeWork.WayPoints[I, adBackward].RType;
        OutputPosition[PathCPos].z := NodeWork.Zone; //NodeWork.WayPoints[I, adBackward].Zone;
        OutputPosition[PathCPos].c := I = 0;
        Inc(PathCPos);
      end;
    until False;
{$ifdef ASTAR_DEBUG}
    TAstar.Log('-----------------------------------------------------------');
{$endif}
    // поиск ближайшей на первом отрезке
    FoundIndex := 1;
    DistMax := Infinity;
    for I := 1 to FCountFromFirst do
    begin
      DistCurrent := TGeoCalcs.GeoLengthKmDeg(
        OutputPosition[I].p.Latitude, OutputPosition[I].p.Longitude, FFromLatitude, FFromLongitude);

      if (DistMax > DistCurrent) then
      begin
        DistMax := DistCurrent;
        FoundIndex := I;
      end;
    end;
    FCountFromFirst := FoundIndex - 1;
    FoundFirst := FoundIndex;
    // поиск ближайшей на последнем отрезке
    FoundIndex := PointsCount - 2;
    DistMax := Infinity;
    for I := PointsCount - FCountToFirst - 1 to PointsCount - 2 do
    begin
      DistCurrent := TGeoCalcs.GeoLengthKmDeg(
        OutputPosition[I].p.Latitude, OutputPosition[I].p.Longitude, FToLatitude, FToLongitude
      );
      if (DistMax > DistCurrent) then
      begin
        DistMax := DistCurrent;
        FoundIndex := I;
      end;
    end;
    FoundLast := FoundIndex;
    PointsCount := FoundIndex + 2; // коррекция количества точек
    // заполнение - данные нам начало и конец

    if OutputPosition[FoundFirst].p.IsSame(FFromLatitude, FFromLongitude) then
      FCountFromFirst := FoundFirst
    else
    begin
      OutputPosition[FCountFromFirst].p.Latitude := FFromLatitude;
      OutputPosition[FCountFromFirst].p.Longitude := FFromLongitude;
      OutputPosition[FCountFromFirst].s := FRoadSpeeds.Residential;
      OutputPosition[FCountFromFirst].t := RT_residential;
      OutputPosition[FCountFromFirst].z := 0;
      OutputPosition[FCountFromFirst].c := False;
    end;

    if OutputPosition[FoundLast].p.IsSame(FToLatitude, FToLongitude) then
      Dec(PointsCount)
    else
    begin
      OutputPosition[PointsCount - 1].p.Latitude := FToLatitude;
      OutputPosition[PointsCount - 1].p.Longitude := FToLongitude;
      OutputPosition[PointsCount - 1].s := FRoadSpeeds.Residential;
      OutputPosition[PointsCount - 1].t := RT_residential;
      OutputPosition[PointsCount - 1].z := 0;
      OutputPosition[PointsCount - 1].c := False;
    end;
    // коррекция количества точек
    PointsCount := PointsCount - FCountFromFirst;
    OutputPosition := Copy(OutputPosition, FCountFromFirst, PointsCount);
  end;
  // итог
  RezStr := AnsiString(TGeoHash.EncodeArrayAnyFormat(OutputPosition, 12, TGeoHash.GeoHashStringFormat(AFormat)));
  VLog('Length(RezStr)=' + IntToStr(Length(RezStr)));
  BytesCount := Length(RezStr) + 1;
  GetMem(RHashString, BytesCount);
//  FillChar(RHashString^, BytesCount, 0);
  Move(RezStr[1], RHashString^, BytesCount);
end;

procedure TAstar.MakeSpeedline(var RSpeedlineString: PAnsiChar);
var
  I: Integer;
  BytesCount: Integer;
  PathCPos: Integer;
  NodeWork: TNode;
  RezStr: AnsiString;
  SpeedStr: AnsiString;
begin
  // а) заполнение - прямой путь
  // координаты последней точки последнего ребра
  RezStr := AnsiString(TGeoHash.EncodePointString(Node[adForward].CoordsTo.Latitude, Node[adForward].CoordsTo.Longitude, 12));
  // далее - путь
  NodeWork := Node[adForward];
  PathCPos := FCountFrom;
  repeat
    PathCPos := PathCPos - NodeWork.WayCount;
    SpeedStr := AnsiString(Format('%.3d', [NodeWork.ReportedSpeed]));
    for I := NodeWork.WayIndex + NodeWork.WayCount - 1 downto NodeWork.WayIndex do
    begin
      RezStr :=
          AnsiString(TGeoHash.EncodePointString(NodeWork.WayPoints[I, adForward].Latitude, NodeWork.WayPoints[I, adForward].Longitude, 12))
        + SpeedStr
        + RezStr;
    end;
    RezStr :=
        AnsiString(TGeoHash.EncodePointString(NodeWork.CoordsFrom.Latitude, NodeWork.CoordsFrom.Longitude, 12))
      + SpeedStr
      + RezStr;
    //
    NodeWork := NodeWork.ParentNode;
    Dec(PathCPos);
  until (PathCPos = 0);
  // б) заполнение - обратный путь
  NodeWork := Node[adBackward];
  repeat
    NodeWork := NodeWork.ParentNode; // сразу пропускаем первое ребро = оно - копия прямого пути
    if not Assigned(NodeWork) then Break;
    for I := NodeWork.WayIndex to NodeWork.WayIndex + NodeWork.WayCount - 1 do
    begin
      RezStr :=
          RezStr
        + SpeedStr
        + AnsiString(TGeoHash.EncodePointString(NodeWork.WayPoints[I, adBackward].Latitude, NodeWork.WayPoints[I, adBackward].Longitude, 12));
    end;
    RezStr :=
        RezStr
      + SpeedStr
      + AnsiString(TGeoHash.EncodePointString(NodeWork.CoordsFrom.Latitude, NodeWork.CoordsFrom.Longitude, 12));
  until False;
  // начальная и конечная точки и префикс
  RezStr :=
      'SL'
    + AnsiString(TGeoHash.EncodePointString(FFromLatitude, FFromLongitude, 12)) + '040'
    + RezStr
    + '040' + AnsiString(TGeoHash.EncodePointString(FToLatitude, FToLongitude, 12));
  //
  BytesCount := Length(RezStr) + 1;
  GetMem(RSpeedlineString, BytesCount);
  Move(RezStr[1], RSpeedlineString^, BytesCount);
end;

function TAstar.FindAndAddNearestEdge(
  const ALatitude: Double;
  const ALongitude: Double;
  const ADir: TADir;
  const ARoadWorkSet: TRoadTypeSet;
  const ARoadSpeeds: TRoadSpeedsRecord;
  const AZoneMask: TZoneControl
): Boolean;
var
  I, J: Integer;
  SquareArray: THashSquareArray;
  WorkEdges: TEdgeArray;
  WorkZones: TZoneControlArray;
  WorkArea: TArea;
  AddedNode: TNode;
  DistCurrent, DistFound: Double;

  //! для данного ребра находим расстояние до него с учётом промежуточных точек пути
  function DistanceForIter(): Double;
  var
    K: Integer;
    C: Double;
    GeoArray: TGeoPosArray;
  begin
    if (WorkEdges[I].WayCount <> 0) then
    begin
      SetLength(GeoArray, WorkEdges[I].WayCount + 2);
      MoveMemory(@GeoArray[1], @WorkArea.Ways[WorkEdges[I].WayIndex], WorkEdges[I].WayCount * SizeOf(TGeoPos));
      if (ADir = adForward) then
      begin
        GeoArray[0] := WorkEdges[I].CoordsFrom;
        GeoArray[High(GeoArray)] := WorkEdges[I].CoordsTo;
      end
      else
      begin
        GeoArray[0] := WorkEdges[I].CoordsTo;
        GeoArray[High(GeoArray)] := WorkEdges[I].CoordsFrom;
      end;
      Result := Infinity;
      for K := Low(GeoArray) to High(GeoArray) - 1 do
      begin
        C := TGeoCalcs.GeoDistancePointToLineDeg(
          ALatitude,
          ALongitude,
          GeoArray[K].Latitude,
          GeoArray[K].Longitude,
          GeoArray[K + 1].Latitude,
          GeoArray[K + 1].Longitude
        );
        if (Result > C) then
          Result := C;
      end;
    end
    else
    begin
      Result := TGeoCalcs.GeoDistancePointToLineDeg(
        ALatitude,
        ALongitude,
        WorkEdges[I].CoordsFrom.Latitude,
        WorkEdges[I].CoordsFrom.Longitude,
        WorkEdges[I].CoordsTo.Latitude,
        WorkEdges[I].CoordsTo.Longitude
      );
    end;
  end;

  function ExistsInList(const ADir: TADir; const Edge: TEdge): Boolean;
  var
    I: Integer;
  begin
    for I := 0 to List[ADir].Count - 1 do
      if List[ADir][I].SEdge.HashVector = Edge.HashVector then
        Exit(True);
    Result := False;
  end;

begin
  Result := False;
  DistFound := Infinity;
  SquaresAround(TGeoHash.EncodePointBin(ALatitude, ALongitude) and CAreaHashMask, SquareArray);
  // стадия 1 - поиск ближайшего
  for J := Low(SquareArray) to High(SquareArray) do
  begin
    WorkArea:= FFilesHolder.LoadArea(SquareArray[J], FAccounts);
    if not Assigned(WorkArea) then
      Continue;
    WorkEdges := WorkArea.Edges[ADir];
    WorkZones := WorkArea.ZoneCtrls[ADir];
    for I := Low(WorkEdges) to High(WorkEdges) do
    begin
      if ((AZoneMask and WorkZones[I]) = 0)
      and IsRoadFits(WorkEdges[I].RoadType, ARoadWorkSet)
      and not ExistsInList(ADir, WorkEdges[I]) then
      begin
        DistCurrent := DistanceForIter();
        if (DistFound > DistCurrent) then
          DistFound := DistCurrent;
      end;
    end;
  end;
  // стадия 2 - добавление в списки
  for J := Low(SquareArray) to High(SquareArray) do
  begin
    WorkArea:= FFilesHolder.LoadArea(SquareArray[J], FAccounts);
    if not Assigned(WorkArea) then
      Continue;
    WorkEdges := WorkArea.Edges[ADir];
    WorkZones := WorkArea.ZoneCtrls[ADir];
    for I := Low(WorkEdges) to High(WorkEdges) do
    begin
      if ((WorkZones[I] and AZoneMask) = 0)
      and IsRoadFits(WorkEdges[I].RoadType, ARoadWorkSet) then
      begin
        if (DistanceForIter() = DistFound)
        {or (TGeoCalcs.GeoLengthDeg(
          // или если расстояние до начала дороги менее 3-х метров
          WorkEdges[I].CoordsFrom.Latitude,
          WorkEdges[I].CoordsFrom.Longitude,
          Latitude[ADir],
          Longitude[ADir]
        ) < 1) }then
        begin
          Result := True;
          AddedNode := TNode.Create(nil, WorkArea, ADir, I);
          AddedNode.H := TGeoCalcs.GeoLengthKmDeg(
            WorkEdges[I].CoordsTo.Latitude,
            WorkEdges[I].CoordsTo.Longitude,
            Latitude[Reverse(ADir)],
            Longitude[Reverse(ADir)]
          ) * CBackwardCoeff;
          AddedNode.F := AddedNode.H;
          List[ADir].AddNewWithPresentCheck(AddedNode);
        end;
      end;
    end;
  end;
end;

function TAstar.GetList(aDir: TADir): TAStarList;
begin
  Result := nil;
  case aDir of
    adForward: Result := FFromList;
    adBackward: Result := FToList;
  end;
end;

function TAstar.GetNode(aDir: TADir): TNode;
begin
  Result := nil;
  case aDir of
    adForward: Result := FFromNode;
    adBackward: Result := FToNode;
  end;
end;

procedure TAstar.SetNode(aDir: TADir; const Value: TNode);
begin
  case aDir of
    adForward: FFromNode := Value;
    adBackward: FToNode := Value;
  end;
end;

function TAstar.GetRadius(aDir: TADir): Double;
begin
  Result := 0;
  case aDir of
    adForward: Result := FRadiusFrom;
    adBackward: Result := FRadiusTo;
  end;
end;

procedure TAstar.SetRadius(aDir: TADir; Value: Double);
begin
  case aDir of
    adForward: FRadiusFrom := Value;
    adBackward: FRadiusTo := Value;
  end;
end;

function TAstar.GetRadiusMax(aDir: TADir): Double;
begin
  Result := 0;
  case aDir of
    adForward: Result := FRadiusFromMax;
    adBackward: Result := FRadiusToMax;
  end;
end;

procedure TAstar.SetRadiusMax(aDir: TADir; Value: Double);
begin
  case aDir of
    adForward: FRadiusFromMax := Value;
    adBackward: FRadiusToMax := Value;
  end;
end;

function TAstar.GetReductionFactor(aDir: TADir):Integer;
begin
  Result := 0;
  case aDir of
    adForward: Result := FReductionFactorFrom;
    adBackward: Result := FReductionFactorTo;
  end;
end;

procedure TAstar.SetReductionFactor(aDir: TADir; Value:Integer);
begin
  case aDir of
    adForward: FReductionFactorFrom := Value;
    adBackward: FReductionFactorTo := Value;
  end;
end;

function TAstar.GetLatitude(aDir: TADir): Double;
begin
  Result := 0;
  case aDir of
    adForward: Result := FFromLatitude;
    adBackward: Result := FToLatitude;
  end;
end;

procedure TAstar.SetLatitude(aDir: TADir; Value: Double);
begin
  case aDir of
    adForward: FFromLatitude := Value;
    adBackward: FToLatitude := Value;
  end;
end;

function TAstar.GetLongitude(aDir: TADir): Double;
begin
  Result := 0;
  case aDir of
    adForward: Result := FFromLongitude;
    adBackward: Result := FToLongitude;
  end;
end;

procedure TAstar.SetLongitude(aDir: TADir; Value: Double);
begin
  case aDir of
    adForward: FFromLongitude := Value;
    adBackward: FToLongitude := Value;
  end;
end;

procedure TAstar.ReadLMRectangle(var RNWHash, RSEHash: Int64);
begin
  if not Assigned(FLandMarkPicker) then
    Exit;
  RNWHash := FLandMarkPicker.LMNW;
  RSEHash := FLandMarkPicker.LMSE;
end;

procedure VLog(const AMessage: string; const AForceLog: Boolean = False);
var
  WS: string;
begin
  try
    WS := Format('pid=%d tid=%d %s' ,[GetCurrentProcessId(), GetCurrentThreadId(), AMessage]);
    if AForceLog or GWriteTextLog then
    begin
      GMutex.Acquire();
      try
        ToLog(WS);
      finally
        GMutex.Release();
      end;
    end;
    if AForceLog or GWriteUDPLog then
      ToSysLog('127.0.0.1', FCL_UserLevel, Debug, Format('AStar.dll=%s %s', [FormatDateTime('yyyy-mm-dd hh:nn:ss.zzz', Now()), WS]));
  finally
    // подавляем боль
  end;
end;

procedure SetupLogMode();
begin
  GWriteTextLog := GetFileAttributes(PChar(ChangeFileExt(GetModuleName(HInstance), '.textlog'))) <> INVALID_FILE_ATTRIBUTES;
  GWriteUDPLog := GetFileAttributes(PChar(ChangeFileExt(GetModuleName(HInstance), '.udplog'))) <> INVALID_FILE_ATTRIBUTES;
end;

procedure Init;
var
  SD: SECURITY_DESCRIPTOR;
  SA: SECURITY_ATTRIBUTES;
begin
  SA.nLength := SizeOf(SA);
  SA.bInheritHandle := False;
  InitializeSecurityDescriptor(@SD, SECURITY_DESCRIPTOR_REVISION);
  SetSecurityDescriptorDacl(@SD, True, nil, False);
  SA.lpSecurityDescriptor := @SD;
  GMutex := TMutex.Create(@SA, False, 'Global\AStar.dll', False);
  GetLastError();
end;

//------------------------------------------------------------------------------
initialization
  Init();

//------------------------------------------------------------------------------
finalization

end.

