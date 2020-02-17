unit uSearchEdge;
//------------------------------------------------------------------------------
// центральный модуль DLL
//
// содержит класс, реализующий расчёт
//------------------------------------------------------------------------------

interface

uses
  SysUtils, Windows, Math, DateUtils,
  System.Generics.Collections, System.Types,
  Geo.Pos, AStar64.FileStructs, Geo.Hash, Geo.Calcs, AStar64.Areas,
  AStar64.Common, AStar64.Extra, AStar64.Typ, slogsend,
  IniFiles, StrUtils, Geo.Hash.Search;

const
  CTimeoutDef = 600;

  CZonesMaskDef = 0;

  CSignsMaskDef = 0;

  CMinDistToLineM = 3;

//------------------------------------------------------------------------------
type

//------------------------------------------------------------------------------
//! класс расчёта
//------------------------------------------------------------------------------
  TSearch = class
  private
    //!
    FAccounts: TIntegerDynArray;
    //!
    FFromLatitude: Double;
    //!
    FFromLongitude: Double;
    //!
    FToLatitude: Double;
    //!
    FToLongitude: Double;
    //!
    FTimeOut: Integer;
    //!
    FLenTreshold: Double;
    //!
    FZones: TZoneControl;
    //!
    FSigns: TSignControl;
    //!
    FRoadSpeeds: TRoadSpeedsRecord;
    //!
    FRoadWorkSet: TRoadTypeSet;
    //! списки узлов
    FFromList, FToList: TAStarList;
    //!
    FFilesHolder: TFileAgglomeration;
    //!
    FFromNode, FToNode: TNode;
    //!
    FCountFrom: Integer;
    //!
    FCountFromFirst: Integer;
    //!
    FCountTo: Integer;
    //!
    FCountToFirst: Integer;
    //!
    //FRadiusTo: Double;
    //!
//    FRadiusFrom: Double;
    //!
//    FRadiusFromMax: Double;
    //!
//    FRadiusToMax: Double;
    //!
//    FReductionFactorTo: Integer;
    //!
//    FReductionFactorFrom: Integer;
    //
    FFeatureLimit: UInt64;

//    FFromCurrentFeature: UInt64;

//    FToCurrentFeature: UInt64;

    //! поиск ближайшего ребра в массиве областей 3*3
    //! возвращает FALSE, если найти ребро не удалось

    //function GetList(aDir: TADir): TAStarList;
    function GetNode(aDir: TADir): TNode;
    procedure SetNode(aDir: TADir; const Value: TNode);
    //function GetRadius(aDir: TADir): Double;
    //procedure SetRadius(aDir: TADir; Value: Double);
//    function GetRadiusMax(aDir: TADir): Double;
//    procedure SetRadiusMax(aDir: TADir; Value: Double);
//    function GetReductionFactor(aDir: TADir):Integer;
//    procedure SetReductionFactor(aDir: TADir; Value:Integer);
//    function GetLatitude(aDir: TADir): Double;
//    procedure SetLatitude(aDir: TADir; Value: Double);
//    function GetLongitude(aDir: TADir): Double;
//    procedure SetLongitude(aDir: TADir; Value: Double);
//    function GetCurrentFeatue(aDir: TADir): UInt64;
//    procedure SetCurrentFeatue(aDir: TADir; Value: UInt64);

//    property List[aDir: TADir]: TAStarList read GetList;
    property Node[aDir: TADir]: TNode read GetNode write SetNode;
//    property Radius[aDir: TADir]: Double read GetRadius write SetRadius;
    //property RadiusMax[aDir: TADir]: Double read GetRadiusMax write SetRadiusMax;
//    property ReductionFactor[aDir: TADir]: Integer read GetReductionFactor write SetReductionFactor;
//    property Latitude[aDir: TADir]: Double read GetLatitude write SetLatitude;
//    property Longitude[aDir: TADir]: Double read GetLongitude write SetLongitude;
//    property CurrentFeatue[aDir: TADir]: UInt64 read GetCurrentFeatue write SetCurrentFeatue;
  public
//    class var AStar: TSearch;
//    class constructor Create;
//    class destructor Destroy;
    constructor Create; overload;

    destructor Destroy(); override;

    function FindAndAddNearestEdge(
      const ALatitude: Double;
      const ALongitude: Double;
      const ADir: TADir;
      const AMinDistToLineM: Integer;
      var AID: TIDLines;
      var ADist: double;
      var APointOnLine: TGeoPos{;
      const ARoadWorkSet: TRoadTypeSet;
      const ARoadSpeeds: TRoadSpeedsRecord;
      const AZoneMask: TZoneControl;
      const AFeatureLimit: UInt64}
    ): Boolean;
{    function Calc(
      const AAccs: PInteger;
      const AAStarRequest: PAstarRequest
    ): Integer; overload;
    function Calc(): Integer; overload;
    function Calc(AReductionFactorDef: Integer): Integer; overload;}
    procedure Init;
    procedure CleanUp;
    procedure CleanUpFull;
    procedure SetCalcParams(
      const AAccs: PInteger;
      const AAStarRequest: PAstarRequest
    ); overload;
    procedure SetCalcParams3(
      const AAccs: PInteger;
      const AAStarRequest: PAstarRequest3
    ); //overload;
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
      const ASigns: TSignControl = CSignsMaskDef
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
      const ARLAR: PRoadLengthAggregateRecord = nil
    ): Integer;
    procedure MakeHash(
      var RHashString: PAnsiChar; const AFaultReason: Integer;
      const AFormat: Integer = 0
    );
    procedure MakeSpeedline(
      var RSpeedlineString: PAnsiChar
    );
    function CheckGraphVersion: Boolean;
    class procedure Log(AMessage: string);
  end;

procedure TransformArr(
  const ASrc: PInteger;
  var RDest: TIntegerDynArray
);

//------------------------------------------------------------------------------
implementation

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
//------------------------------------------------------------------------------
begin
{  SetLength(RDest, 2);
  RDest[0] := 2;
  RDest[1] := 1;}
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

function Reverse(aDir: TADir): TADir;
begin
  Result := adForward;
  case aDir of
    adForward: Result := adBackward;
    adBackward: Result := adForward;
  end;
end;

//------------------------------------------------------------------------------
// TSearch
//------------------------------------------------------------------------------

constructor TSearch.Create;
begin
  Init;
end;

//class constructor TSearch.Create;
//begin
//  AStar := TSearch.Create;
//end;
//
//class destructor TSearch.Destroy;
//begin
//  AStar.Free;
//end;

procedure TSearch.SetCalcParams(
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

procedure TSearch.SetCalcParams3(
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
    AAStarRequest.ZonesLimit
  );
  FFeatureLimit := AAStarRequest.FeatureLimit;
end;

procedure TSearch.SetCalcParams(
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
    AAStarRequest.ZonesLimit
  );
  FFeatureLimit := AAStarRequest.Feature;
end;

procedure TSearch.SetCalcParams(
  const AAccs: PInteger;
  const AFromLatitude: Double;
  const AFromLongitude: Double;
  const AToLatitude: Double;
  const AToLongitude: Double;
  const ARoadSpeeds: TRoadSpeedsRecord;
  const ATimeOut: Integer = CTimeoutDef;
  const ALenTreshold: Double = CReductionTresholdDef;
  const AZones: TZoneControl = CZonesMaskDef;
  const ASigns: TSignControl = CSignsMaskDef
);
begin
  TransformArr(AAccs, FAccounts);
  //
//  SetLength(FAccounts, 1);
//  FAccounts[0] := 1;
  FFromLatitude := AFromLatitude;
  FFromLongitude := AFromLongitude;
  FToLatitude := AToLatitude;
  FToLongitude := AToLongitude;
  FTimeOut := ATimeOut;
  FLenTreshold := ALenTreshOld;
  FZones := AZones;
  FSigns := ASigns;
  FRoadSpeeds := ARoadSpeeds;
  FRoadWorkSet := GetFullRoadWorkSet(FRoadSpeeds);
  FFeatureLimit := 0;
end;

procedure TSearch.SetCalcParamsSign(
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
  FFeatureLimit := AAStarRequest.FeatureLimit;
end;
{
procedure TSearch.SetCurrentFeatue(aDir: TADir; Value: UInt64);
begin
  case aDir of
    adForward: FFromCurrentFeature := Value;
    adBackward: FToCurrentFeature := Value;
  end;
end;   }

procedure TSearch.Init;
begin
  FFeatureLimit := 0;
  FFilesHolder := TFileAgglomeration.Create();

  FFromList := TAStarList.Create();
  FToList := TAStarList.Create();
end;

procedure TSearch.CleanUp;
begin
  FCountFrom := 0;
  FCountTo := 0;
  Node[adForward] := nil;
  Node[adBackward] := nil;
  FFromList.Clear;
  FToList.Clear;
end;

procedure TSearch.CleanUpFull;
begin
  CleanUp;

  FFilesHolder.Clear;
end;

{constructor TSearch.Create(
//  const AHandle: Pointer;
//  const ASpeedCB: TSpeedCallbackFunc;
  const AFromLatitude: Double;
  const AFromLongitude: Double;
  const AToLatitude: Double;
  const AToLongitude: Double;
  const ATimeOut: Integer;
  const ALenTreshOld: Double;
  const AZones: TZoneControl;
  const ARoadSpeeds: PRoadSpeedsRecord
);
begin
  Init;
  SetCalcParams(
    AFromLatitude, AFromLongitude, AToLatitude, AToLongitude,
    ATimeOut, ALenTreshOld, AZones, ARoadSpeeds);
end;}

destructor TSearch.Destroy();
begin
  FFilesHolder.Free();
  FFromList.Free();
  FToList.Free();
  //
  SetLength(FAccounts, 0);
  //
  inherited Destroy();
end;


function TSearch.CheckGraphVersion: Boolean;
var
  VerStr: string;
  ini: TIniFile;
begin
//
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
(*
function TSearch.Calc(
  const AAccs: PInteger;
  const AAStarRequest: PAstarRequest
): Integer;
begin
  CleanUp;
  SetCalcParams(
    AAccs,
    AAStarRequest.FromLatitude,
    AAStarRequest.FromLongitude,
    AAStarRequest.ToLatitude,
    AAStarRequest.ToLongitude,
    AAStarRequest.RoadSpeedsRecord,
    AAStarRequest.Timeout,
    AAStarRequest.LenTreshold,
    AAStarRequest.ZonesLimit
  );
  Result := Calc;
end;

function TSearch.Calc(): Integer;
var
  ReductionFactor: Integer;
begin
  ReductionFactor := CReductionFactorStartDef;
  repeat
    Result := Calc(ReductionFactor);
    Dec(ReductionFactor);
  until (ReductionFactor < CReductionFactorDef) or (Result = 0);
end;

function TSearch.Calc(AReductionFactorDef: Integer): Integer;
label S;
var
  //!
  PassCounter: Integer;
  //!
  FirstTryCounter: Integer;
  //!
  NeedRecalcFrom, NeedRecalcTo: Boolean;
  //! обрабатываемая область
  AreaWork: TArea;
  //!
  NewNodesList: TNodeArray;
  //!
  NodesIter: TNode;
  //! структура для функции обратного вызова
  // CBRecord: TMetaDataV1;
  //! результат функции обратного вызова
  // CBResultSpeed: Integer;
  ReverseSpeed: Integer;
  //!
  Started: TDateTime;
  StepStarted: TDateTime;
  StepTime: TDateTime;
  StepSec: Integer;

  fs: TFormatSettings;

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

  procedure SetNeedToRecalc(const ADir: TADir; const AValue: Boolean);
  begin
    if ADir = adForward then
      NeedRecalcFrom := AValue
    else
      NeedRecalcTo := AValue;
  end;

const
  CMaxNodeListLimit = 10000;  //подобрано експериментально
  CReportedSpeedFactor = 1;//0.9;

var
  Dir: TADir;
//------------------------------------------------------------------------------
begin
  Radius[adForward] := 0;
  Radius[adBackward] := 0;
  RadiusMax[adForward] := 0;
  RadiusMax[adBackward] := 0;
  ReductionFactor[adForward] := 0;
  ReductionFactor[adBackward] := 0;
  Started := 0;
  StepSec := 0;
  Result := 0;
  PassCounter := 0;
  CurrentFeatue[adForward] := 0;
  CurrentFeatue[adBackward] := 0;

  Log(
    'arf=' + IntToStr(AReductionFactorDef) +
    ',cfrom=(' + IntToStr(List[adForward].Count) + ';'+ IntToStr(List[adForward].OpenedCount) + ')' +
    ',cto=(' + IntToStr(List[adBackward].Count)  + ';'+ IntToStr(List[adBackward].OpenedCount) + ')' +
    ''
  );

  try
    List[adForward].Clear;
    List[adBackward].Clear;

    fs := TFormatSettings.Create;
    fs.DecimalSeparator := '.';
    Started := Now;
    StepStarted := Now;
    // 0.
    //  CBRecord.Version := 1;
    FirstTryCounter := 0;
    NeedRecalcFrom := True;
    NeedRecalcTo := True;
    ReverseSpeed := CRoadSpeedRecordDef.GetSpeedByRoadType(cBackwardRoadTypeDef);
    // 0. Здесь начинается цикл смещения поиска начального ребра при заходе в тупик
S:
    PassCounter := 0;
    // 1. найдём и добавим в списки начальные рёбра графа
    // А) для начальной координаты
    // поиск ближайшего ребра
    if NeedRecalcFrom then
    begin
      if not FindAndAddNearestEdge(FFromLatitude, FFromLongitude, adForward, FRoadWorkSet, FRoadSpeeds, FZones, FFeatureLimit) then
        Exit(ERROR_ASTAR_NEAR_EDGE_NOT_FOUND);
      NeedRecalcFrom := False;
    end;
    // Б) для конечной координаты
    // поиск ближайшего ребра
    if NeedRecalcTo then
    begin
      if not FindAndAddNearestEdge(FToLatitude, FToLongitude, adBackward, FRoadWorkSet, FRoadSpeeds, FZones, FFeatureLimit) then
        Exit(ERROR_ASTAR_NEAR_EDGE_NOT_FOUND);
      NeedRecalcTo := False;
    end;

    repeat
      for Dir := Low(TADir) to High(TADir) do
      begin
        SetLength(NewNodesList, 0);
        repeat

          Node[Dir] := List[Dir].GetWorkNode(CurrentFeatue[Dir]);
          if not Assigned(Node[Dir]) then
          begin
            if (CurrentFeatue[Dir] > 0) then
              Break;

            if (FirstTryCounter >= CFirstSpecialIterations) then // если не получили, то пути не существует
              Exit(ERROR_ASTAR_NO_WAY_FOUND);

            SetNeedToRecalc(Dir, True);

            Inc(FirstTryCounter);
            goto S;
          end;
          // проверяем на соединение прямого и обратного путей

          Node[Reverse(Dir)] :=  List[Dir].GetConnection(Node[Dir], List[Reverse(Dir)]);
          if Assigned(Node[Reverse(Dir)]) then
          begin
{$ifdef ASTAR_DEBUG}
            TSearch.Log(' ' + IfThen(Dir = adForward, 'f', 'b') + ' CONNECTED forw: ' +
              TGeoHash.ConvertBinToString(Node[Dir].HashVector.PointFrom, 12) + '-' +
              TGeoHash.ConvertBinToString(Node[Dir].HashVector.PointTo, 12));
            TSearch.Log(' ' + IfThen(Dir = adForward, 'f', 'b') + ' CONNECTED backw: ' +
              TGeoHash.ConvertBinToString(Node[Reverse(Dir)].HashVector.PointFrom, 12) + '-' +
              TGeoHash.ConvertBinToString(Node[Reverse(Dir)].HashVector.PointTo, 12));
{$endif}
            Exit(ERROR_ASTAR_SUCCESS);
          end;

          // делаем запрос к TArea на развёртку ребра
          AreaWork := FFilesHolder.LoadArea(Node[Dir].HashVector.PointFrom, FAccounts);
          if not Assigned(AreaWork) then
            Exit(ERROR_ASTAR_FILE_NOT_FOUND);


          Radius[Dir] := TGeoCalcs.GeoLengthKmDeg(Node[Dir].CoordsTo.Latitude, Node[Dir].CoordsTo.Longitude,  Latitude[Dir], Longitude[Dir]);
          if Radius[Dir] > RadiusMax[Dir] then
            RadiusMax[Dir] := Radius[Dir];

          ReductionFactor[Dir] :=
            IfThen(
              (RadiusMax[Dir] < FLenTreshold) and (List[Dir].Count < CMaxNodeListLimit),
              0, AReductionFactorDef);
          AreaWork.ParseRef(Node[Dir], ReductionFactor[Dir], Dir, FRoadWorkSet, FRoadSpeeds, FFeatureLimit, NewNodesList, FAccounts);
          // выходной массив нулевой длины - не обязательно фатальная ошибка
        until (Length(NewNodesList) <> 0);
        // по списку рёбер
        for NodesIter in NewNodesList do
        begin
          if NodesIter.HasZone(FZones) then
            Continue;

          if (NodesIter.ThinLink.Count > 0) and (CurrentFeatue[Dir] = 0) then
            CurrentFeatue[Dir] := 1;

          // рассчитать характеристики и добавить к списку
          NodesIter.FillNodeHeuristics(
            {CalcSpeed,} {ReverseSpeed,} {ReportedSpeed,}
            Latitude[Dir], Longitude[Dir], Latitude[Reverse(Dir)], Longitude[Reverse(Dir)], FRoadSpeeds);

{$ifdef ASTAR_DEBUG}
          TSearch.Log('   new ' + IfThen(Assigned(NodesIter.ThinEdge), 'THIN ', '') +
          'node (' + Format('f, g, h = %.3f, %.3f, %.3f, l, d = %.3f, %.3f', [NodesIter.F, NodesIter.G, NodesIter.H, NodesIter.Distance, NodesIter.Duration]) + ', ' +
          '): ' +
            TGeoHash.ConvertBinToString(nodesiter.HashVector.PointFrom, 12) + '-' +
            TGeoHash.ConvertBinToString(nodesiter.HashVector.PointTo, 12));
{$endif}
          List[Dir].AddNew(NodesIter);
          // проверим на дубль пути в эту точку
          List[Dir].CheckLastEdge(PassCounter);
        end;
      end;
      //
      Inc(PassCounter);

      if (PassCounter mod 1000) = 0 then
      begin
        StepTime := Now - StepStarted;
        StepSec := Trunc((StepTime) * SecsPerDay);
        Log(
          'pass=' + IntToStr(PassCounter) +
          ', time=' + IntToStr(Trunc((Now - Started) * SecsPerDay)) + 's' +
          ', from='+FormatFloat('#.##', FRadiusFromMax, fs)+
          ', to='+FormatFloat('#.##', FRadiusToMax, fs)+
          ', rfrom=' + IntToStr(FReductionFactorFrom) +
          ', rto=' + IntToStr(FReductionFactorTo) +
          ', rdef=' + IntToStr(AReductionFactorDef) +
          ', ffrom = ' + IntToStr(FFromCurrentFeature) +
          ', fto = ' + IntToStr(FToCurrentFeature) +
          ', cfrom=(' + IntToStr(FFromList.Count) + ';'+ IntToStr(FFromList.OpenedCount) + ')' +
          ', cto=(' + IntToStr(FToList.Count)  + ';'+ IntToStr(FToList.OpenedCount) + ')' +
          ', rsfrom=('+ CountWS(FRoadWorkSet)+';' + CountWS(GetReducedRoadWorkSet(FRoadWorkSet, FReductionFactorFrom)) + ')' +
          ', rsto=('+ CountWS(FRoadWorkSet)+';' + CountWS(GetReducedRoadWorkSet(FRoadWorkSet, FReductionFactorTo)) + ')' +
          ', stime=' + IntToStr(StepSec) + 's' +
//          ', npsfrom = ' + IntToStr(Trunc(IfThen(StepTime = 0, 0, FFromList.Count/(StepTime * SecsPerDay)))) +
//          ', npsto = ' + IntToStr(Trunc(IfThen(StepTime = 0, 0, FToList.Count/(StepTime * SecsPerDay)))) +
          '');
        StepStarted := Now;
      end;

      if ((Now - Started) * SecsPerDay) > FTimeOut then
      begin
        Exit(ERROR_ASTAR_TIMEOUT);
      end;
    until False;
  finally
    Log(
      'pass = ' + IntToStr(PassCounter) +
      ', time  = ' + IntToStr(Trunc((Now - Started) * SecsPerDay)) + 's' +
      ', from = '+FormatFloat('#.##', FRadiusFromMax, fs)+
      ', to = '+FormatFloat('#.##', FRadiusToMax, fs)+
      ', rfrom = ' + IntToStr(FReductionFactorFrom) +
      ', rto = ' + IntToStr(FReductionFactorTo) +
      ', rdef = ' + IntToStr(AReductionFactorDef) +
      ', ffrom = ' + IntToStr(FFromCurrentFeature) +
      ', fto = ' + IntToStr(FToCurrentFeature) +
      ', cfrom = (' + IntToStr(FFromList.Count) + ';'+ IntToStr(FFromList.OpenedCount) + ')' +
      ', cto = (' + IntToStr(FToList.Count)  + ';'+ IntToStr(FToList.OpenedCount) + ')' +
      ', rsfrom = ('+ CountWS(FRoadWorkSet)+';' + CountWS(GetReducedRoadWorkSet(FRoadWorkSet, FReductionFactorFrom)) + ')' +
      ', rsto = ('+ CountWS(FRoadWorkSet)+';' + CountWS(GetReducedRoadWorkSet(FRoadWorkSet, FReductionFactorTo)) + ')' +
      ', stime = ' + IntToStr(StepSec) + 's' +
      ', res = ' + IntToStr(Result));

  end;
end;*)

function TSearch.CountDD(
  var RDistance: Double;
  var RDuration: Double;
  const AFaultReason: Integer;
  const ARLTLen: Integer;
  const ARLALen: Integer;
  const AStatRecord: PStatRecord = nil;
  const ARZTL: PRoadLengthByTypeRecord = nil;
  const ARLAR: PRoadLengthAggregateRecord = nil
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

  procedure AddRoadLen(az: UInt64; at: Cardinal; al: Double);
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
      begin
        l := ANodeWork.SEdge.Distance;
        z := ANodeWork.Zone;
        t := ANodeWork.SEdge.RoadType;
        AddRoadLen(z, t, l);
        if Assigned(AStatRecord) then
          AStatRecord.OrdinaryLen := AStatRecord.OrdinaryLen + l;
      end;
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
  //! обрабатываемый узел
  NodeWork: TNode;
//------------------------------------------------------------------------------
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
      FCountFromFirst := NodeWork.WayCount + 1;
      FCountFrom := FCountFrom + FCountFromFirst;
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

      FCountToFirst := NodeWork.WayCount + 1;
      FCountTo := FCountTo + FCountToFirst;
      RDistance := RDistance + NodeWork.Distance;
      RDuration := RDuration + NodeWork.Duration;
      CalcLength(NodeWork, adBackward);
    until False;
    FillLength;
  finally
    ZRLTHash.Free;
  end;
end;

procedure TSearch.MakeHash(
  var RHashString: PAnsiChar; const AFaultReason: Integer;
  const AFormat: Integer
);
var
  //!
  I: Integer;
  //!
  PointsCount: Integer;
  //!
  BytesCount: Integer;
  //! индексы ближейших найденных рёбер
  FoundIndex: Integer;
  //!
  PathCPos: Integer;
  //!
  DistCurrent: Double;
  //!
  DistMax: Double;
  //! обрабатываемый узел
  NodeWork: TNode;
  //! выходные данные - список точек
  OutputPosition: TGeoPathPointArray;
  //!
  RezStr: AnsiString;
  //!
//  ReportedSpeed: Integer;

  FoundFirst: Integer;

  FoundLast: Integer;
//------------------------------------------------------------------------------
  function GeoHashStringFormat(const AFormat: Integer): TGeoHashStringFormat;
  begin
    case AFormat of
      1: Result := gfSpeed;
      2: Result := gfTypeAndZone;
      3: Result := gfType;
      4: Result := gfZone;
      else Result := gfPath;
    end;
  end;

begin
  if AFaultReason < ERROR_ASTAR_SUCCESS then
  begin
    SetLength(OutputPosition, 2);
    OutputPosition[0].p := TGeoPos.Create(FFromLatitude, FFromLongitude);
    OutputPosition[0].s := FRoadSpeeds.Residential;
    OutputPosition[0].t := RT_residential;
    OutputPosition[0].z := 0;
    OutputPosition[1].p := TGeoPos.Create(FToLatitude, FToLongitude);
    OutputPosition[1].s := FRoadSpeeds.Residential;
    OutputPosition[1].t := RT_residential;
    OutputPosition[1].z := 0;
  end
  else
  begin
    PointsCount := FCountFrom + FCountTo + 2; // (+2 точки начала и конца)
    SetLength(OutputPosition, PointsCount);
    // заполнение - прямой путь
//    OutputPosition[FCountFrom + 1].p := Node[adForward].CoordsTo; // сразу ставим последнюю точку последнего ребра = точку связи
//TODO!!!
//    OutputPosition[FCountFrom + 1].s := Node[adForward].MaxSpeed;
//    OutputPosition[FCountFrom + 1].t := Node[adForward].RoadType;
//    OutputPosition[FCountFrom + 1].z := Node[adForward].Zone;
    NodeWork := Node[adForward];

{$ifdef ASTAR_DEBUG}
            TSearch.Log(' Node[adForward]: ' +
              TGeoHash.ConvertBinToString(NodeWork.HashVector.PointFrom, 12) + '-' +
              TGeoHash.ConvertBinToString(NodeWork.HashVector.PointTo, 12));
{$endif}

    PathCPos := FCountFrom;
    repeat
//      if Assigned(NodeWork.ThinEdge) then
//      begin
{$ifdef ASTAR_DEBUG}
        TSearch.Log(
          TGeoHash.ConvertBinToString(NodeWork.HashVector.PointTo, 12) + '-' +
          TGeoHash.ConvertBinToString(NodeWork.HashVector.PointFrom, 12));
//        TSearch.Log(
//          TGeoHash.EncodePointString(WpToGeoPos(NodeWork.WayPoints[0, adForward]), 12) + '-' +
//          TGeoHash.EncodePointString(WpToGeoPos(NodeWork.WayPoints[NodeWork.WayCount, adForward]), 12));
{$endif}
//      end;

      PathCPos := PathCPos - NodeWork.WayCount;
      for I := 0 to NodeWork.WayCount do
      begin
        OutputPosition[PathCPos + I].p := NodeWork.WayPoints[I, adForward].AsGeoPos;
        OutputPosition[PathCPos + I].s := FRoadSpeeds.GetSpeedByRoadType(NodeWork.WayPoints[I, adForward].RType); //NodeWork.WayPoints[I, adForward].RSpeed;
        OutputPosition[PathCPos + I].t := NodeWork.WayPoints[I, adForward].RType;
        OutputPosition[PathCPos + I].z := NodeWork.Zone; //NodeWork.WayPoints[I, adForward].Zone;
      end;
      //
      NodeWork := NodeWork.ParentNode;
      Dec(PathCPos);
    until (PathCPos = 0);
{$ifdef ASTAR_DEBUG}
    TSearch.Log('---------');
{$endif}
    // заполнение - обратный путь
    NodeWork := Node[adBackward];

{$ifdef ASTAR_DEBUG}
    TSearch.Log(' Node[adBackward]: ' +
      TGeoHash.ConvertBinToString(NodeWork.HashVector.PointFrom, 12) + '-' +
      TGeoHash.ConvertBinToString(NodeWork.HashVector.PointTo, 12));
{$endif}
    PathCPos := FCountFrom + 1;
    repeat
      NodeWork := NodeWork.ParentNode; // сразу пропускаем первое ребро = оно - копия прямого пути

      if not Assigned(NodeWork) then
        Break;

//      if Assigned(NodeWork.ThinEdge) then
//      begin
{$ifdef ASTAR_DEBUG}
        TSearch.Log(
          TGeoHash.ConvertBinToString(NodeWork.HashVector.PointTo, 12) + '-' +
          TGeoHash.ConvertBinToString(NodeWork.HashVector.PointFrom, 12));
//        TSearch.Log(
//          TGeoHash.EncodePointString(WpToGeoPos(NodeWork.WayPoints[0, adBackward]), 12) + '-' +
//          TGeoHash.EncodePointString(WpToGeoPos(NodeWork.WayPoints[NodeWork.WayCount, adBackward]), 12));
{$endif}
//      end;
      for I := 0 to NodeWork.WayCount do
      begin
        OutputPosition[PathCPos].p := NodeWork.WayPoints[I, adBackward].AsGeoPos;
        OutputPosition[PathCPos].s := FRoadSpeeds.GetSpeedByRoadType(NodeWork.WayPoints[I, adBackward].RType); //NodeWork.WayPoints[I, adBackward].RSpeed;
        OutputPosition[PathCPos].t := NodeWork.WayPoints[I, adBackward].RType;
        OutputPosition[PathCPos].z := NodeWork.Zone; //NodeWork.WayPoints[I, adBackward].Zone;
        Inc(PathCPos);
      end;
    until False;
{$ifdef ASTAR_DEBUG}
    TSearch.Log('-----------------------------------------------------------');
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
    end;
    // коррекция количества точек
    PointsCount := PointsCount - FCountFromFirst;
    OutputPosition := Copy(OutputPosition, FCountFromFirst, PointsCount);
  end;
  // итог
  RezStr := AnsiString(TGeoHash.EncodeArrayAnyFormat(OutputPosition, 12, GeoHashStringFormat(AFormat)));
  Log('Length(RezStr)=' + IntToStr(Length(RezStr)));
  BytesCount := Length(RezStr) + 1;
  GetMem(RHashString, BytesCount);
  FillChar(RHashString^, BytesCount, 0);
  Move(RezStr[1], RHashString^, BytesCount);
end;

procedure TSearch.MakeSpeedline(
  var RSpeedlineString: PAnsiChar
);
var
  //!
  I: Integer;
  //!
  BytesCount: Integer;
  //!
  PathCPos: Integer;
  //! обрабатываемый узел
  NodeWork: TNode;
  //!
  RezStr: AnsiString;
  //!
  SpeedStr: AnsiString;
//------------------------------------------------------------------------------
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

function TSearch.FindAndAddNearestEdge(
  const ALatitude: Double;
  const ALongitude: Double;
  const ADir: TADir;
  const AMinDistToLineM: Integer;
  var AID: TIDLines;
  var ADist: double;
  var APointOnLine: TGeoPos{;
  const ARoadWorkSet: TRoadTypeSet;
  const ARoadSpeeds: TRoadSpeedsRecord;
  const AZoneMask: TZoneControl;
  const AFeatureLimit: UInt64}
): Boolean;
var
  //!
  I, J: Integer;
  //!
  SquareArray: THashSquareArray;
  //!
  WorkEdges: TEdgeArray;
  //!
  WorkZones: TZoneControlArray;
  //!
  WorkArea: AStar64.Extra.TArea;
  //!
//  AddedNode: TNode;
  //!
  DistCurrent, DistFound: Double;
  fs: TFormatSettings;
  PointOnLine: TGeoPos;

//! для данного ребра находим расстояние до него с учётом промежуточных точек пути
function DistanceForIter(var APoint: TGeoPos): Double;
var
  //!
  K: Integer;
  //!
  C: Double;
  //!
  GeoArray: TGeoPosArray;
  Point: TGeoPos;
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
      C := TGeoCalcs.GeoDistancePointToLineM(
        TGeoPos.Create(ALatitude, ALongitude),
        TGeoPos.Create(GeoArray[K].Latitude, GeoArray[K].Longitude),
        TGeoPos.Create(GeoArray[K + 1].Latitude, GeoArray[K + 1].Longitude),
        Point
      );
      if (Result > C) then
      begin
        Result := C;
        APoint := Point;
      end;
    end;
  end
  else
  begin
    Result := TGeoCalcs.GeoDistancePointToLineM(
      TGeoPos.Create(ALatitude,ALongitude),
      TGeoPos.Create(WorkEdges[I].CoordsFrom.Latitude, WorkEdges[I].CoordsFrom.Longitude),
      TGeoPos.Create(WorkEdges[I].CoordsTo.Latitude,WorkEdges[I].CoordsTo.Longitude),
      APoint
    );
  end;
end;

//------------------------------------------------------------------------------
begin
  Result := False;
  fs := TFormatSettings.Create;
  fs.DecimalSeparator := '.';
  DistFound := Infinity;
  SquaresAround(TGeoHash.EncodePointBin(ALatitude, ALongitude) and CAreaHashMask, SquareArray);
  // стадия 1 - поиск ближайшего
  for J := Low(SquareArray) to High(SquareArray) do
  begin
    WorkArea := FFilesHolder.LoadArea(SquareArray[J], FAccounts);
    if not Assigned(WorkArea) then
      Continue;
    WorkEdges := WorkArea.Edges[ADir];
    WorkZones := WorkArea.ZoneCtrls[ADir];
    for I := Low(WorkEdges) to High(WorkEdges) do
    begin
{      if ((AZoneMask and WorkZones[I]) = 0)
      and IsRoadFits(WorkEdges[I].RoadType, ARoadWorkSet) then}
      begin
        DistCurrent := DistanceForIter(PointOnLine);
        if (DistFound > DistCurrent) then
        begin
          DistFound := DistCurrent;
          if DistFound <= AMinDistToLineM then
          begin
            Result := True;
            APointOnLine := PointOnLine;
            AID.OSM_ID := WorkEdges[I].ID;
            AID.HashStart := WorkEdges[I].HashVector.HashFrom;
            AID.HashEnd := WorkEdges[I].HashVector.HashTo;
          end;
          ADist := DistFound;
        end;
      end;
    end;
  end;

  // стадия 2 - добавление в списки
(*  for J := Low(SquareArray) to High(SquareArray) do
  begin
    WorkArea:= FFilesHolder.LoadArea(SquareArray[J], FAccounts);
    if not Assigned(WorkArea) then
      Continue;
    WorkEdges := WorkArea.Edges[ADir];
    WorkZones := WorkArea.ZoneCtrls[ADir];
    for I := Low(WorkEdges) to High(WorkEdges) do
    begin
{      if ((WorkZones[I] and AZoneMask) = 0)
      and IsRoadFits(WorkEdges[I].RoadType, ARoadWorkSet) then}
      begin
        if (DistanceForIter() = DistFound)
        or (TGeoCalcs.GeoLengthDeg(
          // или если расстояние до начала дороги менее 3-х метров
          WorkEdges[I].CoordsFrom.Latitude,
          WorkEdges[I].CoordsFrom.Longitude,
          Latitude[ADir],
          Longitude[ADir]
        ) < 1) then
        begin
          Result := True;
//          AddedNode := TNode.Create(nil, WorkArea, ADir, I, AFeatureLimit, True);
          AddedNode := TNode.Create(nil, WorkArea, ADir, I, 0, True);
          AddedNode.H := TGeoCalcs.GeoLengthKmDeg(
            WorkEdges[I].CoordsTo.Latitude,
            WorkEdges[I].CoordsTo.Longitude,
            Latitude[Reverse(ADir)],
            Longitude[Reverse(ADir)]
          ) * CBackwardCoeff;
          AddedNode.F := AddedNode.H;
          List[ADir].AddNew(AddedNode);
          AID.OSM_ID := WorkEdges[I].ID;
          AID.HashStart := WorkEdges[I].HashVector.PointFrom;
          AID.HashEnd := WorkEdges[I].HashVector.PointTo;
        end;
      end;
    end;
  end;*)
end;
{
function TSearch.GetList(aDir: TADir): TAStarList;
begin
  Result := nil;
  case aDir of
    adForward: Result := FFromList;
    adBackward: Result := FToList;
  end;
end; }

function TSearch.GetNode(aDir: TADir): TNode;
begin
  Result := nil;
  case aDir of
    adForward: Result := FFromNode;
    adBackward: Result := FToNode;
  end;
end;

procedure TSearch.SetNode(aDir: TADir; const Value: TNode);
begin
  case aDir of
    adForward: FFromNode := Value;
    adBackward: FToNode := Value;
  end;
end;

{function TSearch.GetRadius(aDir: TADir): Double;
begin
  Result := 0;
  case aDir of
    adForward: Result := FRadiusFrom;
    adBackward: Result := FRadiusTo;
  end;
end;

procedure TSearch.SetRadius(aDir: TADir; Value: Double);
begin
  case aDir of
    adForward: FRadiusFrom := Value;
    adBackward: FRadiusTo := Value;
  end;
end;

function TSearch.GetRadiusMax(aDir: TADir): Double;
begin
  Result := 0;
  case aDir of
    adForward: Result := FRadiusFromMax;
    adBackward: Result := FRadiusToMax;
  end;
end;

procedure TSearch.SetRadiusMax(aDir: TADir; Value: Double);
begin
  case aDir of
    adForward: FRadiusFromMax := Value;
    adBackward: FRadiusToMax := Value;
  end;
end;

function TSearch.GetReductionFactor(aDir: TADir):Integer;
begin
  Result := 0;
  case aDir of
    adForward: Result := FReductionFactorFrom;
    adBackward: Result := FReductionFactorTo;
  end;
end;

procedure TSearch.SetReductionFactor(aDir: TADir; Value:Integer);
begin
  case aDir of
    adForward: FReductionFactorFrom := Value;
    adBackward: FReductionFactorTo := Value;
  end;
end;

function TSearch.GetCurrentFeatue(aDir: TADir): UInt64;
begin
  Result := 0;
  case aDir of
    adForward: Result := FFromCurrentFeature;
    adBackward: Result := FToCurrentFeature;
  end;
end;

function TSearch.GetLatitude(aDir: TADir): Double;
begin
  Result := 0;
  case aDir of
    adForward: Result := FFromLatitude;
    adBackward: Result := FToLatitude;
  end;
end;

procedure TSearch.SetLatitude(aDir: TADir; Value: Double);
begin
  case aDir of
    adForward: FFromLatitude := Value;
    adBackward: FToLatitude := Value;
  end;
end;

function TSearch.GetLongitude(aDir: TADir): Double;
begin
  Result := 0;
  case aDir of
    adForward: Result := FFromLongitude;
    adBackward: Result := FToLongitude;
  end;
end;

procedure TSearch.SetLongitude(aDir: TADir; Value: Double);
begin
  case aDir of
    adForward: FFromLongitude := Value;
    adBackward: FToLongitude := Value;
  end;
end;}

class procedure TSearch.Log(AMessage: string);
begin
  ToSysLog('127.0.0.1', FCL_UserLevel, Debug, 'Astar.Calc.Heartbeat='+FormatDateTime('yyyy-mm-dd hh:nn:ss.zzz', Now)+';pid=' + IntToStr(GetCurrentProcessId) + ';' + AMessage);
end;

end.

