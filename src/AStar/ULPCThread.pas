unit ULPCThread;

interface

uses
  SysUtils, Classes, Windows, SyncObjs,
  System.Generics.Collections, System.Generics.Defaults,
  AStar64.FileStructs, AStar64.Typ, AStar64.DynImport,
  Geo.Hash, Ils.Utils, Ils.Logger, System.Types,
  AStar64.LandMark;

type
  TSaveFunc = procedure(
    const ALmMatrixFrom: TLandMarkMatrix
  ) of object;

  TTaskThread = class(TThread)
  private
    FLmKey: TLandMarkWayKey;
    FSaveFunc: TSaveFunc;
    FAccounts: TIntegerDynArray;
    FTimeout: Integer;
    FRecalcAll: Boolean;
    FResDir: string;
    procedure DoWork();
  protected
    procedure Execute(); override;
  public
    constructor Create(
      const ALmKey: TLandMarkWayKey;
      const ASaveFunc: TSaveFunc;
      const AAccounts: TIntegerDynArray;
      const ATimeout: Integer;
      const ARecalcAll: Boolean;
      const AResDir: string = ''
    );
  end;

  TTaskList = class(TList<TTaskThread>)
    destructor Destroy; override;
  end;

  TTaskQueue = class
  private
    FTaskList: TTaskList;
    FLmMatrix: TLandMarkMatrix;
    FThreads: Integer;
    FTimeout: Integer;
    FRecalcAll: Boolean;
//    FAccounts: TIntegerDynArray;
    FCS: TCriticalSection;
    function CollectGarbage(): Integer;
    procedure SaveResults(
      const ALmMatrixFrom: TLandMarkMatrix
    );
  public
    constructor Create(
      const ALmMatrix: TLandMarkMatrix;
      const AThreads: Integer;
      const ATimeout: Integer;
      const ARecalcAll: Boolean
    );
    destructor Destroy(); override;
    procedure PushTask(
      const ALmKey: TLandMarkWayKey;
      const AFullCalc: Boolean;
      const AAccounts: TIntegerDynArray
    );
    procedure WaitForAllDone();
  end;

procedure Log(const AStr: string);

implementation

var
  GCS: TCriticalSection;

procedure WritePath(
  const ALandMarkMatrix: TLandMarkMatrix;
  const AKey: TLandMarkWayKey;
  const APath: string;
  const ADistance: Double
);
var
  Way: TLandMarkWay;
begin
  if not ALandMarkMatrix.ContainsKey(AKey) then
    ALandMarkMatrix.Add(AKey, TLandMarkWay.Create(APath, ADistance))
  else
  begin
    Way := ALandMarkMatrix.Items[AKey];
    if (Way.Distance = 0) or (Way.Distance > ADistance) then
      Way.NewData(APath, ADistance);
  end;
end;

procedure WriteErrorPath(
  const ALandMarkMatrix: TLandMarkMatrix;
  const AKey: TLandMarkWayKey;
  const AValue: string = 'error'
);
begin
  if not ALandMarkMatrix.ContainsKey(AKey) then
    ALandMarkMatrix.Add(AKey, TLandMarkWay.Create(AValue, 0))
  else
    if (ALandMarkMatrix.Items[AKey].GeoHash = '') then
      ALandMarkMatrix.Items[AKey].NewData(AValue, 0);
end;

procedure Log(const AStr: string);
begin
  GCS.Acquire;
  try
    ToLog('"' + IntToStr(GetCurrentThreadId()) + '" ' + AStr);
//    Writeln(DateTimeToIls(Now()), '> "', GetCurrentThreadId(), '" ', AStr);
  finally
    GCS.Release;
  end;
end;

function CalcPath(
  const AVector: THashVector;
  const AZonesExcluded: UInt64;
  out RZonesVisited: UInt64;
  out RDistance: Double;
  AAccounts: TIntegerDynArray;
  const ATimeout: Integer
): string;

  function GetZones(
    const ALMPath: string;
    const AFormatStartPos: Integer;
    const AFormatIncrement: Integer
  ): Int64;
  var
    i: Integer;
    d: string;
  begin
    Result := 0;
    i := AFormatStartPos;
    repeat
      d := Copy(ALMPath, i + (12 + 1 + 2), 16);
      if (d = '') then
        Break;
      Result := Result or StrToInt64('$' + d);
      Inc(i, AFormatIncrement);
    until False;
  end;

var
  ar: TAstarRequest4;
  CalcRez: Integer;
  fmt:TFormatSettings;
begin
  if Length(AAccounts) = 0 then
  begin
    SetLength(AAccounts, 2);
    AAccounts[0] := 1;
    AAccounts[1] := 0;
  end else
  begin
    SetLength(AAccounts, Length(AAccounts) + 1);
    AAccounts[High(AAccounts)] := 0;
  end;

Log(
  'Calculating path ' + AVector.HashStr + Format(
    ' %.6f %.6f - %.6f %.6f',
    [AVector.PointFrom.Latitude, AVector.PointFrom.Longitude, AVector.PointTo.Latitude, AVector.PointTo.Longitude]
  ) + ' excluding zones ' + IntToHex(AZonesExcluded, 16) + ' Timeout='+IntToStr(ATimeout)
);
  fmt.DecimalSeparator := '.';
  Result := '';
//  Log('LoadAStar64()');
  LoadAStar64();
//  Log('Loaded');
  try
    FillChar(ar, SizeOf(ar), 0);
    with ar do
    begin
      Version := 1;
      FromLatitude := AVector.PointFrom.Latitude;
      FromLongitude := AVector.PointFrom.Longitude;
      ToLatitude := AVector.PointTo.Latitude;
      ToLongitude := AVector.PointTo.Longitude;
      ZonesLimit := AZonesExcluded; // исключаемые зоны
      FormatVariant := Ord(gfLandMark);
      LenTreshold := 15;
      Timeout := ATimeout;
      BufferSize := 1024*1024;
      HashString := AllocMem(BufferSize);
      Feature := 0;
    end;
//    Log('AStarCalc4Acc AAccounts='+IntToStr(AAccounts[0])+IntToStr(AAccounts[1]));
    CalcRez := AStarCalc4Acc(@ar, @AAccounts[0]);
//    Log('calc()');
    if (CalcRez = 0) then
    begin
      Result := string(AnsiString(ar.HashString));
      Delete(Result, 3, 12 + 19);
      Delete(Result, Length(Result) + (1 - 12 - 19), 12 + 19);
      RZonesVisited := GetZones(Result, ar.FormatStartPos, ar.FormatIncrement);
      RDistance := ar.Distance;
//      Result := FloatToStrF(ar.Distance, ffFixed, 9, 3, fmt) + Result;
//!!      Log('Calculating done with zones crossed ' + IntToHex(RZonesVisited, 16));
      Log('Calculating done with zones crossed ' + IntToHex(AZonesExcluded, 16));
    end
    else
    begin
      Log('Calculating failed with rezult ' + IntToStr(CalcRez));
    end;
  finally
    FreeMem(ar.HashString);
    UnloadAStar64();
  end;
end;

{ TTaskThread }

constructor TTaskThread.Create(
  const ALmKey: TLandMarkWayKey;
  const ASaveFunc: TSaveFunc;
  const AAccounts: TIntegerDynArray;
  const ATimeout: Integer;
  const ARecalcAll: Boolean;
  const AResDir: string
);
begin
  inherited Create(False);
  FreeOnTerminate := False;
  //
  FLmKey := ALmKey;
  FSaveFunc := ASaveFunc;
  FAccounts := AAccounts;
  FTimeout := ATimeout;
  FRecalcAll := ARecalcAll;
  FResDir := AResDir;
end;

procedure TTaskThread.DoWork();

  function IsBitsNotPresentedInList(
    ABits: UInt64;
    const AList: TDictionary<UInt64, Boolean>
  ): Boolean;
  var
    Iter: UInt64;
  begin
    for Iter in AList.Keys do
    begin
      ABits := ABits and not Iter;
    end;
    Result := ABits <> 0;
  end;

  procedure InsertNewBitsPackInList(
    const ABits: UInt64;
    const AList: TDictionary<UInt64, Boolean>
  );
  var
    Mask: UInt64;
    OneBit: UInt64;
    Iter: UInt64;
    InsSet: UInt64;
    AllInserted: Boolean;
  begin
Log('InsertNewBits begin  AList.Count ' + IntToStr(AList.Count) + ' adding ' + IntToHex(ABits, 16));
    Mask := 1;
    repeat
      OneBit := ABits and Mask;
      if (OneBit <> 0) then
      begin
        if not AList.ContainsKey(OneBit) then
        begin
Log('InsertNewBits adding ' + IntToHex(OneBit, 16));
          AList.Add(OneBit, False);
        end;
        repeat
          AllInserted := True;
          for Iter in AList.Keys do
          begin
            InsSet := OneBit or Iter;
            if not AList.ContainsKey(InsSet) then
            begin
Log('InsertNewBits adding ' + IntToHex(InsSet, 16));
              AList.Add(InsSet, False);
              AllInserted := False;
              Break;
            end;
          end;
        until AllInserted;
      end;
      Mask := Mask shl 1;
    until (Mask = 0);
Log('InsertNewBits end    AList.Count ' + IntToStr(AList.Count));
  end;

var
  RezStr: string;
  LmMatrix: TLandMarkMatrix;
  CrossedZonesList: TDictionary<UInt64, Boolean>;
  WayKey: TLandMarkWayKey;
  ZonesToExclude: UInt64;
  Dist: Double;
  AllZonesDone: Boolean;
begin
  //если пересчитываем все
  if FRecalcAll then
  begin
  // FLmKey.z всегда 0 (как результат работы InsertLightsFromGeoPosArray)
    try
  //    Log('DoWork. Timeout='+IntToStr(FTimeout));
      CrossedZonesList := nil;
      LmMatrix := TLandMarkMatrix.Create('', '', FAccounts, FResDir);
      try
        CrossedZonesList := TDictionary<UInt64, Boolean>.Create(16);
        RezStr := CalcPath(FLmKey.v, 0{без ограничений по зонам}, WayKey.z, Dist, FAccounts, FTimeout);
  //      ToLog('Vector '+FLmKey.v.HashStr + ' Out WayKey.z='+IntToStr(WayKey.z));
        if (RezStr = '') then
        begin // не удалось посчитать путь
          WriteErrorPath(LmMatrix, FLmKey, 'fullcalc');
          Exit;
        end;
        if (WayKey.z = 0) then
        begin // тривиально = вообще нет зон, по которым мы прошли
          WritePath(LmMatrix, FLmKey, RezStr, Dist);
          Exit;
        end;
        // ...
        WayKey.v := FLmKey.v;
        WritePath(LmMatrix, WayKey, RezStr, Dist);
//!!        WritePath(LmMatrix, FLmKey, RezStr, Dist);
        CrossedZonesList.Clear();
        InsertNewBitsPackInList(WayKey.z, CrossedZonesList);
//        ToLog('InsertNewBitsPackInList WayKey.z='+IntToStr(WayKey.z)+' CrossedZonesList='+IntToStr(CrossedZonesList.Count));
        repeat
          AllZonesDone := True;
          for ZonesToExclude in CrossedZonesList.Keys do
          begin
            if CrossedZonesList[ZonesToExclude] then
              Continue;
            AllZonesDone := False;
            CrossedZonesList[ZonesToExclude] := True;
//!!            FLmKey.z := ZonesToExclude;
            RezStr := CalcPath(FLmKey.v, ZonesToExclude, WayKey.z, Dist, FAccounts, FTimeout);
            if (RezStr <> '') then
            begin
              WritePath(LmMatrix, WayKey, RezStr, Dist);
//!!              WritePath(LmMatrix, FLmKey, RezStr, Dist);
              if IsBitsNotPresentedInList(WayKey.z, CrossedZonesList) then
                InsertNewBitsPackInList(WayKey.z, CrossedZonesList);
            end
            else
            begin
              WayKey.z := 0;
              WriteErrorPath(LmMatrix, WayKey);
//!!              WriteErrorPath(LmMatrix, FLmKey);
            end;
            //
            Break;
          end;
        until AllZonesDone;
      finally
        FSaveFunc(LmMatrix);
        LmMatrix.Free();
        CrossedZonesList.Free();
      end;
    except
      on E: Exception do
      begin
        Log(E.ClassName + ': ' + E.Message);
      end;
    end;
  end else
  //если только нужные
  begin
    try
      Log('DoWork. Считаем только непросчитанные '+FLmKey.v.HashStr+','+ IntToStr(FLmKey.z));
      CrossedZonesList := nil;
      LmMatrix := TLandMarkMatrix.Create('', '', FAccounts, FResDir);
      try
        CrossedZonesList := TDictionary<UInt64, Boolean>.Create(16);
        RezStr := CalcPath(FLmKey.v, FLmKey.z, WayKey.z, Dist, FAccounts, FTimeout);
        if (RezStr = '') then
        begin // не удалось посчитать путь
          WriteErrorPath(LmMatrix, FLmKey);
          Exit;
        end else
        begin // тривиально = вообще нет зон, по которым мы прошли
          WritePath(LmMatrix, FLmKey, RezStr, Dist);
          Exit;
        end;
      finally
        FSaveFunc(LmMatrix);
        LmMatrix.Free();
        CrossedZonesList.Free();
      end;
    except
      on E: Exception do
      begin
        Log(E.ClassName + ': ' + E.Message);
      end;
    end;
  end;
end;

procedure TTaskThread.Execute();
begin
  DoWork();
  Terminate();
end;

{ TTaskList }

destructor TTaskList.Destroy();
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
  begin
    Items[I].Free();
  end;
  //
  inherited Destroy();
end;

{ TTaskQueue }

constructor TTaskQueue.Create(
  const ALmMatrix: TLandMarkMatrix;
  const AThreads: Integer;
  const ATimeout: Integer;
  const ARecalcAll: Boolean
);
begin
  inherited Create();
  //
  FLmMatrix := ALmMatrix;
  FThreads := AThreads;
  FTimeout := ATimeout;
  FRecalcAll := ARecalcAll;
//  FAccounts := AAccounts;
  FCS := TCriticalSection.Create();
  FTaskList := TTaskList.Create();
end;

destructor TTaskQueue.Destroy();
begin
  FTaskList.Free();
  FCS.Free();
  //
  inherited Destroy();
end;

function TTaskQueue.CollectGarbage(): Integer;
var
  I: Integer;
begin
  I := FTaskList.Count - 1;
  while (I >= 0) do
  begin
    if FTaskList.Items[I].Terminated then
    begin
      FTaskList.Items[I].Free();
      FTaskList.Delete(I);
    end;
    Dec(I);
  end;
  Result := FTaskList.Count;
end;

procedure TTaskQueue.SaveResults(
  const ALmMatrixFrom: TLandMarkMatrix
);
var
  MatrixKeyIter: TLandMarkWayKey;
begin
  FCS.Acquire();
  try
    for MatrixKeyIter in ALmMatrixFrom.Keys do
    begin
      WritePath(FLmMatrix, MatrixKeyIter, ALmMatrixFrom[MatrixKeyIter].GeoHash, ALmMatrixFrom[MatrixKeyIter].Distance);
    end;
  finally
    FCS.Release();
  end;
end;

procedure TTaskQueue.PushTask(
  const ALmKey: TLandMarkWayKey;
  const AFullCalc: Boolean;
  const AAccounts: TIntegerDynArray
);
begin
  while (CollectGarbage() >= FThreads) do
    Sleep(1);
  FTaskList.Add(TTaskThread.Create(ALmKey, SaveResults, AAccounts, FTimeout, AFullCalc));
end;

procedure TTaskQueue.WaitForAllDone();
begin
  while (CollectGarbage() > 0) do
    Sleep(1);
end;

initialization
  GCS := TCriticalSection.Create;

finalization
  GCS.Free;

end.

