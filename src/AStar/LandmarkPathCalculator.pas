unit LandmarkPathCalculator;

interface

uses
  SysUtils,
  System.Generics.Collections, System.Generics.Defaults,
  Ils.Utils, Geo.Hash, Geo.Pos,
  ULPCThread, System.Types,
  AStar64.DynImport, AStar64.Typ, AStar64.FileStructs, AStar64.LandMark;

type
  TLPCProgressFunc = procedure(
    const ACurrent: Integer;
    const ATotal: Integer
  ) of object;

  TLandmarkPathCalculator = class
  private
    FFullPath: string;
    FFileName: string;
    FLandmarkCoords: TGeoPosArray;
    FProgressCB: TLPCProgressFunc;
    FThreads: Integer;
  public
    constructor Create(
      const AFullPath: string;
      const AFileName: string;
      const ALandmarkCoords: TGeoPosArray;
      const AProgressCB: TLPCProgressFunc;
      const AThreads: Integer
    );
    procedure CalculateAndSave();
  end;
//------------------------------------------------------------------------------

  TLocalProgressCBFunc = procedure(
    const ACurrent: Integer;
    const ATotal: Integer
  );

  TLPC = class
  public
    class procedure GenMulti(
      const AFullPath: string;
      const AFileName: string;
      const ALandmarkCoords: TGeoPosArray;
      const AAccounts: TIntegerDynArray;
      const AProgressCB: TLocalProgressCBFunc;
      const AThreads: Integer;
      const ATimeout: Integer;
      const ARecalcAll: Boolean;
      const AResDir: string = ''
    );
{    class procedure GetCalcMulti(
      const ARootPath: string;
      const AAccounts: TIntegerDynArray;
//      const ALandmarkCoords: TGeoPosArray;
      const AProgressCB: TLocalProgressCBFunc;
      const AThreads: Integer;
      const ATimeout: Integer;
      var ARLandmarkMatrix: TLandMarkMatrix
    );}
//    class procedure Gen(
//      const AFullPath: string;
//      const AFileName: string;
//      const ALandmarkCoords: TGeoPosArray
//    );
    class procedure InsertLightsFromGeoPosArray(
      const ARLandmarkMatrix: TLandMarkMatrix;
      const ALandmarkCoords: TGeoPosArray
    );
    class procedure InsertLightsFromGeoHashArray(
      const ARLandmarkMatrix: TLandMarkMatrix;
      const ALandmarkCoords: TArray<Int64>
    );
  end;

implementation

{ TLPC }

class procedure TLPC.GenMulti(
  const AFullPath: string;
  const AFileName: string;
  const ALandmarkCoords: TGeoPosArray;
  const AAccounts: TIntegerDynArray;
  const AProgressCB: TLocalProgressCBFunc;
  const AThreads: Integer;
  const ATimeout: Integer;
  const ARecalcAll: Boolean;
  const AResDir: string
);
var
  LMKeyIter: TLandMarkWayKey;
  LandMarkMatrixInput, LandMarkMatrixOutput: TLandMarkMatrix;
  Tasks: TTaskQueue;
  CntTotal, CntCurrent: Integer;
begin
  Log('GENMT started with ' + AFullPath + AFileName);
  LandMarkMatrixInput := nil;
  LandMarkMatrixOutput := nil;
  Tasks := nil;
  try
    LandMarkMatrixInput := TLandMarkMatrix.Create(AFullPath, AFileName, AAccounts, AResDir);
    LandMarkMatrixOutput := TLandMarkMatrix.Create(AFullPath, AFileName, AAccounts, AResDir);
    Tasks := TTaskQueue.Create(LandMarkMatrixOutput, AThreads, ATimeout, ARecalcAll);
    //
    if ARecalcAll then
    begin
      Log('GenMulti. Full recalc');
      InsertLightsFromGeoPosArray(LandMarkMatrixInput, ALandmarkCoords);
      CntCurrent := 0;
      CntTotal := LandMarkMatrixInput.Count;
      Log('GENMT pathways ' + IntToStr(CntTotal));
      // расчёт
      for LMKeyIter in LandMarkMatrixInput.Keys do
      begin
        if (LandMarkMatrixInput.Items[LMKeyIter].GeoHash <> '') then
          Continue;
        Log('GENMT pass ' + IntToStr(CntCurrent) + ' of ' + IntToStr(CntTotal));
        if Assigned(AProgressCB) then
          AProgressCB(CntCurrent, CntTotal);
        Inc(CntCurrent);
        Tasks.PushTask(LMKeyIter, ARecalcAll, AAccounts);
      end;
    end else
    begin
      Log('GenMulti. Partial recalc');
      LandMarkMatrixOutput.LoadIndex;
      CntCurrent := 0;
      CntTotal := LandMarkMatrixOutput.Count;
      for LMKeyIter in LandMarkMatrixOutput.Keys do
      begin
        if (LandMarkMatrixOutput.Items[LMKeyIter].GeoHash = '') then
        begin
          if Assigned(AProgressCB) then
            AProgressCB(CntCurrent, CntTotal);
          Inc(CntCurrent);
          Tasks.PushTask(LMKeyIter, False, AAccounts);
        end else
        if (LandMarkMatrixOutput.Items[LMKeyIter].GeoHash = 'fullcalc') then
        begin
          if Assigned(AProgressCB) then
            AProgressCB(CntCurrent, CntTotal);
          Inc(CntCurrent);
          Tasks.PushTask(LMKeyIter, true, AAccounts);
        end;
      end;
    end;
    Log('GENMT waiting');
    Tasks.WaitForAllDone();
    Log('GENMT saving');
    LandMarkMatrixOutput.Save();
    Log('GENMT done');
    if Assigned(AProgressCB) then
      AProgressCB(CntCurrent, CntTotal);
  finally
    LandMarkMatrixInput.Free();
    LandMarkMatrixOutput.Free();
    Tasks.Free();
  end;
end;

class procedure TLPC.InsertLightsFromGeoPosArray(
  const ARLandmarkMatrix: TLandMarkMatrix;
  const ALandmarkCoords: TGeoPosArray
);
var
  K: TLandMarkWayKey;
  I, J: Integer;
begin
  K.z := 0;
  for I := Low(ALandmarkCoords) to High(ALandmarkCoords) do
  begin
    for J := Low(ALandmarkCoords) to High(ALandmarkCoords) do
    begin
      if (I = J) then
        Continue;
      K.v.HashFrom := TGeoHash.EncodePointBin(ALandmarkCoords[I].Latitude, ALandmarkCoords[I].Longitude);
      K.v.HashTo := TGeoHash.EncodePointBin(ALandmarkCoords[J].Latitude, ALandmarkCoords[J].Longitude);
      if not ARLandmarkMatrix.ContainsKey(K) then
        ARLandmarkMatrix.Add(K, TLandMarkWay.Create('', 0))
      else
        ARLandmarkMatrix.Items[K].Clear();
    end;
  end;
end;
(*
class procedure TLPC.GetCalcMulti(const ARootPath: string;
      const AAccounts: TIntegerDynArray;
//      const ALandmarkCoords: TGeoPosArray;
      const AProgressCB: TLocalProgressCBFunc;
      const AThreads: Integer;
      const ATimeout: Integer;
      var ARLandmarkMatrix: TLandMarkMatrix
    );
var
  LMKeyIter: TLandMarkWayKey;
//  LandMarkMatrixInput: TLandMarkMatrix;
  Tasks: TTaskQueue;
  CntTotal, CntCurrent: Integer;
begin
  Log('TLPC.GetCalcMulti started');
//  LandMarkMatrixInput := nil;
  Tasks := nil;
  try
//    LandMarkMatrixInput := TLandMarkMatrix.Create(AFullPath, AFileName);
//    LandMarkMatrixOutput := TLandMarkMatrix.Create(AFullPath, AFileName);
//    Result := TLandMarkMatrix.Create(ARootPath, '');
    Tasks := TTaskQueue.Create(ARLandmarkMatrix, AThreads, ATimeout, False);
//    InsertLightsFromGeoPosArray(LandMarkMatrixInput, ALandmarkCoords);
    CntCurrent := 0;
    CntTotal := ARLandmarkMatrix.Count;
    Log('TLPC.GetCalcMulti pathways ' + IntToStr(CntTotal));
    // расчёт
    for LMKeyIter in ARLandmarkMatrix.Keys do
    begin
      if (ARLandmarkMatrix.Items[LMKeyIter].GeoHash <> '') then
        Continue;
      Log('TLPC.GetCalcMulti pass ' + IntToStr(CntCurrent) + ' of ' + IntToStr(CntTotal));
      if Assigned(AProgressCB) then
        AProgressCB(CntCurrent, CntTotal);
      Inc(CntCurrent);
      Tasks.PushTask(LMKeyIter, True, AAccounts);
    end;
    Log('TLPC.GetCalcMulti waiting');
    Tasks.WaitForAllDone();
    Log('TLPC.GetCalcMulti saving');
//    LandMarkMatrixOutput.Save();
    Log('TLPC.GetCalcMulti done');
    if Assigned(AProgressCB) then
      AProgressCB(CntCurrent, CntTotal);
  finally
//    LandMarkMatrixInput.Free();
//    LandMarkMatrixOutput.Free();
    Tasks.Free();
  end;
end;
*)
class procedure TLPC.InsertLightsFromGeoHashArray(
  const ARLandmarkMatrix: TLandMarkMatrix;
  const ALandmarkCoords: TArray<Int64>
);
var
  K: TLandMarkWayKey;
  I, J: Integer;
begin
  K.z := 0;
  for I := Low(ALandmarkCoords) to High(ALandmarkCoords) do
  begin
    for J := Low(ALandmarkCoords) to High(ALandmarkCoords) do
    begin
      if (I = J) then
        Continue;
      K.v.HashFrom := ALandmarkCoords[I];
      K.v.HashTo := ALandmarkCoords[J];
      if not ARLandmarkMatrix.ContainsKey(K) then
        ARLandmarkMatrix.Add(K, TLandMarkWay.Create('', 0))
      else
        ARLandmarkMatrix.Items[K].Clear();
    end;
  end;
end;

{ TLandmarkPathCalculator }

constructor TLandmarkPathCalculator.Create(
  const AFullPath: string;
  const AFileName: string;
  const ALandmarkCoords: TGeoPosArray;
  const AProgressCB: TLPCProgressFunc;
  const AThreads: Integer
);
begin
  inherited Create();
  //
  FFullPath := AFullPath;
  FFileName := AFileName;
  FLandmarkCoords := ALandmarkCoords;
  FProgressCB := AProgressCB;
  FThreads := AThreads;
end;

procedure TLandmarkPathCalculator.CalculateAndSave;
begin
//
end;

end.

