unit LandmarkPathCalculator;

interface

uses
  SysUtils,
  System.Generics.Collections, System.Generics.Defaults,
  Ils.Utils, Geo.Hash, Geo.Pos,
  AStar64.DynImport, AStar64.Typ, AStar64.FileStructs, AStar64.LandMark;

type
  TLPC = class
  public
    class procedure Gen(
      const AFullPath: string;
      const AFileName: string;
      const ALandmarkCoords: TGeoPosArray
    );
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

class procedure TLPC.Gen(
  const AFullPath: string;
  const AFileName: string;
  const ALandmarkCoords: TGeoPosArray
);

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

  function CalcPath(
    const AVector: THashVector;
    const AZonesExcluded: UInt64;
    out RZonesVisited: UInt64
  ): string;
  var
    ar: TAstarRequest4;
  begin
    Result := '';
    LoadAStar64();
    try
      FillChar(ar, SizeOf(ar), 0);
      with ar do
      begin
//  Version    FromLatitude    FromLongitude    ToLatitude    ToLongitude
//  ZonesLimit    RoadTypeLimit    OsmTypeLimit    Feature    FormatVariant
//  LenTreshold    Timeout    Distance    Stat
//  RoadLengthAggregateBufferSize    RoadLengthAggregate    RoadLengthCount
//  BufferSize    HashString    SignsLimit
        Version := 1;
        FromLatitude := AVector.PointFrom.Latitude;
        FromLongitude := AVector.PointFrom.Longitude;
        ToLatitude := AVector.PointTo.Latitude;
        ToLongitude := AVector.PointTo.Longitude;
        ZonesLimit := AZonesExcluded; // исключаемые зоны
        FormatVariant := Ord(gfLandMark);
        LenTreshold := 15;
        Timeout := 900;
        BufferSize := 1024*1024;
        HashString := AllocMem(BufferSize);
        Feature := 0;
      end;
      if (AStarCalc4(@ar) = 0) then
      begin
        Result := string(AnsiString(ar.HashString));
        Delete(Result, 3, 12 + 19);
        Delete(Result, Length(Result) + (1 - 12 - 19), 12 + 19);
        RZonesVisited := GetZones(Result, ar.FormatStartPos, ar.FormatIncrement);
      end;
    finally
      FreeMem(ar.HashString);
//      FreeMem(ar.RoadLengthAggregate);
      UnloadAStar64();
    end;
  end;

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
    InsSet: UInt64;
    Iter: UInt64;
    AllInserted: Boolean;
  begin
    Mask := 1;
    repeat
      OneBit := ABits and Mask;
      if (OneBit <> 0) then
      begin
        AllInserted := True;
        repeat
          for Iter in AList.Keys do
          begin
            if ((OneBit and Iter) <> 0) then
              Continue;
            AList.Add(OneBit or Iter, False);
            AllInserted := False;
            Break;
          end;
        until AllInserted;
      end;
      Mask := Mask shl 1;
    until (Mask = 0);
  end;

var
  RezStr, RezStr1: string;
//  TempStr: string;
//  TempBegin: string;
//  TempEnd: string;
//  TempNode: string;
//  TempType: string;
//  TempZone: string;
  lmk, lmki: TLandMarkWayKey;
  LandMarkMatrix: TLandMarkMatrix;
  CrossedList: TDictionary<UInt64, Boolean>;
  CrossedIter: UInt64;
  AllWaysDone, AllZonesDone: Boolean;

  procedure WritePath(
    const AKey: TLandMarkWayKey;
    const APath: string
  );
  begin
    if not LandMarkMatrix.ContainsKey(AKey) then
      LandMarkMatrix.Add(AKey, TLandMarkWay.Create(APath))
    else
      LandMarkMatrix.Items[AKey].GeoHash := APath;
  end;

begin
  CrossedList := nil;
  LandMarkMatrix := nil;
  try
    CrossedList := TDictionary<UInt64, Boolean>.Create();
    LandMarkMatrix := TLandMarkMatrix.Create(AFullPath, AFileName);
//    // загрузка предыдущего файла
//    LandMarkMatrix.LoadIndex();
    // !!! генерация маяков (-добавление новых, очистка загруженного пути у старых-) !!!
    InsertLightsFromGeoPosArray(LandMarkMatrix, ALandmarkCoords);
    // расчёт
    repeat // пока есть нерассчитанные пути в LandMarkMatrix
      AllWaysDone := True;
      for lmk in LandMarkMatrix.Keys do
      begin
// lmk.z всегда 0 (как результат работы InsertLightsFromGeoPosArray)
        if (LandMarkMatrix.Items[lmk].GeoHash <> '') then
          Continue;
        AllWaysDone := False;
        RezStr := CalcPath(lmk.v, 0{без ограничений по зонам}, lmki.z);
        if (RezStr = '') then
        begin // не удалось посчитать путь
          // просто удалим из списка расчёта
          LandMarkMatrix.Remove(lmk);
          Break;
        end;
        if (lmki.z = 0) then
        begin // тривиально = вообще нет зон, по которым мы прошли
          // записать этот путь
          WritePath(lmk, RezStr);
          Break;
        end;
        // ...
        lmki.v := lmk.v;
        CrossedList.Clear();
        InsertNewBitsPackInList(lmki.z, CrossedList);
        AllZonesDone := True;
        repeat
          for CrossedIter in CrossedList.Keys do
          begin
            if CrossedList[CrossedIter] then
              Continue;
            AllZonesDone := False;
            CrossedList[CrossedIter] := True;
            RezStr1 := CalcPath(lmk.v, CrossedIter, lmki.z);
            if (RezStr1 <> '') then
            begin
              WritePath(lmki, RezStr1);
              if IsBitsNotPresentedInList(lmki.z, CrossedList) then
                InsertNewBitsPackInList(lmki.z, CrossedList);
            end;
            //
            Break;
          end;
        until AllZonesDone;
        //
        Break;
      end;
    until AllWaysDone;
    LandMarkMatrix.Save();
  finally
    LandMarkMatrix.Free();
    CrossedList.Free();
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
        ARLandmarkMatrix.Add(K, TLandMarkWay.Create(''))
      else
        ARLandmarkMatrix.Items[K].Clear();
    end;
  end;
end;

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
        ARLandmarkMatrix.Add(K, TLandMarkWay.Create(''))
      else
        ARLandmarkMatrix.Items[K].Clear();
    end;
  end;
end;

end.

