unit uLandMark;

interface

uses
  JsonDataObjects, Ils.Json.Names, Ils.Json.Utils, AStar64.FileStructs, Geo.Hash, Geo.Pos, AStar64.Typ,
  SysUtils, Classes, UFiles, {Windows, }Ils.Logger, System.IniFiles,
  System.Generics.Collections, System.Math, AStar64.Files, AStar64.Areas,
  AStar64.Extra, uGeneral, Geo.Calcs, LandmarkPathCalculator, AStar64.LandMark,
  System.Types;

const
//  CAreaFile = 'Areas.txt';
  CHashChars: string = '0123456789bcdefghjkmnpqrstuvwxyz';
  PathDelim = '\';

type

{  TLandMarkWayKey = packed record
    v: THashVector; //16
    z: UInt64;      //24
  end;

  TLandMarkIdxFileRecord = packed record
    k: TLandMarkWayKey; //24
    i: Int64;           //32
  end;

  TWayLen = packed record
    Way: string;
    Len: Double;
  end;

  TLandMarkIdxLenFileRecord = packed record
    k: TLandMarkWayKey;  //24
    i: Int64;            //32
    l: Double;           //40
    RESERVED: Int64;     //48
  end;
}
{  TAreaStateType = (stNoCalc, stPartialCalc, stInCalc, stFullCalc);

  TWayLen = packed record
    Way: string;
    Len: Double;
  end;

  TZoneTrackDictionary = TDictionary<UInt64, TWayLen>;

  TLandMarkDictionary = TDictionary<THashVector, TZoneTrackDictionary>;

  TLandMarkAreaInfo = record
//    ID: Integer;
    AccountID: Integer;
    State: TAreaStateType; //состояние рассчитанности маршрутов
    //0 не рассчитано, 1 частично рассчитано, 2 в процессе расчета, 3 рассчитано
//    Color: Integer;
    GeoHashID: string;
    Name: string;
    PolygonHash: string;
    LandMarksHash: string;
    Polygon: TGeoPosArray;
    LandMarks: TGeoPosArray;
    LandMarkDict: TLandMarkDictionary;
  end;

  TLandMarkAreaDictionary  = TDictionary<Integer, TLandMarkAreaInfo>;}

  TLandMarkArea = class
  private
    FDetailLog: Boolean;
    FRequestNo: Cardinal;
  public
    constructor Create(const ARequestNo: Cardinal; const ADetailLog: Boolean = False); overload;
    destructor Destroy(); override;
    class procedure Stop;
    function LoadOneAreaFile(const ARootPath: string; const AAccount: integer; const ADetailLog: Boolean = false): TLandMarkAreaDictionary;
    function LoadAreaFile(const ARootPath: string;
                          const AAccounts: TIntegerDynArray;
                          const ADetailLog: Boolean = false): TLandMarkAreaDictionary;
    function LoadAreaFileMini(const ARootPath: string;
                          const AAccounts: TIntegerDynArray;
                          const ADetailLog: Boolean = false): TLandMarkAreaDictionary;
    function SaveAreas(const ARootPath: string;
                       const AAccount: integer;
                       const AAreas: TLandMarkAreaDictionary;
                       const ASaveTracks: Boolean;
                       const ADetailLog: Boolean = false): Boolean;
    function SaveLandMarks(const ARootPath: string; const AAccount: integer; const AGeoHashID: string; ALandMarkDict: TLandMarkDictionary): Boolean;

    function GetAreas(ALandMarkAreaDict: TLandMarkAreaDictionary): TJsonObject;
    function GetAreasStateOk(ALandMarkAreaDict: TLandMarkAreaDictionary): TJsonObject;
    function GetLandMarks(const APolygon: string; const ADistKm: double = 0): TGeoPosArray;
    function GetLandMarksJson(const ALandMarks: string): TJsonObject;
{    function GetPointsForCalc(AAreaInfo: TLandMarkAreaInfo;
                              const AAreaID: Integer;
                              const ADetailLog: Boolean = false): TLandMarkMatrix;}
    function GetAreaTracks(const ARootPath: string;
                           const AAccount: Integer;
                           const AGeoHashID: string;
                           const ADetailLog: Boolean = false): TLandMarkDictionary;
    function CalcTrackFiles(const ARootPath: string;
                            const AAccounts: TIntegerDynArray;
                            const AAreaID: Integer;
                            const AThreads: Integer;
                            const AFullCalc: Boolean = False;
                            const ADetailLog: Boolean = false): Boolean;
    function DelTrackFiles(const ARootPath: string;
                            const AAccount: Integer;
                            const AGeoHashID: string;
                            const ADetailLog: Boolean = false): Boolean;
    function CompareAreas(const ARootPath: string;
                          const AAccounts: TIntegerDynArray;
                          AAreaInfoOld, AAreaInfoNew: TLandMarkAreaInfo): TLandMarkAreaInfo;
    function LM2GW(AWay: string): string;
    function GetAreaIDFromGeoHashID(const ARootPath: string;
                                    const AAccounts: TIntegerDynArray; AGeoHashID: string): Integer;
    function CheckRouteInTrack(const ARootPath: string;
                               const AAccounts: TIntegerDynArray;
                               const ARoutePoints: TGeoPosArray;
                               var AAreas: TLandMarkAreaDictionary;
                               const ADetailLog: Boolean = false): Boolean;
    function CheckRouteInRoute(const ARoute1, ARoute2: TGeoPosArray): Boolean;
{    function LoadOneZoneFile(AAccount: integer; ADetailLog: Boolean = false): TZoneDictionary;
    function LoadOneZoneTypeFile(AAccount: integer; ADetailLog: Boolean = false): TZoneTypeDictionary;

    function LoadZoneFile(const ARootPath: string;
                                const AAccounts: TIntegerDynArray; ADetailLog: Boolean = false): TJsonObject;
    function LoadZoneTypeFile(AAccounts: TIntegerDynArray; ADetailLog: Boolean = false): TJsonObject;
    function LoadProgress(AAccount: integer; ADetailLog: Boolean = false): Integer;
    procedure SaveProgress(AAccount: integer);
    function GetCorrectBorder(ABorder: TGeoPosArray): TGeoPosArray;

    function SaveZones(AAccount: integer; AZones: TZoneDictionary; ADetailLog: Boolean = false): Boolean;
    function SaveZoneTypes(AAccount: integer; AZoneTypes: TZoneTypeDictionary; ADetailLog: Boolean = false): Boolean;

    function GetFilesNameArray(ATopLeft, ABottomRight: TGeoPos; ADetailLog: Boolean = false): TFileNameArray;
    function GetFilesNameArrayHash(TopLeftHash, BottomRightHash : int64; ADetailLog: Boolean = false): TFileNameArray;
    function CheckFilesExists(const ARootPath: string;
                                    const AAccount: Integer;
                                    const AHash: string; ADetailLog: Boolean = false
                                    ): boolean;
    function CheckFilesExists1(const ARootPath: string;
                                    const AAccount: Integer;
                                    const AHash: string; ADetailLog: Boolean = false
                                    ): boolean;
    function CheckFilesExists2(const ARootPath: string;
                                    const AAccount: Integer;
                                    const AHash: string
                                    ): boolean;
    function GetCoverZones(const ABounds: string;
                                 const AAccounts: TIntegerDynArray;
                                 const ARootPath: string;
                                 const ADepth: integer = -1; ADetailLog: Boolean = false): TJsonObject;

    function PatchZoneHash(AHash: string): string;
    function CheckArea(const AArea: TGeoPosArray;const APoint: TGeoPos; ADetailLog: Boolean = false): Boolean;
    function CheckRouteInThroughArea(const AZone: TGeoPosArray; const ARoutePoints: TGeoPosArray; ADetailLog: Boolean = false): Boolean;
    function Intersection(const APoint1, APoint2, BPoint1, BPoint2: TGeoPos):boolean;
    function ZCPlaceIsEmpty(AZCPlace: TArray<UInt64>): Boolean;
    function ScanForAll(AAccounts: TIntegerDynArray): Boolean; //старый код, не используется
    procedure ParseZones();
    function SaveOneZC(AEdgeFileName, AZCFileName: string): Boolean;
    function SaveTwoZC(const ARootPath: string;
                             const AAccounts: TIntegerDynArray;
                             AFileName: string): Boolean;
    function SaveAllZC(const ARootPath: string;
                             const AAccounts: TIntegerDynArray; ADetailLog: Boolean = false): Boolean;
    function SaveFileListZC(const ARootPath: string;
                            const ATopLeft, ABottomRight: TGeoPos;
                            const AAccounts: TIntegerDynArray; ADetailLog: Boolean = false): Boolean;
    function DeleteAllZC(const ARootPath: string;
                               const AAccounts: TIntegerDynArray; ADetailLog: Boolean = false): Boolean;
    function FillFileNameArray: integer;
    function CreateZCAll(const ARootPath: string;
                               const AAccounts: TIntegerDynArray; ADetailLog: Boolean = false): Boolean;
    function CopyFilesForZone(const ARootPath: string;
                               const AAccounts: TIntegerDynArray;
                               const AZoneName: string; ADetailLog: Boolean = false): Boolean;}
  end;

implementation

{ TLandMarkArea }

function TLandMarkArea.CalcTrackFiles(const ARootPath: string;
                                      const AAccounts: TIntegerDynArray;
                                      const AAreaID: Integer;
                                      const AThreads: Integer;
                                      const AFullCalc: Boolean;
                                      const ADetailLog: Boolean): Boolean;
var
  AreaInfo: TLandMarkAreaInfo;
  LandMarkMatrix: TLandMarkMatrix;
  Areas: TLandMarkAreaDictionary;
//  K: TLandMarkWayKey;
//  LMWay: TLandMarkWay;
//  WayLen: TWayLen;
//  ZoneTrackDict: TZoneTrackDictionary;
  iForRecalc: Integer;
  FileName: string;
begin
  Result := True;
  Areas := LoadAreaFile(ARootPath, AAccounts, ADetailLog);
  if Areas.TryGetValue(AAreaID, AreaInfo) then
  begin
    AreaInfo.State := stInCalc;
    Areas.AddOrSetValue(AAreaID, AreaInfo);
    SaveAreas(ARootPath, AAccounts[0], Areas, False);
    if Length(AreaInfo.GeoHashID) = 26 then
      FileName := Copy(AreaInfo.GeoHashID,3,12)+'_'+Copy(AreaInfo.GeoHashID,15,12);
    ToLog('Request # ' + IntToStr(FRequestNo) + ' TLandMarkArea.CalcTrackFiles. Set to stInCalc. ID='+IntToStr(AAreaID));
//    LandMarkMatrix := GetPointsForCalc(AreaInfo, AAreaID, ADetailLog);
//    ToLog('Request # ' + IntToStr(FRequestNo) + ' TLandMarkArea.CalcTrackFiles. Need calc vectors = '+IntToStr(LandMarkMatrix.Count));
//    TLPC.GetCalcMulti(ARootPath, AAccounts, nil, AThreads, TGeneral.FLandMarkCalcTimeout, LandMarkMatrix);
    TLPC.GenMulti(ARootPath, FileName,
                    AreaInfo.LandMarks, AAccounts, nil, AThreads,
                    TGeneral.FLandMarkCalcTimeout, AFullCalc);
    LandMarkMatrix := TLandMarkMatrix.Create(ARootPath+IntToStr(AAccounts[0])+'\', FileName, AAccounts, IntToStr(AAccounts[0]));
    LandMarkMatrix.LoadIndex;
{    for K in LandMarkMatrix.Keys do
    begin
      if LandMarkMatrix.TryGetValue(K, LMWay) then
      begin
        if not AreaInfo.LandMarkDict.TryGetValue(K.v, ZoneTrackDict) then
          ZoneTrackDict := TDictionary<UInt64, TWayLen>.Create;

        WayLen.Len := LMWay.Distance;
        WayLen.Way := LMWay.GeoHash;
        ZoneTrackDict.AddOrSetValue(K.z, WayLen);
        AreaInfo.LandMarkDict.AddOrSetValue(K.v, ZoneTrackDict);
      end;
    end;
    SaveLandMarks(ARootPath, AAccounts[0], AreaInfo.GeoHashID, AreaInfo.LandMarkDict);
    LandMarkMatrix := GetPointsForCalc(AreaInfo, AAreaID, ADetailLog);}
    iForRecalc := Length(LandMarkMatrix.GetKeysForRecalc);
    ToLog('Request # ' + IntToStr(FRequestNo) + ' TLandMarkArea.CalcTrackFiles. After calc. Need calc vectors = '+IntToStr(iForRecalc));
    if iForRecalc = 0 then
      AreaInfo.State := stFullCalc
    else
      AreaInfo.State := stPartialCalc;
    Areas.AddOrSetValue(AAreaID, AreaInfo);
    SaveAreas(ARootPath, AAccounts[0], Areas, False);
  end;
end;

function TLandMarkArea.CheckRouteInRoute(const ARoute1,
  ARoute2: TGeoPosArray): Boolean;
var
  i, j: Integer;
begin
  Result := False;
  for I := Low(ARoute1) to High(ARoute1) do
    for J := Low(ARoute2) to High(ARoute2) do
      if ARoute1[i] = ARoute2[j] then
        Exit(True);
end;

function TLandMarkArea.CheckRouteInTrack(const ARootPath: string;
  const AAccounts: TIntegerDynArray;
  const ARoutePoints: TGeoPosArray;
  var AAreas: TLandMarkAreaDictionary;
  const ADetailLog: Boolean = false): Boolean;
var
  HashVector: THashVector;
  AreaID: Integer;
  AreaInfo: TLandMarkAreaInfo;
  ZoneTrack: TZoneTrackDictionary;
  WayLen: TWayLen;
  Zone: UInt64;
  TrackPoints: TGeoPosArray;
  DeleteVectors: TDictionary<THashVector,Integer>;
  NeedSave: Boolean;
begin
  DeleteVectors := TDictionary<THashVector,Integer>.Create;
  NeedSave := False;
  try
    for AreaID in AAreas.Keys do
    begin
      AAreas.TryGetValue(AreaID, AreaInfo);
      DeleteVectors.Clear;
      for HashVector in AreaInfo.LandMarkDict.Keys do
      begin
        AreaInfo.LandMarkDict.TryGetValue(HashVector, ZoneTrack);
        for Zone in ZoneTrack.Keys do
        begin
          ZoneTrack.TryGetValue(Zone, WayLen);
          TGeoHash.DecodeArrayWorld(WayLen.Way, TrackPoints);
          //выясняем какие треки пересекаются с дорогой и удаляем их вектора
          if CheckRouteInRoute(ARoutePoints, TrackPoints) then
            DeleteVectors.AddOrSetValue(HashVector, 0);
        end;
      end;
      //меняем статус региона если что-то удаляем
      if DeleteVectors.Count > 0 then
      begin
        NeedSave := True;

        if AreaInfo.State = stFullCalc then
          AreaInfo.State := stPartialCalc;
      //удаляем вектора
         for HashVector in DeleteVectors.Keys do
           AreaInfo.LandMarkDict.Remove(HashVector);

         AAreas.AddOrSetValue(AreaID, AreaInfo);
      end;
    end;
    Result := NeedSave;
  finally
    DeleteVectors.Free;
  end;

end;

function TLandMarkArea.CompareAreas(const ARootPath: string;
                          const AAccounts: TIntegerDynArray;
                          AAreaInfoOld, AAreaInfoNew: TLandMarkAreaInfo): TLandMarkAreaInfo;
var
  i, j: Integer;
//  HashVector: THashVector;
  LMDictOld, LMDictNew,
  AddedPoints, DeletedPoints: TDictionary<TGeoPos,Integer>;
  DeleteHashVector, AddHashVector: TDictionary<THashVector,Integer>;
  HashVector: THashVector;
  Point: TGeoPos;
  FilenameOld, FilenameNew: string;
  LMMatrixOld, LMMatrixNew : TLandMarkMatrix;
  LMKey: TLandMarkWayKey;
  LMWay: TLandMarkWay;
begin
  //сверяем маяки
  Result := AAreaInfoNew;
  LMDictOld := TDictionary<TGeoPos,Integer>.Create;
  LMDictNew := TDictionary<TGeoPos,Integer>.Create;
  AddedPoints := TDictionary<TGeoPos,Integer>.Create;
  DeletedPoints := TDictionary<TGeoPos,Integer>.Create;
  DeleteHashVector := TDictionary<THashVector,Integer>.Create;
  AddHashVector := TDictionary<THashVector,Integer>.Create;
  try
    TGeoHash.DecodeArrayWorld(AAreaInfoOld.LandMarksHash, AAreaInfoOld.LandMarks);
    TGeoHash.DecodeArrayWorld(AAreaInfoNew.LandMarksHash, AAreaInfoNew.LandMarks);
    for I := Low(AAreaInfoOld.LandMarks) to High(AAreaInfoOld.LandMarks) do
      LMDictOld.AddOrSetValue(AAreaInfoOld.LandMarks[i], 0);
    for I := Low(AAreaInfoNew.LandMarks) to High(AAreaInfoNew.LandMarks) do
      LMDictNew.AddOrSetValue(AAreaInfoNew.LandMarks[i], 0);
    for Point in LMDictOld.Keys do
    begin
      if not LMDictNew.TryGetValue(Point, j) then
        DeletedPoints.AddOrSetValue(Point, 0);
    end;
    for Point in LMDictNew.Keys do
    begin
      if not LMDictOld.TryGetValue(Point, j) then
        AddedPoints.AddOrSetValue(Point, 0);
    end;
    FilenameOld := Copy(AAreaInfoOld.GeoHashID,3,12)+'_'+Copy(AAreaInfoOld.GeoHashID,15,12);
    LMMatrixOld := TLandMarkMatrix.Create(ARootPath,FilenameOld,AAccounts, IntToStr(AAccounts[0]));
    LMMatrixOld.LoadIndex;
    FilenameNew := Copy(AAreaInfoNew.GeoHashID,3,12)+'_'+Copy(AAreaInfoNew.GeoHashID,15,12);
    if FilenameNew <> FilenameOld then
      LMMatrixNew := TLandMarkMatrix.Create(ARootPath,FilenameNew,AAccounts, IntToStr(AAccounts[0]))
    else
      LMMatrixNew := LMMatrixOld;

    Result := AAreaInfoNew;
    Result.LandMarkDict := AAreaInfoOld.LandMarkDict;
    Result.State := AAreaInfoOld.State;
    if DeletedPoints.Count <> 0 then
    begin
      for Point in DeletedPoints.Keys do
      begin
        for HashVector in Result.LandMarkDict.Keys do
        begin
          if HashVector.HashFrom = Point.ToHash then
            DeleteHashVector.AddOrSetValue(HashVector, 0);
          if HashVector.HashTo = Point.ToHash then
            DeleteHashVector.AddOrSetValue(HashVector, 0);
        end;

      //удалились маяки, удаляем их треки
        for LMKey in LMMatrixOld.Keys do
        begin
          if LMKey.v.HashFrom = Point.ToHash then
            LMMatrixNew.Remove(LMKey);
          if LMKey.v.HashTo = Point.ToHash then
            LMMatrixNew.Remove(LMKey);
        end;
      end;
      //удаляем старые файлы с треками
      if AAreaInfoOld.GeoHashID <> AAreaInfoNew.GeoHashID then
        DelTrackFiles(ARootPath, AAccounts[0], AAreaInfoOld.GeoHashID, FDetailLog);

      for HashVector in DeleteHashVector.Keys do
      begin
        Result.LandMarkDict.Remove(HashVector);
      end;
{      Result.LandMarkDict := AAreaInfoOld.LandMarkDict;
      for HashVector in Result.LandMarkDict.Keys do
      begin
        if DeletedPoints.TryGetValue(HashVector.PointFrom, j) then
          DeleteHashVector.AddOrSetValue(HashVector, 0);
        if DeletedPoints.TryGetValue(HashVector.PointTo, j) then
          DeleteHashVector.AddOrSetValue(HashVector, 0);
      end;
      for HashVector in DeleteHashVector.Keys do
      begin
        Result.LandMarkDict.Remove(HashVector);
      end;
      //удаляем старые файлы с треками
      DelTrackFiles(ARootPath, AAccounts[0], AAreaInfoOld.GeoHashID, FDetailLog);}
    end;
    if AddedPoints.Count <> 0 then
    begin
      //Добавились маяки, понижаем статус
      if Result.State = stFullCalc then
        Result.State := stPartialCalc;
      for Point in AddedPoints.Keys do
      begin
        for I := Low(Result.LandMarks) to High(Result.LandMarks) do
        begin
          if Point.ToHash <> Result.LandMarks[i].ToHash then
          begin
            LMKey.z := 0;
            LMWay := TLandMarkWay.Create('fullcalc',0);
            LMKey.v.HashFrom := Point.ToHash;
            LMKey.v.HashTo := Result.LandMarks[i].ToHash;
            LMMatrixNew.AddOrSetValue(LMKey, LMWay);
            LMKey.z := 0;
            LMWay := TLandMarkWay.Create('fullcalc',0);
            LMKey.v.HashTo := Point.ToHash;
            LMKey.v.HashFrom := Result.LandMarks[i].ToHash;
            LMMatrixNew.AddOrSetValue(LMKey, LMWay);
            HashVector.HashFrom := Point.ToHash;
            HashVector.HashTo := Result.LandMarks[i].ToHash;
            AddHashVector.AddOrSetValue(HashVector, 0);
            HashVector.HashTo := Point.ToHash;
            HashVector.HashFrom := Result.LandMarks[i].ToHash;
            AddHashVector.AddOrSetValue(HashVector, 0);
          end;
        end;
      end;
      for HashVector in AddHashVector.Keys do
        Result.LandMarkDict.AddOrSetValue(HashVector, TDictionary<UInt64, TWayLen>.Create);
    end;
    LMMatrixNew.Save;

  finally
    LMDictOld.Free;
    LMDictNew.Free;
    AddedPoints.Free;
    DeletedPoints.Free;
    DeleteHashVector.Free;
    AddHashVector.Free;
  end;


end;

constructor TLandMarkArea.Create(const ARequestNo: Cardinal; const ADetailLog: Boolean);
begin
  FDetailLog := ADetailLog;
  FRequestNo := ARequestNo;
//  FLandMarkAreaDictionary := TDictionary<Integer, TLandMarkAreaInfo>.Create;
  if FDetailLog then
    ToLog('TLandMarkArea Start');
end;

function TLandMarkArea.DelTrackFiles(const ARootPath: string;
  const AAccount: Integer; const AGeoHashID: string;
  const ADetailLog: Boolean): Boolean;
var
  FileName: string;
  FullPath: string;
begin
  FileName := Copy(AGeoHashID,3,12) + '_' + Copy(AGeoHashID,15,12);
  FullPath := ARootPath + IntToStr(AAccount) + PathDelim;
  if not (FileExists(FullPath + FileName + CDataFileExt) and
          FileExists(FullPath + FileName + CIndexFileExt) and
          FileExists(FullPath + FileName + CRecalcFileExt)) then
    Exit(True);
  Result := DeleteFile(PChar(FullPath + FileName + CDataFileExt)) and
            DeleteFile(PChar(FullPath + FileName + CIndexFileExt)) and
            DeleteFile(PChar(FullPath + FileName + CRecalcFileExt));
end;

destructor TLandMarkArea.Destroy;
begin
//  FLandMarkAreaDictionary.Free;
  if FDetailLog then
    ToLog('TLandMarkArea Stop');
  inherited;
end;

function TLandMarkArea.GetAreaIDFromGeoHashID(const ARootPath: string;
                            const AAccounts: TIntegerDynArray; AGeoHashID: string): Integer;
var
  AreasDict: TLandMarkAreaDictionary;
  AreaInfo: TLandMarkAreaInfo;
  ID: Integer;
begin
  Result := -1;
  AreasDict := LoadAreaFileMini(ARootPath, AAccounts);
  for ID in AreasDict.Keys do
  begin
    if AreasDict.TryGetValue(ID, AreaInfo) then
    begin
      if AreaInfo.GeoHashID = AGeoHashID then
        Exit(ID);
    end;
  end;

end;

function TLandMarkArea.GetAreas(ALandMarkAreaDict: TLandMarkAreaDictionary): TJsonObject;
var
  AreaInfo: TLandMarkAreaInfo;
  HashVector: THashVector;
  ZoneTrackDict: TZoneTrackDictionary;
  Zone: UInt64;
  Way: TWayLen;
  Points: TGeoPosArray;
  ID: Integer;
begin
  Result := TJsonObject.Create;
  for ID in ALandMarkAreaDict.Keys do
  begin
    ALandMarkAreaDict.TryGetValue(ID, AreaInfo);
    with Result.A['Areas'].AddObject do
    begin
      I['ID'] := ID;
      I['AccountID'] := AreaInfo.AccountID;
      I['State'] := Integer(AreaInfo.State);
      S['Name'] := AreaInfo.Name;
      S['GeoHashID'] := AreaInfo.GeoHashID;
      S['Polygon'] := AreaInfo.PolygonHash;
      S['LandMarks'] := AreaInfo.LandMarksHash;
      if Assigned(AreaInfo.LandMarkDict) then
      for HashVector in AreaInfo.LandMarkDict.Keys do
      begin
        AreaInfo.LandMarkDict.TryGetValue(HashVector, ZoneTrackDict);
        for Zone in ZoneTrackDict.Keys do
        begin
          ZoneTrackDict.TryGetValue(Zone, Way);
          SetLength(Points, 2);
          Points[0] := HashVector.PointFrom;
          Points[1] := HashVector.PointTo;
          with A['Tracks'].AddObject do
          begin
            S['Points'] := TGeoHash.EncodeArrayWorld(Points, gfPointsArray);
            S['Zone'] := UIntToStr(Zone);
            S['Way'] := LM2GW(Way.Way);
          end;
        end;
      end;
    end;
  end;

end;

function TLandMarkArea.GetAreasStateOk(
  ALandMarkAreaDict: TLandMarkAreaDictionary): TJsonObject;
var
  AreaInfo: TLandMarkAreaInfo;
  ID: Integer;
begin
  Result := TJsonObject.Create;
  Result.S['Result'] := 'Ok';
  for ID in ALandMarkAreaDict.Keys do
  begin
    ALandMarkAreaDict.TryGetValue(ID, AreaInfo);
    with Result.A['Areas'].AddObject do
    begin
      I['ID'] := ID;
      I['State'] := Integer(AreaInfo.State);
    end;
  end;
end;

function TLandMarkArea.GetAreaTracks(const ARootPath: string;
                                     const AAccount: Integer;
                                     const AGeoHashID: string;
                                     const ADetailLog: Boolean): TLandMarkDictionary;
var
  LandMarkArr: array of TLandMarkIdxLenFileRecord;
  IdxFile, DatFile: TFileStream;
  DatFileReader: TStreamReader;
  IdxFileName, DatFileName, FileName: string;
  i: Integer;
  Way: TWayLen;
  ZoneTrackDict: TZoneTrackDictionary;
begin
  Result := TDictionary<THashVector, TZoneTrackDictionary>.Create;
  begin
    FileName := Copy(AGeoHashID,3,12) + '_' + Copy(AGeoHashID,15,12);
    IdxFileName := ARootPath + IntToStr(AAccount) + PathDelim + FileName + '.idx';
    DatFileName := ARootPath + IntToStr(AAccount) + PathDelim + FileName + '.dat';
    if not (FileExists(IdxFileName) and FileExists(DatFileName)) then
      Exit;
    IdxFile := TFileStream.Create(IdxFileName, fmOpenRead and fmShareDenyNone);
    DatFile := TFileStream.Create(DatFileName, fmOpenRead and fmShareDenyNone);
    DatFileReader := TStreamReader.Create(DatFile, TEncoding.ANSI, True);
    try
      SetLength(LandMarkArr, IdxFile.Size div SizeOf(TLandMarkIdxLenFileRecord));
      IdxFile.ReadBuffer(LandMarkArr[0], IdxFile.Size);
      for i := Low(LandMarkArr) to High(LandMarkArr) do
      begin
//        LandMark := LandMarkArr[i];
        if (LandMarkArr[i].k.v.HashFrom > 0) and (LandMarkArr[i].k.v.HashTo > 0) and (LandMarkArr[i].i < DatFile.Size) then
        begin
          DatFileReader.DiscardBufferedData;
          DatFile.Position := LandMarkArr[i].i;
          Way.Way := Trim(DatFileReader.ReadLine);
          Way.Len := 0;
//          TGeoHash.DecodeArrayWorld(s, Points);
//          s := TGeoHash.EncodeArrayWorld(Points);
          if not Result.TryGetValue(LandMarkArr[i].k.v, ZoneTrackDict) then
            ZoneTrackDict := TDictionary<UInt64, TWayLen>.Create;
          ZoneTrackDict.AddOrSetValue(LandMarkArr[i].k.z, Way);
          Result.AddOrSetValue(LandMarkArr[i].k.v, ZoneTrackDict);
        end;
      end;

    finally
      IdxFile.Free;
      DatFileReader.Free;
      DatFile.Free;
    end;
  end;
end;

function TLandMarkArea.GetLandMarks(const APolygon: string; const ADistKm: double): TGeoPosArray;
var
  Points, PointsCalc: TGeoPosArray;
  i, j, k: Integer;
  Distance, LatitudeInc, LongitudeInc: Double;
  NeedSection: integer;
begin
  SetLength(Result, 0);
  Exit;

//  Result := TJsonObject.Create;
  TGeoHash.DecodeArrayWorld(APolygon, Points);
  if ADistKm = 0 then
  begin
  //возвращаем точки полигона (прямоугольника)
    Result := Points;
//    s := TGeoHash.EncodeArrayWorld(Points, 'PA');
//    Result.S['LandMarks'] := s;
  end else
  begin
  //рассчитываем расстояния между точками не больше ADistKm
    i := 1;
    j := 1;
    SetLength(PointsCalc,1);
    PointsCalc[0] := Points[0];
    while i < Length(Points) do
    begin
      Distance := TGeoCalcs.GeoLengthKmDeg(Points[i-1], Points[i]);
      if Distance > ADistKm then
      begin
        NeedSection :=  trunc(Distance/ADistKm)+1;
        LatitudeInc := (Points[i].Latitude-Points[i-1].Latitude)/NeedSection;
        LongitudeInc := (Points[i].Longitude-Points[i-1].Longitude)/NeedSection;
        for k := 1 to NeedSection-1 do
        begin
          SetLength(PointsCalc, Length(PointsCalc)+1);
          PointsCalc[j].Latitude := Points[i-1].Latitude + LatitudeInc*k;
          PointsCalc[j].Longitude := Points[i-1].Longitude + LongitudeInc*k;
          inc(j);
        end;
        SetLength(PointsCalc, Length(PointsCalc)+1);
        PointsCalc[j] := Points[i];
        inc(j);
      end else
      begin
        SetLength(PointsCalc, Length(PointsCalc)+1);
        PointsCalc[j] := Points[i];
        inc(j);
      end;
      inc(i);
    end;
//если полигон незамкнутый, то обрабатываем между первой и последней
    if Low(Points) <> High(Points) then
    begin
      Distance := TGeoCalcs.GeoLengthKmDeg(Points[0], Points[High(Points)]);
      if Distance > ADistKm then
      begin
        NeedSection :=  trunc(Distance/ADistKm)+1;
        LatitudeInc := (Points[0].Latitude-Points[High(Points)].Latitude)/NeedSection;
        LongitudeInc := (Points[0].Longitude-Points[High(Points)].Longitude)/NeedSection;
        for k := 1 to NeedSection-1 do
        begin
          SetLength(PointsCalc, Length(PointsCalc)+1);
          PointsCalc[High(PointsCalc)].Latitude := Points[High(Points)].Latitude + LatitudeInc*k;
          PointsCalc[High(PointsCalc)].Longitude := Points[High(Points)].Longitude + LongitudeInc*k;
        end;
      end;
    end;
    Result := PointsCalc;
  end;
end;

function TLandMarkArea.GetLandMarksJson(const ALandMarks: string): TJsonObject;
begin
  Result := TJsonObject.Create;
  Result.S['LandMarks'] := ALandMarks;
end;

(*function TLandMarkArea.GetPointsForCalc(AAreaInfo: TLandMarkAreaInfo;
  const AAreaID: Integer;
  const ADetailLog: Boolean): TLandMarkMatrix;
var
  ZoneTrack: TZoneTrackDictionary;
//  AreaInfo: TLandMarkAreaInfo;
  K: TLandMarkWayKey;
  i,j: Integer;
begin
  //загружаем все маяки, сверяем маяки и имеющиеся треки, выводим недостающие вектора
//  Result := TLandMarkMatrix.Create(ARootPath, '');
  Result := TLandMarkMatrix.Create('', '', FAccounts);
  TGeoHash.DecodeArrayWorld(AAreaInfo.LandMarksHash, AAreaInfo.LandMarks);
  K.z := 0;
  for I := Low(AAreaInfo.LandMarks) to High(AAreaInfo.LandMarks) do
  begin
    for J := Low(AAreaInfo.LandMarks) to High(AAreaInfo.LandMarks) do
    begin
      if (I = J) then
        Continue;
      K.v.HashFrom := AAreaInfo.LandMarks[I].ToHash;
      K.v.HashTo := AAreaInfo.LandMarks[J].ToHash;
      if AAreaInfo.LandMarkDict.TryGetValue(K.v, ZoneTrack) then
        Continue;
      if not Result.ContainsKey(K) then
        Result.Add(K, TLandMarkWay.Create('', 0))
    end;
  end;
end;
*)
function TLandMarkArea.LoadAreaFile(const ARootPath: string;
  const AAccounts: TIntegerDynArray; const ADetailLog: Boolean): TLandMarkAreaDictionary;
var
  AreaInfo: TLandMarkAreaInfo;
  ID: Integer;
{  AreaList: TStringList;
  sHashID, sFileName: string;
  I, j: Integer;
  Json: TJsonObject;}
begin
  Result  := LoadAreaFileMini(ARootPath, AAccounts, ADetailLog);
  for ID in Result.Keys do
  begin
    Result.TryGetValue(ID, AreaInfo);
    AreaInfo.LandMarkDict := GetAreaTracks(ARootPath, AreaInfo.AccountID, AreaInfo.GeoHashID, ADetailLog);
    Result.AddOrSetValue(ID, AreaInfo);
  end;
//  Result := False;
{  Json := TJsonObject.Create;
  AreaList := TStringList.Create();
  try
    for j := High(AAccounts) downto 0 do
    begin
      sFileName := ARootPath + IntToStr(AAccounts[j]) + PathDelim + CAreaFile;
      if FileExists(sFileName) then
      begin
        AreaInfo.AccountID := AAccounts[j];
        AreaList.LoadFromFile(sFileName);
        Json := TJsonObject(TJsonObject.Parse(AreaList.Text));
        for i := 0 to json.A['Areas'].Count - 1 do
        begin
          ID := json.A['Areas'].O[i].I['ID'];
          AreaInfo.Name := json.A['Areas'].O[i].S['GeoHashID'];
          AreaInfo.Name := json.A['Areas'].O[i].S['Name'];
          AreaInfo.PolygonHash := json.A['Areas'].O[i].S['Polygon'];
          AreaInfo.LandMarksHash := json.A['Areas'].O[i].S['LandMarks'];
          TGeoHash.DecodeArrayWorld(AreaInfo.PolygonHash, AreaInfo.Polygon);
          TGeoHash.DecodeArrayWorld(AreaInfo.LandMarksHash, AreaInfo.LandMarks);
          AreaInfo.LandMarkDict := GetAreaTracks(ARootPath, AreaInfo.AccountID, sHashID, ADetailLog);
          FLandMarkAreaDictionary.AddOrSetValue(ID, AreaInfo);
        end;
      end;
    end;
    Result := True;
  finally
    AreaList.Free();
    Json.Free;
  end;}
end;

function TLandMarkArea.LoadAreaFileMini(const ARootPath: string;
  const AAccounts: TIntegerDynArray; const ADetailLog: Boolean): TLandMarkAreaDictionary;
var
  AreaList: TStringList;
  sFileName: string;
  I, j, ID: Integer;
  AreaInfo: TLandMarkAreaInfo;
  Json: TJsonObject;
begin
//  Result := False;
  Result := TLandMarkAreaDictionary.Create;//<Integer, TLandMarkAreaInfo>.Create;
  Json := TJsonObject.Create;
  AreaList := TStringList.Create();
  try
    for j := High(AAccounts) downto 0 do
    begin
      sFileName := ARootPath + IntToStr(AAccounts[j]) + PathDelim + CAreaFile;
      if FileExists(sFileName) then
      begin
        AreaInfo.AccountID := AAccounts[j];
        AreaList.LoadFromFile(sFileName);
        Json := TJsonObject(TJsonObject.Parse(AreaList.Text));
        for i := 0 to json.A['Areas'].Count - 1 do
        begin
          ID := json.A['Areas'].O[i].I['ID'];
          AreaInfo.State := TAreaStateType(json.A['Areas'].O[i].I['State']);
          AreaInfo.GeoHashID := json.A['Areas'].O[i].S['GeoHashID'];
          AreaInfo.Name := json.A['Areas'].O[i].S['Name'];
          AreaInfo.PolygonHash := json.A['Areas'].O[i].S['Polygon'];
          AreaInfo.LandMarksHash := json.A['Areas'].O[i].S['LandMarks'];
          TGeoHash.DecodeArrayWorld(AreaInfo.PolygonHash, AreaInfo.Polygon);
          TGeoHash.DecodeArrayWorld(AreaInfo.LandMarksHash, AreaInfo.LandMarks);
          AreaInfo.LandMarkDict := nil;
//          AreaInfo.LandMarkDict := GetAreaTracks(ARootPath, AreaInfo.AccountID, sHashID, ADetailLog);
          Result.AddOrSetValue(ID, AreaInfo);
        end;
      end;
    end;
//    Result := True;
  finally
    AreaList.Free();
    Json.Free;
  end;
end;

function TLandMarkArea.LoadOneAreaFile(const ARootPath: string; const AAccount: integer;
  const ADetailLog: Boolean): TLandMarkAreaDictionary;
var
  AreaList: TStringList;
  sFileName: string;
  i, ID: Integer;
  AreaInfo: TLandMarkAreaInfo;
  Json: TJsonObject;
begin
  Result := TLandMarkAreaDictionary.Create;//<Integer, TLandMarkAreaInfo>.Create;
  AreaList := TStringList.Create();
  Json := TJsonObject.Create;
  try
    sFileName := ARootPath + IntToStr(AAccount) + PathDelim + CAreaFile;
    if FileExists(sFileName) then
    begin
      AreaInfo.AccountID := AAccount;
      AreaList.LoadFromFile(sFileName);
      Json := TJsonObject(TJsonObject.Parse(AreaList.Text));
      for i := 0 to json.A['Areas'].Count - 1 do
      begin
        ID := json.A['Areas'].O[i].I['ID'];
        AreaInfo.GeoHashID := json.A['Areas'].O[i].S['GeoHashID'];
        AreaInfo.Name := json.A['Areas'].O[i].S['Name'];
        AreaInfo.PolygonHash := json.A['Areas'].O[i].S['Polygon'];
        AreaInfo.LandMarksHash := json.A['Areas'].O[i].S['LandMarks'];
        TGeoHash.DecodeArrayWorld(AreaInfo.PolygonHash, AreaInfo.Polygon);
        TGeoHash.DecodeArrayWorld(AreaInfo.LandMarksHash, AreaInfo.LandMarks);
        Result.AddOrSetValue(ID, AreaInfo);
      end;
{    if FileExists(sFileName) then
    begin
      AreaInfo.AccountID := AAccount;
      AreaList.LoadFromFile(sFileName);
      for I := 0 to AreaList.Count - 1 do
      begin
        if TGeneral.FStop then
        begin
          ToLog('Request # '+ IntToStr(FRequestNo) + ' TLandMarkArea.LoadOneAreaFile. Прервано пользователем');
          Exit;
        end;
        TempStr := AreaList[I];
        // ID
        TabPos := Pos(#9, TempStr);
        sHashID := Copy(TempStr, 1, TabPos - 1);
        Delete(TempStr, 1, TabPos);
        // _имя
        TabPos := Pos(#9, TempStr);
        AreaInfo.Name := Copy(TempStr, 1, TabPos - 1);
        Delete(TempStr, 1, TabPos);
        // _Polygon
        TabPos := Pos(#9, TempStr);
        AreaInfo.PolygonHash := Copy(TempStr, 1, TabPos - 1);
        Delete(TempStr, 1, TabPos);
        // LandMark
        AreaInfo.LandMarksHash := TempStr;
        TGeoHash.DecodeArrayWorld(AreaInfo.PolygonHash, AreaInfo.Polygon);
        TGeoHash.DecodeArrayWorld(AreaInfo.LandMarksHash, AreaInfo.LandMarks);
        Result.AddOrSetValue(sHashID, AreaInfo);
      end;}
    end;
  finally
    AreaList.Free();
    Json.Free();
  end;
end;

function TLandMarkArea.SaveAreas(const ARootPath: string; const AAccount: integer;
  const AAreas: TLandMarkAreaDictionary;
  const ASaveTracks: Boolean;
  const ADetailLog: Boolean): Boolean;
var
  List, LmdList: TStringList;
  ID, j: Integer;
  AreaInfo: TLandMarkAreaInfo;
  RootDir, AccDir: string;
  Json: TJsonObject;
begin
  Json := TJsonObject.Create;
  try
    List := TStringList.Create;
    try
      for ID in AAreas.Keys do
      begin
        try
          LmdList := TStringList.Create;
          AAreas.TryGetValue(ID, AreaInfo);
          with Json.A['Areas'].AddObject do
          begin
            I['ID'] := ID;
            S['Name'] := AreaInfo.Name;
            S['GeoHashID'] := AreaInfo.GeoHashID;
            I['State'] := Integer(AreaInfo.State);
            S['Polygon'] := AreaInfo.PolygonHash;
            S['LandMarks'] := AreaInfo.LandMarksHash;
            for j := Low(AreaInfo.LandMarks) to High(AreaInfo.LandMarks) do
              LmdList.Add(FloatToStr(AreaInfo.LandMarks[j].Latitude, TGeneral.Ffs) + #9 + FloatToStr(AreaInfo.LandMarks[j].Longitude, TGeneral.Ffs));

            LmdList.SaveToFile(ARootPath+IntToStr(AAccount)+'\'+AreaInfo.Name+'.lmd');
            if ASaveTracks and Assigned(AreaInfo.LandMarkDict) and (AreaInfo.LandMarkDict.Count > 0) then
            begin
              SaveLandMarks(ARootPath, AAccount, AreaInfo.GeoHashID, AreaInfo.LandMarkDict);
            end;
          end;
        finally
          LmdList.Free;
        end;
      end;
      List.Text := Json.ToString;
      RootDir := IncludeTrailingPathDelimiter(ExtractFileDir(GetModuleName(HInstance)));
      AccDir := IntToStr(AAccount) + PathDelim;
      // каталог
      ForceDirectories(ARootPath + AccDir);

      List.SaveToFile(ARootPath + AccDir + CAreaFile, TEncoding.UTF8);
      Result := True;
    finally
      List.Free;
      Json.Free;
    end;
  except
    Result := False;
  end;
end;

function TLandMarkArea.SaveLandMarks(const ARootPath: string;
  const AAccount: integer; const AGeoHashID: string; ALandMarkDict: TLandMarkDictionary): Boolean;
var
  IdxFile, DatFile: TFileStream;
  DatFileWriter: TStreamWriter;
  IdxFileName, DatFileName, FileName: string;
  Way: TWayLen;
  HashVector: THashVector;
  ZoneTrackDict: TZoneTrackDictionary;
  Zone: UInt64;
  LandMarkIdx: TLandMarkIdxLenFileRecord;
begin
  FileName := Copy(AGeoHashID,3,12) + '_' + Copy(AGeoHashID,15,12);
  IdxFileName := ARootPath + IntToStr(AAccount) + PathDelim + FileName + '.idx';
  DatFileName := ARootPath + IntToStr(AAccount) + PathDelim + FileName + '.dat';
  if FileExists(IdxFileName) then
    DeleteFile(PChar(IdxFileName));
  if FileExists(DatFileName) then
    DeleteFile(PChar(DatFileName));
  try
    IdxFile := TFileStream.Create(IdxFileName, fmCreate + fmOpenReadWrite);
    DatFile := TFileStream.Create(DatFileName, fmCreate + fmOpenReadWrite);
    DatFileWriter := TStreamWriter.Create(DatFile);
    try
      IdxFile.Seek(0, soFromBeginning);
      DatFile.Seek(0, soFromBeginning);
      for HashVector in ALandMarkDict.Keys do
      begin
        ALandMarkDict.TryGetValue(HashVector,ZoneTrackDict);
        for Zone in ZoneTrackDict.Keys do
        begin
          ZoneTrackDict.TryGetValue(Zone, Way);
          if Way.Way <> 'error' then
          begin
            LandMarkIdx.i := DatFile.Position;
            LandMarkIdx.k.v := HashVector;
            LandMarkIdx.k.z := Zone;
            LandMarkIdx.l := Way.Len;//Items[k].Distance;
            DatFileWriter.WriteLine(Way.Way);
            IdxFile.Write(LandMarkIdx, SizeOf(LandMarkIdx));
          end;
        end;
      end;
      Result := True;

    finally
      IdxFile.Free;
      DatFileWriter.Free;
      DatFile.Free;
    end;
  except
    Result := False;
  end;
end;

function TLandMarkArea.LM2GW(AWay: string): string;
var
  Points: TGeoPosArray;
begin
  TGeoHash.DecodeArrayWorld(AWay, Points);
  Result := TGeoHash.EncodeArrayWorld(Points);
end;

class procedure TLandMarkArea.Stop;
begin
  ToLog('TLandMarkArea Manual Stop');
end;

end.
