unit UMapEditor;

interface

uses JsonDataObjects, Ils.Json.Names, Ils.Json.Utils, AStar64.FileStructs, System.Classes,
  SysUtils, StrUtils, Types, Math, Windows, Generics.Collections, IdURI,
  Geo.Hash, Geo.Pos, Geo.Calcs, System.AnsiStrings, Geo.Hash.Search,
  AStar64.Typ, AStar64.Areas, Ils.Logger, System.IniFiles,
  Ils.Redis.Conf, Ils.Utils.SaaS, uZC, uSign, uLandMark, AStar64.Files, AStar64.Extra,
  AStar64.Intf, AStar64.Common, Vcl.ExtCtrls, System.DateUtils, Redis.Client,
  {Vcl.Forms, }uGeneral, uSearchEdge, AStar64.LandMark, uSpeed;

const
 C1KmLongitude = 0.01595863;
 C1KmLatitude = 0.008993216;
 CPointToRouteDistance = 5;
 CMaxAzimuthDifference = 30;

type
  TOsmNameDict = TDictionary<Int64,string>;
  TOsmNameDictHash = TDictionary<string,TOsmNameDict>;


  TTrackWay = packed record
    StartID, EndID: TIDLines;
    Way: string;
  end;

  TIDAndPoint = packed record
    ID: TIDLines;
    PointOnLine: TGeoPos;
  end;

  TRouteProperties = packed record
    RoadType: Integer;
    Speed: Integer;
    Zone: Integer;
    OneWay: byte;
    sWay: string;
    Sign: UInt64;
    AvgSpeedForward: TSpeedControl;
    AvgSpeedBackward: TSpeedControl;
  end;

  TRouteInfoMini = packed record
    ID: TIDLines;
    RoadType: Integer;
    Speed: Integer;
    PointsCount: Integer;
    Length: Double;
    PosInEdge: Integer;
  end;

  TAddrIdx = packed record
    OsmID: Int64;
    Idx: Int64;
  end;

  TIntSet = set of byte;
  TRoadTypeArray = Array of Byte;
  THashSquareArray = array[0..8] of Int64;
  THashSquareArrayStr = array[0..8] of string[5];
  TAddrOsmDict = TDictionary<Int64,Int64>;
  TPointDict = TDictionary<TGeoPos,integer>;
  TWayArray = TArray<string>; //array of string;

  TNewCrossPoint = record
    IDStr: string;
    PointGW: string;
  end;

  TNewCrossPointArray = array of TNewCrossPoint;

  TNewRouteFeatures = record
    UserID: Integer;
    RoadType: Integer;
    Speed: Integer;
    OneWay: Boolean;
    name: string;
    way: string;
    sign: string;
    NewCross: TNewCrossPointArray;
  end;

  TUpdateRouteMin = record
    id: TIDLines;
    RoadType: Integer;
    Speed: Integer;
    OneWay: boolean;
    name: string;
    sign: string;
  end;

  TUpdateRouteWay = record
    id: TIDLines;
    RoadType: Integer;
    Speed: Integer;
    OneWay: integer;
    name: string;
    way: string;
    sign: string;
  end;


  TRouteInfoDict = TDictionary<TRouteInfoMini,Integer>;
  THashList = TDictionary<int64, Integer>;
  THashDict = TDictionary<Int64, THashList>;

  TRouteIDArray = array of TIDLines;

  TUpdateMinArray = array of TUpdateRouteMin;

  TUpdateWayArray = array of TUpdateRouteWay;

  TNewRouteArray = array of TNewRouteFeatures;

  TMapEditor = class
  private
    FMinDistToLineM: Integer;
    FRequestNo: Cardinal;
    FRootPath: string;
    FAccounts: TIntegerDynArray;
    FAddressDataFile: TFileStream;
    FIdxFile: TFileStream;
    FAddressStreamReader: TStreamReader;
    FAddressDataFileName: string;
    FIdxFileName: string;
    FStartEdgeFiles, FEndEdgeFiles: THoldRec;
    FHoldRecDict: THoldRecDict;
    FOsmNameHashDict: TDictionary<string,Integer>;
    FDict: TOsmNameDict;
    FRedisCli: TRedisClient;
  public
    FAddrOsmDict : TAddrOsmDict;
    FZC: TZC;
    FLandMarkArea: TLandMarkArea;
    FSign: TSign;
    FDeleteArr: TRouteIDArray;
    FUpdateMinArr: TUpdateMinArray;
    FUpdateWayArr: TUpdateWayArray;
    FNewRouteArr: TNewRouteArray;
    FOsmNameDict: TDictionary<Int64, string>;
    FOsmNameCsvDict: TOsmNameDictHash;
    constructor Create(AMinDistToLineM: Integer; ARequestNo: Cardinal; ARedisCli: TRedisClient; ARootPath: string = ''); overload;
    destructor Destroy(); override;
    procedure Stop;
    procedure SetHoldRecDict(ADict: THoldRecDict);
    function GetHoldRecDict: THoldRecDict;

    function ParseJson(AJsonStr: string): string;

    function LoadAddrOSM : Int64;
    function LoadAddrOSMHash(const AAccounts: TIntegerDynArray; AHashArr: TWayArray) : Int64;
    function LoadAddrOSMCsv(const AAccounts: TIntegerDynArray; AHashArr: TWayArray) : Int64;
    function GetRouteLengthKm(const APoints: TGeoPosArray): Double;
    function GetRouteNameByOSM(const OsmID: Int64;const aLog: TStringList = nil): string;
    function GetRouteName(const aIdx: Int64;const aLog: TStringList = nil): string;

    function GetRouteNameByOSMHash(const OsmID: Int64; AHashArr: TWayArray; AAccounts: TIntegerDynArray; const aLog: TStringList = nil): string;
    function GetRouteNameByOSMCsv(const AOsmID: Int64; AHashArr: TWayArray; AAccounts: TIntegerDynArray; const aLog: TStringList = nil): string;

    function GetRoutesFromRangeByType(const AAccounts: TIntegerDynArray;
                                            const ATopLeftLa, ATopLeftLo, ABottomRightLa, ABottomRightLo : double;
                                            const ARoadTypes : TIntSet; AJoin: integer): TJsonObject;
    function GetRoutesFromRangeNew(const AAccounts: TIntegerDynArray;
                                      const ATopLeftLa, ATopLeftLo, ABottomRightLa, ABottomRightLo : double;
                                      const ARoadTypes : TIntSet; AJoin: integer) : TDictionary<TIDLines,TRouteProperties>;
    function GetRoutesInfo(const AAccounts: TIntegerDynArray;
                                 const AID: TIDLines;
                                 const AHashStartStr, AHashEndStr: string;
                                 var ARouteInfo: TDictionary<TIDLines, TRouteProperties>;
                                 OnlyOne: Boolean = false): integer;
    function GetRoutesJson(ARouteInfo: TDictionary<TIDLines,TRouteProperties>): TJsonObject;
    function GetRoutesJsonCsv(const AAccounts: TIntegerDynArray; AHashArr: TWayArray; ARouteInfo: TDictionary<TIDLines,TRouteProperties>): TJsonObject;

    function GetNewID(const AAccounts: TIntegerDynArray): Int64;

    function UpdateRoadType(const AAccounts: TIntegerDynArray;
                                  const AOsmid, AHashStart, AHashEnd : Int64;
                                  ARoadType, ASpeed: integer; AOneWay: Boolean; AName: string) : boolean;
    function UpdateRoadTypeWay(const AAccounts: TIntegerDynArray;
                                     const AOsmid, AHashStart, AHashEnd : Int64;
                                  ARoadType, ASpeed: integer; AOneWay: Boolean; AName, AWay: string) : Boolean;

    function SaveNewRoute(const AAccounts: TIntegerDynArray;
                                const ARoadType, ASpeed : integer;
                                const AWay, AName : string;
                                const AOneWay : boolean;
                                var ANewID: TIDLines;
                                var ARoutePoints: TGeoPosArray;
                                const ASaveFile: Boolean = True): TJsonObject;
    function SaveNewRouteCross(const AAccounts: TIntegerDynArray; ARoadType, ASpeed : integer; AWay, AName : string; AOneWay : boolean; ACross: TNewCrossPointArray): TJsonObject;
    function SaveNewRoute1(const AAccounts: TIntegerDynArray;
                                   const OSMId: Int64; ARoadType,
                                   ASpeed: integer; APoints: TGeoPosArray; AOneWayFile: boolean): boolean;
    function SaveRouteName(const AOsmID : Int64; AName : string): boolean;
    function SaveRouteNameCsv(const AOsmID : Int64; AName : string; AAccounts: TIntegerDynArray; AHashStr: string): boolean;
    function SaveAllNameCsv(AAccounts: TIntegerDynArray; AHashArr: TWayArray): boolean;
    function SaveRouteNameHash(const AAccounts: TIntegerDynArray; AHashArr: TWayArray; const AOsmID : Int64; AName : string): boolean;
    function SaveReverseRoute(const AAccounts: TIntegerDynArray; const AID: TIDLines): Boolean;

    function DeleteRoute(AAccounts: TIntegerDynArray; AID: TIDLines; var ARoutePoints: TGeoPosArray): Boolean;
    function GetFromArray(AHash: Int64; AEdgeArray: TEdgeArray; NotID:Integer; var AFromArray: THashVectorArray): integer;
    function GetToArray(AHash: Int64; AEdgeArray: TEdgeArray; NotID:Integer; var AToArray: THashVectorArray): integer;
    function ReverseWayArr(AWayArr: TGeoPosArray): TGeoPosArray;
    function ReverseWayString(AWay: string): string;

    //-------------пакетное выполнение обновления, удаления-------------
    function SaveNewRoutesFromArray(const AAccounts: TIntegerDynArray): TJsonObject;
    function UpdateRoutesMinFromArray: TJsonObject;
    function UpdateRoutesWayFromArray(const AAccounts: TIntegerDynArray): TJsonObject;
    function DeleteRoutesFromArray(const AAccounts: TIntegerDynArray; ASave: boolean = True): boolean;

    //-------------вставка элементов в массив-------------
    procedure AddInWayPointArray(var AWayArr: TWayPointArray; APos: Int64; APoint: TGeoPos);
    procedure AddInListArray(var AListArr: THashVectorArray; APos: Int64; APoint: THashVector);
    //-------------удаление из массива со смещением-------------
    procedure DeleteWayPointArray(var AWayArr: TWayPointArray; APos: Int64);
    procedure DeleteEdgeArray(var AEdgeArr: TEdgeArray; APos: Int64);
    procedure DeleteListArray(var AListArr: THashVectorArray; APos: Int64);

    function SaveAllFiles(const AAccounts: TIntegerDynArray; const ATypeSet: TFileTypeSet = C7FileTypes): Boolean;
    function SaveZCFiles(const AAccounts: TIntegerDynArray): Boolean;

    //через AStar64.dll
    function GetAStarPath(APoints: string; ASpeed, ALimit, AFormatVariant, ATimeout: integer; AZone, ASign: Uint64; AAccounts: TIntegerDynArray;
                          AAStarDll: string = 'AStar64.dll'): string;
    //без AStar64.dll, напрямую
    function GetAStarAcc(APoints: string; ASpeed, ALimit, AFormatVariant, ATimeout: integer; AZone, ASign: Uint64; AAccounts: TIntegerDynArray): string;
    function GetRoutePointNum(APoint: TGeoPos; APointsArr: TGeoPosArray; ARadius: integer; var APointOnLine: TGeoPos): Integer;
    function MakeTwoRoutesFromOne(APoint: TGeoPos; AWaySource: string; var AWay1, AWay2: string): boolean;
    function MakeFewRoutesFromOne(APoints: array of TGeoPos; AWaySource: string; AMinDistToLineM: Integer; var AWays: TWayArray): boolean;
    function MakeFewRoutesFromOneWithWay(APoints: TPointDict; AWaySource: string; AMinDistToLineM: Integer; var AWays: TWayArray): boolean;

    function UnloadAStar: boolean;
    procedure SetAccountBorder(const AAccountID: Integer;
                                     const ADateTime: TDateTime;
                                     const APoint1, APoint2: TGeoPos);
    procedure SetAccountBorderZone(const AAccountID: Integer;
                                     const ADateTime: TDateTime;
                                     const ABorder: string);
    function ClearAllRoutes(const ARootPath: string;
                               const AAccounts: TIntegerDynArray): Boolean;
    function CheckTrack(const ARootPath: string;
                              const AAccounts: TIntegerDynArray;
                              const AWay: string): TJsonObject;
    //проверка фактического пути на соответствие имеющимся
    function CheckWay(const ARootPath: string;
                              const AAccounts: TIntegerDynArray;
                              const AWay: string;
                              const AMinRouteLengthM, AMinDistToLineM, ATimeout: integer): TJsonObject;
    function SetToString(ASet: TIntSet): string;
    function CorrectDoubleHashes(ARootPath: string; AAccounts: TIntegerDynArray; AHashStr: string; ACorrect, ASave: boolean): Integer;
    function CorrectDoubleHashesAll(ARootPath: string; AAccounts: TIntegerDynArray) : Integer;
    function CheckLinks(ARootPath: string; AAccounts: TIntegerDynArray; AHashStr: string; ASave: Boolean = false): Integer;
    function SeparateRoute(ARootPath: string; AAccounts: TIntegerDynArray; AID: TIDLines): Boolean;
    function ReCalcRoutes(ARootPath: string; AAccounts: TIntegerDynArray;
                          AIDInfo1, AIDInfo2: TRouteInfoMini; AHashStr: string): Integer;


  end;


implementation

var
  HAStar64: THandle;

{function GetHttpreqwestResult(strHttpReqwest: string): string;
var
  HTTPReqw : TIdHTTP;
  IDSSL : TIdSSLIOHandlerSocketOpenSSL;
  strResultReqw : string;
begin
  HTTPReqw := TIdHTTP.Create;
  IDSSL := TIdSSLIOHandlerSocketOpenSSL.Create;
  try
    try
      HTTPReqw.IOHandler := IDSSL;
      HTTPReqw.Request.UserAgent := 'Mozilla/5.0 (Windows NT 6.1; WOW64; Trident/7.0; AS; rv:11.0) like Gecko';
      HTTPReqw.Request.Accept := 'text/html';
      HTTPReqw.Request.AcceptLanguage := 'ru,en-us;q=0.7,en;q=0.3';
      HTTPReqw.Request.AcceptCharSet := 'windows-1251,utf-8;q=0.7,*;q=0.7';
      HTTPReqw.ReadTimeout :=50000;
      HTTPReqw.ConnectTimeout := 50000;

      strResultReqw := HTTPReqw.Get(Trim(strHttpReqwest));

      if Pos('200 OK', HTTPReqw.Response.ResponseText) <> 0 then
      begin
        Result := strResultReqw;
      end else
      begin
        Result := '';
      end;

    except
      on E: Exception do
      begin
        ToLog('Ошибка при выполнении Http запроса:' + strHttpReqwest + '  ('+ E.Message + ')');
        Result := '';
      end;
    end;
  finally
    HTTPReqw.Free;
    IDSSL.Free;
  end;
end;
 }
function FullRemoveDir(Dir: string; DeleteAllFilesAndFolders,
  StopIfNotAllDeleted, RemoveRoot: boolean): Boolean;
var
  i: Integer;
  SRec: TSearchRec;
  FN: string;
begin
  {$WARN SYMBOL_PLATFORM OFF}
  Result := False;
  if not DirectoryExists(Dir) then
    exit;
  Result := True;
  // Добавляем слэш в конце и задаем маску - "все файлы и директории"
  Dir := IncludeTrailingBackslash(Dir);
  i := FindFirst(Dir + '*', faAnyFile, SRec);
  try
    while i = 0 do
    begin
      // Получаем полный путь к файлу или директорию
      FN := Dir + SRec.Name;
      // Если это директория
      if (SRec.Attr = faDirectory) or (SRec.Attr = 48) then
      begin
        // Рекурсивный вызов этой же функции с ключом удаления корня
        if (SRec.Name <> '') and (SRec.Name <> '.') and (SRec.Name <> '..') then
        begin
          if DeleteAllFilesAndFolders then
            FileSetAttr(FN, faArchive);
          Result := FullRemoveDir(FN, DeleteAllFilesAndFolders,
            StopIfNotAllDeleted, True);
          if not Result and StopIfNotAllDeleted then
            exit;
        end;
      end
      else // Иначе удаляем файл
      begin
        if DeleteAllFilesAndFolders then
          FileSetAttr(FN, faArchive);
        Result := SysUtils.DeleteFile(FN);
        if not Result and StopIfNotAllDeleted then
          exit;
      end;
      // Берем следующий файл или директорию
      i := FindNext(SRec);
    end;
  finally
    SysUtils.FindClose(SRec);
  end;
  if not Result then
    exit;
  if RemoveRoot then // Если необходимо удалить корень - удаляем
    if not RemoveDir(Dir) then
      Result := false;
  {$WARN SYMBOL_PLATFORM ON}
end;

function TMapEditor.SaveAllFiles(
  const AAccounts: TIntegerDynArray;
  const ATypeSet: TFileTypeSet): Boolean;
var
  sHash: string;
  Holder: THoldRec;
begin
  try
    for sHash in FHoldRecDict.Keys do
    begin
      FHoldRecDict.TryGetValue(sHash, Holder);
      SetLength(Holder.ZCForward, Length(Holder.EdgeForward));
      SetLength(Holder.ZCBackward, Length(Holder.EdgeBackward));
      try
        TGFAdapter.Save(FRootPath, AAccounts[0], sHash, Holder, ATypeSet);
      except on E: Exception do
        begin
          ToLog('Ошибка сохранения файлов. Account='+IntToStr(AAccounts[0])+', Name='+sHash + ' ' + E.Message);
          TGeneral.IncWatchdog(CSAVE_ROUTE_FILE_ACCESS_DENIED);
          TGeneral.SendWatchdog;
          Exit(False);
        end;
      end;
    end;
    Result := True;
  except
    Result := False;
  end;
end;


function TMapEditor.SaveAllNameCsv(AAccounts: TIntegerDynArray;
  AHashArr: TWayArray): boolean;
var
  AddressDataFile: TFileStream;
  AddressWriter: TStreamWriter;
  sHash, sAcc, sName, AddressDataFileName: string;
  OSM_ID, iSize: Int64;
  RootPath: string;
  Dict: TOsmNameDict;
begin
  //Result := False;
  sAcc := IntToStr(AAccounts[0]);
  for sHash in AHashArr do
  begin
    RootPath := FRootPath{D:\IGF\}
      + sAcc + PathDelim{\}
      + Copy(sHash, 1, 1){u} + PathDelim{\}
      + Copy(sHash, 1, 2){uc} + PathDelim{\}
      + Copy(sHash, 1, 3){ucf} + PathDelim;
    // каталог
    ForceDirectories(RootPath);
    AddressDataFileName := RootPath + 'osm.csv';
    try
      AddressDataFile := TFileStream.Create(AddressDataFileName, fmCreate{ + fmOpenReadWrite});
      AddressWriter := TStreamWriter.Create(AddressDataFile, TEncoding.UTF8);
      if FOsmNameCsvDict.TryGetValue(sHash,Dict) then
      try
        iSize := Dict.Count;
        for OSM_ID in Dict.Keys do
        begin
//          application.ProcessMessages;
          Dict.TryGetValue(OSM_ID, sName);
          AddressWriter.WriteLine(IntToStr(OSM_ID) + #9 + sName);
        end;
        ToLog('Save '+AddressDataFileName +' '+ IntToStr(iSize));
      finally
        FreeAndNil(AddressDataFile);
        AddressWriter.Free;
  //      Dict.Free;
      end;
    except on E: Exception do
      begin
        ToLog('Ошибка сохранения файла '+RootPath + 'osm.csv '+E.Message);
        TGeneral.IncWatchdog(CSAVE_ROUTE_NAME_ACCESS_DENIED);
        TGeneral.SendWatchdog;
        Exit(False);
      end;
    end;
  end;
  ToLog('Сохранение файлов имен завершено');
  Result := True;
end;

function TMapEditor.SaveNewRoute(const AAccounts: TIntegerDynArray;
                                const ARoadType, ASpeed : integer;
                                const AWay, AName : string;
                                const AOneWay : boolean;
                                var ANewID: TIDLines;
                                var ARoutePoints: TGeoPosArray;
                                const ASaveFile: Boolean): TJsonObject;
var
  HashStart, HashEnd: Int64;
  HashStartStr, HashEndStr: string;
  Points: TGeoPosArray;
  bOneWayFile: Boolean;
  iPoints: Integer;
  NewID: Int64;
  ARoutes: TDictionary<TIDLines,TRouteProperties>;
  ID: TIDLines;
  Holder: THoldRec;
begin
  Result := TJsonObject.Create;
  ID.OSM_ID := 0;
  TGeoHash.DecodeArrayWorld(AWay, Points);
  iPoints := Length(Points);
  if iPoints < 2 then Exit;

  HashStart := Points[0].ToHash;
  HashEnd := Points[iPoints-1].ToHash;
  SetLength(ARoutePoints, 2);
  ARoutePoints[0] := Points[0];
  ARoutePoints[1] := Points[iPoints-1];

  HashStartStr := TGeoHash.ConvertBinToString(HashStart and CAreaHashMask, 5);
  HashEndStr := TGeoHash.ConvertBinToString(HashEnd and CAreaHashMask, 5);
  bOneWayFile := HashStartStr = HashEndStr;

  NewID := GetNewID(AAccounts);
  if not FHoldRecDict.ContainsKey(HashStartStr) then
  begin
    TGFAdapter.Load(FRootPath, AAccounts, HashStartStr, Holder);
    FHoldRecDict.Add(HashStartStr, Holder);
  end;
  if not FHoldRecDict.ContainsKey(HashEndStr) then
  begin
    TGFAdapter.Load(FRootPath, AAccounts, HashEndStr, Holder);
    FHoldRecDict.Add(HashEndStr, Holder);
  end;

  SaveNewRoute1(AAccounts, NewID, ARoadType, ASpeed, Points, bOneWayFile);

  if not AOneWay then
  begin
    Points := ReverseWayArr(Points);
    SaveNewRoute1(AAccounts, NewID, ARoadType, ASpeed, Points, bOneWayFile);
  end;

  SaveRouteNameCsv(NewID, AName, AAccounts, TGeoHash.ConvertBinToString(Points[0].ToHash and CAreaHashMask, 3));
  if ASaveFile then
  begin
    SaveAllFiles(AAccounts);
    SaveAllNameCsv(AAccounts, FOsmNameCsvDict.Keys.ToArray);
  end;
  SetAccountBorder(AAccounts[0], Now(), Points[0], Points[High(Points)]);

  ARoutes := TDictionary<TIDLines,TRouteProperties>.Create;
  try
    ID.OSM_ID := NewID;
    ID.HashStart := HashStart;
    ID.HashEnd := HashEnd;
    ANewID := ID;
    GetRoutesInfo(AAccounts, ID, HashStartStr, HashEndStr, ARoutes);
    Result := GetRoutesJsonCsv(AAccounts, FOsmNameCsvDict.Keys.ToArray, ARoutes);
  finally
    ARoutes.Free;
  end;

end;

function TMapEditor.SaveNewRoute1(const AAccounts: TIntegerDynArray;
  const OSMId: Int64; ARoadType, ASpeed: integer; APoints: TGeoPosArray;
  AOneWayFile: boolean): boolean;
var
  HashStart, HashEnd: Int64;
  i, j, iPoints: Integer;
  EdgeF, EdgeB: TEdge;
  dWayLength: Double;
  HashStr: string;
  HashStartStr, HashEndStr: string;
  Holder, HolderStart, HolderEnd: THoldRec;
  bFound, bOneWayFile: Boolean;
  HashVector: THashVector;
begin
  Result := False;
  iPoints := Length(APoints);
  if iPoints < 2 then Exit;

  HashStart := APoints[0].ToHash;
  HashEnd := APoints[iPoints-1].ToHash;
  HashStartStr := TGeoHash.ConvertBinToString(HashStart and CAreaHashMask, 5);
  HashEndStr := TGeoHash.ConvertBinToString(HashEnd and CAreaHashMask, 5);
  bOneWayFile := HashStartStr = HashEndStr;

  if not FHoldRecDict.TryGetValue(HashStartStr, HolderStart) then
  begin
    TGFAdapter.Load(FRootPath, AAccounts, HashStartStr, HolderStart);
    FHoldRecDict.Add(HashStartStr, HolderStart);
      if Length(HolderStart.EdgeBackward) = 0 then
        ToLog('New route Load error forward ' + HashEndStr);
  end;
  if not FHoldRecDict.TryGetValue(HashEndStr, HolderEnd) then
  begin
    TGFAdapter.Load(FRootPath, AAccounts, HashEndStr, HolderEnd);
    FHoldRecDict.Add(HashEndStr, HolderEnd);
      if Length(HolderEnd.EdgeBackward) = 0 then
        ToLog('New route Load error backward ' + HashEndStr);
  end;

  dWayLength := GetRouteLengthKm(APoints);

  EdgeF.HashVector.HashFrom := HashStart;
  EdgeF.HashVector.HashTo := HashEnd;
  EdgeF.CoordsFrom := APoints[0];
  EdgeF.CoordsTo := APoints[iPoints - 1];
  EdgeB.HashVector.HashFrom := HashEnd;
  EdgeB.HashVector.HashTo := HashStart;
  EdgeB.CoordsTo := APoints[0];
  EdgeB.CoordsFrom := APoints[iPoints - 1];

  EdgeF.Distance := dWayLength;
  EdgeF.MaxSpeed := ASpeed;
  EdgeF.RoadType := ARoadType;
  EdgeB.Distance := dWayLength;
  EdgeB.MaxSpeed := ASpeed;
  EdgeB.RoadType := ARoadType;

  if iPoints = 2 then
  begin
    EdgeF.WayIndex := -1;
    EdgeB.WayIndex := -1;
  end else
  begin
    EdgeF.WayIndex := Length(HolderStart.Ways);
    EdgeB.WayIndex := Length(HolderEnd.Ways)
  end;
  EdgeF.WayCount := iPoints - 2;
  EdgeB.WayCount := iPoints - 2;
  EdgeF.ID := OSMId;
  EdgeB.ID := OSMId;

  EdgeF.LinkIndex := Length(HolderStart.ListForward);
  EdgeB.LinkIndex := Length(HolderEnd.ListBackward);
  EdgeF.LinkCount := 0;
  EdgeB.LinkCount := 0;

  for I := 1 to iPoints - 2 do
  begin
    SetLength(HolderStart.Ways, Length(HolderStart.Ways)+1);
    HolderStart.Ways[High(HolderStart.Ways)] := APoints[i];
  end;

  if not bOneWayFile then
  begin
    for I := 1 to iPoints - 2 do
    begin
      SetLength(HolderEnd.Ways, Length(HolderEnd.Ways)+1);
      HolderEnd.Ways[High(HolderEnd.Ways)] := APoints[i];
    end;
  end;
  //считаем свои линки и добавляем
  for I := Low(HolderEnd.EdgeForward) to High(HolderEnd.EdgeForward) do
  begin
    if HashEnd = HolderEnd.EdgeForward[i].HashVector.HashFrom then
    begin
      EdgeF.LinkCount := EdgeF.LinkCount + 1;
      SetLength(HolderStart.ListForward, Length(HolderStart.ListForward) + 1);
      HolderStart.ListForward[High(HolderStart.ListForward)].HashFrom := HolderEnd.EdgeForward[i].HashVector.HashFrom;
      HolderStart.ListForward[High(HolderStart.ListForward)].HashTo := HolderEnd.EdgeForward[i].HashVector.HashTo;
    end;
  end;
  for I := Low(HolderStart.EdgeBackward) to High(HolderStart.EdgeBackward) do
  begin
    if HashStart = HolderStart.EdgeBackward[i].HashVector.HashFrom then
    begin
      EdgeB.LinkCount := EdgeB.LinkCount + 1;
      if bOneWayFile then
      begin
        SetLength(HolderStart.ListBackward, Length(HolderStart.ListBackward) + 1);
        HolderStart.ListBackward[High(HolderStart.ListBackward)].HashFrom := HolderStart.EdgeBackward[i].HashVector.HashFrom;
        HolderStart.ListBackward[High(HolderStart.ListBackward)].HashTo := HolderStart.EdgeBackward[i].HashVector.HashTo;
      end else
      begin
        SetLength(HolderEnd.ListBackward, Length(HolderEnd.ListBackward) + 1);
        HolderEnd.ListBackward[High(HolderEnd.ListBackward)].HashFrom := HolderStart.EdgeBackward[i].HashVector.HashFrom;
        HolderEnd.ListBackward[High(HolderEnd.ListBackward)].HashTo := HolderStart.EdgeBackward[i].HashVector.HashTo;
      end;
    end;
  end;

  SetLength(HolderStart.EdgeForward, Length(HolderStart.EdgeForward)+1);
  HolderStart.EdgeForward[High(HolderStart.EdgeForward)] := EdgeF;

  if bOneWayFile then
  begin
    SetLength(HolderStart.EdgeBackward, Length(HolderStart.EdgeBackward)+1);
    HolderStart.EdgeBackward[High(HolderStart.EdgeBackward)] := EdgeB;
    FHoldRecDict.AddOrSetValue(HashStartStr, HolderStart);
  end else
  begin
    SetLength(HolderEnd.EdgeBackward, Length(HolderEnd.EdgeBackward)+1);
    HolderEnd.EdgeBackward[High(HolderEnd.EdgeBackward)] := EdgeB;
    FHoldRecDict.AddOrSetValue(HashStartStr, HolderStart);
    FHoldRecDict.AddOrSetValue(HashEndStr, HolderEnd);
  end;


  //пересчитываем линки у других
  //проверяем вхожящие в HashStart, смотрим в EdgeBackward, правим EdgeForward
  for I := Low(HolderStart.EdgeBackward) to High(HolderStart.EdgeBackward) do
  begin
    if HolderStart.EdgeBackward[i].HashVector.HashFrom = HashStart then
    begin
      //нашли конец в EdgeBackward, ищем другой конец
      HashStr := TGeoHash.ConvertBinToString(HolderStart.EdgeBackward[i].HashVector.HashTo and CAreaHashMask, 5);
      if not FHoldRecDict.ContainsKey(HashStr) then
      begin
        TGFAdapter.Load(FRootPath, AAccounts, HashStr, Holder);
        FHoldRecDict.Add(HashStr, Holder);
      end else
        FHoldRecDict.TryGetValue(HashStr, Holder);
      //ищем начало в EdgeForward и уменьшаем на 1
      bFound := False;
      j := 0;
      while (j < Length(Holder.EdgeForward)) do
      begin
        if (Holder.EdgeForward[j].HashVector.HashTo = HolderStart.EdgeBackward[i].HashVector.HashFrom) and
           (Holder.EdgeForward[j].HashVector.HashFrom = HolderStart.EdgeBackward[i].HashVector.HashTo) and
           (Holder.EdgeForward[j].ID = HolderStart.EdgeBackward[i].ID) then
        begin
         //когда находим увеличиваем LinkCount и добавляем в со смещением последующих
          bFound := True;
          HashVector.HashFrom := HashStart;
          HashVector.HashTo := HashEnd;
          Holder.EdgeForward[j].LinkCount := Holder.EdgeForward[j].LinkCount + 1;
          AddInListArray(Holder.ListForward, Holder.EdgeForward[j].LinkIndex, HashVector);
        end else
        if bFound then
          Holder.EdgeForward[j].LinkIndex := Holder.EdgeForward[j].LinkIndex + 1;

        inc(j);
      end;
      FHoldRecDict.AddOrSetValue(HashStr, Holder);
    end;
  end;

  //проверяем исходящие из HashEnd
  for I := Low(HolderEnd.EdgeForward) to High(HolderEnd.EdgeForward) do
  begin
    if HolderEnd.EdgeForward[i].HashVector.HashFrom = HashEnd then
    begin
      //нашли конец в EdgeForward, ищем другой конец
      HashStr := TGeoHash.ConvertBinToString(HolderEnd.EdgeForward[i].HashVector.HashTo and CAreaHashMask, 5);
      if not FHoldRecDict.ContainsKey(HashStr) then
      begin
        TGFAdapter.Load(FRootPath, AAccounts, HashStr, Holder);
        FHoldRecDict.Add(HashStr, Holder);
      end else
        FHoldRecDict.TryGetValue(HashStr, Holder);
      //ищем конец в EdgeBackward и уменьшаем на 1
      bFound := False;
      j := 0;
      while (j < Length(Holder.EdgeBackward)) do
      begin
        if (Holder.EdgeBackward[j].HashVector.HashTo = HolderEnd.EdgeForward[i].HashVector.HashFrom) and
           (Holder.EdgeBackward[j].HashVector.HashFrom = HolderEnd.EdgeForward[i].HashVector.HashTo) and
           (Holder.EdgeBackward[j].ID = HolderEnd.EdgeForward[i].ID) then
        begin
         //когда находим увеличиваем LinkCount и добавляем в со смещением последующих
          bFound := True;
          HashVector.HashFrom := HashEnd;
          HashVector.HashTo := HashStart;
          Holder.EdgeBackward[j].LinkCount := Holder.EdgeBackward[j].LinkCount + 1;
          AddInListArray(Holder.ListBackward, Holder.EdgeBackward[j].LinkIndex, HashVector);
        end else
        if bFound then
          Holder.EdgeBackward[j].LinkIndex := Holder.EdgeBackward[j].LinkIndex + 1;

        inc(j);
      end;
      FHoldRecDict.AddOrSetValue(HashStr, Holder);
    end;
  end;

  Result := True;
end;

function TMapEditor.SaveNewRouteCross(const AAccounts: TIntegerDynArray; ARoadType,
  ASpeed: integer; AWay, AName: string; AOneWay: boolean;
  ACross: TNewCrossPointArray): TJsonObject;
var
  sWay1, sWay2, sRouteName: string;
  Points: TGeoPosArray;
  i: Integer;
  ARoutes: TDictionary<TIDLines,TRouteProperties>;
  RouteProperties: TRouteProperties;
  ID, IDRev, NewID: TIDLines;
  IDArray: array of TIDLines;
  HashStartStr, HashEndStr: string;
  Holder: THoldRec;
  Names: TDictionary<string,integer>;
  RoutePoints: TGeoPosArray;
begin
//сначала делим пересекаемые дороги на части
  Names := TDictionary<string,integer>.Create;
  try
    SetLength(IDArray, 0);
    for I := Low(ACross) to High(ACross) do
    begin
      TGeoHash.DecodeArrayWorld(ACross[i].PointGW, Points);
      if Length(Points) <> 1 then
      begin
        Result := TJsonObject(TJsonObject.Parse('{"Result":"Connection not 1 Point"}'));
        Exit;
      end;
      ID := ID.StrToIDLines(ACross[i].IDStr);
      HashStartStr := TGeoHash.ConvertBinToString(ID.HashStart and CAreaHashMask, 5);
      HashEndStr := TGeoHash.ConvertBinToString(ID.HashEnd and CAreaHashMask, 5);
      Names.AddOrSetValue(Copy(HashStartStr,1,3),0);
      Names.AddOrSetValue(Copy(HashEndStr,1,3),0);
      if not FHoldRecDict.ContainsKey(HashStartStr) then
      begin
        TGFAdapter.Load(FRootPath, AAccounts, HashStartStr, Holder);
        FHoldRecDict.Add(HashStartStr, Holder);
      end;
      if not FHoldRecDict.ContainsKey(HashEndStr) then
      begin
        TGFAdapter.Load(FRootPath, AAccounts, HashEndStr, Holder);
        FHoldRecDict.Add(HashEndStr, Holder);
      end;
      //если находим пересекаемую дорогу, удаляем, создаем 2 новые
      ARoutes := TDictionary<TIDLines,TRouteProperties>.Create;
      if GetRoutesInfo(AAccounts, ID, HashStartStr, HashEndStr, ARoutes) = 1 then
      begin
        ARoutes.TryGetValue(ID, RouteProperties);
        sRouteName := GetRouteNameByOSMCsv(ID.OSM_ID, Names.Keys.ToArray, AAccounts);
        MakeTwoRoutesFromOne(Points[0], RouteProperties.sWay, sWay1, sWay2);
        if sWay1 = '' then
        begin
          Result := TJsonObject(TJsonObject.Parse('{"Result":"Not Point on route"}'));
          Exit;
        end;
        //удаляем старую и создаем 2 новые с такими же параметрами
        if DeleteRoute(AAccounts, ID, RoutePoints) then
        begin
          if RouteProperties.OneWay = 0 then
          begin
            IDRev.OSM_ID := ID.OSM_ID;
            IDRev.HashStart := ID.HashEnd;
            IDRev.HashEnd := ID.HashStart;
            DeleteRoute(AAccounts, IDRev, RoutePoints);
          end;
          SaveNewRoute(AAccounts, RouteProperties.RoadType, RouteProperties.Speed,
                       sWay1, sRouteName, RouteProperties.OneWay = 1, NewID, RoutePoints, False);
          if NewID.OSM_ID <> 0 then
          begin
            SetLength(IDArray, Length(IDArray) + 1);
            IDArray[High(IDArray)] := NewID;
          end;
          SaveNewRoute(AAccounts, RouteProperties.RoadType, RouteProperties.Speed,
                       sWay2, sRouteName, RouteProperties.OneWay = 1, NewID, RoutePoints, False);
          if NewID.OSM_ID <> 0 then
          begin
            SetLength(IDArray, Length(IDArray) + 1);
            IDArray[High(IDArray)] := NewID;
          end;
        end;
      end;
    end;
    //создаем новую дорогу
    SaveNewRoute(AAccounts, ARoadType, ASpeed,
                 AWay, AName, AOneWay, NewID, RoutePoints, False);
    if NewID.OSM_ID <> 0 then
    begin
      SetLength(IDArray, Length(IDArray) + 1);
      IDArray[High(IDArray)] := NewID;
    end;

    SaveAllFiles(AAccounts);

    ARoutes.Clear;
    try
      for I := Low(IDArray) to High(IDArray) do
        GetRoutesInfo(AAccounts, IDArray[i],
                     TGeoHash.ConvertBinToString(IDArray[i].HashStart and CAreaHashMask, 5),
                     TGeoHash.ConvertBinToString(IDArray[i].HashEnd and CAreaHashMask, 5),
                     ARoutes);
      Result := GetRoutesJsonCsv(AAccounts, FOsmNameCsvDict.Keys.ToArray, ARoutes);
    finally
      ARoutes.Free;
    end;
  finally
    Names.Free;
  end;
end;

function TMapEditor.SaveNewRoutesFromArray(const AAccounts: TIntegerDynArray): TJsonObject;
var
  i, j: Integer;
  RouteWithCross: TDictionary<TIDLines,TPointDict>;
  ID, IDRev, NewID: TIDLines;
  PointsList: TPointDict;
  Points: TGeoPosArray;
  Ways: TWayArray;
  ARoutes: TDictionary<TIDLines,TRouteProperties>;
  Names: TDictionary<string,Integer>;
  RouteProperties: TRouteProperties;
  HashStartStr, HashEndStr, sRouteName, sWay: string;
  Holder: THoldRec;
  IDArray: array of TIDLines;
  Areas: TLandMarkAreaDictionary;
  NeedSaveAreas: Boolean;
  RoutePoints: TGeoPosArray;
begin
  RouteWithCross := TDictionary<TIDLines,TPointDict>.Create;
  ARoutes := TDictionary<TIDLines,TRouteProperties>.Create;
  Names := TDictionary<string,Integer>.Create;
  Areas := FLandMarkArea.LoadAreaFile(FRootPath, AAccounts);
  NeedSaveAreas := False;
  try
    //составляем список пересекаемых дорог и точек
    for i := Low(FNewRouteArr) to High(FNewRouteArr) do
    begin
      for j := Low(FNewRouteArr[i].NewCross) to High(FNewRouteArr[i].NewCross) do
      begin
        TGeoHash.DecodeArrayWorld(FNewRouteArr[i].NewCross[j].PointGW, Points);
        if not RouteWithCross.TryGetValue(ID.StrToIDLines(FNewRouteArr[i].NewCross[j].IDStr), PointsList) then
        begin
          PointsList := TDictionary<TGeoPos, integer>.Create;
          PointsList.Add(Points[0], i);
        end else
        begin
          if not PointsList.ContainsKey(Points[0]) then
            PointsList.Add(Points[0], i);
        end;
        RouteWithCross.AddOrSetValue(ID.StrToIDLines(FNewRouteArr[i].NewCross[j].IDStr), PointsList);
        //заполняем массив файлов имен
        Names.AddOrSetValue(TGeoHash.ConvertBinToString(Points[0].ToHash and CAreaHashMask, 3), 0);
        Names.AddOrSetValue(TGeoHash.ConvertBinToString(Points[High(Points)].ToHash and CAreaHashMask, 3), 0);
      end;
      TGeoHash.DecodeArrayWorld(FNewRouteArr[i].way, Points);
      Names.AddOrSetValue(TGeoHash.ConvertBinToString(Points[0].ToHash and CAreaHashMask, 3), 0);
      Names.AddOrSetValue(TGeoHash.ConvertBinToString(Points[High(Points)].ToHash and CAreaHashMask, 3), 0);
    end;

    LoadAddrOSMCsv(AAccounts, Names.Keys.ToArray);
    //разделяем дороги в точках пересечения
    for ID in RouteWithCross.Keys do
    begin
      if TGeneral.FStop then
      begin
        ToLog('TMapEditor.SaveNewRoutesFromArray. Прервано пользователем. ');
        Exit(TJsonObject.Create);
      end;
      SetLength(RoutePoints, 0);
      RouteWithCross.TryGetValue(ID, PointsList);
      HashStartStr := TGeoHash.ConvertBinToString(ID.HashStart and CAreaHashMask, 5);
      HashEndStr := TGeoHash.ConvertBinToString(ID.HashEnd and CAreaHashMask, 5);
      Names.AddOrSetValue(Copy(HashStartStr,1,3), 0);
      Names.AddOrSetValue(Copy(HashEndStr,1,3), 0);
      if not FHoldRecDict.ContainsKey(HashStartStr) then
      begin
        TGFAdapter.Load(FRootPath, AAccounts, HashStartStr, Holder);
        FHoldRecDict.Add(HashStartStr, Holder);
      end;
      if not FHoldRecDict.ContainsKey(HashEndStr) then
      begin
        TGFAdapter.Load(FRootPath, AAccounts, HashEndStr, Holder);
        FHoldRecDict.Add(HashEndStr, Holder);
      end;
      //если находим пересекаемую дорогу, удаляем, создаем  новые
      ARoutes.Clear;
      if GetRoutesInfo(AAccounts, ID, HashStartStr, HashEndStr, ARoutes) = 1 then
      begin
        ARoutes.TryGetValue(ID, RouteProperties);
        sRouteName := GetRouteNameByOSMCsv(ID.OSM_ID, Names.Keys.ToArray, AAccounts);
        MakeFewRoutesFromOneWithWay(PointsList, RouteProperties.sWay, TGeneral.FRadius, Ways);
        //удаляем старую и создаем новые с такими же параметрами
        if DeleteRoute(AAccounts, ID, RoutePoints) then
        begin
          if RouteProperties.OneWay = 0 then
          begin
            IDRev.OSM_ID := ID.OSM_ID;
            IDRev.HashStart := ID.HashEnd;
            IDRev.HashEnd := ID.HashStart;
            DeleteRoute(AAccounts, IDRev, RoutePoints);
          end;
        end;
        for sWay in Ways do
        begin
          SaveNewRoute(AAccounts, RouteProperties.RoadType, RouteProperties.Speed,
                       sWay, sRouteName, RouteProperties.OneWay = 1, NewID, RoutePoints, False);
          if NewID.OSM_ID <> 0 then
          begin
            SetLength(IDArray, Length(IDArray) + 1);
            IDArray[High(IDArray)] := NewID;
          end;
        end;
      end;
      NeedSaveAreas := NeedSaveAreas or
                       FLandMarkArea.CheckRouteInTrack(FRootPath, AAccounts, RoutePoints, Areas, TGeneral.FDetailLog);
    end;
    //Теперь создаем новые дороги
    for i := Low(FNewRouteArr) to High(FNewRouteArr) do
    begin
      SaveNewRoute(AAccounts, FNewRouteArr[i].RoadType, FNewRouteArr[i].Speed,
                   FNewRouteArr[i].Way, FNewRouteArr[i].Name, FNewRouteArr[i].OneWay, NewID, RoutePoints, False);
      if NewID.OSM_ID <> 0 then
      begin
        SetLength(IDArray, Length(IDArray) + 1);
        IDArray[High(IDArray)] := NewID;
      end;
      NeedSaveAreas := NeedSaveAreas or
                       FLandMarkArea.CheckRouteInTrack(FRootPath, AAccounts, RoutePoints, Areas, TGeneral.FDetailLog);
    end;

    SaveAllFiles(AAccounts);
    SaveAllNameCsv(AAccounts, FOsmNameCsvDict.Keys.ToArray);
    if NeedSaveAreas then
      FLandMarkArea.SaveAreas(FRootPath, AAccounts[0], Areas, True, TGeneral.FDetailLog);

    ARoutes.Clear;

    for I := Low(IDArray) to High(IDArray) do
      GetRoutesInfo(AAccounts, IDArray[i],
                   TGeoHash.ConvertBinToString(IDArray[i].HashStart and CAreaHashMask, 5),
                   TGeoHash.ConvertBinToString(IDArray[i].HashEnd and CAreaHashMask, 5),
                   ARoutes);
    Result := GetRoutesJsonCsv(AAccounts, FOsmNameCsvDict.Keys.ToArray, ARoutes);

  finally
    Names.Free;
    ARoutes.Free;
    RouteWithCross.Free;
    Areas.Free;
    if Assigned(PointsList) then
      PointsList.Free;
  end;
end;

function TMapEditor.SaveReverseRoute(const AAccounts: TIntegerDynArray; const AID: TIDLines): Boolean;
var
  HashStart, HashEnd: Int64;
  HashStartStr, HashEndStr: string;
  Points: TGeoPosArray;
  bOneWayFile: Boolean;
  i, iPoints: Integer;
  NewID: Int64;
  ARoutes: TDictionary<TIDLines,TRouteProperties>;
  ID: TIDLines;
  RouteProperties: TRouteProperties;
begin
  Result := False;
  ID.OSM_ID := AID.OSM_ID;
  ID.HashStart := AID.HashEnd;
  ID.HashEnd := AID.HashStart;

  NewID := AID.OSM_ID;
  HashStart := AID.HashEnd;
  HashEnd := AID.HashStart;

  HashStartStr := TGeoHash.ConvertBinToString(HashStart and CAreaHashMask, 5);
  HashEndStr := TGeoHash.ConvertBinToString(HashEnd and CAreaHashMask, 5);
  bOneWayFile := HashStartStr = HashEndStr;

  ARoutes := TDictionary<TIDLines,TRouteProperties>.Create;
  try
    i := GetRoutesInfo(AAccounts, ID, HashStartStr, HashEndStr, ARoutes);
    if i = 0 then //если обратной дороги нет и надо создавать
    begin
      ARoutes.Clear;
      i := GetRoutesInfo(AAccounts, AID, HashEndStr, HashStartStr, ARoutes);
      if i = 0 then
      begin
        ToLog('не загружена дорога '+AID.ToString);
        Exit(False);
      end;
      ARoutes.TryGetValue(AID, RouteProperties);

      TGeoHash.DecodeArrayWorld(RouteProperties.sWay, Points);
      Points := ReverseWayArr(Points);

      iPoints := Length(Points);
      if iPoints < 2 then Exit;

      SaveNewRoute1(AAccounts, NewID, RouteProperties.RoadType, RouteProperties.Speed, Points, bOneWayFile);
    end;
    Result := True;

  finally
    ARoutes.Free;
  end;

end;


function TMapEditor.SaveRouteName(const AOsmID: Int64;
  AName: string): boolean;
var
  Idx: Int64;
  FIdxArr: array of TAddrIdx;
  AddrIdx: TAddrIdx;
  i : Integer;
  bFind: Boolean;
  AddressWriter: TStreamWriter;
begin
  Result := False;
  if FAddrOsmDict.TryGetValue(AOsmID, Idx) then
  begin
    //в списке есть, добавляем имя и меняем адрес
    FIdxFile := TFileStream.Create(FIdxFileName, fmOpenReadWrite + fmShareDenyNone);
    FAddressDataFile := TFileStream.Create(FAddressDataFileName, fmOpenReadWrite + fmShareDenyNone);
    AddressWriter := TStreamWriter.Create(FAddressDataFile, TEncoding.ANSI);
    try
      SetLength(FIdxArr, FIdxFile.Size div SizeOf(TAddrIdx));
      FIdxFile.ReadBuffer(FIdxArr[0], FIdxFile.Size);
      i := 0;
      bFind := False;
      while (i < Length(FIdxArr)) and not bFind do
      begin
        if FIdxArr[i].OsmID = AOsmID then
        begin
          bFind := True;
          AddrIdx.OsmID := AOsmID;
          AddrIdx.Idx := FAddressDataFile.Size;
          FIdxFile.Seek(i * SizeOf(TAddrIdx), soBeginning);
          FIdxFile.Write(AddrIdx, SizeOf(TAddrIdx));
          FAddressDataFile.Seek(0, soEnd);
          AddressWriter.Write(AName + #13#10);
          FAddrOsmDict.AddOrSetValue(AddrIdx.OsmID, AddrIdx.Idx);
          Result := True;
        end else
          inc(i);
      end;

    finally
      FIdxFile.Free;
      AddressWriter.Free;
      FreeAndNil(FAddressDataFile);
    end;
  end else
  begin
    //в списке нет, добавляем OsmID, имя
    FIdxFile := TFileStream.Create(FIdxFileName, fmOpenReadWrite + fmShareDenyNone);
    FAddressDataFile := TFileStream.Create(FAddressDataFileName, fmOpenReadWrite + fmShareDenyNone);
    AddressWriter := TStreamWriter.Create(FAddressDataFile, TEncoding.ANSI);
    try
      SetLength(FIdxArr, FIdxFile.Size div SizeOf(TAddrIdx));
      FIdxFile.ReadBuffer(FIdxArr[0], FIdxFile.Size);

      AddrIdx.OsmID := AOsmID;
      AddrIdx.Idx := FAddressDataFile.Size;
      FIdxFile.Seek(0, soEnd);
      FIdxFile.Write(AddrIdx, SizeOf(TAddrIdx));
      FAddressDataFile.Seek(0, soEnd);
      AddressWriter.Write(AName + #13#10);
      FAddrOsmDict.AddOrSetValue(AddrIdx.OsmID, AddrIdx.Idx);
      Result := True;

    finally
      FIdxFile.Free;
      AddressWriter.Free;
      FreeAndNil(FAddressDataFile);
    end;
  end;
end;


function TMapEditor.SaveRouteNameCsv(const AOsmID: Int64; AName: string;
  AAccounts: TIntegerDynArray; AHashStr: string): boolean;
var
  Dict: TOsmNameDict;
begin
  if AName = '' then Exit(True);

  if not Assigned(FOsmNameCsvDict) then
    FOsmNameCsvDict := TDictionary<string,TOsmNameDict>.Create;

  if not FOsmNameCsvDict.TryGetValue(AHashStr, Dict) then
    Dict := TDictionary<Int64,string>.Create;

  Dict.AddOrSetValue(AOsmID, AName);
  FOsmNameCsvDict.AddOrSetValue(AHashStr, Dict);

  Result := True;
end;

function TMapEditor.SaveRouteNameHash(const AAccounts: TIntegerDynArray;
  AHashArr: TWayArray; const AOsmID: Int64; AName: string): boolean;
var
  FIdxArr: array of TAddrIdx;
  AddrIdx: TAddrIdx;
  i : Integer;
  bFind: Boolean;
  AddressWriter: TStreamWriter;
  sName: string;
begin
  Result := False;
  LoadAddrOSMHash(AAccounts, AHashArr);
  if FOsmNameDict.TryGetValue(AOsmID, sName) then
  begin
    //в списке есть, добавляем имя и меняем адрес
    FIdxFile := TFileStream.Create(FIdxFileName, fmOpenReadWrite + fmShareDenyNone);
    FAddressDataFile := TFileStream.Create(FAddressDataFileName, fmOpenReadWrite + fmShareDenyNone);
    AddressWriter := TStreamWriter.Create(FAddressDataFile, TEncoding.ANSI);
    try
      SetLength(FIdxArr, FIdxFile.Size div SizeOf(TAddrIdx));
      FIdxFile.ReadBuffer(FIdxArr[0], FIdxFile.Size);
      i := 0;
      bFind := False;
      while (i < Length(FIdxArr)) and not bFind do
      begin
        if FIdxArr[i].OsmID = AOsmID then
        begin
          bFind := True;
          AddrIdx.OsmID := AOsmID;
          AddrIdx.Idx := FAddressDataFile.Size;
          FIdxFile.Seek(i * SizeOf(TAddrIdx), soBeginning);
          FIdxFile.Write(AddrIdx, SizeOf(TAddrIdx));
          FAddressDataFile.Seek(0, soEnd);
          AddressWriter.Write(AName + #13#10);
          FAddrOsmDict.AddOrSetValue(AddrIdx.OsmID, AddrIdx.Idx);
          Result := True;
        end else
          inc(i);
      end;

    finally
      FIdxFile.Free;
      AddressWriter.Free;
      FreeAndNil(FAddressDataFile);
    end;
  end else
  begin
    //в списке нет, добавляем OsmID, имя
    FIdxFile := TFileStream.Create(FIdxFileName, fmOpenReadWrite + fmShareDenyNone);
    FAddressDataFile := TFileStream.Create(FAddressDataFileName, fmOpenReadWrite + fmShareDenyNone);
    AddressWriter := TStreamWriter.Create(FAddressDataFile, TEncoding.ANSI);
    try
      SetLength(FIdxArr, FIdxFile.Size div SizeOf(TAddrIdx));
      FIdxFile.ReadBuffer(FIdxArr[0], FIdxFile.Size);

      AddrIdx.OsmID := AOsmID;
      AddrIdx.Idx := FAddressDataFile.Size;
      FIdxFile.Seek(0, soEnd);
      FIdxFile.Write(AddrIdx, SizeOf(TAddrIdx));
      FAddressDataFile.Seek(0, soEnd);
      AddressWriter.Write(AName + #13#10);
      FAddrOsmDict.AddOrSetValue(AddrIdx.OsmID, AddrIdx.Idx);
      Result := True;

    finally
      FIdxFile.Free;
      AddressWriter.Free;
      FreeAndNil(FAddressDataFile);
    end;
  end;
end;

function TMapEditor.SaveZCFiles(
  const AAccounts: TIntegerDynArray): Boolean;
var
  sHash: string;
begin
  Result := True;
  for sHash in FHoldRecDict.Keys do
  begin
    Result := Result and FZC.SaveTwoZC(FRootPath, AAccounts, sHash);
  end;
end;


function TMapEditor.SeparateRoute(ARootPath: string; AAccounts: TIntegerDynArray;
  AID: TIDLines): Boolean;
var
  i, k: Integer;
  HashStr1, HashStr2: string;
  Points1, Points2, Points: TGeoPosArray;
  Point: TGeoPos;
  bFound: Boolean;
  RoadType, Speed: Integer;
begin
  HashStr1 := TGeoHash.ConvertBinToString(AID.HashStart and CAreaHashMask, 5);
  HashStr2 := TGeoHash.ConvertBinToString(AID.HashEnd and CAreaHashMask, 5);

  if not FHoldRecDict.TryGetValue(HashStr1, FStartEdgeFiles) then
  begin
    TGFAdapter.Load(ARootPath, AAccounts, HashStr1, FStartEdgeFiles);
    FHoldRecDict.Add(HashStr1, FStartEdgeFiles);
    if Length(FStartEdgeFiles.EdgeForward) = 0 then
      ToLog('function Load error forward ' + HashStr1);
  end;
  if not FHoldRecDict.TryGetValue(HashStr2, FEndEdgeFiles) then
  begin
    TGFAdapter.Load(ARootPath, AAccounts, HashStr2, FEndEdgeFiles);
    FHoldRecDict.Add(HashStr2, FEndEdgeFiles);
    if Length(FEndEdgeFiles.EdgeBackward) = 0 then
      ToLog('function Load error backward ' + HashStr2);
  end;

  bFound := False;
  i := 0;
  k := -1;
  while (i < Length(FStartEdgeFiles.EdgeForward)) and not bFound do
  begin
    if (AID.OSM_ID = FStartEdgeFiles.EdgeForward[i].ID) and
       (AID.HashStart = FStartEdgeFiles.EdgeForward[i].HashVector.HashFrom) and
       (AID.HashEnd = FStartEdgeFiles.EdgeForward[i].HashVector.HashTo) then
    begin
      k := i;
      bFound := True;
    end;
    inc(i);
  end;

  if not bFound then
    Exit(False);

  Result := True;
  RoadType := FStartEdgeFiles.EdgeForward[k].RoadType;
  Speed := FStartEdgeFiles.EdgeForward[k].MaxSpeed;
  if FStartEdgeFiles.EdgeForward[k].WayCount > 0 then
  begin
  //сделано для того, чтобы прямая и обратная дороги делились в одной точке
    if FStartEdgeFiles.EdgeForward[k].HashVector.HashFrom > FStartEdgeFiles.EdgeForward[k].HashVector.HashTo then
    begin
      Point := FStartEdgeFiles.Ways[FStartEdgeFiles.EdgeForward[k].WayIndex];
      SetLength(Points1, 2);
      Points1[0] := FStartEdgeFiles.EdgeForward[k].CoordsFrom;
      Points1[1] := Point;

      SetLength(Points2, FStartEdgeFiles.EdgeForward[k].WayCount + 1);
      Points2[0] := Point;
      Points2[High(Points2)] := FStartEdgeFiles.EdgeForward[k].CoordsTo;
      for I := 1 to FStartEdgeFiles.EdgeForward[k].WayCount - 1  do
        Points2[i] := FStartEdgeFiles.Ways[FStartEdgeFiles.EdgeForward[k].WayIndex + i];
    end else
    begin //обратная дорога, точки наоборот считаем
      Point := FStartEdgeFiles.Ways[FStartEdgeFiles.EdgeForward[k].WayIndex + FStartEdgeFiles.EdgeForward[k].WayCount - 1];
      SetLength(Points1, 2);
      Points1[0] := Point;
      Points1[1] := FStartEdgeFiles.EdgeForward[k].CoordsTo;

      SetLength(Points2, FStartEdgeFiles.EdgeForward[k].WayCount + 1);
      Points2[0] := FStartEdgeFiles.EdgeForward[k].CoordsFrom;
      Points2[High(Points2)] := Point;
      for I := 1 to FStartEdgeFiles.EdgeForward[k].WayCount - 1  do
        Points2[i] := FStartEdgeFiles.Ways[FStartEdgeFiles.EdgeForward[k].WayIndex + i - 1];
    end;
  end else
  begin
    Point.Latitude := (FStartEdgeFiles.EdgeForward[k].CoordsFrom.Latitude +
                       FStartEdgeFiles.EdgeForward[k].CoordsTo.Latitude)/2;
    Point.Longitude := (FStartEdgeFiles.EdgeForward[k].CoordsFrom.Longitude +
                       FStartEdgeFiles.EdgeForward[k].CoordsTo.Longitude)/2;
    SetLength(Points1, 2);
    Points1[0] := FStartEdgeFiles.EdgeForward[k].CoordsFrom;
    Points1[1] := Point;
    SetLength(Points2, 2);
    Points2[0] := Point;
    Points2[1] := FStartEdgeFiles.EdgeForward[k].CoordsTo;
  end;

  if DeleteRoute(AAccounts, AID, Points) then
  begin
    if not SaveNewRoute1(AAccounts, AID.OSM_ID,
                         RoadType, Speed, Points1, true) then
    begin
      ToLog('Separate. Error save Points1, ID= '+AID.ToString);
      Exit(False);
    end;
    if not SaveNewRoute1(AAccounts, AID.OSM_ID,
                         RoadType, Speed, Points2, true) then
    begin
      ToLog('Separate. Error save Points2, ID= '+AID.ToString);
      Exit(False);
    end;
  end else
  begin
    ToLog('Separate. Error delete, ID= '+AID.ToString);
    Exit(False);
  end;

end;

procedure TMapEditor.SetAccountBorder(const AAccountID: Integer;
  const ADateTime: TDateTime; const APoint1, APoint2: TGeoPos);
var
  Border: TChangeBorder;
begin
  if not Assigned(TGeneral.FAccountChangeBorder) then Exit;

  Border.LastChange := 0;

  if not TGeneral.FAccountChangeBorder.TryGetValue(AAccountID, Border) then
  begin
    Border.TopLeft.Latitude := Max(APoint1.Latitude,APoint2.Latitude);
    Border.TopLeft.Longitude := Min(APoint1.Longitude,APoint2.Longitude);
    Border.BottomRight.Latitude := Min(APoint1.Latitude,APoint2.Latitude);
    Border.BottomRight.Longitude := Max(APoint1.Longitude,APoint2.Longitude);
  end;

  if Border.LastChange = 0 then
    Border.LastChange := ADateTime;
  Border.TopLeft.Latitude := Max(Border.TopLeft.Latitude, Max(APoint1.Latitude,APoint1.Latitude));
  Border.TopLeft.Longitude := Min(Border.TopLeft.Longitude, Min(APoint1.Longitude,APoint1.Longitude));
  Border.BottomRight.Latitude := Min(Border.BottomRight.Latitude, Min(APoint1.Latitude,APoint1.Latitude));
  Border.BottomRight.Longitude := Max(Border.BottomRight.Longitude, Max(APoint1.Longitude,APoint1.Longitude));

  TGeneral.FAccountChangeBorder.AddOrSetValue(AAccountID, Border);
  if TGeneral.FDetailLog then
    ToLog('Request # '+ IntToStr(FRequestNo) + ' TGeneral.FAccountChangeBorder = '+IntToStr(TGeneral.FAccountChangeBorder.Count)+
        'Border.LastChange=' + DateTimeToStr(Border.LastChange));
end;

procedure TMapEditor.SetAccountBorderZone(const AAccountID: Integer;
  const ADateTime: TDateTime; const ABorder: string);
var
  Border: TChangeBorder;
  Points: TGeoPosArray;
  i: Integer;
begin
  TGeoHash.DecodeArrayWorld(ABorder, Points);

  if not TGeneral.FAccountChangeBorder.TryGetValue(AAccountID, Border) then
  begin
    Border.TopLeft.Latitude := 0;
    Border.TopLeft.Longitude := 180;
    Border.BottomRight.Latitude := 90;
    Border.BottomRight.Longitude := 0;
    Border.LastChange := 0;
  end;

  for i := Low(Points) to High(Points) do
  begin
    Border.TopLeft.Latitude := Max(Border.TopLeft.Latitude,Points[i].Latitude);
    Border.TopLeft.Longitude := Min(Border.TopLeft.Longitude,Points[i].Longitude);
    Border.BottomRight.Latitude := Min(Border.BottomRight.Latitude,Points[i].Latitude);
    Border.BottomRight.Longitude := Max(Border.BottomRight.Longitude,Points[i].Longitude);
  end;

  if Border.LastChange = 0 then
    Border.LastChange := ADateTime;

  TGeneral.FAccountChangeBorder.AddOrSetValue(AAccountID, Border);
end;

procedure TMapEditor.SetHoldRecDict(ADict: THoldRecDict);
begin
  FHoldRecDict := ADict;
end;

function TMapEditor.SetToString(ASet: TIntSet): string;
var
  i: Byte;
begin
  Result := '[';
  for I in ASet do
    Result := Result + IntToStr(i) + ',';
  Result[Length(Result)] := ']';
end;

function TMapEditor.UnloadAStar: boolean;
begin
  Result := FreeLibrary(HAStar64);
  if TGeneral.FDetailLog then
    ToLog('Free AStar Handle = '+ IntToStr(HAStar64) + ' Result=' + BoolToStr(Result, True));
end;

function TMapEditor.UpdateRoadType(const AAccounts: TIntegerDynArray;
const AOsmid, AHashStart, AHashEnd : Int64;
ARoadType, ASpeed: integer; AOneWay: Boolean; AName: string) : boolean;
var
  HashStartStr, HashEndStr: string;
  i, iFPos: Integer;
  bOneFileName, bFindRoute: boolean;
  Names: TDictionary<string,integer>;
begin
  Names := TDictionary<string,Integer>.Create;
  try
    HashStartStr := TGeoHash.ConvertBinToString(AHashStart and CAreaHashMask, 5);
    HashEndStr := TGeoHash.ConvertBinToString(AHashEnd and CAreaHashMask, 5);
    Names.AddOrSetValue(Copy(HashStartStr,1,3),0);
    Names.AddOrSetValue(Copy(HashEndStr,1,3),0);
    bOneFileName := HashStartStr = HashEndStr;
    if TGeneral.FDetailLog then
      ToLog('TMapEditor.UpdateRoadType. Account='+IntToStr(AAccounts[0])+' ID='+
            IntToStr(AOsmid)+'-'+IntToStr(AHashStart)+'-'+IntToStr(AHashEnd)+' hash='+
            HashStartStr+','+HashEndStr);

    iFPos := 0;
    if bOneFileName then
    begin
      FHoldRecDict.TryGetValue(HashStartStr, FStartEdgeFiles);
      bFindRoute := False;
      i := 0;
      while (i < Length(FStartEdgeFiles.EdgeForward)) and (not bFindRoute) do
      begin
        if (FStartEdgeFiles.EdgeForward[i].ID = AOsmID) and
           (FStartEdgeFiles.EdgeForward[i].HashVector.HashFrom = AHashStart) and
           (FStartEdgeFiles.EdgeForward[i].HashVector.HashTo = AHashEnd) then
        begin
          FStartEdgeFiles.EdgeForward[i].MaxSpeed := ASpeed;
          FStartEdgeFiles.EdgeForward[i].RoadType := ARoadType;
          iFPos := i;
          bFindRoute := True;
        end;
        inc(i);
      end;
      i := 0;
      bFindRoute := False;
      if not AOneWay then
      while (i < Length(FStartEdgeFiles.EdgeForward)) and (not bFindRoute) do
      begin
        if (FStartEdgeFiles.EdgeForward[i].ID = AOsmID) and
           (FStartEdgeFiles.EdgeForward[i].HashVector.HashFrom = AHashEnd) and
           (FStartEdgeFiles.EdgeForward[i].HashVector.HashTo = AHashStart) then
        begin
          FStartEdgeFiles.EdgeForward[i].MaxSpeed := ASpeed;
          FStartEdgeFiles.EdgeForward[i].RoadType := ARoadType;
          bFindRoute := True;
        end;
        inc(i);
      end;
      bFindRoute := False;
      i := 0;
      while (i < Length(FStartEdgeFiles.EdgeBackward)) and (not bFindRoute) do
      begin
        if (FStartEdgeFiles.EdgeBackward[i].ID = AOsmID) and
           (FStartEdgeFiles.EdgeBackward[i].HashVector.HashFrom = AHashEnd) and
           (FStartEdgeFiles.EdgeBackward[i].HashVector.HashTo = AHashStart) then
        begin
          FStartEdgeFiles.EdgeBackward[i].MaxSpeed := ASpeed;
          FStartEdgeFiles.EdgeBackward[i].RoadType := ARoadType;
          bFindRoute := True;
        end;
        inc(i);
      end;
      bFindRoute := False;
      i := 0;
      if (not AOneWay) then
      while (i < Length(FStartEdgeFiles.EdgeBackward)) and (not bFindRoute) do
      begin
        if (FStartEdgeFiles.EdgeBackward[i].ID = AOsmID) and
           (FStartEdgeFiles.EdgeBackward[i].HashVector.HashFrom = AHashStart) and
           (FStartEdgeFiles.EdgeBackward[i].HashVector.HashTo = AHashEnd) then
        begin
          FStartEdgeFiles.EdgeBackward[i].MaxSpeed := ASpeed;
          FStartEdgeFiles.EdgeBackward[i].RoadType := ARoadType;
          bFindRoute := True;
        end;
        inc(i);
      end;
      FHoldRecDict.AddOrSetValue(HashStartStr, FStartEdgeFiles);
      SetAccountBorder(AAccounts[0], Now(), FStartEdgeFiles.EdgeForward[iFPos].CoordsFrom, FStartEdgeFiles.EdgeForward[iFPos].CoordsTo);
      Result := SaveRouteNameCsv(AOsmID, AName, AAccounts, Copy(HashStartStr,1,3));
    end else
    begin
      FHoldRecDict.TryGetValue(HashStartStr, FStartEdgeFiles);
      FHoldRecDict.TryGetValue(HashEndStr, FEndEdgeFiles);

      bFindRoute := False;
      i := 0;
      while (i < Length(FStartEdgeFiles.EdgeForward)) and (not bFindRoute) do
      begin
        if (FStartEdgeFiles.EdgeForward[i].ID = AOsmid) and
           (FStartEdgeFiles.EdgeForward[i].HashVector.HashFrom = AHashStart) and
           (FStartEdgeFiles.EdgeForward[i].HashVector.HashTo = AHashEnd) then
        begin
          FStartEdgeFiles.EdgeForward[i].MaxSpeed := ASpeed;
          FStartEdgeFiles.EdgeForward[i].RoadType := ARoadType;
          bFindRoute := True;
          iFPos := i;
        end;
        inc(i);
      end;
      bFindRoute := False;
      i := 0;
      if (not AOneWay) then
      while (i < Length(FStartEdgeFiles.EdgeForward)) and (not bFindRoute) do
      begin
        if (FStartEdgeFiles.EdgeForward[i].ID = AOsmid) and
           (FStartEdgeFiles.EdgeForward[i].HashVector.HashFrom = AHashEnd) and
           (FStartEdgeFiles.EdgeForward[i].HashVector.HashTo = AHashStart) then
        begin
          FStartEdgeFiles.EdgeForward[i].MaxSpeed := ASpeed;
          FStartEdgeFiles.EdgeForward[i].RoadType := ARoadType;
          bFindRoute := True;
        end;
        inc(i);
      end;
      bFindRoute := False;
      i := 0;
      while (i < Length(FEndEdgeFiles.EdgeBackward)) and (not bFindRoute) do
      begin
        if (FEndEdgeFiles.EdgeBackward[i].ID = AOsmid) and
           (FEndEdgeFiles.EdgeBackward[i].HashVector.HashFrom = AHashEnd) and
           (FEndEdgeFiles.EdgeBackward[i].HashVector.HashTo = AHashStart) then
        begin
          FEndEdgeFiles.EdgeBackward[i].MaxSpeed := ASpeed;
          FEndEdgeFiles.EdgeBackward[i].RoadType := ARoadType;
          bFindRoute := True;
        end;
        Inc(i);
      end;
      bFindRoute := False;
      i := 0;
      if (not AOneWay) then
      while (i < Length(FEndEdgeFiles.EdgeBackward)) and (not bFindRoute) do
      begin
        if (FEndEdgeFiles.EdgeBackward[i].ID = AOsmid) and
           (FEndEdgeFiles.EdgeBackward[i].HashVector.HashFrom = AHashStart) and
           (FEndEdgeFiles.EdgeBackward[i].HashVector.HashTo = AHashEnd) then
        begin
          FEndEdgeFiles.EdgeBackward[i].MaxSpeed := ASpeed;
          FEndEdgeFiles.EdgeBackward[i].RoadType := ARoadType;
          bFindRoute := True;
        end;
        Inc(i);
      end;
      FHoldRecDict.AddOrSetValue(HashStartStr, FStartEdgeFiles);
      FHoldRecDict.AddOrSetValue(HashEndStr, FEndEdgeFiles);
      SetAccountBorder(AAccounts[0], Now(), FStartEdgeFiles.EdgeForward[iFPos].CoordsFrom, FStartEdgeFiles.EdgeForward[iFPos].CoordsTo);
      Result := SaveRouteNameCsv(AOsmID, AName, AAccounts, Copy(HashStartStr,1,3));
      Result := Result and SaveRouteNameCsv(AOsmID, AName, AAccounts, Copy(HashEndStr,1,3));
    end;
  finally
    Names.Free;
  end;
end;

function TMapEditor.UpdateRoadTypeWay(const AAccounts: TIntegerDynArray;
  const AOsmid, AHashStart,
  AHashEnd: Int64; ARoadType, ASpeed: integer; AOneWay: Boolean;  AName,
  AWay: string): Boolean;
var
  HashStartStr, HashEndStr: string;
  i, j, iPoints, iShiftPoints, iFPos, iBPos: Integer;
  bOneFileName, bFindRoute, bNeedShift: boolean;
  Points, RevPoints: TGeoPosArray;
  iPos, iWayPosF, iWayPosB: Int64;
  Names: TDictionary<string,integer>;
begin
  Names := TDictionary<string,Integer>.Create;
  try
    HashStartStr := TGeoHash.ConvertBinToString(AHashStart and CAreaHashMask, 5);
    HashEndStr := TGeoHash.ConvertBinToString(AHashEnd and CAreaHashMask, 5);
    Names.AddOrSetValue(Copy(HashStartStr,1,3),0);
    Names.AddOrSetValue(Copy(HashEndStr,1,3),0);
    bOneFileName := HashStartStr = HashEndStr;
    if TGeneral.FDetailLog then
      ToLog('TMapEditor.UpdateRoadTypeWay. Account='+IntToStr(AAccounts[0])+' ID='+
            IntToStr(AOsmid)+'-'+IntToStr(AHashStart)+'-'+IntToStr(AHashEnd)+' hash='+
            HashStartStr+','+HashEndStr+' way='+AWay);

    TGeoHash.DecodeArrayWorld(AWay, Points);
    RevPoints := ReverseWayArr(Points);
    iPoints := Length(Points);
    iFPos := 0;
    if bOneFileName then
    begin
      FHoldRecDict.TryGetValue(HashStartStr, FStartEdgeFiles);
      iShiftPoints := 0;
      iWayPosF := 0;
      iWayPosB := 0;
      //ищем позиции дороги  в EdgeFArr и EdgeBArr
      i := 0;
      bFindRoute := False;
      while (i < Length(FStartEdgeFiles.EdgeForward)) and not bFindRoute do
      begin
        if (FStartEdgeFiles.EdgeForward[i].ID = AOsmID) and
           (FStartEdgeFiles.EdgeForward[i].HashVector.HashFrom = AHashStart) and
           (FStartEdgeFiles.EdgeForward[i].HashVector.HashTo = AHashEnd) then
        begin
          bFindRoute := True;
          iFPos := i;
          if FStartEdgeFiles.EdgeForward[i].WayIndex <> -1 then
            iWayPosF := FStartEdgeFiles.EdgeForward[i].WayIndex;
        end else
        begin
          if FStartEdgeFiles.EdgeForward[i].WayIndex <> -1 then
            iWayPosF := FStartEdgeFiles.EdgeForward[i].WayIndex + FStartEdgeFiles.EdgeForward[i].WayCount;
          inc(i);
        end;
      end;

      i := 0;
      iBPos := 0;
      bFindRoute := False;
      while (i < Length(FStartEdgeFiles.EdgeBackward)) and not bFindRoute do
      begin
        if (FStartEdgeFiles.EdgeBackward[i].ID = AOsmID) and
           (FStartEdgeFiles.EdgeBackward[i].HashVector.HashFrom = AHashEnd) and
           (FStartEdgeFiles.EdgeBackward[i].HashVector.HashTo = AHashStart) then
        begin
          bFindRoute := True;
          iBPos := i;
          if FStartEdgeFiles.EdgeBackward[i].WayIndex <> -1 then
            iWayPosB := FStartEdgeFiles.EdgeBackward[i].WayIndex;
        end else
        begin
          if FStartEdgeFiles.EdgeBackward[i].WayIndex <> -1 then
            iWayPosB := FStartEdgeFiles.EdgeBackward[i].WayIndex + FStartEdgeFiles.EdgeBackward[i].WayCount;
          inc(i);
        end;
      end;
      //определяем нужно ли сдвигать и в какую сторону
      bNeedShift := (FStartEdgeFiles.EdgeForward[iFPos].WayCount <> (iPoints - 2));
      iShiftPoints := iShiftPoints + iPoints - FStartEdgeFiles.EdgeForward[i].WayCount - 2;

      if iPoints = 2 then
      begin
        FStartEdgeFiles.EdgeForward[iFPos].WayIndex := -1;
        FStartEdgeFiles.EdgeBackward[iBPos].WayIndex := -1;
      end else
      begin
        FStartEdgeFiles.EdgeForward[iFPos].WayCount := iPoints - 2;
        FStartEdgeFiles.EdgeBackward[iBPos].WayCount := iPoints - 2;
        FStartEdgeFiles.EdgeForward[iFPos].WayIndex := iWayPosF;
        FStartEdgeFiles.EdgeBackward[iBPos].WayIndex := iWayPosB;
      end;

       //добавляем промежуточные точки
      if iShiftPoints > 0 then
      begin
        for j := 1 to iShiftPoints do
          AddInWayPointArray(FStartEdgeFiles.Ways, iWayPosF, Points[j]);
      end;
       //удаляем промежуточные точки
      if iShiftPoints < 0 then
      begin
        for j := 1 to Abs(iShiftPoints) do
          DeleteWayPointArray(FStartEdgeFiles.Ways, iWayPosF);
      end;
      //изменияем промежуточные точки
      for j := 1 to Length(Points) - 2 do
        FStartEdgeFiles.Ways[iWayPosF+j-1] := Points[j];

      //если позиция одна, то больше промежуточных точек не добавляем, сдвиг одинаковый
      if iWayPosF = iWayPosB then
      begin
        i := iFPos + 1;
        while (i < Length(FStartEdgeFiles.EdgeForward)) and bNeedShift do
        begin
          if FStartEdgeFiles.EdgeForward[i].WayIndex <> -1 then
            FStartEdgeFiles.EdgeForward[i].WayIndex := FStartEdgeFiles.EdgeForward[i].WayIndex + iShiftPoints;
          inc(i);
        end;
        i := iBPos + 1;
        while (i < Length(FStartEdgeFiles.EdgeBackward)) and bNeedShift do
        begin
          if FStartEdgeFiles.EdgeBackward[i].WayIndex <> -1 then
            FStartEdgeFiles.EdgeBackward[i].WayIndex := FStartEdgeFiles.EdgeBackward[i].WayIndex + iShiftPoints;
          inc(i);
        end;
      end else
      //иначе добавляем еще промежуточных точек, сдвиг разный
      begin
        if iWayPosF > iWayPosB then
          ToLog('Ошибка смещения. Доработать алгоритм');

        i := iFPos + 1;
        while (i < Length(FStartEdgeFiles.EdgeForward)) and bNeedShift do
        begin
          if FStartEdgeFiles.EdgeForward[i].WayIndex <> -1 then
            FStartEdgeFiles.EdgeForward[i].WayIndex := FStartEdgeFiles.EdgeForward[i].WayIndex + iShiftPoints;
          inc(i);
        end;
        i := iBPos + 1;
        while (i < Length(FStartEdgeFiles.EdgeBackward)) and bNeedShift do
        begin
          if FStartEdgeFiles.EdgeBackward[i].WayIndex <> -1 then
            FStartEdgeFiles.EdgeBackward[i].WayIndex := FStartEdgeFiles.EdgeBackward[i].WayIndex + iShiftPoints;
          inc(i);
        end;
      end;
      FHoldRecDict.AddOrSetValue(HashStartStr, FStartEdgeFiles);
      SaveRouteNameCsv(AOsmID, AName, AAccounts, Copy(HashStartStr,1,3));
      SetAccountBorder(AAccounts[0], Now(), FStartEdgeFiles.EdgeForward[iFPos].CoordsFrom, FStartEdgeFiles.EdgeForward[iFPos].CoordsTo);
    end else
    begin
      FHoldRecDict.TryGetValue(HashStartStr, FStartEdgeFiles);
      FHoldRecDict.TryGetValue(HashEndStr, FEndEdgeFiles);

      bNeedShift := False;
      iShiftPoints := 0;
      for I := 0 to Length(FStartEdgeFiles.EdgeForward) - 1 do
      begin
        if (FStartEdgeFiles.EdgeForward[i].ID = AOsmID) and
           (FStartEdgeFiles.EdgeForward[i].HashVector.HashFrom = AHashStart) and
           (FStartEdgeFiles.EdgeForward[i].HashVector.HashTo = AHashEnd) then
        begin
          FStartEdgeFiles.EdgeForward[i].MaxSpeed := ASpeed;
          FStartEdgeFiles.EdgeForward[i].RoadType := ARoadType;
          if iPoints = 2 then
          begin
            FStartEdgeFiles.EdgeForward[i].WayIndex := -1;
          end;
          bNeedShift := (FStartEdgeFiles.EdgeForward[i].WayCount <> (iPoints - 2));
          iShiftPoints := iShiftPoints + iPoints - FStartEdgeFiles.EdgeForward[i].WayCount - 2;
          FStartEdgeFiles.EdgeForward[i].WayCount := iPoints - 2;
          iPos := IfThen(FStartEdgeFiles.EdgeForward[i].WayIndex=-1,0,FStartEdgeFiles.EdgeForward[i].WayIndex);
          if iShiftPoints > 0 then
          begin
            for j := 1 to iShiftPoints do
              AddInWayPointArray(FStartEdgeFiles.Ways, iPos, Points[j]);
          end;
          if iShiftPoints < 0 then
          begin
            for j := 1 to Abs(iShiftPoints) do
              DeleteWayPointArray(FStartEdgeFiles.Ways, iPos);
          end;
          for j := 1 to Length(Points) - 2 do
            FStartEdgeFiles.Ways[iPos+j-1] := Points[j];
        end else
        if bNeedShift then
        begin
          FStartEdgeFiles.EdgeForward[i].WayIndex := FStartEdgeFiles.EdgeForward[i].WayIndex + iShiftPoints;
          FStartEdgeFiles.EdgeBackward[i].WayIndex := FStartEdgeFiles.EdgeBackward[i].WayIndex + iShiftPoints;
        end;
      end;

      bNeedShift := False;
      iShiftPoints := 0;
      for I := 0 to Length(FEndEdgeFiles.EdgeBackward) - 1 do
      begin
        if (FEndEdgeFiles.EdgeBackward[i].ID = AOsmID) and
           (FEndEdgeFiles.EdgeBackward[i].HashVector.HashFrom = AHashEnd) and
           (FEndEdgeFiles.EdgeBackward[i].HashVector.HashTo = AHashStart) then
        begin
          FEndEdgeFiles.EdgeBackward[i].MaxSpeed := ASpeed;
          FEndEdgeFiles.EdgeBackward[i].RoadType := ARoadType;
          if iPoints = 2 then
          begin
            FEndEdgeFiles.EdgeBackward[i].WayIndex := -1;
          end;
          bNeedShift := (FEndEdgeFiles.EdgeBackward[i].WayCount <> (iPoints - 2));
          iShiftPoints := iShiftPoints + iPoints - FEndEdgeFiles.EdgeBackward[i].WayCount - 2;
          FEndEdgeFiles.EdgeBackward[i].WayCount := iPoints - 2;
          iPos := IfThen(FEndEdgeFiles.EdgeBackward[i].WayIndex=-1,0,FEndEdgeFiles.EdgeBackward[i].WayIndex);
          if iShiftPoints > 0 then
          begin
            for j := 1 to iShiftPoints do
              AddInWayPointArray(FEndEdgeFiles.Ways, iPos, Points[j]);
          end;
          if iShiftPoints < 0 then
          begin
            for j := 1 to Abs(iShiftPoints) do
              DeleteWayPointArray(FEndEdgeFiles.Ways, iPos);
          end;
          for j := 1 to Length(Points) - 2 do
            FStartEdgeFiles.Ways[iPos+j-1] := Points[j];
        end else
        if bNeedShift then
        begin
          FEndEdgeFiles.EdgeBackward[i].WayIndex := FEndEdgeFiles.EdgeBackward[i].WayIndex + iShiftPoints;
          FEndEdgeFiles.EdgeForward[i].WayIndex := FEndEdgeFiles.EdgeForward[i].WayIndex + iShiftPoints;
        end;
      end;

      if (not AOneWay) then
      begin
        Points := RevPoints;
        bNeedShift := False;
        iShiftPoints := 0;
        for I := 0 to Length(FEndEdgeFiles.EdgeForward) - 1 do
        begin
          if (FEndEdgeFiles.EdgeForward[i].ID = AOsmID) and
             (FEndEdgeFiles.EdgeForward[i].HashVector.HashFrom = AHashEnd) and
             (FEndEdgeFiles.EdgeForward[i].HashVector.HashTo = AHashStart) then
          begin
            FEndEdgeFiles.EdgeForward[i].MaxSpeed := ASpeed;
            FEndEdgeFiles.EdgeForward[i].RoadType := ARoadType;
            if iPoints = 2 then
            begin
              FEndEdgeFiles.EdgeForward[i].WayIndex := -1;
            end;
            bNeedShift := (FEndEdgeFiles.EdgeForward[i].WayCount <> (iPoints - 2));
            iShiftPoints := iShiftPoints + iPoints - FEndEdgeFiles.EdgeForward[i].WayCount - 2;
            FEndEdgeFiles.EdgeForward[i].WayCount := iPoints - 2;
            iPos := IfThen(FEndEdgeFiles.EdgeForward[i].WayIndex=-1,0,FEndEdgeFiles.EdgeForward[i].WayIndex);
            if iShiftPoints > 0 then
            begin
              for j := 1 to iShiftPoints do
                AddInWayPointArray(FEndEdgeFiles.Ways, iPos, Points[j]);
            end;
            if iShiftPoints < 0 then
            begin
              for j := 1 to Abs(iShiftPoints) do
                DeleteWayPointArray(FEndEdgeFiles.Ways, iPos);
            end;
            for j := 1 to Length(Points) - 2 do
              FEndEdgeFiles.Ways[iPos+j-1] := Points[j];
          end else
          if bNeedShift then
          begin
            FEndEdgeFiles.EdgeForward[i].WayIndex := FEndEdgeFiles.EdgeForward[i].WayIndex + iShiftPoints;
            FEndEdgeFiles.EdgeBackward[i].WayIndex := FEndEdgeFiles.EdgeBackward[i].WayIndex + iShiftPoints;
          end;
        end;

        bNeedShift := False;
        iShiftPoints := 0;
        for I := 0 to Length(FStartEdgeFiles.EdgeBackward) - 1 do
        begin
          if (FStartEdgeFiles.EdgeBackward[i].ID = AOsmID) and
             (FStartEdgeFiles.EdgeBackward[i].HashVector.HashFrom = AHashStart) and
             (FStartEdgeFiles.EdgeBackward[i].HashVector.HashTo = AHashEnd) then
          begin
            FStartEdgeFiles.EdgeBackward[i].MaxSpeed := ASpeed;
            FStartEdgeFiles.EdgeBackward[i].RoadType := ARoadType;
            if iPoints = 2 then
            begin
              FStartEdgeFiles.EdgeBackward[i].WayIndex := -1;
            end;
            bNeedShift := (FStartEdgeFiles.EdgeBackward[i].WayCount <> (iPoints - 2));
            iShiftPoints := iShiftPoints + iPoints - FStartEdgeFiles.EdgeBackward[i].WayCount - 2;

            FStartEdgeFiles.EdgeBackward[i].WayCount := iPoints - 2;
            iPos := IfThen(FStartEdgeFiles.EdgeBackward[i].WayIndex=-1,0,FStartEdgeFiles.EdgeBackward[i].WayIndex);
            if iShiftPoints > 0 then
            begin
              for j := 1 to iShiftPoints do
                AddInWayPointArray(FStartEdgeFiles.Ways, iPos, Points[j]);
            end;
            if iShiftPoints < 0 then
            begin
              for j := 1 to Abs(iShiftPoints) do
                DeleteWayPointArray(FStartEdgeFiles.Ways, iPos);
            end;
            for j := 1 to Length(Points) - 2 do
              FStartEdgeFiles.Ways[iPos+j-1] := Points[j];
          end else
          if bNeedShift then
          begin
            FStartEdgeFiles.EdgeBackward[i].WayIndex := FStartEdgeFiles.EdgeBackward[i].WayIndex + iShiftPoints;
            FStartEdgeFiles.EdgeForward[i].WayIndex := FStartEdgeFiles.EdgeForward[i].WayIndex + iShiftPoints;
          end;
        end;
      end;

      FHoldRecDict.AddOrSetValue(HashStartStr, FStartEdgeFiles);
      FHoldRecDict.AddOrSetValue(HashEndStr, FEndEdgeFiles);
      SaveRouteNameCsv(AOsmID, AName, AAccounts, Copy(HashStartStr,1,3));
      SaveRouteNameCsv(AOsmID, AName, AAccounts, Copy(HashEndStr,1,3));
      SetAccountBorder(AAccounts[0], Now(), FStartEdgeFiles.EdgeForward[iFPos].CoordsFrom, FStartEdgeFiles.EdgeForward[iFPos].CoordsTo);
    end;
    Result := True;
  finally
    Names.Free;
  end;
end;

procedure TMapEditor.Stop;
begin
  ToLog('MapEditor Manual Stop');
  FZC.Stop;
  FSign.Stop;
end;

function TMapEditor.UpdateRoutesMinFromArray: TJsonObject;
var
  i: Integer;
  bRes: Boolean;
  ARoutes: TDictionary<TIDLines,TRouteProperties>;
  ID: TIDLines;
begin
  bRes := True;
  for i := 0 to Length(FUpdateWayArr) - 1 do
    bRes := bRes and
              UpdateRoadType(FAccounts,
                             FUpdateMinArr[i].id.OSM_ID,
                             FUpdateMinArr[i].id.HashStart,
                             FUpdateMinArr[i].id.HashEnd,
                             FUpdateMinArr[i].RoadType,
                             FUpdateMinArr[i].Speed,
                             FUpdateMinArr[i].OneWay,
                             FUpdateMinArr[i].name);
  if bRes then
  begin
    ARoutes := TDictionary<TIDLines,TRouteProperties>.Create;
    try
      for i := 0 to Length(FUpdateMinArr) - 1 do
        begin
        ID.OSM_ID := FUpdateMinArr[i].id.OSM_ID;
        ID.HashStart := FUpdateMinArr[i].id.HashStart;
        ID.HashEnd := FUpdateMinArr[i].id.HashEnd;
        GetRoutesInfo(FAccounts, ID, TGeoHash.ConvertBinToString(ID.HashStart and CAreaHashMask, 5), TGeoHash.ConvertBinToString(ID.HashEnd and CAreaHashMask, 5), ARoutes);
      end;
    Result := GetRoutesJsonCsv(FAccounts, FOsmNameCsvDict.Keys.ToArray, ARoutes);
    finally
      ARoutes.Free;
    end;
  end else
    Result := TJsonObject(TJsonObject.Parse('{"Result":"Error"}'));
end;

function TMapEditor.UpdateRoutesWayFromArray(const AAccounts: TIntegerDynArray): TJsonObject;
var
  i, iCountMini, iCountWay: Integer;
  bRes: Boolean;
  ARoutes: TDictionary<TIDLines,TRouteProperties>;
  ID: TIDLines;
  RouteProperties: TRouteProperties;
  sWay: string;
  HashStartStr, HashEndStr: string;
  Holder: THoldRec;
  Names: TDictionary<string,Integer>;
  RoutePoints: TGeoPosArray;
  Areas: TLandMarkAreaDictionary;
  NeedSaveAreas: Boolean;
begin
  bRes := True;
  iCountMini := 0;
  iCountWay := 0;
  Areas := FLandMarkArea.LoadAreaFile(FRootPath, AAccounts);
  NeedSaveAreas := False;
  ARoutes := TDictionary<TIDLines,TRouteProperties>.Create;
  Names := TDictionary<string,Integer>.Create;
  try
  //загружаем все нужные файлы в словарь
    for i := 0 to Length(FUpdateWayArr) - 1 do
    begin
//      Application.ProcessMessages;
      if TGeneral.FStop then
      begin
        if TGeneral.FDetailLog then
          ToLog('TMapEditor.UpdateRoutesWayFromArray. Прервано пользователем');
        Exit(TJsonObject.Create);
      end;
      HashStartStr := TGeoHash.ConvertBinToString(FUpdateWayArr[i].id.HashStart and CAreaHashMask, 5);
      HashEndStr := TGeoHash.ConvertBinToString(FUpdateWayArr[i].id.HashEnd and CAreaHashMask, 5);
      Names.AddOrSetValue(Copy(HashStartStr,1,3), 0);
      Names.AddOrSetValue(Copy(HashStartStr,1,3),0);
      LoadAddrOSMCsv(AAccounts, Names.Keys.ToArray);

      if not FHoldRecDict.ContainsKey(HashStartStr) then
      begin
        TGFAdapter.Load(FRootPath, AAccounts, HashStartStr, Holder);
        FHoldRecDict.Add(HashStartStr, Holder);
      end;
      if not FHoldRecDict.ContainsKey(HashEndStr) then
      begin
        TGFAdapter.Load(FRootPath, AAccounts, HashEndStr, Holder);
        FHoldRecDict.Add(HashEndStr, Holder);
      end;
     //читаем информацию о дороге
      ID := FUpdateWayArr[i].id;
      GetRoutesInfo(AAccounts, ID, HashStartStr, HashEndStr, ARoutes);
      ID.HashStart := FUpdateWayArr[i].id.HashEnd;
      ID.HashEnd := FUpdateWayArr[i].id.HashStart;
      sWay := FUpdateWayArr[i].Way;
      SetLength(RoutePoints, 0);
//сверяем входные данные и решаем что делать дальше
      if ARoutes.TryGetValue(FUpdateWayArr[i].id, RouteProperties) then
      begin
//если из 2-сторонней стала 1-сторонней, то удаляем реверсную
        if (RouteProperties.OneWay = 0) and (FUpdateWayArr[i].OneWay = 1)  then
        begin
          if DeleteRoute(FAccounts, ID, RoutePoints) then
          begin
            RouteProperties.OneWay := 1;
            ARoutes.AddOrSetValue(FUpdateWayArr[i].id, RouteProperties);
            sWay := FUpdateWayArr[i].way;
          end;
        end else
//если из 2-сторонней стала 1-сторонней и OneWay = -1, то удаляем текущую
        if (RouteProperties.OneWay = 0) and (FUpdateWayArr[i].OneWay = -1)  then
        begin
          if DeleteRoute(FAccounts, FUpdateWayArr[i].id, RoutePoints) then
          begin
            ARoutes.Remove(FUpdateWayArr[i].id);
            GetRoutesInfo(AAccounts, ID, TGeoHash.ConvertBinToString(ID.HashStart and CAreaHashMask, 5), TGeoHash.ConvertBinToString(ID.HashEnd and CAreaHashMask, 5), ARoutes);
            ARoutes.TryGetValue(id, RouteProperties);
            FUpdateWayArr[i].id := ID;
            sWay := ReverseWayString(FUpdateWayArr[i].Way);
          end;
        end else
//если из 1-сторонней стала 2-сторонней
        if (RouteProperties.OneWay = 1) and (FUpdateWayArr[i].OneWay = 0)  then
        begin
          if SaveReverseRoute(FAccounts, FUpdateWayArr[i].id) then
          begin
            RouteProperties.OneWay := 0;
            ARoutes.AddOrSetValue(FUpdateWayArr[i].id, RouteProperties);
            sWay := FUpdateWayArr[i].Way;
          end;
        end else
//если из 1-сторонней стала реверсивной
        if (RouteProperties.OneWay = 1) and (FUpdateWayArr[i].OneWay = -1)  then
        begin
          if SaveReverseRoute(FAccounts, FUpdateWayArr[i].id) then
          if DeleteRoute(FAccounts, FUpdateWayArr[i].id, RoutePoints) then
          begin
            ARoutes.Remove(FUpdateWayArr[i].id);
            GetRoutesInfo(AAccounts, ID, TGeoHash.ConvertBinToString(ID.HashStart and CAreaHashMask, 5), TGeoHash.ConvertBinToString(ID.HashEnd and CAreaHashMask, 5), ARoutes);
            ARoutes.TryGetValue(id, RouteProperties);
//            ARoutes.AddOrSetValue(id, RouteProperties);
            FUpdateWayArr[i].id := ID;
            sWay := ReverseWayString(FUpdateWayArr[i].Way);
          end;
        end;

//если геометрия не изменилась
        if (RouteProperties.sWay = sway) then
        begin
          if FUpdateWayArr[i].OneWay = -1 then
          begin
            bRes := bRes and UpdateRoadType(AAccounts,
                               id.OSM_ID,
                               id.HashStart,
                               id.HashEnd,
                               FUpdateWayArr[i].RoadType,
                               FUpdateWayArr[i].Speed,
                               RouteProperties.OneWay <> 0,
                               FUpdateWayArr[i].name);
            RouteProperties.RoadType := FUpdateWayArr[i].RoadType;
            RouteProperties.Speed := FUpdateWayArr[i].Speed;
            RouteProperties.OneWay := RouteProperties.OneWay;
            ARoutes.AddOrSetValue(id, RouteProperties);
          end else
          begin
            bRes := bRes and UpdateRoadType(AAccounts,
                               FUpdateWayArr[i].id.OSM_ID,
                               FUpdateWayArr[i].id.HashStart,
                               FUpdateWayArr[i].id.HashEnd,
                               FUpdateWayArr[i].RoadType,
                               FUpdateWayArr[i].Speed,
                               FUpdateWayArr[i].OneWay <> 0,
                               FUpdateWayArr[i].name);

            RouteProperties.RoadType := FUpdateWayArr[i].RoadType;
            RouteProperties.Speed := FUpdateWayArr[i].Speed;
            RouteProperties.OneWay := FUpdateWayArr[i].OneWay;
            ARoutes.AddOrSetValue(FUpdateWayArr[i].id, RouteProperties);
          end;
          Inc(iCountMini);
        end else
        begin
          if FUpdateWayArr[i].OneWay = -1 then
          begin
            bRes := bRes and  UpdateRoadTypeWay(AAccounts,
                               id.OSM_ID,
                               id.HashStart,
                               id.HashEnd,
                               FUpdateWayArr[i].RoadType,
                               FUpdateWayArr[i].Speed,
                               RouteProperties.OneWay <> 0,
                               FUpdateWayArr[i].name,
                               FUpdateWayArr[i].way);
            RouteProperties.RoadType := FUpdateWayArr[i].RoadType;
            RouteProperties.Speed := FUpdateWayArr[i].Speed;
            RouteProperties.OneWay := RouteProperties.OneWay;
            RouteProperties.sWay := FUpdateWayArr[i].way;
            ARoutes.AddOrSetValue(id, RouteProperties);
          end else
          begin
            bRes := bRes and  UpdateRoadTypeWay(AAccounts,
                               FUpdateWayArr[i].id.OSM_ID,
                               FUpdateWayArr[i].id.HashStart,
                               FUpdateWayArr[i].id.HashEnd,
                               FUpdateWayArr[i].RoadType,
                               FUpdateWayArr[i].Speed,
                               FUpdateWayArr[i].OneWay <> 0,
                               FUpdateWayArr[i].name,
                               FUpdateWayArr[i].way);
            RouteProperties.RoadType := FUpdateWayArr[i].RoadType;
            RouteProperties.Speed := FUpdateWayArr[i].Speed;
            RouteProperties.OneWay := FUpdateWayArr[i].OneWay;
            RouteProperties.sWay := FUpdateWayArr[i].way;
            ARoutes.AddOrSetValue(FUpdateWayArr[i].id, RouteProperties);
          end;
            Inc(iCountWay);
        end;
      end;
      NeedSaveAreas := NeedSaveAreas or
                       FLandMarkArea.CheckRouteInTrack(FRootPath, AAccounts, RoutePoints, Areas, TGeneral.FDetailLog);
    end;//for
  //сохраняем все файлы
  SaveAllFiles(AAccounts);
  SaveAllNameCsv(AAccounts, FOsmNameCsvDict.Keys.ToArray);
  if NeedSaveAreas then
    FLandMarkArea.SaveAreas(FRootPath, AAccounts[0], Areas, True, TGeneral.FDetailLog);

    if bRes then
    begin
      if TGeneral.FDetailLog then
        ToLog('Mini='+IntToStr(iCountMini)+' Way='+IntToStr(iCountWay)+' All='+IntToStr(ARoutes.Count));
      Result := GetRoutesJsonCsv(AAccounts, FOsmNameCsvDict.Keys.ToArray, ARoutes);
    end else
      Result := TJsonObject(TJsonObject.Parse('{"Result":"Error"}'));
  finally
    ARoutes.Free;
    Names.Free;
    FHoldRecDict.Clear;
    Areas.Free;
  end;
end;

function TMapEditor.GetRouteLengthKm(const APoints: TGeoPosArray): Double;
var
  i: Integer;
  Km: Double;
begin
  Km := 0;
  for I := 0 to Length(APoints) - 2 do
    Km := Km + TGeoCalcs.GeoLengthKmDeg(APoints[i],APoints[i+1]);
  Result := Km;
end;


function TMapEditor.GetRoutesInfo(const AAccounts: TIntegerDynArray;
                                        const AID: TIDLines;
                                        const AHashStartStr, AHashEndStr: string;
                                        var ARouteInfo: TDictionary<TIDLines, TRouteProperties>;
                                        OnlyOne: Boolean): integer;
var
  i,j: Integer;
  bFindRoute, bFindRoute2, bOneWay, bOneFileName: boolean;
  RouteProperties: TRouteProperties;
  AWay: TGeoPathPointArray;
  RevID: TIDLines;
begin
  bOneFileName := AHashStartStr = AHashEndStr;
  bOneWay := True;
  RevID.OSM_ID := AID.OSM_ID;
  RevID.HashStart := AID.HashEnd;
  RevID.HashEnd := AID.HashStart;
  if bOneFileName then
  begin
    FHoldRecDict.TryGetValue(AHashStartStr, FStartEdgeFiles);
    bFindRoute := False;
    i := 0;
    //проходим весь массив, если находим только в одну сторону, то односторонняя
    while (i < Length(FStartEdgeFiles.EdgeForward)) do
    begin
      //ищем обратную дорогу
      if (not OnlyOne) and
         ((FStartEdgeFiles.EdgeForward[i].ID = AID.OSM_ID) and
         (FStartEdgeFiles.EdgeForward[i].HashVector.HashFrom = AID.HashEnd) and
         (FStartEdgeFiles.EdgeForward[i].HashVector.HashTo = AID.HashStart)) then
         bOneWay := False;

      if (FStartEdgeFiles.EdgeForward[i].ID = AID.OSM_ID) and
         (FStartEdgeFiles.EdgeForward[i].HashVector.HashFrom = AID.HashStart) and
         (FStartEdgeFiles.EdgeForward[i].HashVector.HashTo = AID.HashEnd) then
      begin
        RouteProperties.RoadType := FStartEdgeFiles.EdgeForward[i].RoadType;
        RouteProperties.Speed := FStartEdgeFiles.EdgeForward[i].MaxSpeed;
        RouteProperties.Zone := 0;
        if Length(FStartEdgeFiles.EdgeForward) = Length(FStartEdgeFiles.SCForward) then
          RouteProperties.Sign := FStartEdgeFiles.SCForward[i]
        else
          RouteProperties.Sign := 0;

        SetLength(AWay, FStartEdgeFiles.EdgeForward[i].WayCount + 2);
        if FStartEdgeFiles.EdgeForward[i].WayCount = 0 then
        begin
          AWay[0].p := FStartEdgeFiles.EdgeForward[i].CoordsFrom;
          AWay[1].p := FStartEdgeFiles.EdgeForward[i].CoordsTo;
        end else
        begin
          AWay[0].p := FStartEdgeFiles.EdgeForward[i].CoordsFrom;
          AWay[FStartEdgeFiles.EdgeForward[i].WayCount + 1].p := FStartEdgeFiles.EdgeForward[i].CoordsTo;
          for j := 1 to FStartEdgeFiles.EdgeForward[i].WayCount do
            AWay[j].p := FStartEdgeFiles.Ways[FStartEdgeFiles.EdgeForward[i].WayIndex + j - 1];
        end;
        RouteProperties.sWay := TGeoHash.EncodeArrayAnyFormat(AWay, 12, gfPath);
        bFindRoute := True;
      end;
      inc(i);
    end;
    if bFindRoute then
    begin
      RouteProperties.OneWay := IfThen(bOneWay, 1, 0);
      ARouteInfo.AddOrSetValue(AID, RouteProperties);
    end;
  end else
  begin
    if not FHoldRecDict.TryGetValue(AHashStartStr, FStartEdgeFiles) then
    begin
      TGFAdapter.Load(FRootPath, AAccounts, AHashStartStr, FStartEdgeFiles);
      FHoldRecDict.Add(AHashStartStr, FStartEdgeFiles);
    end;
    if not FHoldRecDict.TryGetValue(AHashEndStr, FEndEdgeFiles) then
    begin
      TGFAdapter.Load(FRootPath, AAccounts, AHashEndStr, FEndEdgeFiles);
      FHoldRecDict.Add(AHashEndStr, FEndEdgeFiles);
    end;

    bFindRoute := False;
    i := 0;
    //ищем в одном файле прямую, а в другом обратную
    while (i < Length(FStartEdgeFiles.EdgeForward))
          and (not bFindRoute) do
    begin
      if (FStartEdgeFiles.EdgeForward[i].ID = AID.OSM_ID) and
         (FStartEdgeFiles.EdgeForward[i].HashVector.HashFrom = AID.HashStart) and
         (FStartEdgeFiles.EdgeForward[i].HashVector.HashTo = AID.HashEnd) then
      begin
        RouteProperties.RoadType := FStartEdgeFiles.EdgeForward[i].RoadType;
        RouteProperties.Speed := FStartEdgeFiles.EdgeForward[i].MaxSpeed;
        RouteProperties.Zone := 0;

        SetLength(AWay, FStartEdgeFiles.EdgeForward[i].WayCount + 2);
        if FStartEdgeFiles.EdgeForward[i].WayCount = 0 then
        begin
          AWay[0].p := FStartEdgeFiles.EdgeForward[i].CoordsFrom;
          AWay[1].p := FStartEdgeFiles.EdgeForward[i].CoordsTo;
        end else
        begin
          AWay[0].p := FStartEdgeFiles.EdgeForward[i].CoordsFrom;
          AWay[FStartEdgeFiles.EdgeForward[i].WayCount + 1].p := FStartEdgeFiles.EdgeForward[i].CoordsTo;
          for j := 1 to FStartEdgeFiles.EdgeForward[i].WayCount do
            AWay[j].p := FStartEdgeFiles.Ways[FStartEdgeFiles.EdgeForward[i].WayIndex + j - 1];
        end;
        RouteProperties.sWay := TGeoHash.EncodeArrayAnyFormat(AWay, 12, gfPath);
        bFindRoute := True;
      end;
      inc(i);
    end;

    bFindRoute2 := False;
    i := 0;
    while (i < Length(FEndEdgeFiles.EdgeForward))
          and (not bFindRoute2) do
    begin
      if (FEndEdgeFiles.EdgeForward[i].ID = AID.OSM_ID) and
         (FEndEdgeFiles.EdgeForward[i].HashVector.HashFrom = AID.HashEnd) and
         (FEndEdgeFiles.EdgeForward[i].HashVector.HashTo = AID.HashStart) then
      begin
        bFindRoute2 := True;
        bOneWay := False;
      end;
      inc(i);
    end;
    if bFindRoute then
    begin
      RouteProperties.OneWay := IfThen(bOneWay, 1, 0);
      ARouteInfo.AddOrSetValue(AID, RouteProperties);
    end;
  end;

  Result := ARouteInfo.Count;
end;

function TMapEditor.GetRoutesJson(
  ARouteInfo: TDictionary<TIDLines, TRouteProperties>): TJsonObject;
var
  RouteID: TIDLines;
  RouteProperties: TRouteProperties;
  sID : string;
  Idx: Int64;
begin
//  LoadAddrOSM;
  Result := TJsonObject.Create;
  if ARouteInfo.Count = 0 then
  begin
    if TGeneral.FDetailLog then
      ToLog('GetRoutesJson. Ни одна дорога не найдена');
    Result.S['errors'] := 'Roads not found';
  end else
  begin
    for RouteID in ARouteInfo.Keys do
    begin
      ARouteInfo.TryGetValue(RouteID,RouteProperties);
      sID := IntToStr(RouteID.OSM_ID)+'-'+IntToStr(RouteID.HashStart)+'-'+IntToStr(RouteID.HashEnd);
      Result.S['Type'] := 'FeatureCollection';
      with Result.A['Features'].AddObject do
      begin
        S['Type'] := 'Feature';
        O['Properties'].S['ID'] := sID;
        O['Properties'].S['Speed'] := IntToStr(RouteProperties.Speed);
        O['Properties'].S['RoadType'] := IntToStr(RouteProperties.RoadType);
        O['Properties'].S['OneWay'] := IntToStr(RouteProperties.OneWay);
        O['Properties'].S['SignMask'] := IntToStr(RouteProperties.Sign);

        if FAddrOsmDict.TryGetValue(RouteID.OSM_ID, Idx) then
          O['Properties'].S['Name'] := GetRouteName(Idx)
        else
          O['Properties'].S['Name'] := '';
        O['Geometry'].S['Type'] := 'LineString';
        O['Geometry'].A['Coordinates'].Add(RouteProperties.sWay);
      end;
    end;
  end;
end;

function TMapEditor.GetRoutesJsonCsv(const AAccounts: TIntegerDynArray;
  AHashArr: TWayArray;
  ARouteInfo: TDictionary<TIDLines, TRouteProperties>): TJsonObject;
var
  RouteID: TIDLines;
  RouteProperties: TRouteProperties;
  sID : string;
  sName: string;
begin
  Result := TJsonObject.Create;
  if ARouteInfo.Count = 0 then
  begin
    if TGeneral.FDetailLog then
      ToLog('GetRoutesJsonCsv. Ни одна дорога не найдена');
    Result.S['errors'] := 'Roads not found';
  end else
  begin
    for RouteID in ARouteInfo.Keys do
    begin
      ARouteInfo.TryGetValue(RouteID,RouteProperties);
      sID := IntToStr(RouteID.OSM_ID)+'-'+IntToStr(RouteID.HashStart)+'-'+IntToStr(RouteID.HashEnd);
      Result.S['Type'] := 'FeatureCollection';
      with Result.A['Features'].AddObject do
      begin
        S['Type'] := 'Feature';
        O['Properties'].S['ID'] := sID;
        O['Properties'].S['Speed'] := IntToStr(RouteProperties.Speed);
        O['Properties'].S['RoadType'] := IntToStr(RouteProperties.RoadType);
        O['Properties'].S['OneWay'] := IntToStr(RouteProperties.OneWay);
        O['Properties'].S['SignMask'] := IntToStr(RouteProperties.Sign);
        sName := GetRouteNameByOSMCsv(RouteID.OSM_ID,AHashArr,AAccounts);
        O['Properties'].S['Name'] := sName;
        O['Geometry'].S['Type'] := 'LineString';
        O['Geometry'].A['Coordinates'].Add(RouteProperties.sWay);
      end;
    end;
  end;
end;

function TMapEditor.GetToArray(AHash: Int64; AEdgeArray: TEdgeArray;
  NotID:Integer; var AToArray: THashVectorArray): integer;
var
  i: Integer;
begin
  SetLength(AToArray, 0);
  for I := 0 to Length(AEdgeArray) - 1 do
    if (AEdgeArray[i].HashVector.HashFrom = AHash) and (I <> NotID) then
    begin
      SetLength(AToArray, Length(AToArray) + 1);
      AToArray[Length(AToArray)-1].HashFrom := AEdgeArray[i].HashVector.HashFrom;
      AToArray[Length(AToArray)-1].HashTo := AEdgeArray[i].HashVector.HashTo;
    end;
  Result := Length(AToArray);
end;


function TMapEditor.GetRoutesFromRangeByType(const AAccounts: TIntegerDynArray;
                                            const ATopLeftLa, ATopLeftLo, ABottomRightLa, ABottomRightLo : double;
                                            const ARoadTypes : TIntSet; AJoin: integer) : TJsonObject;
var
  RouteDict: TDictionary<TIDLines,TRouteProperties>;
  HashArr: TWayArray;
begin

  if TGeneral.FDetailLog then
    ToLog('TMapEditor.GetRoutesFromRangeByType.');
  RouteDict := GetRoutesFromRangeNew(AAccounts, ATopLeftLa, ATopLeftLo, ABottomRightLa, ABottomRightLo, ARoadTypes, AJoin);
  try
    HashArr := FOsmNameHashDict.Keys.ToArray;
    ToLog('Request # ' + IntToStr(FRequestNo) + ' TMapEditor.GetRoutesFromRangeByType. Route count='+IntToStr(RouteDict.Count));
    Result := GetRoutesJsonCsv(AAccounts, HashArr, RouteDict);
  finally
    RouteDict.Free;
  end;
end;

function TMapEditor.GetRoutesFromRangeNew(
  const AAccounts: TIntegerDynArray; const ATopLeftLa, ATopLeftLo,
  ABottomRightLa, ABottomRightLo: double; const ARoadTypes: TIntSet;
  AJoin: integer): TDictionary<TIDLines, TRouteProperties>;
var
  WayArray: TGeoPathPointArray;
  sWay: string;
  i, j, WayArrayCount: Integer;
  RouteID: TIDLines;
  RouteProperties: TRouteProperties;
  //список районов, которые входят в заданный прямоугольник
  AreaArray : TFileNameArray;
  sHashEnd : string;
  sHash: string;
  it1, it2, it3, iCheckCount, iAddCount: Cardinal;
  RevID: TIDLines;
  bNeedAdd, bFound: boolean;
  Holder: THoldRec;
  bOneWay: Byte;
  SpeedF, SpeedB: TSpeedControl;
  iSign: UInt64;
begin
  it1 := GetTickCount;
  Result := TDictionary<TIDLines,TRouteProperties>.Create;
  FOsmNameHashDict := TDictionary<string,integer>.Create;

  AreaArray := FZC.GetFilesNameArray(TGeoPos.Create(ATopLeftLa, ATopLeftLo), TGeoPos.Create(ABottomRightLa, ABottomRightLo));
  it2 := GetTickCount;
  if TGeneral.FDetailLog then
    ToLog('Get files array. Count='+IntToStr(Length(AreaArray))+' ticks='+IntToStr(it2-it1));

  iCheckCount := 0;
  iAddCount := 0;
  try
    for sHash in AreaArray do
    begin
      FOsmNameHashDict.AddOrSetValue(Copy(sHash,1,3), 0);
      if not FHoldRecDict.ContainsKey(sHash) then
      begin
        TGFAdapter.LoadFileType(FRootPath, AAccounts, sHash, Holder, [ftEdgeF, ftEdgeB, ftWay, ftSpeed]);
        FHoldRecDict.AddOrSetValue(sHash, Holder);
      end;
    end;
    it1 := GetTickCount;
    for sHash in FHoldRecDict.Keys do
    begin
      if FHoldRecDict.TryGetValue(sHash, Holder) then
      for I := Low(Holder.EdgeForward) to High(Holder.EdgeForward) do
      begin
        if TGeneral.FStop then
        begin
          ToLog('TMapEditor.GetRoutesFromRangeNew. Прервано пользователем');
          Exit;
        end;
        if (Holder.EdgeForward[i].RoadType in ARoadTypes) and
           (((Holder.EdgeForward[i].CoordsFrom.Latitude <= ATopLeftLa) and
           (Holder.EdgeForward[i].CoordsFrom.Longitude >= ATopLeftLo) and
           (Holder.EdgeForward[i].CoordsFrom.Latitude >= ABottomRightLa) and
           (Holder.EdgeForward[i].CoordsFrom.Longitude <= ABottomRightLo)) or
           ((Holder.EdgeForward[i].CoordsTo.Latitude <= ATopLeftLa) and
           (Holder.EdgeForward[i].CoordsTo.Longitude >= ATopLeftLo) and
           (Holder.EdgeForward[i].CoordsTo.Latitude >= ABottomRightLa) and
           (Holder.EdgeForward[i].CoordsTo.Longitude <= ABottomRightLo)))
            then
        begin
          bNeedAdd := True;
          it3 := GetTickCount;
          RouteID.OSM_ID := Holder.EdgeForward[i].ID;
          RouteID.HashStart := Holder.EdgeForward[i].HashVector.HashFrom;
          RouteID.HashEnd := Holder.EdgeForward[i].HashVector.HashTo;
          RevID.OSM_ID := Holder.EdgeForward[i].ID;
          RevID.HashStart := Holder.EdgeForward[i].HashVector.HashTo;
          RevID.HashEnd := Holder.EdgeForward[i].HashVector.HashFrom;
          bOneWay := 1;
          if TGeneral.FDetailLog then
            ToLog('bNeedAdd ID='+RouteID.ToString);

          if Length(Holder.SCForward) = Length(Holder.EdgeForward) then
          begin
            iSign := Holder.SCForward[i];
          end else
            iSign := 0;
          if Length(Holder.Speeds) = Length(Holder.EdgeForward) then
          begin
            SpeedF := Holder.Speeds[i];
          end else
            FillChar(SpeedF, SizeOf(TSpeedControl), Holder.EdgeForward[i].MaxSpeed);

          sHashEnd := TGeoHash.ConvertBinToString(RouteID.HashEnd and CAreaHashMask, 5);
          if not FHoldRecDict.ContainsKey(sHashEnd) then
          begin
            j := 0;
            bFound := False;
            while (j < Length(Holder.EdgeBackward)) and (not bFound) do
            begin
              if (Holder.EdgeBackward[j].ID = RouteID.OSM_ID) and
                 (Holder.EdgeBackward[j].HashVector.HashFrom = RouteID.HashStart) and
                 (Holder.EdgeBackward[j].HashVector.HashTo = RouteID.HashEnd) then
              begin
                bFound := True;
                bOneWay := 0;
                if Length(Holder.SCBackward) = Length(Holder.EdgeBackward) then
                begin
                  iSign := Holder.SCBackward[j];
                end else
                  iSign := 0;

                RouteProperties.Sign := iSign;
                RouteProperties.AvgSpeedForward := SpeedF;
                RouteProperties.AvgSpeedBackward := SpeedB;
                Result.AddOrSetValue(RouteID,RouteProperties);
              end;
              inc(j);
            end;
          end;

          if Result.TryGetValue(RevID,RouteProperties) then
          begin
            bNeedAdd := False;
            RouteProperties.OneWay := 0;
            if Length(Holder.SCForward) = Length(Holder.EdgeForward) then
            begin
              iSign := Holder.SCForward[i];
            end else
              iSign := 0;
            RouteProperties.Sign := iSign;
            Result.AddOrSetValue(RevID,RouteProperties);
          end;
          iCheckCount := iCheckCount + GetTickCount - it3;

          if bNeedAdd then
          begin
            WayArrayCount := Holder.EdgeForward[i].WayCount;
            SetLength(WayArray, WayArrayCount + 2);
            if WayArrayCount = 0 then
            begin
              WayArray[0].p := Holder.EdgeForward[i].CoordsFrom;
              WayArray[1].p := Holder.EdgeForward[i].CoordsTo;
            end else
            begin
              WayArray[0].p := Holder.EdgeForward[i].CoordsFrom;
              WayArray[WayArrayCount + 1].p := Holder.EdgeForward[i].CoordsTo;
              for j := 1 to WayArrayCount do
              begin
                WayArray[j].p := Holder.Ways[Holder.EdgeForward[i].WayIndex+j-1];
              end;
            end;

            if Length(WayArray) > 0 then
            begin
              sWay := TGeoHash.EncodeArrayAnyFormat(WayArray, 12, gfPath);
            end else
              sWay := '';
            RouteID.OSM_ID := Holder.EdgeForward[i].ID;
            RouteID.HashStart := Holder.EdgeForward[i].HashVector.HashFrom;
            RouteID.HashEnd := Holder.EdgeForward[i].HashVector.HashTo;
            RouteProperties.RoadType := Holder.EdgeForward[i].RoadType;
            RouteProperties.Speed := Holder.EdgeForward[i].MaxSpeed;
            RouteProperties.Zone := 0;
            RouteProperties.sWay := sWay;
            RouteProperties.OneWay := bOneWay;
            RouteProperties.Sign := iSign;
            it3 := GetTickCount;
            Result.AddOrSetValue(RouteID, RouteProperties);
            iAddCount := iAddCount + GetTickCount - it3;
          end;
        end else //if in the border
        begin
          if Length(Holder.SCForward) = Length(Holder.EdgeForward) then
          begin
            RouteProperties.Sign := Holder.SCForward[i];
          end else
            RouteProperties.Sign := 0;
        end;
      end;
    end;
    it2 := GetTickCount;
    if TGeneral.FDetailLog then
      ToLog('Result.Add Count='+IntToStr(Result.Count)+' ticks='+IntToStr(it2-it1));

  finally
    SetLength(WayArray, 0);
    SetLength(AreaArray, 0);
  end;
end;

procedure TMapEditor.AddInListArray(var AListArr: THashVectorArray;
  APos: Int64; APoint: THashVector);
var
  Len: integer;
begin
  Len := Length( AListArr );
  setLength( AListArr, Len + 1);
  if aPos = Len then
  begin
    AListArr[APos] := APoint;
    Exit;
  end;
  if aPos > Len then
    APos := Len + 1;
  move( AListArr[APos], AListArr[APos + 1],
       (Len-APos) * sizeof( AListArr[APos]));
  AListArr[APos] := APoint;
end;

procedure TMapEditor.AddInWayPointArray(var AWayArr: TWayPointArray; APos: Int64; APoint: TGeoPos);
var
  Len: integer;
begin
  Len := Length( AWayArr );
  setLength( AWayArr, Len + 1);
  if aPos = Len then
  begin
    AWayArr[APos] := APoint;
    Exit;
  end;
  if aPos >= Len then
    APos := Len + 1;
  move( AWayArr[APos], AWayArr[APos + 1],
       (Len-APos) * sizeof( AWayArr[APos]));
  AWayArr[APos] := APoint;
end;

function TMapEditor.GetRouteName(const aIdx: Int64;
  const aLog: TStringList): string;
begin
  Result := '';
  if not Assigned(FAddressDataFile) then
  begin
    FAddressDataFileName := ExtractFileDir(GetModuleName(HInstance)) + '\osm.dat';
    FAddressDataFile := TFileStream.Create(FAddressDataFileName, fmOpenRead + fmShareDenyNone);
    FAddressStreamReader := TStreamReader.Create(FAddressDataFile, TEncoding.UTF8, True);
  end;

  if aIdx < FAddressDataFile.Size then
  begin
    FAddressStreamReader.DiscardBufferedData;
    FAddressDataFile.Position := aIdx;
    Result := Trim(FAddressStreamReader.ReadLine);
  end;
end;

function TMapEditor.GetRouteNameByOSM(const OsmID: Int64;
  const aLog: TStringList): string;
var
  Idx: Int64;
begin
  LoadAddrOSM;
  if FAddrOsmDict.TryGetValue(OsmID, Idx) then
    Result := GetRouteName(Idx)
  else
    Result := '';
end;

function TMapEditor.GetRouteNameByOSMCsv(const AOsmID: Int64;
  AHashArr: TWayArray; AAccounts: TIntegerDynArray; const aLog: TStringList): string;
var
  sHash, sName: string;
  Dict: TOsmNameDict;
begin
  Result := '';
  LoadAddrOSMCsv(AAccounts, AHashArr);
  if FOsmNameCsvDict.Count = 0 then
    Exit('');
  for sHash in FOsmNameCsvDict.Keys do
  begin
    if FOsmNameCsvDict.TryGetValue(sHash, Dict) then
    begin
      if (Dict.Count > 0) and Dict.TryGetValue(AOsmID, sName) then
      begin
        Result := sName;
        Exit;
      end;
    end;
  end;
end;

function TMapEditor.GetRouteNameByOSMHash(const OsmID: Int64; AHashArr: TWayArray;
  AAccounts: TIntegerDynArray; const aLog: TStringList): string;
begin
  LoadAddrOSMHash(AAccounts, AHashArr);
  if not FOsmNameDict.TryGetValue(OsmID, Result) then
    Result := '';
end;


function TMapEditor.GetRoutePointNum(APoint: TGeoPos;
  APointsArr: TGeoPosArray; ARadius: integer; var APointOnLine: TGeoPos): Integer;
var
  i: Integer;
  Distance, MinDistance: Double;
  TempPoint: TGeoPos;
begin
  MinDistance := 1000;
  Result := -1;
  i := 0;
  while (i < High(APointsArr)) do
  begin
    if APoint = APointsArr[i+1] then
    begin
      Result := i;
      APointOnLine := APoint;
    end else
    begin
      Distance := TGeoCalcs.GeoDistancePointToLineM(APoint, APointsArr[i], APointsArr[i+1], TempPoint);
      if (Distance <= ARadius) and (Distance <= MinDistance) then
      begin
        MinDistance := Distance;
        APointOnLine := TempPoint;
        Result := i;
      end;
    end;
    inc(i);
  end;
end;

function TMapEditor.CheckLinks(ARootPath: string; AAccounts: TIntegerDynArray;
  AHashStr: string; ASave: Boolean): Integer;
var
  i, j: Integer;
  DeleteDict: TDictionary<TIDLines, Integer>;
  Holder, Holder1: THoldRec;
//  bCanSave: Boolean;
  HashList: THashList;
  AHashFromDict, AHashToDict: THashDict;
  ID: TIDLines;
  Count: Integer;
  HashStr, s: string;
  Hash: Int64;
begin
  AHashFromDict := TDictionary<Int64,THashList>.Create;
  AHashToDict := TDictionary<Int64,THashList>.Create;
  DeleteDict := TDictionary<TIDLines,Integer>.Create;
  try
    FRootPath := ARootPath;
    TGFAdapter.Load(ARootPath, AAccounts, AHashStr, Holder);
    FHoldRecDict.AddOrSetValue(AHashStr, Holder);
//    HVDict := TDictionary<THashVector,TRouteInfoDict>.Create;

//    bCanSave := False;
    Result := 0;
    //заполняем словари
    for j := Low(Holder.EdgeForward) to High(Holder.EdgeForward) do
    begin
      Count := 1;
      HashStr := TGeoHash.ConvertBinToString(Holder.EdgeForward[j].HashVector.HashTo and CAreaHashMask, 5);
      if HashStr <> AHashStr then
      begin
        if not FHoldRecDict.TryGetValue(HashStr, Holder1) then
        begin
          TGFAdapter.Load(ARootPath, AAccounts, HashStr, Holder1);
          FHoldRecDict.Add(HashStr, Holder1);
          for i := Low(Holder1.EdgeForward) to High(Holder1.EdgeForward) do
          begin
            if not AHashFromDict.TryGetValue(Holder1.EdgeForward[i].HashVector.HashFrom, HashList) then
              HashList := TDictionary<Int64,Integer>.Create;

            HashList.AddOrSetValue(Holder1.EdgeForward[i].HashVector.HashTo, 0);
            AHashFromDict.AddOrSetValue(Holder1.EdgeForward[i].HashVector.HashFrom, HashList);

          end;
          for i := Low(Holder1.EdgeBackward) to High(Holder1.EdgeBackward) do
          begin
            if not AHashToDict.TryGetValue(Holder1.EdgeBackward[i].HashVector.HashFrom, HashList) then
              HashList := TDictionary<Int64,Integer>.Create;

            HashList.AddOrSetValue(Holder1.EdgeBackward[i].HashVector.HashTo, 0);
            AHashToDict.AddOrSetValue(Holder1.EdgeBackward[i].HashVector.HashFrom, HashList);
          end;
        end;
      end;// else
      begin
        if not AHashFromDict.TryGetValue(Holder.EdgeForward[j].HashVector.HashFrom, HashList) then
          HashList := TDictionary<Int64,Integer>.Create;

        HashList.AddOrSetValue(Holder.EdgeForward[j].HashVector.HashTo, 0);
        AHashFromDict.AddOrSetValue(Holder.EdgeForward[j].HashVector.HashFrom, HashList);
      end;
    end;

    for j := Low(Holder.EdgeBackward) to High(Holder.EdgeBackward) do
    begin
      Count := 1;
      HashStr := TGeoHash.ConvertBinToString(Holder.EdgeBackward[j].HashVector.HashTo and CAreaHashMask, 5);
      if HashStr <> AHashStr then
      begin
        if not FHoldRecDict.TryGetValue(HashStr, Holder1) then
        begin
          TGFAdapter.Load(ARootPath, AAccounts, HashStr, Holder1);
          FHoldRecDict.Add(HashStr, Holder1);
          for i := Low(Holder1.EdgeBackward) to High(Holder1.EdgeBackward) do
          begin
            if not AHashToDict.TryGetValue(Holder1.EdgeBackward[i].HashVector.HashFrom, HashList) then
              HashList := TDictionary<Int64,Integer>.Create;

            HashList.AddOrSetValue(Holder1.EdgeBackward[i].HashVector.HashTo, 0);
            AHashToDict.AddOrSetValue(Holder1.EdgeBackward[i].HashVector.HashFrom, HashList);

          end;
        end;
      end;// else
      begin
        if not AHashToDict.TryGetValue(Holder.EdgeBackward[j].HashVector.HashFrom, HashList) then
          HashList := TDictionary<Int64,Integer>.Create;

        HashList.AddOrSetValue(Holder.EdgeBackward[j].HashVector.HashTo, 0);
        AHashToDict.AddOrSetValue(Holder.EdgeBackward[j].HashVector.HashFrom, HashList);

      end;
    end;

    FHoldRecDict.TryGetValue(AHashStr, Holder);
    if ASave then
    begin
    //проставляем новые линки и сохраняем
      SetLength(Holder.ListForward, 0);
      SetLength(Holder.ListBackward, 0);
      ToLog('Правим линки в '+ ARootPath + IntToStr(AAccounts[0]) + ', '+ AHashStr);
      for I := Low(Holder.EdgeForward) to High(Holder.EdgeForward) do
      begin
        if AHashFromDict.TryGetValue(Holder.EdgeForward[i].HashVector.HashTo, HashList) then
        begin
          Holder.EdgeForward[i].LinkIndex := Length(Holder.ListForward);
          Holder.EdgeForward[i].LinkCount := HashList.Count;
          for Hash in HashList.Keys do
          begin
            SetLength(Holder.ListForward, Length(Holder.ListForward) + 1);
            Holder.ListForward[High(Holder.ListForward)].HashFrom := Holder.EdgeForward[i].HashVector.HashTo;
            Holder.ListForward[High(Holder.ListForward)].HashTo := Hash;
          end;
        end else
        begin
          Holder.EdgeForward[i].LinkIndex := Length(Holder.ListForward);
          Holder.EdgeForward[i].LinkCount := 0;
          ID.OSM_ID := Holder.EdgeForward[i].ID;
          ID.HashStart := Holder.EdgeForward[i].HashVector.HashFrom;
          ID.HashEnd := Holder.EdgeForward[i].HashVector.HashTo;
          DeleteDict.AddOrSetValue(ID, 0);
          ToLog('Not found edge_f '+ IntToStr(Holder.EdgeForward[i].ID)+'-'+
                      IntToStr(Holder.EdgeForward[i].HashVector.HashFrom)+'-'+
                      IntToStr(Holder.EdgeForward[i].HashVector.HashTo));
        end;
      end;

{      for i := Low(Holder.EdgeBackward) to High(Holder.EdgeBackward) do
      begin
        if AHashToDict.TryGetValue(Holder.EdgeBackward[j].HashVector.HashTo, HashList) then
        begin
          Holder.EdgeBackward[i].LinkIndex := Length(Holder.ListBackward);
          Holder.EdgeBackward[i].LinkCount := HashList.Count;
          for Hash in HashList.Keys do
          begin
            SetLength(Holder.ListBackward, Length(Holder.ListBackward) + 1);
            Holder.ListBackward[High(Holder.ListBackward)].HashFrom := Holder.EdgeBackward[i].HashVector.HashTo;
            Holder.ListBackward[High(Holder.ListBackward)].HashTo := Hash;
          end;
        end else
        begin
            ToLog(IntToStr(Holder.EdgeBackward[j].ID)+'-'+
                      IntToStr(Holder.EdgeBackward[j].HashVector.HashFrom)+'-'+
                      IntToStr(Holder.EdgeBackward[j].HashVector.HashTo)+
                      ', Link='+IntToStr(Holder.EdgeBackward[j].LinkCount)+
                      ', Points=0');
        end;
      end;  }
      for I := Low(Holder.EdgeBackward) to High(Holder.EdgeBackward) do
      begin
        if AHashToDict.TryGetValue(Holder.EdgeBackward[i].HashVector.HashTo, HashList) then
        begin
          Holder.EdgeBackward[i].LinkIndex := Length(Holder.ListBackward);
          Holder.EdgeBackward[i].LinkCount := HashList.Count;
          for Hash in HashList.Keys do
          begin
            SetLength(Holder.ListBackward, Length(Holder.ListBackward) + 1);
            Holder.ListBackward[High(Holder.ListBackward)].HashFrom := Holder.EdgeBackward[i].HashVector.HashTo;
            Holder.ListBackward[High(Holder.ListBackward)].HashTo := Hash;
          end;
        end else
        begin
          Holder.EdgeBackward[i].LinkIndex := Length(Holder.ListBackward);
          Holder.EdgeBackward[i].LinkCount := 0;
          ID.OSM_ID := Holder.EdgeBackward[i].ID;
          ID.HashStart := Holder.EdgeBackward[i].HashVector.HashFrom;
          ID.HashEnd := Holder.EdgeBackward[i].HashVector.HashTo;
          DeleteDict.AddOrSetValue(ID, 0);
          ToLog('Not found edge_b '+ IntToStr(Holder.EdgeBackward[i].ID)+'-'+
                      IntToStr(Holder.EdgeBackward[i].HashVector.HashFrom)+'-'+
                      IntToStr(Holder.EdgeBackward[i].HashVector.HashTo));
        end;

//        DeleteDict.AddOrSetValue(ID, 0);
      end;
      TGFAdapter.Save(ARootPath, AAccounts[0], AHashStr, Holder, C5FileTypes);
{      SetLength(FDeleteArr, 0);
      for ID in DeleteDict.Keys do
      begin
        SetLength(FDeleteArr, Length(FDeleteArr) + 1);
        FDeleteArr[High(FDeleteArr)] := ID;
      end;
      DeleteRoutesFromArray(AAccounts, True);
      SaveAllFiles(AAccounts, C5FileTypes);}
    end else //ASave
    begin
//      FHoldRecDict.TryGetValue(AHashStr, Holder);
      ToLog(ARootPath + ' ' + AHashStr+'.edge_f');
      for j := Low(Holder.EdgeForward) to High(Holder.EdgeForward) do
      begin
        if AHashFromDict.TryGetValue(Holder.EdgeForward[j].HashVector.HashTo, HashList) then
        begin
          s := '';
          for Hash in HashList.Keys do
            s := s + IntToStr(Hash)+'('+TGeoHash.ConvertBinToString(Hash and CAreaHashMask, 5)+'), ';

          if Holder.EdgeForward[j].LinkCount <> HashList.Count then
            ToLog(IntToStr(Holder.EdgeForward[j].ID)+'-'+
                      IntToStr(Holder.EdgeForward[j].HashVector.HashFrom)+'-'+
                      IntToStr(Holder.EdgeForward[j].HashVector.HashTo)+
                      ', Link='+IntToStr(Holder.EdgeForward[j].LinkCount)+
                      ', Points='+IntToStr(HashList.Count) + ' ' + s);
        end else
        begin
            ToLog(IntToStr(Holder.EdgeForward[j].ID)+'-'+
                      IntToStr(Holder.EdgeForward[j].HashVector.HashFrom)+'-'+
                      IntToStr(Holder.EdgeForward[j].HashVector.HashTo)+
                      ', Link='+IntToStr(Holder.EdgeForward[j].LinkCount)+
                      ', Points=0');
        end;
      end;
      ToLog(ARootPath + ' ' + AHashStr+'.edge_b');
      for j := Low(Holder.EdgeBackward) to High(Holder.EdgeBackward) do
      begin
        if AHashToDict.TryGetValue(Holder.EdgeBackward[j].HashVector.HashTo, HashList) then
        begin
          s := '';
          for Hash in HashList.Keys do
            s := s + IntToStr(Hash)+'('+TGeoHash.ConvertBinToString(Hash and CAreaHashMask, 5)+'), ';

          if Holder.EdgeBackward[j].LinkCount <> HashList.Count then
            ToLog(IntToStr(Holder.EdgeBackward[j].ID)+'-'+
                      IntToStr(Holder.EdgeBackward[j].HashVector.HashFrom)+'-'+
                      IntToStr(Holder.EdgeBackward[j].HashVector.HashTo)+
                      ', Link='+IntToStr(Holder.EdgeBackward[j].LinkCount)+
                      ', Points='+IntToStr(HashList.Count) + ' ' + s);
        end else
        begin
            ToLog(IntToStr(Holder.EdgeBackward[j].ID)+'-'+
                      IntToStr(Holder.EdgeBackward[j].HashVector.HashFrom)+'-'+
                      IntToStr(Holder.EdgeBackward[j].HashVector.HashTo)+
                      ', Link='+IntToStr(Holder.EdgeBackward[j].LinkCount)+
                      ', Points=0');
        end;
      end;
    end;
  finally
//    HVDict.Free;
    AHashFromDict.Free;
    AHashToDict.Free;
    Holder.Clear;
    if Assigned(HashList) then
      HashList.Free;
  end;

end;

function TMapEditor.CheckTrack(const ARootPath: string;
  const AAccounts: TIntegerDynArray; const AWay: string): TJsonObject;
var
  Points: TGeoPosArray;
  Hash: Int64;
  HashStr: string;
  i, j, k: Integer;
  Holder: THoldRec;
  WayAzimuth, RouteAzimuth, Distance: Double;
  bFound: Boolean;
  IDArr: array of TIDLines;
  ID: TIDLines;
begin
  Result := TJsonObject.Create;
  TGeoHash.DecodeArrayWorld(AWay, Points);
  SetLength(IDArr, Length(Points));
  i := 0;
  WayAzimuth := 0;
  while I < Length(Points) do
  begin
    Hash := Points[i].ToHash;
    HashStr := TGeoHash.ConvertBinToString(Hash and CAreaHashMask, 5);
    if not FHoldRecDict.ContainsKey(HashStr) then
    begin
      TGFAdapter.Load(FRootPath, AAccounts, HashStr, Holder);
      FHoldRecDict.Add(HashStr, Holder);
    end;
    if i < High(Points) then
      WayAzimuth := TGeoCalcs.GetAzimuthDeg(Points[i], Points[i-1]);
    //ищем дорогу, к которой ближе всего наша точка, проверяем азимут
    j := 0;
    bFound := False;
    while (j < Length(Holder.EdgeForward)) and not bFound do
    begin
      Distance := TGeoCalcs.GeoLengthDeg(Points[i], Holder.EdgeForward[j].CoordsFrom);
      //точка рядом, проверяем азимут
      if Distance < CPointToRouteDistance then
      begin
        if Holder.EdgeForward[j].WayCount = 0 then
          RouteAzimuth := TGeoCalcs.GetAzimuthDeg(Holder.EdgeForward[j].CoordsFrom, Holder.EdgeForward[j].CoordsTo)
        else
          RouteAzimuth := TGeoCalcs.GetAzimuthDeg(Holder.EdgeForward[j].CoordsFrom, Holder.Ways[Holder.EdgeForward[j].WayIndex]);
        if Abs(WayAzimuth-RouteAzimuth) < CMaxAzimuthDifference then
        begin
          //считаем что это наша дорога
          bFound := True;
          ID.OSM_ID := Holder.EdgeForward[j].ID;
          ID.HashStart := Holder.EdgeForward[j].HashVector.HashFrom;
          ID.HashEnd := Holder.EdgeForward[j].HashVector.HashTo;
          IDArr[i] := ID;
        end;
      end else
      begin
        //проверяем расстояние до отрезка дороги
        if Holder.EdgeForward[j].WayCount = 0 then
        begin
          Distance := TGeoCalcs.GeoDistancePointToLineMDeg(Points[i].Latitude,Points[i].Longitude,
                      Holder.EdgeForward[j].CoordsFrom.Latitude, Holder.EdgeForward[j].CoordsFrom.Longitude,
                      Holder.EdgeForward[j].CoordsTo.Latitude, Holder.EdgeForward[j].CoordsTo.Longitude);
          if Distance < CPointToRouteDistance then
          begin
            RouteAzimuth := TGeoCalcs.GetAzimuthDeg(Holder.EdgeForward[j].CoordsFrom, Holder.EdgeForward[j].CoordsTo);
            if Abs(WayAzimuth-RouteAzimuth) < CMaxAzimuthDifference then
            begin
              //считаем что это наша дорога
              bFound := True;
              ID.OSM_ID := Holder.EdgeForward[j].ID;
              ID.HashStart := Holder.EdgeForward[j].HashVector.HashFrom;
              ID.HashEnd := Holder.EdgeForward[j].HashVector.HashTo;
              IDArr[i] := ID;
            end;
          end;
        end else
        begin
          //с начала до первой промежуточной точки
          Distance := TGeoCalcs.GeoDistancePointToLineMDeg(Points[i].Latitude,Points[i].Longitude,
                      Holder.EdgeForward[j].CoordsFrom.Latitude, Holder.EdgeForward[j].CoordsFrom.Longitude,
                      Holder.Ways[Holder.EdgeForward[j].WayIndex].Latitude, Holder.Ways[Holder.EdgeForward[j].WayIndex].Longitude);
          if Distance < CPointToRouteDistance then
          begin
            RouteAzimuth := TGeoCalcs.GetAzimuthDeg(Holder.EdgeForward[j].CoordsFrom, Holder.Ways[Holder.EdgeForward[j].WayIndex]);
            if Abs(WayAzimuth-RouteAzimuth) < CMaxAzimuthDifference then
            begin
              //считаем что это наша дорога
              bFound := True;
              ID.OSM_ID := Holder.EdgeForward[j].ID;
              ID.HashStart := Holder.EdgeForward[j].HashVector.HashFrom;
              ID.HashEnd := Holder.EdgeForward[j].HashVector.HashTo;
              IDArr[i] := ID;
            end;
          end;

          k := 0;
          if not bFound then
          begin
          //с первой промежуточной точки до последней
            while (k < Holder.EdgeForward[j].WayCount - 1) and not bFound do
            begin
              Distance := TGeoCalcs.GeoDistancePointToLineMDeg(Points[i].Latitude,Points[i].Longitude,
                          Holder.Ways[Holder.EdgeForward[j].WayIndex + k].Latitude, Holder.Ways[Holder.EdgeForward[j].WayIndex + k].Longitude,
                          Holder.Ways[Holder.EdgeForward[j].WayIndex + k + 1].Latitude, Holder.Ways[Holder.EdgeForward[j].WayIndex + k + 1].Longitude);
              if Distance < CPointToRouteDistance then
              begin
                RouteAzimuth := TGeoCalcs.GetAzimuthDeg(Holder.Ways[Holder.EdgeForward[j].WayIndex + k], Holder.Ways[Holder.EdgeForward[j].WayIndex + k + 1]);
                if Abs(WayAzimuth-RouteAzimuth) < CMaxAzimuthDifference then
                begin
                  //считаем что это наша дорога
                  bFound := True;
                  ID.OSM_ID := Holder.EdgeForward[j].ID;
                  ID.HashStart := Holder.EdgeForward[j].HashVector.HashFrom;
                  ID.HashEnd := Holder.EdgeForward[j].HashVector.HashTo;
                  IDArr[i] := ID;
                end;
              end;
              inc(k);
            end;
          end;

          if not bFound then
          begin
          //с последней промежуточной точки до конечной
            Distance := TGeoCalcs.GeoDistancePointToLineMDeg(Points[i].Latitude,Points[i].Longitude,
                        Holder.Ways[Holder.EdgeForward[j].WayIndex + k].Latitude, Holder.Ways[Holder.EdgeForward[j].WayIndex + k].Longitude,
                        Holder.EdgeForward[j].CoordsTo.Latitude, Holder.EdgeForward[j].CoordsTo.Longitude);
            if Distance < CPointToRouteDistance then
            begin
              RouteAzimuth := TGeoCalcs.GetAzimuthDeg(Holder.Ways[Holder.EdgeForward[j].WayIndex + k], Holder.EdgeForward[j].CoordsTo);
              if Abs(WayAzimuth-RouteAzimuth) < CMaxAzimuthDifference then
              begin
                //считаем что это наша дорога
                bFound := True;
                ID.OSM_ID := Holder.EdgeForward[j].ID;
                ID.HashStart := Holder.EdgeForward[j].HashVector.HashFrom;
                ID.HashEnd := Holder.EdgeForward[j].HashVector.HashTo;
                IDArr[i] := ID;
              end;
            end;
          end;
        end;
      end;

      inc(j);
    end;
    inc(i);
  end;

end;

function TMapEditor.CheckWay(const ARootPath: string;
  const AAccounts: TIntegerDynArray; const AWay: string;
  const AMinRouteLengthM, AMinDistToLineM, ATimeout: integer): TJsonObject;
var
  Points, RealRoute: TGeoPosArray;
  Way: TGeoPathPointArray;
  sWay: string;
  NewWay: TTrackWay;
  i, j, k: Integer;
  DistArr: array of Double;
  IDArr: array of TIDAndPoint;
  WayArr: array of TTrackWay;
  ID: TIDLines;
  Search: TSearch;
  ar: TAstarRequest3;
  fs: TFormatSettings;
  RouteLength, Dist: Double;
  PointOnLine: TGeoPos;
begin
  Result := TJsonObject.Create;
  TGeoHash.DecodeArrayWorld(AWay, RealRoute);
  //чистим от двойных подряд точек
  SetLength(Points, 0);
  PointOnLine := TGeoPos.Create(0,0);
  for I := Low(RealRoute) to High(RealRoute) do
  begin
    if (RealRoute[i].Latitude <> PointOnLine.Latitude) or
       (RealRoute[i].Longitude <> PointOnLine.Longitude)then
    begin
      SetLength(Points, Length(Points) + 1);
      Points[High(Points)] := RealRoute[i];
      PointOnLine := RealRoute[i];
    end;
  end;

  SetLength(IDArr, Length(Points));
  SetLength(DistArr, Length(Points));
  SetLength(RealRoute, 0);
  Search := TSearch.Create;
  fs := TFormatSettings.Create;
  fs.DecimalSeparator := '.';
  try
    with ar do
    begin
      Version := 1;
      FormatVariant := 1;
      LenTreshold   := 15;
      Timeout       := ATimeout;
      Distance      := 0;
  //    Duration      := 0;
      BufferSize    := 1024*1024;
      HashString    := AllocMem(1024*1024);
      RoadLengthBufferSize := SizeOf(TRoadLengthByTypeRecord) * 10;
      RoadLengthByZoneByType := AllocMem(RoadLengthBufferSize);
      FeatureLimit  := 0;
    end;
    Search.CleanUpFull;
    Search.SetCalcParams3(@AAccounts[0], @ar);

    i := 0;
    if Length(Points) < 2 then
      Exit;
    while I < Length(Points) do
    begin
      if Search.FindAndAddNearestEdge(Points[i].Latitude, Points[i].Longitude, adForward, AMinDistToLineM, ID, Dist, PointOnLine) then
      begin
        IDArr[i].ID := ID;
        IDArr[i].PointOnLine := PointOnLine;
      end else
      begin
        IDArr[i].ID.OSM_ID := 0;
      end;
      DistArr[i] := Dist;
      inc(i);
    end;

    i := 1;
    SetLength(Way, 0);
    SetLength(WayArr, 0);
    if (IDArr[0].ID.OSM_ID = 0) then
    begin
      SetLength(Way, 1);
      Way[High(Way)].p := Points[0];
      SetLength(WayArr, 1 );
      WayArr[High(WayArr)].StartID.OSM_ID := 0;
    end else
    begin
      SetLength(RealRoute, 2);
      RealRoute[0] := IDArr[0].PointOnLine;
      RealRoute[1] := Points[0];
    end;

    while I < Length(IDArr) - 1 do
    begin
      //если 2 подряд пустые, то продолжаем писать трек
      if (IDArr[i-1].ID.OSM_ID = 0) and (IDArr[i].ID.OSM_ID = 0) then
      begin
        SetLength(Way, Length(Way) + 1 );
        Way[High(Way)].p := Points[i];
      end else
      //если 1 пустая, а 2 нет, то заканчиваем дорогу
      if (IDArr[i-1].ID.OSM_ID = 0) and (IDArr[i].ID.OSM_ID <> 0) then
      begin
        SetLength(Way, Length(Way) + 1 );
        Way[High(Way)].p := Points[i];
        SetLength(Way, Length(Way) + 1 );
        Way[High(Way)].p := IDArr[i].PointOnLine;
        sWay := TGeoHash.EncodeArrayAnyFormat(Way, 12, gfPath);
//        SetLength(WayArr, Length(WayArr) + 1 );
        WayArr[High(WayArr)].EndID := IDArr[i].ID;
        WayArr[High(WayArr)].Way := sWay;
      end else
      //если 2 пустая, а 1 нет, то начинаем дорогу
      if (IDArr[i-1].ID.OSM_ID <> 0) and (IDArr[i].ID.OSM_ID = 0) then
      begin
        RouteLength := GetRouteLengthKm(RealRoute);
        if RouteLength*1000 <= AMinRouteLengthM then
        //существующая дорога меньше минимального, добавляем ее к новой
        begin
          SetLength(Way, Length(RealRoute) + 1);
          for j := Low(RealRoute) to High(RealRoute) do
            Way[j].p := RealRoute[j];
          Way[High(Way)].p := Points[i];

          SetLength(WayArr, Length(WayArr) + 1 );
          WayArr[High(WayArr)].StartID.OSM_ID := 0;
        end else
        begin
          SetLength(RealRoute, 0 );
          SetLength(Way, 3 );
          Way[0].p := IDArr[i-1].PointOnLine;
          Way[1].p := Points[i-1];
          Way[2].p := Points[i];
          SetLength(WayArr, Length(WayArr) + 1 );
          WayArr[High(WayArr)].StartID := IDArr[i-1].ID;
        end;
      end else
      //если обе непустые, то пишем существующую дорогу
      if (IDArr[i-1].ID.OSM_ID <> 0) and (IDArr[i].ID.OSM_ID <> 0) then
      begin
        SetLength(RealRoute, Length(RealRoute) + 1 );
        RealRoute[High(RealRoute)] := Points[i];
      end;
      inc(i);
    end;
    i := Length(IDArr) - 1;
    //смотрим последнюю и предпоследнюю точку
    //если 2 подряд пустые, то заканчиваем писать трек
    if (IDArr[i-1].ID.OSM_ID = 0) and (IDArr[i].ID.OSM_ID = 0) then
    begin
      SetLength(Way, Length(Way) + 1 );
      Way[High(Way)].p := Points[i];
      sWay := TGeoHash.EncodeArrayAnyFormat(Way, 12, gfPath);
      WayArr[High(WayArr)].EndID.OSM_ID := 0;
      WayArr[High(WayArr)].Way := sWay;
    end else
    //если 1 пустая, а 2 нет, то заканчиваем дорогу
    if (IDArr[i-1].ID.OSM_ID = 0) and (IDArr[i].ID.OSM_ID <> 0) then
    begin
      SetLength(Way, Length(Way) + 1 );
      Way[High(Way)].p := Points[i];
      SetLength(Way, Length(Way) + 1 );
      Way[High(Way)].p := IDArr[i].PointOnLine;
      sWay := TGeoHash.EncodeArrayAnyFormat(Way, 12, gfPath);
      WayArr[High(WayArr)].EndID := IDArr[i].ID;
      WayArr[High(WayArr)].Way := sWay;
    end else
    //если 2 пустая, а 1 нет, то создаем маленькую дорогу из 3-х точек
    if (IDArr[i-1].ID.OSM_ID <> 0) and (IDArr[i].ID.OSM_ID = 0) then
    begin
      SetLength(Way, 3 );
      Way[0].p := IDArr[i-1].PointOnLine;
      Way[1].p := Points[i-1];
      Way[2].p := Points[i];
      sWay := TGeoHash.EncodeArrayAnyFormat(Way, 12, gfPath);
      SetLength(WayArr, Length(WayArr) + 1 );
      WayArr[High(WayArr)].StartID := IDArr[i-1].ID;
      WayArr[High(WayArr)].EndID.OSM_ID := 0;
      WayArr[High(WayArr)].Way := sWay;
    end else
    if (IDArr[i-1].ID.OSM_ID <> 0) and (IDArr[i].ID.OSM_ID <> 0) then
    begin
      SetLength(RealRoute, Length(RealRoute) + 1 );
      RealRoute[High(RealRoute)] := Points[i];
      RouteLength := GetRouteLengthKm(RealRoute);
      if RouteLength*1000 <= AMinRouteLengthM then
      //существующая дорога меньше минимального, добавляем ее к старой
      begin
        k := Length(Way);
        SetLength(Way, k + Length(RealRoute) - 1);
        for j := 1 to High(RealRoute) do
          Way[k + j - 1].p := RealRoute[j];

        sWay := TGeoHash.EncodeArrayAnyFormat(Way, 12, gfPath);
        WayArr[High(WayArr)].EndID.OSM_ID := 0;
        WayArr[High(WayArr)].Way := sWay;
      end;
    end;

    //заполняем Json
    for NewWay in WayArr do
    begin
      with Result.A['NewWays'].AddObject do
      begin
        if NewWay.StartID.OSM_ID <> 0 then
          S['StartID'] := NewWay.StartID.ToString;
        S['Way'] := NewWay.Way;
        if NewWay.EndID.OSM_ID <> 0 then
          S['EndID'] := NewWay.EndID.ToString;
      end;
    end;
  finally
    Search.Free;
  end;
end;

function TMapEditor.ClearAllRoutes(const ARootPath: string;
  const AAccounts: TIntegerDynArray): Boolean;
var
  FullPath: string;
  C: Char;
begin
  Result := True;
  for C in CHashChars do
  begin
    FullPath := ARootPath + IntToStr(AAccounts[0]) + PathDelim + C;
    if DirectoryExists(FullPath) then
      Result := Result and FullRemoveDir(FullPath, true, true, false);;
  end;

end;


function TMapEditor.CorrectDoubleHashes(ARootPath: string; AAccounts: TIntegerDynArray; AHashStr: string; ACorrect, ASave: boolean): Integer;
var
  j{, z}: Integer;
  HashVector: THashVector;
  HVDict, HVDoubleDict: TDictionary<THashVector,TRouteInfoDict>;
  keys: TArray<THashVector>;
//  HVDoubleDict: TDictionary<THashVector,TRouteInfoDict>;
  IDInfoMini, IDInfo1, IDInfo2: TRouteInfoMini;
  RouteInfoDict: TRouteInfoDict;
  //RouteProperties: TRouteProperties;
  Holder: THoldRec;
//  Points, Points1, Points2: TGeoPosArray;
//  PointsList: TPointDict;
  {bCanSave,} bNeedSave: Boolean;
  HashStr1: string;

begin
  Result := 0;
  FRootPath := ARootPath;
  if not FHoldRecDict.TryGetValue(AHashStr, Holder) then
  begin
    TGFAdapter.Load(ARootPath, AAccounts, AHashStr, Holder);
    FHoldRecDict.AddOrSetValue(AHashStr, Holder);
  end;
//  PointsList := TDictionary<TGeoPos,integer>.Create;
//  TRouteInfoDict = TDictionary<TRouteInfoMini,Integer>;

//  bCanSave := False;
  Result := 0;
  HVDict := TDictionary<THashVector,TRouteInfoDict>.Create;
  HVDoubleDict := TDictionary<THashVector,TRouteInfoDict>.Create;
  try
  //заполняем словари
    for j := Low(Holder.EdgeForward) to High(Holder.EdgeForward) do
    begin
      HashVector := Holder.EdgeForward[j].HashVector;
      IDInfoMini.ID.OSM_ID := Holder.EdgeForward[j].ID;
      IDInfoMini.ID.HashStart := Holder.EdgeForward[j].HashVector.HashFrom;
      IDInfoMini.ID.HashEnd := Holder.EdgeForward[j].HashVector.HashTo;
      IDInfoMini.RoadType := Holder.EdgeForward[j].RoadType;
      IDInfoMini.Speed := Holder.EdgeForward[j].MaxSpeed;
      IDInfoMini.Length := Holder.EdgeForward[j].Distance;
      IDInfoMini.PointsCount := Holder.EdgeForward[j].WayCount + 2;
      IDInfoMini.PosInEdge := j;
      if not HVDict.TryGetValue(HashVector, RouteInfoDict) then
      begin
        RouteInfoDict := TDictionary<TRouteInfoMini,Integer>.Create;
        RouteInfoDict.Add(IDInfoMini, 1);
        HVDict.AddOrSetValue(HashVector, RouteInfoDict);
      end else
      begin
        RouteInfoDict.AddOrSetValue(IDInfoMini, 1);
        HVDict.AddOrSetValue(HashVector, RouteInfoDict);
        HVDoubleDict.AddOrSetValue(HashVector, RouteInfoDict);
      end;
    end;
    Result := HVDoubleDict.Count;
//    bCanSave := True;
    bNeedSave := False;
//    z := 0;
    if HVDoubleDict.Count > 0 then
      ToLog(AHashStr + ' = ' +  IntToStr(HVDoubleDict.Count));
    for HashVector in HVDoubleDict.Keys do
    begin
      RouteInfoDict := HVDoubleDict.Items[HashVector];
      if not ACorrect then
      begin
        ToLog('Vector '+IntToStr(HashVector.HashFrom)+'-'+IntToStr(HashVector.HashTo));
        for IDInfoMini in RouteInfoDict.Keys do
        begin
          ToLog(IDInfoMini.ID.ToString + ', type='+ IntToStr(IDInfoMini.RoadType) +
                ', speed=' + IntToStr(IDInfoMini.Speed) +
                ', points=' + IntToStr(IDInfoMini.PointsCount) +
                ', length=' + FloatToStr(IDInfoMini.Length));
        end;
      end else
      begin
        bNeedSave := True;
        if RouteInfoDict.Count = 2 then
        begin
          IDInfo1 := RouteInfoDict.Keys.ToArray[0];
          IDInfo2 := RouteInfoDict.Keys.ToArray[1];
          if ReCalcRoutes(ARootPath, AAccounts, IDInfo1, IDInfo2, AHashStr) = 3 then
            Exit;
        end else //if RouteInfoDict.Count = 2
        begin
          ToLog('больше 2 дорог, Count=' + IntToStr(RouteInfoDict.Count) +
                  '1 of routes ' + RouteInfoDict.Keys.ToArray[0].ID.ToString);
          IDInfo1.ID.OSM_ID := 0;
          IDInfo2.ID.OSM_ID := 0;
          for IDInfoMini in RouteInfoDict.Keys do
          begin
            if IDInfo1.ID.OSM_ID = 0 then
              IDInfo1 := IDInfoMini
            else
            if IDInfo2.ID.OSM_ID = 0 then
              IDInfo2 := IDInfoMini;
            if (IDInfo1.ID.OSM_ID <> 0) and (IDInfo2.ID.OSM_ID <> 0) then
              case ReCalcRoutes(ARootPath, AAccounts, IDInfo1, IDInfo2, AHashStr) of
              1 :
                IDInfo1.ID.OSM_ID := 0;
              2 :
                IDInfo2.ID.OSM_ID := 0;
              3:
                Exit;
              end;
          end
        end;
      end; // if ACorrect
    end; //for

//    FHoldRecDict.TryGetValue(AHashStr, Holder1);
//    FHoldRecDict.AddOrSetValue(AHashStr, Holder);

    if bNeedSave {and bCanSave} and ASave then
      for HashStr1 in FHoldRecDict.Keys do
      begin
        FHoldRecDict.TryGetValue(HashStr1, Holder);
        if Length(Holder.EdgeForward) > 0 then
        begin
//          ToLog('Save Edge '+AHashStr+','+HashStr1 + ', count='+IntToStr(Length(Holder.EdgeForward)));
          TGFAdapter.Save(ARootPath, AAccounts[0], HashStr1, Holder, C5FileTypes)
        end else
          ToLog('Save Edge Error '+AHashStr+','+HashStr1);
      end;

  finally
    keys := HVDict.Keys.ToArray;
    for HashVector in keys do
    begin
      HVDict.ExtractPair(HashVector).Value.Free;
    end;
{
    keys := HVDoubleDict.Keys.ToArray;
    for HashVector in keys do
    begin
      if HVDoubleDict.TryGetValue(HashVector, RouteInfoDict) then
        HVDoubleDict.Items[HashVector].Free;
    end;
}
    HVDict.Free;
    HVDoubleDict.Free;
    Holder.Clear;
//    PointsList.Free;
  end;

end;

function TMapEditor.CorrectDoubleHashesAll(ARootPath: string; AAccounts: TIntegerDynArray): Integer;
var
  HashStr: string;
begin
  Result := FHoldRecDict.Count;
  for HashStr in FHoldRecDict.Keys do
    CorrectDoubleHashes(ARootPath, AAccounts, HashStr, True, False);
end;

constructor TMapEditor.Create(AMinDistToLineM: Integer; ARequestNo: Cardinal; ARedisCli: TRedisClient; ARootPath: string);
begin
  if TGeneral.FDetailLog then
    ToLog('MapEditor Start');
  FRedisCli := ARedisCli;
  FMinDistToLineM := AMinDistToLineM;
  FRequestNo := ARequestNo;
  if ARootPath = '' then
    FRootPath := IncludeTrailingPathDelimiter(ExtractFileDir(GetModuleName(HInstance)))
  else
    FRootPath := ARootPath;
  FZC := TZC.Create(FRequestNo, TGeneral.FDetailLog);
  FLandMarkArea := TLandMarkArea.Create(FRequestNo, TGeneral.FDetailLog);
  FSign := TSign.Create(FRequestNo, TGeneral.FDetailLog);
  FHoldRecDict := TDictionary<string, THoldRec>.Create;
end;

procedure TMapEditor.DeleteWayPointArray(var AWayArr: TWayPointArray;
  APos: Int64);
var
  Last: integer;
begin
  Last:= high( AWayArr );
  if APos <  Last then
    move( AWayArr[APos+1], AWayArr[APos],
      (Last-APos) * sizeof( AWayArr[APos] ));
  setLength( AWayArr, Last );
end;

destructor TMapEditor.Destroy;
var
  s: string;
begin
  FZC.Free;
  FSign.Free;
  FHoldRecDict.Free;
  if Assigned(FAddrOsmDict) then
    FAddrOsmDict.Free;
  if Assigned(FOsmNameDict) then
    FOsmNameDict.Free;
  if Assigned(FOsmNameHashDict) then
  begin
    FOsmNameHashDict.Free;
  end;
  if Assigned(FOsmNameCsvDict) then
  begin
    for s in FOsmNameCsvDict.Keys do
    begin
      FOsmNameCsvDict.ExtractPair(s).Value.Free;
    end;
    FOsmNameCsvDict.Free;
  end;
  if Assigned(FDict) then
    FDict.Free;
  if Assigned(FLandMarkArea) then
    FLandMarkArea.Free;
 { if Assigned(FAddressDataFile) and (FAddressDataFile.Size>0) then
    FreeAndNil(FAddressDataFile);
  if Assigned(FIdxFile) then
    FIdxFile.Free;
  if Assigned(FAddressStreamReader) then
    FAddressStreamReader.Free;    }

  FStartEdgeFiles.Clear;
  FEndEdgeFiles.Clear;

  if TGeneral.FDetailLog then
    ToLog('MapEditor.Stop FHoldRecDict.Free; FAddrOsmDict.Free');
  SetLength(FUpdateMinArr, 0);
  SetLength(FUpdateWayArr, 0);
  SetLength(FDeleteArr, 0);
  SetLength(FNewRouteArr, 0);
  inherited;
end;

procedure TMapEditor.DeleteEdgeArray(var AEdgeArr: TEdgeArray;
  APos: Int64);
var
  Last: integer;
begin
  Last:= high( AEdgeArr );
  if APos <  Last then
    move( AEdgeArr[APos+1], AEdgeArr[APos],
      (Last-APos) * sizeof( AEdgeArr[APos] ));
  setLength( AEdgeArr, Last );
end;

procedure TMapEditor.DeleteListArray(var AListArr: THashVectorArray;
  APos: Int64);
var
  Last: integer;
begin
  try
  if Length(AListArr) = 0 then
    Exit;
  Last:= high( AListArr );
  if APos <  Last then
    move( AListArr[APos+1], AListArr[APos],
      (Last-APos) * sizeof( AListArr[APos] ));
  setLength( AListArr, Last );
  except
    ToLog('AListArr='+IntToStr(Length(AListArr))+', APos='+IntToStr(APos));
  end;
end;

function TMapEditor.DeleteRoute(AAccounts: TIntegerDynArray; AID: TIDLines; var ARoutePoints: TGeoPosArray): Boolean;
var
  HashStartStr, HashEndStr, HashStr: string;
  bOneFileName, bDelete, bFound, bDoubleHash: Boolean;
  i, j, k, iPosInF, iPosInB, iPosMin, iPosMax, iPosWayMin, iPosWayMax, iCurWayIndex: Integer;
  iWayShift, iLinkShift: Integer;
  Holder: THoldRec;
begin
  HashStartStr := TGeoHash.ConvertBinToString(AID.HashStart and CAreaHashMask, 5);
  HashEndStr := TGeoHash.ConvertBinToString(AID.HashEnd and CAreaHashMask, 5);
  bDoubleHash := False;
  bOneFileName := HashStartStr = HashEndStr;
  iWayShift := 0;
  if bOneFileName then
  begin
    if not FHoldRecDict.TryGetValue(HashStartStr, FStartEdgeFiles) then
    begin
      TGFAdapter.Load(FRootPath, AAccounts, HashStartStr, FStartEdgeFiles);
      FHoldRecDict.Add(HashStartStr, FStartEdgeFiles);
      if Length(FStartEdgeFiles.EdgeForward) = 0 then
        ToLog('Load error ' + HashStartStr);
    end;
    //ищем позиции удаляемой дороги, если дополнительных точек нет, то просто удаляем
    //если есть, то считаем смещение, пересчитываем нижележащие дороги
    bFound := False;
    i := 0;
    iPosInF := 0;
    while (i < Length(FStartEdgeFiles.EdgeForward)) and not bFound do
    begin
      if (FStartEdgeFiles.EdgeForward[i].ID = AID.OSM_ID) and
         (FStartEdgeFiles.EdgeForward[i].HashVector.HashFrom = AID.HashStart) and
         (FStartEdgeFiles.EdgeForward[i].HashVector.HashTo = AID.HashEnd) then
      begin
        bFound := True;
        iPosInF := i;
      end else
        inc(i);
    end;

    if not bFound then
    //дорога не найдена, ничего не удаляем, выходим
    begin
      ToLog('Delete '+ HashStartStr+'-'+HashEndStr+' not found EdgeForward');
      Result := False;
      Exit;
    end;

    bFound := False;
    i := 0;
    iPosInB := 0;
    while (i < Length(FStartEdgeFiles.EdgeBackward)) and not bFound do
    begin
      if (FStartEdgeFiles.EdgeBackward[i].ID = AID.OSM_ID) and
         (FStartEdgeFiles.EdgeBackward[i].HashVector.HashFrom = AID.HashEnd) and
         (FStartEdgeFiles.EdgeBackward[i].HashVector.HashTo = AID.HashStart) then
      begin
        bFound := True;
        iPosInB := i;
      end else
        inc(i);
    end;

    if not bFound then
    //дорога не найдена, ничего не удаляем, выходим
    begin
      ToLog('Delete '+ HashStartStr+'-'+HashEndStr+' not found EdgeBackward');
      Result := False;
      Exit;
    end;

//    SetLength(ARoutePoints, FStartEdgeFiles.EdgeForward[iPosInF].WayCount + 2);
    SetLength(ARoutePoints, 2);
    ARoutePoints[0] := FStartEdgeFiles.EdgeForward[iPosInF].CoordsFrom;
    ARoutePoints[High(ARoutePoints)] := FStartEdgeFiles.EdgeForward[iPosInF].CoordsTo;
    //удаляем из файлов way
    if FStartEdgeFiles.EdgeForward[iPosInF].WayCount > 0 then
    begin
      if FStartEdgeFiles.EdgeForward[iPosInF].WayIndex = FStartEdgeFiles.EdgeBackward[iPosInB].WayIndex then
      //если позиции одинаковы, то ссылаются на одну и ту же позицию, смещение не увеличивается
      begin
        iWayShift := FStartEdgeFiles.EdgeForward[iPosInF].WayCount;
        iCurWayIndex := FStartEdgeFiles.EdgeForward[iPosInF].WayIndex;
        for j := 1 to iWayShift do
        begin
//          ARoutePoints[i] := FStartEdgeFiles.Ways[FStartEdgeFiles.EdgeForward[iPosInF].WayIndex + i - 1];
          DeleteWayPointArray(FStartEdgeFiles.Ways, FStartEdgeFiles.EdgeForward[iPosInF].WayIndex);
        end;

        for i := iPosInF + 1 to High(FStartEdgeFiles.EdgeForward) do
//        for i := 0 to High(FStartEdgeFiles.EdgeForward) do
          if FStartEdgeFiles.EdgeForward[i].WayIndex > 0 then
            FStartEdgeFiles.EdgeForward[i].WayIndex := FStartEdgeFiles.EdgeForward[i].WayIndex - iWayShift;

        for i := iPosInB + 1 to High(FStartEdgeFiles.EdgeBackward) do
//        for i := 0 to High(FStartEdgeFiles.EdgeBackward) do
          if (FStartEdgeFiles.EdgeBackward[i].WayIndex > iCurWayIndex)  then
            FStartEdgeFiles.EdgeBackward[i].WayIndex := FStartEdgeFiles.EdgeBackward[i].WayIndex - iWayShift;
      end else
      begin
        if TGeneral.FDetailLog then
          ToLog(Format('FPos=%d, FWayIndex=%d, BPos=%d, BWayIndex=%d',[iPosInF,iPosInB,
        FStartEdgeFiles.EdgeForward[iPosInF].WayIndex,
        FStartEdgeFiles.EdgeBackward[iPosInB].WayIndex]));
      //позиции разные, смещение будет увеличиваться
        if FStartEdgeFiles.EdgeForward[iPosInF].WayIndex < FStartEdgeFiles.EdgeBackward[iPosInB].WayIndex then
        begin
          iPosMin := iPosInF;
          iPosMax := iPosInB;
        end else
        begin
          iPosMin := iPosInB;
          iPosMax := iPosInF;
        end;
        iPosWayMin := FStartEdgeFiles.EdgeForward[iPosMin].WayIndex;
        iPosWayMax := FStartEdgeFiles.EdgeBackward[iPosMax].WayIndex;

        //удаляем с максимальной позиции и смещаем все, что ниже
        for j := 1 to iWayShift do
          DeleteWayPointArray(FStartEdgeFiles.Ways, iPosWayMax);

        for i := iPosMax + 1 to High(FStartEdgeFiles.EdgeForward) do
//          if FStartEdgeFiles.EdgeForward[i].WayIndex > 0 then
          if FStartEdgeFiles.EdgeForward[i].WayIndex > iPosWayMax then
            FStartEdgeFiles.EdgeForward[i].WayIndex := FStartEdgeFiles.EdgeForward[i].WayIndex - iWayShift;

        for i := iPosMax + 1 to High(FStartEdgeFiles.EdgeBackward) do
//          if FStartEdgeFiles.EdgeBackward[i].WayIndex > 0 then
          if FStartEdgeFiles.EdgeBackward[i].WayIndex > iPosWayMax then
            FStartEdgeFiles.EdgeBackward[i].WayIndex := FStartEdgeFiles.EdgeBackward[i].WayIndex - iWayShift;

        //удаляем с минимальной позиции и смещаем все, что ниже
        iWayShift := FStartEdgeFiles.EdgeForward[iPosInF].WayCount;
        for j := 1 to iWayShift do
//          DeleteWayPointArray(FStartEdgeFiles.Ways, FStartEdgeFiles.EdgeForward[iPosMin].WayIndex);
          DeleteWayPointArray(FStartEdgeFiles.Ways, iPosWayMin);

        for i := iPosMin + 1 to High(FStartEdgeFiles.EdgeForward) do
//          if FStartEdgeFiles.EdgeForward[i].WayIndex > 0 then
          if FStartEdgeFiles.EdgeForward[i].WayIndex > iPosWayMin then
            FStartEdgeFiles.EdgeForward[i].WayIndex := FStartEdgeFiles.EdgeForward[i].WayIndex - iWayShift;

        for i := iPosMin + 1 to High(FStartEdgeFiles.EdgeBackward) do
//          if FStartEdgeFiles.EdgeBackward[i].WayIndex > 0 then
          if FStartEdgeFiles.EdgeBackward[i].WayIndex > iPosWayMin then
            FStartEdgeFiles.EdgeBackward[i].WayIndex := FStartEdgeFiles.EdgeBackward[i].WayIndex - iWayShift;

      end;
    end;
    //проверяем на двойные hashstart-hashend, если находим, то дорогу удаляем, а линки оставляем
    bDoubleHash := False;
    i := 0;
    while (i<=High(FStartEdgeFiles.EdgeForward)) and (not bDoubleHash) do
    begin
      if (FStartEdgeFiles.EdgeForward[i].HashVector.HashFrom = AID.HashStart) and
         (FStartEdgeFiles.EdgeForward[i].HashVector.HashTo = AID.HashEnd) and
         (FStartEdgeFiles.EdgeForward[i].ID <> AID.OSM_ID) then
        bDoubleHash := True;
      Inc(i);
    end;

    //удаляем из файлов list
    if not bDoubleHash then
    begin
      iLinkShift := FStartEdgeFiles.EdgeForward[iPosInF].LinkCount;
      if iLinkShift > 0 then
      begin
        for j := 1 to iLinkShift do
          DeleteListArray(FStartEdgeFiles.ListForward, FStartEdgeFiles.EdgeForward[iPosInF].LinkIndex);
        //смещаем LinkIndex всем кто ниже
        for j := iPosInF + 1 to High(FStartEdgeFiles.EdgeForward) do
          FStartEdgeFiles.EdgeForward[j].LinkIndex := FStartEdgeFiles.EdgeForward[j].LinkIndex - iLinkShift;
      end;
      iLinkShift := FStartEdgeFiles.EdgeBackward[iPosInB].LinkCount;
      if iLinkShift > 0 then
      begin
        for j := 1 to iLinkShift do
          DeleteListArray(FStartEdgeFiles.ListBackward, FStartEdgeFiles.EdgeBackward[iPosInB].LinkIndex);
        //смещаем LinkIndex всем кто ниже
        for j := iPosInB + 1 to High(FStartEdgeFiles.EdgeBackward) do
          FStartEdgeFiles.EdgeBackward[j].LinkIndex := FStartEdgeFiles.EdgeBackward[j].LinkIndex - iLinkShift;
      end;
    end;

    //удаляем из файлов Edge
    SetAccountBorder(AAccounts[0], Now(), FStartEdgeFiles.EdgeForward[iPosInF].CoordsFrom, FStartEdgeFiles.EdgeForward[iPosInF].CoordsTo);
    DeleteEdgeArray(FStartEdgeFiles.EdgeForward, iPosInF);
    DeleteEdgeArray(FStartEdgeFiles.EdgeBackward, iPosInB);
    FHoldRecDict.AddOrSetValue(HashStartStr, FStartEdgeFiles);

    Result := True;
  end else
  begin
    if not FHoldRecDict.TryGetValue(HashStartStr, FStartEdgeFiles) then
    begin
      TGFAdapter.Load(FRootPath, AAccounts, HashStartStr, FStartEdgeFiles);
      FHoldRecDict.Add(HashStartStr, FStartEdgeFiles);
      if Length(FStartEdgeFiles.EdgeForward) = 0 then
        ToLog('Load error forward' + HashStartStr);
    end;
    if not FHoldRecDict.TryGetValue(HashEndStr, FEndEdgeFiles) then
    begin
      TGFAdapter.Load(FRootPath, AAccounts, HashEndStr, FEndEdgeFiles);
      FHoldRecDict.Add(HashEndStr, FEndEdgeFiles);
      if Length(FEndEdgeFiles.EdgeBackward) = 0 then
        ToLog('Load error backward' + HashEndStr);
    end;

    //ищем позиции удаляемой дороги, если дополнительных точек нет, то просто удаляем
    //если есть, то считаем смещение, пересчитываем нижележащие дороги
    bFound := False;
    i := 0;
    iPosInF := 0;
    while (i < Length(FStartEdgeFiles.EdgeForward)) and not bFound do
    begin
      if (FStartEdgeFiles.EdgeForward[i].ID = AID.OSM_ID) and
         (FStartEdgeFiles.EdgeForward[i].HashVector.HashFrom = AID.HashStart) and
         (FStartEdgeFiles.EdgeForward[i].HashVector.HashTo = AID.HashEnd) then
      begin
        bFound := True;
        iPosInF := i;
      end else
        inc(i);
    end;

    if not bFound then
    //дорога не найдена, ничего не удаляем, выходим
    begin
      ToLog('Delete '+ HashStartStr+'-'+HashEndStr+' not found EdgeForward');
      Result := False;
      Exit;
    end;

    bFound := False;
    i := 0;
    iPosInB := 0;
    while (i < Length(FEndEdgeFiles.EdgeBackward)) and not bFound do
    begin
      if (FEndEdgeFiles.EdgeBackward[i].ID = AID.OSM_ID) and
         (FEndEdgeFiles.EdgeBackward[i].HashVector.HashFrom = AID.HashEnd) and
         (FEndEdgeFiles.EdgeBackward[i].HashVector.HashTo = AID.HashStart) then
      begin
        bFound := True;
        iPosInB := i;
      end else
        inc(i);
    end;

    if not bFound then
    //дорога не найдена, ничего не удаляем, выходим
    begin
      ToLog('Delete '+ HashStartStr+'-'+HashEndStr+' not found EdgeBackward');
      Result := False;
      Exit;
    end;
    try
    if FStartEdgeFiles.EdgeForward[iPosInF].WayCount > 0 then
    begin
      //находим позицию в FStartEdgeFiles.EdgeBackward с которой начинается смещение
      i := 0;
      iPosMin := 0;
      bFound := False;
      while (i < Length(FStartEdgeFiles.EdgeBackward)) and not bFound do
      begin
        if (FStartEdgeFiles.EdgeBackward[i].WayIndex >= FStartEdgeFiles.EdgeForward[iPosInF].WayIndex) then
        begin
          bFound := True;
          iPosMin := i;
        end else
          inc(i);
      end;
      iWayShift := FStartEdgeFiles.EdgeForward[iPosInF].WayCount;
      //удаляем точки в FStartEdgeFiles.Ways
      for j := 1 to iWayShift do
        DeleteWayPointArray(FStartEdgeFiles.Ways, FStartEdgeFiles.EdgeForward[iPosInF].WayIndex);
      //пересчитываем индексы в FStartEdgeFiles.EdgeForward
      for i := iPosInF + 1 to High(FStartEdgeFiles.EdgeForward) do
        if FStartEdgeFiles.EdgeForward[i].WayIndex > 0 then
          FStartEdgeFiles.EdgeForward[i].WayIndex := FStartEdgeFiles.EdgeForward[i].WayIndex - iWayShift;
      //пересчитываем индексы в FStartEdgeFiles.EdgeBackward
      if bFound then // если найдена дорога после нашей, то смещаем
      for i := iPosMin to High(FStartEdgeFiles.EdgeBackward) do
        if FStartEdgeFiles.EdgeBackward[i].WayIndex > 0 then
          FStartEdgeFiles.EdgeBackward[i].WayIndex := FStartEdgeFiles.EdgeBackward[i].WayIndex - iWayShift;

      //находим позицию в FEndEdgeFiles.EdgeForward с которой начинается смещение
      i := 0;
      bFound := False;
      while (i < Length(FEndEdgeFiles.EdgeForward)) and not bFound do
      begin
        if (FEndEdgeFiles.EdgeForward[i].WayIndex >= FEndEdgeFiles.EdgeBackward[iPosInB].WayIndex) then
        begin
          bFound := True;
          iPosMin := i;
        end else
          inc(i);
      end;

      //удаляем точки в FStartEdgeFiles.Ways
      for j := 1 to iWayShift do
        DeleteWayPointArray(FEndEdgeFiles.Ways, FEndEdgeFiles.EdgeBackward[iPosInB].WayIndex);
      //пересчитываем индексы в FStartEdgeFiles.EdgeForward
      if bFound then // если найдена дорога после нашей, то смещаем
      for i := iPosMin to High(FEndEdgeFiles.EdgeForward) do
        if FEndEdgeFiles.EdgeForward[i].WayIndex > 0 then
          FEndEdgeFiles.EdgeForward[i].WayIndex := FEndEdgeFiles.EdgeForward[i].WayIndex - iWayShift;
      //пересчитываем индексы в FEndEdgeFiles.EdgeBackward
      for i := iPosInB + 1 to High(FEndEdgeFiles.EdgeBackward) do
        if FEndEdgeFiles.EdgeBackward[i].WayIndex > 0 then
          FEndEdgeFiles.EdgeBackward[i].WayIndex := FEndEdgeFiles.EdgeBackward[i].WayIndex - iWayShift;

    end;
    //проверяем на двойные hashstart-hashend, если находим, то дорогу удаляем, а линки оставляем
    bDoubleHash := False;
    i := 0;
    while (i<=High(FStartEdgeFiles.EdgeForward)) and (not bDoubleHash) do
    begin
      if (FStartEdgeFiles.EdgeForward[i].HashVector.HashFrom = AID.HashStart) and
         (FStartEdgeFiles.EdgeForward[i].HashVector.HashTo = AID.HashEnd) and
         (FStartEdgeFiles.EdgeForward[i].ID <> AID.OSM_ID) then
        bDoubleHash := True;
      Inc(i);
    end;

    //удаляем из файлов list
    if not bDoubleHash then
    begin
      //удаляем из файлов list
      iLinkShift := FStartEdgeFiles.EdgeForward[iPosInF].LinkCount;
      if iLinkShift > 0 then
      begin
        for j := 1 to iLinkShift do
          DeleteListArray(FStartEdgeFiles.ListForward, FStartEdgeFiles.EdgeForward[iPosInF].LinkIndex);
        //смещаем LinkIndex всем кто ниже
        for j := iPosInF + 1 to High(FStartEdgeFiles.EdgeForward) do
          FStartEdgeFiles.EdgeForward[j].LinkIndex := FStartEdgeFiles.EdgeForward[j].LinkIndex - iLinkShift;
      end;
      iLinkShift := FEndEdgeFiles.EdgeBackward[iPosInB].LinkCount;
      if iLinkShift > 0 then
      begin
        for j := 1 to iLinkShift do
          DeleteListArray(FEndEdgeFiles.ListBackward, FEndEdgeFiles.EdgeBackward[iPosInB].LinkIndex);
        //смещаем LinkIndex всем кто ниже
        for j := iPosInB + 1 to High(FEndEdgeFiles.EdgeBackward) do
          FEndEdgeFiles.EdgeBackward[j].LinkIndex := FEndEdgeFiles.EdgeBackward[j].LinkIndex - iLinkShift;
      end;
    end;
    except
      ToLog('Delete Error ID='+IntToStr(AID.OSM_ID)+'-'+
                               IntToStr(AID.HashStart)+'-'+
                               IntToStr(AID.HashEnd));
    end;
    SetAccountBorder(AAccounts[0], Now(), FStartEdgeFiles.EdgeForward[iPosInF].CoordsFrom, FStartEdgeFiles.EdgeForward[iPosInF].CoordsTo);
    DeleteEdgeArray(FStartEdgeFiles.EdgeForward, iPosInF);
    DeleteEdgeArray(FEndEdgeFiles.EdgeBackward, iPosInB);
    FHoldRecDict.AddOrSetValue(HashStartStr, FStartEdgeFiles);
    FHoldRecDict.AddOrSetValue(HashEndStr, FEndEdgeFiles);

    Result := True;
  end;

  if not bDoubleHash then
  begin
    //пересчитываем линки
    //проверяем вхожящие в HashStart, смотрим в EdgeBackward, правим EdgeForward
    FHoldRecDict.TryGetValue(HashStartStr, FStartEdgeFiles);
    i := 0;
    while i < Length(FStartEdgeFiles.EdgeBackward) do
    begin
      if FStartEdgeFiles.EdgeBackward[i].HashVector.HashFrom = AID.HashStart then
      begin
        //нашли конец в EdgeBackward, ищем другой конец
        HashStr := TGeoHash.ConvertBinToString(FStartEdgeFiles.EdgeBackward[i].HashVector.HashTo and CAreaHashMask, 5);
        if not FHoldRecDict.ContainsKey(HashStr) then
        begin
          TGFAdapter.Load(FRootPath, AAccounts, HashStr, Holder);
          FHoldRecDict.Add(HashStr, Holder);
        end else
          FHoldRecDict.TryGetValue(HashStr, Holder);
        //ищем начало в EdgeForward и уменьшаем на 1, удаляем ссылку в и смещаем остальные на 1
        bFound := False;
        j := 0;
        while (j < Length(Holder.EdgeForward)) {and not bFound} do
        begin
          if (Holder.EdgeForward[j].HashVector.HashTo = FStartEdgeFiles.EdgeBackward[i].HashVector.HashFrom) and
             (Holder.EdgeForward[j].HashVector.HashFrom = FStartEdgeFiles.EdgeBackward[i].HashVector.HashTo) and
             (Holder.EdgeForward[j].ID = FStartEdgeFiles.EdgeBackward[i].ID) then
          begin
            bFound := True;
            k := Holder.EdgeForward[j].LinkIndex;
            bDelete := False;
            while (k < Holder.EdgeForward[j].LinkIndex + Holder.EdgeForward[j].LinkCount) and not bDelete do
            begin
              if (Holder.ListForward[k].HashFrom = AID.HashStart) and
                 (Holder.ListForward[k].HashTo = AID.HashEnd) then
              begin
                DeleteListArray(Holder.ListForward, k);
                bDelete := True;
              end else
              inc(k);
            end;
//            if not bDoubleHash then
            Holder.EdgeForward[j].LinkCount := Holder.EdgeForward[j].LinkCount - 1;
          end else
          if bFound then
          begin
            Holder.EdgeForward[j].LinkIndex := Holder.EdgeForward[j].LinkIndex - 1;
          end;
          inc(j);
        end;
        FHoldRecDict.AddOrSetValue(HashStr, Holder);
      end;
      inc(i);
    end;

    //проверяем исходящие из HashEnd
    FHoldRecDict.TryGetValue(HashEndStr, FEndEdgeFiles);
    i := 0;
    while i < Length(FEndEdgeFiles.EdgeForward) do
  //  for I := Low(FEndEdgeFiles.EdgeForward) to High(FEndEdgeFiles.EdgeForward) do
    begin
      if FEndEdgeFiles.EdgeForward[i].HashVector.HashFrom = AID.HashEnd then
      begin
        //нашли конец в EdgeForward, ищем другой конец
        HashStr := TGeoHash.ConvertBinToString(FEndEdgeFiles.EdgeForward[i].HashVector.HashTo and CAreaHashMask, 5);
        if not FHoldRecDict.ContainsKey(HashStr) then
        begin
          TGFAdapter.Load(FRootPath, AAccounts, HashStr, Holder);
          FHoldRecDict.Add(HashStr, Holder);
        end else
          FHoldRecDict.TryGetValue(HashStr, Holder);
        //ищем конец в EdgeBackward и уменьшаем на 1
        bFound := False;
        j := 0;
        k := 0;
        while (j < Length(Holder.EdgeBackward)) {and not bFound} do
        begin
        try
          if (Holder.EdgeBackward[j].HashVector.HashTo = FEndEdgeFiles.EdgeForward[i].HashVector.HashFrom) and
             (Holder.EdgeBackward[j].HashVector.HashFrom = FEndEdgeFiles.EdgeForward[i].HashVector.HashTo) and
             (Holder.EdgeBackward[j].ID = FEndEdgeFiles.EdgeForward[i].ID) then
          begin
            bFound := True;
            k := Holder.EdgeBackward[j].LinkIndex;
            bDelete := False;
            while (k < Holder.EdgeBackward[j].LinkIndex + Holder.EdgeBackward[j].LinkCount)
                  and not bDelete do
            begin
              if (Holder.ListBackward[k].HashFrom = AID.HashEnd) and
                 (Holder.ListBackward[k].HashTo = AID.HashStart) then
              begin
                DeleteListArray(Holder.ListBackward, k);
                bDelete := True;
              end else
              inc(k);
            end;
  //          if not bDoubleHash then
            Holder.EdgeBackward[j].LinkCount := Holder.EdgeBackward[j].LinkCount - 1;
          end else
          if bFound then
          begin
            Holder.EdgeBackward[j].LinkIndex := Holder.EdgeBackward[j].LinkIndex - 1;
          end;
          inc(j);
      except
        ToLog('i='+IntToStr(i)+',j='+IntToStr(j)+',k='+IntToStr(k));
      end;
        end;
        FHoldRecDict.AddOrSetValue(HashStr, Holder);
      end;
      inc(i);
    end;
  end;

end;

function TMapEditor.DeleteRoutesFromArray(const AAccounts: TIntegerDynArray; ASave: boolean = True): boolean;
var
  i: Integer;
  iT: Cardinal;
  ARouteInfo: TDictionary<TIDLines, TRouteProperties>;
  ID: TIDLines;
  HashStartStr, HashEndStr: string;
  Holder: THoldRec;
  Names: TDictionary<string,Integer>;
  Areas: TLandMarkAreaDictionary;
  NeedSaveAreas: Boolean;
  RoutePoints: TGeoPosArray;
begin
  Result := False;
  iT := GetTickCount;
  Areas := FLandMarkArea.LoadAreaFile(FRootPath, AAccounts);
  NeedSaveAreas := False;
  ToLog('TMapEditor.DeleteRoutesFromArray. For delete = '+IntToStr(Length(FDeleteArr)));
  Names := TDictionary<string,Integer>.Create;
  for i := Low(FDeleteArr) to High(FDeleteArr) do
  begin
//    Application.ProcessMessages;
    if TGeneral.FStop then
    begin
      ToLog('TMapEditor.DeleteRoutesFromArray. Прервано пользователем. ' + IntToStr(i) + '/' + IntToStr(High(FDeleteArr)));
      Exit(False);
    end;
    SetLength(RoutePoints, 0);
  //загружаем все нужные файлы в словарь
    HashStartStr := TGeoHash.ConvertBinToString(FDeleteArr[i].HashStart and CAreaHashMask, 5);
    HashEndStr := TGeoHash.ConvertBinToString(FDeleteArr[i].HashEnd and CAreaHashMask, 5);
    if not FHoldRecDict.ContainsKey(HashStartStr) then
    begin
      TGFAdapter.Load(FRootPath, AAccounts, HashStartStr, Holder);
      FHoldRecDict.Add(HashStartStr, Holder);
    end;
    if not FHoldRecDict.ContainsKey(HashEndStr) then
    begin
      TGFAdapter.Load(FRootPath, AAccounts, HashEndStr, Holder);
      FHoldRecDict.Add(HashEndStr, Holder);
    end;
    Names.AddOrSetValue(Copy(HashStartStr,1,3) ,0);
    Names.AddOrSetValue(Copy(HashEndStr,1,3) ,0);
    LoadAddrOSMCsv(AAccounts, Names.Keys.ToArray);

    if not DeleteRoute(AAccounts, FDeleteArr[i], RoutePoints) then
      ToLog('Request # '+ IntToStr(FRequestNo) +
            ' Delete error. ID='+FDeleteArr[i].ToString);
    NeedSaveAreas := NeedSaveAreas or
                     FLandMarkArea.CheckRouteInTrack(FRootPath, AAccounts, RoutePoints, Areas, TGeneral.FDetailLog);
  end;
  if ASave then
  begin
    //сохраняем все файлы
    if TGeneral.FDetailLog then
      ToLog('TMapEditor.DeleteRoutesFromArray. Сохраняем файлы после удаления дорог');
    SaveAllFiles(AAccounts);
  //  SaveAllNameCsv(AAccounts, Names.Keys.ToArray);
    if NeedSaveAreas then
      FLandMarkArea.SaveAreas(FRootPath, AAccounts[0], Areas, True, TGeneral.FDetailLog);

    //загружаем все нужные файлы в словарь
    for i := Low(FDeleteArr) to High(FDeleteArr) do
    begin
      HashStartStr := TGeoHash.ConvertBinToString(FDeleteArr[i].HashStart and CAreaHashMask, 5);
      HashEndStr := TGeoHash.ConvertBinToString(FDeleteArr[i].HashEnd and CAreaHashMask, 5);
      if not FHoldRecDict.ContainsKey(HashStartStr) then
      begin
        TGFAdapter.Load(FRootPath, AAccounts, HashStartStr, Holder);
        FHoldRecDict.Add(HashStartStr, Holder);
      end;
      if not FHoldRecDict.ContainsKey(HashEndStr) then
      begin
        TGFAdapter.Load(FRootPath, AAccounts, HashEndStr, Holder);
        FHoldRecDict.Add(HashEndStr, Holder);
      end;
    end;

  //  if TGeneral.FDetailLog then
    ToLog('Request # '+ IntToStr(FRequestNo) + ' Delete '+ IntToStr(Length(FDeleteArr))+' roads, tick ='+IntToStr(GetTickCount-iT));
  //  iT := GetTickCount;
    ARouteInfo := TDictionary<TIDLines, TRouteProperties>.Create;
    try
      for i := Low(FDeleteArr) to High(FDeleteArr) do
      begin
        GetRoutesInfo(AAccounts, ID,
                      TGeoHash.ConvertBinToString(FDeleteArr[i].HashStart and CAreaHashMask,5),
                      TGeoHash.ConvertBinToString(FDeleteArr[i].HashEnd and CAreaHashMask, 5),
                      ARouteInfo);
      end;
      Result := ARouteInfo.Count = 0;
      for ID in ARouteInfo.Keys do
        ToLog('Request # '+ IntToStr(FRequestNo) +' Not Deleted '+ID.ToString);
    finally
      ARouteInfo.Free;
      Names.Free;
      FHoldRecDict.Clear;
      Areas.Free;
    end;
  end;
end;

function TMapEditor.GetAStarAcc(APoints: string; ASpeed, ALimit, AFormatVariant, ATimeout: integer;
  AZone, ASign: Uint64; AAccounts: TIntegerDynArray): string;
const
  CArrowMinDegree = 0.10471975511966; // 6 градусов
var
  Rez: Integer;
  i: Integer;
  KStart, KEnd: TLargeInteger;
  KCounterPerSec: TLargeInteger;
  RezStr: string;
  TempStr, Step: string;
  RCoords: TGeoPosArray;
//  ar: TAstarRequest3;
  ar: TAstarRequest4;
  Accounts: TIntegerDynArray;

  s: string;
//  d: Double;

begin
  TGeoHash.DecodeArrayWorld(APoints, RCoords);
  if Length(RCoords) <> 2 then
  begin
    Result := '{"result":"not 2 Point"}';
    Exit;
  end;

  try
  Step := ' Step 1';
  QueryPerformanceFrequency( KCounterPerSec );
  QueryPerformanceCounter( KStart );

  SetLength(Accounts, Length(AAccounts)+1);
  for I := Low(AAccounts) to High(AAccounts) do
    Accounts[i] := AAccounts[i];
  Accounts[High(Accounts)] := 0;

  Step := ' Step 2';
  with ar do
  begin
    if TGeneral.FDetailLog then
      ToLog(s);
    Version := 1;
    FromLatitude := RCoords[0].Latitude;
    FromLongitude := RCoords[0].Longitude;
    ToLatitude := RCoords[1].Latitude;
    ToLongitude := RCoords[1].Longitude;
//    Speed := ASpeed;

    ZonesLimit    := AZone;
    SignsLimit    := ASign;
    FormatVariant := AFormatVariant;
    LenTreshold   := 15;
    Timeout       := ATimeout;//GetTimeout(s, 30000);
    Distance      := 0;
    BufferSize    := 1024*1024;
    HashString    := AllocMem(1024*1024);
//    RoadLengthBufferSize := SizeOf(TRoadLengthByTypeRecord) * 10;
//    RoadLengthByZoneByType := AllocMem(RoadLengthBufferSize);
//    FeatureLimit  := ALimit;
  end;
    Step := ' Step 3';

    ToLog('Request # '+IntToStr(FRequestNo)+' Используем внутренний AStar, timeout='+IntToStr(ATimeout));
    try
//      Rez := AStarCalcSignAcc(@ar, @AAccounts[0]);
//      Rez := AStarCalcAcc(@ar, @Accounts[0]);
      Rez := AStarCalc4Acc(@ar, @AAccounts[0]);
      ToLog('Request # '+IntToStr(FRequestNo)+' Timeout '+IntToStr(ATimeout));
      Step := ' Step 4';

      QueryPerformanceCounter( KEnd );
      if (Rez = 0) then
      begin
        RezStr := string(ar.HashString);//RezChar
        Result := '{"Path":"'+RezStr+'"}';
        if Length(RezStr) > 24 then
        begin
          if TGeneral.FDetailLog then
            ToLog('Request # '+IntToStr(FRequestNo)+' Path = '+RezStr)
          else
            ToLog('Request # '+IntToStr(FRequestNo)+' Path length = '+IntToStr(Length(RezStr)));
        end else
          ToLog('Request # '+IntToStr(FRequestNo)+' Path length < 24');
        SetLength(RCoords, 0);
        case ar.FormatVariant of
          0: i := 3;
          else i := 3;
        end;

        repeat
          TempStr := Copy(RezStr, i, 12);
          if (TempStr = '') then Break;
          SetLength(RCoords, Length(RCoords) + 1);
          TGeoHash.DecodePointString(
            TempStr,
            RCoords[High(RCoords)].Latitude,
            RCoords[High(RCoords)].Longitude
          );

          case ar.FormatVariant of
            0: Inc(i, 12);
            else //if I = 3 then
                //Inc(I, 12)
              //else
                Inc(i, 15);
          end;

        until False;
        {d := 0;
        for i := 0 to ar.RoadLengthCount - 1 do
          d := d + CRoadSpeedRecordDef.GetDuration(TRoadLengthByTypeRecordArray(ar.RoadLengthByZoneByType)[i]) * 60;
         }
      end
      else
      begin
        TGeneral.IncWatchdog(Abs(Rez));
        Result := '{"Error":"'+CErrorNames[Abs(Rez)]+'"}';
        ToLog('Request # '+IntToStr(FRequestNo)+' Error : ' + CErrorNames[Abs(Rez)]);
        RezStr := string(ar.HashString);
        if TGeneral.FDetailLog then
          ToLog(Step +' GetAStarAcc. Rez='+IntTostr(rez));
      end;
    except on E : Exception do
      ToLog(Step +' AStar Exception: '+E.Message);
    end;

  finally
    FreeMem(ar.HashString);
//    FreeMem(ar.RoadLengthByZoneByType);
  end;
end;

function TMapEditor.GetAStarPath(APoints: string; ASpeed, ALimit, AFormatVariant, ATimeout: integer; AZone, ASign: Uint64; AAccounts: TIntegerDynArray; AAStarDll: string): string;
const
  CArrowMinDegree = 0.10471975511966; // 6 градусов
var
  Rez: Integer;
  AreaID, i: Integer;
  KStart, KEnd: TLargeInteger;
  KCounterPerSec: TLargeInteger;
  RezStr: string;
  TempStr, LandMarkID: string;
  RCoords: TGeoPosArray;
  ar: TAstarRequest3;
  Accounts: TIntegerDynArray;
  RNWHash, RSEHash: Int64;
  s, AstarImage: string;
  T1, RunTime: Cardinal;
  JSon: TJsonObject;
  Points: TGeoPosArray;

//  d: Double;
//  AStarCalcSignAcc: function (const AReq: PAstarRequest3; const AAccs: PInteger): Integer; stdcall;
  AStarCalcAcc: function (const AReq: PAstarRequest3; const AAccs: PInteger): Integer; stdcall;
  AStarCalcLandmarksAcc: function(const AReq: PAstarRequest3; var RNWHash: Int64; var RSEHash: Int64;  const AAccs: PInteger): Integer; stdcall;
  //AStarCalc4Acc: function(const AReq: PAstarRequest3; var RNWHash: Int64; var RSEHash: Int64;  const AAccs: PInteger): Integer; stdcall;
begin
  RNWHash := 0;
  RSEHash := 0;
  LandMarkID := '';
  SetLength(Accounts, Length(AAccounts)+1);
  for I := Low(AAccounts) to High(AAccounts) do
    Accounts[i] := AAccounts[i];
  Accounts[High(Accounts)] := 0;

  TGeoHash.DecodeArrayWorld(APoints, RCoords);
  if Length(RCoords) <> 2 then
  begin
    Result := '{"result":"not 2 Point"}';
    Exit;
  end;
  AstarImage := IncludeTrailingPathDelimiter(FRootPath) + AAStarDll;
  HAStar64 := LoadLibrary(PChar(AstarImage));
//  HAStar64 := LoadLibrary(PChar(FRootPath + 'AStar64.dll'));
  if HAStar64 = 0 then
  begin
    ToLog(AstarImage);
    Exit('{"Error":"ERROR_LOAD_ASTAR64.DLL"}');
  end;
  ToLog('Request # '+IntToStr(FRequestNo)+' Используем внешний AStar - ' + AstarImage+', timeout='+IntToStr(ATimeout));

  if TGeneral.FDetailLog then
    ToLog('Load AStar Handle = '+ IntToStr(HAStar64));

  try
  QueryPerformanceFrequency( KCounterPerSec );
  QueryPerformanceCounter( KStart );

//  Speed := 58;
  with ar do
  begin
    ToLog(s);
    Version := 1;
    FromLatitude := RCoords[0].Latitude;
    FromLongitude := RCoords[0].Longitude;
    ToLatitude := RCoords[1].Latitude;
    ToLongitude := RCoords[1].Longitude;
//    Speed := ASpeed;

    ZonesLimit    := AZone;
    SignsLimit    := ASign;
    FormatVariant := AFormatVariant;
    LenTreshold   := 15;
    Timeout       := ATimeout;//GetTimeout(s, 30000);
    Distance      := 0;
    //Duration      := 0;
    BufferSize    := 1024*1024;
    HashString    := AllocMem(1024*1024);
    RoadLengthBufferSize := SizeOf(TRoadLengthByTypeRecord) * 10;
    RoadLengthByZoneByType := AllocMem(RoadLengthBufferSize);

    FeatureLimit  := ALimit;
  end;
    try
     { получаем указатель }
//      AStarCalcSignAcc := GetProcAddress(HAStar64, 'AStarCalcSignAcc');
      AStarCalcAcc := GetProcAddress(HAStar64, 'AStarCalcAcc');
      AStarCalcLandmarksAcc := GetProcAddress(HAStar64, 'AStarCalcLandmarksAcc');
//      Rez := AStarCalcSignAcc(@ar, @AAccounts[0]);
//      ToLog(string(Accounts));
//      Rez := AStarCalcAcc(@ar, @Accounts[0]);
      T1 := GetTickCount;
      if (AAStarDll = 'AStar64.dll') or (AAStarDll = 'AStar64x64.dll') then
        Rez := AStarCalcLandmarksAcc(@ar, RNWHash, RSEHash, @Accounts[0])
      else
        Rez := AStarCalcAcc(@ar, @Accounts[0]);
      RunTime := GetTickCount - T1;
      QueryPerformanceCounter( KEnd );
      AreaID := -1;
      if (Rez = 0) then
      begin
        RezStr := string(ar.HashString);//RezChar
        JSon := TJsonObject.Create;
        JSon.S['Path'] := RezStr;
        JSon.I['Time'] := RunTime;
        if (RNWHash<>0) and (RSEHash<>0) then
        begin
          SetLength(Points, 2);
          TGeoHash.DecodePointBin(RNWHash, Points[0].Latitude, Points[0].Longitude);
          TGeoHash.DecodePointBin(RSEHash, Points[1].Latitude, Points[1].Longitude);
          LandMarkID := TGeoHash.EncodeArrayWorld(Points, gfPointsArray);
          AreaID := FLandMarkArea.GetAreaIDFromGeoHashID(FRootPath, Accounts, LandMarkID);
          SetLength(Points, 0);
        end;
        if AreaID <> -1 then
          JSon.A['AreasID'].Add(IntToStr(AreaID));
        Result := JSon.ToString;//'{"Path":"'+RezStr+'"}';
        JSon.Free;
        if Length(RezStr) > 24 then
        begin
          if TGeneral.FDetailLog then
            ToLog('Request # '+IntToStr(FRequestNo)+' Маршрут рассчитан - '+ RezStr)
          else
            ToLog('Request # '+IntToStr(FRequestNo)+' Маршрут рассчитан, точек - '+IntToStr((Length(RezStr)+1) div 15) + ' Регион ' + LandMarkID);
        end else
          ToLog('Request # '+IntToStr(FRequestNo)+' Path length < 24');
        SetLength(RCoords, 0);
        case ar.FormatVariant of
          0: i := 3;
          else i := 3;
        end;

        repeat
          TempStr := Copy(RezStr, i, 12);
          if (TempStr = '') then Break;
          SetLength(RCoords, Length(RCoords) + 1);
          TGeoHash.DecodePointString(
            TempStr,
            RCoords[High(RCoords)].Latitude,
            RCoords[High(RCoords)].Longitude
          );

          case ar.FormatVariant of
            0: Inc(i, 12);
            else //if I = 3 then
                //Inc(I, 12)
              //else
                Inc(i, 15);
          end;

        until False;
{        d := 0;
        for i := 0 to ar.RoadLengthCount - 1 do
          d := d + CRoadSpeedRecordDef.GetDuration(TRoadLengthByTypeRecordArray(ar.RoadLengthByZoneByType)[i]) * 60;
}
      end
      else
      begin
        TGeneral.IncWatchdog(Abs(Rez));
        Result := '{"Error":"'+CErrorNames[Abs(Rez)]+'"}';
        ToLog('Request # '+IntToStr(FRequestNo)+' Error : ' + CErrorNames[Abs(Rez)]);
        RezStr := string(ar.HashString);
        if TGeneral.FDetailLog then
          ToLog(' GetAStarPath. Rez='+IntTostr(rez));
      end;
    except on E : Exception do
      ToLog('Request # '+IntToStr(FRequestNo)+' AStar Exception:'+E.Message);
    end;

  finally
    FreeMem(ar.HashString);
    FreeMem(ar.RoadLengthByZoneByType);
    UnloadAStar;
  end;
end;


function TMapEditor.GetFromArray(AHash: Int64; AEdgeArray: TEdgeArray;
  NotID:Integer; var AFromArray: THashVectorArray): integer;
var
  i: Integer;
begin
  SetLength(AFromArray, 0);
  for I := 0 to Length(AEdgeArray) - 1 do
    if (AEdgeArray[i].HashVector.HashFrom = AHash) and (I <> NotID) then
    begin
      SetLength(AFromArray, Length(AFromArray) + 1);
      AFromArray[Length(AFromArray)-1].HashFrom := AEdgeArray[i].HashVector.HashFrom;
      AFromArray[Length(AFromArray)-1].HashTo := AEdgeArray[i].HashVector.HashTo;
    end;
  Result := Length(AFromArray);
end;

function TMapEditor.GetHoldRecDict: THoldRecDict;
begin
  Result := FHoldRecDict;
end;

function TMapEditor.LoadAddrOSM: Int64;
var
  FIdxArr : array of TAddrIdx;
  i : Integer;
  t: Cardinal;
begin

  if Assigned(FAddrOsmDict) then
  if FAddrOsmDict.Count > 0 then
  begin
    Result := FAddrOsmDict.Count;
    Exit;
  end;
  t := GetTickCount;
  FAddressDataFileName := ExtractFileDir(GetModuleName(HInstance)) + '\osm.dat';
  FIdxFileName := ExtractFileDir(GetModuleName(HInstance)) + '\osm.idx';
  FIdxFile := TFileStream.Create(FIdxFileName, fmOpenRead + fmShareDenyNone);
  try
    FAddrOsmDict := TDictionary<Int64,Int64>.Create;
    SetLength(FIdxArr, FIdxFile.Size div SizeOf(TAddrIdx));
    FIdxFile.ReadBuffer(FIdxArr[0], FIdxFile.Size);
    for I := 0 to Length(FIdxArr) - 1 do
      FAddrOsmDict.Add(FIdxArr[i].OsmID, FIdxArr[i].Idx);
//      FAddrOsmDict.AddOrSetValue(FIdxArr[i].OsmID, FIdxArr[i].Idx);

  finally
    FIdxFile.Free;
  end;
  ToLog('TMapEditor.LoadAddrOSM ms='+IntToStr(GetTickCount-t));
  Result := FAddrOsmDict.Count;
end;


function TMapEditor.LoadAddrOSMCsv(const AAccounts: TIntegerDynArray;
  AHashArr: TWayArray): Int64;
var
  i, iCount, TabPos: Integer;
  t: Cardinal;
  sHash, RootPath, s, sName: string;
  Acc, j: Integer;
  bFound: Boolean;
  SList: TStringList;
  OSM_ID: Int64;
begin
  if not Assigned(FOsmNameCsvDict) then
    FOsmNameCsvDict := TDictionary<string,TOsmNameDict>.Create;
  if Length(AHashArr) = 0 then Exit(0);

  t := GetTickCount;
  j := 0;
  iCount := 0;
  for sHash in AHashArr do
  begin
    if FOsmNameCsvDict.ContainsKey(sHash) then
      Continue;
    bFound := False;
    while (j < Length(AAccounts)) and not bFound do
    begin
      Acc := AAccounts[j];
      RootPath := ExtractFileDir(GetModuleName(HInstance)) + PathDelim{D:\IGF\}
        + IntToStr(Acc) + PathDelim{\}
        + Copy(sHash, 1, 1){u} + PathDelim{\}
        + Copy(sHash, 1, 2){uc} + PathDelim{\}
        + Copy(sHash, 1, 3){ucf} + PathDelim{\};
      FAddressDataFileName := RootPath + 'osm.csv';
      inc(j);
      if (FileExists(FAddressDataFileName)) then
      begin
        try
          FAddressDataFile := TFileStream.Create(FAddressDataFileName, fmOpenRead + fmCreate + fmShareDenyNone);
          SList := TStringList.Create;
          try
            SList.LoadFromStream(FAddressDataFile);
            for I := 0 to SList.Count - 1 do
            begin
              s := SList[i];
              TabPos := Pos(#9, s);
              OSM_ID := StrToInt64(Copy(s, 1, TabPos - 1));
              Delete(s, 1, TabPos);
              sName := s;
              if not FOsmNameCsvDict.TryGetValue(sHash, FDict) then
                FDict := TDictionary<Int64,string>.Create;
              FDict.AddOrSetValue(OSM_ID, sName);
              FOsmNameCsvDict.AddOrSetValue(sHash, FDict);
              inc(iCount);
            end;
            bFound := True;
          finally
            FAddressDataFile.Free;
            SList.Free;
  //          Dict.Free;
          end;
        except on E: Exception do
          begin
            ToLog('Ошибка чтения файла '+RootPath + 'osm.csv '+E.Message);
            TGeneral.IncWatchdog(CLOAD_ROUTE_NAME_ACCESS_DENIED);
            TGeneral.SendWatchdog;
            Exit(0);
          end;
        end;
      end;
    end;
  end;
  if iCount > 0 then
    ToLog('TMapEditor.LoadAddrOSMCsv ms='+IntToStr(GetTickCount-t) + ' Count='+IntToStr(iCount));
  Result := iCount;
end;

function TMapEditor.LoadAddrOSMHash(const AAccounts: TIntegerDynArray; AHashArr: TWayArray): Int64;
var
  FIdxArr : array of TAddrIdx;
  i : Integer;
  t: Cardinal;
  sHash, RootPath, sName: string;
  Acc, j: Integer;
  bFound: Boolean;
begin

  if not Assigned(FOsmNameDict) then
    FOsmNameDict := TDictionary<Int64,string>.Create;

  if FOsmNameDict.Count > 0 then
  begin
    Result := FOsmNameDict.Count;
    Exit;
  end;

  t := GetTickCount;
  j := High(AAccounts);
  for sHash in AHashArr do
  begin
    bFound := False;
    Acc := AAccounts[j];
    while (j >= Low(AAccounts)) and not bFound do
    begin
      RootPath := ExtractFileDir(GetModuleName(HInstance)) + PathDelim{D:\IGF\}
        + IntToStr(Acc) + PathDelim{\}
        + Copy(sHash, 1, 1){u} + PathDelim{\}
        + Copy(sHash, 1, 2){uc} + PathDelim{\}
        + Copy(sHash, 1, 3){ucf} + PathDelim{\};
      FIdxFileName := RootPath + 'osm.idx';
      FAddressDataFileName := RootPath + 'osm.dat';
      if (FileExists(FIdxFileName) and FileExists(FAddressDataFileName)) then
      try
        FIdxFile := TFileStream.Create(FIdxFileName, fmOpenRead + fmShareDenyNone);
        FAddressDataFile := TFileStream.Create(FAddressDataFileName, fmOpenRead + fmShareDenyNone);
        FAddressStreamReader := TStreamReader.Create(FAddressDataFile, TEncoding.UTF8, True);
        SetLength(FIdxArr, FIdxFile.Size div SizeOf(TAddrIdx));
        FIdxFile.ReadBuffer(FIdxArr[0], FIdxFile.Size);
        for I := 0 to High(FIdxArr) do
        begin
          if FIdxArr[i].Idx < FAddressDataFile.Size then
          begin
            FAddressStreamReader.DiscardBufferedData;
            FAddressDataFile.Position := FIdxArr[i].Idx;
            sName := Trim(FAddressStreamReader.ReadLine);
            FOsmNameDict.AddOrSetValue(FIdxArr[i].OsmID, sName);
          end;
        end;
        bFound := True;
        Dec(j);
      finally
        FIdxFile.Free;
        FAddressDataFile.Free;
        FAddressStreamReader.Free;
      end;
    end;

  end;
  ToLog('TMapEditor.LoadAddrOSMHash ms='+IntToStr(GetTickCount-t) + ' Count='+IntToStr(FOsmNameDict.Count));
  Result := FOsmNameDict.Count;
end;

function TMapEditor.MakeFewRoutesFromOne(APoints: array of TGeoPos;
  AWaySource: string; AMinDistToLineM: Integer; var AWays: TWayArray): boolean;
var
  Point, PointOnLine: TGeoPos;
  i, iArr, iPointNum: Integer;
  PointsArr1, PointsArr2: TGeoPosArray;
  PointsArrArr: array of TGeoPosArray;
  PointArrWay: TGeoPathPointArray;
  sWay: string;

begin
  try
    TGeoHash.DecodeArrayWorld(AWaySource, PointsArr1);
    SetLength(PointsArrArr, 1);
    SetLength(AWays, 0);
    PointsArrArr[High(PointsArrArr)] := PointsArr1;
    for Point in APoints do
    begin
      iArr := 0;
      iPointNum := -1;
      while (iArr < Length(PointsArrArr)) and (iPointNum = -1) do
      begin
        iPointNum := GetRoutePointNum(Point, PointsArrArr[iArr], AMinDistToLineM, PointOnLine);
        if iPointNum <> -1 then
        begin
          //нашли точку на дороге и разбиваем текущую на 2
          SetLength(PointsArr1, iPointNum + 2);
          for I := 0 to iPointNum do
            PointsArr1[i] := PointsArrArr[iArr][i];
          PointsArr1[High(PointsArr1)] := PointOnLine;

          SetLength(PointsArr2, Length(PointsArrArr[iArr]) - iPointNum);
          PointsArr2[0] := PointOnLine;
          for I := 1 to Length(PointsArrArr[iArr]) - iPointNum - 1 do
            PointsArr2[i] := PointsArrArr[iArr][iPointNum + i];

          SetLength(PointsArrArr, Length(PointsArrArr) + 1);
          PointsArrArr[iArr] := PointsArr1;
          PointsArrArr[High(PointsArrArr)] := PointsArr2;
        end;
        inc(iArr);
      end;
    end;
    for PointsArr1 in PointsArrArr do
    begin
      SetLength(PointArrWay, Length(PointsArr1));
      for I := Low(PointsArr1) to High(PointsArr1) do
        PointArrWay[i].p := PointsArr1[i];
      sWay := TGeoHash.EncodeArrayAnyFormat(PointArrWay, 12, gfPath);
      SetLength(AWays, Length(AWays) + 1);
      AWays[High(AWays)] := sWay;
    end;
    Result := True;
  except
    Result := False;
  end;
end;

function TMapEditor.MakeFewRoutesFromOneWithWay(APoints: TPointDict;
  AWaySource: string; AMinDistToLineM: Integer; var AWays: TWayArray): boolean;
var
  Point, PointOnLine: TGeoPos;
  i, iArr, iPointNum, iCurrPoint, iPosInArr: Integer;
  PointsArr1, PointsArr2, StartPointsArr: TGeoPosArray;
  PointsArrArr: array of TGeoPosArray;
  PointArrWay: TGeoPathPointArray;
  sWay, sStartWay: string;

begin
  try
    TGeoHash.DecodeArrayWorld(AWaySource, PointsArr1);
    SetLength(PointsArrArr, 1);
    SetLength(AWays, 0);
    PointsArrArr[High(PointsArrArr)] := PointsArr1;
    for Point in APoints.Keys do
    begin
      iCurrPoint := -1;
      APoints.TryGetValue(Point, iPosInArr);
      sStartWay := FNewRouteArr[iPosInArr].way;
      TGeoHash.DecodeArrayWorld(sStartWay, StartPointsArr);
      if Point = StartPointsArr[0] then
        iCurrPoint := 0;
      if Point = StartPointsArr[High(StartPointsArr)] then
        iCurrPoint := High(StartPointsArr);

      iArr := 0;
      iPointNum := -1;
      while (iArr < Length(PointsArrArr)) and (iPointNum = -1) do
      begin
        iPointNum := GetRoutePointNum(Point, PointsArrArr[iArr], AMinDistToLineM, PointOnLine);
        if iPointNum <> -1 then
        begin
          //нашли точку на дороге и разбиваем текущую на 2
          SetLength(PointsArr1, iPointNum + 2);
          for I := 0 to iPointNum do
            PointsArr1[i] := PointsArrArr[iArr][i];
          PointsArr1[High(PointsArr1)] := PointOnLine;

          SetLength(PointsArr2, Length(PointsArrArr[iArr]) - iPointNum);
          PointsArr2[0] := PointOnLine;
          for I := 1 to Length(PointsArrArr[iArr]) - iPointNum - 1 do
            PointsArr2[i] := PointsArrArr[iArr][iPointNum + i];

          SetLength(PointsArrArr, Length(PointsArrArr) + 1);
          PointsArrArr[iArr] := PointsArr1;
          PointsArrArr[High(PointsArrArr)] := PointsArr2;
        end;
        inc(iArr);
      end;
      if iCurrPoint = 0 then
      begin
        //дописываем точку в начало
        if (StartPointsArr[0].ToHash <> PointOnLine.ToHash) then
        begin
          SetLength(PointArrWay, Length(StartPointsArr) + 1);
          PointArrWay[0].p := PointOnLine;
          for I := 1 to High(PointArrWay) do
            PointArrWay[i].p := StartPointsArr[i-1];
          FNewRouteArr[iPosInArr].Way := TGeoHash.EncodeArrayAnyFormat(PointArrWay, 12, gfPath);
        end;
      end;
      if iCurrPoint > 0 then
      begin
      //дописываем точку в конец
        if (StartPointsArr[High(StartPointsArr)].ToHash <> PointOnLine.ToHash) then
        begin
          SetLength(PointArrWay, Length(StartPointsArr) + 1);
          PointArrWay[High(PointArrWay)].p := PointOnLine;
          for I := 0 to High(PointArrWay)-1 do
            PointArrWay[i].p := StartPointsArr[i];
          FNewRouteArr[iPosInArr].Way := TGeoHash.EncodeArrayAnyFormat(PointArrWay, 12, gfPath);
        end;
      end;
    end;
    for PointsArr1 in PointsArrArr do
    begin
      SetLength(PointArrWay, Length(PointsArr1));
      for I := Low(PointsArr1) to High(PointsArr1) do
        PointArrWay[i].p := PointsArr1[i];
      sWay := TGeoHash.EncodeArrayAnyFormat(PointArrWay, 12, gfPath);
      SetLength(AWays, Length(AWays) + 1);
      AWays[High(AWays)] := sWay;
    end;
    Result := True;
  except
    Result := False;
  end;
end;

function TMapEditor.MakeTwoRoutesFromOne(APoint: TGeoPos;
  AWaySource: string; var AWay1, AWay2: string): boolean;
var
  i, iPointNum: Integer;
  APointsArr: TGeoPosArray;
  APointArr1, APointArr2: TGeoPathPointArray;
  PointOnLine: TGeoPos;
begin
  Result := False;
  TGeoHash.DecodeArrayWorld(AWaySource, APointsArr);
  iPointNum := GetRoutePointNum(APoint, APointsArr, FMinDistToLineM, PointOnLine);
  if iPointNum = -1 then Exit;
  SetLength(APointArr1, iPointNum + 2);
  for I := 0 to iPointNum do
    APointArr1[i].p := APointsArr[i];
  APointArr1[High(APointArr1)].p := APoint;

  SetLength(APointArr2, Length(APointsArr) - iPointNum);
  APointArr2[0].p := APoint;
  for I := 1 to Length(APointsArr) - iPointNum - 1 do
    APointArr2[i].p := APointsArr[iPointNum + i];

  AWay1 := TGeoHash.EncodeArrayAnyFormat(APointArr1, 12, gfPath);
  AWay2 := TGeoHash.EncodeArrayAnyFormat(APointArr2, 12, gfPath);
  Result := True;

end;


function TMapEditor.ParseJson(AJsonStr: string): string;
var
  id: TIDLines;
  Json : TJsonObject;
  i, j, k, iUpdateType, iTimeout, iFormat,
  iRoadType, iSpeed, iLimit, iAccountID, iOneWay, iZTID, iOldZTID, AreaID: Integer;
  iZones, iSigns: UInt64;
  sName, s, sSign, sWay, sOldName, sAstarDll: string;
  iSet: TIntSet;
  bResult: Boolean;
  Points: TGeoPosArray;
  NewCrossArr: TNewCrossPointArray;
  NewCross: TNewCrossPoint;
  Zones: TZoneDictionary;
  ZoneInfo, ZoneInfoTemp: TZoneInfo;
  ZoneType: TZoneTypeDictionary;
  LandMarkArea: TLandMarkAreaDictionary;
  LandMarkAreaInfo, LandMarkAreaInfoTemp: TLandMarkAreaInfo;
  ZoneTypeInfo, ZoneTypeInfoTemp: TZoneTypeInfo;
  MinRealRouteLengthM, MinDistToLineM: Integer;
  FAccounts: TIntegerDynArray;
//  LandMarkMaxDist: Double;
begin
  FRootPath := IncludeTrailingPathDelimiter(ExtractFileDir(GetModuleName(HInstance)));

  iSet := [];
  bResult := True;
  Result := '{"Result":"Error"}';
  if AJsonStr = '' then Exit;
  sOldName := '';
  iOldZTID := -1;
  Json := TJsonObject.Parse(AJsonStr) as TJsonObject;
  try
    //читаем иерархию аккаунтов
    iAccountID := json.O['Account'].I['Current'];
    SetLength(FAccounts, 1);
    FAccounts[High(FAccounts)] := iAccountID;
    for i := 0 to json.O['Account'].A['Parents'].Count -1 do
    begin
      iAccountID := json.O['Account'].A['Parents'].I[i];
      SetLength(FAccounts, Length(FAccounts) + 1);
      FAccounts[High(FAccounts)] := iAccountID;
    end;
    TGeneral.FAccountsArch.AddOrSetValue(FAccounts[0], FAccounts);

    if json.S['Type'] = 'ClearAllRoutes' then
    begin
      if Length(FAccounts) > 1 then
      begin
        if ClearAllRoutes(FRootPath, FAccounts) then
          Result := '{"Result":"Ok"}'
        else
          Result := '{"Result":"Error"}';
      end else
  //если это рут, то не очищаем
        Result := '{"Result":"Error"}';
    end;
    if json.S['Type'] = 'GetSignTypes' then
    begin
      Result := FSign.LoadSignTypeFile(FRootPath).ToString;
      Exit;
    end;
    if json.S['Type'] = 'RecalcSigns' then
    begin
      if FSign.CreateSignAll(FRootPath, FAccounts) then
        Result := '{"Result":"Ok"}'
      else
        Result := '{"Result":"Error"}';
      Exit;
    end;
    if json.S['Type'] = 'GetPermittedZones' then
    begin
      Result := FZC.LoadZoneFile(FRootPath, FAccounts).ToString;
      Exit;
    end;
    if json.S['Type'] = 'GetPermittedZoneTypes' then
    begin
      Result := FZC.LoadZoneTypeFile(FAccounts).ToString;
      Exit;
    end;
    if json.S['Type'] = 'GetZonesProgress' then
    begin
      Result := '{"Result":"'+IntToStr(FZC.LoadProgress(FAccounts[0]))+'"}';
      Exit;
    end;
    if json.S['Type'] = 'RecalcZones' then
    begin
      if FZC.SaveAllZC(FRootPath, FAccounts) then
      begin
        TGeneral.SendAccountBorder(FAccounts, FRedisCli);
        Result := '{"Result":"Ok"}'
      end else
        Result := '{"Result":"Error"}';
      Exit;
    end;
    if json.S['Type'] = 'SavePermittedZones' then
    begin
      Zones := FZC.LoadOneZoneFile(FAccounts[0]);
      for i := 0 to json.A['Zones'].Count -1 do
      begin
        ZoneInfo.AccountID := FAccounts[0];
        iZones := json.A['Zones'].O[i].L['Type'];
        if iZones <> 0 then
          ZoneInfo.TypeID := Trunc(Log2(iZones))
        else
          ZoneInfo.TypeID := 0;
        ZoneInfo.Active := (json.A['Zones'].O[i].I['Active'] = 1);
        ZoneInfo.Color := json.A['Zones'].O[i].I['Color'];
        sName := json.A['Zones'].O[i].S['Name'];
        sOldName := json.A['Zones'].O[i].S['OldName'];
        ZoneInfo.AreaHash := json.A['Zones'].O[i].S['GeoHash'];
        if (Length(sName)>0) and (sName[1]='%') then
          sName := TIdURI.URLDecode(sName);
        if (Length(sOldName)>0) and (sOldName[1]='%') then
          sOldName := TIdURI.URLDecode(sOldName);
        if json.A['Zones'].O[i].S['UpdateType'] = 'Delete' then
        begin
          if Zones.TryGetValue(sName, ZoneInfoTemp) then
          begin
            SetAccountBorderZone(FAccounts[0], Now(), ZoneInfoTemp.AreaHash);
            Zones.Remove(sName);
          end else
          begin
            Result := '{"Result":"Error. Not Name = '+sName+'"}';
            Exit;
          end;
        end else
        if json.A['Zones'].O[i].S['UpdateType'] = 'Update' then
        begin
          if Zones.TryGetValue(sOldName, ZoneInfoTemp) then
          begin
            if (sOldName = sName) then
            begin
              if (ZoneInfoTemp.AreaHash <> ZoneInfo.AreaHash) or
                 (ZoneInfoTemp.TypeID <> ZoneInfo.TypeID) or
                 (ZoneInfoTemp.Active <> ZoneInfo.Active) then
              begin
                SetAccountBorderZone(FAccounts[0], Now(), ZoneInfoTemp.AreaHash);
                SetAccountBorderZone(FAccounts[0], Now(), ZoneInfo.AreaHash);
              end;
              Zones.AddOrSetValue(sName, ZoneInfo);
            end else
            if not Zones.TryGetValue(sName, ZoneInfoTemp) then
            begin
              if (ZoneInfoTemp.AreaHash <> ZoneInfo.AreaHash) or
                 (ZoneInfoTemp.TypeID <> ZoneInfo.TypeID) or
                 (ZoneInfoTemp.Active <> ZoneInfo.Active) then
              begin
                SetAccountBorderZone(FAccounts[0], Now(), ZoneInfoTemp.AreaHash);
                SetAccountBorderZone(FAccounts[0], Now(), ZoneInfo.AreaHash);
              end;
              Zones.Remove(sOldName);
              Zones.AddOrSetValue(sName, ZoneInfo);
            end else
            begin
              Result := '{"Result":"Error. Dublicate key Name = '+sName+'"}';
              Exit;
            end;
          end else
          begin
            Result := '{"Result":"Error. Not Name = '+sOldName+'"}';
            Exit;
          end;
        end else
        if json.A['Zones'].O[i].S['UpdateType'] = 'Add' then
        begin
          SetAccountBorderZone(FAccounts[0], Now(), ZoneInfo.AreaHash);
          Zones.AddOrSetValue(sName, ZoneInfo);
        end;
      end;
      if FZC.SaveZones(FAccounts[0], Zones) then
      begin
        if (json.A['Zones'].O[0].S['UpdateType'] = 'Add') or
           (json.A['Zones'].O[0].S['UpdateType'] = 'Update') then
        begin
          if FZC.CopyFilesForZone(FRootPath, FAccounts, sName) then
            Result := '{"Result":"Ok"}'
          else
            Result := '{"Result":"Error"}';
        end else
        Result := '{"Result":"Ok"}';
      end else
        Result := '{"Result":"Error"}';
      Exit;
    end;
    if json.S['Type'] = 'SavePermittedZoneTypes' then
    begin
      ZoneType := FZC.LoadOneZoneTypeFile(FAccounts[0]);
      for i := 0 to json.A['ZoneTypes'].Count -1 do
      begin
        ZoneTypeInfo.AccountID := FAccounts[0];
        iOldZTID := json.A['ZoneTypes'].O[i].I['OldID'];
        iZTID := json.A['ZoneTypes'].O[i].I['ID'];
        sName := json.A['ZoneTypes'].O[i].S['Name'];
        if (Length(sName)>0) and (sName[1]='%') then
          sName := TIdURI.URLDecode(sName);
        ZoneTypeInfo.Name := sName;
        if json.A['ZoneTypes'].O[i].S['UpdateType'] = 'Delete' then
        begin
          if ZoneType.TryGetValue(iZTID, ZoneTypeInfoTemp) then
            ZoneType.Remove(iZTID)
          else
          begin
            Result := '{"Result":"Error. Not ID = '+IntToStr(iZTID)+'"}';
            Exit;
          end;
        end else
        if json.A['ZoneTypes'].O[i].S['UpdateType'] = 'Update' then
        begin
          if ZoneType.TryGetValue(iOldZTID, ZoneTypeInfoTemp) then
          begin
            if iZTID = iOldZTID then
            begin
              ZoneType.AddOrSetValue(iZTID, ZoneTypeInfo);
            end else
            if not ZoneType.TryGetValue(iZTID, ZoneTypeInfoTemp) then
            begin
              ZoneType.Remove(iOldZTID);
              ZoneType.AddOrSetValue(iZTID, ZoneTypeInfo);
            end else
            begin
              Result := '{"Result":"Error. Dublicate key ID = '+IntToStr(iZTID)+'"}';
              Exit;
            end;
          end else
          begin
            Result := '{"Result":"Error. Not ID = '+IntToStr(iOldZTID)+'"}';
            Exit;
          end;
        end else
        if json.A['ZoneTypes'].O[i].S['UpdateType'] = 'Add' then
          ZoneType.AddOrSetValue(iZTID, ZoneTypeInfo);
      end;
      if FZC.SaveZoneTypes(FAccounts[0], ZoneType) then
        Result := '{"Result":"Ok"}'
      else
        Result := '{"Result":"Error"}';
      Exit;
    end;
    //------------------регионы-маяки
    if json.S['Type'] = 'GetAreasMini' then
    begin
      LandMarkArea := FLandMarkArea.LoadAreaFileMini(FRootPath, FAccounts, TGeneral.FDetailLog);
      Result := FLandMarkArea.GetAreas(LandMarkArea).ToString;
//      else
//        Result := '{"Result":"Error"}';
      Exit;
    end;
    if json.S['Type'] = 'GetAreas' then
    begin
      LandMarkArea := FLandMarkArea.LoadAreaFile(FRootPath, FAccounts);
      Result := FLandMarkArea.GetAreas(LandMarkArea).ToString;
//      else
//        Result := '{"Result":"Error"}';
      Exit;
    end;
    if json.S['Type'] = 'GetLandMarks' then
    begin
      LandMarkArea := FLandMarkArea.LoadAreaFile(FRootPath, FAccounts);
//      LandMarkMaxDist := json.F['LandMarkMaxDist'];
//      if LandMarkMaxDist = 0 then
//        LandMarkMaxDist := TGeneral.FLandMarkMaxDist;

//      LandMarkAreaInfo.LandMarks := FLandMarkArea.GetLandMarks(json.S['Polygon'], LandMarkMaxDist);
      LandMarkAreaInfo.LandMarksHash := TGeoHash.EncodeArrayWorld(LandMarkAreaInfo.LandMarks, gfPointsArray);
      Result := FLandMarkArea.GetLandMarksJson(LandMarkAreaInfo.LandMarksHash).ToString;
      Exit;
    end;
    if json.S['Type'] = 'GetAreaTracks' then
    begin
//      Result := FLandMarkArea.GetAreaTracks(FRootPath, FAccounts, json.S['GeoHashID']).ToString;
      Exit;
    end;
    if json.S['Type'] = 'CalcAreaTracks' then
    begin
(*      if FLandMarkArea.CalcTrackFiles(FRootPath, FAccounts, json.S['GeoHashID'], TGeneral.FDetailLog) then
        Result := FLandMarkArea.GetAreaTracks(FRootPath, FAccounts, json.S['GeoHashID']).ToString
      else
        Result := '{"Result":"Error"}';    *)
      Exit;
    end;
    if json.S['Type'] = 'SaveAreas' then
    begin
      LandMarkArea := FLandMarkArea.LoadAreaFile(FRootPath, FAccounts);
//      LandMarkArea := FLandMarkArea.LoadOneAreaFile(FRootPath, FAccounts[0]);
      for i := 0 to json.A['Areas'].Count - 1 do
      begin
        LandMarkAreaInfo.AccountID := FAccounts[0];
        AreaID := json.A['Areas'].O[i].I['ID'];
        LandMarkAreaInfo.GeoHashID := json.A['Areas'].O[i].S['GeoHashID'];
        sName := json.A['Areas'].O[i].S['Name'];
        LandMarkAreaInfo.PolygonHash := json.A['Areas'].O[i].S['Polygon'];
        LandMarkAreaInfo.LandMarksHash := json.A['Areas'].O[i].S['LandMarks'];
        if (Length(sName)>0) and (sName[1]='%') then
          sName := TIdURI.URLDecode(sName);
        LandMarkAreaInfo.Name := sName;
        if (json.A['Areas'].O[i].S['UpdateType'] = 'Calc') then
        begin
          if LandMarkArea.TryGetValue(AreaID, LandMarkAreaInfoTemp) then
          begin
            case LandMarkAreaInfoTemp.State of
              stInCalc://уже считает
                begin
                  Result := '{"Result":"Area calcing now"}';
                  Exit;
                end;
              stFullCalc://уже посчитано
                begin
                  Result := '{"Result":"Area not need calc"}';
                  Exit;
                end;
              else
                begin
                  Result := '{"Result":"Ok"}';
                  if FLandMarkArea.CalcTrackFiles(FRootPath, FAccounts,
                           AreaID, TGeneral.FCountCPUForLandMarkCalc, False,
                           TGeneral.FDetailLog) then
                  begin
                    Exit;
                  end;
                end;
            end;
          end else
          begin
            Result := '{"Result":"Error. Not ID = '+IntToStr(AreaID)+'"}';
            Exit;
          end;
        end else
        if (json.A['Areas'].O[i].S['UpdateType'] = 'FullCalc') then
        begin
          if LandMarkArea.TryGetValue(AreaID, LandMarkAreaInfoTemp) then
          begin
            Result := '{"Result":"Ok"}';
            FLandMarkArea.CalcTrackFiles(FRootPath, FAccounts,
                     AreaID, TGeneral.FCountCPUForLandMarkCalc, True,
                     TGeneral.FDetailLog);
            Exit;
          end else
          begin
            Result := '{"Result":"Error. Not ID = '+IntToStr(AreaID)+'"}';
            Exit;
          end;
        end else
        if json.A['Areas'].O[i].S['UpdateType'] = 'Delete' then
        begin
          if LandMarkArea.TryGetValue(AreaID, LandMarkAreaInfoTemp) then
          begin
            if FLandMarkArea.DelTrackFiles(FRootPath, FAccounts[0], LandMarkAreaInfoTemp.GeoHashID)  then
              LandMarkArea.Remove(AreaID)
            else
            begin
              Result := '{"Result":"Error. Can not delete ID = '+IntToStr(AreaID)+'"}';
              Exit;
            end;
          end else
          begin
            Result := '{"Result":"Error. Not ID = '+IntToStr(AreaID)+'"}';
            Exit;
          end;
        end else
        if json.A['Areas'].O[i].S['UpdateType'] = 'Update' then
        begin
//          LandMarkArea.AddOrSetValue(AreaID, LandMarkAreaInfo);
          if LandMarkArea.TryGetValue(AreaID, LandMarkAreaInfoTemp) then
          begin
            LandMarkAreaInfo := FLandMarkArea.CompareAreas(FRootPath, FAccounts, LandMarkAreaInfoTemp, LandMarkAreaInfo);
            LandMarkArea.AddOrSetValue(AreaID, LandMarkAreaInfo);
          end else
          begin
            Result := '{"Result":"Error. Not ID = '+IntToStr(AreaID)+'"}';
            Exit;
          end;
        end else
        if json.A['Areas'].O[i].S['UpdateType'] = 'Add' then
        begin
          TGeoHash.DecodeArrayWorld(LandMarkAreaInfo.LandMarksHash, LandMarkAreaInfo.LandMarks);
          LandMarkAreaInfo.State := stNoCalc;
//          LandMarkAreaInfo.LandMarks := FLandMarkArea.GetLandMarks(LandMarkAreaInfo.PolygonHash, LandMarkMaxDist);
//          LandMarkAreaInfo.LandMarksHash := TGeoHash.EncodeArrayWorld(LandMarkAreaInfo.LandMarks, 'PA');;
          LandMarkArea.AddOrSetValue(AreaID, LandMarkAreaInfo);
        end;
      end;
      if FLandMarkArea.SaveAreas(FRootPath, FAccounts[0], LandMarkArea, False) then
        Result := FLandMarkArea.GetAreasStateOk(LandMarkArea).ToString
//        Result := '{"Result":"Ok"}'
      else
        Result := '{"Result":"Error"}';
      Exit;
    end;
//------------------------------------AStar
    if json.S['Type'] = 'GetAstarPath' then
    begin
      sWay := '';
      iTimeout := json.I['Timeout'];
      iFormat := json.I['Format'];
      iSpeed := json.I['Speed'];
      iLimit := json.I['Limit'];
      iZones := json.L['Zones'];
      iSigns := json.L['Signs'];
      sAstarDll := json.S['AstarDll'];
      if sAstarDll = '' then
      begin
      {$IFDEF CPUX64}
        sAstarDll := 'AStar64x64.dll';
      {$ELSE}
        sAstarDll := 'AStar64.dll';
      {$ENDIF}
      end;
//        sAstarDll := 'AStar64.dll';
      Result := '{"Result":"Path error"}';
      sWay := json.S['Path'];
      if sWay = '' then Exit;
      if iTimeout = 0 then
        iTimeout := TGeneral.FAStarTimeout;
      if iFormat = 0 then
        iFormat := 1;
      // 1 - вывод по скоростям, 3 - по типам дорог
//      Result := GetAstarPath(sWay, iSpeed, iLimit, iFormat, iTimeout, iZones, iSigns, FAccounts);
      if TGeneral.FUseAStarDll then
        Result := GetAStarPath(sWay, iSpeed, iLimit, iFormat, iTimeout, iZones, iSigns, FAccounts, sAstarDll)
      else
        Result := GetAStarAcc(sWay, iSpeed, iLimit, iFormat, iTimeout, iZones, iSigns, FAccounts);
      Exit;
    end;
    if json.S['Type'] = 'GetCoverZones' then
    begin
      sWay := '';
      sWay := json.S['Bounds'];
      i := json.I['Depth'];
      if sWay = '' then Exit;
      Result := FZC.GetCoverZones(sWay, FAccounts, FRootPath, i).ToString;
      Exit;
    end;
    if json.S['Type'] = 'AnalyzeFactTrack' then
    begin
      sWay := '';
      Result := '{"Result":"Path error"}';
      sWay := json.S['Way'];
      MinRealRouteLengthM := json.I['MinRouteLength'];
      if MinRealRouteLengthM = 0 then
        MinRealRouteLengthM := TGeneral.FMinRealRouteLengthM;
      MinDistToLineM := json.I['MinDistToRoute'];
      if MinDistToLineM = 0 then
        MinDistToLineM := TGeneral.FMinDistToLineM;
      if sWay = '' then Exit;
      Result := CheckWay(FRootPath, FAccounts, sWay, MinRealRouteLengthM, MinDistToLineM, TGeneral.FAStarTimeout).ToString;
      Exit;
    end;
    if json.S['Type'] = 'UpdateRoads' then
    begin
      for i := 0 to json.A['Features'].Count -1 do
      begin
        iUpdateType := json.A['Features'].O[i].I['UpdateType'];
        case iUpdateType of
        0:
          begin
            sWay := json.A['Features'].O[i].S['Border'];
            for j := 0 to json.A['Features'].O[i].A['RoadType'].Count - 1 do
            begin
              s := json.A['Features'].O[i].A['RoadType'].S[j];
              iSet := iSet + [StrToInt(s)];
            end;
            TGeoHash.DecodeArrayWorld(sWay, Points);
            if Length(Points) = 2 then
            begin
              if iSet = [] then
              begin
                ToLog('Нет типов - нет дорог');
                Result := '{"Result":"Roads not found"}';
              end else

                sWay := GetRoutesFromRangeByType(FAccounts, Points[0].Latitude, Points[0].Longitude,
                                                         Points[1].Latitude, Points[1].Longitude,
                                                         iSet, 0).ToString;
                Result := sWay;
            end else
              Result := '{"Result":"Error"}';
            Exit;
          end;
        1: //обновление скорости и типа
          begin
            SetLength(FUpdateMinArr, 0);
            iRoadType := json.A['Features'].O[i].O['Properties'].I['RoadType'];
            iSpeed := json.A['Features'].O[i].O['Properties'].I['Speed'];
            iOneWay := json.A['Features'].O[i].O['Properties'].I['OneWay'];
            sName := json.A['Features'].O[i].O['Properties'].S['Name'];
            if (Length(sName)>0) and (sName[1]='%') then
              sName := TIDURI.URLDecode(sName)
            else
              sName := Utf8ToAnsi(RawByteString(sName));
            for j := 0 to json.A['Features'].O[i].O['Road'].A['ID'].Count - 1 do
            begin
              s := json.A['Features'].O[i].O['Road'].A['ID'].S[j];
              SetLength(FUpdateMinArr, Length(FUpdateMinArr) + 1);
              FUpdateMinArr[High(FUpdateMinArr)].ID := ID.StrToIDLines(s);
              FUpdateMinArr[High(FUpdateMinArr)].RoadType := iRoadType;
              FUpdateMinArr[High(FUpdateMinArr)].Speed := iSpeed;
              FUpdateMinArr[High(FUpdateMinArr)].OneWay := (iOneWay=0);
              FUpdateMinArr[High(FUpdateMinArr)].name := sName;
            end;
            Result := UpdateRoutesMinFromArray.ToString;
          end;
        2: //обновление скорости и типа и линии дорог без изменения крайних точек
          begin
            SetLength(FUpdateWayArr, 0);
            for j := 0 to json.A['Features'].O[i].A['Road'].Count - 1 do
            begin
              iRoadType := json.A['Features'].O[i].A['Road'].O[j].I['RoadType'];
              iSpeed := json.A['Features'].O[i].A['Road'].O[j].I['Speed'];
              iOneWay := json.A['Features'].O[i].A['Road'].O[j].I['OneWay'];
              sName := json.A['Features'].O[i].A['Road'].O[j].S['Name'];
              sWay := json.A['Features'].O[i].A['Road'].O[j].S['Way'];
              sSign := json.A['Features'].O[i].A['Road'].O[j].S['Signs'];
              s := json.A['Features'].O[i].A['Road'].O[j].S['ID'];
              if (Length(sName)>0) and (sName[1]='%') then
                sName := TIDURI.URLDecode(sName);
              SetLength(FUpdateWayArr, Length(FUpdateWayArr) + 1);
              FUpdateWayArr[High(FUpdateWayArr)].ID := ID.StrToIDLines(s);
              FUpdateWayArr[High(FUpdateWayArr)].RoadType := iRoadType;
              FUpdateWayArr[High(FUpdateWayArr)].Speed := iSpeed;
              FUpdateWayArr[High(FUpdateWayArr)].OneWay := iOneWay;
              FUpdateWayArr[High(FUpdateWayArr)].name := sName;
              FUpdateWayArr[High(FUpdateWayArr)].Way := sWay;
              FUpdateWayArr[High(FUpdateWayArr)].sign := sSign;
            end;
            Result := UpdateRoutesWayFromArray(FAccounts).ToString;
            ToLog('2. Update Road. '+Result);
          end;
        3: //удаление дорог
          begin
            SetLength(FDeleteArr, 0);
            for j := 0 to json.A['Features'].O[i].O['Road'].A['ID'].Count - 1 do
            begin
              s := json.A['Features'].O[i].O['Road'].A['ID'].S[j];
              SetLength(FDeleteArr, Length(FDeleteArr) + 1);
              FDeleteArr[Length(FDeleteArr)-1] := ID.StrToIDLines(s);
            end;
            if DeleteRoutesFromArray(FAccounts) then
              Result := '{"Result":"Ok"}'
            else
              Result := '{"Result":"Error"}';
          end;
        4:  //новая дорога
          begin
            SetLength(FNewRouteArr, 0);
            for j := 0 to json.A['Features'].O[i].A['Road'].Count - 1 do
            begin
              sName := json.A['Features'].O[i].A['Road'].O[j].S['Name'];
              sSign := json.A['Features'].O[i].A['Road'].O[j].S['Signs'];
              sWay := json.A['Features'].O[i].A['Road'].O[j].S['Way'];
              s := json.A['Features'].O[i].A['Road'].O[j].S['ID'];
              if (Length(sName)>0) and (sName[1]='%') then
                sName := TIDURI.URLDecode(sName);
              SetLength(NewCrossArr, 0);
              for k := 0 to json.A['Features'].O[i].A['Road'].O[j].A['Connections'].Count - 1 do
              begin
                NewCross.PointGW := '';
                NewCross.IDStr := '';
                NewCross.PointGW := json.A['Features'].O[i].A['Road'].O[j].A['Connections'].O[k].S['Point'];
                NewCross.IDStr := json.A['Features'].O[i].A['Road'].O[j].A['Connections'].O[k].S['ID'];
                if (NewCross.IDStr <> '') and (NewCross.PointGW <> '') then
                begin
                  SetLength(NewCrossArr, Length(NewCrossArr) + 1);
                  NewCrossArr[High(NewCrossArr)] := NewCross;
                end;
              end;
              SetLength(FNewRouteArr, Length(FNewRouteArr) + 1);
              FNewRouteArr[High(FNewRouteArr)].UserID := json.A['Features'].O[i].A['Road'].O[j].I['UserID'];
              FNewRouteArr[High(FNewRouteArr)].RoadType := json.A['Features'].O[i].A['Road'].O[j].I['RoadType'];
              FNewRouteArr[High(FNewRouteArr)].Speed := json.A['Features'].O[i].A['Road'].O[j].I['Speed'];
              FNewRouteArr[High(FNewRouteArr)].OneWay := (json.A['Features'].O[i].A['Road'].O[j].I['OneWay']=1);
              FNewRouteArr[High(FNewRouteArr)].name := sName;//json.A['Features'].O[i].A['Road'].O[j].S['Name'];
              FNewRouteArr[High(FNewRouteArr)].Way := json.A['Features'].O[i].A['Road'].O[j].S['Way'];
              FNewRouteArr[High(FNewRouteArr)].NewCross := NewCrossArr;
              FNewRouteArr[High(FNewRouteArr)].sign := sSign;
            end;
            Result := SaveNewRoutesFromArray(FAccounts).ToString;
             ToLog('4. New Road. '+Result);
          end;
        end;
      end;
    end;
  finally
    Json.Free;
  end;
end;

function TMapEditor.ReCalcRoutes(ARootPath: string; AAccounts: TIntegerDynArray;
AIDInfo1, AIDInfo2: TRouteInfoMini; AHashStr: string): Integer;
var
  i, k: Integer;
  HashStr1: string;
  IDInfoForDel: TRouteInfoMini;
  bNeedIncPoint, bFound : Boolean;
  Holder: THoldRec;
  Points, Points1, Points2: TGeoPosArray;
  Point: TGeoPos;
begin
  try
    HashStr1 := TGeoHash.ConvertBinToString(AIDInfo1.ID.HashEnd and CAreaHashMask, 5);
    Result := 0;
    if not FHoldRecDict.TryGetValue(HashStr1, FStartEdgeFiles) then
    begin
      TGFAdapter.Load(ARootPath, AAccounts, HashStr1, FStartEdgeFiles);
      FHoldRecDict.Add(HashStr1, FStartEdgeFiles);
      if Length(FStartEdgeFiles.EdgeBackward) = 0 then
        ToLog('function Load error backward ' + HashStr1);
    end;
    if (AIDInfo1.PointsCount = 2) and (AIDInfo2.PointsCount = 2) then
    begin
      if AIDInfo1.RoadType > AIDInfo2.RoadType then
      begin
        IDInfoForDel := AIDInfo1;
        Result := 1;
      end else
      if AIDInfo1.RoadType < AIDInfo2.RoadType then
      begin
        IDInfoForDel := AIDInfo2;
        Result := 2;
      end else
      if AIDInfo1.Speed < AIDInfo2.Speed then
      begin
        IDInfoForDel := AIDInfo1;
        Result := 1;
      end else
      begin
        IDInfoForDel := AIDInfo2;
        Result := 2;
      end;

      if not DeleteRoute(AAccounts, IDInfoForDel.ID, Points) then
      begin
        ToLog('Error delete, ID= '+IDInfoForDel.ID.ToString);
//        bCanSave := False;
        Result := 3;
      end;
      Exit;
    end;
     //выбираем с большим типом и делим
     bNeedIncPoint := False;
    if AIDInfo1.RoadType > AIDInfo2.RoadType then
    begin
    //делим дорогу со статусом ниже
      IDInfoForDel := AIDInfo1;
      bNeedIncPoint := IDInfoForDel.PointsCount = 2;
      Result := 1;
    end else
    if AIDInfo1.RoadType < AIDInfo2.RoadType then
    begin
    //делим дорогу со статусом ниже
      IDInfoForDel := AIDInfo2;
      bNeedIncPoint := IDInfoForDel.PointsCount = 2;
      Result := 2;
    end else
    begin
      if AIDInfo2.PointsCount > 2 then
      begin
        IDInfoForDel := AIDInfo2;
        bNeedIncPoint := IDInfoForDel.PointsCount = 2;
        Result := 2;
      end else
      if AIDInfo1.PointsCount >= 2 then
      begin
        IDInfoForDel := AIDInfo1;
        bNeedIncPoint := IDInfoForDel.PointsCount = 2;
        Result := 1;
      end;
    end;


    if not FHoldRecDict.TryGetValue(AHashStr, Holder) then
    begin
      ToLog('FUCK!!!');
      Result := 3;
      Exit;
    end;
    i := 0;
    bFound := False;
    k := -1;
    while (i < Length(Holder.EdgeForward)) and (not bFound) do
    begin
      if (IDInfoForDel.ID.OSM_ID = Holder.EdgeForward[i].ID) and
         (IDInfoForDel.ID.HashStart = Holder.EdgeForward[i].HashVector.HashFrom) and
         (IDInfoForDel.ID.HashEnd = Holder.EdgeForward[i].HashVector.HashTo) then
      begin
        k := i;
        bFound := True;
      end;
      inc(i);
    end;

    SetLength(Points1, 0);
    SetLength(Points2, 0);
    if not bNeedIncPoint then
    begin
      if Holder.EdgeForward[k].HashVector.HashFrom < Holder.EdgeForward[k].HashVector.HashTo then
      begin
        Point := Holder.Ways[Holder.EdgeForward[k].WayIndex];
        SetLength(Points1, 2);
        Points1[0] := Holder.EdgeForward[k].CoordsFrom;
        Points1[1] := Point;

        SetLength(Points2, Holder.EdgeForward[k].WayCount + 1);
        Points2[0] := Point;
        Points2[High(Points2)] := Holder.EdgeForward[k].CoordsTo;
        for I := 1 to Holder.EdgeForward[k].WayCount - 1  do
          Points2[i] := Holder.Ways[Holder.EdgeForward[k].WayIndex + i];
      end else
      begin //обратная дорога, точки наоборот считаем
        Point := Holder.Ways[Holder.EdgeForward[k].WayIndex + Holder.EdgeForward[k].WayCount - 1];
        SetLength(Points1, 2);
        Points1[0] := Point;
        Points1[1] := Holder.EdgeForward[k].CoordsTo;

        SetLength(Points2, Holder.EdgeForward[k].WayCount + 1);
        Points2[0] := Holder.EdgeForward[k].CoordsFrom;
        Points2[High(Points2)] := Point;
        for I := 1 to Holder.EdgeForward[k].WayCount - 1  do
          Points2[i] := Holder.Ways[Holder.EdgeForward[k].WayIndex + i - 1];
      end;
    end else
    begin
      Point.Latitude := (Holder.EdgeForward[k].CoordsFrom.Latitude +
                         Holder.EdgeForward[k].CoordsTo.Latitude)/2;
      Point.Longitude := (Holder.EdgeForward[k].CoordsFrom.Longitude +
                         Holder.EdgeForward[k].CoordsTo.Longitude)/2;
      SetLength(Points1, 2);
      Points1[0] := Holder.EdgeForward[k].CoordsFrom;
      Points1[1] := Point;
      SetLength(Points2, 2);
      Points2[0] := Point;
      Points2[1] := Holder.EdgeForward[k].CoordsTo;
    end;

    if DeleteRoute(AAccounts, IDInfoForDel.ID, Points) then
    begin
      if not SaveNewRoute1(AAccounts, IDInfoForDel.ID.OSM_ID,
                           IDInfoForDel.RoadType, IDInfoForDel.Speed, Points1, true) then
      begin
        ToLog('Error save Points1, ID= '+IDInfoForDel.ID.ToString);
//        bCanSave := False;
        Result := 3;
      end;
      if not SaveNewRoute1(AAccounts, IDInfoForDel.ID.OSM_ID,
                           IDInfoForDel.RoadType, IDInfoForDel.Speed, Points2, true) then
      begin
        ToLog('Error save Points2, ID= '+IDInfoForDel.ID.ToString);
//        bCanSave := False;
        Result := 3;
      end;
    end else
    begin
      ToLog('Error delete, ID= '+IDInfoForDel.ID.ToString);
//      bCanSave := False;
      Result := 3;
    end;
  finally
    Holder.Clear;
  end;
  //  FHoldRecDict.AddOrSetValue(AHashStr, Holder);

end;

function TMapEditor.ReverseWayArr(AWayArr: TGeoPosArray): TGeoPosArray;
var
  i, iSize: Integer;
begin
  iSize := Length(AWayArr);
  SetLength(Result, iSize);
  for I := 0 to iSize - 1 do
    Result[i] := AWayArr[iSize - 1 - i];
end;

function TMapEditor.ReverseWayString(AWay: string): string;
var
  i, iLen: integer;
begin
  Result := '';
  iLen := Length(AWay);
  if ((iLen - 2) mod 12) <> 0 then
    Exit;
  for I := 0 to ((iLen - 2) div 12) - 1 do
    Result := copy(AWay, i * 12 + 3, 12) + Result;

  Result := 'GW' + Result;
end;


function TMapEditor.GetNewID(const AAccounts: TIntegerDynArray): Int64;
var
  i, AccountID: Integer;
  FIni: TIniFile;
begin
  FIni := TIniFile.Create(ChangeFileExt(GetModuleName(HInstance) + TGeneral.FSuffix, '.ini'));
  try
    if Length(AAccounts) = 0 then
      AccountID := 0
    else
      AccountID := AAccounts[0];
    i := FIni.ReadInteger('users','u' + IntToStr(AccountID),0);
    inc(i);
    FIni.WriteInteger('users','u' + IntToStr(AccountID),i);
    Result := AccountID * (-1000000) - i;
  finally
    FIni.Free;
  end;
end;
end.
