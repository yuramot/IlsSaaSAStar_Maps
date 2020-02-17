unit AStar64.LandMark;

interface

uses
  SysUtils, Classes, Types, StrUtils, IOUtils, Math,
  System.Generics.Collections, System.Generics.Defaults,
  AStar64.Extra, AStar64.FileStructs, JsonDataObjects, Ils.Json.Names, Ils.Json.Utils,
  Geo.Pos, Geo.Calcs, Geo.Hash,
  slogsend;

const
  CIndexFileExt = '.idx';
  CRecalcFileExt = '.rec';
  CDataFileExt = '.dat';
  CAreaFile = 'Areas.txt';

type
  TLandMarkWayKey = packed record
    v: THashVector;
    z: UInt64;
  end;

  TLandMarkKeyArray = array of TLandMarkWayKey;

  TLandMarkIdxLenFileRecord = packed record
    k: TLandMarkWayKey;
    i: Int64;
    l: Double;
    RESERVED: Int64;
  end;

  TAreaStateType = (stNoCalc, stPartialCalc, stInCalc, stFullCalc);

  TWayLen = packed record
    Way: string;
    Len: Double;
  end;

  TZoneTrackDictionary = TDictionary<UInt64, TWayLen>;

  TLandMarkDictionary = TDictionary<THashVector, TZoneTrackDictionary>;

  TLandMarkAreaInfo = record
    AccountID: Integer;
    State: TAreaStateType; //состояние рассчитанности маршрутов
    GeoHashID: string;
    Name: string;
    PolygonHash: string;
    LandMarksHash: string;
    Polygon: TGeoPosArray;
    LandMarks: TGeoPosArray;
    LandMarkDict: TLandMarkDictionary;
  end;

  TLandMarkAreaDictionary  = class(TDictionary<Integer, TLandMarkAreaInfo>)
  public
    function Save(const ARootPath: string;
                  const AAccount: integer): Boolean;
  end;

  TGeoWayVectorHash = class(TDictionary<THashVector, Integer>)
  public
    function Parse(const AHash: string): Boolean;
  end;

  TLandMarkWay = class
  private
    FIdx: Integer;
    FGeoHashString: string;
    FGeoWayVectorHash: TGeoWayVectorHash;
    FDistance: Double;
    FConnectedNodeForward: TNode;
    FConnectedNodeBackward: TNode;
    function GetConnectedNode(Idx: TADir): TNode;
    procedure SetConnectedNode(Idx: TADir; const Value: TNode);
    function GetHashVector(Idx: Integer): THashVector;
  public
    property Idx: Integer read FIdx write FIdx;
    property ConnectedNode[Idx: TADir]: TNode read GetConnectedNode write SetConnectedNode;
    property GeoHash: string read FGeoHashString write FGeoHashString;
    property GeoWayVectorHash: TGeoWayVectorHash read FGeoWayVectorHash;
    property Distance: Double read FDistance;
    property HashVector[Idx: Integer]: THashVector read GetHashVector;
    procedure NewData(const AGeoHash: string; const ADistance: Double);
    procedure Clear;
    function IsEmpty: Boolean;
    constructor Create(const AGeoHash: string; const ADistance: Double); overload;
    constructor Create(const AIdx: Integer; const AGeoHash: string; const ADistance: Double); overload;
    destructor Destroy; override;
  end;

  TLandMarkMatrix = class(TDictionary<TLandMarkWayKey, TLandMarkWay>)
  private
    FIdxFN: string;
    FRecalcFN: string;
    FDatFN: string;
    FAccounts: TIntegerDynArray;
    FLandMarkWaysChoosen: TList<TLandMarkWayKey>;
  protected
    procedure ValueNotify(const Value: TLandMarkWay; Action: TCollectionNotification); override;
  public
    constructor Create(const ARootPath: string; const AFileName: string; const AAccounts: TIntegerDynArray; const AResDir: string); overload;
    constructor Create(const ARootPath: string; const AFileName: string); overload;

    destructor Destroy; override;
    function LoadChoosenWays: Integer;
    function GetAreaTracks(const ARootPath: string;
                           const AAccount: Integer;
                           const AGeoHashID: string): TLandMarkDictionary;
    function LoadIndex: Boolean;
    function Save: Integer;
    function SaveLandMarks(const ARootPath: string; const AAccount: integer; const AGeoHashID: string; ALandMarkDict: TLandMarkDictionary): Boolean;
    function SaveAreas(const ARootPath: string;
                       const AAccount: integer;
                       const AAreas: TLandMarkAreaDictionary;
                       const ASaveTacks: Boolean): Boolean;
    function GetKeysForRecalc: TLandMarkKeyArray;
    procedure CheckAndLoadLandMarks(const AFromLatitude, AFromLongitude, AToLatitude, AToLongitude: Double; const FZones: Int64);
    function CheckLandmarkWayConnection(const ANode: TNode; const ADir: TADir; var OKey: TLandMarkWayKey): Boolean;
    property LandMarkWaysChoosen: TList<TLandMarkWayKey> read FLandMarkWaysChoosen;
  end;

  TLandMarkPicker = class
  private
    FLandMarkMatrix: TLandMarkMatrix;
    FRootPath: string;
  public
    LMNW: Int64;
    LMSE: Int64;
    constructor Create(const AAccs: TIntegerDynArray; const ALatiFrom, ALongiFrom, ALatiTo, ALongiTo: Double; const AResDir: string = '');
    destructor Destroy; override;
    property LandMarkMatrix: TLandMarkMatrix read FLandMarkMatrix;
  end;

implementation

{ TLandMarkMatrix }

constructor TLandMarkMatrix.Create(const ARootPath: string; const AFileName: string; const AAccounts: TIntegerDynArray; const AResDir: string);
var
  i: Integer;
begin
  inherited Create;
  //
  FLandMarkWaysChoosen := TList<TLandMarkWayKey>.Create;
  FAccounts := AAccounts;
  for I := Low(AAccounts) to High(AAccounts) do
  begin
    if FileExists(ARootPath + IntToStr(AAccounts[i])+ PathDelim + AFileName + CIndexFileExt) then
    begin
      FIdxFN := ARootPath + IntToStr(AAccounts[i])+ PathDelim + AFileName + CIndexFileExt;
      FDatFN := ARootPath + IntToStr(AAccounts[i])+ PathDelim + AFileName + CDataFileExt;
      FRecalcFN := ARootPath + IntToStr(AAccounts[i])+ PathDelim + AFileName + CRecalcFileExt;
      Exit;
    end;
  end;
  if AResDir = '' then
  begin
    FIdxFN := ARootPath + IntToStr(AAccounts[0])+ PathDelim + AFileName + CIndexFileExt;
    FDatFN := ARootPath + IntToStr(AAccounts[0])+ PathDelim + AFileName + CDataFileExt;
    FRecalcFN := ARootPath + IntToStr(AAccounts[0])+ PathDelim + AFileName + CRecalcFileExt;
  end else
  begin
    FIdxFN := ARootPath + AResDir + PathDelim + AFileName + CIndexFileExt;
    FDatFN := ARootPath + AResDir + PathDelim + AFileName + CDataFileExt;
    FRecalcFN := ARootPath + AResDir + PathDelim + AFileName + CRecalcFileExt;
  end;
end;

destructor TLandMarkMatrix.Destroy;
begin
  FLandMarkWaysChoosen.Free;
  Clear;
  //
  inherited Destroy;
end;

function TLandMarkMatrix.GetAreaTracks(const ARootPath: string;
  const AAccount: Integer; const AGeoHashID: string): TLandMarkDictionary;
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
    IdxFileName := ARootPath + IntToStr(AAccount) + PathDelim + FileName + CIndexFileExt;
    DatFileName := ARootPath + IntToStr(AAccount) + PathDelim + FileName + CDataFileExt;
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
        if (LandMarkArr[i].k.v.HashFrom > 0) and (LandMarkArr[i].k.v.HashTo > 0) and (LandMarkArr[i].i < DatFile.Size) then
        begin
          DatFileReader.DiscardBufferedData;
          DatFile.Position := LandMarkArr[i].i;
          Way.Way := Trim(DatFileReader.ReadLine);
          Way.Len := 0;
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

function TLandMarkMatrix.GetKeysForRecalc: TLandMarkKeyArray;
var
  k: TLandMarkWayKey;
begin
  SetLength(Result, 0);
  for k in Keys do
  begin
    if (Items[k].GeoHash = '') then
    begin
      SetLength(Result, Length(Result) + 1);
      Result[High(Result)] := k;
    end;
  end;
end;

function TLandMarkMatrix.LoadIndex: Boolean;
var
  fsIdx, fsRec, fsDat: TFileStream;
  ir: TLandMarkIdxLenFileRecord;
  srDat: TStreamReader;
  HashStr: string;
begin
  if not FileExists(FIdxFN) then
    Exit(False);
  fsIdx := TFileStream.Create(FIdxFN, fmOpenRead + fmShareDenyNone);
  fsDat := TFileStream.Create(FDatFN, fmOpenRead + fmShareDenyNone);
  srDat := TStreamReader.Create(fsDat, TEncoding.ANSI, True);
  fsIdx.Seek(0, soFromBeginning);
  try
    while (fsIdx.Read(ir, SizeOf(ir)) = SizeOf(ir)) do
    begin
      if (ir.i < fsDat.Size) then
      begin
        srDat.DiscardBufferedData;
        fsDat.Position := ir.i;
        HashStr := Trim(srDat.ReadLine);
      end;
      if not ContainsKey(ir.k) then
        Add(ir.k, TLandMarkWay.Create(HashStr, ir.l));
    end;

    if FileExists(FRecalcFN) then
    begin
      fsRec := TFileStream.Create(FRecalcFN, fmOpenRead + fmShareDenyNone);
      fsRec.Seek(0, soFromBeginning);
      try
        while (fsRec.Read(ir, SizeOf(ir)) = SizeOf(ir)) do
          if not ContainsKey(ir.k) then
          begin
            if ir.RESERVED = 0 then
              Add(ir.k, TLandMarkWay.Create('',0))
            else
              Add(ir.k, TLandMarkWay.Create('fullcalc',0));
          end;
      finally
        fsRec.Free;
      end;
    end;
  finally
    fsIdx.Free;
    fsDat.Free;
    srDat.Free;
  end;
  Result := True;
end;


function TLandMarkMatrix.LoadChoosenWays: Integer;
var
  k: TLandMarkWayKey;
  fsDat: TFileStream;
  srDat: TStreamReader;
begin
  Result := 0;
  if not FileExists(FDatFN) then
    Exit;
  fsDat := TFileStream.Create(FDatFN, fmOpenRead + fmShareDenyNone);
  srDat := TStreamReader.Create(fsDat);
  try
    for k in FLandMarkWaysChoosen do
    begin
      srDat.DiscardBufferedData;
      fsDat.Position := Items[k].Idx;
      Items[k].NewData(srDat.ReadLine, Items[k].Distance);
      Inc(Result);
    end;
  finally
    srDat.Free;
    fsDat.Free;
  end;
end;

function TLandMarkMatrix.Save: Integer;
var
  fsIdx: TFileStream;
  fsDat: TFileStream;
  fsRec: TFileStream;
  swDat: TStreamWriter;
  ir: TLandMarkIdxLenFileRecord;
  k: TLandMarkWayKey;
begin
  ir.RESERVED := 0;
  Result := 0;
  if FileExists(FDatFN) then
    DeleteFile(FDatFN);
  if FileExists(FIdxFN) then
    DeleteFile(FIdxFN);
  if FileExists(FRecalcFN) then
    DeleteFile(FRecalcFN);
  fsDat := TFileStream.Create(FDatFN, fmCreate + fmOpenReadWrite + fmShareDenyNone);
  swDat := TStreamWriter.Create(fsDat);
  fsIdx := TFileStream.Create(FIdxFN, fmCreate + fmOpenReadWrite + fmShareDenyNone);
  fsRec := TFileStream.Create(FRecalcFN, fmCreate + fmOpenReadWrite + fmShareDenyNone);
  try
    fsIdx.Seek(0, soFromBeginning);
    fsRec.Seek(0, soFromBeginning);
    fsDat.Seek(0, soFromBeginning);
    for k in Keys do
    begin
      ir.RESERVED := 0;
      if Items[k].IsEmpty then
        Continue;
      if (Items[k].GeoHash = 'error') then
      begin
        ir.k := k;
        ir.i := 0;
        ir.l := 0;
        fsRec.Write(ir, SizeOf(ir));
        Continue;
      end;
      if (Items[k].GeoHash = 'fullcalc') then
      begin
        ir.k := k;
        ir.i := 0;
        ir.l := 0;
        ir.RESERVED := 1;
        fsRec.Write(ir, SizeOf(ir));
        Continue;
      end;
      ir.k := k;
      ir.i := fsDat.Position;
      ir.l := Items[k].Distance;
      swDat.WriteLine(Items[k].GeoHash);
//      swDat.WriteLine(IntToStr(k.v.HashFrom)+'-'+IntToStr(k.v.HashTo)+'-'+IntToStr(k.z)+' '+ Items[k].GeoHash);
      fsIdx.Write(ir, SizeOf(ir));
      Inc(Result);
    end;
  finally
    fsDat.Free;
    fsIdx.Free;
    fsRec.Free;
  end;
end;

function TLandMarkMatrix.SaveAreas(const ARootPath: string;
  const AAccount: integer; const AAreas: TLandMarkAreaDictionary;
  const ASaveTacks: Boolean): Boolean;
var
  List: TStringList;
  ID: Integer;
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
        AAreas.TryGetValue(ID, AreaInfo);
        with Json.A['Areas'].AddObject do
        begin
          I['ID'] := ID;
          S['Name'] := AreaInfo.Name;
          S['GeoHashID'] := AreaInfo.GeoHashID;
          I['State'] := Integer(AreaInfo.State);
          S['Polygon'] := AreaInfo.PolygonHash;
          S['LandMarks'] := AreaInfo.LandMarksHash;
          if ASaveTacks and Assigned(AreaInfo.LandMarkDict) and (AreaInfo.LandMarkDict.Count > 0) then
            SaveLandMarks(ARootPath, AAccount, AreaInfo.GeoHashID, AreaInfo.LandMarkDict);
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

function TLandMarkMatrix.SaveLandMarks(const ARootPath: string;
  const AAccount: integer; const AGeoHashID: string;
  ALandMarkDict: TLandMarkDictionary): Boolean;
var
  IdxFile, RecalcFile, DatFile: TFileStream;
  DatFileWriter: TStreamWriter;
//  RecalcFileName, IdxFileName, DatFileName, FileName: string;
  Way: TWayLen;
  HashVector: THashVector;
  ZoneTrackDict: TZoneTrackDictionary;
  Zone: UInt64;
  LandMarkIdx: TLandMarkIdxLenFileRecord;
begin
//  FileName := Copy(AGeoHashID,3,12) + '_' + Copy(AGeoHashID,15,12);
//  IdxFileName := ARootPath + IntToStr(AAccount) + PathDelim + FileName + '.idx';
//  DatFileName := ARootPath + IntToStr(AAccount) + PathDelim + FileName + '.dat';
  if FileExists(FIdxFN) then
    DeleteFile(PChar(FIdxFN));
  if FileExists(FDatFN) then
    DeleteFile(PChar(FDatFN));
  if FileExists(FRecalcFN) then
    DeleteFile(PChar(FRecalcFN));
  try
    IdxFile := TFileStream.Create(FIdxFN, fmCreate + fmOpenReadWrite);
    DatFile := TFileStream.Create(FDatFN, fmCreate + fmOpenReadWrite);
    RecalcFile := TFileStream.Create(FRecalcFN, fmCreate + fmOpenReadWrite);
    DatFileWriter := TStreamWriter.Create(DatFile);
    try
      IdxFile.Seek(0, soFromBeginning);
      RecalcFile.Seek(0, soFromBeginning);
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
          end else
          begin
            LandMarkIdx.i := 0;
            LandMarkIdx.k.v := HashVector;
            LandMarkIdx.k.z := Zone;
            LandMarkIdx.l := 0;
            RecalcFile.Write(LandMarkIdx, SizeOf(LandMarkIdx));
          end;
        end;
      end;
      Result := True;

    finally
      IdxFile.Free;
      RecalcFile.Free;
      DatFileWriter.Free;
      DatFile.Free;
    end;
  except
    Result := False;
  end;
end;

procedure TLandMarkMatrix.ValueNotify(const Value: TLandMarkWay; Action: TCollectionNotification);
begin
  if (Action = cnRemoved) then
    Value.Free;
end;

function TLandMarkMatrix.CheckLandmarkWayConnection(const ANode: TNode; const ADir: TADir; var OKey: TLandMarkWayKey): Boolean;
var
  k: TLandMarkWayKey;
  HashVector: THashVector;
begin
  Result := False;
  if (ADir = adForward) then
    HashVector := ANode.HashVector^
  else
    HashVector := ANode.HashVector^.Reverse;
  for k in FLandMarkWaysChoosen do
  begin
    if Items[k].GeoWayVectorHash.ContainsKey(HashVector) then
    begin
      Items[k].ConnectedNode[ADir] := ANode;
      if Assigned(Items[k].ConnectedNode[ADir])
      and Assigned(Items[k].ConnectedNode[Reverse(ADir)]) then
      begin
        OKey := k;
        Exit(True);
      end;
    end;
  end;
end;

constructor TLandMarkMatrix.Create(const ARootPath, AFileName: string);
var
  Accounts: TIntegerDynArray;
begin
  SetLength(Accounts, 1);
  Accounts[0] := 1;
  Create(ARootPath, AFileName, Accounts, ARootPath);
end;

procedure TLandMarkMatrix.CheckAndLoadLandMarks(const AFromLatitude, AFromLongitude, AToLatitude, AToLongitude: Double; const FZones: Int64);
var
  PathVector: THashVector;
  WayKey: TLandMarkWayKey;
  LastVector: THashVector;
  LastDistance: Double;
  I: Integer;
const
  CChoosenWaysMax = 8;
  CAzimuthTolerance = 10;
begin
  LoadIndex();
  PathVector.HashFrom := TGeoHash.EncodePointBin(AFromLatitude, AFromLongitude);
  PathVector.HashTo := TGeoHash.EncodePointBin(AToLatitude, AToLongitude);
  FLandMarkWaysChoosen.Clear();
  for WayKey in Keys do
  begin
    if ((WayKey.z and FZones) = 0) then
      FLandMarkWaysChoosen.Add(WayKey);
  end;
  if (FLandMarkWaysChoosen.Count = 0) then
    Exit;
//TODO: в конктно ЭТОЙ реализации нужно сначала отфильтровать по азимуту, а потом всё остальное (для скорости)
  FLandMarkWaysChoosen.Sort(TComparer<TLandMarkWayKey>.Construct(
    function(const Left, Right: TLandMarkWayKey): Integer
    begin
      if (Left.v.HashFrom < Right.v.HashFrom) then
        Result := -1
      else if (Left.v.HashFrom > Right.v.HashFrom) then
        Result := 1
      else if (Left.v.HashTo < Right.v.HashTo) then
        Result := -1
      else if (Left.v.HashTo > Right.v.HashTo) then
        Result := 1
      else
        Result := 0;
    end
  ));
  LastVector := FLandMarkWaysChoosen[0].v;
  LastDistance := Items[FLandMarkWaysChoosen[0]].Distance;
  I := 1;
  while (I < FLandMarkWaysChoosen.Count) do
  begin
    if (LastVector = FLandMarkWaysChoosen[I].v) then
    begin
      if (LastDistance < Items[FLandMarkWaysChoosen[I]].Distance) then
        FLandMarkWaysChoosen.Delete(I)
      else
      begin
        LastVector := FLandMarkWaysChoosen[I].v;
        LastDistance := Items[FLandMarkWaysChoosen[I]].Distance;
        FLandMarkWaysChoosen.Delete(I - 1);
      end;
    end
    else
    begin
      LastVector := FLandMarkWaysChoosen[I].v;
      LastDistance := Items[FLandMarkWaysChoosen[I]].Distance;
      Inc(I);
    end;
  end;
  FLandMarkWaysChoosen.Sort(TComparer<TLandMarkWayKey>.Construct(
    function(const Left, Right: TLandMarkWayKey): Integer
      function CalcDistance1(Key: TLandMarkWayKey): Double;
      var
        ds, dt: Double;
      begin
        ds := TGeoCalcs.GeoDistancePointToLineDeg(
          Key.v.PointFrom.Latitude, Key.v.PointFrom.Longitude, AFromLatitude, AFromLongitude, AToLatitude, AToLongitude
        );
        dt := TGeoCalcs.GeoDistancePointToLineDeg(
          Key.v.PointTo.Latitude, Key.v.PointTo.Longitude, AFromLatitude, AFromLongitude, AToLatitude, AToLongitude
        );
        Result := ds + dt;
      end;
    var
      dl, dr: Double;
    begin
      dl := CalcDistance1(Left);
      dr := CalcDistance1(Right);
      if (dl < dr) then
        Result := -1
      else if (dl > dr) then
        Result := 1
      else
        Result := 0;
    end
  ));
  I := 0;
  while (I < FLandMarkWaysChoosen.Count) do
  begin
    if not FLandMarkWaysChoosen[I].v.AzimuthFits(PathVector.Azimuth, CAzimuthTolerance) then
      FLandMarkWaysChoosen.Delete(I)
    else
      Inc(I);
  end;
  if (FLandMarkWaysChoosen.Count > CChoosenWaysMax) then
    FLandMarkWaysChoosen.DeleteRange(CChoosenWaysMax, FLandMarkWaysChoosen.Count - CChoosenWaysMax);
  LoadChoosenWays();
end;

{ TLandMarkWay }

constructor TLandMarkWay.Create(const AGeoHash: string; const ADistance: Double);
begin
  Create(-1, AGeoHash, ADistance);
end;

constructor TLandMarkWay.Create(const AIdx: Integer; const AGeoHash: string; const ADistance: Double);
begin
  inherited Create;
  //
  FGeoWayVectorHash := TGeoWayVectorHash.Create;
  FConnectedNodeForward := nil;
  FConnectedNodeBackward := nil;
  Idx := AIdx;
  NewData(AGeoHash, ADistance);
end;

destructor TLandMarkWay.Destroy;
begin
  FGeoWayVectorHash.Free;
  //
  inherited Destroy;
end;

procedure TLandMarkWay.Clear;
begin
  Idx := -1;
  FGeoHashString := '';
  FDistance := 0;
  FGeoWayVectorHash.Clear;
end;

function TLandMarkWay.GetHashVector(Idx: Integer): THashVector;
var
  i: Integer;
  s1: string;
  s2: string;
begin
  //LM
  //ucfq9fqxdkxe - 12
  //Z2B0000000000000000 - 19
  //ucfq9fmwpur5 - 12
  i := 3 + (Idx * (12 + 19));
  s1 := Copy(FGeoHashString, i, 12);
  s2 := Copy(FGeoHashString, i + (12 + 19), 12);
  Result := THashVector.Create(TGeoHash.DecodePointString(s1), TGeoHash.DecodePointString(s2));
end;

function TLandMarkWay.IsEmpty: Boolean;
begin
  Result := FGeoHashString = '';
end;

procedure TLandMarkWay.NewData(const AGeoHash: string; const ADistance: Double);
begin
  FDistance := ADistance;
  FGeoHashString := AGeoHash;
  FGeoWayVectorHash.Parse(FGeoHashString);
end;

function TLandMarkWay.GetConnectedNode(Idx: TADir): TNode;
begin
  case Idx of
    adForward: Result := FConnectedNodeForward;
    else Result := FConnectedNodeBackward;
  end;
end;

procedure TLandMarkWay.SetConnectedNode(Idx: TADir; const Value: TNode);
begin
  case Idx of
    adForward: if not Assigned(FConnectedNodeForward) then
      FConnectedNodeForward := Value;
    else if not Assigned(FConnectedNodeBackward) then
      FConnectedNodeBackward := Value;
  end;
end;

{ TGeoWayVectorHash }

function TGeoWayVectorHash.Parse(const AHash: string): Boolean;
const
  CLen = 12 + 19 + 12;
  CFormatIncrement = 12 + 19;
var
  v: THashVector;
  h1, h2: string;
  p1, p2: TGeoPos;
  sPos: Integer;
  Idx: Integer;
begin
  Clear;
  if (AHash = '') then
    Exit(False);
  Idx := 0;
  sPos := 3;
  while ((sPos + CLen) <= Length(AHash)) do
  begin
    h1 := Copy(AHash, sPos, 12);
    h2 := Copy(AHash, sPos + (12 + 19), 12);
    TGeoHash.DecodePointString(h1, p1.Latitude, p1.Longitude);
    TGeoHash.DecodePointString(h2, p2.Latitude, p2.Longitude);
    v := THashVector.Create(p1, p2);
    if not ContainsKey(v) then
      Add(v, Idx);
    Inc(Idx);
    Inc(sPos, CFormatIncrement);
  end;
  Result := True;
end;

{ TLandMarkPicker }

function QExcludeFileExt(const AFullName: string): string;
var
  I: Integer;
begin
  for I := Length(AFullName) downto 1 do
  begin
    if (AFullName[I] = '.') then
      Exit(Copy(AFullName, 1, I - 1));
  end;
  Result := AFullName;
end;

constructor TLandMarkPicker.Create(const AAccs: TIntegerDynArray; const ALatiFrom, ALongiFrom, ALatiTo, ALongiTo: Double; const AResDir: string);
var
  WorkName: string;
  AccPath: string;
  LatiNW, LongiNW, LatiSE, LongiSE: Double;
  CurMinSum, PrevMinSum: Double;
  FilesList: TStringDynArray;
  RezIdx: Integer;
  AccIter: Integer;
  I: Integer;
begin
  inherited Create;
  //
  if (Length(AAccs) = 0) then
    raise Exception.Create('Массив аккаунтов не должен быть пуст');
  FRootPath := ExtractFilePath(GetModuleName(HInstance));
  for AccIter in AAccs do
  begin
    AccPath := FRootPath + IntToStr(AccIter) + PathDelim;
    if not DirectoryExists(AccPath) then
      Continue;
    FilesList := TDirectory.GetFiles(AccPath, '*.idx');
    if (Length(FilesList) = 0) then
      Continue;
    RezIdx := -1;
    PrevMinSum := Infinity;
    for I := Low(FilesList) to High(FilesList) do
    begin
      FilesList[I] := QExcludeFileExt(FilesList[I]);
      WorkName := ExtractFileName(FilesList[I]);
      if (Length(WorkName) = 25)
      and TGeoHash.DecodePointString(Copy(WorkName, 1, 12), LatiNW, LongiNW)
      and TGeoHash.DecodePointString(Copy(WorkName, 14, 12), LatiSE, LongiSE) then
      begin
        if (ALatiFrom > LatiNW) or (ALatiFrom < LatiSE) or (ALatiTo > LatiNW) or (ALatiTo < LatiSE)
        or (ALongiFrom < LongiNW) or (ALongiFrom > LongiSE) or (ALongiTo < LongiNW) or (ALongiTo > LongiSE) then
          Continue;
        CurMinSum := (LatiNW - LatiSE) + (LongiSE - LongiNW);
        if (PrevMinSum > CurMinSum) then
        begin
          LMNW := TGeoHash.ConvertStringToBin(Copy(WorkName, 1, 12));
          LMSE := TGeoHash.ConvertStringToBin(Copy(WorkName, 14, 12));
          PrevMinSum := CurMinSum;
          RezIdx := I;
        end;
      end;
    end;
    if (RezIdx <> -1) then
    begin
      FLandMarkMatrix := TLandMarkMatrix.Create(AccPath, WorkName, AAccs, AResDir);
      Break;
    end;
  end;
end;

destructor TLandMarkPicker.Destroy;
begin
  FLandMarkMatrix.Free;
  //
  inherited Destroy;
end;

{ TLandMarkAreaDictionary }

function TLandMarkAreaDictionary.Save(const ARootPath: string;
                                      const AAccount: integer): Boolean;
var
  List: TStringList;
  ID: Integer;
  AreaInfo: TLandMarkAreaInfo;
  RootDir, AccDir: string;
  Json: TJsonObject;
begin
  Json := TJsonObject.Create;
  try
    List := TStringList.Create;
    try
      for ID in Keys do
      begin
        AreaInfo := Items[ID];
        with Json.A['Areas'].AddObject do
        begin
          I['ID'] := ID;
          S['Name'] := AreaInfo.Name;
          S['GeoHashID'] := AreaInfo.GeoHashID;
          I['State'] := Integer(AreaInfo.State);
          S['Polygon'] := AreaInfo.PolygonHash;
          S['LandMarks'] := AreaInfo.LandMarksHash;
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

end.


