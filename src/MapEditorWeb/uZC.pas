unit uZC;

interface

uses
  JsonDataObjects, Ils.Json.Names, Ils.Json.Utils, AStar64.FileStructs, Geo.Hash, Geo.Pos, AStar64.Typ,
  SysUtils, Classes, UFiles, Windows, Ils.Logger, System.IniFiles,
  System.Generics.Collections, System.Math, AStar64.Files, AStar64.Areas,
  AStar64.Extra, uGeneral, System.Types;

const
  CZoneFile = 'ImplementedZones.txt';
  CZoneTypesFile = 'ZoneTypes.txt';
  CHashChars: string = '0123456789bcdefghjkmnpqrstuvwxyz';
  CFindHashBottom = 5000;
  CFindHashRight = 5000;
  PathDelim = '\';


type

  TZoneInfo = record
    //! AccountID
    AccountID: Integer;
    //! номер (0-63)
    TypeID: Byte;
    //! используется/не используется
    Active: Boolean;
    //! цвет
    Color: integer;
    //! текстовый гео-хэш
    AreaHash: string;
    //! бинарное представление
    Area: TGeoPosArray;
  end;

  TZoneTypeInfo = record
    //! AccountID
    AccountID: Integer;
    //! текстовый гео-хэш
    Name: string;
  end;

  TZoneDictionary = TDictionary<string,TZoneInfo>;

  TZoneTypeDictionary = TDictionary<Byte,TZoneTypeInfo>;

  TFileNameArray = TArray<string>;

  TZC = class
  private
    FZoneDict: TZoneDictionary;
    FZoneType: TZoneTypeDictionary;
    FFileNameArr: TDictionary<string,Integer>;
    FProgress: Integer;
    FDetailLog: Boolean;
    FRequestNo: Cardinal;
  public
    constructor Create(ARequestNo: Cardinal; ADetailLog: Boolean = False); overload;
    destructor Destroy(); override;
    class procedure Stop;
    function LoadOneZoneFile(AAccount: integer; ADetailLog: Boolean = false): TZoneDictionary;
    function LoadOneZoneTypeFile(AAccount: integer; ADetailLog: Boolean = false): TZoneTypeDictionary;

    function LoadZoneFile(const ARootPath: string;
                                const AAccounts: TIntegerDynArray; ADetailLog: Boolean = false): TJsonObject;
    function LoadZoneTypeFile(AAccounts: TIntegerDynArray; ADetailLog: Boolean = false): TJsonObject;
    function LoadProgress(AAccount: integer; ADetailLog: Boolean = false): Integer;
    procedure SaveProgress(AAccount: integer);

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
                               const AZoneName: string; ADetailLog: Boolean = false): Boolean;
  end;


implementation

uses
  UMapEditor;
{ TZC }

function TZC.CheckArea(const AArea: TGeoPosArray;
  const APoint: TGeoPos; ADetailLog: Boolean): Boolean;
var
  A, B: TGeoPos;
  PrevUnder, CurrUnder: Boolean;
  T: Double;
  Prev: Integer;
  I: Integer;
begin
  Result := False;
  if (Length(AArea) < 3) then Exit;
  Prev := High(AArea);
  PrevUnder := AArea[Prev].Latitude < APoint.Latitude;
  for I := Low(AArea) to High(AArea) do
  begin
    CurrUnder := AArea[I].Latitude < APoint.Latitude;
    A.Latitude := AArea[Prev].Latitude - APoint.Latitude;
    A.Longitude := AArea[Prev].Longitude - APoint.Longitude;
    B.Latitude := AArea[I].Latitude - APoint.Latitude;
    B.Longitude := AArea[I].Longitude - APoint.Longitude;
    T := A.Longitude * (B.Latitude - A.Latitude) - A.Latitude * (B.Longitude - A.Longitude);
    if CurrUnder and (not PrevUnder) then
    begin
      if (T > 0) then
        Result := not Result;
    end;
    if (not CurrUnder) and PrevUnder then
    begin
      if (T < 0) then
        Result := not Result;
    end;
    Prev := I;
    PrevUnder := CurrUnder;
  end;
end;

function TZC.CheckFilesExists(const ARootPath: string;
                                    const AAccount: Integer;
                                    const AHash: string; ADetailLog: Boolean): boolean;
var
  RootPath: string;
begin
  RootPath := ARootPath{D:\IGF\}
      + IntToStr(AAccount) + PathDelim{\}
      + Copy(AHash, 1, 1){u} + PathDelim{\}
      + Copy(AHash, 1, 2){uc} + PathDelim{\}
      + Copy(AHash, 1, 3){ucf} + PathDelim{\}
      + Copy(AHash, 1, 4){ucft} + PathDelim{\};
  Result := FileExists(RootPath + AHash + CFileType[ftEdgeF])
    and FileExists(RootPath + AHash + CFileType[ftEdgeB])
    and FileExists(RootPath + AHash + CFileType[ftListF])
    and FileExists(RootPath + AHash + CFileType[ftListB])
    and FileExists(RootPath + AHash + CFileType[ftWay]);
end;

function TZC.CheckFilesExists1(const ARootPath: string;
  const AAccount: Integer; const AHash: string; ADetailLog: Boolean): boolean;
var
  RootPath: string;
begin
  RootPath := ARootPath{D:\IGF\}
      + IntToStr(AAccount) + PathDelim{\}
      + Copy(AHash, 1, 1){u} + PathDelim{\}
      + Copy(AHash, 1, 2){uc} + PathDelim{\}
      + Copy(AHash, 1, 3){ucf} + PathDelim{\}
      + Copy(AHash, 1, 4){ucft} + PathDelim{\};
  Result := FileExists(RootPath + AHash + CFileType[ftEdgeF]);
end;

function TZC.CheckFilesExists2(const ARootPath: string;
  const AAccount: Integer; const AHash: string): boolean;
var
  RootPath: string;
  iLen, i : Integer;
begin
  iLen := Length(AHash);
  if iLen = 5 then
  begin
    RootPath := ARootPath{D:\IGF\}
        + IntToStr(AAccount) + PathDelim{\}
        + Copy(AHash, 1, 1){u} + PathDelim{\}
        + Copy(AHash, 1, 2){uc} + PathDelim{\}
        + Copy(AHash, 1, 3){ucf} + PathDelim{\}
        + Copy(AHash, 1, 4){ucft} + PathDelim{\};
    Result := FileExists(RootPath + AHash + CFileType[ftEdgeF]);
  end else
  begin
    RootPath := '';
    for I := 1 to iLen do
      RootPath := RootPath + Copy(AHash, 1, i){u} + PathDelim{\};
    RootPath := ARootPath{D:\IGF\}
        + IntToStr(AAccount) + PathDelim{\}
        + RootPath;
    Result := DirectoryExists(RootPath);
  end;
end;


function TZC.CheckRouteInThroughArea(const AZone, ARoutePoints: TGeoPosArray;
  ADetailLog: Boolean): Boolean;
var
  I, J: Integer;
begin
  Result := False;
  if (Length(AZone) < 3) then Exit;
  j := 0;
  while (J < Length(ARoutePoints)) and not Result do
  begin
    Result := Result or CheckArea(AZone, ARoutePoints[j]);
    inc(j);
  end;

  if Result then Exit;

  //точки не в зоне, проверяем пересечения отрезков
  i := 0;
  while (i < Length(ARoutePoints) - 1) and not Result do
  begin
    j := 0;
    while (j < Length(AZone) - 1) and not Result do
    begin
      Result := Result or Intersection(ARoutePoints[i],ARoutePoints[i+1],
                                       AZone[j],AZone[j+1]);
      inc(j);
    end;
    inc(i);
  end;
end;

function TZC.CopyFilesForZone(const ARootPath: string;
  const AAccounts: TIntegerDynArray; const AZoneName: string; ADetailLog: Boolean): Boolean;
var
  ZoneInfo: TZoneInfo;
  J: Integer;
  TopLeft, BottomRight: TGeoPos;
  AreaArray : TFileNameArray;
  sName: string;
  Holder: THoldRec;
begin
  Result := False;
  if Length(AAccounts) = 1 then
    Exit(True);
  try
    if ADetailLog then
      ToLog('TZC.CopyFilesForZone. Считаем файлы и копируем');
    // читаем командный файл - заполняем массив
    LoadZoneFile(ARootPath, AAccounts);
    // превращаем текст в числа
    FZoneDict.TryGetValue(AZoneName, ZoneInfo);
    TGeoHash.DecodeArrayWorld(ZoneInfo.AreaHash, ZoneInfo.Area);
    FZoneDict.AddOrSetValue(AZoneName, ZoneInfo);
    // выявляем крайние точки
    TopLeft := ZoneInfo.Area[0];
    BottomRight := ZoneInfo.Area[0];
    for J := Low(ZoneInfo.Area) + 1 to High(ZoneInfo.Area) do
    begin
      if ZoneInfo.Area[j].Latitude > TopLeft.Latitude then
        TopLeft.Latitude := ZoneInfo.Area[j].Latitude;
      if ZoneInfo.Area[j].Longitude < TopLeft.Longitude then
        TopLeft.Longitude := ZoneInfo.Area[j].Longitude;

      if ZoneInfo.Area[j].Latitude < BottomRight.Latitude then
        BottomRight.Latitude := ZoneInfo.Area[j].Latitude;
      if ZoneInfo.Area[j].Longitude > BottomRight.Longitude then
        BottomRight.Longitude := ZoneInfo.Area[j].Longitude;
    end;
//    TopLeftHash := TopLeft.ToHash;
//    BottomRightHash := BottomRight.ToHash;
    //получаем список файлов
    AreaArray := GetFilesNameArray(TopLeft, BottomRight);
    //копируем файлы от родителей
    if ADetailLog then
      ToLog('TZC.CopyFilesForZone. Копируем файлы для зон. Всего - '+IntToStr(Length(AreaArray)));

    for sName in AreaArray do
    begin
      if TGeneral.FStop then
      begin
        ToLog('TZC.CopyFilesForZone. Прервано пользователем');
        Exit;
      end;
      TGFAdapter.Load(ARootPath, AAccounts, sName, Holder);
      TGFAdapter.Save(ARootPath, AAccounts[0], sName, Holder, C7FileTypes);
    end;
    // прогресс
    Result := True;
  except
    // NULL
  end;
end;

constructor TZC.Create(ARequestNo: Cardinal; ADetailLog: Boolean);
begin
  FDetailLog := ADetailLog;
  FRequestNo := ARequestNo;
  FZoneDict := TDictionary<string,TZoneInfo>.Create;
  FZoneType := TDictionary<Byte,TZoneTypeInfo>.Create;
  if FDetailLog then
    ToLog('TZC Start');
end;

function TZC.CreateZCAll(const ARootPath: string;
                               const AAccounts: TIntegerDynArray; ADetailLog: Boolean): Boolean;
var
  i: Integer;
  AccPathStr, sFileName, AFileName, FP: string;
begin
  try
    Result := True;
    ToLog('Request # '+ IntToStr(FRequestNo) + ' TZC.CreateZCAll. Start create zones new. Account='+IntToStr(AAccounts[0]));
    FProgress := 0;
    SaveProgress(AAccounts[0]);
    if DeleteAllZC(ARootPath, AAccounts) then
    begin
      FillFileNameArray;
      AccPathStr := IntToStr(AAccounts[0]) + PathDelim{\};
      i := 0;
      for sFileName in FFileNameArr.Keys do
      begin
        if TGeneral.FStop then
        begin
          ToLog('Request # '+ IntToStr(FRequestNo) + ' TZC.CreateZCAll. Прервано пользователем. Account='+IntToStr(AAccounts[0]));
          Exit(False);
        end;

        FP := ARootPath{D:\IGF\}
          + AccPathStr
          + Copy(sFileName, 1, 1){u} + PathDelim{\}
          + Copy(sFileName, 1, 2){uc} + PathDelim{\}
          + Copy(sFileName, 1, 3){ucf} + PathDelim{\}
          + Copy(sFileName, 1, 4){ucft} + PathDelim{\};
        AFileName := FP + sFileName;
        Inc(i);
        if not FileExists(AFileName+CFileType[ftEdgeF]) then
          Continue;

        Result := Result and SaveOneZC(AFileName+CFileType[ftEdgeF], AFileName+CFileType[ftZCF]);
        Result := Result and SaveOneZC(AFileName+CFileType[ftEdgeB], AFileName+CFileType[ftZCB]);
        if not Result then
        begin
          ToLog('Request # '+ IntToStr(FRequestNo) + ' TZC.CreateZCAll. Ошибка сохранения файлов зон');
          TGeneral.IncWatchdog(CSAVE_ALL_ZONE_FILES_ERROR);
          TGeneral.SendWatchdog;
          Exit(False);
        end;

        if Trunc(i*100/FFileNameArr.Count) <> FProgress then
        begin
          FProgress := Trunc(i*100/FFileNameArr.Count);
          SaveProgress(AAccounts[0]);
        end;
      end;
      ToLog('Request # '+ IntToStr(FRequestNo) + ' TZC.CreateZCAll. Finish create zones new. Files count = ' + IntToStr(FFileNameArr.Count) + ' Account='+IntToStr(AAccounts[0]));
      FProgress := 100;
      SaveProgress(AAccounts[0]);
    end;
  except
    Result := False;
  end;
end;

function TZC.DeleteAllZC(const ARootPath: string;
                               const AAccounts: TIntegerDynArray; ADetailLog: Boolean): Boolean;
var
  I, J, K, L: Char;
  Acc: Integer;
  HashPathStr: string;
  FullPath: string;
  SR: TSearchRec;
begin
  try
    ToLog('Request # '+ IntToStr(FRequestNo) + ' TZC.DeleteAllZC. Start delete all zc');
    Acc := AAccounts[0];
    for I in CHashChars do
    begin
      HashPathStr := I + '\';
      FullPath := ARootPath + IntToStr(Acc) + PathDelim + HashPathStr;
      if not DirectoryExists(FullPath) then Continue;
      ToLog('Request # '+ IntToStr(FRequestNo) + ' TZC.DeleteAllZC.Delete ZC files in folder ' + FullPath + '..');
      for J in CHashChars do
      begin
        HashPathStr := I + '\' + I + J + '\';
        FullPath := ARootPath + IntToStr(Acc) + PathDelim+ {'igf\' + }HashPathStr;
        if TGeneral.FStop then
        begin
          ToLog('Request # '+ IntToStr(FRequestNo) + ' DeleteAllZC. Прервано пользователем');
          Exit(False);
        end;
        if not DirectoryExists(FullPath) then Continue;
        for K in CHashChars do
        begin
          HashPathStr := I + '\' + I + J + '\' + I + J + K + '\';
          FullPath := ARootPath + IntToStr(Acc) + PathDelim+ {'igf\' + }HashPathStr;
          if TGeneral.FStop then
          begin
            ToLog('Request # '+ IntToStr(FRequestNo) + ' DeleteAllZC. Прервано пользователем');
            Exit(False);
          end;
         if not DirectoryExists(FullPath) then Continue;
          for L in CHashChars do
          begin
            //              u\uc\ucf\ucft\
            HashPathStr := I + '\' + I + J + '\' + I + J + K + '\' + I + J + K + L + '\';
            FullPath := ARootPath + IntToStr(Acc) + PathDelim+ {'igf\' + }HashPathStr;
            if not DirectoryExists(FullPath) then Continue;

            if TGeneral.FStop then
            begin
              ToLog('Request # '+ IntToStr(FRequestNo) + ' DeleteAllZC. Прервано пользователем');
              Exit(False);
            end;
            // составляем список файлов edge_f
            try
              if (SysUtils.FindFirst(FullPath + '*.zc_f', faNormal, SR) = 0) then
              begin
                repeat
                  if (SR.Name = '') then Continue;
                  if not TFileHandler.DeleteFile(FullPath + SR.Name) then Continue;
                  if not TFileHandler.DeleteFile(ChangeFileExt(FullPath + SR.Name, '.zc_b')) then Continue;
                until (SysUtils.FindNext(SR) <> 0);
                SysUtils.FindClose(SR);
              end;
            except on E : Exception do
              ToLog('TZC.DeleteAllZC. Error:'+E.Message);
            end;
          end; // K
        end; // L
      end; // J
    end; // I
    ToLog('Request # '+ IntToStr(FRequestNo) + ' TZC.DeleteAllZC. Finish delete all zc for Account=' + IntToStr(Acc));
    Result := True;
  except
    Result := False;
  end;
end;

destructor TZC.Destroy;
begin
  FZoneDict.Free;
  FZoneType.Free;
  FFileNameArr.Free;
  if FDetailLog then
    ToLog('TZC Stop');
  inherited;
end;

function TZC.FillFileNameArray: integer;
var
  J: Integer;
  TopLeft, BottomRight: TGeoPos;
  AreaArray : TFileNameArray;
  sName: string;
  ZoneInfo: TZoneInfo;
begin
  ToLog('Request # '+ IntToStr(FRequestNo) + ' TZC.FillFileNameArray. Start fill filename array');
  FFileNameArr := TDictionary<string,Integer>.Create;
  for sName in FZoneDict.Keys do
  begin
    FZoneDict.TryGetValue(sName, ZoneInfo);
    TopLeft := ZoneInfo.Area[0];
    BottomRight := ZoneInfo.Area[0];
    for J := Low(ZoneInfo.Area) + 1 to High(ZoneInfo.Area) do
    begin
      if ZoneInfo.Area[j].Latitude > TopLeft.Latitude then
        TopLeft.Latitude := ZoneInfo.Area[j].Latitude;
      if ZoneInfo.Area[j].Longitude < TopLeft.Longitude then
        TopLeft.Longitude := ZoneInfo.Area[j].Longitude;

      if ZoneInfo.Area[j].Latitude < BottomRight.Latitude then
        BottomRight.Latitude := ZoneInfo.Area[j].Latitude;
      if ZoneInfo.Area[j].Longitude > BottomRight.Longitude then
        BottomRight.Longitude := ZoneInfo.Area[j].Longitude;
    end;
    AreaArray := GetFilesNameArray(TopLeft, BottomRight);
    for J := Low(AreaArray) to High(AreaArray) do
      FFileNameArr.AddOrSetValue(AreaArray[j],j);
  end;
  ToLog('Request # '+ IntToStr(FRequestNo) + ' TZC.FillFileNameArray. Size array = ' + IntToStr(FFileNameArr.Count));
  Result := FFileNameArr.Count;
end;


function TZC.GetCoverZones(const ABounds: string;
                                 const AAccounts: TIntegerDynArray;
                                 const ARootPath: string;
                                 const ADepth: integer; ADetailLog: Boolean): TJsonObject;
var
  Points: TGeoPosArray;
  i, j, iDepth, iAccount: Integer;
  iT: Cardinal;
  HashStart, HashEnd: Int64;
  HashStartStr, HashEndStr: string;
  s: string;
  HashStr: string;
  AreaArray : TFileNameArray;
  CoverDict: TDictionary<string,Integer>;
begin
  ToLog('Request # '+ IntToStr(FRequestNo) + ' GetCoverZones. Account='+IntToStr(AAccounts[0]));
  TGeoHash.DecodeArrayWorld(ABounds, Points);
  Points := TGeneral.GetCorrectBorder(Points);
  CoverDict := TDictionary<string,Integer>.Create;
  try
    Result := TJsonObject.Create;
    if Length(Points) = 2 then
    begin
      HashStart := Points[0].ToHash;
      HashEnd := Points[1].ToHash;
      HashStartStr := TGeoHash.ConvertBinToString(HashStart and CAreaHashMask, 5);
      HashEndStr := TGeoHash.ConvertBinToString(HashEnd and CAreaHashMask, 5);
      if ADepth = -1 then
      begin
        iDepth := 0;
        for I := 1 to 5 do
        begin
          if HashStartStr[i] = HashEndStr[i] then
          begin
            inc(iDepth);
          end;
        end;
      end else
        iDepth := ADepth;
      iT := GetTickCount;
      AreaArray := GetFilesNameArray(Points[0], Points[1]);
      ToLog('Формирование списка файлов='+IntToStr(GetTickCount - iT)+' файлов='+IntToStr(Length(AreaArray))+' Account='+IntToStr(AAccounts[0]));
      iT := GetTickCount;
      for j := 0 to High(AAccounts) do
      begin
        for I := 0 to High(AreaArray) do
        begin
          if TGeneral.FStop then
          begin
            ToLog('Request # '+ IntToStr(FRequestNo) + ' TZC.GetCoverZones. Прервано пользователем. Account='+IntToStr(AAccounts[0]));
            Exit;
          end;

          if iDepth <> 5 then
            s := Copy(AreaArray[i],1,iDepth+1)
          else
            s := AreaArray[i];
          if (not CoverDict.ContainsKey(s)) and
             CheckFilesExists2(ARootPath, AAccounts[j], s) then
          begin
            CoverDict.AddOrSetValue(s,AAccounts[j]);
          end;
        end;
      end;
      for HashStr in CoverDict.Keys do
      begin
        CoverDict.TryGetValue(HashStr, iAccount);
        with Result.A['Files'].AddObject do
        begin
          I['AccountID'] := iAccount;
          S['File'] := HashStr;
        end;
      end;
      ToLog('Проверка списка файлов='+IntToStr(GetTickCount - iT)+ ' Account='+IntToStr(AAccounts[0]));
    end else
      Result.A['Files'].Clear;
  finally
    CoverDict.Free;
  end;
end;

function TZC.GetFilesNameArray(ATopLeft, ABottomRight: TGeoPos;
  ADetailLog: Boolean): TFileNameArray;
var
  TopLeft, BottomRight: TGeoPos;
begin
  //корректируем точки чтобы реально были NW-SE
  TopLeft.Latitude := Max(ATopLeft.Latitude, ABottomRight.Latitude);
  TopLeft.Longitude := Min(ATopLeft.Longitude, ABottomRight.Longitude);
  BottomRight.Latitude := Min(ATopLeft.Latitude, ABottomRight.Latitude);
  BottomRight.Longitude := Max(ATopLeft.Longitude, ABottomRight.Longitude);
  Result := GetFilesNameArrayHash(TopLeft.ToHash, BottomRight.ToHash, ADetailLog);
end;

function TZC.GetFilesNameArrayHash(TopLeftHash,
  BottomRightHash: int64; ADetailLog: Boolean): TFileNameArray;
var
  TopLeftHashStr, BottomRightHashStr : string;
  i, j, iRight, iBottom: Integer;
  CurrHash, LeftHash: Int64;
  //список районов, которые входят в заданный прямоугольник
  AreaArray: TFileNameArray;
  CurrAreaStr: string;
  bNotFinded: Boolean;
begin
  iRight := 0;
  iBottom := 0;
  TopLeftHashStr := TGeoHash.ConvertBinToString(TopLeftHash and CAreaHashMask, 5);
  BottomRightHashStr := TGeoHash.ConvertBinToString(BottomRightHash and CAreaHashMask, 5);

  if TopLeftHashStr = BottomRightHashStr then
  begin
    SetLength(AreaArray, 1);
    AreaArray[0] := TopLeftHashStr;
  end else
  begin
    i := 0;
    bNotFinded := True;
    LeftHash := TopLeftHash;
    while (i < CFindHashBottom) and bNotFinded do
    begin
      CurrHash := LeftHash;
      j := 0;
//      while (j < CFindHashRight) and bNotFinded do
      while (j <= i + 1) and bNotFinded do   // сдвиг вправо
      begin
        CurrAreaStr := TGeoHash.ConvertBinToString(CurrHash and CAreaHashMask, 5);
        if CurrAreaStr = BottomRightHashStr then
        begin
          iRight := i;
          iBottom := j;
          bNotFinded := False;
        end;
//        CurrHash := AddLongi(CurrHash);
        CurrHash := SubLati(CurrHash); //сдвиг вниз
        inc(j);
      end;

//      LeftHash := SubLati(LeftHash);  //сдвиг вниз
      LeftHash := AddLongi(LeftHash);  // сдвиг вправо
      inc(i);
    end;

    if not bNotFinded then
    begin
      SetLength(AreaArray, (iBottom+1)*(iRight+1));
      LeftHash := TopLeftHash;
      for  i := 0 to iBottom do
      begin
        CurrHash := LeftHash;
        for  j := 1 to iRight + 1 do
        begin
          CurrAreaStr := TGeoHash.ConvertBinToString(CurrHash and CAreaHashMask, 5);
          AreaArray[i*(iRight+1)+j-1] := CurrAreaStr;
          CurrHash := AddLongi(CurrHash);
        end;
        LeftHash := SubLati(LeftHash);

      end;
    end else
      SetLength(AreaArray, 0);
  end;
  Result := AreaArray;
end;

function TZC.Intersection(const APoint1, APoint2, BPoint1,
  BPoint2: TGeoPos): boolean;
var v1,v2,v3,v4:real;
begin
   v1 := (BPoint2.Longitude-BPoint1.Longitude)*(APoint1.Latitude-BPoint1.Latitude)-(BPoint2.Latitude-BPoint1.Latitude)*(APoint1.Longitude-BPoint1.Longitude);
   v2 := (BPoint2.Longitude-BPoint1.Longitude)*(APoint2.Latitude-BPoint1.Latitude)-(BPoint2.Latitude-BPoint1.Latitude)*(APoint2.Longitude-BPoint1.Longitude);
   v3 := (APoint2.Longitude-APoint1.Longitude)*(BPoint1.Latitude-APoint1.Latitude)-(APoint2.Latitude-APoint1.Latitude)*(BPoint1.Longitude-APoint1.Longitude);
   v4 := (APoint2.Longitude-APoint1.Longitude)*(BPoint2.Latitude-APoint1.Latitude)-(APoint2.Latitude-APoint1.Latitude)*(BPoint2.Longitude-APoint1.Longitude);
   Result := (v1*v2<0) and (v3*v4<0);
end;

procedure TZC.ParseZones;
var
  sName: string;
  ZoneInfo: TZoneInfo;
begin
  // парсинг
  for sName in FZoneDict.Keys do
  begin
    FZoneDict.TryGetValue(sName, ZoneInfo);
    TGeoHash.DecodeArrayWorld(ZoneInfo.AreaHash, ZoneInfo.Area);
    FZoneDict.AddOrSetValue(sName, ZoneInfo);
  end;
end;

function TZC.PatchZoneHash(AHash: string): string;
begin
  if Copy(AHash, 3, 12) = Copy(AHash, Length(AHash) - 11, 12) then
    Result := AHash
  else
    Result := AHash + Copy(AHash, 3, 12);
end;

function TZC.LoadZoneFile(const ARootPath: string;
                                const AAccounts: TIntegerDynArray; ADetailLog: Boolean): TJsonObject;
var
  ZoneList: TStringList;
  sName,TempStr, sFileName: string;
  TabPos: Integer;
  I, j: Integer;
  iType: UInt64;
  ZoneInfo: TZoneInfo;
begin
  Result := TJsonObject.Create;
  ZoneList := TStringList.Create();
  try
    for j := High(AAccounts) downto 0 do
    begin
      sFileName := ARootPath + IntToStr(AAccounts[j]) + PathDelim + CZoneFile;
      if FileExists(sFileName) then
      begin
        ZoneInfo.AccountID := AAccounts[j];
        ZoneList.LoadFromFile(sFileName);
        for I := 0 to ZoneList.Count - 1 do
        begin
          if TGeneral.FStop then
          begin
            ToLog('Request # '+ IntToStr(FRequestNo) + ' TZC.LoadZoneFile. Прервано пользователем');
            Exit;
          end;
          TempStr := ZoneList[I];
          // _имя
          TabPos := Pos(#9, TempStr);
          sName := Copy(TempStr, 1, TabPos - 1);
          Delete(TempStr, 1, TabPos);
          // _тип зоны
          TabPos := Pos(#9, TempStr);
          ZoneInfo.TypeID := StrToInt(Copy(TempStr, 1, TabPos - 1));
  //        TypeID := StrToInt(Copy(TempStr, 1, TabPos - 1));
          Delete(TempStr, 1, TabPos);
          // _активность
          TabPos := Pos(#9, TempStr);
          ZoneInfo.Active := Copy(TempStr, 1, TabPos - 1)='1';
          Delete(TempStr, 1, TabPos);
          // _цвет
          TabPos := Pos(#9, TempStr);
          ZoneInfo.Color := StrToInt(Copy(TempStr, 1, TabPos - 1));
          Delete(TempStr, 1, TabPos);
          // _координаты
          ZoneInfo.AreaHash := TempStr;
          FZoneDict.AddOrSetValue(sName, ZoneInfo);
        end;
      end;
    end;
    for sName in FZoneDict.Keys do
    begin
      FZoneDict.TryGetValue(sName, ZoneInfo);
      with Result.A['Zones'].AddObject do
      begin
        I['AccountID'] := ZoneInfo.AccountID;
        S['Name'] := sName;
        iType := UInt64(1) shl ZoneInfo.TypeID;
        L['Type'] := iType;
        I['Active'] := IfThen(ZoneInfo.Active,1,0);
        I['Color'] := ZoneInfo.Color;
        S['GeoHash'] := ZoneInfo.AreaHash;
      end;
    end;
  finally
    ZoneList.Free();
  end;
end;

function TZC.LoadZoneTypeFile(AAccounts: TIntegerDynArray; ADetailLog: Boolean): TJsonObject;
var
  ZoneList: TStringList;
  TempStr, RootDir, sFileName: string;
  TabPos: Integer;
  I, j: Integer;
  ID: Byte;
  ZoneTypeInfo: TZoneTypeInfo;
begin
  Result := TJsonObject.Create;
  ZoneList := TStringList.Create();
  RootDir := IncludeTrailingPathDelimiter(ExtractFileDir(GetModuleName(HInstance)));
  try
    for j := High(AAccounts) downto 0 do
    begin
      sFileName := RootDir + IntToStr(AAccounts[j]) + PathDelim + CZoneTypesFile;
      if FileExists(sFileName) then
      begin
        ZoneTypeInfo.AccountID := AAccounts[j];
        ZoneList.LoadFromFile(sFileName);
        for I := 0 to ZoneList.Count - 1 do
        begin
          if TGeneral.FStop then
          begin
            ToLog('Request # '+ IntToStr(FRequestNo) + ' TZC.LoadZoneTypeFile Прервано пользователем');
            Exit;
          end;

          TempStr := ZoneList[I];
          // ID
          TabPos := Pos(#9, TempStr);
          ID := StrToInt(Copy(TempStr, 1, TabPos - 1));
          Delete(TempStr, 1, TabPos);
          // name
          ZoneTypeInfo.Name := TempStr;
          FZoneType.AddOrSetValue(ID, ZoneTypeInfo);
        end;
      end;
    end;
    for ID in FZoneType.Keys do
    begin
      FZoneType.TryGetValue(ID, ZoneTypeInfo);
      with Result.A['ZoneTypes'].AddObject do
      begin
        I['AccountID'] := ZoneTypeInfo.AccountID;
        I['ID'] := ID;
        S['Name'] := ZoneTypeInfo.Name;
      end;
    end;
  finally
    ZoneList.Free();
  end;
end;

function TZC.LoadOneZoneFile(AAccount: integer; ADetailLog: Boolean): TZoneDictionary;
var
  ZoneList: TStringList;
  sName,TempStr, RootDir, sFileName: string;
  TabPos: Integer;
  I: Integer;
  ZoneInfo: TZoneInfo;
begin
  Result := TDictionary<string,TZoneInfo>.Create;
  ZoneList := TStringList.Create();
  try
    RootDir := IncludeTrailingPathDelimiter(ExtractFileDir(GetModuleName(HInstance)));

    sFileName := RootDir + IntToStr(AAccount) + PathDelim + CZoneFile;
    if FileExists(sFileName) then
    begin
      ZoneInfo.AccountID := AAccount;
      ZoneList.LoadFromFile(sFileName);
      for I := 0 to ZoneList.Count - 1 do
      begin
        TempStr := ZoneList[I];
        // _имя
        TabPos := Pos(#9, TempStr);
        sName := Copy(TempStr, 1, TabPos - 1);
        Delete(TempStr, 1, TabPos);
        // _тип зоны
        TabPos := Pos(#9, TempStr);
        ZoneInfo.TypeID := StrToInt(Copy(TempStr, 1, TabPos - 1));
        Delete(TempStr, 1, TabPos);
        // _активность
        TabPos := Pos(#9, TempStr);
        ZoneInfo.Active := Copy(TempStr, 1, TabPos - 1)='1';
        Delete(TempStr, 1, TabPos);
        // _цвет
        TabPos := Pos(#9, TempStr);
        ZoneInfo.Color := StrToInt(Copy(TempStr, 1, TabPos - 1));
        Delete(TempStr, 1, TabPos);
        // _координаты
        ZoneInfo.AreaHash := TempStr;
        Result.AddOrSetValue(sName, ZoneInfo);
      end;
    end;
  finally
    ZoneList.Free();
  end;
end;

function TZC.LoadOneZoneTypeFile(AAccount: integer; ADetailLog: Boolean): TZoneTypeDictionary;
var
  ZoneList: TStringList;
  TempStr, RootDir, sFileName: string;
  TabPos: Integer;
  I: Integer;
  ID: Byte;
  ZoneTypeInfo: TZoneTypeInfo;
begin
  Result := TDictionary<Byte,TZoneTypeInfo>.Create;
  ZoneList := TStringList.Create();
  try
    RootDir := IncludeTrailingPathDelimiter(ExtractFileDir(GetModuleName(HInstance)));

    sFileName := RootDir + IntToStr(AAccount) + PathDelim + CZoneTypesFile;
    if FileExists(sFileName) then
    begin
      ZoneTypeInfo.AccountID := AAccount;
      ZoneList.LoadFromFile(sFileName);
      for I := 0 to ZoneList.Count - 1 do
      begin
        TempStr := ZoneList[I];
        // ID
        TabPos := Pos(#9, TempStr);
        ID := StrToInt(Copy(TempStr, 1, TabPos - 1));
        Delete(TempStr, 1, TabPos);
        // name
        ZoneTypeInfo.Name := TempStr;
        Result.AddOrSetValue(ID, ZoneTypeInfo);
      end;
    end;
  finally
    ZoneList.Free();
  end;
end;

function TZC.LoadProgress(AAccount: integer; ADetailLog: Boolean): Integer;
var
  FIni: TIniFile;
  sAcc: string;
begin
  FIni := TIniFile.Create(ChangeFileExt(GetModuleName(HInstance)  + TGeneral.FSuffix, '.ini'));
  try
    Result := -1;
    sAcc := IntToStr(AAccount);
    FProgress := FIni.ReadInteger('main','progresszc'+sAcc, 0);
    Result := FProgress;
  finally
    FIni.Free;
  end;
end;

function TZC.SaveAllZC(const ARootPath: string;
                             const AAccounts: TIntegerDynArray; ADetailLog: Boolean): Boolean;
begin
  try
    ToLog('Request # '+ IntToStr(FRequestNo) + ' TZC.SaveAllZC. Account='+IntToStr(AAccounts[0]));
    // читаем командный файл - заполняем массив
    LoadZoneFile(ARootPath, AAccounts);
    // превращаем текст в числа
    ParseZones();
    // работаем
    Result := CreateZCAll(ARootPath, AAccounts);
  except
    Result := False;
  end;
end;

function TZC.SaveFileListZC(const ARootPath: string; const ATopLeft,
  ABottomRight: TGeoPos; const AAccounts: TIntegerDynArray;
  ADetailLog: Boolean): Boolean;
var
  i: Integer;
  AccPathStr, AFileName, FP: string;
  sFileName: string;
  AreaArray: TFileNameArray;
begin
  try
    Result := False;
    // читаем командный файл - заполняем массив
    LoadZoneFile(ARootPath, AAccounts);
    // превращаем текст в числа
    ParseZones();
    ToLog('Request # '+ IntToStr(FRequestNo) + ' TZC.SaveFileListZC Account='+IntToStr(AAccounts[0]));
    // Заполняем массив
    AreaArray := GetFilesNameArray(ATopLeft, ABottomRight, ADetailLog);

    FProgress := 0;
    SaveProgress(AAccounts[0]);
    AccPathStr := IntToStr(AAccounts[0]) + PathDelim{\};
    ToLog('Request # '+ IntToStr(FRequestNo) + ' AreaArray='+IntToStr(Length(AreaArray)));
    i := 0;
    for sFileName in AreaArray do
    begin
      if TGeneral.FStop then
      begin
        ToLog('Request # '+ IntToStr(FRequestNo) + ' TZC.SaveFileListZC Прервано пользователем. Account='+IntToStr(AAccounts[0]));
        Exit(False);
      end;

      FP := ARootPath{D:\IGF\}
        + AccPathStr
        + Copy(sFileName, 1, 1){u} + PathDelim{\}
        + Copy(sFileName, 1, 2){uc} + PathDelim{\}
        + Copy(sFileName, 1, 3){ucf} + PathDelim{\}
        + Copy(sFileName, 1, 4){ucft} + PathDelim{\};
      AFileName := FP + sFileName;
      Inc(i);
      if not FileExists(AFileName+CFileType[ftEdgeF]) then
        Continue;

      Result := SaveOneZC(AFileName+CFileType[ftEdgeF], AFileName+CFileType[ftZCF]);
      Result := SaveOneZC(AFileName+CFileType[ftEdgeB], AFileName+CFileType[ftZCB]);
      if Trunc(i*100/Length(AreaArray)) <> FProgress then
      begin
        FProgress := Trunc(i*100/Length(AreaArray));
        SaveProgress(AAccounts[0]);
      end;
    end;
    ToLog('Request # '+ IntToStr(FRequestNo) + ' TZC.SaveFileListZC. Stop create zones new. Files count = ' +
          IntToStr(Length(AreaArray)) + ' Account='+IntToStr(AAccounts[0]));
    FProgress := 100;
    SaveProgress(AAccounts[0]);
    Result := True;
  finally
    SetLength(AreaArray,0);
  end;
end;

function TZC.SaveOneZC(AEdgeFileName, AZCFileName: string): Boolean;
var
  M: Integer;
  EdgePlace: TEdgeArray;
  ZCPlace: TArray<UInt64>;
  NewLevel: UInt64;
  EdgeFile, WayFile: TFileStream;
  ZCFile: TFileStream;
  sName: string;
  ZoneInfo: TZoneInfo;
  i: integer;
  Points, WayPoints: TGeoPosArray;
begin
  Result := False;
    // загружаем рёбра
  EdgeFile := TFileStream.Create(AEdgeFileName, fmOpenRead or fmShareDenyNone);
  try
    if (EdgeFile.Size mod SizeOf(TEdge) <> 0) then Exit;
    SetLength(EdgePlace, 0); // нет копированию памяти
    SetLength(EdgePlace, EdgeFile.Size div SizeOf(TEdge));
    EdgeFile.ReadBuffer(EdgePlace[0], EdgeFile.Size);
  finally
    FreeAndNil(EdgeFile);
  end;
  WayFile := TFileStream.Create(ChangeFileExt(AEdgeFileName, CFileType[ftWay]), fmOpenRead or fmShareDenyNone);
  try
    if (WayFile.Size mod SizeOf(TGeoPos) <> 0) then Exit;
    SetLength(WayPoints, WayFile.Size div SizeOf(TGeoPos));
    WayFile.ReadBuffer(WayPoints[0], WayFile.Size);
  finally
    FreeAndNil(WayFile);
  end;
  // считаем
  SetLength(ZCPlace, 0); // нет копированию памяти
  SetLength(ZCPlace, Length(EdgePlace));
  for M := Low(ZCPlace) to High(ZCPlace) do
  begin
    SetLength(Points, EdgePlace[M].WayCount + 2);
    Points[Low(Points)] := EdgePlace[M].CoordsFrom;
    Points[High(Points)] := EdgePlace[M].CoordsTo;
    if EdgePlace[M].WayCount > 0 then
      for I := 0 to EdgePlace[M].WayCount - 1 do
        Points[i + 1] := WayPoints[EdgePlace[M].WayIndex + i];

    NewLevel := 0;
    for sName in FZoneDict.Keys do
    begin
      FZoneDict.TryGetValue(sName, ZoneInfo);
      if not ZoneInfo.Active then Exit;
      if (ZoneInfo.TypeID > 63) then Exit;

      if CheckRouteInThroughArea(ZoneInfo.Area, Points) then
        NewLevel := NewLevel or (UInt64(1) shl ZoneInfo.TypeID);
    end;
    ZCPlace[M] := NewLevel;
  end;

  // сохраняем зоны
  if not TFileHandler.DeleteFile(AZCFileName)
    then Exit;
  if ZCPlaceIsEmpty(ZCPlace) then
    Exit(True);
  ZCFile := TFileStream.Create(AZCFileName, fmCreate);
  try
    ZCFile.WriteBuffer(ZCPlace[0], Length(ZCPlace) * SizeOf(NewLevel));
  finally
    FreeAndNil(ZCFile);
  end;
  // прогресс
  Result := True;
end;

function TZC.SaveTwoZC(const ARootPath: string;
                             const AAccounts: TIntegerDynArray;
                             AFileName: string): Boolean;
var
  sFileName, FRootPath, FP, AccPathStr: string;
begin
  try
    // читаем командный файл - заполняем массив
    LoadZoneFile(ARootPath, AAccounts);
    // превращаем текст в числа
    ParseZones();
    // работаем
    if Length(AAccounts) = 0 then
      AccPathStr := ''
    else
      AccPathStr := IntToStr(AAccounts[0]) + PathDelim{\};
    FRootPath := IncludeTrailingPathDelimiter(ExtractFileDir(GetModuleName(HInstance)));
    FP := FRootPath{D:\IGF\}
      + AccPathStr
      + Copy(AFileName, 1, 1){u} + PathDelim{\}
      + Copy(AFileName, 1, 2){uc} + PathDelim{\}
      + Copy(AFileName, 1, 3){ucf} + PathDelim{\}
      + Copy(AFileName, 1, 4){ucft} + PathDelim{\};
    sFileName := FP + AFileName;
    Result := SaveOneZC(sFileName+CFileType[ftEdgeF], sFileName+CFileType[ftZCF]);
    Result := Result and SaveOneZC(sFileName+CFileType[ftEdgeB], sFileName+CFileType[ftZCB]);
  except
    Result := False;
  end;
end;

function TZC.SaveZones(AAccount: integer; AZones: TZoneDictionary; ADetailLog: Boolean): Boolean;
var
  List: TStringList;
  s, sName: string;
  ZoneInfo: TZoneInfo;
  RootDir, AccDir: string;
begin
  try
    List := TStringList.Create;
    try
      for sName in AZones.Keys do
      begin
        AZones.TryGetValue(sName,ZoneInfo);
        s := sName + #9 +
                 IntToStr(ZoneInfo.TypeID) + #9 +
                 IntToStr(IfThen(ZoneInfo.Active,1,0)) + #9 +
                 IntToStr(ZoneInfo.Color) + #9 +
                 PatchZoneHash(ZoneInfo.AreaHash);
        List.Add(s);
      end;
      RootDir := IncludeTrailingPathDelimiter(ExtractFileDir(GetModuleName(HInstance)));
      AccDir := IntToStr(AAccount) + PathDelim;
      // каталог
      ForceDirectories(RootDir + AccDir);

      List.SaveToFile(RootDir + AccDir + CZoneFile, TEncoding.UTF8);
      Result := True;
    finally
      List.Free;
    end;
  except
    Result := False;
  end;
end;

function TZC.SaveZoneTypes(AAccount: integer;
  AZoneTypes: TZoneTypeDictionary; ADetailLog: Boolean): Boolean;
var
  ID: Byte;
  List: TStringList;
  s: string;
  ZoneInfo: TZoneTypeInfo;
  RootDir, AccDir: string;
begin
  try
    List := TStringList.Create;
    try
      for ID in AZoneTypes.Keys do
      begin
        AZoneTypes.TryGetValue(ID,ZoneInfo);
        s := IntToStr(ID) + #9 + ZoneInfo.Name;
        List.Add(s);
      end;
      RootDir := IncludeTrailingPathDelimiter(ExtractFileDir(GetModuleName(HInstance)));
      AccDir := IntToStr(AAccount) + PathDelim;
      // каталог
      ForceDirectories(RootDir + AccDir);

      List.SaveToFile(RootDir + AccDir + CZoneTypesFile, TEncoding.UTF8);
      Result := True;
    finally
      List.Free;
    end;
  except
    Result := False;
  end;
end;

function TZC.ScanForAll(AAccounts: TIntegerDynArray): Boolean;
var
  I, J, K, L: Char;
  HashPathStr: string;
  FullPath: string;
  FPRoot: string;
  DataList: TArray<string>;
  SR: TSearchRec;
  Progress: Integer;
begin
  ToLog('Start create all zc. Account='+IntToStr(AAccounts[0]));
  Progress := 0;
  FProgress := 0;
  try
    SaveProgress(AAccounts[0]);
    FPRoot := IncludeTrailingPathDelimiter(ExtractFileDir(GetModuleName(HInstance)));
    for I in CHashChars do
    begin
      HashPathStr := I + '\';
      FullPath := FPRoot+ {'igf\' + }HashPathStr;
      if TGeneral.FStop then Exit(False);
      if not DirectoryExists(FullPath) then Continue;
      for J in CHashChars do
      begin
        HashPathStr := I + '\' + I + J + '\';
        FullPath := FPRoot+ {'igf\' + }HashPathStr;
        if TGeneral.FStop then Exit(False);
        if not DirectoryExists(FullPath) then Continue;
        for K in CHashChars do
        begin
          HashPathStr := I + '\' + I + J + '\' + I + J + K + '\';
          FullPath := FPRoot+ {'igf\' + }HashPathStr;
          if TGeneral.FStop then Exit(False);
          if not DirectoryExists(FullPath) then Continue;
          for L in CHashChars do
          begin
            //              u\uc\ucf\ucft\
            HashPathStr := I + '\' + I + J + '\' + I + J + K + '\' + I + J + K + L + '\';
            FullPath := FPRoot+ {'igf\' + }HashPathStr;
            if not DirectoryExists(FullPath) then Continue;
            SetLength(DataList, 0);
            // составляем список файлов edge_f
            if (SysUtils.FindFirst(FullPath + '*.edge_f', faNormal, SR) = 0) then
            begin
              repeat
                if (SR.Name = '') then Continue;
                SaveOneZC(FullPath + SR.Name, ChangeFileExt(FullPath + SR.Name, '.zc_f'));
                SaveOneZC(ChangeFileExt(FullPath + SR.Name, '.edge_b'), ChangeFileExt(FullPath + SR.Name, '.zc_b'));
              until (SysUtils.FindNext(SR) <> 0);
              SysUtils.FindClose(SR);
            end;
          end; // K
        end; // L
        // прогресс
        Inc(Progress);

        if Trunc(Progress / (32*32) * 100) <> FProgress then
        begin
          FProgress := Trunc(Progress / (32*32) * 100);
          if FProgress = 100 then FProgress := 99;
          SaveProgress(AAccounts[0]);
        end;
      end; // J
    end; // I
    FProgress := 100;
    SaveProgress(AAccounts[0]);
    ToLog('Stop create all zc. Account='+IntToStr(AAccounts[0]));
    Result := True;
  except
    Result := False;
  end;
end;

class procedure TZC.Stop;
begin
  ToLog('TZC Manual Stop');
end;

procedure TZC.SaveProgress(AAccount: integer);
var
  FIni: TIniFile;
  sAcc: string;
begin
  FIni := TIniFile.Create(ChangeFileExt(GetModuleName(HInstance) + TGeneral.FSuffix, '.ini'));
  try
    sAcc := IntToStr(AAccount);
    ToLog('Request # '+ IntToStr(FRequestNo) + ' ZC progress for Account = ' + IntToStr(AAccount) + ' is '+IntToStr(FProgress)+'%');
    FIni.WriteInteger('main','progresszc'+sAcc, FProgress);
  finally
    FIni.Free;
  end;
end;

function TZC.ZCPlaceIsEmpty(AZCPlace: TArray<UInt64>): Boolean;
var
  i: UInt64;
begin
  Result := True;
  for i in AZCPlace do
    if i <> 0 then
      Exit(False);
end;

end.
