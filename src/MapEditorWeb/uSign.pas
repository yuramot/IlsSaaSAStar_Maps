unit uSign;

interface

uses
  JsonDataObjects, Ils.Json.Names, Ils.Json.Utils, Geo.Hash.Search, System.SysUtils,
  System.Generics.Collections, System.Classes, Ils.Logger, UFiles, System.IniFiles,
  uZC, AStar64.Files, AStar64.Areas, AStar64.Extra, Geo.Hash, AStar64.FileStructs,
  System.Types, uGeneral;


const
  CSignsFile = 'RouteSigns.txt';
  CSignTypesFile = 'SignTypes.txt';
  CHashChars: string = '0123456789bcdefghjkmnpqrstuvwxyz';
  PathDelim = '\';

type

  TSignTypeInfo = record
    Alias: string;
    //! наименование
    Name: string;
    Include: string;
  end;

  TSignInfo = record
    //! AccountID
    Account: Integer;
    //! маска знаков
    Sign: UInt64;
  end;

  TRouteSign = record
    RouteID: TIDLines;
    Sign: UInt64;
  end;

  TSignTypeDictionary = TDictionary<Byte,TSignTypeInfo>;
  TSignDictionary = TDictionary<TIDLines,TSignInfo>;
  TRouteSignArray = array of TRouteSign;

  TSign = class
  private
    FDetailLog: Boolean;
    FProgress: Integer;
    FRequestNo: Cardinal;
    class var FStop: Boolean;
  public
    FSignType : TDictionary<Byte,TSignTypeInfo>;
    constructor Create(ARequestNo: Cardinal; ADetailLog: Boolean = False); overload;
    destructor Destroy(); override;
    class procedure Stop;
    function LoadSignTypeFile(const ARootPath: string): TJsonObject;
    function LoadSignFile(const ARootPath: string;
                                const AAccounts: TIntegerDynArray): TSignDictionary;
    function LoadSignFileOne(const ARootPath: string;
                                const AAccount: integer): TSignDictionary;
    function SaveSignFile(const ARootPath: string;
                                const AAccount: integer;
                                const ARouteSignArr: TRouteSignArray): boolean;
    function SaveSignTypes(const ARootPath: string; AAccount: integer; ASignTypes: TSignTypeDictionary): Boolean;
    function DeleteAllSign(const ARootPath: string;
                               const AAccounts: TIntegerDynArray): Boolean;
    function LoadProgress(AAccount: integer): Integer;
    procedure SaveProgress(AAccount: integer);
    function CreateSignAll(const ARootPath: string;
                               const AAccounts: TIntegerDynArray; ADetailLog: Boolean = false): Boolean;

    procedure SetBit(var AValue: UInt64; BitNum: Byte);
    procedure ClearBit(var AValue: UInt64; BitNum: Byte);
    procedure ToggleBit(var AValue: UInt64; BitNum: Byte);
    function GetBitStat(AValue: UInt64; BitNum: Byte): Boolean;
  end;

implementation
{ TSign }

procedure TSign.SetBit(var AValue: UInt64; BitNum: Byte);
var
  Bit64: UInt64;
begin
  Bit64 := 1;
  Bit64 := Bit64 shl BitNum;
  AValue := AValue or Bit64; { Устанавливаем бит }
end;

class procedure TSign.Stop;
begin
  FStop := True;
  ToLog('TSign Manual Stop');
end;

procedure TSign.ClearBit(var AValue: UInt64; BitNum: Byte);
var
  Bit64: UInt64;
begin
  Bit64 := 1;
  Bit64 := Bit64 shl BitNum;
  AValue := AValue or Bit64; { Устанавливаем бит }
  AValue := AValue xor Bit64; { Переключаем бит   }
end;

procedure TSign.ToggleBit(var AValue: UInt64; BitNum: Byte);
var
  Bit64: UInt64;
begin
  Bit64 := 1;
  Bit64 := Bit64 shl BitNum;
  AValue := AValue xor Bit64; { Переключаем бит   }
end;

constructor TSign.Create(ARequestNo: Cardinal; ADetailLog: Boolean);
begin
  FStop := False;
  FDetailLog := ADetailLog;
  FRequestNo := ARequestNo;
  FSignType := TDictionary<Byte,TSignTypeInfo>.Create;
  if FDetailLog then
    ToLog('TSign Start');
end;

function TSign.CreateSignAll(const ARootPath: string;
  const AAccounts: TIntegerDynArray; ADetailLog: Boolean): Boolean;
var
  i, j, iCount: Integer;
  SignDictionary: TSignDictionary;
  SignInfo: TSignInfo;
  RouteID: TIDLines;
  HashStartStr, HashEndStr, sFile: string;
  Holder: THoldRec;
  FileList : TDictionary<string, Integer>;
begin
  ToLog('TSign.CreateSignAll. Start create sign files');
  Result := False;
  FProgress := 0;
  SaveProgress(AAccounts[0]);
  //удаляем файлы знаков
  DeleteAllSign(ARootPath, AAccounts);
  //получаем список дорог
  SignDictionary := LoadSignFileOne(ARootPath, AAccounts[0]);
  //формируем список файлов
  FileList := TDictionary<string, Integer>.Create;
  try
    for RouteID in SignDictionary.Keys do
    begin
      HashStartStr := TGeoHash.ConvertBinToString(RouteID.HashStart and CAreaHashMask, 5);
      HashEndStr := TGeoHash.ConvertBinToString(RouteID.HashEnd and CAreaHashMask, 5);

      if not FileList.ContainsKey(HashStartStr) then
      begin
        FileList.Add(HashStartStr, 0);
      end;
      if HashStartStr<>HashEndStr then
        if not FileList.ContainsKey(HashEndStr) then
          FileList.Add(HashEndStr, 0);
    end;
    i := 0;
    iCount := FileList.Count;
  //загружаем файлы, проставляем знаки, сохраняем
    for sFile in FileList.Keys do
    begin
      if FStop then
      begin
        ToLog('TSign.CreateSignAll. Прервано пользователем');
        Exit;
      end;
      TGFAdapter.Load(ARootPath, AAccounts, sFile, Holder);
      if Length(Holder.SCForward) <> Length(Holder.EdgeForward) then
        SetLength(Holder.SCForward, Length(Holder.EdgeForward));
      if Length(Holder.SCBackward) <> Length(Holder.EdgeBackward) then
        SetLength(Holder.SCBackward, Length(Holder.EdgeBackward));

      for j := Low(Holder.EdgeForward) to High(Holder.EdgeForward) do
      begin
        RouteID.OSM_ID := Holder.EdgeForward[j].ID;
        RouteID.HashStart := Holder.EdgeForward[j].HashVector.HashFrom;
        RouteID.HashEnd := Holder.EdgeForward[j].HashVector.HashTo;
        if SignDictionary.TryGetValue(RouteID, SignInfo) then
          Holder.SCForward[j] := SignInfo.Sign;
      end;
      for j := Low(Holder.EdgeBackward) to High(Holder.EdgeBackward) do
      begin
        RouteID.OSM_ID := Holder.EdgeBackward[j].ID;
        RouteID.HashStart := Holder.EdgeBackward[j].HashVector.HashFrom;
        RouteID.HashEnd := Holder.EdgeBackward[j].HashVector.HashTo;
        if SignDictionary.TryGetValue(RouteID, SignInfo) then
          Holder.SCBackward[j] := SignInfo.Sign;
      end;

      TGFAdapter.Save(ARootPath, AAccounts[0], sFile, Holder, C9FileTypes);
      Inc(i);
      FProgress := (i*100) div iCount;
      SaveProgress(AAccounts[0]);
    end;
    ToLog('TSign.CreateSignAll. Finish create sign files. Files count = ' + IntToStr(FileList.Count));
    FProgress := 100;
    SaveProgress(AAccounts[0]);
    Result := True;
  finally
    FileList.Free;
  end;
end;

function TSign.DeleteAllSign(const ARootPath: string;
  const AAccounts: TIntegerDynArray): Boolean;
var
  I, J, K, L: Char;
  Acc: Integer;
  HashPathStr: string;
  FullPath: string;
  SR: TSearchRec;
begin
  Result := False;
  Acc := AAccounts[0];
  ToLog('Start delete all sign files. Account='+IntToStr(Acc));
  for I in CHashChars do
  begin
    HashPathStr := I + '\';
    FullPath := ARootPath + IntToStr(Acc) + PathDelim + HashPathStr;
    if not DirectoryExists(FullPath) then Continue;
    ToLog('TSign.DeleteAllSign. Delete SC files in folder ' + FullPath + '..');
    for J in CHashChars do
    begin
      HashPathStr := I + '\' + I + J + '\';
      FullPath := ARootPath + IntToStr(Acc) + PathDelim+ {'igf\' + }HashPathStr;
      if not DirectoryExists(FullPath) then Continue;
      for K in CHashChars do
      begin
        HashPathStr := I + '\' + I + J + '\' + I + J + K + '\';
        FullPath := ARootPath + IntToStr(Acc) + PathDelim+ {'igf\' + }HashPathStr;
        if not DirectoryExists(FullPath) then Continue;
        for L in CHashChars do
        begin
          //              u\uc\ucf\ucft\
          HashPathStr := I + '\' + I + J + '\' + I + J + K + '\' + I + J + K + L + '\';
          FullPath := ARootPath + IntToStr(Acc) + PathDelim+ {'igf\' + }HashPathStr;
          if not DirectoryExists(FullPath) then Continue;
          // составляем список файлов edge_f
          if FStop then
          begin
            ToLog('TSign.DeleteAllSign. Прервано пользователем');
            Exit;
          end;
          try
            if (FindFirst(FullPath + '*.signs_f', faNormal, SR) = 0) then
            begin
              repeat
                if (SR.Name = '') then Continue;
                if not TFileHandler.DeleteFile(FullPath + SR.Name) then Continue;
                if not TFileHandler.DeleteFile(ChangeFileExt(FullPath + SR.Name, '.signs_b')) then Continue;
              until (FindNext(SR) <> 0);
              FindClose(SR);
            end;
          except on E : Exception do
            ToLog('TSign.DeleteAllSign. Error:'+E.Message);
          end;
        end; // K
      end; // L
    end; // J
  end; // I
  ToLog('All signs file deleted. Account='+IntToStr(Acc));
  Result := True;
end;

destructor TSign.Destroy;
begin
  FSignType.Free;
  if FDetailLog then
    ToLog('TSign Stop');
  inherited;
end;

function TSign.GetBitStat(AValue: UInt64; BitNum: Byte): Boolean;
var
  Bit64: UInt64;
begin
  Bit64 := 1;
  Bit64 := Bit64 shl BitNum;
  GetBitStat := (AValue and Bit64) = Bit64 { Если бит установлен }
end;

function TSign.LoadProgress(AAccount: integer): Integer;
var
  FIni: TIniFile;
  sAcc: string;
begin
  FIni := TIniFile.Create(ChangeFileExt(GetModuleName(HInstance) + TGeneral.FSuffix, '.ini'));
  try
    sAcc := IntToStr(AAccount);
    try
      FProgress := FIni.ReadInteger('main','progresssc'+sAcc, 0);
      Result := FProgress;
    except
      Result := -1;
    end;
  finally
    FIni.Free;
  end;
end;

function TSign.LoadSignFile(const ARootPath: string;
  const AAccounts: TIntegerDynArray): TSignDictionary;
var
  RouteSignFile: TFileStream;
  RouteSignArr: TRouteSignArray;
  sFileName: string;
  I, j: Integer;
  SignInfo: TSignInfo;
begin
  Result := TDictionary<TIDLines,TSignInfo>.Create;
  for j := High(AAccounts) downto 0 do
  begin
    sFileName := ARootPath + IntToStr(AAccounts[j]) + PathDelim + CSignsFile;
    if FileExists(sFileName) then
    begin
      RouteSignFile := TFileStream.Create(sFileName, fmOpenRead + fmShareDenyNone);
      try
        SetLength(RouteSignArr, RouteSignFile.Size div SizeOf(TRouteSign));
        for I := Low(RouteSignArr) to High(RouteSignArr) do
        begin
          if FStop then
          begin
            ToLog('TSign.LoadSignFile. Прервано пользователем');
            Exit;
          end;
          SignInfo.Account := AAccounts[j];
          SignInfo.Sign := RouteSignArr[i].Sign;
          Result.AddOrSetValue(RouteSignArr[i].RouteID, SignInfo);
        end;
      finally
        RouteSignFile.Free();
      end;
    end;
  end;
end;

function TSign.LoadSignFileOne(const ARootPath: string;
  const AAccount: integer): TSignDictionary;
var
  RouteSignFile: TFileStream;
  RouteSignArr: TRouteSignArray;
  sFileName: string;
  I: Integer;
  SignInfo: TSignInfo;
begin
  Result := TDictionary<TIDLines,TSignInfo>.Create;
  sFileName := ARootPath + IntToStr(AAccount) + PathDelim + CSignsFile;
  if FileExists(sFileName) then
  begin
    RouteSignFile := TFileStream.Create(sFileName, fmOpenRead + fmShareDenyNone);
    try
      SetLength(RouteSignArr, RouteSignFile.Size div SizeOf(TRouteSign));
      RouteSignFile.ReadBuffer(RouteSignArr[0], RouteSignFile.Size);
      for I := Low(RouteSignArr) to High(RouteSignArr) do
      begin
        if FStop then
        begin
          ToLog('TSign.LoadSignFileOne. Прервано пользователем');
          Exit;
        end;
        SignInfo.Account := AAccount;
        SignInfo.Sign := RouteSignArr[i].Sign;
        Result.AddOrSetValue(RouteSignArr[i].RouteID, SignInfo);
      end;
    finally
      RouteSignFile.Free();
    end;
  end;
end;

function TSign.LoadSignTypeFile(const ARootPath: string): TJsonObject;
var
  SignList: TStringList;
  TempStr, sFileName: string;
  TabPos: Integer;
  I: Integer;
  ID: Byte;
  keyArray: TArray<Byte>;
  SignTypeInfo: TSignTypeInfo;
begin
  Result := TJsonObject.Create;
  SignList := TStringList.Create();
  sFileName := ARootPath +  PathDelim + CSignTypesFile;
  try
    if FileExists(sFileName) then
    begin
      SignList.LoadFromFile(sFileName);
      for I := 0 to SignList.Count - 1 do
      begin
        TempStr := SignList[I];
        // ID
        TabPos := Pos(#9, TempStr);
        ID := StrToInt(Copy(TempStr, 1, TabPos - 1));
        Delete(TempStr, 1, TabPos);
        // name
        TabPos := Pos(#9, TempStr);
        SignTypeInfo.Name := Copy(TempStr, 1, TabPos - 1);
        if SignTypeInfo.Name = '' then
          Continue;
        Delete(TempStr, 1, TabPos);
        // alias
        TabPos := Pos(#9, TempStr);
        SignTypeInfo.Alias := Copy(TempStr, 1, TabPos - 1);
        Delete(TempStr, 1, TabPos);
        // alias
        SignTypeInfo.Include := TempStr;
        FSignType.AddOrSetValue(ID, SignTypeInfo);
      end;
    end;
    keyArray := FSignType.Keys.ToArray;
    TArray.Sort<Byte>(KeyArray);
    for ID in KeyArray do
    begin
      FSignType.TryGetValue(ID, SignTypeInfo);
      with Result.A['SignTypes'].AddObject do
      begin
        I['ID'] := ID;
        S['Name'] := SignTypeInfo.Name;
        S['Alias'] := SignTypeInfo.Alias;
        A['Include'].Add(SignTypeInfo.Include);
      end;
    end;
  finally
    SignList.Free();
  end;
end;

procedure TSign.SaveProgress(AAccount: integer);
var
  FIni: TIniFile;
  sAcc: string;
begin
  FIni := TIniFile.Create(ChangeFileExt(GetModuleName(HInstance) + TGeneral.FSuffix, '.ini'));
  try
    ToLog('SC progress for Account = ' + IntToStr(AAccount) + ' is '+IntToStr(FProgress));
    sAcc := IntToStr(AAccount);
    FIni.WriteInteger('main','progresssc'+sAcc, FProgress);
  finally
    FIni.Free;
  end;
end;

function TSign.SaveSignFile(const ARootPath: string;
  const AAccount: integer;
  const ARouteSignArr: TRouteSignArray): boolean;
var
  RouteSignFile: TFileStream;
  sFileName: string;
begin
  try
    sFileName := ARootPath + IntToStr(AAccount) + PathDelim + CSignsFile;
    RouteSignFile := TFileStream.Create(sFileName, fmCreate);
    try
      RouteSignFile.WriteBuffer(ARouteSignArr[0], Length(ARouteSignArr)*SizeOf(TRouteSign));
      Result := True;
    finally
      RouteSignFile.Free();
    end;
  except
    Result := False;
  end;
end;

function TSign.SaveSignTypes(const ARootPath: string; AAccount: integer;
  ASignTypes: TSignTypeDictionary): Boolean;
var
  ID: Byte;
  List: TStringList;
  s: string;
  SignInfo: TSignTypeInfo;
  AccDir: string;
begin
  List := TStringList.Create;
  try
    Result := False;
    for ID in ASignTypes.Keys do
    begin
      ASignTypes.TryGetValue(ID,SignInfo);
      s := IntToStr(ID) + #9 + SignInfo.Name + #9 + SignInfo.Alias;
      List.Add(s);
    end;
    AccDir := IntToStr(AAccount) + PathDelim;
    // каталог
    ForceDirectories(ARootPath + AccDir);

    List.SaveToFile(ARootPath + AccDir + CSignTypesFile, TEncoding.UTF8);
    Result := True;
  finally
    List.Free;
  end;
end;

end.
