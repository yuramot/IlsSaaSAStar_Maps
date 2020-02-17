unit FormMainDynamicTests;

interface

{$define TEST_NEW}

uses
  SysUtils, Windows, Messages, Variants, Classes, Graphics, Controls, Forms,
  Types,
  Math, Dialogs, StdCtrls, ExtCtrls, ComCtrls,
  AStar64.DynImport, AStar64.Typ, Geo.Pos, Geo.Hash,
  ILSMapControlEx_TLB, System.Win.ComObj, ActiveX,
  Ils.Logger, IdSocketHandle, IdBaseComponent, IdComponent, IdUDPBase,
  AStar64.LandMark,
  IdUDPServer, IdSysLogServer, IdSysLogMessage;

type
  TMainForm = class(TForm)
    pMap: TPanel;
    sbMain: TStatusBar;
    Panel1: TPanel;
    Edit1: TEdit;
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    Button4: TButton;
    edtPin: TEdit;
    Button5: TButton;
    CheckBox1: TCheckBox;
    IdSyslogServer1: TIdSyslogServer;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure AllKeyPress(Sender: TObject; var Key: Char);
    procedure bMakePathClick(Sender: TObject);
    procedure bChangeClick(Sender: TObject);
    procedure Edit1Change(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure Button5Click(Sender: TObject);
    procedure IdSyslogServer1Syslog(Sender: TObject;
      ASysLogMessage: TIdSysLogMessage; ABinding: TIdSocketHandle);
  private
    //!
    FBitmapGreen: IObgBitmap;
    //!
    FBitmapBlue: IObgBitmap;
    //!
    FBitmapRed: IObgBitmap;
    //!
    FBitmapYellow: IObgBitmap;
    //!
    FBitmapCian: IObgBitmap;

    FBitmapBlueDot: IObgBitmap;
    FBitmapBlueArrow: IObgBitmap;
    FBitmapBlueCross: IObgBitmap;

    FBitmapBlueFlag: IObgBitmap;
//    FBitmapYellowFlag: IObgBitmap;
    FBitmapGreenFlag: IObgBitmap;
    FBitmapRedFlag:  IObgBitmap;

    //!
    FMap: TILSMapEx;
    //!
    FZ1Line: ILayerLine;
    FZ2Line: ILayerLine;
    FZ3Line: ILayerLine;
    FLayerLine: ILayerLine;
    //!
    FLayerPath: ILayerLine;
    //!
    FLayerPathA: ILayerBitmap;
    //!
    FPBitmap: IObgBitmap;
    //! ТЕСТ
//    FLayerPoly: ILayerContour;
    //!
    FLatiFrom, FLongiFrom, FLatiTo, FLongiTo: Double;
    FSpeed: Integer;
    FMask: UInt64;
    //!
//    FCarPass: Integer;

    FCounter: Integer;
    //!
//    procedure PatchINT3;

    procedure MapClickLeft(
      ASender: TObject;
      ALatitude: Double;
      ALongitude: Double;
      AX: Integer;
      AY: Integer;
      AKeyState: TKeybState
    );
    //!
    procedure MapClickRight(
      ASender: TObject;
      ALatitude: Double;
      ALongitude: Double;
      AX: Integer;
      AY: Integer;
      AKeyState: TKeybState
    );
    //!
    procedure SetLine();
    //!
//    procedure SetupStructure();
    procedure ReadEditString;
    procedure MakeEditString;
  end;

var
  MainForm: TMainForm;

implementation

{$R *.dfm}

//------------------------------------------------------------------------------
//! выдаём скорость на основе "уровня" дороги
//------------------------------------------------------------------------------
function RoadSpeedByType(
  const AType: Integer
): Integer;
begin
  case AType of
    11: Result := 110;
    12: Result := 108;
    13: Result := 90;
    14: Result := 88;
    15: Result := 70;
    16: Result := 68;
    21: Result := 60;
    22: Result := 59;
    31: Result := 60;
    32: Result := 59;
    41, 42, 43: Result := 40;
    else Result := 20;
{
    11, 12: Result := 110;
    13, 14: Result := 90;
    15, 16: Result := 70; // 70?
    21, 22: Result := 60;
    31, 32: Result := 60;
    41, 42, 43: Result := 40;
    else Result := 20;
}
  end;
end;


function GetDigits(var s: string): string;
var
  Started: Boolean;
begin
  Result := '';
  Started := False;
  while Length(s) > 0 do
  begin
    if CharInSet(s[1], ['0'..'9','.']) then
    begin
      if not Started  then
        Started := True;;
      Result := Result + s[1];
    end else
      if Started then
        Break;

    Delete(s, 1, 1);
  end;

end;

function GetGeo(var s: string): Double;
var
  fs: TFormatSettings;
begin
  fs := TFormatSettings.Create;
  fs.DecimalSeparator := '.';
  Result := StrToFloatDef(GetDigits(s), 0, fs);
end;

function GetSpeed(var s: string): Integer;
begin
  Result := StrToIntDef(GetDigits(s), 90);
end;

function GetZones(var s: string): UInt64;
begin
  Result := StrToInt64Def(GetDigits(s), 0);
end;

function GetTimeout(var s: string; const ADef: Integer): UInt64;
begin
  Result := StrToInt64Def(GetDigits(s), ADef);
end;

procedure TMainForm.FormCreate(Sender: TObject);
begin
  try
    ActiveX.CoInitialize(nil);
//    if DebugHook <> 0 then
//      PatchINT3;
    FMap := TILSMapEx.Create( nil );
  except
//    on E: EOleSysError do
//      raise EOleSysError.Create(Format('%s, ClassID: %s',[E.Message, 'ClassID'{GuidToString(ClassID)}]),E.ErrorCode,0) { Do not localize }
    on E : Exception do
    begin
      ShowMessage(E.ClassName + ': ' + E.Message);
    end;
  end;
  FMap.SetParentComponent( pMap );
  FMap.Align := alClient;
  FMap.LoadMap( mpYandex );
  FMap.CenterMapOnCoords( 55.75, 37.625 );
//  FMap.Zoom := 12;
  //
  FMap.OnMClick := MapClickLeft;
  FMap.OnMClickRight := MapClickRight;
  //

  FBitmapGreen := CoObgBitmap.Create();
  FBitmapGreen.LoadPictureFromFile( ExtractFilePath( ParamStr( 0 ) ) + 'maps\PointObject_5.ico', False, False );
  FBitmapBlue := CoObgBitmap.Create();
  FBitmapBlue.LoadPictureFromFile( ExtractFilePath( ParamStr( 0 ) ) + 'maps\PointObject_6.ico', False, False );
  FBitmapRed := CoObgBitmap.Create();
  FBitmapRed.LoadPictureFromFile( ExtractFilePath( ParamStr( 0 ) ) + 'maps\PointObject_7.ico', False, False );
  FBitmapYellow := CoObgBitmap.Create();
  FBitmapYellow.LoadPictureFromFile( ExtractFilePath( ParamStr( 0 ) ) + 'maps\PointObject_8.ico', False, False );
  FBitmapCian := CoObgBitmap.Create();
  FBitmapCian.LoadPictureFromFile( ExtractFilePath( ParamStr( 0 ) ) + 'maps\PointObject_9.ico', False, False );

  FBitmapBlueDot := CoObgBitmap.Create();
  FBitmapBlueDot.LoadPictureFromFile( ExtractFilePath( ParamStr( 0 ) ) + 'maps\NoRun.ico', False, False );
  FBitmapBlueArrow := CoObgBitmap.Create();
  FBitmapBlueArrow.LoadPictureFromFile( ExtractFilePath( ParamStr( 0 ) ) + 'maps\Arrow.ico', False, False );
  FBitmapBlueCross := CoObgBitmap.Create();
  FBitmapBlueCross.LoadPictureFromFile( ExtractFilePath( ParamStr( 0 ) ) + 'maps\CarEmpty.ico', False, False );
  FBitmapBlueFlag := CoObgBitmap.Create();
  FBitmapBlueFlag.LoadPictureFromFile( ExtractFilePath( ParamStr( 0 ) ) + 'maps\flag-blue.ico', False, False );
  FBitmapGreenFlag := CoObgBitmap.Create();
  FBitmapGreenFlag.LoadPictureFromFile( ExtractFilePath( ParamStr( 0 ) ) + 'maps\flag-green.ico', False, False );
  FBitmapRedFlag := CoObgBitmap.Create();
  FBitmapRedFlag.LoadPictureFromFile( ExtractFilePath( ParamStr( 0 ) ) + 'maps\flag-red.ico', False, False );


  //
  FLayerLine := CoLayerLine.Create();
  FMap.AddLayer( FLayerLine );
  FLayerLine.Color := $005878b8;
  FLayerLine.Visible := True;


  FZ1Line := CoLayerLine.Create();
  FMap.AddLayer( FZ1Line );
  FZ1Line.Color := $002878b8;
  FZ1Line.Visible := True;
  FZ2Line := CoLayerLine.Create();
  FMap.AddLayer( FZ2Line );
  FZ2Line.Color := $005828b8;
  FZ2Line.Visible := True;
  FZ3Line := CoLayerLine.Create();
  FMap.AddLayer( FZ3Line );
  FZ3Line.Color := $00587828;
  FZ3Line.Visible := True;
  //
  FLayerPath := CoLayerLine.Create();
  FMap.AddLayer( FLayerPath );
  FLayerPath.Color := $00a050a0;
  FLayerPath.Visible := True;
  //
  FPBitmap := CoObgBitmap.Create();
  FPBitmap.FillColor := $00a050a0;
  FPBitmap.LoadPictureFromFile( ExtractFilePath( ParamStr( 0 ) ) + 'maps\Arrow_c_w.bmp', True, True );
  //
  FLayerPathA := CoLayerBitmap.Create();
  FMap.AddLayer( FLayerPathA );
  //
//  SetFilePath( PWideChar( ExtractFilePath( ParamStr( 0 ) ) ) );
  //
(*
  FLayerPoly := CoLayerContour.Create();
  FMap.AddLayer( FLayerPoly );
  FLayerPoly.FillColor := $00c09070;
  FLayerPoly.Visible := True;
  FLayerPoly.AddPoint( 55.744278, 37.843216, 1 );
  FLayerPoly.AddPoint( 55.764414, 37.369430, 2 );
  FLayerPoly.AddPoint( 55.906834, 37.541918, 3 );
  FLayerPoly.AddPoint( 55.576495, 37.687912, 4 );
*)
  Edit1Change(Edit1);
end;

procedure TMainForm.FormDestroy(Sender: TObject);
begin
(*
  FLayerPoly.Visible := False;
  FLayerPoly := nil;
*)
  //
  FLayerPathA.Visible := False;
  FLayerPathA := nil;
  FLayerPath.Visible := False;
  FLayerPath := nil;
  FLayerLine.Visible := False;
  FLayerLine := nil;
  FZ1Line.Visible := False;
  FZ1Line := nil;
  FZ2Line.Visible := False;
  FZ2Line := nil;
  FZ3Line.Visible := False;
  FZ3Line := nil;
  FPBitmap := nil;
  FreeAndNil( FMap );
end;

procedure TMainForm.IdSyslogServer1Syslog(Sender: TObject;
  ASysLogMessage: TIdSysLogMessage; ABinding: TIdSocketHandle);
var
  s: string;
  p1: TGeoPos;
  p2: TGeoPos;
  d: string;
  hvPos: Integer;
begin
  s := ASysLogMessage.Msg.Content;

  hvPos := Pos('HV', s);
  if hvPos <> 27 then
    Exit;

  d := Copy(s, hvPos + 2, 1);
  p1 := TGeoHash.DecodePointString(Copy(s, hvPos + 2 + 1, 12));
  p2 := TGeoHash.DecodePointString(Copy(s, hvPos + 2 + 1 + 12, 12));

  if (p1.Latitude = 0) or (p1.Longitude = 0) then
    Exit;

  FLayerPathA.ChainOn();
  if d = 'Z' then
    FLayerPathA.AddPoint(FCounter, p1.Latitude, p1.Longitude, 0,  1, s, FBitmapGreen)
  else if d = 'V' then
    FLayerPathA.AddPoint(FCounter, p1.Latitude, p1.Longitude, 0,  1, s, FBitmapRedFlag)
  else if d = 'G' then
    FLayerPathA.AddPoint(FCounter, p1.Latitude, p1.Longitude, 0,  1, s, FBitmapGreenFlag)
  else if d = 'M' then begin
    FLayerPathA.AddPoint(FCounter, p1.Latitude, p1.Longitude, 0,  1, s, FBitmapRed);
    Inc(FCounter);
    FLayerPathA.AddPoint(FCounter, p2.Latitude, p2.Longitude, 0,  1, s, FBitmapYellow);
  end else if d = 'F' then
    FLayerPathA.AddPoint(FCounter, p1.Latitude, p1.Longitude, 0,  1, s, FBitmapBlueDot)
  else
    FLayerPathA.AddPoint(FCounter, p1.Latitude, p1.Longitude, 0,  1, s, FBitmapBlueCross);
  FLayerPathA.ChainOff();
  Inc(FCounter);

end;

procedure TMainForm.AllKeyPress(Sender: TObject; var Key: Char);
begin
  if ( Key <> #8) and ( Key <> ',' ) and ( ( Key < '0' ) or ( Key > '9' ) ) then
    Key := #0;
end;

procedure TMainForm.Edit1Change(Sender: TObject);
begin
  ReadEditString;
  SetLine;
end;

function DirectionAngle(
  const ALatitudeFrom, ALongitudeFrom, ALatitudeTo, ALongitudeTo: Double
): Double;
begin
  Result := ArcTan( ( ALatitudeTo - ALatitudeFrom ) / ( ( ALongitudeTo - ALongitudeFrom ) * Cos( DegToRad( ALatitudeTo ) ) ) );
  if ( ALongitudeTo < ALongitudeFrom ) then
  begin
    if ( ALatitudeTo > ALatitudeFrom ) then
      Result := Result + Pi
    else
      Result := Result - Pi;
  end;
end;

//------------------------------------------------------------------------------
//!
//------------------------------------------------------------------------------
type
  PCoords = ^TCoords;

  TCoords = packed record
    //! Широта
    Latitude: Double;
    //! Долгота
    Longitude: Double;
    //! Дальше
    Next: PCoords;
  end;

procedure TMainForm.bMakePathClick(Sender: TObject);
const
  CArrowMinDegree = 0.10471975511966; // 6 градусов
var
  Rez: Integer;
//  FromLatitude: Double;
//  FromLongitude: Double;
//  ToLatitude: Double;
//  ToLongitude: Double;
//  Distance: Double;
//  Duration: Double;
  i: Integer;
  //! координаты
//  PrevLatitude, PrevLongitude, Latitude, Longitude: Double;
  //!
//  Di, Du: Integer;
  //! корневой элемент пути
//  PathRoot: PCoords;
  //! указатель для цикла
//  FieldCoords: PCoords;
  //! счётчик отрезков плана
//  CounterMoves, CounterRot: Integer;
  //! временная
//  TempFloat: Double;
  //! направления
//  Dir, PrevDir: Double;
  //!
  KStart, KEnd: TLargeInteger;
  //! число тиков в секунду
  KCounterPerSec: TLargeInteger;
  //! результат PAnsiChar
//  RezChar: PAnsiChar;
  //! результат string
  RezStr: string;
  //! доп. переменная
  TempStr: string;
  //!
  RCoords: TGeoPosArray;
  //!
//  Strs: TStringList;
//------------------------------------------------------------------------------
  ar: TAstarRequest3;

//  CitySpeed: Integer;
//  Speed:  Integer;

  s: string;

  d: Double;

  FormatIncrement: Integer;
  NW, SE: Int64;
const
//  CAcc: array[0..2] of Integer = (23, 1, 0);
  CAcc: array[0..1] of Integer = (83, 0);
begin
  s := Edit1.Text;
  LoadAStar64;
//  s := '53.446922,49.488415,53.255581,50.210890';

  try
//    FromLatitude := FLatiFrom;
//    FromLongitude := FLongiFrom;
//    ToLatitude := FLatiTo;
//    ToLongitude := FLongiTo;
  //  Rez := CreateRouteWithSpeedline(
  QueryPerformanceFrequency( KCounterPerSec );
  QueryPerformanceCounter( KStart );

//  Speed := 58;
  with ar do
  begin
    ToLog(s);
    Version := 1;
    FromLatitude := GetGeo(s);
    FromLongitude := GetGeo(s);
    ToLatitude := GetGeo(s);
    ToLongitude := GetGeo(s);
//    Speed :=
    GetSpeed(s);
//    with RoadSpeedsRecord do begin
//        CitySpeed := Speed;
//        Motorway := Speed;
//        MotorwayLink := Trunc(Speed / 2);
//        Trunk := Speed;
//        TrunkLink := Trunc(Speed / 2);
//        Primary := CitySpeed;
//        PrimaryLink := Trunc(CitySpeed / 2);
//        Secondary := CitySpeed;
//        SecondaryLink := Trunc(CitySpeed / 2);
//        Tertiary := CitySpeed;
//        TertiaryLink := Trunc(CitySpeed / 2);
//        Residential := CitySpeed;
//        Road := Speed;
//        Unclassified := Speed;
//        reverse := 60;
//    end;
//    GetRoadSpeedsDefault(@RoadSpeedsRecord);
//    GetRoadSpeedsKToDef(@RoadSpeedsRecord, Speed, 0);

    ZonesLimit    := GetZones(s);
    FormatVariant := 1;
//    FormatVariant := 5;
    LenTreshold   := 15;
    Timeout       := GetTimeout(s, 1800);
    Distance      := 0;
//    Duration      := 0;
    BufferSize    := 1024*1024;
    HashString    := AllocMem(BufferSize);
    RoadLengthBufferSize := SizeOf(TRoadLengthByTypeRecord) * 10;
    RoadLengthByZoneByType := AllocMem(RoadLengthBufferSize);

    FeatureLimit  := ifthen(CheckBox1.Checked, 1, 0);
  end;

//  Rez := AStarCalc(@ar);
//  Rez := CreateRouteWithPath3(@ar);
//  Rez := AStarCalcAcc(@ar, @CAcc[0]);
  Rez := AStarCalcLandmarksAcc(@ar, NW, SE, @CAcc[0]);
  QueryPerformanceCounter( KEnd );
  sbMain.Panels[1].Text := 'Время выполнения : ' + FormatFloat( '0.000000000', ( KEnd - KStart ) / KCounterPerSec ) + ' сек. '
+ BoolToStr(NW <> 0, True);
    if (Rez = 0) then
    begin
      RezStr := string(AnsiString(ar.HashString));//RezChar
      ToLog(RezStr);
//      Strs := TStringList.Create();
//      Strs.Text := RezStr + #13#10;
//      Strs.SaveToFile(ExtractFilePath(ParamStr(0)) + 'log.txt');
//      Strs.Free();
  //    TGeoHash.DecodeArrayWorld(RezStr, RCoords);
  //
      SetLength(RCoords, 0);
      i :=  ar.FormatStartPos;
      FormatIncrement := ar.FormatIncrement;

      repeat
        TempStr := Copy(RezStr, i, 12);
        if (TempStr = '') then Break;
        SetLength(RCoords, Length(RCoords) + 1);
        TGeoHash.DecodePointString(
          TempStr,
          RCoords[High(RCoords)].Latitude,
          RCoords[High(RCoords)].Longitude
        );

        Inc(i, FormatIncrement);

      until False;
  //
      FLayerPath.ChainOn();
      FLayerPath.ClearAllPoints();
      for i := Low(RCoords) to High(RCoords) do
        FLayerPath.AddPoint(i, RCoords[i].Latitude, RCoords[i].Longitude);

      FLayerPath.ChainOff();
      d := 0;
      for i := 0 to ar.RoadLengthCount - 1 do
        d := d + CRoadSpeedRecordDef.GetDuration(TRoadLengthByTypeRecordArray(ar.RoadLengthByZoneByType)[i]) * 60;

      sbMain.Panels[2].Text :=
        Format('Длина: %.3f км // Время: %.1f мин  // ordinal: %.1f', [ar.Distance, d, ar.Stat.OrdinaryLen]);
    end
    else
    begin
      RezStr := string(AnsiString(ar.HashString));
      ToLog('Rez'+IntTostr(rez));
      ToLog(RezStr);
      FLayerPath.ChainOn();
      FLayerPath.ClearAllPoints();
      FLayerPath.ChainOff();
      sbMain.Panels[2].Text := 'Rez = ' + IntToStr(Rez);
    end;
  finally
    FreeMem(ar.HashString);
    FreeMem(ar.RoadLengthByZoneByType);
    UnloadAStar64;
  end;
end;

procedure TMainForm.Button3Click(Sender: TObject);
type
  TAreaRecord = record
    //! номер (0-63)
    Number: Integer;
    //! используется/не используется
    Active: Integer;
    //! текстовый гео-хэш
    AreaHash: string;
    //! бинарное представление
    Area: TGeoPosArray;
  end;
  TAreaRecordArray = TArray<TAreaRecord>;

//------------------------------------------------------------------------------
var
  CmdArray: TAreaRecordArray;

const
  CCmdFile = 'ImplementedZones.txt';

  procedure ParseZones();
  var
    //!
    I: Integer;
  //------------------------------------------------------------------------------
  begin
    // парсинг
    for I := Low(CmdArray) to High(CmdArray) do
    begin
      TGeoHash.DecodeArrayWorld(CmdArray[I].AreaHash, CmdArray[I].Area);
    end;
  end;

  procedure ReadCmdFile();
  var
    //!
    CmdList: TStringList;
    //!
    TempStr: string;
    //!
    TabPos: Integer;
    //!
    I: Integer;
  //------------------------------------------------------------------------------
  begin
    CmdList := TStringList.Create();
    CmdList.LoadFromFile(IncludeTrailingPathDelimiter(ExtractFilePath(ParamStr(0))) + CCmdFile);
    SetLength(CmdArray, CmdList.Count);
    for I := 0 to CmdList.Count - 1 do
    begin
      TempStr := CmdList[I];
      // _имя
      TabPos := Pos(#9, TempStr);
      Delete(TempStr, 1, TabPos);
      // _приоритет
      TabPos := Pos(#9, TempStr);
      CmdArray[I].Number := StrToInt(Copy(TempStr, 1, TabPos - 1));
      Delete(TempStr, 1, TabPos);
      // _необходимость
      TabPos := Pos(#9, TempStr);
      CmdArray[I].Active := StrToInt(Copy(TempStr, 1, TabPos - 1));
      Delete(TempStr, 1, TabPos);
      // _цвет
      TabPos := Pos(#9, TempStr);
      Delete(TempStr, 1, TabPos);
      // _координаты
      CmdArray[I].AreaHash := TempStr;
    end;
    CmdList.Free();
  end;

  procedure DrawZone(var AZLine: ILayerLine; const aArea: TGeoPosArray);
  var
    I: Integer;
  begin
      AZLine.ChainOn();
      AZLine.ClearAllPoints;
      for I := Low(aArea) to High(aArea) do
      begin
        AZLine.AddPoint(I, aArea[I].Latitude, aArea[I].Longitude);
      end;
      AZLine.ChainOff();

  end;

var
  LMP: TLandMarkPicker;
  ACCS: TIntegerDynArray;
  RCoords: TGeoPosArray;
  Iter: TLandMarkWayKey;
  latiF, longiF, latiT, longiT: Double;
  SlatiF, SlongiF, SlatiT, SlongiT: string;
  HashStr, TempStr: string;
  I: Integer;

begin
  SetLength(ACCS, 2);
  ACCS[0] := 1;
  LMP := TLandMarkPicker.Create(ACCS, 59.940805, 30.456333, 56.129065, 40.393124);
  LMP.LandMarkMatrix.LoadIndex();
  try
    for Iter in LMP.LandMarkMatrix.Keys do
    begin
      if (Iter.z <> 0) then
        Continue;
//2019.11.22 12:06:28.728 pid=6948 tid=15588 connect dir 1 to path (0D3333CFC34B0F34/0D2F75C786D86FEB) <=> (udtmtz1nq3tn/ucvpsy3ehvzc) <=> ([60.0688310,30.1371464]/[56.1969877,40.9922646])
//2019.11.22 12:06:29.117 pid=6948 tid=15588 connect dir 0 to path (0D3333CFC34B0F34/0D3A30C78ED9BBFE) <=> (udtmtz1nq3tn/ufjhsy7emfzy) <=> ([60.0688310,30.1371464]/[57.0759216,40.9950541])
      TGeoHash.DecodePointBin(Iter.v.HashFrom, latiF, longiF);
      TGeoHash.DecodePointBin(Iter.v.HashTo, latiT, longiT);
      if (CompareValue(latiF, 60.0688310, 1e-5) = 0) and (CompareValue(longiF, 30.1371464, 1e-5) = 0)
      and (CompareValue(latiT, 56.1969877, 1e-5) = 0) and (CompareValue(longiT, 40.9922646, 1e-5) = 0) then
      begin
        LMP.LandMarkMatrix.LandMarkWaysChoosen.Add(Iter);
        LMP.LandMarkMatrix.LoadChoosenWays();
        FLayerPath.ChainOn();
        FLayerPath.ClearAllPoints();
        HashStr := LMP.LandMarkMatrix[LMP.LandMarkMatrix.LandMarkWaysChoosen[0]].GeoHash;
        SetLength(RCoords, 0);
        I := 3;
        repeat
          TempStr := Copy(HashStr, I, 12);
          if (TempStr = '') then Break;
          SetLength(RCoords, Length(RCoords) + 1);
          TGeoHash.DecodePointString(
            TempStr,
            RCoords[High(RCoords)].Latitude,
            RCoords[High(RCoords)].Longitude
          );
          Inc(I, 31);
        until False;
        for I := Low(RCoords) to High(RCoords) do
          FLayerPath.AddPoint(I, RCoords[I].Latitude, RCoords[I].Longitude);
        FLayerPath.ChainOff();
        ToLog('this');
      end;
      SlatiF := FloatToStrF(latiF, ffFixed, 15, 7);
      SlongiF := FloatToStrF(longiF, ffFixed, 15, 7);
      SlatiT := FloatToStrF(latiT, ffFixed, 15, 7);
      SlongiT := FloatToStrF(longiT, ffFixed, 15, 7);
      ToLog('[' + SlatiF + '/' + SlongiF + ']-[' + SlatiT + '/' + SlongiT + ']');
      FLayerPathA.ChainOn();
      FLayerPathA.AddPoint(FCounter, latiF, longiF, 0,  1, 'В ' + '[' + SlatiT + '/' + SlongiT + ']', FBitmapGreen);
      Inc(FCounter);
      FLayerPathA.AddPoint(FCounter, latiT, longiT, 0,  1, 'ИЗ ' + '[' + SlatiF + '/' + SlongiF + ']', FBitmapGreen);
      Inc(FCounter);
      FLayerPathA.ChainOff();
    end;
  finally
    LMP.Free();
  end;
  Exit;
  //
  ReadCmdFile;
  ParseZones;
  DrawZone(FZ1Line, CmdArray[0].Area);
  DrawZone(FZ2Line, CmdArray[1].Area);
  DrawZone(FZ3Line, CmdArray[2].Area);
end;

procedure TMainForm.MapClickLeft(
  ASender: TObject;
  ALatitude: Double;
  ALongitude: Double;
  AX: Integer;
  AY: Integer;
  AKeyState: TKeybState
);
begin
  ReadEditString;
  FLatiFrom := ALatitude;
  FLongiFrom := ALongitude;
  MakeEditString;
end;

procedure TMainForm.MapClickRight(
  ASender: TObject;
  ALatitude: Double;
  ALongitude: Double;
  AX: Integer;
  AY: Integer;
  AKeyState: TKeybState
);
begin
  ReadEditString;
  FLatiTo := ALatitude;
  FLongiTo := ALongitude;
  MakeEditString;
end;

{procedure TMainForm.PatchINT3;
var
  NOP: Byte;
  NTDLL: THandle;
  BytesWritten: SIZE_T;//DWORD;
  Address: Pointer;
begin
  if Win32Platform <> VER_PLATFORM_WIN32_NT then
    Exit;
  NTDLL := GetModuleHandle('NTDLL.DLL');
  if NTDLL = 0 then
    Exit;
  Address := GetProcAddress(NTDLL, 'DbgBreakPoint');
  if Address = nil then
    Exit;
  try
    if Char(Address^) <> #$CC then
      Exit;
    NOP := $90;
    if WriteProcessMemory(GetCurrentProcess, Address, @NOP, 1, BytesWritten) and
      (BytesWritten = 1) then
      FlushInstructionCache(GetCurrentProcess, Address, 1);
  except
    //Do not panic if you see an EAccessViolation here, it is perfectly harmless!
    on EAccessViolation do
      ;
  else
    raise;
  end;
end;}

function GeoLengthLongRad(
  const ALatitude1, ALongitude1, ALatitude2, ALongitude2: Double;
  const ARadius: Double
): Double;
begin
  Result := ArcCos( Sin( ALatitude1 ) * Sin( ALatitude2 ) + Cos( ALatitude1 ) * Cos( ALatitude2 ) * Cos( ALongitude1 - ALongitude2 ) ) * ARadius;
end;

procedure TMainForm.SetLine();
var
  //!
  Len: Double;
//------------------------------------------------------------------------------
begin
  Len := GeoLengthLongRad(
    DegToRad( FLatiFrom ),
    DegToRad( FLongiFrom ),
    DegToRad( FLatiTo ),
    DegToRad( FLongiTo ),
    6371.302
  );
  sbMain.Panels[0].Text := Format( 'Расстояние: %.3f Км', [Len] );
  //
  FLayerLine.ChainOn();
  FLayerLine.ClearAllPoints();
  FLayerLine.AddPoint( 0, FLatiFrom, FLongiFrom );
  FLayerLine.AddPoint( 1, FLatiTo, FLongiTo );
  FLayerLine.ChainOff();
end;

//procedure TMainForm.SetupStructure();
//begin
//end;

procedure TMainForm.ReadEditString;
var
  s: string;
begin
  s := Edit1.Text;
  FLatiFrom := GetGeo(s);
  FLongiFrom := GetGeo(s);
  FLatiTo := GetGeo(s);
  FLongiTo := GetGeo(s);
  FSpeed := GetSpeed(s);
  FMask := GetZones(s);
end;

procedure TMainForm.MakeEditString;
var
//  s: string;
  fs: TFormatSettings;
begin
  fs := TFormatSettings.Create('ru-ru');
  fs.DecimalSeparator := '.';
  Edit1.Text := format('[%.6f,%.6f]-[%.6f,%.6f] %d %d', [FLatiFrom, FLongiFrom, FLatiTo, FLongiTo, FSpeed, FMask],  fs);
//  [57.325853,61.901568]-[57.376582,62.291365] 58 0
end;

procedure TMainForm.bChangeClick(Sender: TObject);
var
  l: Double;
//  s: string;
//------------------------------------------------------------------------------
begin
  ReadEditString;

  l := FLatiTo;
  FLatiTo := FLatiFrom;
  FLatiFrom := l;
  l := FLongiTo;
  FLongiTo := FLongiFrom;
  FLongiFrom := l;

  MakeEditString;

  SetLine();
end;

procedure TMainForm.Button4Click(Sender: TObject);
var
  la, lo: Double;
  fs: TFormatSettings;
begin
  fs := TFormatSettings.Create;
  fs.DecimalSeparator := '.';
  if TGeoHash.DecodePointString(edtPin.Text, la, lo) then
  begin
    ToLog('['+FloatToStr(la, fs)+','+FloatToStr(lo, fs)+']');
    FLayerPathA.ChainOn();
    FLayerPathA.AddPoint(FCounter, la, lo, 0,  1, edtPin.Text, FBitmapGreen);
    FLayerPathA.ChainOff();
    Inc(FCounter);
//    FLayerPathA
//    FBitmapGreen
//    pin
  end;
end;

procedure TMainForm.Button5Click(Sender: TObject);
begin
  FLayerPathA.ChainOn();
  FLayerPathA.ClearAllPoints;
  FLayerPathA.ChainOff();
  FCounter := 0;
end;

initialization
//  LoadAStar64;

finalization
//  UnloadAStar64;

end.


//TODO: check [44.927638,34.048069]-[51.666063,39.230919] 90 0 - 300 секунд * 3 мало
