unit LandMarkGeneratorFrm;

interface

{$define TEST_NEW}

uses
  SysUtils, Windows, Messages, Variants, Classes, Graphics, Controls, Forms,
  Math, Dialogs, StdCtrls, ExtCtrls, ComCtrls,
  AStar64.DynImport, AStar64.Typ, AStar64.Common, Geo.Pos, Geo.Hash,
  ILSMapControlEx_TLB, System.Win.ComObj, ActiveX,
  Ils.Logger, Ils.Utils, Generics.Collections, AStar64.LandMark, AStar64.FileStructs;

type
  TMainForm = class(TForm)
    pMap: TPanel;
    sbMain: TStatusBar;
    Panel1: TPanel;
    Edit1: TEdit;
    Button1: TButton;
    Button3: TButton;
    Button4: TButton;
    edtPin: TEdit;
    Button5: TButton;
    CheckBox1: TCheckBox;
    ProgressBar1: TProgressBar;
    Button2: TButton;
    Button6: TButton;
    Memo1: TMemo;
    Splitter1: TSplitter;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure AllKeyPress(Sender: TObject; var Key: Char);
    procedure bMakePathClick(Sender: TObject);
    procedure bChangeClick(Sender: TObject);
    procedure Edit1Change(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure Button5Click(Sender: TObject);
    procedure Button1Click(Sender: TObject);
  private
    FBitmapGreen: IObgBitmap;
    FBitmapBlue: IObgBitmap;
    FBitmapRed: IObgBitmap;
    FBitmapYellow: IObgBitmap;
    FBitmapCian: IObgBitmap;
    FBitmapBlueDot: IObgBitmap;
    FBitmapBlueArrow: IObgBitmap;
    FBitmapBlueCross: IObgBitmap;

    FMap: TILSMapEx;

    FZ1Line: ILayerLine;
    FZ2Line: ILayerLine;
    FZ3Line: ILayerLine;
    FLayerLine: ILayerLine;

    FLayerPath: ILayerLine;
    FLayerPathList: TList<ILayerLine>;
    //!
    FLayerPathA: ILayerBitmap;
    //!
    FPBitmap: IObgBitmap;
    //! ТЕСТ
//    FLayerPoly: ILayerContour;
    //!
    FLatiLMAdd, FLongiLMAdd: Double;
    FLatiFrom, FLongiFrom: Double;
    FLatiTo, FLongiTo: Double;
    FSpeed: Integer;
    FMask: UInt64;
    //!
//    FStructure: TAStarParam;
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


function CallBack(
  const AHandle: Pointer;
  const AMetaData: Pointer
): Integer; stdcall;
begin
  Result := RoadSpeedByType(PMetaDataV1(AMetaData)^.RoadType);
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
  FLayerPathList := TList<ILayerLine>.Create;
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

  FLayerPathList.Clear;
  FLayerPathList.Free;
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
//  SetLM;
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
    LenTreshold   := 15;
    Timeout       := GetTimeout(s, 900);//000);
    Distance      := 0;
//    Duration      := 0;
    BufferSize    := 1024*1024;
    HashString    := AllocMem(1024*1024);
    RoadLengthBufferSize := SizeOf(TRoadLengthByTypeRecord) * 10;
    RoadLengthByZoneByType := AllocMem(RoadLengthBufferSize);

    FeatureLimit  := ifthen(CheckBox1.Checked, 1, 0);
  end;

  Rez := AStarCalc(@ar);
//  Rez := CreateRouteWithPath3(@ar);
  QueryPerformanceCounter( KEnd );
  sbMain.Panels[1].Text := 'Время выполнения : ' + FormatFloat( '0.000000000', ( KEnd - KStart ) / KCounterPerSec ) + ' сек.';
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


//procedure TMainForm.bMakePathClick(Sender: TObject);
//type
//  TPointPair = record
//    s: TGeoPos;
//    t: TGeoPos;
//  end;
//
//const
//  CArrowMinDegree = 0.10471975511966; // 6 градусов
//var
//  Rez: Integer;
////  FromLatitude: Double;
////  FromLongitude: Double;
////  ToLatitude: Double;
////  ToLongitude: Double;
////  Distance: Double;
////  Duration: Double;
//  i: Integer;
//  //! координаты
////  PrevLatitude, PrevLongitude, Latitude, Longitude: Double;
//  //!
////  Di, Du: Integer;
//  //! корневой элемент пути
////  PathRoot: PCoords;
//  //! указатель для цикла
////  FieldCoords: PCoords;
//  //! счётчик отрезков плана
////  CounterMoves, CounterRot: Integer;
//  //! временная
////  TempFloat: Double;
//  //! направления
////  Dir, PrevDir: Double;
//  //!
//  KStart, KEnd: TLargeInteger;
//  //! число тиков в секунду
//  KCounterPerSec: TLargeInteger;
//  //! результат PAnsiChar
////  RezChar: PAnsiChar;
//  //! результат string
//  RezStr: string;
//  //! доп. переменная
//  TempStr: string;
//  //!
//  RCoords: TGeoPosArray;
//  //!
////  Strs: TStringList;
////------------------------------------------------------------------------------
//  ar: TAstarRequest3;
//
////  CitySpeed: Integer;
////  Speed:  Integer;
//
//  s: string;
//
//  d: Double;
//
//  FormatIncrement: Integer;
//
//  LayerPath: ILayerLine;
//
//  procedure MakeLights(const asp, atp: TGeoPos; const StepLa, StepLo: Double; AMatrix: TDictionary<TPointPair, string>);
//  var
//    k: TPointPair;
//    LightArray: TDictionary<TGeoPos,Integer>;
//    l: TGeoPos;
//    s, t: TGeoPos;
//  begin
//    l := asp;
//    LightArray := TDictionary<TGeoPos,Integer>.Create(100);
//
//    try
//      while l.Latitude < atp.Latitude do
//      begin
//        LightArray.AddOrSetValue(l, 1);
//        l.Latitude := l.Latitude + StepLa;
//      end;
//
//      while l.Longitude < atp.Longitude do
//      begin
//        LightArray.AddOrSetValue(l, 1);
//        l.Longitude := l.Longitude + StepLo;
//      end;
//
//      while l.Latitude > asp.Latitude do
//      begin
//        LightArray.AddOrSetValue(l, 1);
//        l.Latitude := l.Latitude - StepLa;
//      end;
//
//      while l.Longitude > asp.Longitude do
//      begin
//        LightArray.AddOrSetValue(l, 1);
//        l.Longitude := l.Longitude - StepLo;
//      end;
//
//      LightArray.AddOrSetValue(l, 1);
//
//            FLayerPathA.ChainOn();
//      for s in LightArray.Keys do
//        for t in LightArray.Keys do
//          if s <> t then
//          begin
//            k.s := s;
//            k.t := t;
//            AMatrix.Add(k, '');
//
//            FLayerPathA.AddPoint(FCounter,s.Latitude, s.Longitude, 0,  1, '', FBitmapBlueCross);
//            Inc(FCounter);
//
//          end;
//            FLayerPathA.ChainOff();
//    finally
//      LightArray.Free
//    end;
//  end;
//
//var
//  pp: TPointPair;
//  LigtsMatrix: TDictionary<TPointPair, string>;
//
//begin
//  s := Edit1.Text;
//
//  LigtsMatrix := TDictionary<TPointPair, string>.Create;
//
//  try
//    MakeLights(TGeoPos.Create(55.5, 37.0), TGeoPos.Create(56.0, 38.0), 0.5, 1.0, LigtsMatrix);
//
//    ProgressBar1.Max := LigtsMatrix.Count;
//    ProgressBar1.Position := 0;
//    for pp in LigtsMatrix.Keys do
//      try
//        LoadAStar64;
//        QueryPerformanceFrequency( KCounterPerSec );
//        QueryPerformanceCounter( KStart );
//
//        with ar do
//        begin
//          ToLog(s);
//          Version := 1;
//          FromLatitude := pp.s.Latitude;// GetGeo(s);
//          FromLongitude := pp.s.Longitude;// GetGeo(s);
//          ToLatitude := pp.t.Latitude;// GetGeo(s);
//          ToLongitude := pp.t.Longitude;// GetGeo(s);
//      //    Speed :=
//          GetSpeed(s);
//          GetZones(s);
//
//          ZonesLimit    := 0;//GetZones(s);
//          FormatVariant := 1;
//          LenTreshold   := 15;
//          Timeout       := GetTimeout(s, 900);//000);
//          Distance      := 0;
//      //    Duration      := 0;
//          BufferSize    := 1024*1024;
//          HashString    := AllocMem(1024*1024);
//          RoadLengthBufferSize := SizeOf(TRoadLengthByTypeRecord) * 10;
//          RoadLengthByZoneByType := AllocMem(RoadLengthBufferSize);
//
//          FeatureLimit  := ifthen(CheckBox1.Checked, 1, 0);
//        end;
//
//        Rez := AStarCalc(@ar);
//
//        QueryPerformanceCounter( KEnd );
//        sbMain.Panels[1].Text := 'Время выполнения : ' + FormatFloat( '0.000000000', ( KEnd - KStart ) / KCounterPerSec ) + ' сек.';
//
//        LayerPath := CoLayerLine.Create();
//        FMap.AddLayer( LayerPath );
//        LayerPath.Color := $00a050a0;
//        LayerPath.Visible := True;
//
//        if (Rez = 0) then
//        begin
//          RezStr := string(AnsiString(ar.HashString));
//          ToLog(RezStr);
//
//          SetLength(RCoords, 0);
//          i :=  ar.FormatStartPos;
//          FormatIncrement := ar.FormatIncrement;
//
//          repeat
//            TempStr := Copy(RezStr, i, 12);
//            if (TempStr = '') then
//              Break;
//
//            SetLength(RCoords, Length(RCoords) + 1);
//            TGeoHash.DecodePointString(
//              TempStr,
//              RCoords[High(RCoords)].Latitude,
//              RCoords[High(RCoords)].Longitude
//            );
//
//            Inc(i, FormatIncrement);
//
//          until False;
//
//          LayerPath.ChainOn();
//          LayerPath.ClearAllPoints();
//          for i := Low(RCoords) to High(RCoords) do
//            LayerPath.AddPoint(i, RCoords[i].Latitude, RCoords[i].Longitude);
//
//          LayerPath.ChainOff();
//          d := 0;
//          for i := 0 to ar.RoadLengthCount - 1 do
//            d := d + CRoadSpeedRecordDef.GetDuration(TRoadLengthByTypeRecordArray(ar.RoadLengthByZoneByType)[i]) * 60;
//
//          sbMain.Panels[2].Text :=
//            Format('Длина: %.3f км // Время: %.1f мин  // ordinal: %.1f', [ar.Distance, d, ar.Stat.OrdinaryLen]);
//        end
//        else
//        begin
//          RezStr := string(AnsiString(ar.HashString));
//          ToLog('Rez'+IntTostr(rez));
//          ToLog(RezStr);
//          LayerPath.ChainOn();
//          LayerPath.ClearAllPoints();
//          LayerPath.ChainOff();
//          sbMain.Panels[2].Text := 'Rez = ' + IntToStr(Rez);
//        end;
//        ProgressBar1.Position := ProgressBar1.Position + 1;
//        Application.ProcessMessages;
//
//      finally
//        FreeMem(ar.HashString);
//        FreeMem(ar.RoadLengthByZoneByType);
//        UnloadAStar64;
//      end;
//  finally
//    LigtsMatrix.Free;
//  end;
//end;

procedure TMainForm.Button1Click(Sender: TObject);
const
  CArrowMinDegree = 0.10471975511966; // 6 градусов
var
  Rez: Integer;
  i: Integer;
  KCounterPerSec: TLargeInteger;
  KStart, KEnd, KFullStart, KFullEnd: TLargeInteger;
  RezStr: string;
  TempStr: string;
  TempBegin: string;
  TempEnd: string;
  TempNode: string;
  TempType: string;
  TempZone: string;
  ar: TAstarRequest4;
  d: Double;
  FormatIncrement: Integer;
  LayerPath: ILayerLine;

  procedure Visualize(
    const AFormatStartPos: Integer;
    const AFormatIncrement: Integer
  );
  var
    RCoords: TGeoPosArray;
    i: Integer;
  begin
    i := AFormatStartPos;
    FLayerPathA.ChainOn();
    try
      repeat
        TempStr := Copy(RezStr, i + (12 + 19), 12);
        if (TempStr = '') then
          Break;

        TempBegin := Copy(RezStr, i + 0, 12);
        TempNode := Copy(RezStr, i + 12, 1);
        TempType := Copy(RezStr, i + (12 + 1), 2);
        TempZone := Copy(RezStr, i + (12 + 1 + 2), 16);
        TempEnd := Copy(RezStr, i + (12 + 1 + 2 + 16), 12);

        SetLength(RCoords, Length(RCoords) + 1);
        TGeoHash.DecodePointString(
          TempStr,
          RCoords[High(RCoords)].Latitude,
          RCoords[High(RCoords)].Longitude
        );
        FLayerPathA.AddPoint(
          FCounter,
          RCoords[High(RCoords)].Latitude,
          RCoords[High(RCoords)].Longitude, 0,  1, '', FBitmapBlueDot
        );
        Inc(FCounter);
        Inc(i, AFormatIncrement);
      until False;
    finally
      FLayerPathA.ChainOff();
    end;
    //
    LayerPath.ChainOn();
    try
      LayerPath.ClearAllPoints();
      for i := Low(RCoords) to High(RCoords) do
      begin
        LayerPath.AddPoint(i, RCoords[i].Latitude, RCoords[i].Longitude);
      end;
    finally
      LayerPath.ChainOff();
    end;
  end;

  function GetZones(
    const AFormatStartPos: Integer;
    const AFormatIncrement: Integer
  ): Int64;
  var
    i: Integer;
  begin
    Result := 0;
    i := AFormatStartPos;
    repeat
      if (Copy(RezStr, i + (12 + 19), 12) = '') then
        Break;
      Result := Result or StrToInt64('$' + Copy(RezStr, i + (12 + 1 + 2), 16));
      Inc(i, AFormatIncrement);
    until False;
  end;

  procedure MakeLights(const asp, atp: TGeoPos; const StepLa, StepLo: Double; const AMatrix: TLandMarkMatrix);
  var
    k: TLandMarkWayKey;
    LandMarks: TDictionary<TGeoPos,Integer>;
    l: TGeoPos;
    s, t: TGeoPos;
  begin
    l := asp;
    LandMarks := TDictionary<TGeoPos,Integer>.Create(100);
    try
      while l.Latitude < atp.Latitude do
      begin
        LandMarks.AddOrSetValue(l, 1);
        l.Latitude := l.Latitude + StepLa;
      end;
      while l.Longitude < atp.Longitude do
      begin
        LandMarks.AddOrSetValue(l, 1);
        l.Longitude := l.Longitude + StepLo;
      end;
      while l.Latitude > asp.Latitude do
      begin
        LandMarks.AddOrSetValue(l, 1);
        l.Latitude := l.Latitude - StepLa;
      end;
      while l.Longitude > asp.Longitude do
      begin
        LandMarks.AddOrSetValue(l, 1);
        l.Longitude := l.Longitude - StepLo;
      end;
      LandMarks.AddOrSetValue(l, 1);
      //
FLayerPathA.ChainOn();
      for s in LandMarks.Keys do
        for t in LandMarks.Keys do
          if s <> t then
          begin
            k.v.HashFrom := TGeoHash.EncodePointBin(s.Latitude, s.Longitude);
            k.v.HashTo := TGeoHash.EncodePointBin(t.Latitude, t.Longitude);
            k.z := 0;
            if not AMatrix.ContainsKey(k) then
              AMatrix.Add(k, TLandMarkWay.Create(''))
            else
              AMatrix.Items[k].Clear;
FLayerPathA.AddPoint(FCounter,s.Latitude, s.Longitude, 0,  1, '', FBitmapBlueCross);
            Inc(FCounter);
          end;
FLayerPathA.ChainOff();
    finally
      LandMarks.Free
    end;
  end;

var
  zp, zi, zx: Int64;
  lmk, lmki: TLandMarkWayKey;
  LandMarkMatrix: TLandMarkMatrix;
  CrossedList: TDictionary<TZoneControl, Boolean>;
  ZonesIdxArr: TArray<Integer>;
  IsNode: Boolean;
  AllWaysDone: Boolean;
begin
  ToLog('--------------------------------------------------------------------');
  CrossedList := nil;
  LandMarkMatrix := nil;
  try
    CrossedList := TDictionary<TZoneControl, Boolean>.Create();
    LandMarkMatrix := TLandMarkMatrix.Create(ExtractFilePath(Application.ExeName) + '1\', 'ucfq9fqxdkxf_ucg58xqybjvf');
    // загрузка предыдущего файла
    LandMarkMatrix.LoadIndex;
    ProgressBar1.Max := LandMarkMatrix.Count;
    ProgressBar1.Position := 0;
    QueryPerformanceFrequency( KCounterPerSec );
    QueryPerformanceCounter(KFullStart);
    // !!! генерация маяков (-добавление новых, очистка загруженного пути у старых-) !!!
//    MakeLights(TGeoPos.Create(55.5, 37.0), TGeoPos.Create(56.0, 38.0), 0.05, 0.1, LandMarkMatrix);
//    MakeLights(TGeoPos.Create(55.5, 37.0), TGeoPos.Create(56.0, 38.0), 0.1, 0.2, LandMarkMatrix);
    MakeLights(TGeoPos.Create(55.5, 37.0), TGeoPos.Create(56.0, 38.0), 0.5, 0.5, LandMarkMatrix);
  // расчёт
  repeat // пока есть нерассчитанные пути в LandMarkMatrix
    AllWaysDone := True;
    for lmk in LandMarkMatrix.Keys do
    begin
      if (LandMarkMatrix.Items[lmk].GeoHash <> '') then
        Continue;
      lmki := lmk;
      AllWaysDone := False;
      try
        LoadAStar64;
        QueryPerformanceCounter( KStart );
        FillChar(ar, SizeOf(ar), 0);
        with ar do
        begin
//  Version    FromLatitude    FromLongitude    ToLatitude    ToLongitude
//  ZonesLimit    RoadTypeLimit    OsmTypeLimit    Feature    FormatVariant
//  LenTreshold    Timeout    Distance    Stat
//  RoadLengthAggregateBufferSize    RoadLengthAggregate    RoadLengthCount
//  BufferSize    HashString    SignsLimit
          Version := 1;
          FromLatitude := lmk.v.PointFrom.Latitude;// GetGeo(s);
          FromLongitude := lmk.v.PointFrom.Longitude;// GetGeo(s);
          ToLatitude := lmk.v.PointTo.Latitude;// GetGeo(s);
          ToLongitude := lmk.v.PointTo.Longitude;// GetGeo(s);
          ZonesLimit := lmk.z; // !!! ВСЕ СЮДА !!!
          FormatVariant := Ord(gfLandMark);
          LenTreshold   := 15;
          Timeout       := 900;//GetTimeout(s, 900);//000);
          BufferSize    := 1024*1024;
          HashString    := AllocMem(BufferSize);
        end;
        Rez := AStarCalc4(@ar);
        QueryPerformanceCounter( KEnd );
        sbMain.Panels[1].Text := 'Время выполнения : ' + FormatFloat( '0.000000000', ( KEnd - KStart ) / KCounterPerSec ) + ' сек.';
        LayerPath := CoLayerLine.Create();
        FMap.AddLayer( LayerPath );
        LayerPath.Color := $00a050a0;
        LayerPath.Visible := True;
        if (Rez = 0) then
        begin
          RezStr := string(AnsiString(ar.HashString));
          ToLog(RezStr);
          Delete(RezStr, 3, 12 + 19);
          Delete(RezStr, Length(RezStr) + (1 - 12 - 19), 12 + 19);
          ToLog(RezStr);
          zp := GetZones(ar.FormatStartPos, ar.FormatIncrement);
          LandMarkMatrix.Items[lmk].GeoHash := RezStr;
// lmk.z - исключённые зоны
// zp - те зоны, по которым мы прошли
//     lmk  zp  /
//     0    0   / тривиально = не было и нет = неисключённая зона, по которой не прошли
//     0    1   / новая зона
//     1    0   / исключённая зона, по которой не прошли
//     1    1   / невозможно
//          zi := zp and not lmk.z; // список новых зон
//          zx := lmk.z and not zp; // список исключённых зон
//          if (zi = 0) then
//          begin
//
//          end
//          else
//          begin
//            SetLength(ZonesIdxArr, 0);
//            for i := GetMinBitIndex(zi) to GetMaxBitIndex(zi) do
//            begin
//              if (((UInt64(1) shl i) and zi) <> 0) then
//              begin
//                SetLength(ZonesIdxArr, Length(ZonesIdxArr) + 1);
//                ZonesIdxArr[High(ZonesIdxArr)] := i;
//              end;
//            end;
//            for i := Low(ZonesIdxArr) to High(ZonesIdxArr) do
//            begin
//
//            end;
//          end;
//          if (lmk.z = zp) then
//          begin // в точности по заданным зонам
//            LandMarkMatrix.Items[lmk].GeoHash := RezStr;
//          end
//          else // if ((lmk.z <> zp) <> 0) then
//          begin // тут может быть бит в zp, при его отсутствии в lmk.z
//            lmki.v := lmk.v;
//            lmki.z := 0;
//            if not LandMarkMatrix.ContainsKey(lmki) then
//              LandMarkMatrix.Add(lmki, TLandMarkWay.Create(''));
//          end;
          // vb
          Visualize(ar.FormatStartPos, ar.FormatIncrement);
          d := 0;
          for i := 0 to ar.RoadLengthCount - 1 do
            d := d + CRoadSpeedRecordDef.GetDuration(TRoadLengthAggregateRecordArray(ar.RoadLengthAggregate)[i]) * 60;
          sbMain.Panels[2].Text := Format('Длина: %.3f км // Время: %.1f мин  // ordinal: %.1f', [ar.Distance, d, ar.Stat.OrdinaryLen]);
          // ve
        end
        else
        begin
          RezStr := string(AnsiString(ar.HashString));
          ToLog('Rez = ' + IntTostr(Rez));
          ToLog(RezStr);
          LayerPath.ChainOn();
          LayerPath.ClearAllPoints();
          LayerPath.ChainOff();
          sbMain.Panels[2].Text := 'Rez = ' + IntToStr(Rez);
        end;
        ProgressBar1.Position := ProgressBar1.Position + 1;
        Application.ProcessMessages;
      finally
        FreeMem(ar.HashString);
        FreeMem(ar.RoadLengthAggregate);
        UnloadAStar64;
      end;
      Break;
    end;
    QueryPerformanceCounter( KFullEnd );
    sbMain.Panels[1].Text := 'Время выполнения : ' + FormatFloat( '0.000000000', ( KFullEnd - KFullStart ) / KCounterPerSec ) + ' сек.';
    until AllWaysDone;
  finally
    LandMarkMatrix.Save;
    LandMarkMatrix.Free;
    CrossedList.Free();
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

begin
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
  if CheckBox1.Checked then
  begin
    ReadEditString;
    FLatiFrom := ALatitude;
    FLongiFrom := ALongitude;
    MakeEditString;
  end else begin
    FLatiLMAdd := ALatitude;
    FLongiLMAdd := ALongitude;
  end;
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
  if CheckBox1.Checked then
  begin
    ReadEditString;
    FLatiTo := ALatitude;
    FLongiTo := ALongitude;
    MakeEditString;
  end else begin

  end;
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
