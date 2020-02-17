unit ILSMapControlEx_TLB;

// ************************************************************************ //
// WARNING                                                                    
// -------                                                                    
// The types declared in this file were generated from data read from a       
// Type Library. If this type library is explicitly or indirectly (via        
// another type library referring to this type library) re-imported, or the   
// 'Refresh' command of the Type Library Editor activated while editing the   
// Type Library, the contents of this file will be regenerated and all        
// manual modifications will be lost.                                         
// ************************************************************************ //

// $Rev: 45604 $
// File generated on 04.12.2017 16:39:07 from Type Library described below.

// ************************************************************************  //
// Type Lib: E:\Git_WORK\bin\New ILS Map\ILSMapControlEx.ocx (1)
// LIBID: {8E443A87-1266-4CFD-B9F0-1F75A540CF73}
// LCID: 0
// Helpfile: 
// HelpString: 
// DepndLst: 
//   (1) v2.0 stdole, (C:\Windows\SysWOW64\stdole2.tlb)
// SYS_KIND: SYS_WIN32
// Errors:
//   Error creating palette bitmap of (TILSMapEx) : Error reading control bitmap
//   Error creating palette bitmap of (TLayerLine) : Server E:\ILSPRO~1\bin\NEWILS~1\ILSMAP~1.OCX contains no icons
//   Error creating palette bitmap of (TLayerContour) : Server E:\ILSPRO~1\bin\NEWILS~1\ILSMAP~1.OCX contains no icons
//   Error creating palette bitmap of (TLayerBitmap) : Server E:\ILSPRO~1\bin\NEWILS~1\ILSMAP~1.OCX contains no icons
//   Error creating palette bitmap of (TLayerText) : Server E:\ILSPRO~1\bin\NEWILS~1\ILSMAP~1.OCX contains no icons
//   Error creating palette bitmap of (TObgBitmap) : Server E:\ILSPRO~1\bin\NEWILS~1\ILSMAP~1.OCX contains no icons
// ************************************************************************ //
{$TYPEDADDRESS OFF} // Unit must be compiled without type-checked pointers. 
{$WARN SYMBOL_PLATFORM OFF}
{$WRITEABLECONST ON}
{$VARPROPSETTER ON}
{$ALIGN 4}

interface

uses Winapi.Windows, System.Classes, System.Variants, System.Win.StdVCL, Vcl.Graphics, Vcl.OleCtrls, Vcl.OleServer, Winapi.ActiveX;
  


// *********************************************************************//
// GUIDS declared in the TypeLibrary. Following prefixes are used:        
//   Type Libraries     : LIBID_xxxx                                      
//   CoClasses          : CLASS_xxxx                                      
//   DISPInterfaces     : DIID_xxxx                                       
//   Non-DISP interfaces: IID_xxxx                                        
// *********************************************************************//
const
  // TypeLibrary Major and minor versions
  ILSMapControlExMajorVersion = 1;
  ILSMapControlExMinorVersion = 0;

  LIBID_ILSMapControlEx: TGUID = '{8E443A87-1266-4CFD-B9F0-1F75A540CF73}';

  IID_IILSMapEx: TGUID = '{04C663DF-43A3-47BF-915C-40CA48DF0C72}';
  DIID_IILSMapExEvents: TGUID = '{D27F75DF-B3C1-4B93-8086-B75E94E4B38F}';
  CLASS_ILSMapEx: TGUID = '{F26AC771-4B1F-40F4-8089-D629E5DD9CE8}';
  IID_ILayer: TGUID = '{FEED1057-9B61-44E3-8410-FD479824C867}';
  IID_ILayerLine: TGUID = '{87617272-2948-4509-A670-E71FAC966D3C}';
  CLASS_LayerLine: TGUID = '{F72A8F28-5882-4F6A-9D6D-F824848C9C6B}';
  IID_ILayerContour: TGUID = '{2DE64A6B-DAB2-4F4E-940B-C4CBB504182D}';
  CLASS_LayerContour: TGUID = '{94D694D2-CF1A-4222-A118-7B6F0CBF4B98}';
  IID_ILayerBitmap: TGUID = '{F9163EA4-A8F7-4992-A477-B60E0ABC4E14}';
  CLASS_LayerBitmap: TGUID = '{AC5597CB-D25E-4A43-B37F-D2CC6583A9FB}';
  IID_ILayerText: TGUID = '{3FF8BA94-887E-40C9-86DF-AF4F2C792FFE}';
  CLASS_LayerText: TGUID = '{918A84C4-E54B-405F-B3F7-A9F2A5BB477B}';
  IID_IObgBitmap: TGUID = '{B91033AC-2CC9-43BF-A4B4-0D481584ADEE}';
  CLASS_ObgBitmap: TGUID = '{DA136049-BD56-4881-9AB3-641ED2D23861}';

// *********************************************************************//
// Declaration of Enumerations defined in Type Library                    
// *********************************************************************//
// Constants for enum TxAlignment
type
  TxAlignment = TOleEnum;
const
  taLeftJustify = $00000000;
  taRightJustify = $00000001;
  taCenter = $00000002;

// Constants for enum TxBevelCut
type
  TxBevelCut = TOleEnum;
const
  bvNone = $00000000;
  bvLowered = $00000001;
  bvRaised = $00000002;
  bvSpace = $00000003;

// Constants for enum TxBevelKind
type
  TxBevelKind = TOleEnum;
const
  bkNone = $00000000;
  bkTile = $00000001;
  bkSoft = $00000002;
  bkFlat = $00000003;

// Constants for enum TxBorderStyle
type
  TxBorderStyle = TOleEnum;
const
  bsNone = $00000000;
  bsSingle = $00000001;

// Constants for enum TxDragMode
type
  TxDragMode = TOleEnum;
const
  dmManual = $00000000;
  dmAutomatic = $00000001;

// Constants for enum TxVerticalAlignment
type
  TxVerticalAlignment = TOleEnum;
const
  taAlignTop = $00000000;
  taAlignBottom = $00000001;
  taVerticalCenter = $00000002;

// Constants for enum TxMouseButton
type
  TxMouseButton = TOleEnum;
const
  mbLeft = $00000000;
  mbRight = $00000001;
  mbMiddle = $00000002;

// Constants for enum TKeybState
type
  TKeybState = TOleEnum;
const
  ksNone = $00000000;
  ksShift = $00000004;
  ksControl = $00000008;
  ksControlShift = $0000000C;

// Constants for enum TMapProvider
type
  TMapProvider = TOleEnum;
const
  mpNone = $00000000;
  mpIngit = $00000001;
  mpGoogle = $00000002;
  mpYandex = $00000003;
  mpOpenStreet = $00000004;
  mpMosMap = $00000005;
  mpILSMap = $00000006;

// Constants for enum TMapMode
type
  TMapMode = TOleEnum;
const
  mmSelect = $00000000;
  mmMeasuringDistance = $00000001;
  mmEditZone = $00000002;
  mmEditObject = $00000003;

// Constants for enum TZoneType
type
  TZoneType = TOleEnum;
const
  ztTransparent = $00000000;
  ztSemi = $00000001;
  ztNet = $00000002;
  ztCross = $00000003;

type

// *********************************************************************//
// Forward declaration of types defined in TypeLibrary                    
// *********************************************************************//
  IILSMapEx = interface;
  IILSMapExDisp = dispinterface;
  IILSMapExEvents = dispinterface;
  ILayer = interface;
  ILayerDisp = dispinterface;
  ILayerLine = interface;
  ILayerLineDisp = dispinterface;
  ILayerContour = interface;
  ILayerContourDisp = dispinterface;
  ILayerBitmap = interface;
  ILayerBitmapDisp = dispinterface;
  ILayerText = interface;
  ILayerTextDisp = dispinterface;
  IObgBitmap = interface;
  IObgBitmapDisp = dispinterface;

// *********************************************************************//
// Declaration of CoClasses defined in Type Library                       
// (NOTE: Here we map each CoClass to its Default Interface)              
// *********************************************************************//
  ILSMapEx = IILSMapEx;
  LayerLine = ILayerLine;
  LayerContour = ILayerContour;
  LayerBitmap = ILayerBitmap;
  LayerText = ILayerText;
  ObgBitmap = IObgBitmap;


// *********************************************************************//
// Declaration of structures, unions and aliases.                         
// *********************************************************************//
  PPUserType1 = ^IFontDisp; {*}


// *********************************************************************//
// Interface: IILSMapEx
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {04C663DF-43A3-47BF-915C-40CA48DF0C72}
// *********************************************************************//
  IILSMapEx = interface(IDispatch)
    ['{04C663DF-43A3-47BF-915C-40CA48DF0C72}']
    function Get_Alignment: TxAlignment; safecall;
    procedure Set_Alignment(Value: TxAlignment); safecall;
    function Get_AutoSize: WordBool; safecall;
    procedure Set_AutoSize(Value: WordBool); safecall;
    function Get_BevelInner: TxBevelCut; safecall;
    procedure Set_BevelInner(Value: TxBevelCut); safecall;
    function Get_BevelKind: TxBevelKind; safecall;
    procedure Set_BevelKind(Value: TxBevelKind); safecall;
    function Get_BevelOuter: TxBevelCut; safecall;
    procedure Set_BevelOuter(Value: TxBevelCut); safecall;
    function Get_BevelWidth: Integer; safecall;
    procedure Set_BevelWidth(Value: Integer); safecall;
    function Get_BorderWidth: Integer; safecall;
    procedure Set_BorderWidth(Value: Integer); safecall;
    function Get_BorderStyle: TxBorderStyle; safecall;
    procedure Set_BorderStyle(Value: TxBorderStyle); safecall;
    function Get_Caption: WideString; safecall;
    procedure Set_Caption(const Value: WideString); safecall;
    function Get_Color: OLE_COLOR; safecall;
    procedure Set_Color(Value: OLE_COLOR); safecall;
    function Get_Ctl3D: WordBool; safecall;
    procedure Set_Ctl3D(Value: WordBool); safecall;
    function Get_UseDockManager: WordBool; safecall;
    procedure Set_UseDockManager(Value: WordBool); safecall;
    function Get_DockSite: WordBool; safecall;
    procedure Set_DockSite(Value: WordBool); safecall;
    function Get_DoubleBuffered: WordBool; safecall;
    procedure Set_DoubleBuffered(Value: WordBool); safecall;
    function Get_DragCursor: Smallint; safecall;
    procedure Set_DragCursor(Value: Smallint); safecall;
    function Get_DragMode: TxDragMode; safecall;
    procedure Set_DragMode(Value: TxDragMode); safecall;
    function Get_Enabled: WordBool; safecall;
    procedure Set_Enabled(Value: WordBool); safecall;
    function Get_FullRepaint: WordBool; safecall;
    procedure Set_FullRepaint(Value: WordBool); safecall;
    function Get_Font: IFontDisp; safecall;
    procedure Set_Font(const Value: IFontDisp); safecall;
    procedure _Set_Font(var Value: IFontDisp); safecall;
    function Get_Locked: WordBool; safecall;
    procedure Set_Locked(Value: WordBool); safecall;
    function Get_ParentBackground: WordBool; safecall;
    procedure Set_ParentBackground(Value: WordBool); safecall;
    function Get_ParentColor: WordBool; safecall;
    procedure Set_ParentColor(Value: WordBool); safecall;
    function Get_ParentCtl3D: WordBool; safecall;
    procedure Set_ParentCtl3D(Value: WordBool); safecall;
    function Get_ParentDoubleBuffered: WordBool; safecall;
    procedure Set_ParentDoubleBuffered(Value: WordBool); safecall;
    function Get_ShowCaption: WordBool; safecall;
    procedure Set_ShowCaption(Value: WordBool); safecall;
    function Get_VerticalAlignment: TxVerticalAlignment; safecall;
    procedure Set_VerticalAlignment(Value: TxVerticalAlignment); safecall;
    function Get_Visible: WordBool; safecall;
    procedure Set_Visible(Value: WordBool); safecall;
    function Get_AlignDisabled: WordBool; safecall;
    function Get_MouseInClient: WordBool; safecall;
    function Get_VisibleDockClientCount: Integer; safecall;
    function DrawTextBiDiModeFlagsReadingOnly: Integer; safecall;
    function IsRightToLeft: WordBool; safecall;
    function UseRightToLeftReading: WordBool; safecall;
    function UseRightToLeftScrollBar: WordBool; safecall;
    function Get_ExplicitLeft: Integer; safecall;
    function Get_ExplicitTop: Integer; safecall;
    function Get_ExplicitWidth: Integer; safecall;
    function Get_ExplicitHeight: Integer; safecall;
    function Get_AlignWithMargins: WordBool; safecall;
    procedure Set_AlignWithMargins(Value: WordBool); safecall;
    function Get_ParentCustomHint: WordBool; safecall;
    procedure Set_ParentCustomHint(Value: WordBool); safecall;
    procedure SetSubComponent(IsSubComponent: WordBool); safecall;
    function QualifiedClassName: WideString; safecall;
    function UnitScope: WideString; safecall;
    function LoadMap(AMapType: TMapProvider): WordBool; safecall;
    procedure CenterMapOnCoords(ALatitude: Double; ALongitude: Double); safecall;
    procedure GetMapCenterCoords(var RLatitude: Double; var RLongitude: Double); safecall;
    procedure AddLayer(const ALayer: ILayer); safecall;
    function Get_Zoom: Integer; safecall;
    procedure Set_Zoom(ARZoom: Integer); safecall;
    function Get_MapMode: TMapMode; safecall;
    procedure Set_MapMode(ARMapMode: TMapMode); safecall;
    function Get_MapTypes: WideString; safecall;
    function Get_MapProvider: TMapProvider; safecall;
    procedure Set_HintFontSize(Param1: Integer); safecall;
    procedure ChangeMapType(const AMapType: WideString); safecall;
    procedure FitOnMap(ALatitudeMin: Double; ALongitudeMin: Double; ALatitudeMax: Double; 
                       ALongitudeMax: Double); safecall;
    procedure SaveMapBMP(const APath: WideString); safecall;
    procedure SetTraffic(ATraffic: WordBool); safecall;
    property Alignment: TxAlignment read Get_Alignment write Set_Alignment;
    property AutoSize: WordBool read Get_AutoSize write Set_AutoSize;
    property BevelInner: TxBevelCut read Get_BevelInner write Set_BevelInner;
    property BevelKind: TxBevelKind read Get_BevelKind write Set_BevelKind;
    property BevelOuter: TxBevelCut read Get_BevelOuter write Set_BevelOuter;
    property BevelWidth: Integer read Get_BevelWidth write Set_BevelWidth;
    property BorderWidth: Integer read Get_BorderWidth write Set_BorderWidth;
    property BorderStyle: TxBorderStyle read Get_BorderStyle write Set_BorderStyle;
    property Caption: WideString read Get_Caption write Set_Caption;
    property Color: OLE_COLOR read Get_Color write Set_Color;
    property Ctl3D: WordBool read Get_Ctl3D write Set_Ctl3D;
    property UseDockManager: WordBool read Get_UseDockManager write Set_UseDockManager;
    property DockSite: WordBool read Get_DockSite write Set_DockSite;
    property DoubleBuffered: WordBool read Get_DoubleBuffered write Set_DoubleBuffered;
    property DragCursor: Smallint read Get_DragCursor write Set_DragCursor;
    property DragMode: TxDragMode read Get_DragMode write Set_DragMode;
    property Enabled: WordBool read Get_Enabled write Set_Enabled;
    property FullRepaint: WordBool read Get_FullRepaint write Set_FullRepaint;
    property Font: IFontDisp read Get_Font write Set_Font;
    property Locked: WordBool read Get_Locked write Set_Locked;
    property ParentBackground: WordBool read Get_ParentBackground write Set_ParentBackground;
    property ParentColor: WordBool read Get_ParentColor write Set_ParentColor;
    property ParentCtl3D: WordBool read Get_ParentCtl3D write Set_ParentCtl3D;
    property ParentDoubleBuffered: WordBool read Get_ParentDoubleBuffered write Set_ParentDoubleBuffered;
    property ShowCaption: WordBool read Get_ShowCaption write Set_ShowCaption;
    property VerticalAlignment: TxVerticalAlignment read Get_VerticalAlignment write Set_VerticalAlignment;
    property Visible: WordBool read Get_Visible write Set_Visible;
    property AlignDisabled: WordBool read Get_AlignDisabled;
    property MouseInClient: WordBool read Get_MouseInClient;
    property VisibleDockClientCount: Integer read Get_VisibleDockClientCount;
    property ExplicitLeft: Integer read Get_ExplicitLeft;
    property ExplicitTop: Integer read Get_ExplicitTop;
    property ExplicitWidth: Integer read Get_ExplicitWidth;
    property ExplicitHeight: Integer read Get_ExplicitHeight;
    property AlignWithMargins: WordBool read Get_AlignWithMargins write Set_AlignWithMargins;
    property ParentCustomHint: WordBool read Get_ParentCustomHint write Set_ParentCustomHint;
    property Zoom: Integer read Get_Zoom write Set_Zoom;
    property MapMode: TMapMode read Get_MapMode write Set_MapMode;
    property MapTypes: WideString read Get_MapTypes;
    property MapProvider: TMapProvider read Get_MapProvider;
    property HintFontSize: Integer write Set_HintFontSize;
  end;

// *********************************************************************//
// DispIntf:  IILSMapExDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {04C663DF-43A3-47BF-915C-40CA48DF0C72}
// *********************************************************************//
  IILSMapExDisp = dispinterface
    ['{04C663DF-43A3-47BF-915C-40CA48DF0C72}']
    property Alignment: TxAlignment dispid 201;
    property AutoSize: WordBool dispid 202;
    property BevelInner: TxBevelCut dispid 203;
    property BevelKind: TxBevelKind dispid 204;
    property BevelOuter: TxBevelCut dispid 205;
    property BevelWidth: Integer dispid 206;
    property BorderWidth: Integer dispid 207;
    property BorderStyle: TxBorderStyle dispid 208;
    property Caption: WideString dispid -518;
    property Color: OLE_COLOR dispid -501;
    property Ctl3D: WordBool dispid 209;
    property UseDockManager: WordBool dispid 210;
    property DockSite: WordBool dispid 211;
    property DoubleBuffered: WordBool dispid 212;
    property DragCursor: Smallint dispid 213;
    property DragMode: TxDragMode dispid 214;
    property Enabled: WordBool dispid -514;
    property FullRepaint: WordBool dispid 215;
    property Font: IFontDisp dispid -512;
    property Locked: WordBool dispid 216;
    property ParentBackground: WordBool dispid 217;
    property ParentColor: WordBool dispid 218;
    property ParentCtl3D: WordBool dispid 219;
    property ParentDoubleBuffered: WordBool dispid 220;
    property ShowCaption: WordBool dispid 221;
    property VerticalAlignment: TxVerticalAlignment dispid 222;
    property Visible: WordBool dispid 223;
    property AlignDisabled: WordBool readonly dispid 225;
    property MouseInClient: WordBool readonly dispid 226;
    property VisibleDockClientCount: Integer readonly dispid 227;
    function DrawTextBiDiModeFlagsReadingOnly: Integer; dispid 228;
    function IsRightToLeft: WordBool; dispid 230;
    function UseRightToLeftReading: WordBool; dispid 231;
    function UseRightToLeftScrollBar: WordBool; dispid 232;
    property ExplicitLeft: Integer readonly dispid 233;
    property ExplicitTop: Integer readonly dispid 234;
    property ExplicitWidth: Integer readonly dispid 235;
    property ExplicitHeight: Integer readonly dispid 236;
    property AlignWithMargins: WordBool dispid 237;
    property ParentCustomHint: WordBool dispid 238;
    procedure SetSubComponent(IsSubComponent: WordBool); dispid 239;
    function QualifiedClassName: WideString; dispid 240;
    function UnitScope: WideString; dispid 241;
    function LoadMap(AMapType: TMapProvider): WordBool; dispid 301;
    procedure CenterMapOnCoords(ALatitude: Double; ALongitude: Double); dispid 302;
    procedure GetMapCenterCoords(var RLatitude: Double; var RLongitude: Double); dispid 303;
    procedure AddLayer(const ALayer: ILayer); dispid 304;
    property Zoom: Integer dispid 305;
    property MapMode: TMapMode dispid 306;
    property MapTypes: WideString readonly dispid 307;
    property MapProvider: TMapProvider readonly dispid 308;
    property HintFontSize: Integer writeonly dispid 309;
    procedure ChangeMapType(const AMapType: WideString); dispid 310;
    procedure FitOnMap(ALatitudeMin: Double; ALongitudeMin: Double; ALatitudeMax: Double; 
                       ALongitudeMax: Double); dispid 311;
    procedure SaveMapBMP(const APath: WideString); dispid 312;
    procedure SetTraffic(ATraffic: WordBool); dispid 313;
  end;

// *********************************************************************//
// DispIntf:  IILSMapExEvents
// Flags:     (4096) Dispatchable
// GUID:      {D27F75DF-B3C1-4B93-8086-B75E94E4B38F}
// *********************************************************************//
  IILSMapExEvents = dispinterface
    ['{D27F75DF-B3C1-4B93-8086-B75E94E4B38F}']
    procedure OnMClick(ALatitude: Double; ALongitude: Double; AX: Integer; AY: Integer; 
                       AKeyState: TKeybState); dispid 301;
    procedure OnMClickRight(ALatitude: Double; ALongitude: Double; AX: Integer; AY: Integer; 
                            AKeyState: TKeybState); dispid 303;
    procedure OnMEnter; dispid 305;
    procedure OnMLeave; dispid 306;
    procedure OnMMove(ALatitude: Double; ALongitude: Double); dispid 307;
    procedure OnMeasuringDistance(ADistance: Double); dispid 308;
    procedure OnZoom(ANewZoom: Integer); dispid 309;
  end;

// *********************************************************************//
// Interface: ILayer
// Flags:     (4432) Hidden Dual OleAutomation Dispatchable
// GUID:      {FEED1057-9B61-44E3-8410-FD479824C867}
// *********************************************************************//
  ILayer = interface(IDispatch)
    ['{FEED1057-9B61-44E3-8410-FD479824C867}']
  end;

// *********************************************************************//
// DispIntf:  ILayerDisp
// Flags:     (4432) Hidden Dual OleAutomation Dispatchable
// GUID:      {FEED1057-9B61-44E3-8410-FD479824C867}
// *********************************************************************//
  ILayerDisp = dispinterface
    ['{FEED1057-9B61-44E3-8410-FD479824C867}']
  end;

// *********************************************************************//
// Interface: ILayerLine
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {87617272-2948-4509-A670-E71FAC966D3C}
// *********************************************************************//
  ILayerLine = interface(ILayer)
    ['{87617272-2948-4509-A670-E71FAC966D3C}']
    procedure Set_Visible(Param1: WordBool); safecall;
    procedure Set_Color(Param1: OLE_COLOR); safecall;
    procedure AddPoint(APos: Integer; ALatitude: Double; ALongitude: Double); safecall;
    procedure ClearAllPoints; safecall;
    procedure ChainOn; safecall;
    procedure ChainOff; safecall;
    procedure SetCount(ACount: Integer); safecall;
    procedure ByCoords(AX: Integer; AY: Integer; out RPos: Integer); safecall;
    property Visible: WordBool write Set_Visible;
    property Color: OLE_COLOR write Set_Color;
  end;

// *********************************************************************//
// DispIntf:  ILayerLineDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {87617272-2948-4509-A670-E71FAC966D3C}
// *********************************************************************//
  ILayerLineDisp = dispinterface
    ['{87617272-2948-4509-A670-E71FAC966D3C}']
    property Visible: WordBool writeonly dispid 501;
    property Color: OLE_COLOR writeonly dispid 502;
    procedure AddPoint(APos: Integer; ALatitude: Double; ALongitude: Double); dispid 303;
    procedure ClearAllPoints; dispid 304;
    procedure ChainOn; dispid 305;
    procedure ChainOff; dispid 306;
    procedure SetCount(ACount: Integer); dispid 308;
    procedure ByCoords(AX: Integer; AY: Integer; out RPos: Integer); dispid 309;
  end;

// *********************************************************************//
// Interface: ILayerContour
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {2DE64A6B-DAB2-4F4E-940B-C4CBB504182D}
// *********************************************************************//
  ILayerContour = interface(ILayer)
    ['{2DE64A6B-DAB2-4F4E-940B-C4CBB504182D}']
    procedure Set_Visible(Param1: WordBool); safecall;
    procedure AddZone(AContourPos: Integer); safecall;
    procedure AddPoint(AContourPos: Integer; APos: Integer; ALatitude: Double; ALongitude: Double); safecall;
    procedure ClearAllPoints; safecall;
    procedure ChainOn; safecall;
    procedure ChainOff; safecall;
    procedure ByCoords(AX: Integer; AY: Integer; out RContourPos: Integer); safecall;
    procedure ReGetCoords(AContourPos: Integer; out RData: WideString); safecall;
    procedure SetNFillColor(AContourPos: Integer; AColor: OLE_COLOR); safecall;
    procedure SetNVisible(AContourPos: Integer; AVisible: WordBool); safecall;
    procedure EditBegin(AContourPos: Integer); safecall;
    procedure EditCancel(AContourPos: Integer); safecall;
    procedure EditCommit(AContourPos: Integer); safecall;
    property Visible: WordBool write Set_Visible;
  end;

// *********************************************************************//
// DispIntf:  ILayerContourDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {2DE64A6B-DAB2-4F4E-940B-C4CBB504182D}
// *********************************************************************//
  ILayerContourDisp = dispinterface
    ['{2DE64A6B-DAB2-4F4E-940B-C4CBB504182D}']
    property Visible: WordBool writeonly dispid 501;
    procedure AddZone(AContourPos: Integer); dispid 301;
    procedure AddPoint(AContourPos: Integer; APos: Integer; ALatitude: Double; ALongitude: Double); dispid 303;
    procedure ClearAllPoints; dispid 304;
    procedure ChainOn; dispid 305;
    procedure ChainOff; dispid 306;
    procedure ByCoords(AX: Integer; AY: Integer; out RContourPos: Integer); dispid 309;
    procedure ReGetCoords(AContourPos: Integer; out RData: WideString); dispid 310;
    procedure SetNFillColor(AContourPos: Integer; AColor: OLE_COLOR); dispid 311;
    procedure SetNVisible(AContourPos: Integer; AVisible: WordBool); dispid 312;
    procedure EditBegin(AContourPos: Integer); dispid 313;
    procedure EditCancel(AContourPos: Integer); dispid 314;
    procedure EditCommit(AContourPos: Integer); dispid 315;
  end;

// *********************************************************************//
// Interface: ILayerBitmap
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {F9163EA4-A8F7-4992-A477-B60E0ABC4E14}
// *********************************************************************//
  ILayerBitmap = interface(ILayer)
    ['{F9163EA4-A8F7-4992-A477-B60E0ABC4E14}']
    procedure Set_Visible(Param1: WordBool); safecall;
    procedure AddPoint(APos: Integer; ALatitude: Double; ALongitude: Double; AAngle: Single; 
                       AScale: Single; const AHint: WideString; const APicture: IObgBitmap); safecall;
    procedure ClearAllPoints; safecall;
    procedure ChainOn; safecall;
    procedure ChainOff; safecall;
    procedure SetCount(ACount: Integer); safecall;
    procedure ByCoords(AX: Integer; AY: Integer; out RNum: Integer); safecall;
    procedure ReGetCoords(APos: Integer; out RLatitude: Double; out RLongitude: Double); safecall;
    procedure SetNVisible(APoz: Integer; AVisible: WordBool); safecall;
    procedure EditBegin(APointID: Integer); safecall;
    procedure EditCancel(APointID: Integer); safecall;
    procedure EditCommit(APointID: Integer); safecall;
    property Visible: WordBool write Set_Visible;
  end;

// *********************************************************************//
// DispIntf:  ILayerBitmapDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {F9163EA4-A8F7-4992-A477-B60E0ABC4E14}
// *********************************************************************//
  ILayerBitmapDisp = dispinterface
    ['{F9163EA4-A8F7-4992-A477-B60E0ABC4E14}']
    property Visible: WordBool writeonly dispid 501;
    procedure AddPoint(APos: Integer; ALatitude: Double; ALongitude: Double; AAngle: Single; 
                       AScale: Single; const AHint: WideString; const APicture: IObgBitmap); dispid 303;
    procedure ClearAllPoints; dispid 304;
    procedure ChainOn; dispid 305;
    procedure ChainOff; dispid 306;
    procedure SetCount(ACount: Integer); dispid 308;
    procedure ByCoords(AX: Integer; AY: Integer; out RNum: Integer); dispid 309;
    procedure ReGetCoords(APos: Integer; out RLatitude: Double; out RLongitude: Double); dispid 310;
    procedure SetNVisible(APoz: Integer; AVisible: WordBool); dispid 312;
    procedure EditBegin(APointID: Integer); dispid 313;
    procedure EditCancel(APointID: Integer); dispid 314;
    procedure EditCommit(APointID: Integer); dispid 315;
  end;

// *********************************************************************//
// Interface: ILayerText
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {3FF8BA94-887E-40C9-86DF-AF4F2C792FFE}
// *********************************************************************//
  ILayerText = interface(ILayer)
    ['{3FF8BA94-887E-40C9-86DF-AF4F2C792FFE}']
    procedure Set_Visible(Param1: WordBool); safecall;
    procedure Set_FillColor(Param1: OLE_COLOR); safecall;
    procedure Set_TextColor(Param1: OLE_COLOR); safecall;
    procedure Set_TextSize(Param1: Integer); safecall;
    procedure AddPoint(ALatitude: Double; ALongitude: Double; const AText: WideString); safecall;
    procedure ClearAllPoints; safecall;
    procedure ChainOn; safecall;
    procedure ChainOff; safecall;
    procedure SetOffset(AOffX: Integer; AOffY: Integer); safecall;
    property Visible: WordBool write Set_Visible;
    property FillColor: OLE_COLOR write Set_FillColor;
    property TextColor: OLE_COLOR write Set_TextColor;
    property TextSize: Integer write Set_TextSize;
  end;

// *********************************************************************//
// DispIntf:  ILayerTextDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {3FF8BA94-887E-40C9-86DF-AF4F2C792FFE}
// *********************************************************************//
  ILayerTextDisp = dispinterface
    ['{3FF8BA94-887E-40C9-86DF-AF4F2C792FFE}']
    property Visible: WordBool writeonly dispid 501;
    property FillColor: OLE_COLOR writeonly dispid 503;
    property TextColor: OLE_COLOR writeonly dispid 504;
    property TextSize: Integer writeonly dispid 505;
    procedure AddPoint(ALatitude: Double; ALongitude: Double; const AText: WideString); dispid 303;
    procedure ClearAllPoints; dispid 304;
    procedure ChainOn; dispid 305;
    procedure ChainOff; dispid 306;
    procedure SetOffset(AOffX: Integer; AOffY: Integer); dispid 316;
  end;

// *********************************************************************//
// Interface: IObgBitmap
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {B91033AC-2CC9-43BF-A4B4-0D481584ADEE}
// *********************************************************************//
  IObgBitmap = interface(IDispatch)
    ['{B91033AC-2CC9-43BF-A4B4-0D481584ADEE}']
    procedure Set_FillColor(Param1: OLE_COLOR); safecall;
    procedure LoadPictureFromFile(const AFilePath: WideString; AFirstTransparent: WordBool; 
                                  AColorizeBlack: WordBool); safecall;
    property FillColor: OLE_COLOR write Set_FillColor;
  end;

// *********************************************************************//
// DispIntf:  IObgBitmapDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {B91033AC-2CC9-43BF-A4B4-0D481584ADEE}
// *********************************************************************//
  IObgBitmapDisp = dispinterface
    ['{B91033AC-2CC9-43BF-A4B4-0D481584ADEE}']
    property FillColor: OLE_COLOR writeonly dispid 503;
    procedure LoadPictureFromFile(const AFilePath: WideString; AFirstTransparent: WordBool; 
                                  AColorizeBlack: WordBool); dispid 307;
  end;


// *********************************************************************//
// OLE Control Proxy class declaration
// Control Name     : TILSMapEx
// Help String      : ILSMapEx Control
// Default Interface: IILSMapEx
// Def. Intf. DISP? : No
// Event   Interface: IILSMapExEvents
// TypeFlags        : (34) CanCreate Control
// *********************************************************************//
  TILSMapExOnMClick = procedure(ASender: TObject; ALatitude: Double; ALongitude: Double; 
                                                  AX: Integer; AY: Integer; AKeyState: TKeybState) of object;
  TILSMapExOnMClickRight = procedure(ASender: TObject; ALatitude: Double; ALongitude: Double; 
                                                       AX: Integer; AY: Integer; 
                                                       AKeyState: TKeybState) of object;
  TILSMapExOnMMove = procedure(ASender: TObject; ALatitude: Double; ALongitude: Double) of object;
  TILSMapExOnMeasuringDistance = procedure(ASender: TObject; ADistance: Double) of object;
  TILSMapExOnZoom = procedure(ASender: TObject; ANewZoom: Integer) of object;

  TILSMapEx = class(TOleControl)
  private
    FOnMClick: TILSMapExOnMClick;
    FOnMClickRight: TILSMapExOnMClickRight;
    FOnMEnter: TNotifyEvent;
    FOnMLeave: TNotifyEvent;
    FOnMMove: TILSMapExOnMMove;
    FOnMeasuringDistance: TILSMapExOnMeasuringDistance;
    FOnZoom: TILSMapExOnZoom;
    FIntf: IILSMapEx;
    function  GetControlInterface: IILSMapEx;
  protected
    procedure CreateControl;
    procedure InitControlData; override;
  public
    function DrawTextBiDiModeFlagsReadingOnly: Integer;
    function IsRightToLeft: WordBool;
    function UseRightToLeftReading: WordBool;
    function UseRightToLeftScrollBar: WordBool;
    procedure SetSubComponent(IsSubComponent: WordBool);
    function QualifiedClassName: WideString;
    function UnitScope: WideString;
    function LoadMap(AMapType: TMapProvider): WordBool;
    procedure CenterMapOnCoords(ALatitude: Double; ALongitude: Double);
    procedure GetMapCenterCoords(var RLatitude: Double; var RLongitude: Double);
    procedure AddLayer(const ALayer: ILayer);
    procedure ChangeMapType(const AMapType: WideString);
    procedure FitOnMap(ALatitudeMin: Double; ALongitudeMin: Double; ALatitudeMax: Double; 
                       ALongitudeMax: Double);
    procedure SaveMapBMP(const APath: WideString);
    procedure SetTraffic(ATraffic: WordBool);
    property  ControlInterface: IILSMapEx read GetControlInterface;
    property  DefaultInterface: IILSMapEx read GetControlInterface;
    property AlignDisabled: WordBool index 225 read GetWordBoolProp;
    property MouseInClient: WordBool index 226 read GetWordBoolProp;
    property VisibleDockClientCount: Integer index 227 read GetIntegerProp;
    property ExplicitLeft: Integer index 233 read GetIntegerProp;
    property ExplicitTop: Integer index 234 read GetIntegerProp;
    property ExplicitWidth: Integer index 235 read GetIntegerProp;
    property ExplicitHeight: Integer index 236 read GetIntegerProp;
    property MapTypes: WideString index 307 read GetWideStringProp;
    property MapProvider: TOleEnum index 308 read GetTOleEnumProp;
    property HintFontSize: Integer index 309 write SetIntegerProp;
  published
    property Anchors;
    property  ParentFont;
    property  TabStop;
    property  Align;
    property  ParentShowHint;
    property  PopupMenu;
    property  ShowHint;
    property  TabOrder;
    property  OnDragDrop;
    property  OnDragOver;
    property  OnEndDrag;
    property  OnEnter;
    property  OnExit;
    property  OnStartDrag;
    property Alignment: TOleEnum index 201 read GetTOleEnumProp write SetTOleEnumProp stored False;
    property AutoSize: WordBool index 202 read GetWordBoolProp write SetWordBoolProp stored False;
    property BevelInner: TOleEnum index 203 read GetTOleEnumProp write SetTOleEnumProp stored False;
    property BevelKind: TOleEnum index 204 read GetTOleEnumProp write SetTOleEnumProp stored False;
    property BevelOuter: TOleEnum index 205 read GetTOleEnumProp write SetTOleEnumProp stored False;
    property BevelWidth: Integer index 206 read GetIntegerProp write SetIntegerProp stored False;
    property BorderWidth: Integer index 207 read GetIntegerProp write SetIntegerProp stored False;
    property BorderStyle: TOleEnum index 208 read GetTOleEnumProp write SetTOleEnumProp stored False;
    property Caption: WideString index -518 read GetWideStringProp write SetWideStringProp stored False;
    property Color: TColor index -501 read GetTColorProp write SetTColorProp stored False;
    property Ctl3D: WordBool index 209 read GetWordBoolProp write SetWordBoolProp stored False;
    property UseDockManager: WordBool index 210 read GetWordBoolProp write SetWordBoolProp stored False;
    property DockSite: WordBool index 211 read GetWordBoolProp write SetWordBoolProp stored False;
    property DoubleBuffered: WordBool index 212 read GetWordBoolProp write SetWordBoolProp stored False;
    property DragCursor: Smallint index 213 read GetSmallintProp write SetSmallintProp stored False;
    property DragMode: TOleEnum index 214 read GetTOleEnumProp write SetTOleEnumProp stored False;
    property Enabled: WordBool index -514 read GetWordBoolProp write SetWordBoolProp stored False;
    property FullRepaint: WordBool index 215 read GetWordBoolProp write SetWordBoolProp stored False;
    property Font: TFont index -512 read GetTFontProp write SetTFontProp stored False;
    property Locked: WordBool index 216 read GetWordBoolProp write SetWordBoolProp stored False;
    property ParentBackground: WordBool index 217 read GetWordBoolProp write SetWordBoolProp stored False;
    property ParentColor: WordBool index 218 read GetWordBoolProp write SetWordBoolProp stored False;
    property ParentCtl3D: WordBool index 219 read GetWordBoolProp write SetWordBoolProp stored False;
    property ParentDoubleBuffered: WordBool index 220 read GetWordBoolProp write SetWordBoolProp stored False;
    property ShowCaption: WordBool index 221 read GetWordBoolProp write SetWordBoolProp stored False;
    property VerticalAlignment: TOleEnum index 222 read GetTOleEnumProp write SetTOleEnumProp stored False;
    property Visible: WordBool index 223 read GetWordBoolProp write SetWordBoolProp stored False;
    property AlignWithMargins: WordBool index 237 read GetWordBoolProp write SetWordBoolProp stored False;
    property ParentCustomHint: WordBool index 238 read GetWordBoolProp write SetWordBoolProp stored False;
    property Zoom: Integer index 305 read GetIntegerProp write SetIntegerProp stored False;
    property MapMode: TOleEnum index 306 read GetTOleEnumProp write SetTOleEnumProp stored False;
    property OnMClick: TILSMapExOnMClick read FOnMClick write FOnMClick;
    property OnMClickRight: TILSMapExOnMClickRight read FOnMClickRight write FOnMClickRight;
    property OnMEnter: TNotifyEvent read FOnMEnter write FOnMEnter;
    property OnMLeave: TNotifyEvent read FOnMLeave write FOnMLeave;
    property OnMMove: TILSMapExOnMMove read FOnMMove write FOnMMove;
    property OnMeasuringDistance: TILSMapExOnMeasuringDistance read FOnMeasuringDistance write FOnMeasuringDistance;
    property OnZoom: TILSMapExOnZoom read FOnZoom write FOnZoom;
  end;

// *********************************************************************//
// The Class CoLayerLine provides a Create and CreateRemote method to          
// create instances of the default interface ILayerLine exposed by              
// the CoClass LayerLine. The functions are intended to be used by             
// clients wishing to automate the CoClass objects exposed by the         
// server of this typelibrary.                                            
// *********************************************************************//
  CoLayerLine = class
    class function Create: ILayerLine;
    class function CreateRemote(const MachineName: string): ILayerLine;
  end;


// *********************************************************************//
// OLE Server Proxy class declaration
// Server Object    : TLayerLine
// Help String      : LayerLine Object
// Default Interface: ILayerLine
// Def. Intf. DISP? : No
// Event   Interface: 
// TypeFlags        : (2) CanCreate
// *********************************************************************//
  TLayerLine = class(TOleServer)
  private
    FIntf: ILayerLine;
    function GetDefaultInterface: ILayerLine;
  protected
    procedure InitServerData; override;
    procedure Set_Visible(Param1: WordBool);
    procedure Set_Color(Param1: OLE_COLOR);
  public
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;
    procedure Connect; override;
    procedure ConnectTo(svrIntf: ILayerLine);
    procedure Disconnect; override;
    procedure AddPoint(APos: Integer; ALatitude: Double; ALongitude: Double);
    procedure ClearAllPoints;
    procedure ChainOn;
    procedure ChainOff;
    procedure SetCount(ACount: Integer);
    procedure ByCoords(AX: Integer; AY: Integer; out RPos: Integer);
    property DefaultInterface: ILayerLine read GetDefaultInterface;
    property Visible: WordBool write Set_Visible;
    property Color: OLE_COLOR write Set_Color;
  published
  end;

// *********************************************************************//
// The Class CoLayerContour provides a Create and CreateRemote method to          
// create instances of the default interface ILayerContour exposed by              
// the CoClass LayerContour. The functions are intended to be used by             
// clients wishing to automate the CoClass objects exposed by the         
// server of this typelibrary.                                            
// *********************************************************************//
  CoLayerContour = class
    class function Create: ILayerContour;
    class function CreateRemote(const MachineName: string): ILayerContour;
  end;


// *********************************************************************//
// OLE Server Proxy class declaration
// Server Object    : TLayerContour
// Help String      : LayerContour Object
// Default Interface: ILayerContour
// Def. Intf. DISP? : No
// Event   Interface: 
// TypeFlags        : (2) CanCreate
// *********************************************************************//
  TLayerContour = class(TOleServer)
  private
    FIntf: ILayerContour;
    function GetDefaultInterface: ILayerContour;
  protected
    procedure InitServerData; override;
    procedure Set_Visible(Param1: WordBool);
  public
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;
    procedure Connect; override;
    procedure ConnectTo(svrIntf: ILayerContour);
    procedure Disconnect; override;
    procedure AddZone(AContourPos: Integer);
    procedure AddPoint(AContourPos: Integer; APos: Integer; ALatitude: Double; ALongitude: Double);
    procedure ClearAllPoints;
    procedure ChainOn;
    procedure ChainOff;
    procedure ByCoords(AX: Integer; AY: Integer; out RContourPos: Integer);
    procedure ReGetCoords(AContourPos: Integer; out RData: WideString);
    procedure SetNFillColor(AContourPos: Integer; AColor: OLE_COLOR);
    procedure SetNVisible(AContourPos: Integer; AVisible: WordBool);
    procedure EditBegin(AContourPos: Integer);
    procedure EditCancel(AContourPos: Integer);
    procedure EditCommit(AContourPos: Integer);
    property DefaultInterface: ILayerContour read GetDefaultInterface;
    property Visible: WordBool write Set_Visible;
  published
  end;

// *********************************************************************//
// The Class CoLayerBitmap provides a Create and CreateRemote method to          
// create instances of the default interface ILayerBitmap exposed by              
// the CoClass LayerBitmap. The functions are intended to be used by             
// clients wishing to automate the CoClass objects exposed by the         
// server of this typelibrary.                                            
// *********************************************************************//
  CoLayerBitmap = class
    class function Create: ILayerBitmap;
    class function CreateRemote(const MachineName: string): ILayerBitmap;
  end;


// *********************************************************************//
// OLE Server Proxy class declaration
// Server Object    : TLayerBitmap
// Help String      : LayerBitmap Object
// Default Interface: ILayerBitmap
// Def. Intf. DISP? : No
// Event   Interface: 
// TypeFlags        : (2) CanCreate
// *********************************************************************//
  TLayerBitmap = class(TOleServer)
  private
    FIntf: ILayerBitmap;
    function GetDefaultInterface: ILayerBitmap;
  protected
    procedure InitServerData; override;
    procedure Set_Visible(Param1: WordBool);
  public
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;
    procedure Connect; override;
    procedure ConnectTo(svrIntf: ILayerBitmap);
    procedure Disconnect; override;
    procedure AddPoint(APos: Integer; ALatitude: Double; ALongitude: Double; AAngle: Single; 
                       AScale: Single; const AHint: WideString; const APicture: IObgBitmap);
    procedure ClearAllPoints;
    procedure ChainOn;
    procedure ChainOff;
    procedure SetCount(ACount: Integer);
    procedure ByCoords(AX: Integer; AY: Integer; out RNum: Integer);
    procedure ReGetCoords(APos: Integer; out RLatitude: Double; out RLongitude: Double);
    procedure SetNVisible(APoz: Integer; AVisible: WordBool);
    procedure EditBegin(APointID: Integer);
    procedure EditCancel(APointID: Integer);
    procedure EditCommit(APointID: Integer);
    property DefaultInterface: ILayerBitmap read GetDefaultInterface;
    property Visible: WordBool write Set_Visible;
  published
  end;

// *********************************************************************//
// The Class CoLayerText provides a Create and CreateRemote method to          
// create instances of the default interface ILayerText exposed by              
// the CoClass LayerText. The functions are intended to be used by             
// clients wishing to automate the CoClass objects exposed by the         
// server of this typelibrary.                                            
// *********************************************************************//
  CoLayerText = class
    class function Create: ILayerText;
    class function CreateRemote(const MachineName: string): ILayerText;
  end;


// *********************************************************************//
// OLE Server Proxy class declaration
// Server Object    : TLayerText
// Help String      : LayerText Object
// Default Interface: ILayerText
// Def. Intf. DISP? : No
// Event   Interface: 
// TypeFlags        : (2) CanCreate
// *********************************************************************//
  TLayerText = class(TOleServer)
  private
    FIntf: ILayerText;
    function GetDefaultInterface: ILayerText;
  protected
    procedure InitServerData; override;
    procedure Set_Visible(Param1: WordBool);
    procedure Set_FillColor(Param1: OLE_COLOR);
    procedure Set_TextColor(Param1: OLE_COLOR);
    procedure Set_TextSize(Param1: Integer);
  public
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;
    procedure Connect; override;
    procedure ConnectTo(svrIntf: ILayerText);
    procedure Disconnect; override;
    procedure AddPoint(ALatitude: Double; ALongitude: Double; const AText: WideString);
    procedure ClearAllPoints;
    procedure ChainOn;
    procedure ChainOff;
    procedure SetOffset(AOffX: Integer; AOffY: Integer);
    property DefaultInterface: ILayerText read GetDefaultInterface;
    property Visible: WordBool write Set_Visible;
    property FillColor: OLE_COLOR write Set_FillColor;
    property TextColor: OLE_COLOR write Set_TextColor;
    property TextSize: Integer write Set_TextSize;
  published
  end;

// *********************************************************************//
// The Class CoObgBitmap provides a Create and CreateRemote method to          
// create instances of the default interface IObgBitmap exposed by              
// the CoClass ObgBitmap. The functions are intended to be used by             
// clients wishing to automate the CoClass objects exposed by the         
// server of this typelibrary.                                            
// *********************************************************************//
  CoObgBitmap = class
    class function Create: IObgBitmap;
    class function CreateRemote(const MachineName: string): IObgBitmap;
  end;


// *********************************************************************//
// OLE Server Proxy class declaration
// Server Object    : TObgBitmap
// Help String      : ObgBitmap Object
// Default Interface: IObgBitmap
// Def. Intf. DISP? : No
// Event   Interface: 
// TypeFlags        : (2) CanCreate
// *********************************************************************//
  TObgBitmap = class(TOleServer)
  private
    FIntf: IObgBitmap;
    function GetDefaultInterface: IObgBitmap;
  protected
    procedure InitServerData; override;
    procedure Set_FillColor(Param1: OLE_COLOR);
  public
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;
    procedure Connect; override;
    procedure ConnectTo(svrIntf: IObgBitmap);
    procedure Disconnect; override;
    procedure LoadPictureFromFile(const AFilePath: WideString; AFirstTransparent: WordBool; 
                                  AColorizeBlack: WordBool);
    property DefaultInterface: IObgBitmap read GetDefaultInterface;
    property FillColor: OLE_COLOR write Set_FillColor;
  published
  end;

procedure Register;

resourcestring
  dtlServerPage = 'ActiveX';

  dtlOcxPage = 'ActiveX';

implementation

uses System.Win.ComObj;

procedure TILSMapEx.InitControlData;
const
  CEventDispIDs: array [0..6] of DWORD = (
    $0000012D, $0000012F, $00000131, $00000132, $00000133, $00000134,
    $00000135);
  CTFontIDs: array [0..0] of DWORD = (
    $FFFFFE00);
  CControlData: TControlData2 = (
    ClassID:      '{F26AC771-4B1F-40F4-8089-D629E5DD9CE8}';
    EventIID:     '{D27F75DF-B3C1-4B93-8086-B75E94E4B38F}';
    EventCount:   7;
    EventDispIDs: @CEventDispIDs;
    LicenseKey:   nil (*HR:$00000000*);
    Flags:        $0000001D;
    Version:      500;
    FontCount:    1;
    FontIDs:      @CTFontIDs);
begin
  ControlData := @CControlData;
  TControlData2(CControlData).FirstEventOfs := UIntPtr(@@FOnMClick) - UIntPtr(Self);
end;

procedure TILSMapEx.CreateControl;

  procedure DoCreate;
  begin
    FIntf := IUnknown(OleObject) as IILSMapEx;
  end;

begin
  if FIntf = nil then DoCreate;
end;

function TILSMapEx.GetControlInterface: IILSMapEx;
begin
  CreateControl;
  Result := FIntf;
end;

function TILSMapEx.DrawTextBiDiModeFlagsReadingOnly: Integer;
begin
  Result := DefaultInterface.DrawTextBiDiModeFlagsReadingOnly;
end;

function TILSMapEx.IsRightToLeft: WordBool;
begin
  Result := DefaultInterface.IsRightToLeft;
end;

function TILSMapEx.UseRightToLeftReading: WordBool;
begin
  Result := DefaultInterface.UseRightToLeftReading;
end;

function TILSMapEx.UseRightToLeftScrollBar: WordBool;
begin
  Result := DefaultInterface.UseRightToLeftScrollBar;
end;

procedure TILSMapEx.SetSubComponent(IsSubComponent: WordBool);
begin
  DefaultInterface.SetSubComponent(IsSubComponent);
end;

function TILSMapEx.QualifiedClassName: WideString;
begin
  Result := DefaultInterface.QualifiedClassName;
end;

function TILSMapEx.UnitScope: WideString;
begin
  Result := DefaultInterface.UnitScope;
end;

function TILSMapEx.LoadMap(AMapType: TMapProvider): WordBool;
begin
  Result := DefaultInterface.LoadMap(AMapType);
end;

procedure TILSMapEx.CenterMapOnCoords(ALatitude: Double; ALongitude: Double);
begin
  DefaultInterface.CenterMapOnCoords(ALatitude, ALongitude);
end;

procedure TILSMapEx.GetMapCenterCoords(var RLatitude: Double; var RLongitude: Double);
begin
  DefaultInterface.GetMapCenterCoords(RLatitude, RLongitude);
end;

procedure TILSMapEx.AddLayer(const ALayer: ILayer);
begin
  DefaultInterface.AddLayer(ALayer);
end;

procedure TILSMapEx.ChangeMapType(const AMapType: WideString);
begin
  DefaultInterface.ChangeMapType(AMapType);
end;

procedure TILSMapEx.FitOnMap(ALatitudeMin: Double; ALongitudeMin: Double; ALatitudeMax: Double; 
                             ALongitudeMax: Double);
begin
  DefaultInterface.FitOnMap(ALatitudeMin, ALongitudeMin, ALatitudeMax, ALongitudeMax);
end;

procedure TILSMapEx.SaveMapBMP(const APath: WideString);
begin
  DefaultInterface.SaveMapBMP(APath);
end;

procedure TILSMapEx.SetTraffic(ATraffic: WordBool);
begin
  DefaultInterface.SetTraffic(ATraffic);
end;

class function CoLayerLine.Create: ILayerLine;
begin
  Result := CreateComObject(CLASS_LayerLine) as ILayerLine;
end;

class function CoLayerLine.CreateRemote(const MachineName: string): ILayerLine;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_LayerLine) as ILayerLine;
end;

procedure TLayerLine.InitServerData;
const
  CServerData: TServerData = (
    ClassID:   '{F72A8F28-5882-4F6A-9D6D-F824848C9C6B}';
    IntfIID:   '{87617272-2948-4509-A670-E71FAC966D3C}';
    EventIID:  '';
    LicenseKey: nil;
    Version: 500);
begin
  ServerData := @CServerData;
end;

procedure TLayerLine.Connect;
var
  punk: IUnknown;
begin
  if FIntf = nil then
  begin
    punk := GetServer;
    Fintf:= punk as ILayerLine;
  end;
end;

procedure TLayerLine.ConnectTo(svrIntf: ILayerLine);
begin
  Disconnect;
  FIntf := svrIntf;
end;

procedure TLayerLine.DisConnect;
begin
  if Fintf <> nil then
  begin
    FIntf := nil;
  end;
end;

function TLayerLine.GetDefaultInterface: ILayerLine;
begin
  if FIntf = nil then
    Connect;
  Assert(FIntf <> nil, 'DefaultInterface is NULL. Component is not connected to Server. You must call "Connect" or "ConnectTo" before this operation');
  Result := FIntf;
end;

constructor TLayerLine.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
end;

destructor TLayerLine.Destroy;
begin
  inherited Destroy;
end;

procedure TLayerLine.Set_Visible(Param1: WordBool);
begin
  DefaultInterface.Visible := Param1;
end;

procedure TLayerLine.Set_Color(Param1: OLE_COLOR);
begin
  DefaultInterface.Color := Param1;
end;

procedure TLayerLine.AddPoint(APos: Integer; ALatitude: Double; ALongitude: Double);
begin
  DefaultInterface.AddPoint(APos, ALatitude, ALongitude);
end;

procedure TLayerLine.ClearAllPoints;
begin
  DefaultInterface.ClearAllPoints;
end;

procedure TLayerLine.ChainOn;
begin
  DefaultInterface.ChainOn;
end;

procedure TLayerLine.ChainOff;
begin
  DefaultInterface.ChainOff;
end;

procedure TLayerLine.SetCount(ACount: Integer);
begin
  DefaultInterface.SetCount(ACount);
end;

procedure TLayerLine.ByCoords(AX: Integer; AY: Integer; out RPos: Integer);
begin
  DefaultInterface.ByCoords(AX, AY, RPos);
end;

class function CoLayerContour.Create: ILayerContour;
begin
  Result := CreateComObject(CLASS_LayerContour) as ILayerContour;
end;

class function CoLayerContour.CreateRemote(const MachineName: string): ILayerContour;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_LayerContour) as ILayerContour;
end;

procedure TLayerContour.InitServerData;
const
  CServerData: TServerData = (
    ClassID:   '{94D694D2-CF1A-4222-A118-7B6F0CBF4B98}';
    IntfIID:   '{2DE64A6B-DAB2-4F4E-940B-C4CBB504182D}';
    EventIID:  '';
    LicenseKey: nil;
    Version: 500);
begin
  ServerData := @CServerData;
end;

procedure TLayerContour.Connect;
var
  punk: IUnknown;
begin
  if FIntf = nil then
  begin
    punk := GetServer;
    Fintf:= punk as ILayerContour;
  end;
end;

procedure TLayerContour.ConnectTo(svrIntf: ILayerContour);
begin
  Disconnect;
  FIntf := svrIntf;
end;

procedure TLayerContour.DisConnect;
begin
  if Fintf <> nil then
  begin
    FIntf := nil;
  end;
end;

function TLayerContour.GetDefaultInterface: ILayerContour;
begin
  if FIntf = nil then
    Connect;
  Assert(FIntf <> nil, 'DefaultInterface is NULL. Component is not connected to Server. You must call "Connect" or "ConnectTo" before this operation');
  Result := FIntf;
end;

constructor TLayerContour.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
end;

destructor TLayerContour.Destroy;
begin
  inherited Destroy;
end;

procedure TLayerContour.Set_Visible(Param1: WordBool);
begin
  DefaultInterface.Visible := Param1;
end;

procedure TLayerContour.AddZone(AContourPos: Integer);
begin
  DefaultInterface.AddZone(AContourPos);
end;

procedure TLayerContour.AddPoint(AContourPos: Integer; APos: Integer; ALatitude: Double; 
                                 ALongitude: Double);
begin
  DefaultInterface.AddPoint(AContourPos, APos, ALatitude, ALongitude);
end;

procedure TLayerContour.ClearAllPoints;
begin
  DefaultInterface.ClearAllPoints;
end;

procedure TLayerContour.ChainOn;
begin
  DefaultInterface.ChainOn;
end;

procedure TLayerContour.ChainOff;
begin
  DefaultInterface.ChainOff;
end;

procedure TLayerContour.ByCoords(AX: Integer; AY: Integer; out RContourPos: Integer);
begin
  DefaultInterface.ByCoords(AX, AY, RContourPos);
end;

procedure TLayerContour.ReGetCoords(AContourPos: Integer; out RData: WideString);
begin
  DefaultInterface.ReGetCoords(AContourPos, RData);
end;

procedure TLayerContour.SetNFillColor(AContourPos: Integer; AColor: OLE_COLOR);
begin
  DefaultInterface.SetNFillColor(AContourPos, AColor);
end;

procedure TLayerContour.SetNVisible(AContourPos: Integer; AVisible: WordBool);
begin
  DefaultInterface.SetNVisible(AContourPos, AVisible);
end;

procedure TLayerContour.EditBegin(AContourPos: Integer);
begin
  DefaultInterface.EditBegin(AContourPos);
end;

procedure TLayerContour.EditCancel(AContourPos: Integer);
begin
  DefaultInterface.EditCancel(AContourPos);
end;

procedure TLayerContour.EditCommit(AContourPos: Integer);
begin
  DefaultInterface.EditCommit(AContourPos);
end;

class function CoLayerBitmap.Create: ILayerBitmap;
begin
  Result := CreateComObject(CLASS_LayerBitmap) as ILayerBitmap;
end;

class function CoLayerBitmap.CreateRemote(const MachineName: string): ILayerBitmap;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_LayerBitmap) as ILayerBitmap;
end;

procedure TLayerBitmap.InitServerData;
const
  CServerData: TServerData = (
    ClassID:   '{AC5597CB-D25E-4A43-B37F-D2CC6583A9FB}';
    IntfIID:   '{F9163EA4-A8F7-4992-A477-B60E0ABC4E14}';
    EventIID:  '';
    LicenseKey: nil;
    Version: 500);
begin
  ServerData := @CServerData;
end;

procedure TLayerBitmap.Connect;
var
  punk: IUnknown;
begin
  if FIntf = nil then
  begin
    punk := GetServer;
    Fintf:= punk as ILayerBitmap;
  end;
end;

procedure TLayerBitmap.ConnectTo(svrIntf: ILayerBitmap);
begin
  Disconnect;
  FIntf := svrIntf;
end;

procedure TLayerBitmap.DisConnect;
begin
  if Fintf <> nil then
  begin
    FIntf := nil;
  end;
end;

function TLayerBitmap.GetDefaultInterface: ILayerBitmap;
begin
  if FIntf = nil then
    Connect;
  Assert(FIntf <> nil, 'DefaultInterface is NULL. Component is not connected to Server. You must call "Connect" or "ConnectTo" before this operation');
  Result := FIntf;
end;

constructor TLayerBitmap.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
end;

destructor TLayerBitmap.Destroy;
begin
  inherited Destroy;
end;

procedure TLayerBitmap.Set_Visible(Param1: WordBool);
begin
  DefaultInterface.Visible := Param1;
end;

procedure TLayerBitmap.AddPoint(APos: Integer; ALatitude: Double; ALongitude: Double; 
                                AAngle: Single; AScale: Single; const AHint: WideString; 
                                const APicture: IObgBitmap);
begin
  DefaultInterface.AddPoint(APos, ALatitude, ALongitude, AAngle, AScale, AHint, APicture);
end;

procedure TLayerBitmap.ClearAllPoints;
begin
  DefaultInterface.ClearAllPoints;
end;

procedure TLayerBitmap.ChainOn;
begin
  DefaultInterface.ChainOn;
end;

procedure TLayerBitmap.ChainOff;
begin
  DefaultInterface.ChainOff;
end;

procedure TLayerBitmap.SetCount(ACount: Integer);
begin
  DefaultInterface.SetCount(ACount);
end;

procedure TLayerBitmap.ByCoords(AX: Integer; AY: Integer; out RNum: Integer);
begin
  DefaultInterface.ByCoords(AX, AY, RNum);
end;

procedure TLayerBitmap.ReGetCoords(APos: Integer; out RLatitude: Double; out RLongitude: Double);
begin
  DefaultInterface.ReGetCoords(APos, RLatitude, RLongitude);
end;

procedure TLayerBitmap.SetNVisible(APoz: Integer; AVisible: WordBool);
begin
  DefaultInterface.SetNVisible(APoz, AVisible);
end;

procedure TLayerBitmap.EditBegin(APointID: Integer);
begin
  DefaultInterface.EditBegin(APointID);
end;

procedure TLayerBitmap.EditCancel(APointID: Integer);
begin
  DefaultInterface.EditCancel(APointID);
end;

procedure TLayerBitmap.EditCommit(APointID: Integer);
begin
  DefaultInterface.EditCommit(APointID);
end;

class function CoLayerText.Create: ILayerText;
begin
  Result := CreateComObject(CLASS_LayerText) as ILayerText;
end;

class function CoLayerText.CreateRemote(const MachineName: string): ILayerText;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_LayerText) as ILayerText;
end;

procedure TLayerText.InitServerData;
const
  CServerData: TServerData = (
    ClassID:   '{918A84C4-E54B-405F-B3F7-A9F2A5BB477B}';
    IntfIID:   '{3FF8BA94-887E-40C9-86DF-AF4F2C792FFE}';
    EventIID:  '';
    LicenseKey: nil;
    Version: 500);
begin
  ServerData := @CServerData;
end;

procedure TLayerText.Connect;
var
  punk: IUnknown;
begin
  if FIntf = nil then
  begin
    punk := GetServer;
    Fintf:= punk as ILayerText;
  end;
end;

procedure TLayerText.ConnectTo(svrIntf: ILayerText);
begin
  Disconnect;
  FIntf := svrIntf;
end;

procedure TLayerText.DisConnect;
begin
  if Fintf <> nil then
  begin
    FIntf := nil;
  end;
end;

function TLayerText.GetDefaultInterface: ILayerText;
begin
  if FIntf = nil then
    Connect;
  Assert(FIntf <> nil, 'DefaultInterface is NULL. Component is not connected to Server. You must call "Connect" or "ConnectTo" before this operation');
  Result := FIntf;
end;

constructor TLayerText.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
end;

destructor TLayerText.Destroy;
begin
  inherited Destroy;
end;

procedure TLayerText.Set_Visible(Param1: WordBool);
begin
  DefaultInterface.Visible := Param1;
end;

procedure TLayerText.Set_FillColor(Param1: OLE_COLOR);
begin
  DefaultInterface.FillColor := Param1;
end;

procedure TLayerText.Set_TextColor(Param1: OLE_COLOR);
begin
  DefaultInterface.TextColor := Param1;
end;

procedure TLayerText.Set_TextSize(Param1: Integer);
begin
  DefaultInterface.TextSize := Param1;
end;

procedure TLayerText.AddPoint(ALatitude: Double; ALongitude: Double; const AText: WideString);
begin
  DefaultInterface.AddPoint(ALatitude, ALongitude, AText);
end;

procedure TLayerText.ClearAllPoints;
begin
  DefaultInterface.ClearAllPoints;
end;

procedure TLayerText.ChainOn;
begin
  DefaultInterface.ChainOn;
end;

procedure TLayerText.ChainOff;
begin
  DefaultInterface.ChainOff;
end;

procedure TLayerText.SetOffset(AOffX: Integer; AOffY: Integer);
begin
  DefaultInterface.SetOffset(AOffX, AOffY);
end;

class function CoObgBitmap.Create: IObgBitmap;
begin
  Result := CreateComObject(CLASS_ObgBitmap) as IObgBitmap;
end;

class function CoObgBitmap.CreateRemote(const MachineName: string): IObgBitmap;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_ObgBitmap) as IObgBitmap;
end;

procedure TObgBitmap.InitServerData;
const
  CServerData: TServerData = (
    ClassID:   '{DA136049-BD56-4881-9AB3-641ED2D23861}';
    IntfIID:   '{B91033AC-2CC9-43BF-A4B4-0D481584ADEE}';
    EventIID:  '';
    LicenseKey: nil;
    Version: 500);
begin
  ServerData := @CServerData;
end;

procedure TObgBitmap.Connect;
var
  punk: IUnknown;
begin
  if FIntf = nil then
  begin
    punk := GetServer;
    Fintf:= punk as IObgBitmap;
  end;
end;

procedure TObgBitmap.ConnectTo(svrIntf: IObgBitmap);
begin
  Disconnect;
  FIntf := svrIntf;
end;

procedure TObgBitmap.DisConnect;
begin
  if Fintf <> nil then
  begin
    FIntf := nil;
  end;
end;

function TObgBitmap.GetDefaultInterface: IObgBitmap;
begin
  if FIntf = nil then
    Connect;
  Assert(FIntf <> nil, 'DefaultInterface is NULL. Component is not connected to Server. You must call "Connect" or "ConnectTo" before this operation');
  Result := FIntf;
end;

constructor TObgBitmap.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
end;

destructor TObgBitmap.Destroy;
begin
  inherited Destroy;
end;

procedure TObgBitmap.Set_FillColor(Param1: OLE_COLOR);
begin
  DefaultInterface.FillColor := Param1;
end;

procedure TObgBitmap.LoadPictureFromFile(const AFilePath: WideString; AFirstTransparent: WordBool; 
                                         AColorizeBlack: WordBool);
begin
  DefaultInterface.LoadPictureFromFile(AFilePath, AFirstTransparent, AColorizeBlack);
end;

procedure Register;
begin
  RegisterComponents(dtlOcxPage, [TILSMapEx]);
  RegisterComponents(dtlServerPage, [TLayerLine, TLayerContour, TLayerBitmap, TLayerText, 
    TObgBitmap]);
end;

end.
