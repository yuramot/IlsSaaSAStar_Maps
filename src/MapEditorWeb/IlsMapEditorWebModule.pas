unit IlsMapEditorWebModule;

interface


uses System.SysUtils, System.Classes, Web.HTTPApp, Ils.Logger, JsonDataObjects,
  Ils.Json.Names, Ils.Json.Utils, Geo.Hash, Geo.Pos, UMapEditor, WinApi.Windows,
  Web.HTTPProd, web.CGIHttp, Vcl.ExtCtrls, System.IniFiles, Ils.Redis.Conf,
  Redis.Client, uGeneral, System.DateUtils, uZC, uSign, AStar64.Extra,
  Geo.Hash.Search, System.Math, IdURI, MMSystem, clMMTimer, System.Types,
  IdHTTP, IdSSLOpenSSL;

type

  TCallbackThunk = class (TObject)
  private
    FCallAddress: Pointer;
    FProcPtr: Pointer;
    FSavedFlag: LongWord;
    FSelfPtr: Pointer;
    function GetCallAddress: Pointer;
  public
    constructor Create(pSelf, pProc: Pointer);
    destructor Destroy; override;
    procedure Clear;
    property CallAddress: Pointer read GetCallAddress;
  end;

  TwmMapEditor = class(TWebModule)
    procedure wmMapEditorDefaultHandlerAction(Sender: TObject;
      Request: TWebRequest; Response: TWebResponse; var Handled: Boolean);
    procedure WebModuleBeforeDispatch(Sender: TObject; Request: TWebRequest;
      Response: TWebResponse; var Handled: Boolean);
    procedure WebModuleAfterDispatch(Sender: TObject; Request: TWebRequest;
      Response: TWebResponse; var Handled: Boolean);
    procedure WebModuleDestroy(Sender: TObject);
    procedure WebModuleException(Sender: TObject; E: Exception;
      var Handled: Boolean);
    procedure WebModuleCreate(Sender: TObject);
  private
    FMMTimerBorderToRedis: TMMTimer;
    FMMTimerErrorsToRedis: TMMTimer;
    FRootPath: string;
    FRequestNo: Cardinal;
    procedure OnMMTimerProc(Sender: TObject);
    procedure OnMMTimerForRedisProc(Sender: TObject);
  public
  end;

var
  WebModuleClass: TComponentClass = TwmMapEditor;

implementation

{$R *.dfm}

type
  PThunk = ^TThunk;
  TThunk = packed record
    POPEDX: Byte;
    MOVEAX: Byte;
    SelfPtr: Pointer;
    PUSHEAX: Byte;
    PUSHEDX: Byte;
    JMP: Byte;
    JmpOffset: Integer;
  end;

{ ----------------------------------------------------------------------- }
constructor TCallbackThunk.Create(pSelf, pProc: Pointer);
begin
  FCallAddress := nil;
  FSelfPtr := pSelf;
  FProcPtr := pProc;
end;

{ ----------------------------------------------------------------------- }
destructor TCallbackThunk.Destroy;
begin
  Clear;
  inherited Destroy;
end;

{ ----------------------------------------------------------------------- }
procedure TCallbackThunk.Clear;
var
  SaveFlag: DWORD;
begin
  if @FCallAddress <> nil then begin
    VirtualProtect(PThunk(@FCallAddress), SizeOf(TThunk),
      FSavedFlag, @SaveFlag);
    VirtualFree(FCallAddress, 0, MEM_RELEASE);
    FCallAddress := nil;
    FSavedFlag := 0;
  end;
end;

{ ----------------------------------------------------------------------- }
function TCallbackThunk.GetCallAddress: Pointer;
begin
  if FCallAddress = nil then begin
    FCallAddress := VirtualAlloc(nil, SizeOf(TThunk), MEM_COMMIT, PAGE_READWRITE);
    with PThunk(FCallAddress)^ do begin
      POPEDX := $5A;
      MOVEAX := $B8;
      SelfPtr := FSelfPtr;
      PUSHEAX := $50;
      PUSHEDX := $52;
      JMP := $E9;
      JmpOffset := Integer(FProcPtr) - Integer(@JMP) - 5;
    end;
    if not VirtualProtect(FCallAddress, SizeOf(TThunk), PAGE_EXECUTE_READ,
       @FSavedFlag) then begin
      FCallAddress := nil;
      raise Exception.Create('Cannot get call address');
    end;
  end;
  Result := FCallAddress;
end;

procedure TwmMapEditor.OnMMTimerForRedisProc(Sender: TObject);
begin
  TGeneral.SendWatchdog;
end;

procedure TwmMapEditor.OnMMTimerProc(Sender: TObject);
var
  Account, iNum: Integer;
  Accounts: TIntegerDynArray;
  Border: TChangeBorder;
  DateTime: TDateTime;
  TopLeft, BottomRight: TGeoPos;
  FZC: TZC;
begin
  iNum := 0;
  try
    if not Assigned(TGeneral.FAccountChangeBorder) then
      Exit;
    iNum := 1;
    if TGeneral.FAccountChangeBorder.Count = 0 then
      Exit;
    iNum := 2;

    DateTime := Now();
    iNum := 3;


    for Account in TGeneral.FAccountChangeBorder.Keys do
    begin
      iNum := 4;
      if TGeneral.FRedisConf.Enabled
        and (Length(TGeneral.FBorderValues) > 0)
        and TGeneral.FRedisConnected then
        if TGeneral.FAccountsArch.TryGetValue(Account, Accounts) then
           TGeneral.SendAccountBorder(Accounts, TGeneral.FRedisCli);

      iNum := 5;
      if TGeneral.FAccountChangeBorder.TryGetValue(Account, Border) then
      begin
        iNum := 6;
        if (Border.LastChange <> 0) and
           (SecondsBetween(DateTime, Border.LastChange) >= TGeneral.FRedisPostPeriod) then
        begin
          TopLeft := Border.TopLeft;
          BottomRight := Border.BottomRight;
          ToLog('Запуск обновления по таймеру. FRedisPostPeriod='+IntToStr(TGeneral.FRedisPostPeriod)+
                ' SecondsBetween(DateTime, Border.LastChange)='+IntToStr(SecondsBetween(DateTime, Border.LastChange))+
                ' Border:['+TopLeft.ToString + ' - ' + BottomRight.ToString +']');

          if TGeneral.FAccountsArch.TryGetValue(Account, Accounts) then
          begin
            FZC := TZC.Create(FRequestNo, TGeneral.FDetailLog);
            try
              FZC.SaveFileListZC(FRootPath, TopLeft, BottomRight, Accounts);
              if TGeneral.FRedisConf.Enabled then
                TGeneral.SendAccountBorder(Accounts, TGeneral.FRedisCli);

//              else}
//              begin
                Border.LastChange := 0;
                Border.TopLeft.Latitude := 0;
                Border.TopLeft.Longitude := 180;
                Border.BottomRight.Latitude := 90;
                Border.BottomRight.Longitude := 0;
                TGeneral.FAccountChangeBorder.AddOrSetValue(Accounts[0], Border);
//              end;
            finally
              FZC.Free;
            end;
          end;
          Border.LastChange := 0;
          Border.TopLeft.Latitude := 0;
          Border.TopLeft.Longitude := 180;
          Border.BottomRight.Latitude := 90;
          Border.BottomRight.Longitude := 0;
          TGeneral.FAccountChangeBorder.AddOrSetValue(Account, Border);
        end;
      end;
    end;
  except on E:Exception do
    ToLog('TwmMapEditor.MMTimerProc iNum='+IntToStr(iNum)+ ' Assigned='+BoolToStr(Assigned(TGeneral.FAccountChangeBorder)) +
          ' ' +E.Message);
  end;
end;

procedure TwmMapEditor.WebModuleAfterDispatch(Sender: TObject;
  Request: TWebRequest; Response: TWebResponse; var Handled: Boolean);
begin
  {$IF RTLVersion < 20.00}
  if Pos('TEXT/HTML; CHARSET=UTF-8',
    AnsiUpperCase(Response.ContentType)) > 0 then
    Response.Content := AnsiToUtf8(Response.Content);
  {$IFEND}
end;

procedure TwmMapEditor.WebModuleBeforeDispatch(Sender: TObject;
  Request: TWebRequest; Response: TWebResponse; var Handled: Boolean);
begin
  Response.ContentType := 'TEXT/HTML; CHARSET=UTF-8';
end;

procedure TwmMapEditor.WebModuleCreate(Sender: TObject);
begin
  if not Assigned(TGeneral.FAccountChangeBorder) then
  begin
    FRequestNo := 0;
    ToLog('Request # '+ IntToStr(FRequestNo) + ' WebModuleCreate');
    FRootPath := IncludeTrailingPathDelimiter(ExtractFileDir(GetModuleName(HInstance)));
    if TGeneral.FRedisConf.Enabled then
    begin
      FMMTimerBorderToRedis := TMMTimer.Create;
      FMMTimerBorderToRedis.OnTimer := OnMMTimerProc;
      FMMTimerBorderToRedis.Interval := TGeneral.FTimerInterval;
      FMMTimerBorderToRedis.Enabled := True;
      FMMTimerErrorsToRedis := TMMTimer.Create;
      FMMTimerErrorsToRedis.OnTimer := OnMMTimerForRedisProc;
      FMMTimerErrorsToRedis.Interval := TGeneral.FTimerRedisInterval*1000;
      FMMTimerErrorsToRedis.Enabled := True;
    end;
  end;
end;

procedure TwmMapEditor.WebModuleDestroy(Sender: TObject);
begin
  if TGeneral.FRedisConf.Enabled then
  begin
    FMMTimerBorderToRedis.Enabled := False;
    FMMTimerErrorsToRedis.Enabled := False;
  end;
end;

procedure TwmMapEditor.WebModuleException(Sender: TObject; E: Exception;
  var Handled: Boolean);
begin
  TGeneral.IncWatchdog(COTHER_ERROR);
  TGeneral.SendWatchdog;
  ToLog('WebModuleException: '+E.Message);
end;

procedure TwmMapEditor.wmMapEditorDefaultHandlerAction(Sender: TObject;
  Request: TWebRequest; Response: TWebResponse; var Handled: Boolean);
var
  s: string;
  iT: Cardinal;
  FMapEditor: TMapEditor;
begin
  iT := GetTickCount;
  s := Request.Content;
  Inc(FRequestNo);
  Sleep(50);
  if Length(s) > 0 then
  begin
    if s[1] <> '{' then
      Response.Content := '{"result":"not JSON"}'
    else
    begin
      s := StringReplace(s,'%22','"',[rfReplaceAll]);
      ToLog('Request # '+ IntToStr(FRequestNo) +' с адреса ' + string(Request.RemoteAddr) +
            ' http://localhost:'+IntToStr(TGeneral.FPort)+'/?'+s);
      FMapEditor := TMapEditor.Create(TGeneral.FMinDistToLineM, FRequestNo, TGeneral.FRedisCli);
      try
        s := FMapEditor.ParseJson(s);
        Response.ContentType := 'text/plain; charset="UTF-8"';
        Response.ContentStream := TStringStream.Create(s, TEncoding.UTF8);
//        Response.Content := String(AnsiToUtf8(ParseJson(s, FMapEditor)));
//        Response.Content := AnsiToUtf8(FMapEditor.ParseJson(s));
//        Response.Content := s;
        ToLog('Request # '+ IntToStr(FRequestNo) +' All ticks='+IntToStr(GetTickCount-iT));
      finally
        FMapEditor.Free;
//        Response.ContentStream.Free;
      end;
    end;
  end;
end;

end.

