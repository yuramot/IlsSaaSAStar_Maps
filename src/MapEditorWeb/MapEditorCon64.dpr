program MapEditorCon64;

{$APPTYPE CONSOLE}

{$R *.res}

uses
  System.SysUtils,
  Web.WebReq,
  IdHTTPWebBrokerBridge,
  IdHTTP,
  IdSSLOpenSSL,
  IlsMapEditorWebModule in 'IlsMapEditorWebModule.pas' {getcover: TWebModule},
  AStar64.DynImport in '..\AStar\AStar64.DynImport.pas',
  UMapEditor in 'UMapEditor.pas',
  uZC in 'uZC.pas',
  uSign in 'uSign.pas',
  uGeneral in 'uGeneral.pas',
  uSpeed in 'uSpeed.pas',
  uLandMark in 'uLandMark.pas',
  LandmarkPathCalculator in '..\AStar\LandmarkPathCalculator.pas',
  Ils.Logger in '..\_Common\Ils.Logger.pas';

const
IMAGE_FILE_LARGE_ADDRESS_AWARE = $0020;
CConsoleVer = '7.2.32 2019.11.26';

{$SetPEFlags IMAGE_FILE_LARGE_ADDRESS_AWARE}

var
  port, i: Integer;
  ConfFile, RootPath: string;
  fs: TFormatSettings;
  FServer: TIdHTTPWebBrokerBridge;

procedure Log(S: string);
begin
  ToLog(S);
  Writeln(S);
end;

function GetHttpreqwestResult(strHttpReqwest: string): string;
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

procedure StartServer;
begin
  if not Assigned(FServer) then
    FServer := TIdHTTPWebBrokerBridge.Create(nil);

  if not FServer.Active then
  begin
    FServer.Bindings.Clear;
    FServer.DefaultPort := TGeneral.FPort;
    Log('Port:'+IntToStr(TGeneral.FPort));
    FServer.Active := True;
  end;
  if FServer.Active then
  begin
    Log('Web Server started');
    //первый запрос для инициализации переменных
    GetHttpreqwestResult('http://localhost:'+IntToStr(TGeneral.FPort)+'/?{"Type":"GetZonesProgress","Account":{"Current":"1","Parents":[]}}');
  end else
    Log('Web Server not started');

end;

procedure StopServer;
begin
  TGeneral.Stop;
  Sleep(1000);
  FServer.Active := False;
  FServer.Bindings.Clear;
  FServer.Free;
  Log('=============WebServer Stop=============');
end;

var
  bBreak: Boolean;
begin
  try
    RootPath := IncludeTrailingPathDelimiter(ExtractFileDir(GetModuleName(HInstance)));
    ConfFile := '';//ChangeFileExt(GetModuleName(HInstance), '.ini');
    port := 0;
    fs := TFormatSettings.Create;
    fs.DecimalSeparator := '.';

    for I := 1 to ParamCount do
    begin
      if (ParamStr(I) = '-config') and (ParamStr(I+1)<>'') then
        ConfFile := RootPath + ParamStr(I+1);
      if (ParamStr(I) = '-port') and (ParamStr(I+1)<>'') then
        port := StrToInt(ParamStr(I+1));
    end;
    if not FileExists(ConfFile) then
    begin
      //ConfFile := ChangeFileExt(GetModuleName(HInstance), '.ini');
      Writeln('Запуск программы обязателен с параметрами');
      Writeln('Пример MapEditorCon.exe -config config.ini [-port 7653]');
      Readln;
      Exit;
    end;
    TGeneral.Start(ConfFile);
    Writeln(ConfFile);
    if port <> 0 then
      TGeneral.FPort := port;
    Log('=============Start Server=============');
    Log('Console version '  + CConsoleVer);
    if WebRequestHandler <> nil then
      WebRequestHandler.WebModuleClass := WebModuleClass;

    StartServer;

    try
      while not bBreak do
        Sleep(1000);
    except
      StopServer;
      bBreak := True;
    end;
  except
    on E: Exception do
      Log(E.ClassName + ': ' + E.Message);
  end;
end.
