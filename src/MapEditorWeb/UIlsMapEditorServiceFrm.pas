unit UIlsMapEditorServiceFrm;

interface

uses Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, IdComponent,
  System.Classes, Vcl.Graphics, Vcl.Controls, Vcl.SvcMgr, Vcl.Dialogs, Ils.Logger,
  Vcl.AppEvnts, Vcl.StdCtrls, IdHTTPWebBrokerBridge, Web.HTTPApp, System.IniFiles,
  UMapEditor, Ils.Utils.Debug, Winapi.ActiveX, IdContext, IdException,
  IdHTTP, IdSSLOpenSSL,uGeneral, Ils.Utils.Svc;

type
  TILSSaaSMapEditorWebService = class(TService)
    procedure ServiceStart(Sender: TService; var Started: Boolean);
    procedure ServiceStop(Sender: TService; var Stopped: Boolean);
    procedure ServiceShutdown(Sender: TService);
    procedure ServiceBeforeInstall(Sender: TService);
    procedure ServiceAfterInstall(Sender: TService);
  private
    FServer: TIdHTTPWebBrokerBridge;
    procedure StartServer;
    procedure StopServer;
    procedure RestartServer;
    procedure IDServerException(
      AContext: TIdContext;
      AException: Exception
    );
    function GetHttpreqwestResult(strHttpReqwest: string): string;

  public
    function GetServiceController: TServiceController; override;
    { Public declarations }
  end;

var
  ILSSaaSMapEditorWebService: TILSSaaSMapEditorWebService;

const
  CServiceVer = '7.3.03 2019.12.31';
  CServiceName = 'ILSSaaSMapEditorWebService';
  CServiceDisplayName = 'ILS SaaS MapEditor Web';

implementation

{$R *.dfm}

uses
  Winapi.ShellApi, IlsMapEditorWebModule;

procedure ServiceController(CtrlCode: DWord); stdcall;
begin
  ILSSaaSMapEditorWebService.Controller(CtrlCode);
end;

function TILSSaaSMapEditorWebService.GetHttpreqwestResult(
  strHttpReqwest: string): string;
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

function TILSSaaSMapEditorWebService.GetServiceController: TServiceController;
begin
  Result := ServiceController;
end;

procedure TILSSaaSMapEditorWebService.StopServer;
begin
//  ToLog('TILSMapEditorWebService.StopServer. FServer.SessionList.ComponentCount='+
//        IntToStr(FServer.SessionList.ComponentCount));
  TGeneral.Stop;
  Sleep(1000);
  FServer.Active := False;
  ToLog('TILSMapEditorWebService.StopServer. FServer.Active');
  FServer.Bindings.Clear;
  ToLog('TILSMapEditorWebService.StopServer. FServer.Bindings.Clear');
  FServer.Free;
  ToLog('=============WebServer Stop=============');
//  TMapEditor.Stop;
end;

procedure TILSSaaSMapEditorWebService.IDServerException(AContext: TIdContext;
  AException: Exception);
begin
  ToLog('TILSSaaSMapEditorWebService.IDServerException:"' + AException.ClassName + '" ' +
        AException.Message + ' Context:' + AContext.ToString);
  if AException is EIdConnClosedGracefully then
    Exit;
  if TGeneral.FStop then
    Exit;
  ToLog('Ошибка работы сервера:'#13#10'"' + AException.ClassName + '" ' + AException.Message);
//  RestartServer();
end;

procedure TILSSaaSMapEditorWebService.RestartServer;
begin
  try
    ToLog('Начинаем перезагрузку системы приёма (http server) - остановка');
//    TMapEditor.Stop;
    FreeAndNil(FServer);
    ToLog('Запуск системы приёма');
    StartServer;
    ToLog('Перезагрузка системы приёма успешно завершена');
  except
    on Ex: Exception do
    begin
      ToLog('Ошибка перезагрузки системы приёма:'#13#10'"' + Ex.ClassName + '" ' + Ex.Message);
    end;
  end;
end;

procedure TILSSaaSMapEditorWebService.ServiceAfterInstall(Sender: TService);
begin
  SvcAfterInstall(CServiceName);
end;

procedure TILSSaaSMapEditorWebService.ServiceBeforeInstall(Sender: TService);
begin
  SvcBeforeInstallUninstall(Self, CServiceName, CServiceDisplayName);
  ToLog('SvcBeforeInstallUninstall ' + CServiceName + ', ' + CServiceDisplayName);
end;

procedure TILSSaaSMapEditorWebService.ServiceShutdown(Sender: TService);
begin
  ToLog('=============Service Shutdown=============');
  StopServer;
end;

procedure TILSSaaSMapEditorWebService.ServiceStart(Sender: TService;
  var Started: Boolean);
begin
  ToLog(ParamStr(0));
  Started := False;
  CoInitialize(nil);
{$IFDEF DEBUG}
  SleepIfIdeRunning(20000);
{$ENDIF}
  ToLog(ParamStr(0));
  ToLog('==== запускается ====');

  //  CoInitialize(nil);
  ToLog('Version ' + CServiceVer);
  SleepIfIdeRunning(10000);
  ToLog('=============Start Service=============');
  StartServer;
  ToLog('============ запущена ============');
  Started := True;
end;

procedure TILSSaaSMapEditorWebService.ServiceStop(Sender: TService;
  var Stopped: Boolean);
begin
  ToLog('=============Начинаем остановку службы=============');
  StopServer;
  ToLog('=============Stop Service=============');
  Stopped := True;
  CoUninitialize();
end;

procedure TILSSaaSMapEditorWebService.StartServer;
begin
  TGeneral.Start(ChangeFileExt(GetModuleName(HInstance),GetConfigSuffix() + '.ini'));
  TGeneral.FSuffix := GetConfigSuffix();

  if not Assigned(FServer) then
    FServer := TIdHTTPWebBrokerBridge.Create(Self);

  if not FServer.Active then
  begin
    FServer.Bindings.Clear;
    FServer.DefaultPort := TGeneral.FPort;
//    FServer.OnException := IDServerException;
    FServer.Active := True;
//    FServer.BeginWork(wmRead);
    GetHttpreqwestResult('http://localhost:'+IntToStr(TGeneral.FPort)+'/?{"Type":"GetZonesProgress","Account":{"Current":"1","Parents":[]}}');
//    WebModuleClass.Create(Self);
//    TMapEditor.Start;
  end;
  if FServer.Active then
    ToLog('Web Server started')
  else
    ToLog('Web Server not started');
end;

end.
