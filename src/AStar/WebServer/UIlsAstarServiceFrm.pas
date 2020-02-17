unit UIlsAstarServiceFrm;

interface

uses Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
  System.Classes, Vcl.Graphics, Vcl.Controls, Vcl.SvcMgr, Vcl.Dialogs,
  Vcl.AppEvnts, Vcl.StdCtrls, IdHTTPWebBrokerBridge, Web.HTTPApp;

type
  TILSAstarWebService = class(TService)
    procedure ServiceStart(Sender: TService; var Started: Boolean);
    procedure ServiceStop(Sender: TService; var Stopped: Boolean);
  private
    FServer: TIdHTTPWebBrokerBridge;
    procedure StartServer;
    procedure StopServer;
    { Private declarations }
  public
    function GetServiceController: TServiceController; override;
    { Public declarations }
  end;

var
  ILSAstarWebService: TILSAstarWebService;

implementation

{$R *.dfm}

uses
  Winapi.ShellApi;

procedure ServiceController(CtrlCode: DWord); stdcall;
begin
  ILSAstarWebService.Controller(CtrlCode);
end;

function TILSAstarWebService.GetServiceController: TServiceController;
begin
  Result := ServiceController;
end;

procedure TILSAstarWebService.StopServer;
begin
  FServer.Active := False;
  FServer.Bindings.Clear;
  FServer.Free;
end;

procedure TILSAstarWebService.ServiceStart(Sender: TService;
  var Started: Boolean);
begin
  StartServer;
  Started := True;
end;

procedure TILSAstarWebService.ServiceStop(Sender: TService;
  var Stopped: Boolean);
begin
  StopServer;
  Stopped := True;
end;

procedure TILSAstarWebService.StartServer;
begin
  if not Assigned(FServer) then
    FServer := TIdHTTPWebBrokerBridge.Create(Self);

  if not FServer.Active then
  begin
    FServer.Bindings.Clear;
    FServer.DefaultPort := 57543;
    FServer.Active := True;
  end;
end;

end.
