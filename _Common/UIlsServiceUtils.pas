unit UIlsServiceUtils;

interface

uses
  System.SysUtils, Winapi.WinSvc;

//function GetWorkParam(): string;
function GetParamByName(const AParamName: string): string;
function HasAnyParamOf(AParams: array of string): Boolean;
procedure ChangeServiceParams(const AServiceName: string; const AServiceParamName: string; const AServiceParamValue: string);

implementation

function GetParamByName(const AParamName: string): string;
var
  i: Integer;
  ConfigParam: string;
begin
  Result := '';
  for i := 1 to System.ParamCount do
  begin
    ConfigParam := ParamStr( I );
    if (Length(ConfigParam) > (Length(AParamName) + 2)) and
       (LowerCase(Copy(ConfigParam, 1, (Length(AParamName) + 2))) = LowerCase('/' + AParamName + ':')) then
    begin
      Result := Copy(ConfigParam, Length(AParamName) + 3, MaxInt);
      Exit;
    end;
  end;
end;

function HasAnyParamOf(AParams: array of string): Boolean;
var
  i, j: Integer;
begin
  Result := False;

  for i := 1 to System.ParamCount do
    for j := Low(AParams) to High(AParams) do
      if Pos(LowerCase('/' + aparams[j]), LowerCase(ParamStr(i))) = 2 then
        Exit(True);
end;

procedure ChangeServiceParams(const AServiceName: string; const AServiceParamName: string; const AServiceParamValue: string);
var
  //!
//  ConfigParam: string;
  //! менеджер служб
  SvcMgr: SC_HANDLE;
  //! служба
  Svc: SC_HANDLE;
//------------------------------------------------------------------------------
begin
  if ( AServiceParamValue <> '' ) then
  begin
    SvcMgr := OpenSCManager( nil, nil, SC_MANAGER_ALL_ACCESS );
    if ( SvcMgr <> 0 ) then
    begin
      Svc := OpenService( SvcMgr, PChar( AServiceName + '_' + AServiceParamValue ), SERVICE_ALL_ACCESS );
      if ( Svc <> 0 ) then
      begin
        if not ChangeServiceConfig(
          Svc,
          SERVICE_WIN32_OWN_PROCESS,
          SERVICE_AUTO_START,
          SERVICE_ERROR_NORMAL,
          PChar( ParamStr( 0 ) + ' /' + AServiceParamName + ':' + AServiceParamValue ),
          nil,
          nil,
          nil,
          nil,
          nil,
          nil
        )
        then
          raise Exception.Create( 'Не удалось изменение статуса службы' );
      end
      else
        raise Exception.Create( 'Не удалось подключение к службе' );
    end
    else
      raise Exception.Create( 'Не удалось подключение к менеджеру служб' );
  end
  else
    raise Exception.Create( 'Параметр клиента не задан' );
end;


end.
