unit Ils.Utils.License;

interface

uses
  IniFiles, Classes, LbCipher, LbClass, LbAsym, LbRSA, SysUtils, LbUtils,
  Ils.Utils, Ils.Logger, JwaIpHlpApi, JwaIpTypes, JwaWinError, StrUtils;

type
  TIlsLicenseChecker = class
  protected
  const
    CPublicKey =
      '30818802818069521B3AB7C725FA2F9B90F763F4751B11CAD053ECB46492B209245BA3' +
      '3724299BC10A0D38D7B2849268BC5C616D7D60EBFB92890CBEB25A34C3FCD1D90A55B8' +
      '593CDA20F4C740DB834DEBA9511E82B7FF84ED22B5CDB998B5001A3231B18F1EF62E01' +
      '972B2402B327A9EBC76298D7719667644817880DB205DE292FE60DB5B70203009310';

  type
    TLicenseEventType = (letNormal, letExpired);
    TLicenseEvent = procedure(ASender: TObject; AEventType: TLicenseEventType) of object;
    class var FIlsLicenseChecker: TIlsLicenseChecker;

  private
    FValidFile: Boolean;
    FValidMac: Boolean;
    FLicenseFileName: string;
    FSignature: AnsiString;
    FValues: TStringList;
    FRSASSA: TLbRSASSA;
    FOnLicenseEvent: TLicenseEvent;
    FLogFunction: TLogFunction;
    procedure OnGetSignature(Sender: TObject; var Sig: TRSASignatureBlock);

    function ToLog(const AMessage: string): string;
    class constructor Create;
    class destructor Destroy;
  public
    function ValidFile: Boolean;
    function Expired: Boolean;
    function ValidMac: Boolean;
    function Valid: Boolean;

    property OnLicenseEvent: TLicenseEvent read FOnLicenseEvent write FOnLicenseEvent;

    constructor Create(const ALicenseFileName: string);
    destructor Destroy; override;
  end;

function IlsLicenseChecker: TIlsLicenseChecker;

implementation

function IlsLicenseChecker: TIlsLicenseChecker;
begin
  Result := TIlsLicenseChecker.FIlsLicenseChecker;
end;

{ TIlsLicenseChecker }

constructor TIlsLicenseChecker.Create(const ALicenseFileName: string);

{  function IsMacValid: Boolean;
  var
    NumInterfaces: Cardinal;
    OutBufLen: Cardinal;
    i: integer;
    AdapterInfo: TArray<TIpAdapterInfo>;
    MAC: string;
  begin
    Result := False;

    if FValues.Values['MAC'] = '' then
      Exit(True);

    try
      GetNumberOfInterfaces(NumInterfaces);
      SetLength(AdapterInfo, NumInterfaces);
      OutBufLen := NumInterfaces * SizeOf(TIpAdapterInfo);
      GetAdaptersInfo(@AdapterInfo[0], OutBufLen);

      for i := 0 to NumInterfaces - 1 do begin
        MAC := Format('%.2x:%.2x:%.2x:%.2x:%.2x:%.2x',
          [AdapterInfo[i].Address[0], AdapterInfo[i].Address[1],
           AdapterInfo[i].Address[2], AdapterInfo[i].Address[3],
           AdapterInfo[i].Address[4], AdapterInfo[i].Address[5]]);

        LogMessage('проверка MAC-адреса(' + MAC + ')');

        if SameText(Trim(MAC), Trim(FValues.Values['MAC'])) then
          Exit(True);
      end;
    except
      on E: Exception do begin

      end;
    end;
  end;}

  function IsMacValid: Boolean;
  const
    AF_UNSPEC = 0;
    GAA_FLAG_INCLUDE_ALL_INTERFACES = $100;
    WORKING_BUFFER_SIZE = 15000;
    MAX_TRIES = 3;
  var
    pAddresses,
    pCurrAddresses: PIpAdapterAddresses;
    dwRetVal,
    outBufLen: Cardinal;
    i: Integer;
    MAC: string;
  begin
//    Memo1.Lines.Clear;
    Result := False;
    if FValues.Values['MAC'] = '' then
      Exit(True);

    outBufLen := WORKING_BUFFER_SIZE;
    pAddresses := nil;
    i := 0;
    repeat
      if Assigned(pAddresses) then
        FreeMem(pAddresses);

      GetMem(pAddresses, outBufLen);
      if not Assigned(pAddresses) then
        Exit(False);

      dwRetVal := GetAdaptersAddresses(AF_UNSPEC, GAA_FLAG_INCLUDE_ALL_INTERFACES, nil, pAddresses, @outBufLen);
      Inc(i);
    until (dwRetVal <> ERROR_BUFFER_OVERFLOW) or (i = MAX_TRIES);

    try
      if NO_ERROR <> dwRetVal then
//      begin
//        if ERROR_NO_DATA = dwRetVal then begin
//          MessageDlg('No addresses were found for the requested parameters', mtInformation, [mbOK], 0);
          Exit(False);
//        end
//        else
//          raise Exception.Create(SysErrorMessage(dwRetVal));
//      end;

      pCurrAddresses := pAddresses;
      while Assigned(pCurrAddresses) do
      begin
        if pCurrAddresses^.PhysicalAddressLength > 0 then
        begin
//          Memo1.Lines.Add(pCurrAddresses^.FriendlyName);
          MAC := '';

          for i := 0 to pCurrAddresses^.PhysicalAddressLength - 1 do
            MAC := MAC + IfThen(i > 0, '-', '') + Format('%.2X', [pCurrAddresses^.PhysicalAddress[i]]);

          if SameText(Trim(MAC), Trim(FValues.Values['MAC'])) then
            Exit(True);
//          Memo1.Lines.Add(macAddress);
//          Memo1.Lines.Add('');
        end;
        pCurrAddresses := pCurrAddresses^.Next;
      end;

    finally
      if Assigned(pAddresses) then
        FreeMem(pAddresses);
    end;
  end;

var
  i: Integer;
  bin: TMemoryStream;
  ssig: TStringList;
  ini: tinifile;
begin
  FLogFunction := Ils.Logger.ToLog;
  FValidFile := False;
  FValidMac := False;
  FLicenseFileName := ALicenseFileName;
  FValues := TStringList.Create;
  ssig := TStringList.Create;
  FRSASSA := TLbRSASSA.Create(nil);
  FRSASSA.OnGetSignature := OnGetSignature;

  bin := TMemoryStream.Create;
  ini := TIniFile.Create(ALicenseFileName);
  try
    FRSASSA.HashMethod := hmSHA1;
    FRSASSA.KeySize := aks1024;
    bin.SetSize(Trunc(Length(CPublicKey) / 2));
    HexToBin(CPublicKey, bin.Memory, bin.Size);
    FRSASSA.PublicKey.LoadFromStream(bin);

    ini.ReadSectionValues('license_values', FValues);
    FValues.Sort;

    ini.ReadSectionValues('license', ssig);
    FSignature := '';
    for i := 0 to ssig.Count - 1 do
    begin
      FSignature := FSignature + AnsiString(ssig.ValueFromIndex[i]);
      ToLog(ssig.ValueFromIndex[i]);
    end;

    try
      FValidFile := FRSASSA.VerifyString(AnsiString(FValues.Text));

      FValidMac := IsMacValid;

    except
      on E: Exception do begin
//        logmessage()
      end;

    end;
  finally
    ssig.Free;
    ini.Free;
  end;
end;

class constructor TIlsLicenseChecker.Create;
begin
  FIlsLicenseChecker := TIlsLicenseChecker.Create(ChangeFileExt(GetModuleName(HInstance), '.license'));
end;

class destructor TIlsLicenseChecker.Destroy;
begin
  FIlsLicenseChecker.Free;
end;

procedure TIlsLicenseChecker.OnGetSignature(Sender: TObject; var Sig: TRSASignatureBlock);
begin
  HexToBuffer(string(FSignature), Sig, SizeOf(Sig));
end;

function TIlsLicenseChecker.Valid: Boolean;
begin
  Result := ValidFile and ValidMac and (not Expired);
  if not Result then
    ToLog('Лицензия проверку не прошла.');
end;

function TIlsLicenseChecker.ValidFile: Boolean;
begin
  Result := FValidFile;
  if not Result then
    ToLog('Некорректный файл лицензии(' + FLicenseFileName + ').');
end;

destructor TIlsLicenseChecker.Destroy;
begin
  FRSASSA.Free;
  FValues.Free;
  inherited;
end;

function TIlsLicenseChecker.Expired: Boolean;
begin
  Result :=
    (FValues.Values['validtrough'] <> '') and
    (Now >= (IlsToDateTime(FValues.Values['validtrough']) + 1));

  if Result then
    ToLog('Лицензия закончилась(' + FValues.Values['validtrough'] + ').');
end;

function TIlsLicenseChecker.ValidMac: Boolean;
begin
  Result := FValidMac;

  if not Result then
    ToLog('Не найден MAC-адрес(' + FValues.Values['MAC'] + ').');
end;

function TIlsLicenseChecker.ToLog(const AMessage: string): string;
begin
  if not Assigned(FLogFunction) then
    Result := AMessage
  else
    Result := FLogFunction(AMessage);
end;

end.
