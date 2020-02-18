unit UConfigSaas;

interface

uses
  UConfigMySql, SysUtils;

type
  TConfigSaasSensorMapping = class(TConfigMySqlAbstract)
  protected
    function GetDBConfigModifiedDateTime: TDateTime; override;
    function ReloadDBConfig: Boolean; override;
  end;


  TConfigSaasTrackRetranslation = class(TConfigMySqlAbstract)
  protected
    function GetDBConfigModifiedDateTime: TDateTime; override;
    function ReloadDBConfig: Boolean; override;
  end;

implementation

{ TConfigSaasSensorMapping }

function TConfigSaasSensorMapping.GetDBConfigModifiedDateTime: TDateTime;
begin
//  FMySqlQuery.SQL.Text := 'select '
  Result := 0;
end;

function TConfigSaasSensorMapping.ReloadDBConfig: Boolean;
begin
  Result := False;
end;

{ TConfigSaasTrackRetranslation }

function TConfigSaasTrackRetranslation.GetDBConfigModifiedDateTime: TDateTime;
begin
  Result := 0;
//
//  FMySqlQuery.Close;
//  FMySqlQuery.SQL.Text := 'select Max(Modified) as LastChanged from TrackRetranslation';
//  try
//    FMySqlQuery.Open;
//    FMySqlQuery.First;
//    if FMySqlQuery.RecordCount >= 1 then
//      Result := FMySqlQuery.FieldByName('LastChanged').AsDateTime;
//    FMySqlQuery.Close;
//  except
//    on E: Exception do begin
//      Log('Ошибка чтения конфигурации TConfigSaasTrackRetranslation "' + FMySqlQuery.SQL.Text + '": ' + E.Message);
//    end;
//  end;
end;

function TConfigSaasTrackRetranslation.ReloadDBConfig: Boolean;
//var
//  IMEI: Int64;
//  ID: Integer;
//  From: TDateTime;
//  Till: TDateTime;
//  Name: string;
//  Topic: string;
//  Bootstrap: string;
begin
  Result := False;
//
//  FJson.Clear;
//
//  FMySqlQuery.Close;
//  FMySqlQuery.SQL.Text :=
//    'select ' +
//    'trn.IMEI, trn.TrackRetranslatorID, trn.Created, trn.Modified, ' +
//    'trr.ID, trr.Name, trr.Comment, trr.Target, trr.Bootstrap, trr.Topic ' +
//    'from TrackRetranslation trn ' +
//    'join TrackRetranslator  trr on trr.ID = trn.TrackRetranslatorID ' +
//    'order by trn.IMEI, trr.ID';
//
//  try
//    FMySqlQuery.Open;
//    FMySqlQuery.First;
//    while not FMySqlQuery.Eof do
//    begin
//      //FMySqlQuery.FieldByName('LastChanged').AsDateTime
//
//      IMEI      := FMySqlQuery.FieldByName('IMEI').AsLargeInt;
//      ID        := FMySqlQuery.FieldByName('ID').AsInteger;
//      Name      := FMySqlQuery.FieldByName('Name').AsString;
//      Topic     := FMySqlQuery.FieldByName('Topic').AsString;
//      Bootstrap := FMySqlQuery.FieldByName('Bootstrap').AsString;
//
////      if not FJson.Contains('imei-split-list') then
//      FJson.O['imei-split-list'].A[IntToStr(IMEI)].Add(Topic);
//
//      FMySqlQuery.Next;
//    end;
//
//    FMySqlQuery.Close;
//
//    Result := True;
//  except
//    on E: Exception do begin
//      Log('Ошибка чтения конфигурации TConfigSaasTrackRetranslation "' + FMySqlQuery.SQL.Text + '": ' + E.Message);
//    end;
//  end;
end;

end.
