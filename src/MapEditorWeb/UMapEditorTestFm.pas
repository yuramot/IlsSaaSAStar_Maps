unit UMapEditorTestFm;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, UMapEditor, JsonDataObjects, Ils.Json.Names, Ils.Json.Utils,
  Vcl.StdCtrls, IdHTTPWebBrokerBridge, Web.HTTPApp, IlsMapEditorWebModule, Ils.Logger;

type
  TForm3 = class(TForm)
    edtID: TEdit;
    btn1: TButton;
    mmo1: TMemo;
    lbl1: TLabel;
    edtRectangle: TEdit;
    lbl2: TLabel;
    btn2: TButton;
    edtRes: TEdit;
    btn3: TButton;
    btnStartWeb: TButton;
    btn4: TButton;
    btn5: TButton;
    procedure btn1Click(Sender: TObject);
    procedure btn2Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure btn3Click(Sender: TObject);
    procedure btnStartWebClick(Sender: TObject);
    procedure btn4Click(Sender: TObject);
    procedure btn5Click(Sender: TObject);
  private
    fs: TFormatSettings;
    FServer: TIdHTTPWebBrokerBridge;
    procedure StartServer;
    procedure StopServer;
  public
    { Public declarations }
  end;

var
  Form3: TForm3;

implementation

{$R *.dfm}

procedure TForm3.btn1Click(Sender: TObject);
var
  osm_id, hashstart, hashend: Int64;
  s: string;
  i: Integer;
begin
  s := Trim(edtID.Text);
  i := Pos(' ',s);
  osm_id := StrToInt64(Copy(s,1,i-1));
  Delete(s,1,i);
  i := Pos(' ',s);
  hashstart := StrToInt64(Copy(s,1,i-1));
  Delete(s,1,i);
  hashend := StrToInt64(s);
  s := GetRouteLine(osm_id, hashstart, hashend);
  mmo1.Lines.Text := s;
end;

procedure TForm3.btn2Click(Sender: TObject);
var
  s: string;
  TopLeftLa, TopLeftLo, BottomRightLa, BottomRightLo : double;
  iRoadType : Integer;
  iZone : UInt64;
  i: Integer;
  iTick : Integer;
  JSon :TJsonObject;
begin
  try
  iTick := GetTickCount;
  iRoadType := -1;
  iZone := High(UInt64);
  s := Trim(edtRectangle.Text);
  i := Pos(' ',s);
  TopLeftLa := StrToFloat(Copy(s,1,i-1), fs);
  Delete(s,1,i);
  i := Pos(' ',s);
  TopLeftLo := StrToFloat(Copy(s,1,i-1), fs);
  Delete(s,1,i);
  i := Pos(' ',s);
  BottomRightLa := StrToFloat(Copy(s,1,i-1), fs);
  Delete(s,1,i);
  i := Pos(' ',s);
  if i > 0 then
  begin
    BottomRightLo := StrToFloat(Copy(s,1,i-1), fs);
    Delete(s,1,i);
    i := Pos(' ',s);
    if i > 0 then
    begin
      iRoadType := StrToInt(Copy(s,1,i-1));
      Delete(s,1,i);
      iZone := StrToInt64(s);
    end else
      iRoadType := StrToInt(s);
  end else
    BottomRightLo := StrToFloat(s, fs);
  JSon := GetRoutesFromRestangle(TopLeftLa, TopLeftLo, BottomRightLa, BottomRightLo, iRoadType, iZone);
  s := Json.ToString;
  mmo1.Lines.Text := s;
  edtRes.Text := 'Ticks = '+IntToStr(GetTickCount-iTick) + ' Count = '+ IntToStr(Json.A['road'].Count);
  finally
    JSon.Free
  end;
end;

procedure TForm3.btn3Click(Sender: TObject);
var
  osm_id, hashstart, hashend: Int64;
  s, s1: string;
  i, iRoadType, iTick: Integer;
  bRes: Boolean;
begin
  iTick := GetTickCount;
  s := Trim(edtID.Text);
  i := Pos(' ',s);
  osm_id := StrToInt64(Copy(s,1,i-1));
  Delete(s,1,i);
  i := Pos(' ',s);
  hashstart := StrToInt64(Copy(s,1,i-1));
  Delete(s,1,i);
  i := Pos(' ',s);
  hashend := StrToInt64(Copy(s,1,i-1));
  Delete(s,1,i);
  iRoadType := StrToInt(s);
  bRes := SetNewRoadType(osm_id, hashstart, hashend, iRoadType, s1);
  s := BoolToStr(bRes, True);
  mmo1.Lines.Text := 'Ticks = ' + IntToStr(GetTickCount - iTick) + ' Result = ' + s;
end;

procedure TForm3.btn4Click(Sender: TObject);
begin
  StopServer;
end;

procedure TForm3.btn5Click(Sender: TObject);
var
  Json : TJsonObject;
begin
  Json := TJsonObject.Create;
  mmo1.Clear;
  Json.S['type'] := 'FeatureCollection';
  with Json.A['features'].AddObject do
  begin
    S['type'] := 'Feature';
    O['properties'].S['@id'] := '203856104-949655268565395222-949655268561885239';
    O['properties'].S['speed'] := '60';
    O['properties'].S['roadtype'] := '15';
    O['geometry'].S['type'] := 'LineString';
    O['geometry'].A['coordinates'].Add('GWucfuu75c9fxqucfuu75cjxm0');
  end;
  with Json.A['features'].AddObject do
  begin
    S['type'] := 'Feature';
    O['properties'].S['@id'] := '303856104-949655268565395222-949655268561885239';
    O['properties'].S['speed'] := '90';
    O['properties'].S['roadtype'] := '1';
    O['geometry'].S['type'] := 'LineString';
    O['geometry'].A['coordinates'].Add('GWucfuu75c9fxqucfuu75cjxm0');
  end;
  mmo1.Lines.Text := Json.ToString;
  Json.Free;
end;

procedure TForm3.btnStartWebClick(Sender: TObject);
begin
  StartServer;
end;

procedure TForm3.FormCreate(Sender: TObject);
begin
  fs := TFormatSettings.Create;
  fs.DecimalSeparator := '.';
end;

procedure TForm3.StartServer;
begin
  ToLog('Starting server...');
  if not Assigned(FServer) then
    FServer := TIdHTTPWebBrokerBridge.Create(Self);

  if Assigned(FServer) then
    ToLog('Assigned(FServer)')
  else
  begin
    ToLog('not Assigned(FServer)') ;
    Exit;
  end;
  try
  if not FServer.Active then
  begin
    FServer.Bindings.Clear;
    FServer.DefaultPort := 7653;
    FServer.Active := True;
    btnStartWeb.Enabled := not FServer.Active;
  end;
  except on E : Exception do
    ToLog('Error - '+E.Message);
  end;
  if FServer.Active then
    ToLog('Server started');
end;

procedure TForm3.StopServer;
begin
  FServer.Active := False;
  btnStartWeb.Enabled := not FServer.Active;
  FServer.Bindings.Clear;
  FServer.Free;
  ToLog('Server stoped');
end;

end.
