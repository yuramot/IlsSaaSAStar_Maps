unit UConfigJson;

interface

uses
  UConfigBase, Ils.Kafka, JsonDataObjects, System.SysUtils, SyncObjs, Windows;

type

  TConfigJson = class(TConfigBase)
  private
    FConfigFileName: string;
  protected
    FCriticalSection: TCriticalSection;
    FJson: TJsonObject;
    function Reload: Boolean; override;
    function GetModifiedDateTime: TDateTime; override;
    function GetModifiedCount: Integer; override;
    function ReadJSONValue(const AValuePath: string): TJsonDataValueHelper;
    procedure Log(AMessage: string); virtual;
    function GetJson: TJsonObject;
  public
    constructor Create(
      const AConfigFileName: string;
      const AConfigChangeCB: TConfigEvent;
      const ALogCB: TLogFunc = nil;
      const ACheckInterval: Integer = 1000); reintroduce; virtual;
    destructor Destroy; override;

    property Json: TJsonObject read GetJson;

    function ReadInteger(const AValuePath: string; const ADef: Integer = 0): Integer;
    function ReadLong(const AValuePath: string; const ADef: Int64 = 0): Int64;
    function ReadFloat(const AValuePath: string; const ADef: Double = 0): Double;
    function ReadDateTime(const AValuePath: string; const ADef: TDateTime = 0): TDateTime;
    function ReadString(const AValuePath: string; const ADef: string = ''): string;
    function IsArray(const AValuePath: string): Boolean;
    function ArrayCount(const AValuePath: string): Integer;
  end;




implementation

function GetFileModifiedDateTime(AFileName: string): TDateTime;
var
  fad: TWin32FileAttributeData;
  ModifiedTime: TFileTime;
  SystemTime: TSystemTime;
begin
  if not FileExists(AFileName) then
    Exit(0);

  if not GetFileAttributesEx(PChar(AFileName), GetFileExInfoStandard, @fad) then
    RaiseLastOSError;

  //fad.ftCreationTime, fad.ftLastAccessTime
  FileTimeToLocalFileTime(fad.ftLastWriteTime, ModifiedTime);
  FileTimeToSystemTime(ModifiedTime, SystemTime);
  Result := SystemTimeToDateTime(SystemTime);
end;

{ TConfigJson }

function TConfigJson.ArrayCount(const AValuePath: string): Integer;
var
  JSONValue: TJsonDataValueHelper;
begin
  JSONValue := ReadJSONValue(AValuePath);
  if not JSONValue.IsNull and (JSONValue.Typ = jdtArray) then
    Result := JSONValue.Count
  else
    Result := -1;
end;

constructor TConfigJson.Create(const AConfigFileName: string; const AConfigChangeCB: TConfigEvent; const ALogCB: TLogFunc; const ACheckInterval: Integer);
begin
  FCriticalSection := TCriticalSection.Create;
  FJson := TJsonObject.Create;
  FConfigFileName := AConfigFileName;
  inherited Create(AConfigChangeCB, ALogCB, ACheckInterval);
  Load;
end;

destructor TConfigJson.Destroy;
begin
  FreeAndNil(FJson);
  FreeAndNil(FCriticalSection);
  inherited;
end;

function TConfigJson.GetJson: TJsonObject;
begin
  FCriticalSection.Enter;
  FCriticalSection.Leave;
  Result := FJson;
end;

function TConfigJson.GetModifiedDateTime: TDateTime;
begin
  Result := GetFileModifiedDateTime(FConfigFileName);
end;

function TConfigJson.GetModifiedCount: Integer;
begin
  Result := 0;
end;

function TConfigJson.IsArray(const AValuePath: string): Boolean;
var
  JSONValue: TJsonDataValueHelper;
begin
  JSONValue := ReadJSONValue(AValuePath);
  Result := not JSONValue.IsNull and (JSONValue.Typ = jdtArray);
end;

procedure TConfigJson.Log(AMessage: string);
begin
  if Assigned(FLogCB) then
    FLogCB(AMessage);
end;

function TConfigJson.ReadDateTime(const AValuePath: string; const ADef: TDateTime = 0): TDateTime;
var
  JSONValue: TJsonDataValueHelper;
begin
  Result := ADef;
  JSONValue := ReadJSONValue(AValuePath);
  if not JSONValue.IsNull and (JSONValue.Typ = jdtDateTime) then
    Result := JSONValue.DateTimeValue
  else
    Log('Ошибка чтения даты/времени');
end;

function TConfigJson.ReadFloat(const AValuePath: string; const ADef: Double = 0): Double;
var
  JSONValue: TJsonDataValueHelper;
begin
  Result := ADef;
  JSONValue := ReadJSONValue(AValuePath);
  if not JSONValue.IsNull and ((JSONValue.Typ = jdtFloat) or (JSONValue.Typ = jdtInt) or (JSONValue.Typ = jdtLong)) then
    Result := JSONValue.FloatValue
  else
    Log('Ошибка чтения числа с плавающей запятой');
end;

function TConfigJson.ReadInteger(const AValuePath: string; const ADef: Integer = 0): Integer;
var
  JSONValue: TJsonDataValueHelper;
begin
  Result := ADef;
  JSONValue := ReadJSONValue(AValuePath);
  if not JSONValue.IsNull and (JSONValue.Typ = jdtInt) then
    Result := JSONValue.IntValue
  else
    Log('Ошибка чтения целого числа');
end;

function TConfigJson.ReadJSONValue(const AValuePath: string): TJsonDataValueHelper;
begin
  try
    Result := FJson.Path[AValuePath];
  except
    on e: Exception do
      Log(e.Message);
  end;
end;

function TConfigJson.ReadLong(const AValuePath: string; const ADef: Int64 = 0): Int64;
var
  JSONValue: TJsonDataValueHelper;
begin
  Result := ADef;
  JSONValue := ReadJSONValue(AValuePath);
  if not JSONValue.IsNull and ((JSONValue.Typ = jdtInt) or (JSONValue.Typ = jdtLong)) then
    Result := JSONValue.LongValue
  else
    Log('Ошибка чтения длинного целого');
end;

function TConfigJson.ReadString(const AValuePath: string; const ADef: string = ''): string;
var
  JSONValue: TJsonDataValueHelper;
begin
  Result := ADef;
  JSONValue := ReadJSONValue(AValuePath);
  if not JSONValue.IsNull and (JSONValue.Typ <> jdtArray) then
    Result := JSONValue.Value
  else
    Log('Ошибка чтения строкового параметра');
end;

function TConfigJson.Reload: Boolean;
begin
  Result := True;
  FCriticalSection.Enter;
  try
    try
      FJson.LoadFromFile(FConfigFileName);
      Log('Config loaded.');
    except
      Result := False;
    end;
  finally
    FCriticalSection.Leave;
  end;
end;


end.
