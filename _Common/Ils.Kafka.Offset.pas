unit Ils.Kafka.Offset;

interface
uses
  StrUtils, DateUtils, IniFiles, SysUtils, ULibKafka, Ils.Logger;

type

  TKafkaOffset = class
  private
    FFileName: string;
    FOffset: Int64;
    FWriteOffsetPeriod: Integer;
    FWriteOffsetDTPrev: TDateTime;
    FWriteOffsetDT: TDateTime;
    FLastOffsetCounted: Int64;
    FLastOffsetDT: TDateTime;
    FPrev10KOffset: Int64;
    FPrev10KDT : TDateTime;

    FOffsetIniFile: TIniFile;

    procedure SaveOffset(const ANewOffset: Int64; const AStoreValue: Boolean; const ADT: TDateTime = 0);
    procedure StoreOffset;
    function GetValue: Int64;
    procedure SetValue(const Value: Int64);
  public
    function Save(AOffset: Int64): Int64;
    property Value: Int64 read GetValue write SetValue;
    constructor Create(const AFileName: string = ''; const AWriteOffsetPeriod: Integer = 1000);
    destructor Destroy; override;
  end;

const
  CKafkaOffsetExt: string =  '.kafka.offset';


implementation

{ TOffset }

constructor TKafkaOffset.Create(const AFileName: string;
  const AWriteOffsetPeriod: Integer);
begin
  FFileName := AFileName;
  FWriteOffsetPeriod := AWriteOffsetPeriod;
  if FFileName = '' then
    FFileName := ChangeFileExt(GetModuleName(0), CKafkaOffsetExt);

  FOffsetIniFile := TIniFile.Create(FFileName);
  FOffset := StrToInt64Def(FOffsetIniFile.ReadString('kafka', 'offset', ''), RD_KAFKA_OFFSET_BEGINNING);
end;

destructor TKafkaOffset.Destroy;
begin
  StoreOffset;
  FOffsetIniFile.Free;

  inherited;
end;

function TKafkaOffset.GetValue: Int64;
begin
  Result := FOffset;
end;

procedure TKafkaOffset.SetValue(const Value: Int64);
begin
  if FOffset = Value then
    Exit;

  SaveOffset(Value, True, 0);
end;

procedure TKafkaOffset.StoreOffset;
begin
  FOffsetIniFile.WriteString('kafka', 'offset', IntToStr(FOffset));
end;

function TKafkaOffset.Save(AOffset: Int64): Int64;
begin
  Value := AOffset;
  Result := Value;
end;

procedure TKafkaOffset.SaveOffset(const ANewOffset: Int64; const AStoreValue: Boolean; const ADT: TDateTime = 0);
begin
  FOffset := ANewOffset;

  if AStoreValue then
    if (FLastOffsetDT + FWriteOffsetPeriod * OneSecond) < Now then
    begin
      StoreOffset;
      FWriteOffsetDTPrev := FWriteOffsetDT;
      FWriteOffsetDT := Now();
      if (FWriteOffsetDTPrev > 0) and (FWriteOffsetDTPrev <> FWriteOffsetDT) then
      begin
        FOffsetIniFile.WriteInteger('stat', 'PointPerSec', Round((ANewOffset - FLastOffsetCounted) / (FWriteOffsetDT - FWriteOffsetDTPrev) / SecsPerDay));
        FLastOffsetCounted := ANewOffset;
      end;

      FLastOffsetDT := Now;
    end;

  if (Trunc(ANewOffset/10000)*10000 = ANewOffset) and (FPrev10KOffset < ANewOffset) then
  begin
    ToLog('Offset = ' + IntToStr(ANewOffset) + IfThen(ADT = 0, '', ', ' + FormatDateTime('yyyy-mm-dd hh:nn:ss.zzz', ADT)) + ', ' + IntToStr( Round((ANewOffset - FPrev10KOffset)/(Now - FPrev10KDT)/SecsPerDay)) + 'pps');

    FPrev10KDT := Now;
    FPrev10KOffset := ANewOffset;
  end;
end;


end.
