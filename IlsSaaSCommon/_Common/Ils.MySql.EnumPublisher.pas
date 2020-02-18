unit Ils.MySql.EnumPublisher;

interface

uses
  TypInfo, SysUtils, SysConst,
  Ils.MySQL.Conf, ZDataset, ZConnection;


type
  TEnumTextInfo = record
    ClassID: Integer;
    Name: string;
    Comment: string;
    Measure: string;
    Default: string;
    Name2: string;
  end;

  TEnumPublishField = (efClassID, efComment, efMeasure, efDefault, efName2);

  TEnumPublishFieldSet = set of TEnumPublishField;

const
  CEnumPublishFieldSetDef = [efComment, efMeasure, efDefault];

// *** пример: ***
//const
//  CTestEnumInfo: array[TTestEnum] of TEnumTextInfo = (
//    (Name: '1'; Comment: '11'),
//    (Name: '2'; Comment: '22'),
//    (Name: '3'; Comment: '33')
//   );

type
  TEnumPublishMySql = class
  protected
    class var FConnection: TZconnection;
    class var FQuery: TZQuery;
    class procedure DoIter(AIdx: Integer; AInfo: TEnumTextInfo; AFieldSet: TEnumPublishFieldSet);
    class procedure EnumAll(ATypeInfo: PTypeInfo; AInfoArray: array of TEnumTextInfo; AFieldSet: TEnumPublishFieldSet); static;
  public
    class procedure Publish(ATypeInfo: Pointer; AInfoArray: array of TEnumTextInfo; ATableName: string; AConf: TMySQlDatabaseConfig; AFieldSet: TEnumPublishFieldSet = CEnumPublishFieldSetDef); static;
  end;

implementation

{ TEnumPublishMySql }

class procedure TEnumPublishMySql.DoIter(AIdx: Integer; AInfo: TEnumTextInfo; AFieldSet: TEnumPublishFieldSet);
begin
  FQuery.ParamByName('ID').AsInteger := AIdx;
  FQuery.ParamByName('Name').AsString := AInfo.Name;
  if efClassID in AFieldSet then
    FQuery.ParamByName('ClassID').AsInteger := AInfo.ClassID;
  if efComment in AFieldSet then
    FQuery.ParamByName('Comment').AsString := AInfo.Comment;
  if efMeasure in AFieldSet then
    FQuery.ParamByName('Measure').AsString := AInfo.Measure;
  if efDefault in AFieldSet then
    FQuery.ParamByName('DefValue').AsString := AInfo.Default;
  if efName2 in AFieldSet then
    FQuery.ParamByName('Name2').AsString := AInfo.Name2;
  FQuery.ExecSQL;
end;

class procedure TEnumPublishMySql.EnumAll(ATypeInfo: PTypeInfo; AInfoArray: array of TEnumTextInfo; AFieldSet: TEnumPublishFieldSet);
var
  typeData: PTypeData;
  iterValue: Integer;
begin
  if ATypeInfo^.Kind <> tkEnumeration then
    raise EInvalidCast.CreateRes(@SInvalidCast);
  typeData := GetTypeData(ATypeInfo);
  for iterValue := typeData.MinValue to typeData.MaxValue do
    DoIter(iterValue, AInfoArray[iterValue], AFieldSet);
end;

class procedure TEnumPublishMySql.Publish(
  ATypeInfo: Pointer;
  AInfoArray: array of TEnumTextInfo; ATableName: string;
  AConf: TMySQlDatabaseConfig;
  AFieldSet: TEnumPublishFieldSet = CEnumPublishFieldSetDef);
var
  f: TEnumPublishField;
  s: string;
begin
  FConnection := TZConnection.Create(nil);
  FConnection.Protocol  := 'mysql-5';
  FConnection.HostName  := AConf.Host;
  FConnection.Port      := AConf.Port;
  FConnection.Catalog   := AConf.Database;
  FConnection.User      := AConf.Login;
  FConnection.Password  := AConf.Password;
  FConnection.Database  := AConf.Database;
  FConnection.Properties.Text := CMySQLUtf8PropertiesText;
//  FConnection.ClientCodepage := 'utf-8';

  FQuery := TZQuery.Create(nil);
  FQuery.Connection := FConnection;
  s := '';
  for f in AFieldSet do
    case f of
      efClassID: s := s + ', ' + 'ClassID = :ClassID';
      efComment: s := s + ', ' + 'Comment = :Comment';
      efMeasure: s := s + ', ' + 'Measure = :Measure';
      efDefault: s := s + ', ' + 'DefaultValue = :DefValue';
      efName2:  s := s + ', ' + 'Name2 = :Name2';
    end;

  FQuery.SQL.Text :=
    'INSERT IGNORE INTO ' + ATableName +
    ' SET ID = :ID, Name = :Name ' +
    s +
    ' ON DUPLICATE KEY UPDATE' +
    ' ID = :ID, Name = :Name ' +
    s;
  try
    EnumAll(ATypeInfo, AInfoArray, AFieldSet);
  finally
    FQuery.Free;
    FConnection.Free;
  end;

end;

end.
