unit Ils.Kafka.Conf;

interface

uses
  SysUtils, IniFiles,
  ULibKafka,
  Ils.Logger,
  StrUtils;

type

  TKafkaConsumerParams = record
    BootStrap: string;
    Group: string;
    Topic: string;
    constructor Create(
      const AIniFile, AOffsetIniFile: TIniFile;
      const ASectionSuffix, ABootstrapDef, AGroupDef, ATopicDef: string;
      out ROffset: Int64
    );
  end;

  TKafkaProducerParams = record
    BootStrap: string;
    Topic: string;
    BackupPath: string;
    BackupTimeout: Integer;
    Enabled: Boolean;
    constructor Create(
      const AIniFile: TIniFile;
      const ASectionSuffix, ABootstrapDef, ATopicDef, ABackupDef: string;
      const ABackupTimeout: Integer
    );
  end;

implementation

uses
  CINIFilesData;

{ TKafkaConsumerParams }

constructor TKafkaConsumerParams.Create(
  const AIniFile, AOffsetIniFile: TIniFile;
  const ASectionSuffix, ABootstrapDef, AGroupDef, ATopicDef: string;
  out ROffset: Int64
);
var
  SectionName: string;
  OffsetStr: string;
begin
  SectionName := CIniKafkaSection + IfThen(ASectionSuffix <> '', '.' + ASectionSuffix, '');
  BootStrap := AIniFile.ReadString(SectionName, CIniKafkaBootstrap, ABootstrapDef);
  Group := AIniFile.ReadString(SectionName, CIniKafkaGroup, AGroupDef);
  Topic := AIniFile.ReadString(SectionName, CIniKafkaTopic, ATopicDef);
  ToLog(Format('"%s" kafka: %s:%s:%s', [ASectionSuffix, BootStrap, Group, Topic]));

  if not Assigned(AOffsetIniFile) then
    Exit;
  OffsetStr := AOffsetIniFile.ReadString(CIniKafkaOffsetSection, CIniKafkaOffset + '_' + Topic, '');
  ToLog(Format('"%s" kafka start offset: %s', [ASectionSuffix, OffsetStr]));
  ROffset := StrToInt64Def(OffsetStr, RD_KAFKA_OFFSET_BEGINNING);
end;

{ TKafkaProducerParams }

constructor TKafkaProducerParams.Create(
  const AIniFile: TIniFile;
  const ASectionSuffix, ABootstrapDef, ATopicDef, ABackupDef: string;
  const ABackupTimeout: Integer
);
var
  SectionName: string;
begin
  SectionName := CIniKafkaSection + IfThen(ASectionSuffix <> '', '.' + ASectionSuffix, '');
  BootStrap := AIniFile.ReadString(SectionName, CIniKafkaBootstrap, ABootstrapDef);
  Topic := AIniFile.ReadString(SectionName, CIniKafkaTopic, ATopicDef);
  BackupPath := AIniFile.ReadString(SectionName, CIniKafkaBackupPath, ABackupDef);
  BackupTimeout := AIniFile.ReadInteger(SectionName, CIniKafkaBackupTimeout, ABackupTimeout);
  Enabled := AIniFile.ReadBool(SectionName, CIniKafkaTopicEnabled, True);

  ToLog(Format('"%s" kafka producer: %s:%s', [ASectionSuffix, BootStrap, Topic]));
end;

end.

