unit CINIFilesData;
//------------------------------------------------------------------------------
// модуль описания ini-файлов
//------------------------------------------------------------------------------

//------------------------------------------------------------------------------
interface

const

//------------------------------------------------------------------------------
//! расширение файла конфигурации
//------------------------------------------------------------------------------
  CIniDefExtDotted = '.ini';

//------------------------------------------------------------------------------
//! расширение файла стандартных watermark'ов
//------------------------------------------------------------------------------
  CWMDefExtDotted = '.wm';

//------------------------------------------------------------------------------
//! расширение файла индексов kafka
//------------------------------------------------------------------------------
  CKafkaIdxDefExtDotted = '.kafka.offset';

//------------------------------------------------------------------------------
//! секции файла конфигурации
//------------------------------------------------------------------------------
  CIniMainParamsSection   = 'Main Params';
  CIniControlSection      = 'Control';
  CIniMySqlSection        = 'MySQL';
  CIniKafkaSection        = 'Kafka';
  CIniKafkaOffsetSection  = 'Kafka';
  CIniStatSection         = 'stat';
  CIniKafkaSectionDefName = 'config';

//------------------------------------------------------------------------------
//! список параметров
//------------------------------------------------------------------------------
  CIniDBAddr              = 'host';
  CIniDBPort              = 'port';
  CIniDBLogin             = 'login';
  CIniDBPass              = 'password';
  CIniDBName              = 'database';
  CIniDBProtocol          = 'protocol';
  CIniKafkaBootstrap      = 'bootstrap';
  CIniDBUpdate            = 'FromDBUpdateInterval';
  CIniStatPPS             = 'PointPerSec';
  CIniKafkaOffset         = 'offset';
  CIniKafkaGroup          = 'group';
  CIniKafkaTopic          = 'topic';
  CIniKafkaBackupPath     = 'backup';
  CIniKafkaBackupTimeout  = 'backuptimeout';
  CIniKafkaTopicEnabled   = 'enabled';

//------------------------------------------------------------------------------
//! значения параметров по умолчанию
//------------------------------------------------------------------------------
  CIniDBAddrDef             = '127.0.0.1';
  CIniDBPortDef             = 3306;
  CIniDBLoginDef            = 'root';
  CIniDBPassDef             = 'password';
  CIniDBNameDef             = 'saas';
  CIniDBProtocolDef         = 'mysql-5';
  CIniKafkaBootstrapDef     = '127.0.0.1';
  CIniDBUpdateDef           = 30;

//------------------------------------------------------------------------------
implementation

end.

