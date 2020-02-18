unit Ils.Redis.Conf;

interface

uses
  IniFiles;

type

  TRedisConf = record
    Host: string;
    Port: Integer;
    Password: string;
    SSL: Boolean;
    Enabled: Boolean;
    constructor Create(const AHost: string; const APort: Integer; const APassword: string; const ASSL: Boolean; const AEnabled: Boolean); overload;
    constructor Create(const AIni: TIniFile; const ASectionSuffix: string = ''); overload;
  end;

implementation

{ TRedisConf }

constructor TRedisConf.Create(const AHost: string; const APort: Integer; const APassword: string; const ASSL: Boolean; const AEnabled: Boolean);
begin
  Host := AHost;
  Port := APort;
  Password := APassword;
  SSL := ASSL;
  Enabled := AEnabled;
end;

constructor TRedisConf.Create(const AIni: TIniFile; const ASectionSuffix: string = '');
var
  SectionName: string;
begin
  SectionName := 'redis';
  if ASectionSuffix <> '' then
    SectionName := SectionName + '.' + ASectionSuffix;

  Host := AIni.ReadString(SectionName, 'host', '127.0.0.1');
  Port := AIni.ReadInteger(SectionName, 'port', 6378);
  Password := AIni.ReadString(SectionName, 'password', '');
  SSL := AIni.ReadBool(SectionName, 'ssl', False);
  Enabled := AIni.ReadBool(SectionName, 'enabled', True);
end;

end.
