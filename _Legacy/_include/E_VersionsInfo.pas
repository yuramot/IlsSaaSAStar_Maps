unit E_VersionsInfo;

interface

type
  TVersionType = (vtApplication, vtDatabase, vtMap);
  TVersions = array[TVersionType] of string;

const
  CVersionTypeNames: TVersions =
  (
    'Версия программы: ',
    'Версия базы данных: ',
    'Версия карты: '
  );

var
  GVersionsInfo: TVersions =
  (
    'нет данных',
    'нет данных',
    'нет данных'
  );

implementation

end.
