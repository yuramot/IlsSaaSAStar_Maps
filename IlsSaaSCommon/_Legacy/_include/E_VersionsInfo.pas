unit E_VersionsInfo;

interface

type
  TVersionType = (vtApplication, vtDatabase, vtMap);
  TVersions = array[TVersionType] of string;

const
  CVersionTypeNames: TVersions =
  (
    '������ ���������: ',
    '������ ���� ������: ',
    '������ �����: '
  );

var
  GVersionsInfo: TVersions =
  (
    '��� ������',
    '��� ������',
    '��� ������'
  );

implementation

end.
