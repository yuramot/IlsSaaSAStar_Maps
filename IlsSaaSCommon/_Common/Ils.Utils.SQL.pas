unit Ils.Utils.SQL;

interface

uses
  StrUtils;

function HasSqlStopWords(const ASql: string): Boolean;

implementation

function HasSqlStopWords(const ASql: string): Boolean;
var
  s: string;
const
  a: array [1..7] of string = ('insert', 'update', 'delete', 'drop', 'alter', 'grant', 'create');
begin
  Result := False;
  for s in a do
    if ContainsText(s, ASql) then
      Exit(True);
end;


end.
