program MapEditorApp;

uses
  Vcl.Forms,
  UMapEditorTestFm in 'UMapEditorTestFm.pas' {Form3},
  UMapEditor in 'UMapEditor.pas',
  Ils.Json.Names in '..\_Common\Ils.Json.Names.pas',
  Ils.Json.Utils in '..\_Common\Ils.Json.Utils.pas',
  JsonDataObjects in '..\..\lib\Json\JsonDataObjects.pas',
  IlsMapEditorWebModule in 'IlsMapEditorWebModule.pas' {wmMapEditor: TWebModule};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm3, Form3);
  Application.Run;
end.
