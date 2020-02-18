unit F_FolderSelect;
//------------------------------------------------------------------------------

//------------------------------------------------------------------------------
interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
  System.Classes, Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs,Vcl.Menus,
  Vcl.ComCtrls, Vcl.StdCtrls, Winapi.ShlObj, cxGraphics, cxLookAndFeels,
  cxLookAndFeelPainters, dxSkinsCore, dxSkinsDefaultPainters, cxShellCommon,
  cxControls, cxContainer, cxEdit, cxShellControls, cxShellTreeView, cxButtons;

//------------------------------------------------------------------------------
type
  TFormFolderSelect = class(TForm)
    bOk: TcxButton;
    bCancel: TcxButton;
    shtwFolder: TcxShellTreeView;
    procedure bOkClick(Sender: TObject);
    procedure bCancelClick(Sender: TObject);
  public
    //! Выбранная папка (путь)
    SelectedPath: string;
  end;

function UserSelectFolder(): string;

//------------------------------------------------------------------------------
implementation

{$R *.dfm}

//------------------------------------------------------------------------------
procedure TFormFolderSelect.bOkClick(Sender: TObject);
begin
  SelectedPath := shtwFolder.Path;
  ModalResult := mrOk;
end;

procedure TFormFolderSelect.bCancelClick(Sender: TObject);
begin
  ModalResult := mrCancel;
end;

//------------------------------------------------------------------------------
function UserSelectFolder(): string;
var
  LForm: TFormFolderSelect;
begin
  LForm := TFormFolderSelect.Create(nil);
  try
    LForm.ShowModal;
    Result := LForm.SelectedPath;
  finally
    LForm.Free;
  end;
end;

end.

