program TestTrackProcessor;

uses
  FastMM4, Vcl.Forms,
  UMainForm in 'UMainForm.pas' {MainForm};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.
