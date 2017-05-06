program CubeUtils;

uses
  Forms,
  MainForm in 'MainForm.pas' {MainForm};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm_var);
  Application.Run;
end.
