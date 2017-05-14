program CubeUtils;

uses
  Forms,
  mainForm in 'mainForm.pas' {MainForm},
  LoginThread in 'Thread\LoginThread.pas',
  pingMod in 'PingModule\pingMod.pas',
  iphlpapi in 'PingModule\iphlpapi.pas',
  PingThread in 'Thread\PingThread.pas',
  PingResult in 'PingResult.pas' {PingResult};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm_var);
  // Application.CreateForm(TPingResult, PingResultForm);
  Application.Run;

end.
