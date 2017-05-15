program CubeUtils;

uses
  Forms,
  mainForm in 'mainForm.pas' {MainForm},
  LoginThread in 'Thread\LoginThread.pas',
  pingMod in 'PingModule\pingMod.pas',
  iphlpapi in 'PingModule\iphlpapi.pas',
  PingTask in 'Thread\PingTask.pas',
  PingResult in 'PingResult.pas' {PingResult},
  ThreadPool in 'Thread\ThreadPool.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm_var);
  // Application.CreateForm(TPingResult, PingResultForm);
  Application.Run;

end.
