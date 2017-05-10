program CubeUtils;

uses
  Forms,
  mainForm in 'mainForm.pas' {MainForm},
  FrameDlProgress in 'DownloadConf\FrameDlProgress.pas' {FrameDlProcessing: TFrame},
  LoginThread in 'Thread\LoginThread.pas',
  pingMod in 'PingModule\pingMod.pas',
  iphlpapi in 'PingModule\iphlpapi.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TMainForm_var, MainForm_var);
  Application.Run;
end.
