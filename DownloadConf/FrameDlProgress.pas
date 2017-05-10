unit FrameDlProgress;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ComCtrls,
  Vcl.ExtCtrls, IdHTTP;

type
  TFrameDlProgress = class(TFrame)
    DlProgressBar: TProgressBar;
    GrDlProgress: TGroupBox;
    Panel1: TPanel;
    procedure setProgressBar(Progress:integer);
  private
    { Private declarations }
    procedure CMRelease(var Message: TMessage); message CM_RELEASE;
  public
    { Public declarations }
    procedure Release;
  end;
function GetSSConf(
  const email: string;
  const password: string;
  const formhash: string;
  const SiteAddress:string): string;

implementation

{$R *.dfm}
procedure TFrameDlProgress.setProgressBar(Progress:integer);
begin
  DlProgressBar.Min:=0;
  DlProgressBar.Max:=100;
  DlProgressBar.Position:=Progress;
end;

procedure TFrameDlProgress.CMRelease(var Message: TMessage);
begin
  Free;
end;

procedure TFrameDlProgress.Release;
begin
  PostMessage(Handle, CM_RELEASE, 0, 0);
end;



end.
