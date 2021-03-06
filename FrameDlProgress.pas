unit FrameDlProgress;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ComCtrls,
  Vcl.ExtCtrls;

type
  TFrameDlProgress = class(TFrame)
    DlProgressBar: TProgressBar;
    GrDlProgress: TGroupBox;
    Panel1: TPanel;
  private
    { Private declarations }
    procedure CMRelease(var Message: TMessage); message CM_RELEASE;
  public
    { Public declarations }
    procedure Release;
  end;

implementation

{$R *.dfm}

procedure TFrameDlProgress.CMRelease(var Message: TMessage);
begin
  Free;
end;

procedure TFrameDlProgress.Release;
begin
  PostMessage(Handle, CM_RELEASE, 0, 0);
end;

end.
