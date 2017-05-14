{
  This program is used to get gui-congfig.json used in Shadowsocks(R) from site
  on http://www.ss3cube.com.
}

unit MainForm;

interface

  uses
    Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
    System.Classes, Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs,
    IdBaseComponent, IdComponent, IdTCPConnection, IdTCPClient, IdHTTP,
    IdCookie,
    Vcl.StdCtrls, Vcl.Buttons, Vcl.ExtCtrls, Vcl.WinXCtrls, Vcl.ComCtrls,
    LoginThread, PingResult;

  type
    TMainForm = class(TForm)
      InfoLabel: TLabel;
      EmailEdit: TLabeledEdit;
      PasswordEdit: TLabeledEdit;
      LoginBtn: TBitBtn; // Login button
      ExitBtn: TBitBtn;
      NameLabel: TLabel;
      ActivityIndicator: TActivityIndicator;
      GridPanel: TGridPanel;
      SaveConf: TBitBtn;
      TestPing: TBitBtn;
      SaveLink: TBitBtn;
      ShowQR: TBitBtn;
      procedure LoginBtnClick(Sender: TObject);
      procedure ExitBtnClick(Sender: TObject);
      procedure DoEnterAsTab(var Msg: TMsg; var Handled: Boolean);
      procedure OnCreate(Sender: TObject);
      procedure OnThreadTerminate(Sender: TObject);
      procedure SaveConfClick(Sender: TObject);
      procedure TestPingClick(Sender: TObject);
      procedure SaveLinkClick(Sender: TObject);
      procedure ShowQRClick(Sender: TObject);
      private
        { Private declarations }
      public
        { Public declarations }
    end;

  var
    MainForm_var: TMainForm;
    SiteAddress: string; // used to specify the site address
    email: string; // username for logging in
    password: string; // password for logging in
    version: string; // define program version
    formhash: string; // used to store formhash
    Filepath: string;
    LoginThread: TLoginThread;
    TimeInterval: integer;
    LoginResult: TStringList;

implementation

{$R *.dfm}

  procedure GetFormHash(var formhash: string; const SiteAddress: string);
  // Get web form hash and store it in global variable formhash
  var
    IdHTTP: TIdHTTP;
    PageResult: TStringList;
    Position: integer;
    StringPattern: string;
  begin
    StringPattern := 'name="formhash" value="';
    try
      PageResult := TStringList.Create;
      try
        IdHTTP := TIdHTTP.Create;
        try
          PageResult.Add(IdHTTP.Get(SiteAddress));
          Position := pos(StringPattern, PageResult.text);
          formhash := Copy(PageResult.text,
            Position + Length(StringPattern), 8);
        finally
          IdHTTP.Free;
        end;
      finally
        PageResult.Free;
      end;
    except
      on E: Exception do
        Showmessage(E.Message);
    end;
  end;

  procedure TMainForm.DoEnterAsTab(var Msg: TMsg; var Handled: Boolean);
  // make enter performs as tab in edit component
  begin
    if Msg.Message = WM_KEYDOWN then
    begin
      if not((Screen.ActiveControl is TCustomMemo) or
          (Screen.ActiveControl is TButtonControl)) then
      begin
        if Msg.wParam = VK_RETURN then
          Screen.ActiveForm.Perform(WM_NextDlgCtl, 0, 0);
      end;
    end;
  end;

  procedure TMainForm.OnCreate(Sender: TObject);
  { Server state check and initialize NameLabel }
  begin
    NameLabel.Caption := 'CubeUtilsD ' + version;
    Application.OnMessage := DoEnterAsTab;
    (*GetFormHash(formhash, SiteAddress);
    if formhash = '' then
    begin
      { display err message and close Program }
      MessageBox(self.Handle, PChar('服务器连接失败'), PChar('连接失败'), MB_OK);
      Application.Terminate;
      Exit;
    end;*)
  end;

  procedure SaveFile(Result: TStringList; const Filepath: string);
  var
    SuccessStr: string; // Save successfully prompt string
  begin
    Result.SaveToFile(Filepath + '\gui-config.json');
    SuccessStr := '成功保存至' + GetCurrentDir + '\gui-config.json';
    MessageBox(MainForm_var.Handle, PChar(SuccessStr), PChar('保存成功'), MB_OK);
  end;

  procedure TMainForm.OnThreadTerminate(Sender: TObject);
  begin
    // if LoginThread.Finished then
    // begin
    if LoginThread.Succ then
    begin
      LoginResult := TStringList.Create;
      MessageBox(MainForm_var.Handle, PChar('登陆成功'), PChar('成功'), MB_OK);
      // SaveFile(LoginThread.Result, Filepath);
      LoginResult.Add(LoginThread.Result.text);
      // LoginThread.Free;
      ActivityIndicator.Animate := False;
      ActivityIndicator.Visible := False;
      GridPanel.Visible := True;
      LoginBtn.Visible := False;
      // Open more option's Form
    end
    else
      MessageBox(self.Handle, PChar('登陆失败,请检查账号和密码是否正确'), PChar('失败'), MB_OK);
  end;

  procedure TMainForm.SaveConfClick(Sender: TObject);
  begin
    SaveFile(LoginResult, Filepath);
  end;

  procedure TMainForm.SaveLinkClick(Sender: TObject);
  begin
    Showmessage('敬请期待');
  end;

  procedure TMainForm.ShowQRClick(Sender: TObject);
  begin
    Showmessage('敬请期待');
  end;

  procedure TMainForm.TestPingClick(Sender: TObject);
  begin
    with TPingResult.Create(nil) do
      try
        ShowModal;
      finally
        Free;
      end;
  end;

  procedure TMainForm.LoginBtnClick(Sender: TObject);
  // download configuration
  var
    TimeCountLogin: integer;
    TimeLimitLogin: integer;
  begin
    TimeLimitLogin := 40;
    email := EmailEdit.text;
    password := PasswordEdit.text;
    if (email = '') and (password = '') then
      MessageBox(self.Handle, PChar('请输入用户名与密码'), PChar('错误'), MB_OK)
    else
    begin
      if formhash <> '' then
      begin
        MessageBox(self.Handle, PChar('即将登陆,登陆时间长度可能有所不一'), PChar('注意'), MB_OK);
        ActivityIndicator.Visible := True;
        ActivityIndicator.Animate := True;
        // FrameDlProgressShow;
        LoginThread := TLoginThread.Create(email, password, formhash,
          SiteAddress);
        LoginThread.Start;
        LoginThread.OnTerminate := OnThreadTerminate;
      end
      else
        MessageBox(self.Handle, PChar('服务器连接失败'), PChar('连接失败'), MB_OK);
    end;
    // FrameDlProgress_var.Release;
  end;

  procedure FormClose(Sender: TObject; var Action: TCloseAction);
  // close form
  begin
    Action := Cafree;
    MainForm_var := nil;
  end;

  procedure TMainForm.ExitBtnClick(Sender: TObject);
  begin
    close;
  end;

initialization

  SiteAddress := 'http://cube-ss.com';
  email := '';
  password := '';
  version := 'V1.3';
  Filepath := '.';
  TimeInterval := 500;

  // TimeLimitLogin:=40;
end.
