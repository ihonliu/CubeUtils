{
This program is used to get gui-congfig.json used in Shadowsocks(R) from site
on http://www.ss3cube.com.
}

unit MainForm;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, IdBaseComponent, IdComponent,
  IdTCPConnection, IdTCPClient, IdHTTP, IdCookie, Vcl.StdCtrls, Vcl.Buttons,
  Vcl.ExtCtrls;

type
  TMainForm = class(TForm)
    InfoLabel: TLabel;
    EmailEdit: TLabeledEdit;
    PasswordEdit: TLabeledEdit;
    LoginBtn: TBitBtn;                              //Login button
    ExitBtn: TBitBtn;
    NameLabel: TLabel;                              //Show CubeUtils V*.*
    procedure LoginBtnClick(Sender: TObject);
    procedure ExitBtnClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);          //s
    procedure DoEnterAsTab(var Msg: TMsg; var Handled: Boolean);
    procedure NameLabelOnCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  MainForm_var: TMainForm;
  SiteAddress: string;      //used to specify the site address
  email:string;             //username for logging in
  password:string;          //password for logging in
  version:string;           //define program version

implementation

{$R *.dfm}

function Login(email: string;password: string;formhash: string): string;
var
  IdHTTP: TIdHTTP;
  Request: TStringList;
  Response: TMemoryStream;
begin
  Result := '';
  try
    Response := TMemoryStream.Create;
    try
      Request := TStringList.Create;
      try
        Request.Add('act=login');
        Request.Add('redirect=');
        Request.Add('email='+email);
        Request.Add('password='+password);
        Request.Add('formhash='+formhash);
        Request.Add('task=login');
        IdHTTP := TIdHTTP.Create;
        try
          IdHTTP.AllowCookies := True;
          IdHTTP.HandleRedirects := True;
          IdHTTP.Request.ContentType := 'application/x-www-form-urlencoded';
          IdHTTP.Request.UserAgent :=
          'Mozilla/5.0 (Windows NT 6.1; WOW64; rv:12.0) Gecko/20100101 Firefox/12.0';
          IdHTTP.Post(SiteAddress, Request, Response);
          Result := IdHTTP.Get(SiteAddress+'/?act=nodes_export');
        finally
          IdHTTP.Free;
        end;
      finally
        Request.Free;
      end;
    finally
      Response.Free;
    end;
  except
    on E: Exception do
      ShowMessage(E.Message);
  end;
end;

function GetFormHash:string;
//Get web form hash
var
  IdHTTP: TIdHTTP;
  PageResult: TStringlist;
  Position: Integer;
  StringPattern: string;
begin
  StringPattern := 'name="formhash" value="';
  try
    PageResult := TStringlist.Create;
    try
      IdHTTP := TIdHTTP.Create;
      try
        PageResult.Add(IdHTTP.Get( SiteAddress ));
        Position := pos(StringPattern, PageResult.text);
        Result := Copy(PageResult.Text, Position+Length(StringPattern),8);
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

procedure TMainForm.LoginBtnClick(Sender: TObject);
//download configuration
var
  HttpResult: TStringlist;
  formhash: string;                   //used to store formhash
  SuccessStr: string;                 //Save successfully prompt string
begin
  email := EmailEdit.Text;
  password := PasswordEdit.Text;
  if (email='') and (password='') then
    MessageBox(self.Handle,PChar('请输入用户名与密码'),PChar('错误'),MB_OK)
  else
  begin
    MessageBox(
      self.Handle,
      Pchar('即将下载配置，时间长度取决于你的网络情况，请耐心等待'),
      Pchar('即将开始下载'),MB_OK);
    formhash:=GetFormHash;
    if formhash<>'' then
    begin
      HttpResult := TStringlist.Create;
      HttpResult.Add(Login(email,password,formhash));
      try
        try
          if Pos('<!DOCTYPE html>',HttpResult.Text) = 0 then
          begin
            HttpResult.SaveToFile('.\gui-config.json');
            SuccessStr:= '成功保存至'+GetCurrentDir+'\gui-config.json';
            MessageBox(self.Handle,PChar(SuccessStr),PChar('保存成功'),MB_OK);
          end
          else
            MessageBox(self.Handle,PChar('登陆失败,请检查账号和密码是否正确'),
              Pchar('失败'),MB_OK);
        except
          on E: Exception do
            Showmessage(E.Message);
        end;
      finally
         HttpResult.Free;
      end;
      end
    else
      MessageBox(self.Handle,PChar('服务器连接失败'),PChar('连接失败'),MB_OK);
  end;
end;

procedure FormClose(Sender: TObject; var Action: TCloseAction);
//close form
begin
  action := Cafree;
  MainForm_var := nil;
end;

procedure TMainForm.ExitBtnClick(Sender: TObject);
begin
  close;
end;

procedure TMainFOrm.DoEnterAsTab(var Msg: TMsg; var Handled: Boolean);
//make enter performs as tab in edit component
begin
  if Msg.Message = WM_KEYDOWN then
  begin
    if  not((Screen.ActiveControl is TCustomMemo)
    or (Screen.ActiveControl is TButtonControl)) then
    begin
      if Msg.wParam = VK_RETURN then
        Screen.ActiveForm.Perform(WM_NextDlgCtl, 0, 0);
    end;
  end;
end;

procedure TMainForm.FormCreate(Sender: TObject);
begin
  // At the end of the method, add:
  Application.OnMessage := DoEnterAsTab;
end;


procedure TMainForm.NameLabelOnCreate(Sender: TObject);
//Software banner initialization
begin
  NameLabel.Caption:='CubeUtilsD '+version;
end;

initialization
SiteAddress:='http://cube-ss.com';
email:='';
password:='';
version:='V1.1';

end.
