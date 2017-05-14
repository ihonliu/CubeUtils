unit LoginThread;

interface

uses
  System.Classes, Winapi.Windows, SysUtils, IdHTTP;

type
  TLoginThread = class(TThread)
  private
    Femail: string;
    Fpassword: string;
    Fformhash: string;
    FSiteAddress: string;
    FResult: TStringList;
    FSucc: boolean;
   // procedure DebugOutput;
    procedure SetResult(HttpResult: TStringList);
    procedure SetSucc(Flag: boolean);
  protected
    procedure Execute; override;
  public
    procedure setLoginInfo(email: string; password: string; formhash: string;
      SiteAddress: string);
    constructor Create(email: string; password: string; formhash: string;
      SiteAddress: string);
    property Succ: boolean read FSucc write SetSucc;
    property Result: TStringList read FResult write SetResult;
    function GetSSConf(const email: string; const password: string;
      const formhash: string; const SiteAddress: string): string;
  end;

implementation

function TLoginThread.GetSSConf(const email: string; const password: string;
  const formhash: string; const SiteAddress: string): string;
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
        Request.Add('email=' + email);
        Request.Add('password=' + password);
        Request.Add('formhash=' + formhash);
        Request.Add('task=login');
        IdHTTP := TIdHTTP.Create;
        try
          IdHTTP.AllowCookies := True;
          IdHTTP.HandleRedirects := True;
          IdHTTP.Request.ContentType := 'application/x-www-form-urlencoded';
          IdHTTP.Request.UserAgent :=
            'Mozilla/5.0 (Windows NT 6.1; WOW64; rv:12.0) Gecko/20100101 Firefox/12.0';
          IdHTTP.Post(SiteAddress, Request, Response);
          Result := IdHTTP.Get(SiteAddress + '/?act=nodes_export');
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
      Synchronize(
        procedure
        begin
          MessageBox(self.Handle, PChar(E.Message), PChar(' ß∞‹'), MB_OK);
        end);
  end;
end;

procedure TLoginThread.setLoginInfo(email: string; password: string;
formhash: string; SiteAddress: string);
begin
  Femail := email;
  Fpassword := password;
end;

constructor TLoginThread.Create(email: string; password: string;
formhash: string; SiteAddress: string);
begin
  inherited Create(True);
  Femail := email;
  Fpassword := password;
  Fformhash := formhash;
  FSiteAddress := SiteAddress;
  FSucc := False;
  FResult := TStringList.Create;
end;
{
procedure TLoginThread.DebugOutput;
begin
  MessageBox(self.Handle, PChar('email:' + Femail + #13#10 + 'pwd:' + Fpassword
    + #13#10), PChar('Debug'), MB_OK);
end;
 }
procedure TLoginThread.Execute;
var
  HttpResult: TStringList;
begin
  FreeOnTerminate := True;
  HttpResult := TStringList.Create;
  HttpResult.Add(GetSSConf(Femail, Fpassword, Fformhash, FSiteAddress));
  try
    try
      if Pos('<!DOCTYPE html>', HttpResult.Text) = 0 then
      begin
        { pass result }
        Result := HttpResult;
        { Synchronize(
          procedure
          begin
          MessageBox(self.Handle,PChar('µ«¬Ω≥…π¶'),
          Pchar('≥…π¶'),MB_OK);
          end); }
        Succ := True; // True For success
      end
      else
      begin
        { Synchronize(
          procedure
          begin
          MessageBox(self.Handle,PChar('µ«¬Ω ß∞‹,«ÎºÏ≤È’À∫≈∫Õ√‹¬Î «∑Ò’˝»∑'),
          Pchar(' ß∞‹'),MB_OK);
          end);
          Succ:=False; }
      end;
    except
      on E: Exception do
      begin
        Synchronize(
          procedure
          begin
            MessageBox(self.Handle, PChar(E.Message), PChar(' ß∞‹'), MB_OK);
          end);
        Succ := False;
      end;
    end;
  finally
    HttpResult.Free;
  end;
  Terminate;
end;

procedure TLoginThread.SetSucc(Flag: boolean);
begin
  FSucc := Flag;
end;

procedure TLoginThread.SetResult(HttpResult: TStringList);
begin
  FResult.Add(HttpResult.Text);
end;

end.
