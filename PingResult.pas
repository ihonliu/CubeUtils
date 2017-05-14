unit PingResult;

interface

  uses
    Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
    System.Classes, Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs,
    Vcl.AppEvnts, Vcl.StdCtrls, Vcl.Buttons, System.Json, Data.DB, Vcl.ExtCtrls,
    Vcl.ComCtrls, Vcl.Grids, Vcl.DBGrids, PingThread;
  type
    TPingResult = class(TForm)
      BtnStartPing: TBitBtn;
      DBGrid1: TDBGrid;
      BtnReadConfigure: TBitBtn;
      ProgressBar1: TProgressBar;
      ComponentHolder: TPanel;
      Memo1: TMemo;
      procedure BtnStartPingClick(Sender: TObject);
      procedure ShowJson(JSONValue: TStringList);
      function GetIP(Domain: AnsiString): string;
      // procedure FormClose(Sender: TObject; var Action: TCloseAction);
    private
      { Private declarations }
    public
      { Public declarations }
    end;

  var
    // PingResultForm: TPingResult;
    SSConf    : TJSONObject;
    PingThread: TPingThread;
    IP        : String;
    DNSHost   : string;

implementation

{$R *.dfm}

  procedure OnThreadTerminate(Sender: TObject);
    begin
      { }

    end;

  procedure TPingResult.ShowJson(JSONValue: TStringList);
    var
      SSIndex    : integer;
      SSConfSize : integer;
      SSConfigs  : TJSONArray;
      SSConfValue: TJSONObject;
      Domain     : string;
    begin
      SSConf := nil;
      SSConf := TJSONObject.ParseJSONValue(JSONValue.text) as TJSONObject;
      { PingThread:=TPingThread.Create(IP,4);
        PingThread.Start;
        PingThread.OnTerminate := OnThreadTerminate; }
      SSConfSize  := TJSONArray(SSConf.Get(0).JSONValue).Size;
      SSConfigs   := TJSONArray(SSConf.Get(0).JSONValue);
      for SSIndex := 0 to SSConfSize - 1 do
        begin
          SSConfValue := SSConfigs.Get(SSIndex) as TJSONObject;
          Domain      := SSConfValue.Get('server').JSONValue.ToString;
          Domain      := copy(Domain, 2, Length(Domain) - 2);
          // Memo1.Lines.Clear;
        end;

    end;

  procedure TPingResult.BtnStartPingClick(Sender: TObject);
    var
      JsonFile: TStringList;
    begin
      { SSConf := TJSONObject.ParseJSONValue(JSONValue.text); }
      JsonFile := TStringList.Create;
      JsonFile.LoadFromFile('.\gui-config.json');
      ShowJson(JsonFile);
    end;

initialization

  IP := '127.0.0.1';
  { DNS             := TIdDNSResolver.Create(nil);
    DNS.WaitingTime := 6000; }
  DNSHost := '119.29.29.29';

end.
