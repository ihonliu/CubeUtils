unit pingThread;

interface

  uses
    System.Classes, Winapi.Windows, SysUtils, pingMod, Math, Json, Winsock;

  type
    TDynamicSingleArray = array of Single;

    TPingThread = class(TThread)
    private
      FIP    : String;
      FSucc  : boolean;
      FResult: TDynamicSingleArray; // in millisecond
      Len    : shortint;            // Array length
      // procedure DebugOutput;
      // procedure SetResult(Delay: integer);
      function ResultMean: Single;
      function ResultMax: Single;
      function ResultMin: Single;
    protected
      procedure Execute; override;
    public
      { constructor Create(const IP: String; Length: shortint);
        reintroduce; overload; }
      { constructor Create(SSConfObject: TJsonObject; Length: shortint); }
      { reintroduce; overload; }
      property IP       : String read FIP write FIP;
      property Succ     : boolean read FSucc write FSucc;
      property MeanDelay: Single read ResultMean;
      property MaxDelay : Single read ResultMax;
      property MinDelay : Single read ResultMin;
    end;


implementation

  procedure TranslateStringToTInAddr(AIP: string; var AInAddr);
    var
      phe      : PHostEnt;
      pac      : PAnsiChar;
      GInitData: TWSAData;
    begin
      WSAStartup($101, GInitData);
      try
        phe := GetHostByName(PAnsiChar(AnsiString(AIP)));
        if Assigned(phe) then
          begin
            pac := phe^.h_addr_list^;
            // showmessage('OK');
            if Assigned(pac) then
              begin
                with TIPAddr(AInAddr).S_un_b do
                  begin
                    s_b1 := Byte(pac[0]);
                    s_b2 := Byte(pac[1]);
                    s_b3 := Byte(pac[2]);
                    s_b4 := Byte(pac[3]);
                  end;
              end
            else
              begin
                raise Exception.Create('Error getting IP from HostName');
              end;
          end
        else
          begin
            raise Exception.Create('Error getting HostName');
          end;
      except
        FillChar(AInAddr, SizeOf(AInAddr), #0);
      end;
      WSACleanup;
    end;
  { constructor TPingThread.Create(const IP: string; Length: shortint);
    begin
    inherited Create(True);
    FIP  := IP;
    Succ := False;
    Len  := Length;
    SetLength(FResult, Len);
    end;
  }
    constructor TPingThread.Create(SSConfObject: TJsonObject; Length: shortint);
    var IP:string;
    begin
    inherited Create(True);
    TranslateStringToTInAddr(SSConfObject.Get('server').JSONValue.ToString,IP);
    FIP  := IP;
    Succ := False;
    Len  := Length;
    SetLength(FResult, Len);
    end;

  {
    procedure TPingThread.SetResult(Delay: integer);
    begin
    FResult := Delay;
    end;
  }

  function TPingThread.ResultMean: Single;
    begin
      Result := Mean(FResult);
    end;

  function TPingThread.ResultMax: Single;
    begin
      Result := MaxValue(FResult);
    end;

  function TPingThread.ResultMin: Single;
    begin
      Result := MinValue(FResult);
    end;

  (*
    procedure TPingThread.DebugOutput;
    begin
    { Put some code here }
    end;
  *)
  procedure TPingThread.Execute;
    var
      i: integer;
    begin
      FreeOnTerminate := True;
      for i           := 1 to Len do
        FResult[i]    := pingMod.Ping(IP);
      Terminate;
    end;

initialization

  DNS             := TIdDNSResolver.Create(nil);
  DNS.WaitingTime := 10;
  DNS.Host        := '119.29.29.29';
  DNS.QueryType   := [qtA];

end.
