unit PingTask;

interface

  uses
    System.Classes, Winapi.Windows, System.SysUtils, pingMod, Json,
    Winsock, vcl.dialogs, ThreadPool;

  type
    TDynamicSingleArray = array of Single;

    TPingTask = class(TPoolTask)
      { Input related }
    protected
      FSuccCount: Cardinal;
      FLen      : Cardinal; // Array length
      FOrder    : Cardinal;
      FSSConf   : TJSonObject;
      FOwner    : TObject;
      { Output related }
    protected
      FResult: TDynamicSingleArray; // in millisecond

      function ResultMean: Single;
      function ResultMax: Single;
      function ResultMin: Single;
      function GetAddress: String;
      function GetSuccRate: Single;
      function GetNodeName: String;
      { Overriden stuff }
    protected
      procedure Assign(Source: TPoolTask); override;
      function IsTheSame(Compare: TPoolTask): Boolean; override;
      { Public input }
    public
      property TargetAddress: string read GetAddress;
      property SuccCount    : Cardinal read FSuccCount write FSuccCount;
      property Len          : Cardinal read FLen write FLen;
      property Order        : Cardinal read FOrder write FOrder;
      property SSConf       : TJSonObject read FSSConf write FSSConf;
      property Owner        : TObject read FOwner write FOwner;
      { Public output }
    public
      property MeanDelay: Single read ResultMean;
      property MaxDelay : Single read ResultMax;
      property MinDelay : Single read ResultMin;
      property SuccRate : Single read GetSuccRate;
      property NodeName : String read GetNodeName;
    end;

    TPingWorker = class(TPoolWorker)
    protected
      procedure ExecuteTask; override;
    end;

    TPingManager = class(TPoolManager)
    protected
      class function WorkerClass: TPoolWorkerClass; override;
    public
      class function Singleton: TPingManager; reintroduce;
    end;

implementation

  function TPingTask.ResultMean: Single;
    var
      i           : Cardinal;
      CountNotZero: Cardinal;
      Sum         : Single;
    begin
      Sum := 0; // Sum initialization
      // SuccCount := 0; // Counter initialization
      CountNotZero := 0;
      for i        := 1 to Len do
        begin
          Sum := Sum + FResult[i];
          if FResult[i] <> 0 then
            Inc(CountNotZero);
        end;
      SuccCount := CountNotZero;
      if CountNotZero <> 0 then
        Result := Sum / CountNotZero
      else
        Result := 0;

    end;

  function TPingTask.ResultMax: Single;
    var
      i      : Cardinal;
      Maximum: Single;
    begin
      Maximum := 0; // Max value initialization
      for i   := 1 to Len do
        begin
          if FResult[i] > Maximum then
            Maximum := FResult[i];
        end;
      Result := Maximum;
    end;

  function TPingTask.ResultMin: Single;
    var
      i      : Cardinal;
      Minimum: Single;
    begin
      Minimum := 100000; // Max value initialization
      for i   := 1 to Len do
        begin
          if (FResult[i] < Minimum) and (FResult[i] > 0) then
            Minimum := FResult[i];
        end;
      Result := Minimum;
    end;

  function TPingTask.GetSuccRate: Single;
    var
      i           : Cardinal;
      CountNotZero: Cardinal;
    begin
      SuccCount    := 0; // Counter initialization
      CountNotZero := 0;
      for i        := 1 to Len do
        begin
          if FResult[i] <> 0 then
            Inc(CountNotZero);
        end;
      SuccCount := CountNotZero;
      Result    := CountNotZero / Len;
    end;

  function TPingTask.GetAddress: String;
    var
      Domain: String;
    begin
      Domain := FSSConf.Get('server').JSONValue.ToString;
      Result := Copy(Domain, 2, System.Length(Domain) - 2);
    end;

  function TPingTask.GetNodeName: String;
    var
      NodeName: String;
    begin
      NodeName := FSSConf.Get('remarks').JSONValue.ToString;
      Result   := Copy(NodeName, 2, System.Length(NodeName) - 2);
    end;

  { Assign only data which is required for do the job (no calculated data) }
  procedure TPingTask.Assign(Source: TPoolTask);
    var
      PT: TPingTask;
    begin
      inherited Assign(Source);
      PT        := TPingTask(Source);
      SSConf    := PT.SSConf;
      SuccCount := PT.SuccCount;
      Len       := PT.Len;
      Order     := PT.Order;
      Owner     := PT.Owner;
    end;

  function TPingTask.IsTheSame(Compare: TPoolTask): Boolean;
    var
      PT: TPingTask;
    begin
      Result := Compare is TPingTask;
      if not Result then
        Exit;
      PT     := TPingTask(Compare);
      Result := (PT.TargetAddress = TargetAddress) and (PT.Len = Len) and
        (PT.Order = Order);
    end;

  { ** TPrimeWorker ** }

  procedure TPingWorker.ExecuteTask;
    var
      cc: Cardinal;
      { Local representation of the property Task, because it's expensive to get it often. }
      Task: TPingTask;
    begin
      Task := TPingTask(ContextTask);
      cc   := 1;
      SetLength(Task.FResult, Task.Len);
      while not Canceled and (cc < Task.Len) do
        begin
          Task.FResult[cc] := pingMod.Ping(Task.TargetAddress);
          Inc(cc);
          Sleep(1000);
        end;

      DoneTask(True);
    end;

  { ** TPrimeManager ** }

  class function TPingManager.Singleton: TPingManager;
    begin
      Result := TPingManager(inherited Singleton);
    end;

  class function TPingManager.WorkerClass: TPoolWorkerClass;
    begin
      Result := TPingWorker;
    end;

end.
