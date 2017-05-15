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
        FTarget: String;
        FSucc: Boolean;
        FLen: Cardinal; // Array length
        { Output related }
      protected
        FResult: TDynamicSingleArray; // in millisecond

        function ResultMean: Single;
        function ResultMax: Single;
        function ResultMin: Single;
        { Overriden stuff }
      protected
        procedure Assign(Source: TPoolTask); override;
        function IsTheSame(Compare: TPoolTask): Boolean; override;
        { Public input }
      public
        property TargetAddress: string read FTarget write FTarget;
        property SuccIndicator: Boolean read FSucc write FSucc;
        property Len: Cardinal read FLen write FLen;
        { Public output }
      public
        property MeanDelay: Single read ResultMean;
        property MaxDelay: Single read ResultMax;
        property MinDelay: Single read ResultMin;
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
    i: Cardinal;
    CountNotZero: Cardinal;
    Sum: Single;
  begin
    Sum := 0; // Sum initialization
    CountNotZero := 0; // Counter initialization
    for i := 1 to Len do
    begin
      Sum := Sum + FResult[i];
      if FResult[i] <> 0 then
        Inc(CountNotZero);
    end;
    Result := Sum / CountNotZero;
  end;

  function TPingTask.ResultMax: Single;
  var
    i: Cardinal;
    Maximum: Single;
  begin
    Maximum := 0; // Max value initialization
    for i := 1 to Len do
    begin
      if FResult[i] > Maximum then
        Maximum := FResult[i];
    end;
    Result:=Maximum;
  end;

  function TPingTask.ResultMin: Single;
  var
    i: Cardinal;
    Minimum: Single;
  begin
    Minimum := 100000; // Max value initialization
    for i := 1 to Len do
    begin
      if (FResult[i] < Minimum) and (FResult[i]<>0) then
        Minimum := FResult[i];
    end;
    Result:=Minimum;
  end;

  { **
    * Assign only data which is required for do the job (no calculated data)
    * }
  procedure TPingTask.Assign(Source: TPoolTask);
  var
    PT: TPingTask;
  begin
    inherited Assign(Source);
    PT := TPingTask(Source);
    TargetAddress := PT.TargetAddress;
    SuccIndicator := PT.SuccIndicator;
    Len := PT.Len;
  end;

  function TPingTask.IsTheSame(Compare: TPoolTask): Boolean;
  var
    PT: TPingTask;
  begin
    Result := Compare is TPingTask;
    if not Result then
      Exit;
    PT := TPingTask(Compare);
    Result := (PT.TargetAddress = TargetAddress) and (PT.Len = Len) and
      (PT.SuccIndicator = SuccIndicator);
  end;

  { ** TPrimeWorker ** }

  procedure TPingWorker.ExecuteTask;
  var
    cc: Cardinal;
    { **
      * Local representation of the property Task, because it's expensive to get it often.
      * }
    Task: TPingTask;
  begin
    Task := TPingTask(ContextTask);
    cc := 1;
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
