unit PingResult;

interface

  uses
    Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
    System.Classes, Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs,
    Vcl.AppEvnts, Vcl.StdCtrls, Vcl.Buttons, System.Json, Data.DB, Vcl.ExtCtrls,
    Vcl.ComCtrls, Vcl.Grids, Vcl.DBGrids, PingTask, ThreadPool;

  type
    TPingResult = class(TForm)
      LogMemo: TMemo;
      DBGrid1: TDBGrid;
      PingProgressBar: TProgressBar;
      ComponentHolder: TPanel;
      BtnStartPing: TBitBtn;
      GridPanel1: TGridPanel;
      BtnReadConf: TBitBtn;
      BtnStop: TBitBtn;
      BitBtn3: TBitBtn;
      Label1: TLabel;
      procedure BtnStartPingClick(Sender: TObject);
      procedure FormCreate(Sender: TObject);
      procedure FormDestroy(Sender: TObject);
      procedure BtnStopClick(Sender: TObject);
    private
      procedure TaskStart(Sender: TObject);
      procedure TaskCanceled(Sender: TObject);
      procedure TaskDone(Sender: TObject);
      procedure TasksStatus(Sender: TObject; Progress: Single);
    public
      { Public declarations }
    end;

  var
    PingResultForm: TPingResult;
    SSConf        : TJSONObject;
    // PingTask: TPingTask;

implementation

{$R *.dfm}

  procedure PingPoolInit(Manager: TPoolManager);
    begin
      if Assigned(PingResultForm) then
        begin
          Manager.ConcurrentWorkersCount := 8;
          Manager.SpareWorkersCount      := 0;
          if not Manager.RestoreOwners then
            Manager.RegisterOwner(PingResultForm, PingResultForm.TasksStatus,
              PingResultForm.TaskDone);
        end;
    end;

  procedure TPingResult.BtnStopClick(Sender: TObject);
    begin
      TPingManager.Singleton.CancelTasksByOwner(Self);
    end;

  procedure TPingResult.FormCreate(Sender: TObject);
    begin
      { Activate the "Demand Mode" of the manager }
      TPingManager.RegisterSingletonOnDemandProc(PingPoolInit);
    end;

  procedure TPingResult.FormDestroy(Sender: TObject);
    begin
      TPoolManager.DispatchOwnerDestroyed(Self);
      TPingManager.UnregisterSingletonOnDemandProc;
      TPoolManager.TerminateSingletonInstances;
    end;

  procedure TPingResult.BtnStartPingClick(Sender: TObject);
    var
      JsonFile   : TStringList;
      PingTask   : TPingTask;
      SSIndex    : Cardinal;
      SSConfSize : Cardinal;
      SSConfigs  : TJSONArray;
      SSConfValue: TJSONObject;
      Domain     : String;
    begin
      JsonFile := TStringList.Create;
      JsonFile.LoadFromFile('.\gui-config.json');
      // LogMemo.Clear;
      SSConf := nil;

      PingTask          := TPingTask.Create(Self);
      PingTask.OnStart  := PingResultForm.TaskStart;
      PingTask.OnCancel := PingResultForm.TaskCanceled;
      PingTask.OnDone   := PingResultForm.TaskDone;

      SSConf      := TJSONObject.ParseJSONValue(JsonFile.Text) as TJSONObject;
      SSConfSize  := TJSONArray(SSConf.Pairs[0].JSONValue).Count;
      SSConfigs   := TJSONArray(SSConf.Pairs[0].JSONValue);
      for SSIndex := 0 to SSConfSize - 1 do
        begin
          SSConfValue := SSConfigs.Items[0] as TJSONObject;
          Domain      := SSConfValue.Get('server').JSONValue.ToString;
          PingTask.TargetAddress := Copy(Domain, 2, System.Length(Domain) - 2);
          PingTask.SuccIndicator := False;
          PingTask.Len           := 5;
          TPingManager.Singleton.AddTask(PingTask);
          PingTask := TPingTask(PingTask.Clone);
        end;
    end;

  procedure TPingResult.TaskStart(Sender: TObject);
    var
      Task    : TPingTask;
      ThreadID: Cardinal;
    begin
      if not(Assigned(Sender) and (Sender is TPingTask)) then
        Exit;
      Task := TPingTask(Sender);
      if Assigned(Task.Owner) and (Task.Owner is TPoolWorker) then
        ThreadID := TPoolWorker(Task.Owner).ThreadID
      else
        ThreadID := 0;
      // showmessage(Task.TargetAddress);
      // LogMemo.Lines.Add(Format('Ping Task started for %s with thread #%d ...',
      // [Task.TargetAddress, ThreadID]));
      // LogMemo.Lines.Add('----');
    end;

  procedure TPingResult.TaskCanceled(Sender: TObject);
    var
      Task: TPingTask;
    begin
      if not(Assigned(Sender) and (Sender is TPingTask)) then
        Exit;
      Task := TPingTask(Sender);
      LogMemo.Lines.Add(Format('Task canceled for %s.', [Task.TargetAddress]));
    end;

  procedure TPingResult.TaskDone(Sender: TObject);
    var
      Task    : TPingTask;
      ThreadID: Cardinal;
    begin
      if not(Assigned(Sender) and (Sender is TPingTask)) then
        Exit;
      Task := TPingTask(Sender);
      if Assigned(Task.Owner) and (Task.Owner is TPoolWorker) then
        ThreadID := TPoolWorker(Task.Owner).ThreadID
      else
        ThreadID := 0;
        {
        LogMemo.Lines.Add
        (Format('Task done for %s by thread #%d and the results are:',
        [Task.TargetAddress, ThreadID]));

        LogMemo.Lines.Add(Format('Max: %f,Min: %f,Mean: %f',
        [Task.MaxDelay, Task.MinDelay, Task.MeanDelay]));

        LogMemo.Lines.Add('----');  }
      Showmessage
        (Format('Task done for %s by thread #%d and the results are:Max: %f,Min: %f,Mean: %f',
          [Task.TargetAddress, ThreadID, Task.MaxDelay, Task.MinDelay,
            Task.MeanDelay]));
    end;

  procedure TPingResult.TasksStatus(Sender: TObject; Progress: Single);
    begin
      PingProgressBar.Max      := 100;
      PingProgressBar.Position := Round(Progress * 100);
    end;

  {
    procedure TPingResult.TerminatePrimeManagerButtonClick(Sender: TObject);
    begin
    if TPrimeManager.HasSingleton then
    TPrimeManager.Singleton.Terminate;
    end;
  }
end.
