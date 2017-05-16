unit PingResult;

interface

  uses
    Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
    System.Classes, Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs,
    Vcl.StdCtrls, Vcl.ExtCtrls, Vcl.ComCtrls, Vcl.Grids, PingTask,
    ThreadPool, System.Json, Vcl.Buttons, Data.DB, Vcl.DBGrids, Data.FMTBcd,
    Data.SqlExpr;

  type
    TPingResult = class(TForm)
      LogMemo: TMemo;
      PingProgressBar: TProgressBar;
      ComponentHolder: TPanel;
      BtnStartPing: TBitBtn;
      GridPanel1: TGridPanel;
      BtnReadConf: TBitBtn;
      BtnStop: TBitBtn;
      BitBtn3: TBitBtn;
      Label1: TLabel;
      StringGrid1: TStringGrid;
      DBGrid1: TDBGrid;
      SQLTable1: TSQLTable;
      procedure BtnStartPingClick(Sender: TObject);
      procedure FormCreate(Sender: TObject);
      procedure FormDestroy(Sender: TObject);
      procedure BtnStopClick(Sender: TObject);
      procedure BitBtn3Click(Sender: TObject);
      function GetThis: TObject;
    private const
      FieldNames: array [0 .. 5] of string = ('序号', '节点名称', '平均延迟', '最低延迟',
        '最高延迟', '成功率');

    public
      { Public declarations }
      property This: TObject read GetThis;
      procedure TaskStart(Sender: TObject);
      procedure TaskCanceled(Sender: TObject);
      procedure TaskDone(Sender: TObject);
      procedure TasksStatus(Sender: TObject; Progress: Single);
    end;

  var
    PingResultForm: TPingResult;
    SSConf        : TJSONObject;

implementation

{$R *.dfm}

  function TPingResult.GetThis: TObject;
    begin
      Result := Self;
    end;

  procedure PingPoolInit(Manager: TPoolManager);
    begin
      if Assigned(PingResultForm) then
        begin
          Manager.ConcurrentWorkersCount := 8;
          Manager.SpareWorkersCount      := 0;
          if not Manager.RestoreOwners then
            begin
              Manager.RegisterOwner(PingResultForm, PingResultForm.TasksStatus,
                PingResultForm.TaskDone);
            end;
        end;
    end;

  procedure TPingResult.BtnStopClick(Sender: TObject);
    begin
      TPingManager.Singleton.CancelTasksByOwner(Self);
    end;

  procedure TPingResult.FormCreate(Sender: TObject);
    var
      ColNum: Cardinal;
    begin
      { Activate the "Demand Mode" of the manager }
      TPingManager.RegisterSingletonOnDemandProc(PingPoolInit);
      // DBGrid1.Fields.Clear;
      for ColNum := 0 to High(FieldNames) do
        begin
          StringGrid1.Cells[ColNum, 0] := FieldNames[ColNum];
          SQLTable1.FieldDefs.Add(FieldNames[ColNum], String);
        end;
      SQLTable1.Open;
    end;

  procedure TPingResult.FormDestroy(Sender: TObject);
    begin
      TPoolManager.DispatchOwnerDestroyed(Self);
      TPingManager.UnregisterSingletonOnDemandProc;
      TPoolManager.TerminateSingletonInstances;
    end;

  procedure TPingResult.BitBtn3Click(Sender: TObject);
    begin
      if TPingManager.HasSingleton then
        TPingManager.Singleton.Terminate;
    end;

  procedure TPingResult.TaskStart(Sender: TObject);
    // var
    // Task: TPingTask;
    // ThreadID: Cardinal;
    begin
      if not(Assigned(Sender) and (Sender is TPingTask)) then
        Exit;
      // Task := TPingTask(Sender);
      { if Assigned(Task.Owner) and (Task.Owner is TPoolWorker) then
        ThreadID := TPoolWorker(Task.Owner).ThreadID
        else
        ThreadID := 0; }
      // showmessage(Task.TargetAddress);
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
      Task      : TPingTask;
      FormResult: TPingResult;
      ThreadID  : Cardinal;
    begin
      if not(Assigned(Sender) and (Sender is TPingTask)) then
        Exit;
      Task := TPingTask(Sender);
      if Assigned(Task.Owner) and (Task.Owner is TPoolWorker) then
        ThreadID := TPoolWorker(Task.Owner).ThreadID
      else
        ThreadID                                  := 0;
      FormResult                                  := TPingResult(Task.Owner);
      FormResult.StringGrid1.Cells[0, Task.Order] := IntToStr(Task.Order);
      FormResult.StringGrid1.Cells[1, Task.Order] := Task.NodeName;
      FormResult.StringGrid1.Cells[2, Task.Order] :=
        FloatToStrF(Task.MeanDelay, ffFixed, 4, 2);
      FormResult.StringGrid1.Cells[3, Task.Order] :=
        FloatToStrF(Task.MinDelay, ffFixed, 4, 2);
      FormResult.StringGrid1.Cells[4, Task.Order] :=
        FloatToStrF(Task.MaxDelay, ffFixed, 4, 2);
      FormResult.StringGrid1.Cells[5, Task.Order] :=
        FloatToStrF(Task.SuccRate, ffFixed, 4, 2);

      FormResult.SQLTable1.Append;
      FormResult.SQLTable1.FieldAddress(1) := Task.NodeName;

      FormResult.StringGrid1.Cells[1, Task.Order] := Task.NodeName;
      FormResult.StringGrid1.Cells[2, Task.Order] :=
        FloatToStrF(Task.MeanDelay, ffFixed, 4, 2);
      FormResult.StringGrid1.Cells[3, Task.Order] :=
        FloatToStrF(Task.MinDelay, ffFixed, 4, 2);
      FormResult.StringGrid1.Cells[4, Task.Order] :=
        FloatToStrF(Task.MaxDelay, ffFixed, 4, 2);
      FormResult.StringGrid1.Cells[5, Task.Order] :=
        FloatToStrF(Task.SuccRate, ffFixed, 4, 2);
    end;

  procedure TPingResult.BtnStartPingClick(Sender: TObject);
    var
      JsonFile   : TStringList;
      PingTask   : TPingTask;
      SSIndex    : Cardinal;
      SSConfSize : Cardinal;
      SSConfigs  : TJSONArray;
      SSConfValue: TJSONObject;
    begin
      JsonFile := TStringList.Create;
      JsonFile.LoadFromFile('.\gui-config.json');
      SSConf := nil;

      PingTask          := TPingTask.Create(Self);
      PingTask.OnStart  := PingResultForm.TaskStart;
      PingTask.OnCancel := PingResultForm.TaskCanceled;
      PingTask.OnDone   := PingResultForm.TaskDone;

      SSConf     := TJSONObject.ParseJSONValue(JsonFile.Text) as TJSONObject;
      SSConfSize := TJSONArray(SSConf.Pairs[0].JSONValue).Count;
      SSConfigs  := TJSONArray(SSConf.Pairs[0].JSONValue);
      StringGrid1.RowCount := SSConfSize;
      for SSIndex := 0 to SSConfSize - 1 do
        begin
          SSConfValue     := SSConfigs.Items[SSIndex] as TJSONObject;
          PingTask.SSConf := SSConfValue;
          PingTask.Order  := SSIndex + 1;
          PingTask.Owner  := This;
          LogMemo.Lines.Add(IntToStr(PingTask.Order));
          PingTask.SuccCount := 0;
          PingTask.Len       := 8;
          TPingManager.Singleton.AddTask(PingTask);
          PingTask := TPingTask(PingTask.Clone);
        end;
    end;

  procedure TPingResult.TasksStatus(Sender: TObject; Progress: Single);
    begin
      PingProgressBar.Max      := 100;
      PingProgressBar.Position := Round(Progress * 100);
    end;

end.
