{**
 * ThreadPool is a abstract class framework for creating specialized
 * pool of workers (TPoolWorker, separate threads) which are managed
 * by a manager (TPoolManager, also a thread)
 *
 *
 * License
 *
 * The contents of this file are subject to the Mozilla Public License
 * Version 1.1 (the "License"); you may not use this file except in
 * compliance with the License. You may obtain a copy of the License at
 * http://www.mozilla.org/MPL/
 *
 * Software distributed under the License is distributed on an "AS IS"
 * basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See the
 * License for the specific language governing rights and limitations
 * under the License.
 *
 * The Original Code is ThreadPool.pas
 *
 * The Initial Developer of the Original Code is Waldemar Derr.
 * Portions created by Waldemar Derr are Copyright (C) 2010 Waldemar Derr.
 * All Rights Reserved.
 *
 *
 * Change log
 *
 * 1.0.6 / 2011-01-27
 * - TPoolManager.DispatchOwnerDestroyed is now atomic
 *
 * 1.0.5 / 2011-01-13
 * - TThreadProcedures introduced for thread safe transport of TThreadProcedure
 * - Some synchronizations between the manager and his workers
 * - New class method TThreadManager.TerminateSingletonInstances for sure termination of
 *   all managers and his workers
 *
 * 1.0.4 / 2011-01-05
 * - TPoolWorker.DoneTask parameter changed to Successful:Boolean
 *
 * 1.0.3 / 2010-12-31 (I wish you a successful new year ;-)
 * - Revised termination synchronization between the manager and his workers
 *   Is much safer and faster now, because the MREW sync is not obtained from
 *   the thread context of the worker.
 * - Code rationalization
 *   Lesser locks and code with the same functionality
 *
 * 1.0.2 / 2010-12-28
 * - Support for named threads (if the conditional symbol DEBUG is defined)
 * - Task sort optimized
 * - "Execution Loop" concept is introduced in TPoolThread
 * - CodeSite integration
 * - Some bugfix and code reorganization
 *
 * 1.0.1 / 2010-12-16
 * - Priorities for tasks and their handling in manager implemented
 * - Some optimizing
 *
 * 1.0.0 / 2010-12-14
 * - First release
 * - Working successful and exactly as designed in examples
 *
 *
 * @author Waldemar Derr <mail@wladid.de>
 * @version $Id: ThreadPool.pas 52 2011-01-28 10:14:32Z WladiD $
 *}
unit ThreadPool;

interface

{$INCLUDE Compile.inc}

uses
	SysUtils, Generics.Defaults, Generics.Collections, Contnrs, SyncObjs, Windows, Classes, Forms
{$IFDEF CODE_SITE}
	, CodeSiteLogging
{$ENDIF};

const
	Version = '1.0.6';

type
	{**
	 * Class forward declarations
	 *}
	TPoolTask = class;
	TPoolWorker = class;
	TPoolManager = class;
	{**
	 * Class type declarations
	 *}
	TPoolTaskClass = class of TPoolTask;
	TPoolWorkerClass = class of TPoolWorker;
	TPoolManagerClass = class of TPoolManager;
	{**
	 * Derived generic list types
	 *}
	TWorkerList = TObjectList<TPoolWorker>;
	TTaskList = TObjectList<TPoolTask>;
	{**
	 * Enumeration of pissible states of a task
	 *
	 * @see TPoolTask
	 *}
	TTaskState = (tsUnknown, tsInWork, tsError, tsSuccess);
	{**
	 * Enumeration for common priorities of a task
	 *
	 * @see TPoolTask.Priority
	 *}
	TTaskPriority = (
		tpLowest = 0, tpLower = 1, tpLow = 2,
		tpNormal = 3,
		tpHigh = 4, tpHigher = 5, tpHighest = 255,
		tpCustom = -1);
	{**
	 * Enumeration of possible states of a worker
	 *
	 * @see TPoolWorker
	 *}
	TWorkerState = (wsReady, wsBusy, wsTaskDone);
	{**
	 * Anonymous method for a task
	 *
	 * Compatible to TPoolTask.IsTheSame
	 *}
	TTaskFunc = reference to function(Task:TPoolTask):Boolean;
	{**
	 * Status event
	 *
	 * @param Progress Float value between 0 and 1. 1 = 100% = Complete.
	 *}
	TStatusEvent = reference to procedure(Sender:TObject; Progress:Single);

	TManagerProc = TProc<TPoolManager>;
	{**
	 * Anonymous notify event, which is compatible with commonly used TNotifyEvent
	 *}
	TAnonymousNotifyEvent = reference to procedure(Sender:TObject);

	{**
	 * TPoolTask represent a dual data holder
	 *
	 * Derived task classes can add further input fields and also any output fields.
	 * But what are input and what are output fields?
	 * This is answered by your implementation of the correspondig TPoolWorker.
	 *}
	TPoolTask = class(TObject)
	private
		{**
		 * As long as the task is processed, the reference of the worker stays here
		 *
		 * This info is set and required only by the manager, don't access it from workers at all.
		 *}
		FProcessingBy:TPoolWorker;

		function GetPriority:TTaskPriority;
		procedure SetPriority(Priority:TTaskPriority);
	protected
		FOwner:TObject;
		FOnStart:TAnonymousNotifyEvent;
		FOnCancel:TAnonymousNotifyEvent;
		FOnDone:TAnonymousNotifyEvent;
		FState:TTaskState;
		FPriority:Byte;

		class function Compare(const Left, Right:TPoolTask):Integer; virtual;

		procedure Assign(Source:TPoolTask); virtual;
		function IsTheSame(Compare:TPoolTask):Boolean; virtual;

		{**
		 * Both, Priority and PriorityRaw, sets the same field for the priority of the task
		 *
		 * The higher the priority the sooner it's taked for processing, if not all tasks
		 * can be processed at the same time.
		 *
		 * PriorityRaw is introduced for purposes, where you need a finer resolution (range of byte)
		 * Priority is comfortable, but allows only 7 common priorities through a enumeration
		 *
		 * Publish one of them in your derived task, if you need it.
		 *
		 * You must also enable the property SortTasks (disabled by default) in the corresponding
		 * derived manager.
		 *
		 * @see TPoolManager.SortTasks
		 *}
		property Priority:TTaskPriority read GetPriority write SetPriority;
		property PriorityRaw:Byte read FPriority write FPriority;
	public
		constructor Create(Owner:TObject); virtual;

		function Clone:TPoolTask;

		property Owner:TObject read FOwner;
		property State:TTaskState read FState;
		{**
		 * Generic event, which is fired, if the processing of the task is started
		 *}
		property OnStart:TAnonymousNotifyEvent read FOnStart write FOnStart;
		{**
		 * Generic event, which is fired, if the task is canceled
		 *}
		property OnCancel:TAnonymousNotifyEvent read FOnCancel write FOnCancel;
		{**
		 * Generic event, which is fired, if the task is done and was not canceled
		 *}
		property OnDone:TAnonymousNotifyEvent read FOnDone write FOnDone;
	end;

	TThreadProcedures = class;

	{**
	 * TPoolThread contains only the intersected structures/functionality of TPoolWorker and
	 * TPoolManager and act as the base class for them.
	 *
	 * The Execute method is full implemented and should be not overriden by descendants, instead
	 * a simple "Execution Loop" concept is introduced there:
	 * - InitializeExecutionLoop is executed once before the loop
	 * - ExecutionLoop is so long executed in loop as ExecutionLoopCondition returns TRUE.
	 *   But each run of the loop requires a signal on MainSignal (see method TriggerMainSignal).
	 * - If ExecutionLoopCondition returns FALSE, the loop will be breaked and FinalizeExecutionLoop
	 *   is called finally.
	 *
	 * Don't create any instances of this class!
	 *}
	TPoolThread = class(TThread)
	private
		FMainSignal:TEvent;
		FInExecutionLoop:Boolean;

		function GetSleeping:Boolean;
	protected
		procedure ExecutionLoopInitialize; virtual; abstract;
		function ExecutionLoopCondition:Boolean; virtual;
		procedure ExecutionLoop; virtual; abstract;
		procedure ExecutionLoopFinalize; virtual; abstract;

		procedure Execute; override;

		procedure TriggerMainSignal;

		property MainSignal:TEvent read FMainSignal;
		{**
		 * Says, whether this thread is currently in active execution. It's FALSE, if it waits for
		 * the signal (MainSignal)
		 *}
		property InExecutionLoop:Boolean read FInExecutionLoop;
	public
		constructor Create(CreateSuspended:Boolean);
		destructor Destroy; override;

		procedure Terminate; reintroduce; virtual;

		{**
		 * Says, whether this thread is currently sleeping.
		 * It's TRUE, if it waits for the signal (MainSignal).
		 *}
		property Sleeping:Boolean read GetSleeping;
	end;

	{**
	 * TPoolWorker is our workhorse and should do the work to complete a task
	 *
	 * This implementation contains all needed mechanisms for interacting with the corresponding
	 * TPoolManager.
	 *
	 * Of course, here are no code for doing the whole work. This must be implemented in
	 * ExecuteTask by the derived class.
	 *}
	TPoolWorker = class(TPoolThread)
	private
{$IFDEF CODE_SITE}
		FWorkerIndex:Integer;
{$ENDIF}
		FOwner:TPoolManager;
		FState:TWorkerState;
		FCanceled:Boolean;
		FProcessTasks:TTaskList;

{$IFDEF CODE_SITE}
		property WorkerIndex:Integer read FWorkerIndex;
{$ENDIF}
		property ProcessTasks:TTaskList read FProcessTasks;
	protected
		FContextTask:TPoolTask;

		procedure FireEvent(FireEventProc:TProc<TPoolTask>; HasEventFunc:TTaskFunc);

		procedure InitializeTask(SameTasks:TTaskList);
		{**
		 * ExecuteTask is the right place to implement the main work
		 *
		 * You have to use the property ContextTask for do the job.
		 *
		 * It's important to call the DoneTask method with a corresponding value in any case,
		 * if the task is done.
		 *
		 * As running condition they should check for the property Canceled, not for Terminated.
		 *
		 * For example:
		 * - On error: DoneTask(FALSE);
		 * - On success: DoneTask(TRUE);
		 *}
		procedure ExecuteTask; virtual; abstract;
		procedure DoneTask(Successful:Boolean);

		procedure ExecutionLoopInitialize; override;
		procedure ExecutionLoop; override;
		procedure ExecutionLoopFinalize; override;

		procedure Cancel;

		property Owner:TPoolManager read FOwner;
		property Canceled:Boolean read FCanceled;
		property ContextTask:TPoolTask read FContextTask;
	public
		constructor Create(Owner:TPoolManager); virtual;
		destructor Destroy; override;

		procedure Terminate; override;

		property State:TWorkerState read FState write FState;

	end;

	{**
	 * TPoolManager is the key class of the whole concept. It's almost full implemented.
	 * Derived managers must only implement the class method WorkerClass.
	 * For more comfort it's recommended to reintroduce the class method Singleton.
	 * Nothing more is needed, but you can introduce new methods for easier tasks handling.
	 *
	 * @see TPoolManager.WorkerClass
	 * @see TPoolManager.Singleton
	 *
	 * Singleton pattern
	 * -----------------
	 *
	 * Because it makes no sense, to hold any instances of the manager, the singleton pattern was
	 * choosed for rich features (e.g. Demand mode).
	 *
	 * To access the manager you should call always the class method Singleton on the derived
	 * manager class, which always return a valid instance of the corresponding class.
	 * On the first call it creates and starts the manager thread and you are instantly able,
	 * to add your tasks.
	 *
	 * Of course you are able to instantiate many instances manually, but this is not a part of
	 * concept and the behaviour isn't tested.
	 *
	 * @see TPoolManager.Singleton
	 *
	 * Concurrent workers
	 * ------------------
	 *
	 * Each manager is able to manage a amount of workers (TPoolWorker) for processing the added
	 * tasks. The workers are created only on demand. Each worker get's the next task from the
	 * task queue, after it has done their current one. If there are no further tasks in the queue,
	 * all sleeping workers are terminated automatically. But this behaviour is influenced by the
	 * property SpareWorkersCount.
	 *
	 * @see TPoolManager.ConcurrentWorkersCount
	 *
	 * Spared workers
	 * --------------
	 *
	 * You can define a amount of workers, which are not terminated automatically, unless you
	 * decrease the property SpareWorkersCount or terminate the manager manually. This is usefull
	 * for often small tasks.
	 *
	 * The "Demand mode" is disabled, if you define more than 0 SpareWorkersCount.
	 *
	 * @see TPoolManager.SpareWorkersCount
	 *
	 * Demand mode
	 * -----------
	 *
	 * By default a single instance/thread for each derived TPoolManager class is created by the
	 * first call of the class method Singleton and resist, until your application is closed or
	 * you manually terminate it.
	 *
	 * This bahavior can be changed, by simple passing an "init" procedure, before the first call
	 * of Singleton is done. If you do that, the manager terminates himself automatically, if all
	 * added tasks are done or get canceled. The property SpareWorkersCount must be zero (0),
	 * otherwise the demand mode can't be activated.
	 *
	 * @see TPoolManager.RegisterSingletonOnDemandProc
	 *}
	TPoolManager = class(TPoolThread)
	private
		{**
		 * I dont't like to waste the global namespace with types, which are only locally required
		 *}
		type
		TTaskComparer = TDelegatedComparer<TPoolTask>;

		TOwner = class
		public
			Owner:TObject;
			TasksTotalCount:Integer;
			TasksDoneCount:Integer;
			OnTasksStatus:TStatusEvent;
			OnTasksComplete:TAnonymousNotifyEvent;
		end;

		TOwnerList = TObjectList<TOwner>;

		TOwnersAssign = class
			ManagerClass:TPoolManagerClass;
			Owners:TOwnerList;
		end;

		TDemandProcAssign = class
			ManagerClass:TPoolManagerClass;
			DemandProc:TManagerProc;
		end;
	{**
	 * Private section for class related stuff
	 *}
	private
		class var
		FSingleInstances:array of TPoolManager;
		FDemandProcs:array of TDemandProcAssign;
		FStoredOwners:array of TOwnersAssign;
{$IFNDEF COMPILER_15_UP}
		FCPUCount:Cardinal; // Is already stored in a TThread field since Delphi XE
{$ENDIF}

		class function GetDemandProcIndex(ReturnOnFreeIndex:Boolean = FALSE):Integer;
		class function GetCPUCount:Integer;
	{**
	 * Private section for object related stuff
	 *}
	private
		FOwners:TOwnerList;
		FOwnersDoneList:TObjectList;
		FContextProcedures:TThreadProcedures;
		FDemandMode:Boolean;
		FComparer:TTaskComparer;
		FTasksSorted:Boolean;
		FDynamicTerminateEnabled:Boolean;
		FWorkers:TWorkerList;
		FTasks:TTaskList;
		FTasksLock:TMultiReadExclusiveWriteSynchronizer;
		FConcurrentWorkersCount:Integer;
		FSpareWorkersCount:Integer;

		function GetOwner(Owner:TObject; AutoAdd:Boolean = TRUE):TOwner;
		procedure UnregisterOwner(Owner:TObject);
		procedure OwnerAddTasksCount(Owner:TObject; AddTotalCount, AddDoneCount:Integer;
			FireEvents:Boolean = TRUE);
		procedure AddOwnerDone(Owner:TObject);
		procedure StoreOwners;

		procedure SetSortTasks(SortTasks:Boolean);
		function GetSortTasks:Boolean;

		property Owners:TOwnerList read FOwners;
		{**
		 * @see TPoolManager.RegisterSingletonOnDemandProc
		 *}
		property DemandMode:Boolean read FDemandMode;
	{**
	 * Protected section for class related stuff
	 *}
	protected

		class destructor Destroy;

		class procedure SingletonTerminateGate(Sender:TObject);
		{**
		 * WorkerClass should return the class of the corresponding TPoolWorker
		 *
		 * This is the one and only method, which you _must_ implement in any derived manager.
		 *
		 * Example:
		 * <code>
		 * TMyManager = class(TPoolManager)
		 * protected
		 *   class function WorkerClass:TPoolWorkerClass; override;
		 * end;
		 *
		 * implementation
		 *
		 * class function TMyManager.WorkerClass:TPoolWorkerClass;
		 * begin
		 *   Result:=TMyWorker;
		 * end;
		 * </code>
		 *}
		class function WorkerClass:TPoolWorkerClass; virtual; abstract;
	{**
	 * Protected section for object related stuff
	 *}
	protected
		procedure BeginReadTasks;
		procedure EndReadTasks;

		function BeginWriteTasks:Boolean;
		procedure EndWriteTasks;

		function GetTaskIndex(State:TTaskState; StartIndex:Integer):Integer;
		function GetSameTaskIndex(CompareTask:TPoolTask; StartIndex:Integer):Integer;

		function CreateWorker:TPoolWorker; virtual;

		procedure CustomTaskCancel(CompareFunction:TTaskFunc);
		function CustomTaskExists(CompareFunction:TTaskFunc):Boolean;
		function CustomTaskCounter(CompareFunction:TTaskFunc):Integer;

		procedure WorkerTerminated(TerminatedWorker:TPoolWorker); virtual;
		procedure WorkerTaskDone(DoneWorker:TPoolWorker; WorkerState:TWorkerState); virtual;

		procedure ExecutionLoopInitialize; override;
		function ExecutionLoopCondition:Boolean; override;
		procedure ExecutionLoop; override;
		procedure ExecutionLoopFinalize; override;

		procedure SetConcurrentWorkersCount(ConcurrentWorkersCount:Integer);
		procedure SetConcurrentWorkersCountPerCPU(ConcurrentWorkersCountPerCPU:Integer);

		procedure SetSpareWorkersCount(SpareWorkersCount:Integer);
		procedure SetSpareWorkersCountPerCPU(SpareWorkersCountPerCPU:Integer);

		property Workers:TWorkerList read FWorkers;
		property Tasks:TTaskList read FTasks;
		{**
		 * Defines, whether the tasks should be sorted, before the next is picked for processing
		 *
		 * How the tasks are sorted, must be implemented in the class method TPoolTask.Compare.
		 * If you need such sorting mechanism, you must set this property to TRUE in the
		 * derived constructor.
		 *
		 * @see TPoolTask.Compare
		 * @default FALSE
		 *}
		property SortTasks:Boolean read GetSortTasks write SetSortTasks;
		property ContextProcedures:TThreadProcedures read FContextProcedures;
	{**
	 * Class related public section
	 *}
	public
		class function Singleton:TPoolManager;
		class function HasSingleton:Boolean;
		class procedure TerminateSingletonInstances(Wait:Boolean = TRUE);

		class procedure RegisterSingletonOnDemandProc(DemandProc:TManagerProc);
		class function HasSingletonOnDemandProc:Boolean;
		class procedure UnregisterSingletonOnDemandProc;

		class procedure DispatchOwnerDestroyed(Owner:TObject);
	{**
	 * Object related public section
	 *}
	public
		constructor Create; virtual;
		destructor Destroy; override;

		procedure Terminate; override;

		procedure AddTask(Task:TPoolTask); virtual;

		procedure CancelAllTasks;
		procedure CancelTasksByOwner(Owner:TObject);
		procedure CancelTasksBySame(Task:TPoolTask);

		function TaskExists(Task:TPoolTask):Boolean;

		function TasksCountByOwner(Owner:TObject):Integer;

		procedure RegisterOwner(Owner:TObject; OnTasksStatus:TStatusEvent;
			OnTasksComplete:TAnonymousNotifyEvent);
		function RestoreOwners:Boolean;

		{**
		 * Defines, how many worker threads (derived from TPoolWorker) are at the same time allowed
		 *
		 * @default 8
		 *}
		property ConcurrentWorkersCount:Integer read FConcurrentWorkersCount
			write SetConcurrentWorkersCount;
		{**
		 * Defines, how many workers for each CPU are at the same time allowed
		 * This is a write only property. To determine, how many workers are indeed defined,
		 * see ConcurrentWorkersCount. If a value is lower than 1, 2 is assigned.
		 *
		 * @see TPoolManager.ConcurrentWorkersCount
		 *}
		property ConcurrentWorkersCountPerCPU:Integer write SetConcurrentWorkersCountPerCPU;
		{**
		 * Defines, how many worker threads should be spared
		 *
		 * The manager terminate workers, which are in the State wsReady.
		 * With this property you are able to define a amount of sleeping workers that are not
		 * terminated. This is useful for tasks which are done often, but their execution is fast.
		 * In such cases the creation of a new thread costs more CPU cycles than the Execute method.
		 *
		 * This value should be lower than or equal to ConcurrentWorkersCount.
		 *
		 * @default 0
		 *}
		property SpareWorkersCount:Integer read FSpareWorkersCount write SetSpareWorkersCount;
		{**
		 * Defines, how many workers for each CPU should be spared
		 * This is a write only property. To determine, how many spared workers are indeed defined,
		 * see SpareWorkersCount. If a value is lower than 1, 2 is assigned.
		 *
		 * @see TPoolManager.SpareWorkersCount
		 *}
		property SpareWorkersCountPerCPU:Integer write SetSpareWorkersCountPerCPU;
	end;

	TThreadProcedures = class
	private
		type
		TThreadProcedureArray = array of TThreadProcedure;

		var
		FProcedures:TThreadProcedureArray;
{$IFDEF USE_SPIN_LOCK}
		FProceduresLock:TSpinLock;
{$ELSE}
		FProceduresLock:TCriticalSection;
{$ENDIF}
		FChangedSignal:TEvent;
		FOwnChangedSignal:Boolean;
	public
		constructor Create(ForeignChangedSignal:TEvent = nil);
		destructor Destroy; override;

		procedure Add(ThreadProcedure:TThreadProcedure);
		function Execute:Integer;

		property ChangedSignal:TEvent read FChangedSignal;
	end;

implementation

{** TPoolTask **}

constructor TPoolTask.Create(Owner:TObject);
begin
	FOwner:=Owner;
	FState:=tsUnknown;
	Priority:=tpNormal;
end;

{**
 * Getter for the property Priority
 *}
function TPoolTask.GetPriority:TTaskPriority;
begin
	case FPriority of
		Ord(tpLowest):
			Result:=tpLowest;
		Ord(tpLower):
			Result:=tpLower;
		Ord(tpLow):
			Result:=tpLow;
		Ord(tpNormal):
			Result:=tpNormal;
		Ord(tpHigh):
			Result:=tpHigh;
		Ord(tpHigher):
			Result:=tpHigher;
		Ord(tpHighest):
			Result:=tpHighest;
		else
			Result:=tpCustom;
	end;
end;

{**
 * Determine, whether the passed task is the same as the current instance
 *
 * The manager checks every time, before they start a new task, whether there are other
 * "same" tasks in the queue and initialize the worker with a bunch at once.
 *}
function TPoolTask.IsTheSame(Compare: TPoolTask): Boolean;
begin
	Result:=FALSE;
end;

procedure TPoolTask.SetPriority(Priority:TTaskPriority);
begin
	Assert(Priority <> tpCustom, 'The priority "tpCustom" can''t be assigned.');
	FPriority:=Ord(Priority);
end;

{**
 * Assigns data from other task
 *
 * The Assign method should only assign events and fields, which are required for doing the task,
 * no results.
 *}
procedure TPoolTask.Assign(Source:TPoolTask);
begin
	PriorityRaw:=Source.PriorityRaw;
	OnDone:=Source.OnDone;
	OnStart:=Source.OnStart;
	OnCancel:=Source.OnCancel;
end;

{**
 * Creates a new instance of the task
 *}
function TPoolTask.Clone:TPoolTask;
begin
	Result:=TPoolTaskClass(ClassType).Create(Owner);
	Result.Assign(Self);
end;

{**
 * Compares two tasks
 *
 * Compare is used for sorting the tasks. This method considers only the priority,
 * but can be overriden by a derived task.
 *
 * @see TPoolTask.Priority
 * @see TPoolManager.SortTasks
 *}
class function TPoolTask.Compare(const Left, Right:TPoolTask):Integer;
begin
	Result:=Right.FPriority - Left.FPriority;
end;

{** TPoolThread **}

constructor TPoolThread.Create(CreateSuspended:Boolean);
begin
	inherited Create(CreateSuspended);

	FMainSignal:=TEvent.Create(nil, FALSE, FALSE, '');
end;

destructor TPoolThread.Destroy;
begin
	FreeAndNil(FMainSignal);

	inherited Destroy;
end;

function TPoolThread.GetSleeping:Boolean;
begin
	Result:=not InExecutionLoop;
end;

procedure TPoolThread.TriggerMainSignal;
begin
	MainSignal.SetEvent;
end;

procedure TPoolThread.Execute;
begin
{$IFDEF DEBUG}
	NameThreadForDebugging(AnsiString(ClassName));
{$ENDIF}
{$IFDEF CODE_SITE}
	if Self is TPoolWorker then
		CodeSiteManager.SetCurrentThreadName(Format('%s #%d', [ClassName,
			TPoolWorker(Self).WorkerIndex]))
	else
		CodeSiteManager.SetCurrentThreadName(ClassName);
{$ENDIF}
	{**
	 * This is the whole secret of the "Execution Loop" concept
	 *}
	ExecutionLoopInitialize;
	try
		while ExecutionLoopCondition do
		begin
			MainSignal.WaitFor(INFINITE);

			FInExecutionLoop:=TRUE;
			try
{$IFDEF CODE_SITE}
				CodeSite.AddSeparator;
{$ENDIF}
				ExecutionLoop;
			finally
				FInExecutionLoop:=FALSE;
			end;
		end;
	finally
		ExecutionLoopFinalize;
	end;
end;

function TPoolThread.ExecutionLoopCondition:Boolean;
begin
	Result:=not Terminated;
end;

procedure TPoolThread.Terminate;
begin
	inherited Terminate;
	TriggerMainSignal;
end;

{** TPoolWorker **}

constructor TPoolWorker.Create(Owner:TPoolManager);
begin
	inherited Create(FALSE);

	FOwner:=Owner;
	FProcessTasks:=TTaskList.Create(FALSE);
	FreeOnTerminate:=TRUE;
end;

destructor TPoolWorker.Destroy;
begin
	FContextTask.Free;
	FProcessTasks.Free;

	inherited Destroy;
end;

procedure TPoolWorker.ExecutionLoopInitialize;
begin

end;

procedure TPoolWorker.ExecutionLoop;
begin
{$IFDEF CSW_EXECUTION_LOOP}
	CodeSite.EnterMethod(Self, 'ExecutionLoop');
{$ENDIF}
	try
		if Assigned(ContextTask) and (State = wsBusy) then
		begin

			FireEvent(
				procedure(Task:TPoolTask)
				begin
					Task.OnStart(ContextTask);
				end,
				function(Task:TPoolTask):Boolean
				begin
					Result:=Assigned(Task.OnStart);
				end);
{$IFDEF CSW_EXECUTE_TASK}
			CodeSite.EnterMethod(Self, 'ExecuteTask');
{$ENDIF}
			ExecuteTask;
{$IFDEF CSW_EXECUTE_TASK}
			CodeSite.ExitMethod(Self, 'ExecuteTask');
{$ENDIF}
		end;
	finally
		FCanceled:=FALSE;
	end;
{$IFDEF CSW_EXECUTION_LOOP}
	CodeSite.ExitMethod(Self, 'ExecutionLoop');
{$ENDIF}
end;

procedure TPoolWorker.ExecutionLoopFinalize;
begin
{$IFDEF CSW_EXECUTION_LOOP_FINALIZE}
	CodeSite.EnterMethod(Self, 'FinalizeExecutionLoop');
{$ENDIF}
	Owner.WorkerTerminated(Self);
{$IFDEF CSW_EXECUTION_LOOP_FINALIZE}
	CodeSite.SendNote('Worker is on the way for termination');
	CodeSite.ExitMethod(Self, 'FinalizeExecutionLoop');
{$ENDIF}
end;

procedure TPoolWorker.FireEvent(FireEventProc:TProc<TPoolTask>; HasEventFunc:TTaskFunc);
var
	FireEventTasks:TTaskList;
	SynchronizeProc:TThreadProcedure;
	cc:Integer;
	HasHasEventFunc:Boolean;

	function IsTaskValid(Task:TPoolTask):Boolean;
	begin
		Result:=Owner.Tasks.IndexOf(Task) >= 0;
	end;

begin
	SynchronizeProc:=nil;
	HasHasEventFunc:=Assigned(HasEventFunc);
	FireEventTasks:=TTaskList.Create(TRUE);
	try
		Owner.BeginReadTasks;
		try
			for cc:=0 to ProcessTasks.Count - 1 do
				if IsTaskValid(ProcessTasks[cc]) and
					(
						not HasHasEventFunc or
						(
							HasHasEventFunc and
							HasEventFunc(ProcessTasks[cc])
						)
					) then
					FireEventTasks.Add(ProcessTasks[cc].Clone);

			if FireEventTasks.Count > 0 then
			begin
				SynchronizeProc:=procedure
				var
					cc:Integer;
				begin
					for cc:=0 to FireEventTasks.Count - 1 do
						FireEventProc(FireEventTasks[cc]);
				end;
			end;
		finally
			Owner.EndReadTasks;
		end;

		if Assigned(SynchronizeProc) then
			Synchronize(SynchronizeProc);
	finally
		FireEventTasks.Free;
		{**
		 * Prevent memory leaks
		 *}
		FireEventProc:=nil;
		HasEventFunc:=nil;
		SynchronizeProc:=nil;
	end;
end;

{**
 * Conditions:
 * - This method should be called only by manager and also only from their thread context
 * - The passed list should never be empty
 * - The tasks in the list must have as State = tsInWork
 *}
procedure TPoolWorker.InitializeTask(SameTasks:TTaskList);

{$IFNDEF COMPILER_15_UP}
	procedure AddProcessTasks;
	var
		cc:Integer;
	begin
		for cc:=0 to SameTasks.Count - 1 do
			FProcessTasks.Add(SameTasks[cc]);
	end;
{$ENDIF}

begin
	if Assigned(FContextTask) then
		FreeAndNil(FContextTask);

	FProcessTasks.Clear;
	if SameTasks.Count > 1 then
{$IFDEF COMPILER_15_UP}
		FProcessTasks.AddRange(SameTasks) // This will not work on Delphi 2010: compiler error
{$ELSE}
		AddProcessTasks
{$ENDIF}
	else
		FProcessTasks.Add(SameTasks[0]);
	{**
	 * The context task is always a clone of the first task
	 *}
	FContextTask:=SameTasks[0].Clone;
	ContextTask.FOwner:=Self;

	State:=wsBusy;

	TriggerMainSignal;
end;

{**
 * This method must be called after the processing of a task was done, indifferent, whether it was
 * canceled or not, it must always called !
 *
 * @see TPoolWorker.ExecuteTask
 *}
procedure TPoolWorker.DoneTask(Successful:Boolean);
var
	DoneState:TWorkerState;
begin
{$IFDEF CSW_DONE_TASK}
	CodeSite.EnterMethod(Self, 'DoneTask');
{$ENDIF}

	if not Canceled then
	begin
		try
			if Successful then
				ContextTask.FState:=tsSuccess
			else
				ContextTask.FState:=tsError;
			FireEvent(
				procedure(DoneTask:TPoolTask)
				begin
					DoneTask.OnDone(ContextTask);
				end,
				function(DoneTask:TPoolTask):Boolean
				begin
					Result:=Assigned(DoneTask.OnDone);
				end);
		finally
			DoneState:=wsTaskDone;
		end;
	end
	else
	begin
		DoneState:=wsReady;
{$IFDEF CSW_DONE_TASK}
		CodeSite.SendWarning('Canceled');
{$ENDIF}
	end;
	{**
	 * Signal the manager, that we have done our task
	 *}
	Owner.WorkerTaskDone(Self, DoneState);
{$IFDEF CSW_DONE_TASK}
	CodeSite.ExitMethod(Self, 'DoneTask');
{$ENDIF}
end;

procedure TPoolWorker.Terminate;
begin
	Cancel;
	inherited Terminate;
end;

procedure TPoolWorker.Cancel;
begin
	if not Canceled then
		FCanceled:=TRUE;
end;

{** TPoolManager **}

class destructor TPoolManager.Destroy;
var
	cc:Integer;
begin
	for cc:=0 to Length(FDemandProcs) - 1 do
		if Assigned(FDemandProcs[cc]) then
		begin
			FDemandProcs[cc].DemandProc:=nil; // prevent a possible memory leak
			FDemandProcs[cc].Free;
		end;
	FDemandProcs:=nil;

	for cc:=0 to Length(FStoredOwners) - 1 do
		if Assigned(FStoredOwners[cc]) then
		begin
			if Assigned(FStoredOwners[cc].Owners) then
				FStoredOwners[cc].Owners.Free;
			FStoredOwners[cc].Free;
		end;
	FStoredOwners:=nil;
end;

constructor TPoolManager.Create;
begin
	{**
	 * The manager should start immediately (Suspended = FALSE)
	 *}
	inherited Create(FALSE);

	FWorkers:=TWorkerList.Create(FALSE);
	FTasks:=TTaskList.Create(TRUE);
	FOwners:=TOwnerList.Create(TRUE);
	FOwnersDoneList:=TObjectList.Create(FALSE);
	FConcurrentWorkersCount:=8;
	FSpareWorkersCount:=0;
	FTasksLock:=TMultiReadExclusiveWriteSynchronizer.Create;
	FContextProcedures:=TThreadProcedures.Create(MainSignal);

	FreeOnTerminate:=TRUE;
end;

destructor TPoolManager.Destroy;
begin
	FTasks.Free;
	FWorkers.Free;
	FOwners.Free;
	FOwnersDoneList.Free;
	SortTasks:=FALSE; // Destroy the comparer
	FTasksLock.Free;
	FContextProcedures.Free;

	inherited Destroy;
end;

procedure TPoolManager.AddOwnerDone(Owner:TObject);
begin
	if not Assigned(Owner) then
		Exit;
	FOwnersDoneList.Add(Owner);
end;

{**
 * Adds a task to the manager
 *
 * The passed task is fully managed by the TPoolManager and should not be freed externally.
 *
 * Conditions for the execution of the passed task:
 * - If there are no workers yet and the property ConcurrentWorker is greater than zero, the first
 *   worker thread will be created, receive this task and begin the work
 * - If there is a sleeping worker thread (State = wsReady), it receive this task immediately and
 *   begin the work
 * - If the count of working workers (TPoolWorker.State = wsBusy) is lower than the property
 *   ConcurrentWorkersCount, then a new worker thread will be created, receive this task and
 *   begin the work
 * - Otherwise, this task waiting for processing, until one of the above condition are be true
 *}
procedure TPoolManager.AddTask(Task:TPoolTask);
begin
	ContextProcedures.Add(
		procedure
		begin
			BeginWriteTasks;
			try
				Tasks.Add(Task);
				OwnerAddTasksCount(Task.Owner, 1, 0, FALSE);
				FTasksSorted:=FALSE;
			finally
				EndWriteTasks;
			end;
		end);
end;

procedure TPoolManager.BeginReadTasks;
begin
	FTasksLock.BeginRead;
end;

function TPoolManager.BeginWriteTasks:Boolean;
begin
	Result:=FTasksLock.BeginWrite;
end;

{**
 * Customizable cancel method for tasks, through the ability for passing a anonymous compare method
 *
 * @param CompareFunction Each task on which this method answers with TRUE, will be canceled
 *}
procedure TPoolManager.CustomTaskCancel(CompareFunction:TTaskFunc);
var
	CanceledTasks:TTaskList;
	TaskIndex, WorkerIndex:Integer;
begin
	CanceledTasks:=TTaskList.Create(TRUE);
	try
		BeginWriteTasks;
		try
			TaskIndex:=0;
			while TaskIndex < Tasks.Count do
			begin
				if Assigned(Tasks[TaskIndex]) and CompareFunction(Tasks[TaskIndex]) then
				begin
					{**
					 * Try to find the connected worker, if the task is in work
					 *}
					if (Tasks[TaskIndex].State = tsInWork) and
						Assigned(Tasks[TaskIndex].FProcessingBy) then
						WorkerIndex:=Workers.IndexOf(Tasks[TaskIndex].FProcessingBy)
					else
						WorkerIndex:=-1;
					{**
					 * Memorize this cancel operation for owner based stats
					 *}
					AddOwnerDone(Tasks[TaskIndex].Owner);
					{**
					 * If the task has a cancel event handler, so extract it from the tasks list...
					 * (the task is destroyed later, after all OnCancel events were fired)
					 *}
					if Assigned(Tasks[TaskIndex].OnCancel) then
						CanceledTasks.Add(Tasks.Extract(Tasks[TaskIndex])) // Mnemonic: Extract removes without freeing, although it is owner of it
					{**
					 * ...otherwise simply delete it (it will be freed too)
					 *}
					else
						Tasks.Delete(TaskIndex);
					{**
					 * Perform a cancel operation on the worker, if one was detected previously
					 *}
					if (WorkerIndex >= 0) and
						not Workers[WorkerIndex].Canceled and
						(Workers[WorkerIndex].State = wsBusy) then
						Workers[WorkerIndex].Cancel;
				end
				else
					Inc(TaskIndex);
			end;
		finally
			EndWriteTasks;
		end;
	finally
		{**
		 * Fire the OnCancel event in batch for all canceled tasks
		 *}
		if CanceledTasks.Count > 0 then
		begin
			Queue(
				procedure
				var
					cc:Integer;
				begin
					try
						for cc:=0 to CanceledTasks.Count - 1 do
							CanceledTasks[cc].OnCancel(CanceledTasks[cc]);
					finally
						CanceledTasks.Free;
					end;
				end);
		end
		else
			CanceledTasks.Free;
	end;
end;

function TPoolManager.CustomTaskCounter(CompareFunction:TTaskFunc):Integer;
var
	cc:Integer;
begin
	Result:=0;
	BeginReadTasks;
	try
		for cc:=0 to Tasks.Count - 1 do
			Inc(Result, Ord(Assigned(Tasks[cc]) and CompareFunction(Tasks[cc])));
	finally
		EndReadTasks;
		CompareFunction:=nil;
	end;
end;

function TPoolManager.CustomTaskExists(CompareFunction:TTaskFunc):Boolean;
var
	cc:Integer;
begin
	BeginReadTasks;
	try
		cc:=0;

		while (cc < Tasks.Count) and not (Assigned(Tasks[cc]) and CompareFunction(Tasks[cc])) do
			Inc(cc);

		Result:=cc < Tasks.Count;
	finally
		EndReadTasks;
		CompareFunction:=nil;
	end;
end;

{**
 * Cancels all tasks, which are the "same" to the passed task
 *
 * @see TPoolTask.IsTheSame
 *}
procedure TPoolManager.CancelTasksBySame(Task:TPoolTask);
begin
	ContextProcedures.Add(
		procedure
		begin
			CustomTaskCancel(Task.IsTheSame);
		end);
end;

procedure TPoolManager.CancelAllTasks;
begin
	ContextProcedures.Add(
		procedure
		begin
			CustomTaskCancel(
				function(Compare:TPoolTask):Boolean
				begin
					Result:=TRUE;
				end);
		end);
end;

{**
 * Cancels all tasks, which have the passed Owner
 *
 * @see TPoolTask.Owner
 *}
procedure TPoolManager.CancelTasksByOwner(Owner:TObject);
begin
	ContextProcedures.Add(
		procedure
		begin
			CustomTaskCancel(
				function(Compare:TPoolTask):Boolean
				begin
					Result:=Compare.Owner = Owner;
				end);
		end);
end;

procedure TPoolManager.EndReadTasks;
begin
	FTasksLock.EndRead;
end;

procedure TPoolManager.EndWriteTasks;
begin
	FTasksLock.EndWrite;
end;

{**
 * Updates the internal owner based statistics
 *}
procedure TPoolManager.OwnerAddTasksCount(Owner:TObject; AddTotalCount, AddDoneCount:Integer;
	FireEvents:Boolean);
var
	OwnerEntry, OriginOwnerEntry:TOwner;
	SyncProc:TThreadProcedure;
begin
	if not Assigned(Owner) or ((AddTotalCount = 0) and (AddDoneCount = 0)) then
		Exit;
	SyncProc:=nil;

	OriginOwnerEntry:=GetOwner(Owner);
	if AddTotalCount <> 0 then
		OriginOwnerEntry.TasksTotalCount:=OriginOwnerEntry.TasksTotalCount + AddTotalCount;
	if AddDoneCount <> 0 then
		OriginOwnerEntry.TasksDoneCount:=OriginOwnerEntry.TasksDoneCount + AddDoneCount;

	{**
	 * Local clone for the OwnerEntry
	 *}
	OwnerEntry:=TOwner.Create;
	OwnerEntry.Owner:=Owner;
	OwnerEntry.TasksDoneCount:=OriginOwnerEntry.TasksDoneCount;
	OwnerEntry.TasksTotalCount:=OriginOwnerEntry.TasksTotalCount;
	OwnerEntry.OnTasksStatus:=OriginOwnerEntry.OnTasksStatus;
	OwnerEntry.OnTasksComplete:=OriginOwnerEntry.OnTasksComplete;

	if FireEvents and Assigned(OwnerEntry.OnTasksStatus) and
		(OwnerEntry.TasksDoneCount < OwnerEntry.TasksTotalCount) then
	begin
		SyncProc:=procedure
			begin
				try
					OwnerEntry.OnTasksStatus(Owner,
						OwnerEntry.TasksDoneCount / OwnerEntry.TasksTotalCount);
				finally
					OwnerEntry.Free;
				end;
			end
	end
	else if OwnerEntry.TasksDoneCount = OwnerEntry.TasksTotalCount then
	begin
		if FireEvents and
			(Assigned(OwnerEntry.OnTasksComplete) or Assigned(OwnerEntry.OnTasksStatus)) then
			SyncProc:=procedure
				begin
					try
						if Assigned(OwnerEntry.OnTasksStatus) then
							OwnerEntry.OnTasksStatus(Owner, 1);
						if Assigned(OwnerEntry.OnTasksComplete) then
							OwnerEntry.OnTasksComplete(Owner);
					finally
						OwnerEntry.Free;
					end;
				end;
		OwnerEntry.TasksTotalCount:=0;
		OwnerEntry.TasksDoneCount:=0;
		OriginOwnerEntry.TasksTotalCount:=0;
		OriginOwnerEntry.TasksDoneCount:=0;
	end;

	if Assigned(SyncProc) then
		Queue(SyncProc)
	else
		OwnerEntry.Free;
end;

{**
 * Registers any TObject instance as owner and connect it with some events
 *
 * Because each TPoolTask can have a owner, internally owner based statistics are led, so
 * you can, for example, easily implement a progress bar.
 *
 * For best practise, the passed event handlers should be methods of the passed owner object.
 *
 * The registered owner can simple unregistered on all singleton instances by
 * DispatchOwnerDestroyed.
 *
 * @see TPoolManager.DispatchOwnerDestroyed
 *}
procedure TPoolManager.RegisterOwner(Owner:TObject; OnTasksStatus:TStatusEvent;
	OnTasksComplete:TAnonymousNotifyEvent);
begin
	Assert(Assigned(Owner), 'You can''t register a nil owner.');

	ContextProcedures.Add(
		procedure
		var
			OwnerEntry:TOwner;
		begin
			OwnerEntry:=GetOwner(Owner);
			OwnerEntry.OnTasksStatus:=OnTasksStatus;
			OwnerEntry.OnTasksComplete:=OnTasksComplete;
		end);
end;

function TPoolManager.GetTaskIndex(State:TTaskState; StartIndex:Integer):Integer;
begin
	for Result:=StartIndex to Tasks.Count - 1 do
		if Assigned(Tasks[Result]) and (Tasks[Result].State = State) then
			Exit;
	Result:=-1;
end;

function TPoolManager.GetSameTaskIndex(CompareTask:TPoolTask; StartIndex:Integer):Integer;
begin
	for Result:=StartIndex to Tasks.Count - 1 do
		if Assigned(Tasks[Result]) and (CompareTask.IsTheSame(Tasks[Result])) then
			Exit;
	Result:=-1;
end;

{**
 * Getter for the property SortTasks
 *}
function TPoolManager.GetSortTasks:Boolean;
begin
	Result:=Assigned(FComparer);
end;

class function TPoolManager.GetCPUCount:Integer;
begin
{$IFDEF COMPILER_15_UP}
	Result:=ProcessorCount; // ProcessorCount is available since Delphi XE
{$ELSE}
	if FCPUCount = 0 then
		FCPUCount:=System.CPUCount;
	Result:=FCPUCount;
{$ENDIF}
end;

{**
 * Notice...
 * - that this method has no locks!
 * - when AutoAdd is TRUE (default), so you must obtain a write lock before
 *}
function TPoolManager.GetOwner(Owner:TObject; AutoAdd:Boolean):TOwner;
var
	Index:Integer;
begin
	Index:=0;
	while (Index < Owners.Count) and (Owners[Index].Owner <> Owner) do
		Inc(Index);
	if Index = Owners.Count then
	begin
		if AutoAdd then
		begin
			Result:=TOwner.Create;
			Result.Owner:=Owner;
			Owners.Add(Result);
		end
		else
			Result:=nil;
	end
	else
		Result:=Owners[Index]
end;

{**
 * Creates a new unmanaged worker
 *
 * Don't call it directly. This method is called by GetReadyWorker automatically on demand.
 * With this you are able to customize the inited worker in any derived manager.
 *
 * @see TPoolManager.GetReadyWorker
 *}
function TPoolManager.CreateWorker:TPoolWorker;
begin
	Result:=WorkerClass.Create(Self);
end;

procedure TPoolManager.ExecutionLoopInitialize;
begin
	FDynamicTerminateEnabled:=FALSE;
end;

procedure TPoolManager.ExecutionLoop;
var
	ConcurrentWorkersCount, SpareWorkersCount:Integer;

	function DynamicTerminate:Boolean;

		function HasTasks:Boolean;
		begin
			BeginReadTasks;
			try
				Result:=Tasks.Count > 0;
			finally
				EndReadTasks;
			end;
		end;

	begin
		Result:=FDynamicTerminateEnabled and (SpareWorkersCount = 0) and not HasTasks;
	end;

	{**
	 * Executes the context procedures
	 *
	 * @see TPoolManager.AddContextProcedure
	 *}
	procedure CheckContextProcedures;
{$IFDEF CSM_CHECK_CONTEXT_PROCEDURES}
	var
		ExecutedProcedures:Integer;
	begin
		CodeSite.EnterMethod(Self, 'CheckContextProcedures');
		ExecutedProcedures:=ContextProcedures.Execute;
		CodeSite.Send('Executed procedures', ExecutedProcedures);
		CodeSite.ExitMethod(Self, 'CheckContextProcedures');
{$ELSE}
	begin
		ContextProcedures.Execute;
{$ENDIF}
	end;

	{**
	 * Check, whether there are further tasks for processing and try to assign it to a new or
	 * sleeping worker
	 *
	 * @return TRUE, if there are further outstanding tasks
	 *}
	procedure CheckOutstandigTasks;
	var
		TaskIndex:Integer;
		SameTasks:TTaskList;
		Worker:TPoolWorker;

		{**
		 * Says, whether there are sleeping workers or the defined boundig (ConcurrentWorkersCount)
		 * allows it to create a new worker
		 *
		 * It's just a try to determine this and you should don't surely rely on the answer,
		 * because the lock on Workers is acquired and released again. So there are possible
		 * situations, that this method return TRUE and the next call on GetReadyWorker
		 * delivers nil. But you can use it as a condition to enters a complex code section,
		 * as long you check the result of GetReadyWorker against nil.
		 *}
		function HasReadyWorkers:Boolean;
		var
			cc, WorkersCount:Integer;
		begin
			WorkersCount:=Workers.Count;
			{**
			 * First (fast) try: Check the bounding
			 *}
			Result:=WorkersCount < ConcurrentWorkersCount;
			if Result then
				Exit;
			{**
			 * Second (slow) try: Search for a sleeping worker
			 *}
			Result:=TRUE;
			for cc:=0 to WorkersCount - 1 do
				if not Workers[cc].Terminated and (Workers[cc].State = wsReady) then
					Exit;
			Result:=FALSE;
		end;

		{**
		 * Returns a "ready" worker thread
		 *
		 * If there are sleeping worker threads, so one of them will be returned.
		 * If no sleeping threads are available and the bounding (ConcurrentWorkersCount)
		 * is not exceeded, a new worker thread will be created and returned.
		 * Otherwise nil will be returned and must be handled properly.
		 *
		 * New workers will be created, if needed and possible, by the method CreateWorker.
		 *
		 * @see TPoolManager.ConcurrentWorkersCount
		 * @see TPoolManager.CreateWorker
		 *}
		function GetReadyWorker:TPoolWorker;
		var
			cc, WorkersCount:Integer;
		begin
			Result:=nil;
			{**
			 * Search for a sleeping worker
			 *}
			WorkersCount:=Workers.Count;
			if WorkersCount > ConcurrentWorkersCount then
				Exit;
			for cc:=0 to WorkersCount - 1 do
			begin
				Result:=Workers[cc];
				if not Result.Terminated and (Result.State = wsReady) then
					Break
				else
					Result:=nil;
			end;
			{**
			 * No sleeping worker found, try to create a new one
			 *}
			if not Assigned(Result) and (WorkersCount < ConcurrentWorkersCount) then
			begin
				Result:=CreateWorker;
				Workers.Add(Result);
			end;
{$IFDEF CODE_SITE}
			if Assigned(Result) then
				Result.FWorkerIndex:=Workers.IndexOf(Result);
{$ENDIF}
		end;

		procedure AddTheSameTasks(StartIndex:Integer);
		var
			SameTaskIndex:Integer;
		begin
			SameTaskIndex:=GetSameTaskIndex(SameTasks[0], StartIndex);
			if SameTaskIndex = -1 then
				Exit;
			repeat
				if Tasks[SameTaskIndex].State = tsUnknown then
					SameTasks.Add(Tasks[SameTaskIndex]);

				SameTaskIndex:=GetSameTaskIndex(SameTasks[0], SameTaskIndex + 1);
			until SameTaskIndex = -1;
		end;

		procedure SetInWorkState;
		var
			cc:Integer;
		begin
			for cc:=0 to SameTasks.Count - 1 do
			begin
				SameTasks[cc].FProcessingBy:=Worker;
				SameTasks[cc].FState:=tsInWork;
			end;
		end;
	begin
{$IFDEF CSM_CHECK_OUTSTANDING_TASKS}
		CodeSite.EnterMethod(Self, 'CheckOutstandigTasks');
{$ENDIF}

		if not HasReadyWorkers then
		begin
{$IFDEF CSM_CHECK_OUTSTANDING_TASKS}
			CodeSite.SendNote('No ready workers found');
			CodeSite.ExitMethod(Self, 'CheckOutstandigTasks');
{$ENDIF}
			Exit;
		end;
		SameTasks:=TTaskList.Create(FALSE);
		BeginWriteTasks;
		try
			if SortTasks and not FTasksSorted then
			begin
				Tasks.Sort(FComparer);
				FTasksSorted:=TRUE;
			end;
{$IFDEF CSM_CHECK_OUTSTANDING_TASKS}
			CodeSite.EnterMethod('Search for outstanding tasks');
{$ENDIF}
			TaskIndex:=GetTaskIndex(tsUnknown, 0);
			if TaskIndex >= 0 then
			begin
				repeat
{$IFDEF CSM_CHECK_OUTSTANDING_TASKS}
					CodeSite.SendNote('Outstanding task found on index #%d...', [TaskIndex]);
{$ENDIF}
					Worker:=GetReadyWorker;
					if not Assigned(Worker) then
					begin
{$IFDEF CSM_CHECK_OUTSTANDING_TASKS}
						CodeSite.SendWarning('...no further ready workers found');
{$ENDIF}
						Break;
					end;

					SameTasks.Clear;
					SameTasks.Add(Tasks[TaskIndex]);

					AddTheSameTasks(TaskIndex + 1);

					SetInWorkState;

					Worker.InitializeTask(SameTasks);
{$IFDEF CSM_CHECK_OUTSTANDING_TASKS}
					CodeSite.SendNote('...and initialized with worker on index #%d',
						[Workers.IndexOf(Worker)]);
{$ENDIF}

					TaskIndex:=GetTaskIndex(tsUnknown, TaskIndex + 1);
				until TaskIndex = -1;

				FDynamicTerminateEnabled:=DemandMode;
			end;
{$IFDEF CSM_CHECK_OUTSTANDING_TASKS}
			CodeSite.ExitMethod('Search for outstanding tasks');
{$ENDIF}
		finally
			EndWriteTasks;
			SameTasks.Free;
		end;
{$IFDEF CSM_CHECK_OUTSTANDING_TASKS}
		CodeSite.ExitMethod(Self, 'CheckOutstandigTasks');
{$ENDIF}
	end;

	procedure UpdateStats;
	var
		cc:Integer;
	begin
		for cc:=0 to FOwnersDoneList.Count - 1 do
			OwnerAddTasksCount(FOwnersDoneList[cc], 0, 1,
				// Condition for the paramater FireEvents, to minimize the syncs
				(cc = (FOwnersDoneList.Count - 1)) or
				(
					((cc + 1) < FOwnersDoneList.Count) and
					(FOwnersDoneList[cc] <> FOwnersDoneList[cc + 1])
				));
		FOwnersDoneList.Clear;
	end;

	{**
	 * Terminates workers which are sleeping (State = wsReady) with the property
	 * SpareWorkersCount in mind.
	 *}
	procedure TerminateSleepingWorkers;
	var
		SleepingWorkerIndex:Integer;

		function GetSleepingWorkersCount(out FirstSleepingIndex:Integer):Integer;
		var
			WorkerIndex:Integer;
		begin
			Result:=0;
			FirstSleepingIndex:=-1;
			for WorkerIndex:=0 to Workers.Count - 1 do
				if (Workers[WorkerIndex].State = wsReady) and
					not Workers[WorkerIndex].Terminated then
				begin
					Inc(Result);
					if FirstSleepingIndex = -1 then
						FirstSleepingIndex:=WorkerIndex;
				end;
{$IFDEF CSM_TERMINATE_SLEEPING_WORKERS}
			CodeSite.Send('Sleeping workers count', Result);
{$ENDIF}
		end;

	begin
{$IFDEF CSM_TERMINATE_SLEEPING_WORKERS}
		CodeSite.EnterMethod(Self, 'TerminateSleepingWorkers');
{$ENDIF}

		while GetSleepingWorkersCount(SleepingWorkerIndex) > SpareWorkersCount do
		begin
{$IFDEF CSM_TERMINATE_SLEEPING_WORKERS}
			CodeSite.SendNote('Terminate worker on index #%d', [SleepingWorkerIndex]);
{$ENDIF}
			Workers[SleepingWorkerIndex].Terminate;
		end;

{$IFDEF CSM_TERMINATE_SLEEPING_WORKERS}
		CodeSite.ExitMethod(Self, 'TerminateSleepingWorkers');
{$ENDIF}
	end;

begin
{$IFDEF CSM_EXECUTION_LOOP}
	CodeSite.EnterMethod(Self, 'ExecutionLoop');
{$ENDIF}
	{**
	 * Store the boundings for the count of workers, to prevent any side effects for this loop
	 *}
	ConcurrentWorkersCount:=Self.ConcurrentWorkersCount;
	SpareWorkersCount:=Self.SpareWorkersCount;

	CheckContextProcedures;
	{**
	 * Context procedures are able to terminate this thread, but in that case further
	 * processing must be prevented.
	 *}
	if not Terminated then
	begin
		CheckOutstandigTasks;
		UpdateStats;
	end;
	TerminateSleepingWorkers;
	if not Terminated and DynamicTerminate then
	begin
		Terminate;
		TriggerMainSignal;
	end;
{$IFDEF CSM_EXECUTION_LOOP}
	CodeSite.ExitMethod(Self, 'ExecutionLoop');
{$ENDIF}
end;

function TPoolManager.ExecutionLoopCondition:Boolean;
begin
	{**
	 * Manager is so long active as any workers exists
	 *}
	Result:=not Terminated or (Workers.Count > 0);
end;

procedure TPoolManager.ExecutionLoopFinalize;
begin
{$IFDEF CSM_EXECUTION_LOOP_FINALIZE}
	CodeSite.EnterMethod(Self, 'FinalizeExecutionLoop');
{$ENDIF}
	{**
	 * Exit the thread only, if there are no (write) locks
	 *}
	BeginReadTasks;
	EndReadTasks;

{$IFDEF CSM_EXECUTION_LOOP_FINALIZE}
	CodeSite.SendNote('Manager is on the way for termination');
	CodeSite.ExitMethod(Self, 'FinalizeExecutionLoop');
{$ENDIF}
end;

{**
 * Notify all singleton instances, that the passed owner was be destroyed
 *
 * - You can call this method on the TPoolManager class or on any derived class.
 * - This method is blocking, but you are sure, that all managers were notified about the object
 *   destroy.
 * - This method should only be called from the main thread.
 *}
class procedure TPoolManager.DispatchOwnerDestroyed(Owner:TObject);
var
	cc, ccc:Integer;
	CommitsProcessed, CommitsTotal:Integer;

	procedure ManagerCommit(PoolManager:TPoolManager);
	begin
		PoolManager.ContextProcedures.Add(
			procedure
			begin
				try
					PoolManager.UnregisterOwner(Owner);
					PoolManager.CancelTasksByOwner(Owner);
					{**
					 * Because UnregisterOwner and CancelTasksByOwner adds itself to
					 * ContextProcedures, we must manually execute it, but we are always on the
					 * right place:
					 * In the beginning of the ExecutionLoop inside the manager thread ;)
					 *}
					PoolManager.ContextProcedures.Execute;
				finally
{$IFDEF COMPILER_15_UP}
					TInterlocked.Increment(CommitsProcessed);
{$ELSE}
					InterlockedIncrement(CommitsProcessed);
{$ENDIF}
				end;
			end);
		Inc(CommitsTotal);
	end;

begin
	if not Assigned(Owner) then
		Exit;
	Assert(CurrentThread.ThreadID = MainThreadID, 'Call DispatchOwnerDestroyed from the main thread');

	CommitsProcessed:=0;
	CommitsTotal:=0;
	{**
	 * Notify all singleton instances
	 *}
	for cc:=0 to Length(FSingleInstances) - 1 do
		if Assigned(FSingleInstances[cc]) and not FSingleInstances[cc].Terminated then
			ManagerCommit(FSingleInstances[cc]);
	{**
	 * Wait for all commits, if any
	 *}
	if CommitsTotal > 0 then
		repeat
			CheckSynchronize(50);
		until CommitsProcessed = CommitsTotal;
	{**
	 * Remove the destroyed owner from all stored owners
	 *}
	for cc:=0 to Length(FStoredOwners) - 1 do
	begin
		if Assigned(FStoredOwners[cc]) and Assigned(FStoredOwners[cc].Owners) then
		begin
			for ccc:=FStoredOwners[cc].Owners.Count - 1 downto 0 do
			begin
				if Assigned(FStoredOwners[cc].Owners[ccc]) and
					(FStoredOwners[cc].Owners[ccc].Owner = Owner) then
					FStoredOwners[cc].Owners.Delete(ccc);
			end;
			if FStoredOwners[cc].Owners.Count = 0 then
				FreeAndNil(FStoredOwners[cc].Owners);
		end;
	end;
end;

{**
 * Setter for the property ConcurrentWorkersCount
 *}
procedure TPoolManager.SetConcurrentWorkersCount(ConcurrentWorkersCount:Integer);
begin
	if InterlockedExchange(FConcurrentWorkersCount, ConcurrentWorkersCount) <> ConcurrentWorkersCount then
		TriggerMainSignal;
end;

{**
 * Setter for the property ConcurrentWorkersCountPerCPU
 *}
procedure TPoolManager.SetConcurrentWorkersCountPerCPU(ConcurrentWorkersCountPerCPU:Integer);
var
	PerCPUCount:Integer;
begin
	PerCPUCount:=ConcurrentWorkersCountPerCPU * GetCPUCount;
	if PerCPUCount = 0 then
		PerCPUCount:=2;
	ConcurrentWorkersCount:=PerCPUCount;
end;

{**
 * Setter for the property SortTasks
 *}
procedure TPoolManager.SetSortTasks(SortTasks:Boolean);
begin
	if SortTasks = Self.SortTasks then
		Exit;
	FreeAndNil(FComparer);
	if SortTasks then
		FComparer:=TTaskComparer.Create(
			function(const Left, Right:TPoolTask):Integer
			begin
				Result:=Ord(Assigned(Right)) - Ord(Assigned(Left));
				if Result = 0 then
					Result:=TPoolTaskClass(Left.ClassType).Compare(Left, Right);
			end);
end;

{**
 * Setter for the property SpareWorkersCount
 *}
procedure TPoolManager.SetSpareWorkersCount(SpareWorkersCount:Integer);
begin
	if InterlockedExchange(FSpareWorkersCount, SpareWorkersCount) <> SpareWorkersCount then
		TriggerMainSignal;
end;

{**
 * Setter for the property SpareWorkersCountPerCPU
 *}
procedure TPoolManager.SetSpareWorkersCountPerCPU(SpareWorkersCountPerCPU:Integer);
var
	PerCPUCount:Integer;
begin
	PerCPUCount:=SpareWorkersCountPerCPU * GetCPUCount;
	if PerCPUCount = 0 then
		PerCPUCount:=2;
	SpareWorkersCount:=PerCPUCount;
end;

{**
 * "Generic" singleton method
 *
 * It creates for each derived TPoolManager class, a single instance. For strong type access you can
 * reintroduce a new Singleton method, but obtain the instance form this one. The advantage of this
 * solution is, that you don't need to declare a new class field for the single instance in each
 * derived class.
 *
 * Derived TPoolManager's can use this Singleton concept as follows:
 *
 * <code>
 * TDerivedManager = class(TPoolManager)
 * public
 *   class function Singleton:TDerivedManager; reintroduce;
 * end;
 *
 * implementation
 *
 * class function TDerivedManager.Singleton:TDerivedManager;
 * begin
 *   Result:=TDerivedManager(inherited Singleton); // Don't worried about this hard type cast ;) see the implementation
 * end;
 * </code>
 *}
class function TPoolManager.Singleton:TPoolManager;
const
	GrowLength = 4;
var
	Index, ArrayLength, DemandProcIndex:Integer;
begin
	Assert(Self <> TPoolManager, 'Singleton must be called on a derived class.');

	ArrayLength:=Length(FSingleInstances);
	// Mnemonic: Self is the current class in a class method
	for Index:=0 to ArrayLength - 1 do
		if Assigned(FSingleInstances[Index]) and
			(FSingleInstances[Index].ClassType = Self) then
		begin
			Result:=FSingleInstances[Index];
			Exit;
		end;
	{**
	 * No single instance exists for the current class, so we have to create a new one
	 *}
	Result:=Self.Create;
	Result.OnTerminate:=SingletonTerminateGate;
	{**
	 * Lookup for a demand "init" proc
	 *}
	DemandProcIndex:=GetDemandProcIndex;
	if DemandProcIndex >= 0 then
	begin
		Result.FDemandMode:=TRUE;
		FDemandProcs[DemandProcIndex].DemandProc(Result);
	end;
	{**
	 * Try to find a free position in array
	 *}
	Index:=0;
	while (Index < ArrayLength) and Assigned(FSingleInstances[Index]) do
		Inc(Index);
	{**
	 * Enlarge the array
	 *}
	if Index = ArrayLength then
		SetLength(FSingleInstances, Index + GrowLength);
	{**
	 * Store the instance in our class field
	 *}
	FSingleInstances[Index]:=Result;
end;

{**
 * Determines, whether there is already a single instance available for the current class
 *
 * @see TPoolManager.Singleton
 *}
class function TPoolManager.HasSingleton:Boolean;
var
	Index:Integer;
begin
	Assert(Self <> TPoolManager, 'HasSingleton must be called on a derived class.');

	Result:=TRUE;

	for Index:=0 to Length(FSingleInstances) - 1 do
		if Assigned(FSingleInstances[Index]) and (FSingleInstances[Index].ClassType = Self) then
			Exit;

	Result:=FALSE;
end;

class function TPoolManager.GetDemandProcIndex(ReturnOnFreeIndex:Boolean):Integer;
begin
	for Result:=0 to Length(FDemandProcs) - 1 do
		if (ReturnOnFreeIndex and not Assigned(FDemandProcs[Result])) or
			(Assigned(FDemandProcs[Result]) and (FDemandProcs[Result].ManagerClass = Self)) then
			Exit;
	Result:=-1;
end;

class procedure TPoolManager.RegisterSingletonOnDemandProc(DemandProc:TManagerProc);
var
	Index:Integer;
begin
	Assert(Self <> TPoolManager, 'RegisterSingletonOnDemandProc must be called on a derived class.');
	Index:=GetDemandProcIndex(TRUE);
	if Index = -1 then
	begin
		Index:=Length(FDemandProcs);
		SetLength(FDemandProcs, Index + 1);
	end;
	if not Assigned(FDemandProcs[Index]) then
	begin
		FDemandProcs[Index]:=TDemandProcAssign.Create;
		FDemandProcs[Index].ManagerClass:=Self;
	end;
	FDemandProcs[Index].DemandProc:=DemandProc;
end;

{**
 * Stores all registered owners
 *
 * It's executed in SingletonTerminateGate, if this instance is in "Demand Mode".
 * This method must be called from the main thread.
 *
 * @see TPoolManager.SingletonTerminateGate
 *}
procedure TPoolManager.StoreOwners;
var
	Index, ArrLength, cc:Integer;
	Store:TOwnersAssign;
begin
	{**
	 * We need no locks, because this method should always be called from the main thread
	 *}
	Index:=0;
	ArrLength:=Length(FStoredOwners);
	{**
	 * Try to find the correct class position
	 *}
	while (Index < ArrLength) and
		not (Assigned(FStoredOwners[Index]) and (FStoredOwners[Index].ManagerClass = ClassType)) do
		Inc(Index);
	{**
	 * Create entry and grow the array, if not found
	 *}
	if Index = ArrLength then
	begin
		Store:=TOwnersAssign.Create;
		Store.ManagerClass:=TPoolManagerClass(ClassType);
		Index:=ArrLength;
		SetLength(FStoredOwners, ArrLength + 1);
		FStoredOwners[Index]:=Store;
	end
	{**
	 * Entry found
	 *}
	else
		Store:=FStoredOwners[Index];

	if Assigned(Store.Owners) then
		FreeAndNil(Store.Owners);
	{**
	 * Remove owners, for which are no events registered and reset the stats
	 *}
	for cc:=Owners.Count - 1 downto 0 do
	begin
		if not (Assigned(Owners[cc].OnTasksStatus) or Assigned(Owners[cc].OnTasksComplete)) then
			Owners.Delete(cc)
		{**
		 * Reset stats for owners which are applied
		 *}
		else
		begin
			Owners[cc].TasksDoneCount:=0;
			Owners[cc].TasksTotalCount:=0;
		end;
	end;
	{**
	 * Steal the Owners list
	 *}
	if Owners.Count > 0 then
		Store.Owners:=Owners;
	{**
	 * There are no further access's on this list, except the Free call in Destroy
	 *}
	FOwners:=nil;
end;

{**
 * Restores all previously registered owners
 *
 * This method should only called in a demand init proc (RegisterSingeltonOnDemandProc).
 * It must called from the main thread, else you running into a crazy access violations loop.
 *
 * Here is a usage example of this method in a demand init proc:
 *
 * <code>
 * procedure PoolInit(Manager:TPoolManager);
 * begin
 *   Manager.ConcurrentWorkersCount:=16;
 *   if not Manager.RestoreOwners then
 *     Manager.RegisterOwner(Form1, Form1.TasksStatus, Form1.TaskDone);
 * end;
 *
 * TDerivedManager.RegisterSingletonOnDemandProc(PoolInit);
 * </code>
 *
 * @see TPoolManager.RegisterSingletonOnDemandProc
 * @see TPoolManager.RegisterOwner
 *
 * @return TRUE, if there was previously any owners stored, otherwise FALSE is returned.
 *}
function TPoolManager.RestoreOwners:Boolean;
var
	Index, ArrLength:Integer;
begin
	{**
	 * We need no locks, because this method should always be called from the main thread
	 *}
	Index:=0;
	ArrLength:=Length(FStoredOwners);
	{**
	 * Try to find the correct class position
	 *}
	while (Index < ArrLength) and
		not (Assigned(FStoredOwners[Index]) and (FStoredOwners[Index].ManagerClass = ClassType)) do
		Inc(Index);

	Result:=(Index < ArrLength) and Assigned(FStoredOwners[Index].Owners);
	if not Result then
		Exit;
	{**
	 * Reassign the Owners list
	 *}
	Owners.Free;
	FOwners:=FStoredOwners[Index].Owners;
	FStoredOwners[Index].Owners:=nil;
end;

class function TPoolManager.HasSingletonOnDemandProc:Boolean;
begin
	Assert(Self <> TPoolManager, 'HasSingletonOnDemandProc must be called on a derived class.');
	Result:=GetDemandProcIndex >= 0;
end;

class procedure TPoolManager.UnregisterSingletonOnDemandProc;
var
	Index:Integer;
begin
	Assert(Self <> TPoolManager, 'UnregisterSingletonOnDemandProc must be called on a derived class.');
	Index:=GetDemandProcIndex;
	if Index >= 0 then
		FreeAndNil(FDemandProcs[Index]);
end;

{**
 * Determines, with the implementation of TPoolTask.IsTheSame, whether the task exists
 *
 * This method can be called from any thread, because it do a read lock on the task list.
 *}
function TPoolManager.TaskExists(Task:TPoolTask):Boolean;
begin
	Result:=CustomTaskExists(Task.IsTheSame);
end;

{**
 * Return the count of tasks, which have the same passed owner
 *}
function TPoolManager.TasksCountByOwner(Owner:TObject):Integer;
begin
	Result:=CustomTaskCounter(
		function(Task:TPoolTask):Boolean
		begin
			Result:=Task.Owner = Owner;
		end);
end;

procedure TPoolManager.Terminate;
begin
	ContextProcedures.Add(
		procedure
		var
			cc:Integer;
		begin
			{**
			 * Terminate all running workers
			 *}
			for cc:=0 to Workers.Count - 1 do
				Workers[cc].Terminate;

			inherited Terminate;
		end);
end;

{**
 * Terminates all singleton instances of the managers and his workers in one step
 *
 * This is useful on application exit. In this case the Wait parameter must be TRUE, because
 * otherwise all threads are hard killed.
 *
 * This method must called from the context of the main thread.
 *}
class procedure TPoolManager.TerminateSingletonInstances(Wait:Boolean);
var
	cc, SingleInstancesLength:Integer;
	AnyTerminateDischarged:Boolean;
begin
	Assert(CurrentThread.ThreadID = MainThreadID, 'Call TerminateSingletonInstances from the main thread');
	SingleInstancesLength:=Length(FSingleInstances);
	AnyTerminateDischarged:=FALSE;
	for cc:=0 to SingleInstancesLength - 1 do
		if Assigned(FSingleInstances[cc]) then
		begin
			FSingleInstances[cc].Terminate;
			AnyTerminateDischarged:=TRUE;
		end;
	if not (Wait and AnyTerminateDischarged) then
		Exit;
	repeat
		CheckSynchronize(50);
		cc:=0;
		while (cc < SingleInstancesLength) and not Assigned(FSingleInstances[cc]) do
			Inc(cc);
	until cc = SingleInstancesLength;
end;

{**
 * Event handler for TPoolManager.OnTerminate (just for singleton instances)
 *
 * Notice: It's always called from the context of the main thread.
 *}
class procedure TPoolManager.SingletonTerminateGate(Sender:TObject);
var
	cc:Integer;
begin
	{**
	 * Remove the connection from FSingleInstances, if one is there
	 *}
	for cc:=0 to Length(FSingleInstances) - 1 do
		if FSingleInstances[cc] = Sender then
		begin
			if FSingleInstances[cc].DemandMode then
				FSingleInstances[cc].StoreOwners;
			FSingleInstances[cc]:=nil;
			Break;
		end;
end;

procedure TPoolManager.UnregisterOwner(Owner:TObject);
begin
	ContextProcedures.Add(
		procedure
		begin
			Owners.Remove(GetOwner(Owner, FALSE));
		end);
end;

{**
 * Callback method for TPoolWorker's, which has done their task
 *
 * It's called in TPoolWorker.DoneTask from their thread context
 *}
procedure TPoolManager.WorkerTaskDone(DoneWorker:TPoolWorker; WorkerState:TWorkerState);
var
	BlockSignal:TEvent;
begin
	BlockSignal:=TEvent.Create(nil, FALSE, FALSE, '');
	try
		ContextProcedures.Add(
			procedure
			var
				TaskIndex, ProcessedTaskIndex:Integer;
			begin
				try
					if WorkerState <> wsTaskDone then
						Exit;

					BeginWriteTasks;
					try
						{**
						 * Remove the processed tasks from tasks
						 *}
						for ProcessedTaskIndex:=0 to DoneWorker.ProcessTasks.Count - 1 do
						begin
							TaskIndex:=Tasks.IndexOf(
								DoneWorker.ProcessTasks[ProcessedTaskIndex]);
							if TaskIndex >= 0 then
							begin
								AddOwnerDone(Tasks[TaskIndex].Owner);
								Tasks.Delete(TaskIndex);
							end;
						end;
					finally
						EndWriteTasks;
					end;
				finally
					DoneWorker.State:=wsReady;
					BlockSignal.SetEvent;
				end;
			end);
		BlockSignal.WaitFor(INFINITE);
	finally
		BlockSignal.Free;
	end;
end;

{**
 * Callback method for TPoolWorker's, which are terminated
 *
 * It's called in TPoolWorker.ExecutionLoopFinalize automatically.
 *}
procedure TPoolManager.WorkerTerminated(TerminatedWorker:TPoolWorker);
var
	BlockSignal:TEvent;
begin
	BlockSignal:=TEvent.Create(nil, FALSE, FALSE, '');
	try
		ContextProcedures.Add(
			procedure
			begin
				try
					Workers.Remove(TerminatedWorker);
				finally
					BlockSignal.SetEvent;
				end;
			end);
		BlockSignal.WaitFor(INFINITE);
	finally
		BlockSignal.Free;
	end;
end;

{** TThreadProcedures **}

constructor TThreadProcedures.Create(ForeignChangedSignal:TEvent);
begin
{$IFDEF USE_SPIN_LOCK}
	FProceduresLock:=TSpinLock.Create(TRUE);
{$ELSE}
	FProceduresLock:=TCriticalSection.Create;
{$ENDIF}
	if Assigned(ForeignChangedSignal) then
		FChangedSignal:=ForeignChangedSignal
	else
		FChangedSignal:=TEvent.Create(nil, FALSE, FALSE, '');

	FOwnChangedSignal:=ForeignChangedSignal <> FChangedSignal;
end;

destructor TThreadProcedures.Destroy;
var
	cc:Integer;
begin
	{**
	 * Possible memory leaks, if we don't reset all references of closures
	 *}
	for cc:=0 to Length(FProcedures) - 1 do
		FProcedures[cc]:=nil;
	FProcedures:=nil;
{$IFNDEF USE_SPIN_LOCK}
	FProceduresLock.Free;
{$ENDIF}
	if FOwnChangedSignal then
		FChangedSignal.Free;

	inherited;
end;

procedure TThreadProcedures.Add(ThreadProcedure:TThreadProcedure);
var
	Index:Integer;
begin
	FProceduresLock.Enter;
	try
		Index:=Length(FProcedures);
		SetLength(FProcedures, Index + 1);
		FProcedures[Index]:=ThreadProcedure;
	finally
{$IFDEF USE_SPIN_LOCK}
		FProceduresLock.Exit;
{$ELSE}
		FProceduresLock.Leave;
{$ENDIF}
	end;

	ChangedSignal.SetEvent;
end;

{**
 * Executes all added procedures
 *
 * @return The count of executed procedures
 *}
function TThreadProcedures.Execute:Integer;
var
	cc, ProceduresCount:Integer;
	LocalProcedures:TThreadProcedureArray;
begin
	Result:=0;
	{**
	 * Copy the current added procedures locally and reset FProcedures
	 *}
	FProceduresLock.Enter;
	try
		if not Assigned(FProcedures) then
			Exit;
		ProceduresCount:=Length(FProcedures);
		LocalProcedures:=Copy(FProcedures, 0, ProceduresCount);
		FProcedures:=nil;
	finally
{$IFDEF USE_SPIN_LOCK}
		FProceduresLock.Exit;
{$ELSE}
		FProceduresLock.Leave;
{$ENDIF}
	end;
	{**
	 * Whole execution is outside the lock, so Add can be further called by other threads
	 *}
	try
		for cc:=0 to ProceduresCount - 1 do
		begin
			LocalProcedures[cc]();
			LocalProcedures[cc]:=nil;
			Inc(Result);
		end;
	finally
		LocalProcedures:=nil;
	end;
end;

{$IFDEF CS_FILE_LOGGING}
var
	Dest:TCodeSiteDestination;

initialization
Dest:=TCodeSiteDestination.Create(nil);

Dest.LogFile.Active:=True;
Dest.LogFile.FileName:='ThreadPool.csl';
Dest.LogFile.FilePath:='$(MyDocs)';
Dest.LogFile.MaxSize:=16 * 1024; // 16 MB
Dest.LogFile.MaxParts:=2;

CodeSite.Destination:=Dest;

finalization
Dest.Free;

{$ENDIF}


end.
