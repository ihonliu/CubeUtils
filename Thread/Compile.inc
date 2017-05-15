
{$INCLUDE Compilers.inc}
(*
{$IFNDEF COMPILER_14_UP}
	{$MESSAGE FATAL 'Delphi 2010 or higher required.'}
{$ENDIF}
*)
{$IFDEF COMPILER_15_UP}
	{**
	 * TSpinLock is avaialable since Delphi XE and is faster in situations, where the lock is hold
	 * only for a "few" cpu cycles, than an critical section of the OS.
	 *}
	{$DEFINE USE_SPIN_LOCK}
{$ENDIF}

{$IFDEF DEBUG}
	{.$DEFINE CODE_SITE}
{$ENDIF}

{$IFDEF CODE_SITE}
	{**
	 * Log all messages in file, instead of live logging
	 *
	 * MyDocs\ThreadPool.csl
	 *}
	{.$DEFINE CS_FILE_LOGGING}

	{**
	 * Enable / Disable whole tracking of TPoolManager
	 *}
	{$DEFINE CS_MANAGER}

	{**
	 * Enable / Disable whole tracking of TPoolWorker
	 *}
	{$DEFINE CS_WORKER}
{$ENDIF}

{**
 * Track only required methods of TPoolManager
 *
 * CSM = CodeSiteManager
 *}
{$IFDEF CS_MANAGER}
	{.$DEFINE CSM_EXECUTION_LOOP}
	{.$DEFINE CSM_CHECK_CONTEXT_PROCEDURES}
	{.$DEFINE CSM_CHECK_OUTSTANDING_TASKS}
	{.$DEFINE CSM_TERMINATE_SLEEPING_WORKERS}
	{.$DEFINE CSM_EXECUTION_LOOP_FINALIZE}
{$ENDIF}

{**
 * Track only required methods of TPoolWorker
 *
 * CSW = CodeSiteWorker
 *}
{$IFDEF CS_WORKER}
	{.$DEFINE CSW_EXECUTION_LOOP}
	{.$DEFINE CSW_EXECUTE_TASK}
	{.$DEFINE CSW_EXECUTION_LOOP_FINALIZE}
	{.$DEFINE CSW_DONE_TASK}
{$ENDIF}

{$IFDEF CODE_SITE}
	{$MESSAGE 'CodeSite integration is activated.'}
{$ENDIF}
