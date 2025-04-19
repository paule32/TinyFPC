// ---------------------------------------------------------------------------------------
// Copyright(c) 2025 Jens Kallup
// 
// Permission is hereby granted, free of charge, to any person obtaining a copy of this
// software and associated documentation files(the "Software"), to deal in the Software
// without restriction, including without limitation the rights to use, copy, modify,
// merge, publish, distribute, sublicense, and /or sell copies of the Software, and to
// permit persons to whom the Software is furnished to do so, subject to the following
// conditions :
// 
// The above copyright notice and this permission notice shall be included in all copies
// or substantial portions of the Software.
// 
// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED,
// INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A
// PARTICULAR PURPOSE AND NONINFRINGEMENT.IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
// HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF
// CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE
// OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
// ---------------------------------------------------------------------------------------

{$if defined(codeh) and defined(codei)}
{$FATAL interface and implementation at the same time not possible}
{$endif}

{$ifdef codeh}

type
  PM128A = ^M128A;
  M128A = record
    Low: QWord;
    High: int64;
  end;

  PContext = ^TContext;
  TContext = record
    P1Home: QWord;
    P2Home: QWord;
    P3Home: QWord;
    P4Home: QWord;
    P5Home: QWord;
    P6Home: QWord;
    ContextFlags: DWord;
    MxCsr: DWord;
    SegCs: word;
    SegDs: word;
    SegEs: word;
    SegFs: word;
    SegGs: word;
    SegSs: word;
    EFlags: DWord;
    Dr0: QWord;
    Dr1: QWord;
    Dr2: QWord;
    Dr3: QWord;
    Dr6: QWord;
    Dr7: QWord;
    Rax: QWord;
    Rcx: QWord;
    Rdx: QWord;
    Rbx: QWord;
    Rsp: QWord;
    Rbp: QWord;
    Rsi: QWord;
    Rdi: QWord;
    R8: QWord;
    R9: QWord;
    R10: QWord;
    R11: QWord;
    R12: QWord;
    R13: QWord;
    R14: QWord;
    R15: QWord;
    Rip: QWord;
    Header: array[0..1] of M128A;
    Legacy: array[0..7] of M128A;
    Xmm0: M128A;
    Xmm1: M128A;
    Xmm2: M128A;
    Xmm3: M128A;
    Xmm4: M128A;
    Xmm5: M128A;
    Xmm6: M128A;
    Xmm7: M128A;
    Xmm8: M128A;
    Xmm9: M128A;
    Xmm10: M128A;
    Xmm11: M128A;
    Xmm12: M128A;
    Xmm13: M128A;
    Xmm14: M128A;
    Xmm15: M128A;
    VectorRegister: array[0..25] of M128A;
    VectorControl: QWord;
    DebugControl: QWord;
    LastBranchToRip: QWord;
    LastBranchFromRip: QWord;
    LastExceptionToRip: QWord;
    LastExceptionFromRip: QWord;
  end;

  PEXCEPTION_POINTERS = ^EXCEPTION_POINTERS;
  EXCEPTION_POINTERS = record
    ExceptionRecord: PEXCEPTION_RECORD;
    ContextRecord: PCONTEXT;
  end;

const
  SCOPE_FINALLY  = 0; // finally code in RvaHandler
  SCOPE_CATCHALL = 1; // unwinds to RvaEnd, RvaHandler is the end of except block
  SCOPE_IMPLICIT = 2; // finally code in RvaHandler, unwinds to RvaEnd
                      // otherwise: except with 'on' stmts, value is RVA of filter data

  UNWIND_HISTORY_TABLE_SIZE = 12;
  UNW_FLAG_NHANDLER         = 0;

  RaiseMaxFrameCount: LongInt = 16;

type
  PScopeRec = ^TScopeRec;
  TScopeRec = record
    Typ: DWord;
    RvaStart: DWord;
    RvaEnd: DWord;
    RvaHandler: DWord;
  end;

  EXCEPTION_ROUTINE = function(var ExceptionRecord: EXCEPTION_RECORD; EstablisherFrame: Pointer; var ContextRecord: TContext; DispatcherContext: Pointer): EXCEPTION_DISPOSITION;

  PRUNTIME_FUNCTION = ^RUNTIME_FUNCTION;
  RUNTIME_FUNCTION = record
    BeginAddress: DWORD;
    EndAddress: DWORD;
    UnwindData: DWORD;
  end;

  UNWIND_HISTORY_TABLE_ENTRY = record
    ImageBase: QWord;
    FunctionEntry: PRUNTIME_FUNCTION;
  end;

  PUNWIND_HISTORY_TABLE = ^UNWIND_HISTORY_TABLE;
  UNWIND_HISTORY_TABLE = record
    Count: DWORD;
    Search: byte;
    RaiseStatusIndex: byte;
    Unwind: byte;
    Exception: byte;
    LowAddress: QWord;
    HighAddress: QWord;
    Entry: array[0..UNWIND_HISTORY_TABLE_SIZE - 1] of UNWIND_HISTORY_TABLE_ENTRY;
  end;

  PDispatcherContext = ^TDispatcherContext;
  TDispatcherContext = record
    ControlPc: QWord;
    ImageBase: QWord;
    FunctionEntry: Pointer;//PRUNTIME_FUNCTION;
    EstablisherFrame: QWord;
    TargetIp: QWord;
    ContextRecord: PContext;
    LanguageHandler: EXCEPTION_ROUTINE;
    HandlerData: Pointer;
    HistoryTable: Pointer;//PUNWIND_HISTORY_TABLE;
    ScopeIndex: DWord;
    Fill0: DWord;
  end;    

procedure install_exception_handlers;
procedure remove_exception_handlers;

function __FPC_specific_handler(var rec: EXCEPTION_RECORD; var frame: TSEHFrame; var context: TContext; var dispatch: TDispatcherContext): EXCEPTION_DISPOSITION;

function RtlLookupFunctionEntry(ControlPC: QWord; out ImageBase: QWord; HistoryTable: PUNWIND_HISTORY_TABLE): PRUNTIME_FUNCTION; stdcall; external KERNEL32;
function RtlVirtualUnwind(
  HandlerType: DWORD;
  ImageBase: QWord;
  ControlPc: QWord;
  FunctionEntry: PRUNTIME_FUNCTION;
  var ContextRecord: TContext;
  HandlerData: PPointer;
  EstablisherFrame: PQWord;
  ContextPointers: Pointer{PKNONVOLATILE_CONTEXT_POINTERS}): EXCEPTION_ROUTINE; stdcall; external KERNEL32;

procedure fpc_doneexception; compilerproc;
procedure fpc_reraise; compilerproc;
procedure fpc_raise_nested; compilerproc;     
procedure fpc_raiseexception(obj: TObject; anaddr: codepointer; aframe: pointer); compilerproc;

{$ifdef DLLEXPORT}
function PushException_(
  var rec     : EXCEPTION_RECORD;
  var context : TContext;
  out obj     : TObject;
  AcceptNull  : boolean): boolean; stdcall; export;
  
function signals_exception_handler_(
  exrec     : PEXCEPTION_RECORD;
  exframe   : PEXCEPTION_FRAME;
  excontext : PCONTEXT;
  dispatch  : pointer): integer; stdcall; export;
  
function fpc_popobjectstack_: TObject; stdcall export;
{$endif DLLEXPORT}
  
{$ifdef DLLIMPORT}
function PushException_(
  var rec     : EXCEPTION_RECORD;
  var context : TContext;
  out obj     : TObject;
  AcceptNull  : boolean): boolean; stdcall; external RTLDLL;
  
function signals_exception_handler_(
  exrec     : PEXCEPTION_RECORD;
  exframe   : PEXCEPTION_FRAME;
  excontext : PCONTEXT;
  dispatch  : pointer): integer; stdcall; external RTLDLL;
  
function fpc_popobjectstack_: TObject; stdcall external RTLDLL;
{$endif DLLIMPORT}

{$endif codeh}

{$ifdef codei}

const
  ex_max_level = 16;
  ex_level: byte = 0;

var
  ex_rip   : array[0..ex_max_level-1] of qword;
  except_signal : array[0..ex_max_level-1] of qword;
  reset_fpu    : array[0..ex_max_level-1] of boolean;

procedure JumpToHandleSignal;
//var
//  res, rip, _rbp, sigtype : dword64;
begin
  writeln('JumpToHandleSignal');
//  asm
//    movq (%rbp),%rax
//    movq %rax,_rbp
//  end;
//{$ifdef SIGNALS_DEBUG}
//  if IsConsole then
//    Writeln(stderr,'In start of JumpToHandleSignal');
//{$endif SIGNALS_DEBUG}
//
//  if except_level>0 then
//    dec(except_level)
//  else
//    RunError(216);
//  rip:=except_rip[except_level];
//
//  sigtype:=except_signal[except_level];
//  if reset_fpu[except_level] then
//    SysResetFPU;
//  if assigned(System_exception_frame) then
//    { get the handler in front again }
//    asm
//      movq  System_exception_frame,%rax
//      movq  %rax,%gs:(0)
//    end;
//  if (sigtype>=SIGABRT) and (sigtype<=SIGMAX) and
//     (signal_list[sigtype]<>@SIG_DFL) then
//    begin
//      res:=signal_list[sigtype](sigtype);
//    end
//  else
//    res:=0;
//
//  if res=0 then
//    Begin
//{$ifdef SIGNALS_DEBUG}
//      if IsConsole then
//        Writeln(stderr,'In JumpToHandleSignal');
//{$endif SIGNALS_DEBUG}
//      RunError(sigtype);
//    end
//  else
//    { jump back to old code }
//    asm
//      movq rip,%rax
//      push %rax
//      movq _rbp,%rax
//      push %rax
//      leave
//      ret
//    end;
end;

{$ifdef DLLEXPORT}
function signals_exception_handler_(
  exrec     : PEXCEPTION_RECORD;
  exframe   : PEXCEPTION_FRAME;
  excontext : PCONTEXT;
  dispatch  : pointer): integer; stdcall; export;
  function CallSignal(sigtype, frame: longint; must_reset_fpu: boolean): integer;
  begin
    if ex_level >= ex_max_level then exit;
    ex_rip[ex_level] := excontext^.Rip;
    except_signal[ex_level] := sigtype;
    reset_fpu[ex_level] := must_reset_fpu;
    inc(ex_level);
    excontext^.rip := ptruint(@JumpToHandleSignal);
    exrec^.ExceptionCode := 0;
    result := 0;
  end;
var
  frame, ret: integer;
begin
  writeln('signals_exception_handler');

  frame := excontext^.rbp;
  { default: unhandled! }
  ret := 1;

  writeln('exrec^.ExceptionCode = ', exrec^.ExceptionCode);

  //case exrec^.ExceptionCode of
  //  STATUS_ACCESS_VIOLATION: ret := CallSignal(SIGSEGV, frame, false);
  //  STATUS_INVALID_HANDLE: ret := 0;
  //end;
  ret := CallSignal(SIGSEGV, frame, false);

  result := ret;
end;
function signals_exception_handler(
  exrec     : PEXCEPTION_RECORD;
  exframe   : PEXCEPTION_FRAME;
  excontext : PCONTEXT;
  dispatch  : pointer): integer; stdcall;
begin
  Exit(signals_exception_handler_(exrec, exframe, excontext, dispatch));
end;
{$endif DLLEXPORT}
{$ifdef DLLIMPORT}
function signals_exception_handler(
  exrec     : PEXCEPTION_RECORD;
  exframe   : PEXCEPTION_FRAME;
  excontext : PCONTEXT;
  dispatch  : pointer): integer; stdcall;
begin
  Exit(signals_exception_handler_(exrec, exframe, excontext, dispatch));
end;
{$endif DLLIMPORT}

function syswin_x64_exception_handler(ex: PEXCEPTION_POINTERS): integer; stdcall;
begin
  writeln('syswin_x64_exception_handler');

  result := signals_exception_handler(@ex^.ExceptionRecord, nil, @ex^.ContextRecord, nil);

  wait_for_enter;
end;

var
  vectored_exception_handler: pointer = nil;

procedure install_exception_handlers;
begin
  // this thing doesnt work, find out why
  //SetUnhandledExceptionFilter(@syswin_x64_exception_handler);
  vectored_exception_handler := AddVectoredExceptionHandler(1{1 = higher priotity}, @syswin_x64_exception_handler);
end;

procedure remove_exception_handlers;
begin
  //SetUnhandledExceptionFilter(nil);
  if vectored_exception_handler <> nil then
    if RemoveVectoredExceptionHandler(vectored_exception_handler) then
      vectored_exception_handler := nil;
end;

// x64 only
function RunErrorCodeSEH(const rec: EXCEPTION_RECORD; const context: TContext): longint;
begin
  result := RunErrorCode(rec);
  if (result = -255) then TranslateMxcsr(context.MxCsr, result);
end;

procedure SysResetFPU;
var
  localmxcsr: dword;
  localfpucw: word;
begin
  localfpucw := Default8087CW;
  localmxcsr := DefaultMXCSR;
  asm
    fninit
    fwait
    fldcw   localfpucw
    ldmxcsr localmxcsr
  end;
end;

{ note: context must be passed by value, so modifications are made to a local copy }
{$ifdef DLLEXPORT}
function GetBacktrace_(
  Context       : TContext;
  StartingFrame : Pointer;
  out Frames    : PPointer): longint; stdcall; export;
var
  UnwindHistory: UNWIND_HISTORY_TABLE;
  RuntimeFunction: PRUNTIME_FUNCTION;
  HandlerData: Pointer;
  EstablisherFrame: QWord;
  ImageBase: QWord;
  FrameCount, FrameBufSize: longint;
begin
  FillChar(UnwindHistory, sizeof(UNWIND_HISTORY_TABLE), 0);
  UnwindHistory.Unwind := 1;

  FrameCount := 0;
  FrameBufSize := 0;
  Frames := nil;
  repeat
    RuntimeFunction := RtlLookupFunctionEntry(context.Rip, ImageBase, @UnwindHistory);

    if Assigned(RuntimeFunction) then RtlVirtualUnwind(UNW_FLAG_NHANDLER, ImageBase, context.Rip,
        RuntimeFunction, Context, @HandlerData, @EstablisherFrame, nil)
    else  { a leaf function } begin
      Context.Rip := PQWord(Context.Rsp)^;
      Inc(Context.Rsp, sizeof(Pointer));
    end;

    if (context.Rip = 0) or (FrameCount >= RaiseMaxFrameCount) then break;

    if (Pointer(context.Rbp) > StartingFrame) or (FrameCount > 0) then begin
      if (FrameCount >= FrameBufSize) then begin
        Inc(FrameBufSize, 16);
        ReallocMem(Frames, FrameBufSize * sizeof(Pointer));
      end;
      Frames[FrameCount] := Pointer(context.Rip);
      Inc(FrameCount);
    end;
  until false;
  Result := FrameCount;
end;
function GetBacktrace(
  Context       : TContext;
  StartingFrame : Pointer;
  out Frames    : PPointer): longint; stdcall;
begin
  Exit(GetBacktrace_(Context, StartingFrame, Frames));
end;
{$endif DLLEXPORT}
{$ifdef DLLIMPORT}
function GetBacktrace_(
  Context       : TContext;
  StartingFrame : Pointer;
  out Frames    : PPointer): longint; stdcall; external RTLDLL;
function GetBacktrace(
  Context       : TContext;
  StartingFrame : Pointer;
  out Frames    : PPointer): longint; stdcall;
begin
  Exit(GetBacktrace_(Context, StartingFrame, Frames));
end;
{$endif DLLIMPORT}

{$ifdef DLLEXPORT}
function PushException_(
  var rec     : EXCEPTION_RECORD;
  var context : TContext;
  out obj     : TObject;
  AcceptNull  : boolean): boolean; stdcall; export;
var
  adr: Pointer;
  exc: PExceptObject;
  Frames: PPointer;
  FrameCount: longint;
  code: longint;
begin
  Adr := Pointer(rec.ExceptionInformation[0]);
  Obj := TObject(rec.ExceptionInformation[1]);
  Framecount := longint(PtrUInt(rec.ExceptionInformation[2]));
  Frames := Pointer(rec.ExceptionInformation[3]);

  if rec.ExceptionCode <> FPC_EXCEPTION_CODE then begin
    Obj := nil;
    Result := false;
    code := RunErrorCodeSEH(rec, context);
    if Assigned(ExceptObjProc) then Obj := TObject(TExceptObjProc(ExceptObjProc)(abs(code), rec));
    if (Obj = nil) and (not AcceptNull) then Exit;
    adr := rec.ExceptionAddress;
    FrameCount := GetBacktrace(context, nil, Frames);
    if code < 0 then SysResetFPU;
  end;

  New(exc);
  exc^.FObject := Obj;
  exc^.Addr := adr;
  exc^.Frames := Frames;
  exc^.FrameCount := FrameCount;
  exc^.Refcount := 0;
  exc^.Next := ExceptObjectStack;
  ExceptObjectStack := exc;
  Result := true;
end;
function PushException(var rec: EXCEPTION_RECORD; var context: TContext; out obj: TObject; AcceptNull: boolean): boolean;
begin
  Exit(PushException_(rec, context, obj, AcceptNull));
end;
{$endif DLLEXPORT}
{$ifdef DLLIMPORT}
function PushException(
  var rec     : EXCEPTION_RECORD;
  var context : TContext;
  out obj     : TObject;
  AcceptNull  : boolean): boolean;
begin
  Exit(PushException_(rec, context, obj, AcceptNull));
end;
{$endif DLLIMPORT}

// we need to return the exception object to dispose it
{$ifdef DLLEXPORT}
function fpc_popobjectstack_: TObject; stdcall export;
var
  hp: PExceptObject;
begin
  hp := ExceptObjectStack;
  if hp = nil then
    halt(1)
  else begin
    if hp^.refcount = 0 then result := hp^.FObject else result := nil;
    ExceptObjectStack := hp^.next;
    if assigned(hp^.frames) then freemem(hp^.frames);
    dispose(hp);
    erroraddr := nil;
  end;
end;
function fpc_popobjectstack: TObject; [public , alias: 'FPC_POPOBJECTSTACK']; compilerproc;
begin
  Exit(fpc_popobjectstack_);
end;
{$endif DLLEXPORT}
{$ifdef DLLIMPORT}
function fpc_popobjectstack: TObject; [public , alias: 'FPC_POPOBJECTSTACK']; compilerproc;
begin
  Exit(fpc_popobjectstack_);
end;
{$endif DLLIMPORT}

function internal_popobjectstack: TObject; external name 'FPC_POPOBJECTSTACK';

// this is required for x64 only?
{ This handler is installed by compiler for every try..finally and try..except statement, including implicit ones. }
{$ifdef DLLEXPORT}
function __FPC_specific_handler_(
  var rec      : EXCEPTION_RECORD;
  var frame    : TSEHFrame;
  var context  : TCONTEXT;
  var dispatch : TDispatcherContext): EXCEPTION_DISPOSITION; stdcall; export;
var
  TargetRva, ControlRva: DWord;
  scope: PScopeRec;
  scopeIdx: DWord;
  TargetAddr: Pointer;
  obj: TObject;
begin
  writeln('__FPC_specific_handler');
  result := ExceptionContinueSearch;
  ControlRva := dispatch.ControlPc - dispatch.ImageBase;
  ScopeIdx := dispatch.ScopeIndex;
  if (rec.ExceptionFlags and EXCEPTION_UNWIND) = 0 then begin
    while ScopeIdx < PDWord(dispatch.HandlerData)^ do begin
      scope := @PScopeRec(dispatch.HandlerData + sizeof(Dword))[ScopeIdx];
      { Check if the exception was raised in the 'except' block,
        and dispose the existing exception object if so. }
      if (ControlRva >= scope^.RvaEnd) and (ControlRva < scope^.RvaHandler) and ((scope^.Typ = SCOPE_CATCHALL) or (scope^.Typ > SCOPE_IMPLICIT)) then
        internal_popobjectstack.Free
      else if (ControlRva >= scope^.RvaStart) and (ControlRva < scope^.RvaEnd) and (scope^.Typ <> SCOPE_FINALLY) then begin
        if scope^.Typ > SCOPE_IMPLICIT then begin
          TargetAddr := FilterException(rec, dispatch.ImageBase, scope^.Typ, abs(RunErrorCodeSEH(rec, context)));
          if TargetAddr = nil then begin
            inc(ScopeIdx);
            continue;
          end;
        end else
          TargetAddr := Pointer(scope^.RvaEnd + dispatch.ImageBase);

        if not PushException(rec, context, obj, Scope^.Typ = SCOPE_IMPLICIT) then exit;

        { Does not return, control is transferred to TargetAddr,
          obj is placed into RAX. }
        RtlUnwindEx(@frame, TargetAddr, @rec, obj, dispatch.ContextRecord, dispatch.HistoryTable);
      end;
      inc(ScopeIdx);
    end;
  end else begin
    TargetRva := dispatch.TargetIp - dispatch.ImageBase;

    while ScopeIdx < PDword(dispatch.HandlerData)^ do begin
      scope := @PScopeRec(dispatch.HandlerData + sizeof(Dword))[ScopeIdx];
      if (ControlRva >= scope^.RvaStart) and (ControlRva < scope^.RvaEnd) and ((scope^.Typ = SCOPE_FINALLY) or (scope^.Typ = SCOPE_IMPLICIT)) then begin
        if (TargetRva >= scope^.RvaStart) and (TargetRva < scope^.RvaEnd) and ((rec.ExceptionFlags and EXCEPTION_TARGET_UNWIND) <> 0) then exit;

        dispatch.ScopeIndex := ScopeIdx + 1;

        TUnwindProc(dispatch.ImageBase + scope^.RvaHandler)(context.Rbp);
      end;
      inc(ScopeIdx);
    end;
  end;
end;
function __FPC_specific_handler(
  var rec      : EXCEPTION_RECORD;
  var frame    : TSEHFrame;
  var context  : TCONTEXT;
  var dispatch : TDispatcherContext): EXCEPTION_DISPOSITION; [public, alias: '__FPC_specific_handler'];
begin
  Exit(__FPC_specific_handler(rec, frame, context, dispatch));
end;
{$endif DLLEXPORT}
{$ifdef DLLIMPORT}
function __FPC_specific_handler_(
  var rec      : EXCEPTION_RECORD;
  var frame    : TSEHFrame;
  var context  : TCONTEXT;
  var dispatch : TDispatcherContext): EXCEPTION_DISPOSITION; stdcall; external RTLDLL;
function __FPC_specific_handler(
  var rec      : EXCEPTION_RECORD;
  var frame    : TSEHFrame;
  var context  : TCONTEXT;
  var dispatch : TDispatcherContext): EXCEPTION_DISPOSITION; [public, alias: '__FPC_specific_handler'];
begin
  Exit(__FPC_specific_handler(rec, frame, context, dispatch));
end;
{$endif DLLIMPORT}
// ----

procedure fpc_doneexception; [public, alias: 'FPC_DONEEXCEPTION'] compilerproc;
begin
  internal_popobjectstack.Free;
end;

procedure fpc_reraise; [public, alias: 'FPC_RERAISE']; compilerproc;
begin
end;

procedure fpc_raise_nested; [public, alias: 'FPC_RAISE_NESTED']; compilerproc;
begin
end;

{$ifdef DLLEXPORT}
procedure fpc_raiseexception_(
  obj    : TObject;
  anaddr : codepointer;
  aframe : pointer); stdcall; export;
begin
  writeln('fpc_raiseexception');

  if obj = nil then writeln('obj nil');
  if anaddr = nil then writeln('anaddr nil');
  if aframe = nil then writeln('aframe nil');

  if obj <> nil then begin
    writeln('exception message = ', TTestException(obj).test);
  end;
end;
procedure fpc_raiseexception(
  obj    : TObject;
  anaddr : codepointer;
  aframe : pointer); [public, alias: 'FPC_RAISEEXCEPTION']; compilerproc;
begin
  fpc_raiseexception_(obj, anaddr, aframe);
end;
{$endif DLLEXPORT}
{$ifdef DLLIMPORT}
procedure fpc_raiseexception_(
  obj    : TObject;
  anaddr : codepointer;
  aframe : pointer); stdcall; external RTLDLL;
procedure fpc_raiseexception(
  obj    : TObject;
  anaddr : codepointer;
  aframe : pointer); [public, alias: 'FPC_RAISEEXCEPTION']; compilerproc;
begin
  fpc_raiseexception_(obj, anaddr, aframe);
end;
{$endif DLLIMPORT}

{$endif}
