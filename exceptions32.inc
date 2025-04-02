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
  PFloatingSaveArea = ^TFloatingSaveArea;
  TFloatingSaveArea = packed record
    ControlWord: cardinal;
    StatusWord: cardinal;
    TagWord: cardinal;
    ErrorOffset: cardinal;
    ErrorSelector: cardinal;
    DataOffset: cardinal;
    DataSelector: cardinal;
    RegisterArea: array[0..79] of byte;
    Cr0NpxState: cardinal;
  end;
          
  PContext = ^TContext;
  TContext = packed record
    // The flags values within this flag control the contents of a CONTEXT record.
    ContextFlags: cardinal;
    // This section is specified/returned if CONTEXT_DEBUG_REGISTERS is
    // set in ContextFlags.  Note that CONTEXT_DEBUG_REGISTERS is NOT
    // included in CONTEXT_FULL.
    Dr0, Dr1, Dr2,
    Dr3, Dr6, Dr7: cardinal;
    // This section is specified/returned if the ContextFlags word contains the flag CONTEXT_FLOATING_POINT.
    FloatSave: TFloatingSaveArea;
    // This section is specified/returned if the ContextFlags word contains the flag CONTEXT_SEGMENTS.
    SegGs, SegFs,
    SegEs, SegDs: cardinal;
    // This section is specified/returned if the ContextFlags word contains the flag CONTEXT_INTEGER.
    Edi, Esi, Ebx,
    Edx, Ecx, Eax: cardinal;
    // This section is specified/returned if the ContextFlags word contains the flag CONTEXT_CONTROL.
    Ebp: cardinal;
    Eip: cardinal;
    SegCs: cardinal;
    EFlags, Esp, SegSs: cardinal;
    // This section is specified/returned if the ContextFlags word contains the flag CONTEXT_EXTENDED_REGISTERS.
    // The format and contexts are processor specific
    ExtendedRegisters: array[0..MAXIMUM_SUPPORTED_EXTENSION-1] of byte;
  end;

  PEXCEPTION_POINTERS = ^EXCEPTION_POINTERS;
  EXCEPTION_POINTERS = record
    ExceptionRecord: PEXCEPTION_RECORD;
    ContextRecord: PCONTEXT;
  end;

  TDispatcherContext = record
  end;

procedure install_exception_handlers;
procedure remove_exception_handlers;

function __FPC_finally_handler(var rec: EXCEPTION_RECORD; var frame: TSEHFrame; var context: TContext; var dispatch: TDispatcherContext): EXCEPTION_DISPOSITION; cdecl;
function __FPC_except_handler(var rec: EXCEPTION_RECORD; var frame: TSEHFrame; var context: TContext; var dispatch: TDispatcherContext): EXCEPTION_DISPOSITION; cdecl;
function __FPC_default_handler(var rec: EXCEPTION_RECORD; var frame: TSEHFrame; var context: TContext; var dispatch: TDispatcherContext): EXCEPTION_DISPOSITION; cdecl;
function __FPC_except_safecall(var rec: EXCEPTION_RECORD; var frame: TSEHFrame; var context: TContext; var dispatch: TDispatcherContext): EXCEPTION_DISPOSITION; cdecl;
function __FPC_on_handler(var rec: EXCEPTION_RECORD; var frame: TSEHFrame; var context: TContext; var dispatch: TDispatcherContext): EXCEPTION_DISPOSITION; cdecl;

procedure fpc_doneexception; compilerproc;
procedure fpc_reraise; compilerproc;
procedure fpc_raise_nested; compilerproc;
procedure fpc_raiseexception(obj: TObject; anaddr: codepointer; aframe: pointer); compilerproc;

{$endif}

{$ifdef codei}

function syswin_x86_exception_handler(ex: PEXCEPTION_POINTERS): EXCEPTION_DISPOSITION; stdcall;
begin
  writeln('syswin_x86_exception_handler');
  //
  //result := ExceptionContinueSearch;
  //
  //case ex^.ExceptionRecord^.ExceptionCode of
  //  STATUS_ACCESS_VIOLATION: begin
  //    writeln('VectoredExceptionHandler catched: AV');
  //  end;
  //end;
end;

var
  vectored_exception_handler: pointer = nil;

procedure install_exception_handlers;
begin
  // this thing doesnt work, find out why
  //SetUnhandledExceptionFilter(@syswin_x86_exception_handler);
  vectored_exception_handler := AddVectoredExceptionHandler(1{1 = higher priotity}, @syswin_x86_exception_handler);
end;

procedure remove_exception_handlers;
begin
  //SetUnhandledExceptionFilter(nil);
  if vectored_exception_handler <> nil then
    if RemoveVectoredExceptionHandler(vectored_exception_handler) then
      vectored_exception_handler := nil;
end;

function NullHandler(var rec: EXCEPTION_RECORD; var frame: TSEHFrame; var context: TContext; var dispatch: TDispatcherContext): EXCEPTION_DISPOSITION; cdecl;
begin
  result := ExceptionContinueSearch;
end;

// x86 only
function RunErrorCode386(const rec: EXCEPTION_RECORD; const context: TContext): LongInt;
begin
  result := RunErrorCode(rec);
  { deal with SSE exceptions }
  if (result = -255) and ((context.ContextFlags and CONTEXT_EXTENDED_REGISTERS) <> 0) then
    TranslateMxcsr(PLongWord(@context.ExtendedRegisters[24])^, result);
end;

function PopObjectStack: PExceptObject;
var
  hp: PExceptObject;
begin
  hp := ExceptObjectStack;
  if hp = nil then
    halt(255)
  else begin
    ExceptObjectStack := hp^.next;
    if assigned(hp^.frames) then FreeMem(hp^.frames);
  end;
  result := hp;
end;

function NestedHandler(var rec: EXCEPTION_RECORD; var frame: TSEHFrame; var context: TContext; var dispatch: TDispatcherContext): EXCEPTION_DISPOSITION; cdecl;
var
  hp: PExceptObject;
begin
  if (rec.ExceptionFlags and EXCEPTION_UNWIND) <> 0 then begin
    hp := PopObjectStack;
    if hp^.refcount = 0 then hp^.FObject.Free;
  end;
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

procedure CommonHandler(var rec: EXCEPTION_RECORD; var frame: TSEHFrame; var context: TContext; TargetAddr: Pointer);
var
  p: pointer;
  exc: TExceptObject;
  code: LongInt;
begin
  exc.FObject := nil;
  code := RunErrorCode386(rec, context);
  // call ExceptObjProc
  if ExceptObjProc <> nil then exc.FObject := TObject(TExceptObjProc(ExceptObjProc)(abs(code), rec));

  if (exc.FObject = nil) and (frame.Addr <> Pointer(@__FPC_except_safecall)) then exit;

  exc.Addr := rec.ExceptionAddress;
  // to do, or not to do?
  //exc.FrameCount := GetBacktrace(context, nil, exc.Frames);
  exc.Framecount := 0;

  // FPU reset required?
  if code < 0 then SysResetFPU;

  RtlUnwind(@frame, nil, @rec, nil);

  exc.refcount := 0;
  exc.SEHFrame := @frame;
  exc.ExceptRec := @rec;
  exc.Next := ExceptObjectStack;
  ExceptObjectStack := @exc;

  frame.Addr := @NestedHandler;

  if setjmp(exc.ReraiseBuf) = 0 then begin
    {$asmmode ATT}
    asm
      movl   exc.FObject,%eax
      movl   frame,%edx
      movl   TargetAddr,%ecx              // load ebp-based var before changing ebp
      movl   TSEHFrame._EBP(%edx),%ebp
      jmpl   *%ecx
    end;
  end;

  // control comes here if exception is re-raised
  rec.ExceptionFlags := rec.ExceptionFlags and (not EXCEPTION_UNWINDING);
end;
// ----

function __FPC_finally_handler(
  var rec: EXCEPTION_RECORD;
  var frame: TSEHFrame;
  var context: TCONTEXT;
  var dispatch: TDispatcherContext): EXCEPTION_DISPOSITION; cdecl; [public, alias: '__FPC_finally_handler'];
var
  p: pointer;
begin
  if (rec.ExceptionFlags and EXCEPTION_UNWIND) <> 0 then begin
    frame.Addr := @NullHandler;
    TUnwindProc(frame.HandlerArg)(frame._EBP);
  end;
  result := ExceptionContinueSearch;
end;

function __FPC_except_handler(
  var rec: EXCEPTION_RECORD;
  var frame: TSEHFrame;
  var context: TCONTEXT;
  var dispatch: TDispatcherContext): EXCEPTION_DISPOSITION; cdecl; [public, alias: '__FPC_except_handler'];
var
  p: pointer;
begin
  {$ifdef CPU86}
  CommonHandler(rec, frame, context, frame.HandlerArg);
  result := ExceptionContinueSearch;
  exit;
  {$else}
  // ???
  // on X64 the "specific_handler" is called
  result := ExceptionContinueSearch;
  {$endif}
end;

function __FPC_default_handler(
  var rec: EXCEPTION_RECORD;
  var frame: TSEHFrame;
  var context: TCONTEXT;
  var dispatch: TDispatcherContext): EXCEPTION_DISPOSITION; cdecl; [public, alias: '__FPC_DEFAULT_HANDLER'];
begin
  result := ExceptionContinueSearch;
end;

function __FPC_except_safecall(
  var rec: EXCEPTION_RECORD;
  var frame: TSEHFrame;
  var context: TCONTEXT;
  var dispatch: TDispatcherContext): EXCEPTION_DISPOSITION; cdecl; [public, alias: '__FPC_except_safecall'];
begin
  asm
    jmp  __FPC_except_handler;
  end;
end;

function __FPC_on_handler(
  var rec: EXCEPTION_RECORD;
  var frame: TSEHFrame;
  var context: TCONTEXT;
  var dispatch: TDispatcherContext): EXCEPTION_DISPOSITION; cdecl; [public, alias: '__FPC_on_handler'];
var
  TargetAddr: Pointer;
begin
  {$ifdef CPU86}
  if (rec.ExceptionFlags and EXCEPTION_UNWIND) = 0 then begin
    TargetAddr := FilterException(rec, 0, PtrUInt(frame.HandlerArg), Abs(RunErrorCode386(rec, context)));
    if TargetAddr <> nil then CommonHandler(rec, frame, context, TargetAddr);
  end;
  {$endif}

  result := ExceptionContinueSearch;
end;

// we need to return the exception object to dispose it
function fpc_popobjectstack: TObject; [public , alias: 'FPC_POPOBJECTSTACK']; compilerproc;
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

function internal_popobjectstack: TObject; external name 'FPC_POPOBJECTSTACK';

procedure fpc_doneexception; [public, alias: 'FPC_DONEEXCEPTION'] compilerproc;
begin
  // X86 is freed somewhere else
end;

procedure fpc_reraise; [public, alias: 'FPC_RERAISE']; compilerproc;
begin
end;

procedure fpc_raise_nested; [public, alias: 'FPC_RAISE_NESTED']; compilerproc;
begin
end;

procedure fpc_raiseexception(obj: TObject; anaddr: codepointer; aframe: pointer); [public, alias: 'FPC_RAISEEXCEPTION']; compilerproc;
begin
  writeln('fpc_raiseexception');

  if obj = nil then writeln('obj nil');
  if anaddr = nil then writeln('anaddr nil');
  if aframe = nil then writeln('aframe nil');

  if obj <> nil then begin
    writeln('exception message = ', TTestException(obj).test);
  end;
end;

{$endif}

