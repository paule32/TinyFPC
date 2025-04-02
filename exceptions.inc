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

//{$define FULLEXCEPTIONS} // <- for testing

{$ifdef codeh}

const
  { Exceptions raised by RTL use this code }
  FPC_EXCEPTION_CODE = $E0465043;

  STATUS_ACCESS_VIOLATION         = $C0000005;
  STATUS_IN_PAGE_ERROR            = $C0000006;
  STATUS_INVALID_HANDLE           = $C0000008;
  STATUS_NO_MEMORY                = $C0000017;
  STATUS_ILLEGAL_INSTRUCTION      = $C000001D;
  STATUS_NONCONTINUABLE_EXCEPTION = $C0000025;
  STATUS_INVALID_DISPOSITION      = $C0000026;
  STATUS_ARRAY_BOUNDS_EXCEEDED    = $C000008C;
  STATUS_FLOAT_DENORMAL_OPERAND   = $C000008D;
  STATUS_FLOAT_DIVIDE_BY_ZERO     = $C000008E;
  STATUS_FLOAT_INEXACT_RESULT     = $C000008F;
  STATUS_FLOAT_INVALID_OPERATION  = $C0000090;
  STATUS_FLOAT_OVERFLOW           = $C0000091;
  STATUS_FLOAT_STACK_CHECK        = $C0000092;
  STATUS_FLOAT_UNDERFLOW          = $C0000093;
  STATUS_INTEGER_DIVIDE_BY_ZERO   = $C0000094;
  STATUS_INTEGER_OVERFLOW         = $C0000095;
  STATUS_PRIVILEGED_INSTRUCTION   = $C0000096;
  STATUS_STACK_OVERFLOW           = $C00000FD;
  STATUS_CONTROL_C_EXIT           = $C000013A;
  STATUS_FLOAT_MULTIPLE_FAULTS    = $C00002B4;
  STATUS_FLOAT_MULTIPLE_TRAPS     = $C00002B5;
  STATUS_REG_NAT_CONSUMPTION      = $C00002C9;

  { exception flags (not everything applies to Win32!) }
  EXCEPTION_NONCONTINUABLE  = $01;
  EXCEPTION_UNWINDING       = $02;
  EXCEPTION_EXIT_UNWIND     = $04;
  EXCEPTION_STACK_INVALID   = $08;
  EXCEPTION_NESTED_CALL     = $10;
  EXCEPTION_TARGET_UNWIND   = $20;
  EXCEPTION_COLLIDED_UNWIND = $40;

  CONTEXT_X86                = $00010000;
  CONTEXT_CONTROL            = CONTEXT_X86 or $00000001;
  CONTEXT_INTEGER            = CONTEXT_X86 or $00000002;
  CONTEXT_SEGMENTS           = CONTEXT_X86 or $00000004;
  CONTEXT_FLOATING_POINT     = CONTEXT_X86 or $00000008;
  CONTEXT_DEBUG_REGISTERS    = CONTEXT_X86 or $00000010;
  CONTEXT_EXTENDED_REGISTERS = CONTEXT_X86 or $00000020;
  CONTEXT_FULL               = CONTEXT_CONTROL or CONTEXT_INTEGER or CONTEXT_SEGMENTS;

  EXCEPTION_UNWIND = EXCEPTION_UNWINDING or EXCEPTION_EXIT_UNWIND;

  EXCEPTION_MAXIMUM_PARAMETERS = 15;
  MAXIMUM_SUPPORTED_EXTENSION  = 512;

  Default8087CW: Word = $1332;
  DefaultMXCSR: DWord = $1900;

  { Signals }
  const
    SIGABRT   = 288;
    SIGFPE    = 289;
    SIGILL    = 290;
    SIGSEGV   = 291;
    SIGTERM   = 292;
    SIGALRM   = 293;
    SIGHUP    = 294;
    SIGINT    = 295;
    SIGKILL   = 296;
    SIGPIPE   = 297;
    SIGQUIT   = 298;
    SIGUSR1   = 299;
    SIGUSR2   = 300;
    SIGNOFP   = 301;
    SIGTRAP   = 302;
    SIGTIMR   = 303;    { Internal for setitimer (SIGALRM, SIGPROF) }
    SIGPROF   = 304;
    SIGMAX    = 320;

    SIG_BLOCK   = 1;
    SIG_SETMASK = 2;
    SIG_UNBLOCK = 3;

type
  EXCEPTION_DISPOSITION = (
    ExceptionContinueExecution,
    ExceptionContinueSearch,
    ExceptionNestedException,
    ExceptionCollidedUnwind
  );

  PEXCEPTION_RECORD = ^EXCEPTION_RECORD;
  EXCEPTION_RECORD = record
    ExceptionCode: DWORD;
    ExceptionFlags: DWORD;
    ExceptionRecord: PEXCEPTION_RECORD;
    ExceptionAddress: Pointer;
    NumberParameters: DWORD;
    ExceptionInformation: array[0..(EXCEPTION_MAXIMUM_PARAMETERS)-1] of ULONG_PTR;
  end;

  PSEHFrame = ^TSEHFrame;
  TSEHFrame = record
    Next: PSEHFrame;
    Addr: Pointer;
    _EBP: PtrUint;
    HandlerArg: Pointer;
  end;

  { Exception object stack }
  PExceptObject = ^TExceptObject;
  TExceptObject = record
    FObject: TObject;
    Addr: codepointer;
    Next: PExceptObject;
    refcount: longint;
    Framecount: longint;
    Frames: PCodePointer;
    {$ifdef FPC_USE_WIN32_SEH}
    SEHFrame: Pointer;
    ExceptRec: Pointer;
    ReraiseBuf: jmp_buf;
    {$endif FPC_USE_WIN32_SEH}
  end;

  PFilterRec = ^TFilterRec;
  TFilterRec = record
    RvaClass: DWord;
    RvaHandler: DWord;
  end;

  PEXCEPTION_FRAME = ^TEXCEPTION_FRAME;
  TEXCEPTION_FRAME = record
    next: PEXCEPTION_FRAME;
    handler: pointer;
  end;

  TUnwindProc = procedure(frame: PtrUInt);
  TExceptObjProc = function(code: LongInt; const rec: EXCEPTION_RECORD): Pointer; { Exception }
  TExceptClsProc = function(code: LongInt): Pointer; { ExceptClass }
  TErrorProc = procedure (ErrNo: LongInt; Address: CodePointer; Frame: Pointer);

var
  ExceptAddrStack:   PExceptAddr;
  ExceptObjectStack: PExceptObject;
  //ExceptTryLevel:   ObjpasInt;

  ExceptObjProc: TExceptObjProc = nil;
  ExceptClsProc: TExceptClsProc = nil;
  ErrorProc: TErrorProc = nil;

{$endif}

{$ifdef codei}

function FilterException(var rec: EXCEPTION_RECORD; imagebase: PtrUInt; filterRva: DWord; errcode: longint): Pointer;
var
  exclass: TClass;
  i: integer;
  filter, p: Pointer;
  curfilt: PFilterRec;
begin
  result := nil;

  if rec.ExceptionCode = FPC_EXCEPTION_CODE then
    TObject(rec.ExceptionInformation[1]).ClassType
  else if ExceptClsProc <> nil then begin
    exclass := TClass(TExceptClsProc(ExceptClsProc)(errcode));
  end else
    exit; // cant proceed

  // every "on" in except block is a filter
  filter := pointer(imagebase+filterRva);
  for i := 0 to PLongint(filter)^-1 do begin
    curfilt := @PFilterRec(filter+sizeof(LongInt))[i];
    // pointer to handler
    p := pointer(imagebase+curfilt^.RvaHandler);
    // i dont know why, but thats how it is in the original RTL
    if curfilt^.RvaClass = $FFFFFFFF then exit(p);
    // check if class is valid
    {$ifdef VER3_2}
    if exclass.InheritsFrom(TClass(imagebase+curfilt^.RvaClass)) then exit(p);
    {$else}
    // assume its trunk
    if exclass.InheritsFrom(PClass(imagebase+CurFilt^.RvaClass)^) then exit(p);
    {$endif}
  end;
end;

function RunErrorCode(const rec: EXCEPTION_RECORD): longint;
begin
  { negative result means 'FPU reset required' }
  case rec.ExceptionCode of
    STATUS_INTEGER_DIVIDE_BY_ZERO:        result := 200;  // reDivByZero
    STATUS_FLOAT_DIVIDE_BY_ZERO:          result := -208; // !!reZeroDivide
    STATUS_ARRAY_BOUNDS_EXCEEDED:         result := 201;  // reRangeError
    STATUS_STACK_OVERFLOW:                result := 202;  // reStackOverflow
    STATUS_FLOAT_OVERFLOW:                result := -205; // reOverflow
    STATUS_FLOAT_DENORMAL_OPERAND,
    STATUS_FLOAT_UNDERFLOW:               result := -206; // reUnderflow
    STATUS_FLOAT_INEXACT_RESULT,
    STATUS_FLOAT_INVALID_OPERATION,
    STATUS_FLOAT_STACK_CHECK:             result := -207; // reInvalidOp
    STATUS_INTEGER_OVERFLOW:              result := 215;  // reIntOverflow
    STATUS_ILLEGAL_INSTRUCTION:           result := -216;
    STATUS_ACCESS_VIOLATION:              result := 216;  // reAccessViolation
    STATUS_CONTROL_C_EXIT:                result := 217;  // reControlBreak
    STATUS_PRIVILEGED_INSTRUCTION:        result := 218;  // rePrivilegedInstruction
    STATUS_FLOAT_MULTIPLE_TRAPS,
    STATUS_FLOAT_MULTIPLE_FAULTS:         result := -255; // indicate FPU reset
  else
    result := 255; // reExternalException
  end;
end;

procedure TranslateMxcsr(mxcsr: longword; var code: longint);
begin
  { we can return only one value, further one's are lost }
  { InvalidOp }
  if (mxcsr and 1) <> 0 then code := -207
  { Denormal }
  else if (mxcsr and 2) <> 0 then code := -206
  { !!reZeroDivide }
  else if (mxcsr and 4) <> 0 then code := -208
  { reOverflow }
  else if (mxcsr and 8) <> 0 then code := -205
  { Underflow }
  else if (mxcsr and 16) <> 0 then code := -206
  { Precision }
  else if (mxcsr and 32) <> 0 then code := -207
  else { this should not happen }
    code := -255;
end;

{$endif}

{$ifdef CPU64}
  {$I exceptions64.inc}
{$else}
  {$I exceptions32.inc}
{$endif}


