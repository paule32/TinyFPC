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

unit system;

{$mode objfpc}{$H+}
{$modeswitch advancedrecords}
{$macro on}

// @@todo: reorder functions; interface vs implementation mismatch

// system.pas(9,1) Error: Internal error 2016060303

// IE 2006012304 means "compile system.pas first", and happens on latest trunk

{$WARN 6058 off : Call to subroutine "$1" marked as inline is not inlined}

interface

const rtllib  = 'rtllib.dll';

type
  TByteLookup = array[0..255] of Byte;
  PByteLookup = ^TByteLookup;
  
const
  CP_ACP     = 0;     // default to ANSI code page
  CP_OEMCP   = 1;     // default to OEM (console) code page
  CP_UTF16   = 1200;  // utf-16
  CP_UTF16BE = 1201;  // unicodeFFFE
  CP_UTF7    = 65000; // utf-7
  CP_UTF8    = 65001; // utf-8
  CP_ASCII   = 20127; // us-ascii
  CP_NONE    = $FFFF; // rawbytestring encoding

// -- types -----------------------------------------

{$I innr.inc}
{$I system_types.inc}     
{$I filerec.inc}

// -- internproc ------------------------------------

{$I math.inc}

const
  ExitProc: CodePointer = nil;
  ErrorAddr: CodePointer = nil;
  ErrorCode: Word = 0;
  ErrorBase: Pointer = nil; public name 'FPC_ERRORBASE';

// -- imports ---------------------------------------

const                         
  USER32   = 'user32.dll';
  KERNEL32 = 'kernel32.dll';
  NTDLL    = 'ntdll.dll';
  RTLDLL   = 'rtllib.dll';
                                             
function MessageBoxA(hWnd: HWND;lpText:LPCSTR; lpCaption: LPCSTR; uType: UINT): longint; stdcall; external USER32;
procedure ExitProcess(ExitCode: longint); stdcall; external KERNEL32;
procedure RtlMoveMemory(Destination: PVOID; const Source: PVOID; Length: size_t); stdcall; external KERNEL32;
function VirtualAlloc(lpAddress: Pointer; dwSize: SizeUInt; flAllocationType, flProtect: DWORD): Pointer; stdcall; external KERNEL32;
function VirtualFree(lpAddress: LPVOID; dwSize: size_t; dwFreeType: DWORD): BOOL; stdcall; external KERNEL32;
function VirtualProtect(lpAddress: LPVOID; dwSize: SIZE_T; flNewProtect: DWORD; var lpflOldProtect: DWORD): BOOL; stdcall; external KERNEL32;
procedure RtlUnwind(TargetFrame: Pointer; TargetIp: Pointer; ExceptionRecord: Pointer; ReturnValue: Pointer); stdcall; external NTDLL;
//procedure RtlUnwindEx(TargetFrame: Pointer; TargetIp: Pointer; ExceptionRecord: PExceptionRecord; ReturnValue: Pointer; OriginalContext: PContext; HistoryTable: PUNWIND_HISTORY_TABLE); stdcall; external KERNEL32;
procedure RtlUnwindEx(TargetFrame: Pointer; TargetIp: Pointer; ExceptionRecord: Pointer; ReturnValue: Pointer; OriginalContext: Pointer; HistoryTable: Pointer); stdcall; external KERNEL32;
//function SetUnhandledExceptionFilter(lpTopLevelExceptionFilter: Pointer): Pointer; stdcall; external KERNEL32;
function AddVectoredExceptionHandler(FirstHandler: ULONG; VectoredHandler: Pointer): Pointer; stdcall; external KERNEL32;
function RemoveVectoredExceptionHandler(Handle: Pointer): LongBool; stdcall; external KERNEL32;

{$undef codei} {$define codeh} {$I objects.inc}
{$undef codei} {$define codeh} {$I exceptions.inc}
{$undef codei} {$define codeh} {$I rtti.inc}
{$undef codei} {$define codeh} {$I io.inc}
{$undef codei} {$define codeh} {$I strings.inc}   
{$undef codei} {$define codeh} {$I unicode.inc}
{$undef codei} {$define codeh} {$I ustrings.inc}
{$undef codei} {$define codeh} {$I wstrings.inc}
{$undef codei} {$define codeh} {$I misc.inc}  
{$undef codei} {$define codeh} {$I constarray.inc}
{$undef codei} {$define codeh} {$I heap.inc}

// raise exception test
type
  TTestException = class
    test: ansistring;
    constructor Create(msg: ansistring);
  end;

// -- unnamed things --------------------------------

// required at rebase to one commit before 5bb121e91cc266a93c300054cd6edfeb85a37da9
procedure fpc_popaddrstack; compilerproc;
function fpc_pushexceptaddr(ft: longint; _buf, _newaddr: pointer): pjmp_buf; compilerproc;
function fpc_setjmp(var s: jmp_buf): longint; assembler; compilerproc;
procedure fpc_longjmp(var s: jmp_buf; value: LongInt); assembler; compilerproc;

procedure HandleErrorAddrFrame(Errno: longint; addr: CodePointer; frame: Pointer); {$ifdef CPUI386} register; {$endif}

// -- symbols ---------------------------------------

// @@todo
// add more of them from system.inc
// fpc_overflow is 215 IntOverflow
// there is div by zero, range chec etc

procedure fpc_overflow; compilerproc;

// -- dynamic arrays --------------------------------

type
  tdynarrayindex = sizeint;
  pdynarrayindex = ^tdynarrayindex;

  pdynarray = ^tdynarray;
  tdynarray = record
    refcount: ptrint;
    high: tdynarrayindex;
  end;

  pdynarraytypedata = ^tdynarraytypedata;
  tdynarraytypedata = packed record
    case TTypeKind of
      tkArray: (
        elSize: SizeUInt;
        elType2: PPointer;
        varType: Longint;
        elType: PPointer;
      );
      { include for proper alignment }
      tkInt64: (
        dummy: Int64
      );
  end;

procedure fpc_dynarray_setlength(var p: pointer; pti: pointer; dimcount: sizeint; dims: pdynarrayindex); compilerproc;
function fpc_dynarray_length(p: pointer): tdynarrayindex; compilerproc;
function fpc_dynarray_high(p: pointer): tdynarrayindex; compilerproc;
procedure fpc_dynarray_incr_ref(p: pointer); compilerproc;
procedure fpc_dynarray_clear(var p: pointer; ti: pointer); compilerproc;

// -- console mode support --------------------------

{$ifdef CONSOLE}
const
  STD_INPUT_HANDLE  = DWORD(-10);
  STD_OUTPUT_HANDLE = DWORD(-11);
  STD_ERROR_HANDLE  = DWORD(-12);

function GetStdHandle(nStdHandle: DWORD): HANDLE; stdcall; external KERNEL32;
function SetConsoleMode(hConsoleHandle: HANDLE; dwMode: DWORD): BOOL; stdcall; external KERNEL32;
function WriteConsoleA(hConsoleOutput: HANDLE; lpBuffer: PVOID; nNumberOfCharsToWrite: DWORD; lpNumberOfCharsWritten: LPDWORD; lpReserved: LPVOID): BOOL; stdcall; external KERNEL32;
function WriteConsoleW(hConsoleOutput: HANDLE; lpBuffer: PVOID; nNumberOfCharsToWrite: DWORD; lpNumberOfCharsWritten: LPDWORD; lpReserved: LPVOID): BOOL; stdcall; external KERNEL32;
function ReadConsoleA(hConsoleInput: HANDLE; lpuffer: LPVOID; nNumberOfCharsToRead: DWORD; lpNumberOfCharsRead: LPDWORD; pInputControl: LPVOID): BOOL; stdcall; external KERNEL32;
function ReadConsoleW(hConsoleInput: HANDLE; lpuffer: LPVOID; nNumberOfCharsToRead: DWORD; lpNumberOfCharsRead: LPDWORD; pInputControl: LPVOID): BOOL; stdcall; external KERNEL32;
{$endif}

// -- custom function -------------------------------

{$if defined(VER3_2) or defined(VER3_0)}
function Chr(b: byte): AnsiChar; [INTERNPROC: fpc_in_chr_byte];
{$endif defined(VER3_2) or defined(VER3_0)}

procedure msgdebug(msg: ansistring);

// -- various rountines -----------------------------

procedure fpc_initializeunits; compilerproc;
procedure fpc_finalizeunits; compilerproc;
procedure fpc_libinitializeunits; compilerproc;

procedure internal_do_exit; external name 'FPC_DO_EXIT'; // NOT in implementation! no code, external import from compiler
procedure Halt(err: integer); noreturn;
procedure fpc_do_exit; compilerproc;
procedure _fpc_leave(a1, a2, a3, a4: pointer); stdcall; compilerproc; assembler;

// these 3 needed to make int64 version of itoa()
//function fpc_mod_int64(n, z: int64): int64; compilerproc;
//function fpc_mul_int64(f1, f2: int64): int64; compilerproc;
function fpc_div_qword(n, z: qword): qword; compilerproc;
function fpc_div_int64(n, z: int64): int64; compilerproc;

type
  TEntryInformation = record
    InitFinalTable : Pointer;
    ThreadvarTablesTable : Pointer;
    ResourceStringTables : Pointer;
    ResStrInitTables : Pointer;
    ResLocation : Pointer;
    PascalMain : Procedure;
    valgrind_used : boolean;
  end;

var
  StdIn, StdOut, StdErr: HANDLE;       // initialized in sysinitpas if app type is CONSOLE
  EntryInformation: TEntryInformation; // @@todo: check if EntryInformation is used
  //RandSeed: Cardinal;                // @@todo: currently unused
  ExitCode: LongInt;           public name 'operatingsystem_result';
  is_library: boolean = false; public name 'operatingsystem_islibrary';
  is_console: boolean = false; public name 'operatingsystem_isconsole';
  IsMultithread: boolean = false;

Procedure fpc_Copy_proc (Src, Dest, TypeInfo : Pointer); compilerproc; inline;

function  fpcsetjmp(var s: jmp_buf): longint; stdcall; export;
procedure fpclongjmp(var s: jmp_buf; value: LongInt); stdcall; export;

procedure fpchandleerror(errno: longint); stdcall; export;
function  fpcdivint64(n, z: int64): int64; stdcall; export;
function  fpcdivqword(n, z: qword): qword; stdcall; export;

function BsrDWord_(Const AValue : DWord): cardinal; stdcall export;
function BsrQWord_(Const AValue : QWord): cardinal; stdcall export;

procedure fpcdynarraysetlength(var p: pointer; pti: pointer; dimcount: sizeint; dims: pdynarrayindex); stdcall; export;

implementation

uses xmm;

procedure HandleError(errno: LongInt); external name 'FPC_HANDLEERROR';

procedure wait_for_enter;
var
  c: char;
  d: dword;
begin
  writeln('press <enter>...');

  while true do begin
    ReadConsoleA(StdIn, @c, 1, @d, nil);
    if c = #13 then break;
  end;
end;

{$undef codeh} {$define codei} {$I objects.inc}
{$undef codeh} {$define codei} {$I exceptions.inc}
{$undef codeh} {$define codei} {$I rtti.inc}
{$undef codeh} {$define codei} {$I io.inc}
{$undef codeh} {$define codei} {$I strings.inc}   
{$undef codeh} {$define codei} {$I unicode.inc}
{$undef codeh} {$define codei} {$I ustrings.inc}
{$undef codeh} {$define codei} {$I wstrings.inc}
{$undef codeh} {$define codei} {$I misc.inc}
{$undef codeh} {$define codei} {$I constarray.inc}
{$undef codeh} {$define codei} {$I heap.inc}

// -- custom function -------------------------------

//{$if defined(VER3_2) or defined(VER3_0)}
//function Chr(v: byte): char;
//begin
//  pbyte(@result)^ := v;
//end;
//{$endif defined(VER3_2) or defined(VER3_0)}

procedure msgdebug(msg: ansistring);
begin
  MessageBoxA(0, @msg[1], 'DEBUG', 0);
end;

// INITIALIZATION
const
  MAXUNITS = 1024;

type
  TInitFinalRec = record
    InitProc: TProcedure;
    FinalProc: TProcedure;
  end;
               
  PInitFinalTable = ^TInitFinalTable;
  TInitFinalTable = record
    TableCount, InitCount: PtrUInt;
    Procs: array[1..MAXUNITS] of TInitFinalRec;
  end;

var
  InitFinalTable: TInitFinalTable; external name 'INITFINAL';

procedure fpc_initializeunits; [public, alias: 'FPC_INITIALIZEUNITS'];
var
  i: integer;
begin
  for i := 1 to InitFinalTable.TableCount do begin
    if InitFinalTable.Procs[i].InitProc <> nil then begin
      InitFinalTable.Procs[i].InitProc();
    end;
  end;
end;

// for internal use
procedure fpc_initializeunits; [external name 'FPC_INITIALIZEUNITS'];


{ define alias for internal use in the system unit }
Function fpc_Copy_internal (Src, Dest, TypeInfo : Pointer) : SizeInt;[external name 'FPC_COPY'];

Function fpc_Copy (Src, Dest, TypeInfo : Pointer) : SizeInt;[Public,alias : 'FPC_COPY']; compilerproc;
begin
  result:=sizeof(pointer);
end;

procedure fpc_Copy_proc (Src, Dest, TypeInfo : Pointer);compilerproc; inline;
begin
  fpc_copy_internal(src,dest,typeinfo);
end;


// internal use only
procedure fpc_finalizeunits; [public, alias: 'FPC_FINALIZEUNITS'];
var
  i: integer;
begin
  for i := 1 to InitFinalTable.TableCount do begin
    if InitFinalTable.Procs[i].FinalProc <> nil then begin
      InitFinalTable.Procs[i].FinalProc();
    end;
  end;
end;

// for internal use
procedure fpc_finalizeunits; [external name 'FPC_FINALIZEUNITS'];

// .DLL
procedure fpc_libinitializeunits; [public, alias: 'FPC_LIBINITIALIZEUNITS']; compilerproc;
begin
  is_library := true;
  fpc_initializeunits;
end;

procedure Halt(err: integer); noreturn;
Begin
  internal_do_exit;
end;

procedure fpc_do_exit; [public, alias: 'FPC_DO_EXIT'];
begin
  fpc_finalizeunits;
  ExitProcess(ExitCode);
end;

{ Parameters are dummy and used to force "ret 16" at the end;
  this removes a TSEHFrame record from the stack }
{$asmmode att}
procedure _fpc_leave(a1, a2, a3, a4: pointer); [public, alias: '_FPC_leave']; stdcall; compilerproc; assembler; nostackframe;
asm
  movl   4(%esp),%eax
  movl   %eax,%fs:(0)
  movl   %ebp,%eax
  call   16(%esp)
end;

procedure fpc_emptymethod; [public, alias : 'FPC_EMPTYMETHOD'];
begin
end;

//function fpc_mod_int64(n, z: int64): int64; [public, alias: 'FPC_MOD_INT64']; compilerproc;
//begin
//end;
//
//function fpc_mul_int64(f1, f : int64) : int64; [public, alias: 'FPC_MUL_INT64']; compilerproc;
//begin
//end;

// MOVE THESE!!!!!!!!!!!!

function GetBsr8bit: PByteLookup; stdcall; external rtllib;
function BsrByte(Const AValue: Byte): Byte;
var
  bsr: PByteLookup;
begin
  bsr := GetBsr8bit;
  result := bsr^[AValue];
end;

{$ifdef DLLEXPORT}
function BsrDWord_(Const AValue : DWord): cardinal; stdcall; export;
var
  tmp: DWord;
begin
  result:=ord(AValue>$FFFF)*16;
  tmp:=AValue shr result;
  result:=result or (ord(tmp>$FF)*8);
  tmp:=tmp shr (result and 8);
  result:=result or BsrByte(byte(tmp));
end;
function BsrDWord(Const AValue : DWord): cardinal;
begin
  result := BsrDWord_(AValue);
end;
{$endif DLLEXPORT}
{$ifdef DLLIMPORT}
function BsrDWord_(Const AValue : DWord): cardinal; stdcall external RTLDLL;
function BsrDWord(Const AValue : DWord): cardinal;
begin
  result := BsrDWord_(AValue);
end;
{$endif DLLIMPORT}

{$ifdef DLLEXPORT}
function BsrQWord_(Const AValue : QWord): cardinal; stdcall export;
var
  tmp: DWord;
begin
  result:=32;
  tmp:=hi(AValue);
  if (tmp=0) then
    begin
      tmp:=lo(AValue);
      result:=0;
    end;
  result:=result or BsrDword(tmp);
end;
function BsrQWord(Const AValue : QWord): cardinal;
begin
  result := BsrQWord_(AValue);
end;
{$endif DLLEXPORT}
{$ifdef DLLIMPORT}
function BsrQWord_(Const AValue : QWord): cardinal; stdcall external RTLDLL;
function BsrQWord(Const AValue : QWord): cardinal;
begin
  result := BsrQWord_(AValue);
end;
{$endif DLLIMPORT}

{$ifdef DLLEXPORT}
function fpcdivqword(n, z: qword): qword; stdcall; export;
var
  shift, lzz, lzn: LongInt;
begin
  { Use the usually faster 32-bit division if possible }
  if (hi(z) = 0) and (hi(n) = 0) then begin
    fpcdivqword := Dword(z) div Dword(n);
    exit;
  end;

  fpcdivqword:=0;
  //if n=0 then HandleErrorAddrFrameInd(200,get_pc_addr,get_frame);
  //if z=0 then exit;
  lzz:=BsrQWord(z);
  lzn:=BsrQWord(n);
  { if the denominator contains less zeros }
  { than the numerator                     }
  { then d is greater than the n           }
  if lzn > lzz then exit;

  shift:=lzz-lzn;
  n:=n shl shift;
  for shift:=shift downto 0 do
  begin
  if z>=n then
  begin
  z:=z-n;
  fpcdivqword:=fpcdivqword+(qword(1) shl shift);
  end;
  n:=n shr 1;
  end;
end;
function fpc_div_qword(n, z: qword): qword; [public, alias: 'FPC_DIV_QWORD']; compilerproc;
begin
  result := fpcdivqword(n, z);
end;
{$endif DLLEXPORT}
{$ifdef DLLIMPORT}
function fpcdivqword(n, z: qword): qword; stdcall; external RTLDLL;
function fpc_div_qword(n, z: qword): qword; [public, alias: 'FPC_DIV_QWORD']; compilerproc;
begin
  result := fpcdivqword(n, z);
end;
{$endif}

{$ifdef DLLEXPORT}
function fpcdivint64(n, z: int64): int64; stdcall; export;
var
  sign: boolean;
  q1, q2: qword;
begin
  //if n = 0 then HandleError(200); // div by zero
  // original: HandleErrorAddrFrameInd(200,get_pc_addr,get_frame);
  // @@todo: ^ search for this, leads to exceptions (implementation?) and other errors that must be handled
  // interesting syntax found: raise TObject(nil) at nil, nil;
  sign := false;
  if z < 0 then begin
    sign := not sign;
    q1 := qword(-z);
  end
  else
    q1 := z;

  if n < 0 then begin
    sign := not sign;
    q2 := qword(-n);
  end
  else
    q2 := n;

  if sign then
    result := -(q1 div q2)
  else
    result := q1 div q2;
end;
function fpc_div_int64(n, z: int64): int64; [public, alias: 'FPC_DIV_INT64']; compilerproc;
begin
  result := fpcdivint64(n, z);
end;
{$endif DLLEXPORT}
{$ifdef DLLIMPORT}
function fpcdivint64(n, z: int64): int64; stdcall; external RTLDLL;
function fpc_div_int64(n, z: int64): int64; [public, alias: 'FPC_DIV_INT64']; compilerproc;
begin
  result := fpcdivint64(n, z);
end;
{$endif}

// called by HandleError
{$ifdef DLLEXPORT}
procedure fpchandleerror(errno: longint); stdcall; export;
const
  errmap: array[200..236] of ansistring = (
    'DivByZero',        'RangeError',      'StackOverflow',     '203',            '204',
    'Overflow',         'Underflow',       'InvalidOp',         'ZeroDivide',     '209',
    'ObjectCheckError', 'AbstractError',   'ExternalException', '213',            'BusError',
    'IntOverflow',      'AccessViolation', 'ControlC',          'Privilege',      'InvalidCast',
    'InvalidVarCast',   'InvalidVarOp',    'DispatchError',     'VarArrayCreate', 'VarNotArray',
    'VarArrayBounds',   '226',             'AssertionFailed',   'IntfCastError',  'SafecallException',
    '230',              'iconvError',      'NoThreadSupport',   'SigQuit',        'MissingWStringManager',
    'NoDynLibsSupport', 'ThreadError'
  );
begin
  writeln('fpc_handleerror, errno = ', errno, ', meaning = ', errmap[errno]);
end;
procedure fpc_handleerror(errno: longint); compilerproc; [public, alias: 'FPC_HANDLEERROR'];
begin
  fpchandleerror(errno);
end;
{$endif DLLEXPORT}
{$ifdef DLLIMPORT}
procedure fpchandleerror(errno: longint); stdcall; external RTLDLL;
procedure fpc_handleerror(errno: longint); compilerproc; [public, alias: 'FPC_HANDLEERROR'];
begin
  fpchandleerror(errno);
end;
{$endif}

// required at rebase to one commit before 5bb121e91cc266a93c300054cd6edfeb85a37da9
procedure fpc_popaddrstack; [public, alias: 'FPC_POPADDRSTACK']; compilerproc;
begin   
  writeln('fpc_popaddrstack');
end;

function fpc_pushexceptaddr(ft: longint; _buf, _newaddr: pointer): pjmp_buf; [public, alias: 'FPC_PUSHEXCEPTADDR']; compilerproc;
begin
  writeln('fpc_pushexceptaddr');
end;

{$asmmode att}
{$ifdef CPU86}
function fpc_setjmp(var s: jmp_buf): longint; assembler; nostackframe; [public, alias: 'FPC_SETJMP']; compilerproc;
asm
  movl %ebx,jmp_buf.ebx(%eax)
  movl %esi,jmp_buf.esi(%eax)
  movl %edi,jmp_buf.edi(%eax)
  movl %ebp,jmp_buf.bp(%eax)
  leal 4(%esp),%edi
  movl %edi,jmp_buf.sp(%eax)
  movl (%esp),%edi
  movl %edi,jmp_buf.pc(%eax)
  {$ifdef FPC_USE_WIN32_SEH}
    movl %fs:(0),%edi
    movl %edi,jmp_buf.exhead(%eax)
  {$endif FPC_USE_WIN32_SEH}
  movl jmp_buf.edi(%eax),%edi
  xorl %eax,%eax
end;
{$else}
  {$ifdef DLLEXPORT}
  function fpcsetjmp(var s: jmp_buf): longint; stdcall; assembler; nostackframe; export;
  asm
    movq     %rbx,jmp_buf.rbx(%rcx)
    movq     %rbp,jmp_buf.rbp(%rcx)
    movq     %r12,jmp_buf.r12(%rcx)
    movq     %r13,jmp_buf.r13(%rcx)
    movq     %r14,jmp_buf.r14(%rcx)
    movq     %r15,jmp_buf.r15(%rcx)
    movq     %rsi,jmp_buf.rsi(%rcx)
    movq     %rdi,jmp_buf.rdi(%rcx)
    leaq     8(%rsp),%rax
    movq     %rax,jmp_buf.rsp(%rcx)
    movq     (%rsp),%rax
    movq     %rax,jmp_buf.rip(%rcx)
    movdqu   %xmm6,jmp_buf.xmm6(%rcx)
    movdqu   %xmm7,jmp_buf.xmm7(%rcx)
    movdqu   %xmm8,jmp_buf.xmm8(%rcx)
    movdqu   %xmm9,jmp_buf.xmm9(%rcx)
    movdqu   %xmm10,jmp_buf.xmm10(%rcx)
    movdqu   %xmm11,jmp_buf.xmm11(%rcx)
    movdqu   %xmm12,jmp_buf.xmm12(%rcx)
    movdqu   %xmm13,jmp_buf.xmm13(%rcx)
    movdqu   %xmm14,jmp_buf.xmm14(%rcx)
    movdqu   %xmm15,jmp_buf.xmm15(%rcx)
    stmxcsr  jmp_buf.mxcsr(%rcx)
    fnstcw   jmp_buf.fpucw(%rcx)
    xorl     %eax,%eax
  end;
  {$asmmode intel}
  function fpc_setjmp(var s: jmp_buf): longint; assembler; nostackframe; [public, alias: 'FPC_SETJMP']; compilerproc;
  asm
    sub rsp, 32          // Shadow space für call
    call fpcsetjmp       // bar erwartet s in rcx
    add rsp, 32          // Stack wieder freigeben
    // Rückgabewert ist bereits in RAX
  end;
  {$endif DLLEXPORT}
  {$ifdef DLLIMPORT}
  {$asmmode intel}
  function fpcsetjmp(var s: jmp_buf): longint; stdcall; external RTLDLL;
  function fpc_setjmp(var s: jmp_buf): longint; assembler; nostackframe; [public, alias: 'FPC_SETJMP']; compilerproc;
  asm
    sub rsp, 32          // Shadow space für call
    call fpcsetjmp       // bar erwartet s in rcx
    add rsp, 32          // Stack wieder freigeben
    // Rückgabewert ist bereits in RAX
  end;
  {$endif DLLIMPORT}
{$endif CPU86}

{$ifdef DLLEXPORT}
{$asmmode att}
procedure fpclongjmp(var s: jmp_buf; value: LongInt); assembler; nostackframe; stdcall; export;
asm
  {$ifdef CPU86}
  xchgl %edx,%eax
  cmpl  $1,%eax
  adcl  $0,%eax
  {$ifdef FPC_USE_WIN32_SEH}
  movl  Jmp_buf.exhead(%edx),%edi
  movl  %edi,%fs:(0)
  {$endif FPC_USE_WIN32_SEH}
  movl Jmp_buf.ebx(%edx),%ebx
  movl Jmp_buf.esi(%edx),%esi
  movl Jmp_buf.edi(%edx),%edi
  movl Jmp_buf.bp(%edx),%ebp
  movl Jmp_buf.sp(%edx),%esp
  jmp Jmp_buf.pc(%edx)
  {$else}
  {$asmmode intel}
  cmpl     $1,%edx
  adcl     $0,%edx
  movl     %edx,%eax
  movq     jmp_buf.rbx(%rcx),%rbx
  movq     jmp_buf.rbp(%rcx),%rbp
  movq     jmp_buf.r12(%rcx),%r12
  movq     jmp_buf.r13(%rcx),%r13
  movq     jmp_buf.r14(%rcx),%r14
  movq     jmp_buf.r15(%rcx),%r15
  movq     jmp_buf.rsi(%rcx),%rsi
  movq     jmp_buf.rdi(%rcx),%rdi
  movq     jmp_buf.rsp(%rcx),%rsp
  movdqu   jmp_buf.xmm6(%rcx),%xmm6
  movdqu   jmp_buf.xmm7(%rcx),%xmm7
  movdqu   jmp_buf.xmm8(%rcx),%xmm8
  movdqu   jmp_buf.xmm9(%rcx),%xmm9
  movdqu   jmp_buf.xmm10(%rcx),%xmm10
  movdqu   jmp_buf.xmm11(%rcx),%xmm11
  movdqu   jmp_buf.xmm12(%rcx),%xmm12
  movdqu   jmp_buf.xmm13(%rcx),%xmm13
  movdqu   jmp_buf.xmm14(%rcx),%xmm14
  movdqu   jmp_buf.xmm15(%rcx),%xmm15
  ldmxcsr  jmp_buf.mxcsr(%rcx)
  fnclex
  fldcw    jmp_buf.fpucw(%rcx)
  jmpq     jmp_buf.rip(%rcx)
  {$endif}
end;
procedure fpc_longjmp(var s: jmp_buf; value: LongInt); assembler; nostackframe; compilerproc; [public, alias: 'FPC_LONGJMP'];
asm
  sub rsp, 64          // Shadow space für call
  call fpclongjmp      // bar erwartet s in rcx
  add rsp, 64          // Stack wieder freigeben
end;
{$endif DLLEXPORT}
{$ifdef DLLIMPORT}
procedure fpclongjmp(var s: jmp_buf; value: LongInt); stdcall; external RTLDLL;
procedure fpc_longjmp(var s: jmp_buf; value: LongInt); assembler; nostackframe; compilerproc; [public, alias: 'FPC_LONGJMP'];
asm
  sub rsp, 64          // Shadow space für call
  call fpclongjmp      // bar erwartet s in rcx
  add rsp, 64          // Stack wieder freigeben
end;
{$endif DLLIMPORT}

procedure HandleErrorAddrFrame(Errno: longint; addr: CodePointer; frame: Pointer); [public, alias: 'FPC_BREAK_ERROR']; {$ifdef CPUI386} register; {$endif}
begin
  if CodePointer(ErrorProc) <> nil then ErrorProc(Errno, addr, frame);
  ErrorCode := Word(Errno);
  ErrorAddr := addr;
  ErrorBase := frame;
  if ExceptAddrStack <> nil then raise TObject(nil) at addr, frame;
  Halt(ErrorCode);
end;

procedure fpc_overflow; [public, alias: 'FPC_OVERFLOW']; compilerproc;
begin
  Halt(215); // IntOverflow
end;

constructor TTestException.Create(msg: ansistring);
begin
  test := msg;
end;

// -- dynamic arrays --------------------------------

// how about change "pti" from pointer to pdynarraytypedata later?
{$ifdef DLLEXPORT}
procedure fpcdynarraysetlength(var p: pointer; pti: pointer; dimcount: sizeint; dims: pdynarrayindex); stdcall; export;
var
  elesize: sizeint;
  eletype, eletypemngd: pointer;
  ti: pointer;
  size: sizeint;
  newp: pdynarray;
begin
  // p        = array data
  // pti      = pdynarraytypedata
  // dimcount = its for nested array: setlength(arr, 5, 5), type array of array of...
  // dims     = value to set the length to

  //writeln('fpc_dynarray_setlength');
  //writeln('dimcount = ', dimcount);
  //writeln('dims = ', dims^);
  //writeln('dims[0] = ', dims[0]); // index 0 values to 5, well its the same as dims^

  if dimcount > 1 then begin
    // nested arrays not implemented
    exit;
  end;

  if dims[0] < 0 then begin
    // 201 RangeError should be thrown
    // and ExitProcess(201) called, or Halt(201)
  end;

  if dims[0] = 0 then begin
    // setlength to 0, so free the mem
    // @@todo
    exit;
  end;

  ti := pointer(pti)+2+pbyte(pti)[1];
  elesize := psizeuint(ti)^;
  //writeln('elesize = ', elesize);
  eletype := pdynarraytypedata(ti)^.elType2^;  
  //writeln('eletype = ', ptrint(eletype));

  // is array element managed?
  if assigned(pdynarraytypedata(ti)^.elType) then eletypemngd := pdynarraytypedata(ti)^.elType^ else eletypemngd := nil;
  //writeln('is managed? ', eletypemngd <> nil);

  { determine new memory size, throw a runtime error on overflow }
  // @@todo: check what helper me to see that IntOverflow! was it q+ or r+?
  {$push} {$q+,r+}
  size := elesize*dims[0]+sizeof(tdynarray);
  {$pop}
  //writeln('new size = ', size);

  if not assigned(p) then begin
    // now the Memory Manager goes in
    // we need to allocate memory for the array
    newp := xallocmem(size);
    //writeln('allocated mem at ', ptrint(newp));

    //if eletypemngd <> nil and (PTypeKind(eletype)^ in [tkRecord, tkObject]) then begin
    //  // @@todo: initialization
    //end;
  end else begin
    writeln('array needs reallocation');
    writeln('@@todo to be implemented!');
  end;

  p := pointer(newp)+sizeof(tdynarray);

  newp^.refcount := 1;
  newp^.high := dims[0]-1;
end;
procedure fpc_dynarray_setlength(var p: pointer; pti: pointer; dimcount: sizeint; dims: pdynarrayindex); [public, alias: 'FPC_DYNARR_SETLENGTH']; compilerproc;
begin
  fpcdynarraysetlength(p, pti, dimcount, dims);
end;
{$endif DLLEXPORT}
{$ifdef DLLIMPORT}
procedure fpcdynarraysetlength(var p: pointer; pti: pointer; dimcount: sizeint; dims: pdynarrayindex); stdcall; external RTLDLL;
procedure fpc_dynarray_setlength(var p: pointer; pti: pointer; dimcount: sizeint; dims: pdynarrayindex); [public, alias: 'FPC_DYNARR_SETLENGTH']; compilerproc;
begin
  fpcdynarraysetlength(p, pti, dimcount, dims);
end;
{$endif DLLIMPORT}

function fpc_dynarray_length(p: pointer): tdynarrayindex; [public, alias: 'FPC_DYNARRAY_LENGTH']; compilerproc;
begin
  if p = nil then exit(0);
  result := pdynarray(p-sizeof(tdynarray))^.high+1;
end;

function fpc_dynarray_high(p: pointer): tdynarrayindex; [public, alias: 'FPC_DYNARRAY_HIGH']; compilerproc;
begin
  if p = nil then exit(0);
  result := pdynarray(p-sizeof(tdynarray))^.high;
end;

procedure fpc_dynarray_incr_ref(p: pointer); compilerproc; [public, alias: 'FPC_DYNARRAY_INCR_REF'];
var
  d: pdynarray;
begin
  if p = nil then exit;
  d := pdynarray(p-sizeof(tdynarray));
  if d^.refcount = 0 then
    //HandleErrorAddrFrameInd(204, get_pc_addr, get_frame)
    HandleError(204)
  else if d^.refcount>0 then
    //inclocked(d^.refcount);
    // @@todo: thread safe
    inc(d^.refcount);
end;

procedure fpc_dynarray_clear(var p: pointer; ti: pointer); compilerproc; [public,alias:'FPC_DYNARRAY_CLEAR'];
var
  d: pdynarray;
begin
  if p = nil then exit;
  d := pdynarray(p-sizeof(tdynarray));
  if d^.refcount = 0 then
    //HandleErrorAddrFrameInd(204, get_pc_addr, get_frame);
    HandleError(204);
  if d^.refcount > 0 then begin
    dec(d^.refcount); // todo: locked
    // @@@todo...
    //ti := aligntoqword(ti+2+pbyte(ti)[1]);
    //if pdynarraytypedata(ti)^.elType <> nil then int_finalizearray(p,pdynarraytypedata(ti)^.elType^, d^.high+1);
    FreeMem(d);
  end;
  p := nil;
end;

{$asmmode intel}
function InterlockedDecrement(var Addend: LongInt): LongInt; assembler;
asm
  {$ifdef CPU64}
  mov rax, -1
  lock xadd [Addend], rax
  dec rax
  {$else}
  mov eax, -1
  lock xadd [Addend], eax
  dec eax
  {$endif}
end;

{$asmmode intel}
function InterlockedIncrement(var Addend: LongInt): LongInt; assembler;
asm
  {$ifdef CPU64}
  mov rax, 1
  lock xadd [Addend], rax
  inc rax
  {$else}
  mov eax, 1
  lock xadd [Addend], eax
  inc eax
  {$endif}
end;

// those are for WIN64

//{$asmmode ATT}
//procedure inclocked(var l: LongInt); assembler; nostackframe;
//asm
//  { this check should be done because a lock takes a lot }
//  { of time!                                             }
//  cmpl       $0,IsMultithread(%rip)
//  jz         .Linclockednolock
//  lock
//  incl       (%rcx)
//  ret
//  {$ifndef VER3_2}
//  .p2align 4,,10
//  {$endif VER3_2}
//  .p2align 3
//  .Linclockednolock:
//  incl       (%rcx)
//end;
//
//procedure inclocked(var l: Int64); assembler; nostackframe;
//asm
//  { this check should be done because a lock takes a lot }
//  { of time!                                             }
//  cmpl       $0,IsMultithread(%rip)
//  jz         .Linclockednolock
//  lock
//  incq       (%rcx)
//  ret
//  {$ifndef VER3_2}
//  .p2align 4,,10
//  {$endif VER3_2}
//  .p2align 3
//  .Linclockednolock:
//  incq       (%rcx)
//end;
//
//function declocked(var l: LongInt): boolean; assembler; nostackframe;
//asm
//  { this check should be done because a lock takes a lot }
//  { of time!                                             }
//  cmpl       $0,IsMultithread(%rip)
//  {$ifndef win64}
//  mov        %rdi, %rcx
//  {$endif win64}
//  jz         .Ldeclockednolock
//  lock
//  decl       (%rcx)
//  setzb      %al
//  ret
//  {$ifndef VER3_2}
//  .p2align 4,,10
//  {$endif VER3_2}
//  .p2align 3
//  .Ldeclockednolock:
//  decl       (%rcx)
//  setzb      %al
//end;
//
//function declocked(var l: Int64): boolean; assembler; nostackframe;
//asm
//  { this check should be done because a lock takes a lot }
//  { of time!                                             }
//  cmpl       $0,IsMultithread(%rip)
//  {$ifndef win64}
//  mov        %rdi, %rcx
//  {$endif win64}
//  jz         .Ldeclockednolock
//  lock
//  decq       (%rcx)
//  setzb      %al
//  ret
//  {$ifndef VER3_2}
//  .p2align 4,,10
//  {$endif VER3_2}
//  .p2align 3
//  .Ldeclockednolock:
//  decq       (%rcx)
//  setzb      %al
//end;

initialization
  install_exception_handlers;

end.

