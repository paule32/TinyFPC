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

type
  Cardinal = 0..$FFFFFFFF;
  HRESULT = type LongInt;

  // -- Char
  {$if not declared(Char)}
  Char = AnsiChar;
  {$endif}
  {$if not declared(AnsiChar)}
  AnsiChar = Char;
  {$endif}
  TAnsiChar = AnsiChar;
  PChar = ^AnsiChar;
  PAnsiChar = ^AnsiChar;
  PAnsiString = ^AnsiString;

  // -- WideChar
  WChar = WideChar;
  PWideChar = ^WideChar;
  PPWideChar = ^PWideChar;

  // -- ShortString
  PShortString = ^ShortString;

  // -- RawByteString
  RawByteString = type AnsiString(CP_NONE);
  PRawByteString = ^RawByteString;

  // -- UTF8Char
  UTF8Char = AnsiChar;
  PUTF8Char = PAnsiChar;
  UTF8String = type AnsiString(CP_UTF8);
  PUTF8String = ^UTF8String;

  // -- WideChar, UnicodeChar, UnicodeString
  UnicodeChar = WideChar;
  PUnicodeChar = ^UnicodeChar;
  PUnicodeString = ^UnicodeString;

  // -- basic types
  Integer     = LongInt;
  DWord       = LongWord;
  {$ifdef CPU64}
  NativeInt   = Int64;
  NativeUInt  = QWord;
  {$else}
  NativeInt   = Integer;
  NativeUInt  = DWord;
  {$endif}
  PtrInt      = NativeInt;
  PtrUInt     = NativeUInt;
  SizeInt     = PtrInt;
  SizeUInt    = PtrUInt;
  ValReal     = Double;
  THandle     = NativeUInt;
  UINT        = Cardinal;
  UINT32      = Cardinal;
  UINT64      = QWord;
  ULONG       = Cardinal;
  ULONG_PTR   = NativeUInt; 
  ValSInt     = NativeInt;
  ValUInt     = NativeUInt;  
  SIZE_T      = NativeUInt;

  PSmallInt   = ^Smallint;
  PShortInt   = ^Shortint;
  PInteger    = ^Integer;
  PByte       = ^Byte;
  PWord       = ^word;
  PDWord      = ^DWord;
  PLongWord   = ^LongWord;
  PLongint    = ^Longint;
  PCardinal   = ^Cardinal;
  PQWord      = ^QWord;
  PInt64      = ^Int64;
  PUInt64     = ^UInt64;
  PPtrInt     = ^PtrInt;
  PPtrUInt    = ^PtrUInt;
  PSizeInt    = ^SizeInt;
  PSizeUInt   = ^SizeUInt;
  PSingle     = ^Single;
  PDouble     = ^Double;
  PExtended   = ^Extended;
  PBoolean    = ^Boolean;
  PBoolean16  = ^Boolean16;
  PBoolean32  = ^Boolean32;
  PBoolean64  = ^Boolean64;
  PByteBool   = ^ByteBool;
  PWordBool   = ^WordBool;
  PLongBool   = ^LongBool;
  PQWordBool  = ^QWordBool;
  PNativeInt  = ^NativeInt;
  PNativeUInt = ^NativeUint;
  PInt8   	  = PShortInt;
  PInt16  	  = PSmallint;
  PInt32  	  = PLongint;
  PIntPtr 	  = PPtrInt;
  PUInt8  	  = PByte;
  PUInt16 	  = PWord;
  PUInt32 	  = PDWord;
  PUIntPtr	  = PPtrUInt;
  PCurrency   = ^Currency;
  PVariant    = ^Variant;
  PPointer    = ^Pointer;

  // -- more types
  TDateTime = type Double;
  TDate = type Double;
  TTime = type Double;
  TProcedure = procedure;
  PText = ^Text; // fpc_get_output
  CodePointer = Pointer;
  PCodePointer = ^CodePointer;
  TFileTextRecChar = {$if defined(FPC_ANSI_TEXTFILEREC) or not(defined(FPC_HAS_FEATURE_WIDESTRINGS))}AnsiChar{$else}UnicodeChar{$endif};
  PFileTextRecChar = ^TFileTextRecChar;
  //TSystemCodePage     = Word;
  TSystemCodePage     = DWord;

  // --

  // MOVE THIS PART SOMEWHERE ELSE   
  pjmp_buf = ^jmp_buf;
  {$ifdef CPU64}
  jmp_buf = packed record
    rbx, rbp, r12, r13, r14, r15, rsp, rip: qword;
    rsi, rdi: qword;
    xmm6, xmm7, xmm8, xmm9, xmm10, xmm11, xmm12, xmm13, xmm14, xmm15: record m1, m2: qword; end;
    mxcsr: longword;
    fpucw: word;
    padding: word;
  end;
  {$else}
  jmp_buf = packed record
    ebx, esi, edi: LongInt;
    bp, sp, pc: Pointer;
    {$ifdef FPC_USE_WIN32_SEH}
    exhead: Pointer;
    {$endif FPC_USE_WIN32_SEH}
  end;
  {$endif}

  function setjmp(var s: jmp_buf): longint; [external name 'FPC_SETJMP'];
  procedure longjmp(var s: jmp_buf; value: longint); [external name 'FPC_LONGJMP'];
  // END OF PART TO MOVE
type

  PExceptAddr = ^TExceptAddr;
  TExceptAddr = record
    buf: pjmp_buf;
    next: PExceptAddr;
    {$ifdef CPU16}
    frametype: SmallInt;
    {$else CPU16}
    frametype: LongInt;
    {$endif CPU16}
  end;

  PGUID = ^TGuid;
  TGUID = packed record
    case Byte of
    1: (
      Data1: LongWord;
      Data2: Word;
      Data3: Word;
      Data4: array[0..7] of Byte;
    );
    2: (
      D1: LongWord;
      D2: Word;
      D3: Word;
      D4: array[0..7] of Byte;
    );
    3: (
      { uuid fields according to RFC4122 }
      time_low: LongWord; // The low field of the timestamp
      time_mid: Word; // The middle field of the timestamp
      time_hi_and_version: Word; // The high field of the timestamp multiplexed with the version number
      clock_seq_hi_and_reserved: Byte; // The high field of the clock sequence multiplexed with the variant
      clock_seq_low: Byte; // The low field of the clock sequence
      node: array[0..5] of Byte; // The spatially unique node identifier
    );
  end;
  // --

// textrec
const
  TextRecNameLength = 256;
  TextRecBufSize    = 256;

type
  TLineEndStr = string [3];
  TextBuf = array[0..TextRecBufSize-1] of ansichar;
  TTextBuf = TextBuf;

  { using packed makes the compiler to generate ugly code on some CPUs, further
  using packed causes the compiler to handle arrays of text wrongly, see  see tw0754 e.g. on arm  }
  TextRec = record
    Handle    : THandle;
    {$if defined(CPU8) or defined(CPU16)}
    Mode      : Word;
    {$else}
    Mode      : longint;
    {$endif}
    bufsize   : SizeInt;
    _private  : SizeInt;
    bufpos,
    bufend    : SizeInt;
    bufptr    : ^textbuf;
    openfunc,
    inoutfunc,
    flushfunc,
    closefunc : codepointer;
    UserData  : array[1..32] of byte;
    name      : array[0..textrecnamelength-1] of TFileTextRecChar;
    LineEnd   : TLineEndStr;
    buffer    : textbuf;
    {$ifdef FPC_HAS_CPSTRING}
    CodePage  : TSystemCodePage;
    {$endif}
    {$ifdef FPC_HAS_FEATURE_UNICODESTRINGS}
    FullName  : Pointer;
    {$endif FPC_HAS_FEATURE_UNICODESTRINGS}
  end;

// -- strings handling

type
  PAnsiRec = ^TAnsiRec;
  TAnsiRec = record
    CodePage: TSystemCodePage;
    ElementSize: word;
    {$ifdef CPU64}
    { align fields  }
    Dummy: DWord;
    {$endif CPU64}
    Ref: SizeInt;
    Len: SizeInt;
  end;

  PWideRec = ^TWideRec;
  TWideRec = Packed Record
    Len   : DWord;
    First : WideChar;
  end;

var
  FPC_EMPTYCHAR: AnsiChar; public name 'FPC_EMPTYCHAR';

const
  AnsiFirstOff = SizeOf(TAnsiRec);
  WideRecLen = SizeOf(TWideRec);
  WideFirstOff = SizeOf(TWideRec)-SizeOf(WideChar);
  LineEnding = #13#10;
  DefaultSystemCodePage        = 1250;
  DefaultUnicodeCodePage       = 1200;
  DefaultRTLFileSystemCodePage = 1250;
  DefaultFileSystemCodePage    = 65001;

const
  // apparently "ZBase" uses that, lets define some
  MaxLongint  = $7fffffff;
  MaxSmallint = 32767;
  MaxInt      = maxsmallint;

// -- WinAPI types
{$I system_types_winapi.inc}

