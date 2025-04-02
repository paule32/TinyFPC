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

{$mode delphi}
{$WARN IMPLICIT_STRING_CAST_LOSS OFF}
{$M-}
unit RTLUnit;

interface

type
  TReplaceFlag  = (
    rfReplaceAll,
    rfIgnoreCase
  );
  TReplaceFlags = set of TReplaceFlag;

type
  TWidgetSet = class(TObject)
  public
    constructor Create;
    destructor Destroy; override;
  end;

type
  TRtl = class(TObject)
  private
    FWidgetSet: TWidgetSet;
  public
    constructor Create;
    destructor Destroy; override;
  end;

function TRtl_Create: TRtl; stdcall; export;
procedure TRtl_Destroy(AValue: TRTL); stdcall; export;

type
  T_Array_Boolean = Array of Boolean;
  T_Array_Char    = Array of Char;
  T_Array_Byte    = Array of Byte;
  T_Array_Word    = Array of Word;
  T_Array_DWord   = Array of DWord;
  T_Array_Integer = Array of Integer;
  T_Array_String  = Array of String;

type
  TArray<T> = Array of T;

  TBooleanArray<T> = class
    class function Create(NewLength: Integer): TArray<T>; stdcall;
  end;
  TCharArray<T> = class
    class function Create(NewLength: Integer): TArray<T>; stdcall;
  end;
  TByteArray<T> = class
    class function Create(NewLength: Integer): TArray<T>; stdcall;
  end;
  TWordArray<T> = class
    class function Create(NewLength: Integer): TArray<T>; stdcall;
  end;
  TDWordArray<T> = class
    class function Create(NewLength: Integer): TArray<T>; stdcall;
  end;
  TIntegerArray<T> = class
    class function Create(NewLength: Integer): TArray<T>; stdcall;
  end;
  TStringArray<T> = class
    class function Create(NewLength: Integer): TArray<T>; stdcall;
  end;

procedure FPC_move(const source; var dest; count: DWord); stdcall; export; assembler;

(*)
procedure GetMem_PS(p: Pointer; Size: PtrUInt);           stdcall; export;
function  GetMem_S (            Size: PtrUInt): Pointer;  stdcall; export;

procedure FreeMem_PS(p: Pointer; Size: PtrUInt);          stdcall; export;
function  FreeMem_P (p: Pointer               ): PtrUInt; stdcall; export;
*)
//procedure New_P (var P: Pointer               ); stdcall; export;
//procedure New_PC(var P: Pointer; Size: Integer); stdcall; export;

(*
procedure FillBool_A  (var x; count: SizeInt; Value: Boolean); stdcall; export;
procedure FillBool_B  (var x; count: SizeInt; Value: Byte   ); stdcall; export;
procedure FillBool_C  (var x; count: SizeInt; Value: Char   ); stdcall; export;
procedure FillBool_D  (var x; count: SizeInt; Value: Word   ); stdcall; export;
procedure FillBool_E  (var x; count: SizeInt; Value: DWord  ); stdcall; export;

procedure FillChar_A  (var x; count: SizeInt; Value: Boolean); stdcall; export;
procedure FillChar_B  (var x; count: SizeInt; Value: Byte   ); stdcall; export;
procedure FillChar_C  (var x; count: SizeInt; Value: Char   ); stdcall; export;
procedure FillChar_D  (var x; count: SizeInt; Value: Word   ); stdcall; export;
procedure FillChar_E  (var x; count: SizeInt; Value: DWord  ); stdcall; export;

procedure FillByte_A  (var x; count: SizeInt; Value: Boolean); stdcall; export;
procedure FillByte_B  (var x; count: SizeInt; Value: Byte   ); stdcall; export;
procedure FillByte_C  (var x; count: SizeInt; Value: Char   ); stdcall; export;
procedure FillByte_D  (var x; count: SizeInt; Value: Word   ); stdcall; export;
procedure FillByte_E  (var x; count: SizeInt; Value: DWord  ); stdcall; export;

procedure FillWord_A  (var x; count: SizeInt; Value: Boolean); stdcall; export;
procedure FillWord_B  (var x; count: SizeInt; Value: Byte   ); stdcall; export;
procedure FillWord_C  (var x; count: SizeInt; Value: Char   ); stdcall; export;
procedure FillWord_D  (var x; count: SizeInt; Value: Word   ); stdcall; export;
procedure FillWord_E  (var x; count: SizeInt; Value: DWord  ); stdcall; export;

procedure FillDWord_A (var x; count: SizeInt; Value: Boolean); stdcall; export;
procedure FillDWord_B (var x; count: SizeInt; Value: Byte   ); stdcall; export;
procedure FillDWord_C (var x; count: SizeInt; Value: Char   ); stdcall; export;
procedure FillDWord_D (var x; count: SizeInt; Value: Word   ); stdcall; export;
procedure FillDWord_E (var x; count: SizeInt; Value: DWord  ); stdcall; export;

function Abs_Int  (l: Integer): Integer; stdcall; export;
function Abs_Int64(l: Int64  ): Int64;   stdcall; export;
function Abs_VR   (l: ValReal): ValReal; stdcall; export;

function Frac_VR (d: ValReal): ValReal; stdcall; export;
function Int_VR  (d: ValReal): ValReal; stdcall; export;
function Round_VR(d: ValReal): ValReal; stdcall; export;
function Trunc_VR(d: ValReal): ValReal; stdcall; export;
*)
procedure Array_Boolean (NewLength: Integer; var A: T_Array_Boolean); stdcall; export;
procedure Array_Char    (NewLength: Integer; var A: T_Array_Char   ); stdcall; export;
procedure Array_Byte    (NewLength: Integer; var A: T_Array_Byte   ); stdcall; export;
procedure Array_Word    (NewLength: Integer; var A: T_Array_Word   ); stdcall; export;
procedure Array_DWord   (NewLength: Integer; var A: T_Array_DWord  ); stdcall; export;
procedure Array_Integer (NewLength: Integer; var A: T_Array_Integer); stdcall; export;
procedure Array_String  (NewLength: Integer; var A: T_Array_String ); stdcall; export;

procedure SetLength_Array_Boolean  (var A: TArray<Boolean>; NewLength: Integer); stdcall; export;
procedure SetLength_Array_Char     (var A: TArray<Char   >; NewLength: Integer); stdcall; export;
procedure SetLength_Array_Byte     (var A: TArray<Byte   >; NewLength: Integer); stdcall; export;
procedure SetLength_Array_Word     (var A: TArray<Word   >; NewLength: Integer); stdcall; export;
procedure SetLength_Array_DWord    (var A: TArray<DWord  >; NewLength: Integer); stdcall; export;
procedure SetLength_Array_Integer  (var A: TArray<Integer>; NewLength: Integer); stdcall; export;
procedure SetLength_Array_String   (var A: TArray<String >; NewLength: Integer); stdcall; export;

procedure SetLength_String         (var S: String;        NewLength: Integer); stdcall; export;
procedure SetLength_String_Ansi    (var S: AnsiString;    NewLength: Integer); stdcall; export;
procedure SetLength_String_Wide    (var S: WideString;    NewLength: Integer); stdcall; export;
procedure SetLength_String_Unicode (var S: UnicodeString; NewLength: Integer); stdcall; export;

function StringReplace_Ansi_A    (const S: AnsiString;    const oldPattern: AnsiString;    const newPattern: AnsiString;    Flags: TReplaceFlags; aCount: Integer): AnsiString;    stdcall export;
function StringReplace_Wide_A    (const S: WideString;    const oldPattern: WideString;    const newPattern: WideString;    Flags: TReplaceFlags; aCount: Integer): WideString;    stdcall export;
function StringReplace_Unicode_A (const S: UnicodeString; const oldPattern: UnicodeString; const newPattern: UnicodeString; Flags: TReplaceFlags; aCount: Integer): UnicodeString; stdcall export;
 
function StringReplace(
    const S: AnsiString;
    const oldPattern: AnsiString;
    const newPattern: AnsiString;
    Flags: Cardinal): AnsiString; stdcall export;
    
function StringReplace_Wide_B    (const S: WideString;    const oldPattern: WideString;    const newPattern: WideString;    Flags: TReplaceFlags): WideString;    stdcall export;
function StringReplace_Unicode_B (const S: UnicodeString; const oldPattern: UnicodeString; const newPattern: UnicodeString; Flags: TReplaceFlags): UnicodeString; stdcall export;

function WideStringReplace       (const S:    WideString; const oldPattern:    WideString; const newPattern:    WideString; Flags: TReplaceFlags):    WideString; stdcall; export;

//procedure AbstractError_E; stdcall; export;

implementation

procedure GetMem_PS(p: Pointer; Size: PtrUInt);           stdcall; [public, alias: 'GetMem_PS'  ]; export; begin           GetMem(p,  Size); end;
function  GetMem_S (            Size: PtrUInt): Pointer;  stdcall; [public, alias: 'GetMem_S'   ]; export; begin result := GetMem(    Size); end;

//procedure FreeMem_PS(p: Pointer; Size: PtrUInt);          stdcall; [public, alias: 'FreeMem_PS' ]; export; begin                   FreeMem(p, Size) ; end;
function  FreeMem_P (p: Pointer               ): PtrUInt; stdcall; [public, alias: 'FreeMem_P'  ]; export; begin result := PtrUInt(FreeMem(p      )); end;

//procedure New_P (var P: Pointer               ); stdcall; [public, alias: 'New_P'  ]; export; begin New(P      ); end;
//procedure New_PC(var P: Pointer; Size: Integer); stdcall; [public, alias: 'New_PC' ]; export; begin New(P, Size); end;
(*
procedure FillBool_A  (var x; count: SizeInt; Value: Boolean); stdcall; [public, alias: 'FillBool_A'  ]; export; begin FillBool(x, count, Value); end;
procedure FillBool_B  (var x; count: SizeInt; Value: Byte   ); stdcall; [public, alias: 'FillBool_B'  ]; export; begin FillBool(x, count, Value); end;
procedure FillBool_C  (var x; count: SizeInt; Value: Char   ); stdcall; [public, alias: 'FillBool_C'  ]; export; begin FillBool(x, count, Value); end;
procedure FillBool_D  (var x; count: SizeInt; Value: Word   ); stdcall; [public, alias: 'FillBool_D'  ]; export; begin FillBool(x, count, Value); end;
procedure FillBool_E  (var x; count: SizeInt; Value: DWord  ); stdcall; [public, alias: 'FillBool_E'  ]; export; begin FillBool(x, count, Value); end;

//procedure FillChar_A  (var x; count: SizeInt; Value: Boolean); stdcall; [public, alias: 'FillChar_A'  ]; export; begin FillChar(x, count, Byte/(Value)); end;
procedure FillChar_B  (var x; count: SizeInt; Value: Byte   ); stdcall; [public, alias: 'FillChar_B'  ]; export; begin FillChar(x, count, Value); end;
procedure FillChar_C  (var x; count: SizeInt; Value: Char   ); stdcall; [public, alias: 'FillChar_C'  ]; export; begin FillChar(x, count, Value); end;
procedure FillChar_D  (var x; count: SizeInt; Value: Word   ); stdcall; [public, alias: 'FillChar_D'  ]; export; begin FillChar(x, count, Value); end;
procedure FillChar_E  (var x; count: SizeInt; Value: DWord  ); stdcall; [public, alias: 'FillChar_E'  ]; export; begin FillChar(x, count, Value); end;

procedure FillByte_A  (var x; count: SizeInt; Value: Boolean); stdcall; [public, alias: 'FillByte_A'  ]; export; begin FillByte(x, count, Byte(Value)); end;
procedure FillByte_B  (var x; count: SizeInt; Value: Byte   ); stdcall; [public, alias: 'FillByte_B'  ]; export; begin FillByte(x, count, Byte(Value)); end;
procedure FillByte_C  (var x; count: SizeInt; Value: Char   ); stdcall; [public, alias: 'FillByte_C'  ]; export; begin FillByte(x, count, Byte(Value)); end;
procedure FillByte_D  (var x; count: SizeInt; Value: Word   ); stdcall; [public, alias: 'FillByte_D'  ]; export; begin FillByte(x, count, Value); end;
procedure FillByte_E  (var x; count: SizeInt; Value: DWord  ); stdcall; [public, alias: 'FillByte_E'  ]; export; begin FillByte(x, count, Value); end;

(*
procedure FillWord_A  (var x; count: SizeInt; Value: Boolean); stdcall; [public, alias: 'FillWord_A'  ]; export; begin FillWord(x, count, Word(Value)); end;
procedure FillWord_B  (var x; count: SizeInt; Value: Byte   ); stdcall; [public, alias: 'FillWord_B'  ]; export; begin FillWord(x, count, Word(Value)); end;
procedure FillWord_C  (var x; count: SizeInt; Value: Char   ); stdcall; [public, alias: 'FillWord_C'  ]; export; begin FillWord(x, count, Word(Value)); end;
procedure FillWord_D  (var x; count: SizeInt; Value: Word   ); stdcall; [public, alias: 'FillWord_D'  ]; export; begin FillWord(x, count, Value); end;
procedure FillWord_E  (var x; count: SizeInt; Value: DWord  ); stdcall; [public, alias: 'FillWord_E'  ]; export; begin FillWord(x, count, Value); end;

procedure FillDWord_A (var x; count: SizeInt; Value: Boolean); stdcall; [public, alias: 'FillDWord_A'  ]; export; begin FillDWord(x, count, DWord(Value)); end;
procedure FillDWord_B (var x; count: SizeInt; Value: Byte   ); stdcall; [public, alias: 'FillDWord_B'  ]; export; begin FillDWord(x, count, DWord(Value)); end;
procedure FillDWord_C (var x; count: SizeInt; Value: Char   ); stdcall; [public, alias: 'FillDWord_C'  ]; export; begin FillDWord(x, count, DWord(Value)); end;
procedure FillDWord_D (var x; count: SizeInt; Value: Word   ); stdcall; [public, alias: 'FillDWord_D'  ]; export; begin FillDWord(x, count, Value); end;
procedure FillDWord_E (var x; count: SizeInt; Value: DWord  ); stdcall; [public, alias: 'FillDWord_E'  ]; export; begin FillDWord(x, count, Value); end;

function Abs_Int  (l: Integer): Integer; stdcall; [public, alias: 'Abs_Int'  ]; export; begin result := Integer(Abs(Integer(l))); end;
function Abs_Int64(l: Int64  ): Int64;   stdcall; [public, alias: 'Abs_Int64']; export; begin result := Int64  (Abs(Int64  (l))); end;*)
//function Abs_VR   (l: ValReal): ValReal; stdcall; [public, alias: 'Abs_VR'   ]; export; begin result := ValReal(Abs(Double(l))); end;

(*
function Frac_VR (d: ValReal): ValReal; stdcall; [public, alias: 'Frac_VR'  ]; export; begin result := Frac (d); end;
function Int_VR  (d: ValReal): ValReal; stdcall; [public, alias: 'Int_VR'   ]; export; begin result := Int  (d); end;
function Round_VR(d: ValReal): ValReal; stdcall; [public, alias: 'Round_VR' ]; export; begin result := Round(d); end;
function Trunc_VR(d: ValReal): ValReal; stdcall; [public, alias: 'Trunc_VR' ]; export; begin result := Trunc(d); end;
*)

procedure Array_Boolean(NewLength: Integer; var A: T_Array_Boolean); stdcall; [public, alias: 'Array_Boolean']; export;
var
  I: Integer;
begin
  SetLength(A, NewLength);
  for I := 0 to High(A) do
  A[I] := False;
end;

procedure Array_Char(NewLength: Integer; var A: T_Array_Char); stdcall; [public, alias: 'Array_Char']; export;
var
  I: Integer;
begin
  SetLength(A, NewLength);
  for I := 0 to High(A) do
  A[I] := #0;
end;

procedure Array_Byte(NewLength: Integer; var A: T_Array_Byte); stdcall; [public, alias: 'Array_Byte']; export;
var
  I: Integer;
begin
  SetLength(A, NewLength);
  for I := 0 to High(A) do
  A[I] := 0;
end;

procedure Array_Word(NewLength: Integer; var A: T_Array_Word); stdcall; [public, alias: 'Array_Word']; export;
var
  I: Integer;
begin
  SetLength(A, NewLength);
  for I := 0 to High(A) do
  A[I] := 0;
end;

procedure Array_DWord(NewLength: Integer; var A: T_Array_DWord); stdcall; [public, alias: 'Array_DWord']; export;
var
  I: Integer;
begin
  SetLength(A, NewLength);
  for I := 0 to High(A) do
  A[I] := 0;
end;

procedure Array_Integer(NewLength: Integer; var A: T_Array_Integer); stdcall; [public, alias: 'Array_Integer']; export;
var
  I: Integer;
begin
  SetLength(A, NewLength);
  for I := 0 to High(A) do
  A[I] := 0;
end;

procedure Array_String(NewLength: Integer; var A: T_Array_String); stdcall; [public, alias: 'Array_String']; export;
var
  I: Integer;
begin
  SetLength(A, NewLength);
  for I := 0 to High(A) do
  A[I] := '';
end;

procedure SetLength_Array_Boolean  (var A: TArray<Boolean>; NewLength: Integer); stdcall; [public, alias: 'SetLength_Array_Boolean']; export; begin SetLength(A, NewLength); end;
procedure SetLength_Array_Char     (var A: TArray<Char   >; NewLength: Integer); stdcall; [public, alias: 'SetLength_Array_Char'   ]; export; begin SetLength(A, NewLength); end;
procedure SetLength_Array_Byte     (var A: TArray<Byte   >; NewLength: Integer); stdcall; [public, alias: 'SetLength_Array_Byte'   ]; export; begin SetLength(A, NewLength); end;
procedure SetLength_Array_Word     (var A: TArray<Word   >; NewLength: Integer); stdcall; [public, alias: 'SetLength_Array_Word'   ]; export; begin SetLength(A, NewLength); end;
procedure SetLength_Array_DWord    (var A: TArray<DWord  >; NewLength: Integer); stdcall; [public, alias: 'SetLength_Array_DWord'  ]; export; begin SetLength(A, NewLength); end;
procedure SetLength_Array_Integer  (var A: TArray<Integer>; NewLength: Integer); stdcall; [public, alias: 'SetLength_Array_Integer']; export; begin SetLength(A, NewLength); end;
procedure SetLength_Array_String   (var A: TArray<String >; NewLength: Integer); stdcall; [public, alias: 'SetLength_Array_String' ]; export; begin SetLength(A, NewLength); end;

procedure SetLength_String         (var S: String;        NewLength: Integer); stdcall; [public, alias: 'SetLength_String'          ]; export; begin SetLength(S, NewLength); end;
procedure SetLength_String_Ansi    (var S: AnsiString;    NewLength: Integer); stdcall; [public, alias: 'SetLength_String_Ansi'     ]; export; begin SetLength(S, NewLength); end;
procedure SetLength_String_Wide    (var S: WideString;    NewLength: Integer); stdcall; [public, alias: 'SetLength_String_Wide'     ]; export; begin SetLength(S, NewLength); end;
procedure SetLength_String_Unicode (var S: UnicodeString; NewLength: Integer); stdcall; [public, alias: 'SetLength_String_Unicode'  ]; export; begin SetLength(S, NewLength); end;


procedure ReplaceText(
    S: PAnsiChar;
    oldPattern: PAnsiChar;
    newPattern: PAnsiChar;
    Flags: Cardinal;
    lenS: Integer;
    lenOldPattern: Integer;
    lenNewPattern: Integen); cdecl; external mfcfpc;

function StringReplace(
    const S: AnsiString;
    const oldPattern: AnsiString;
    const newPattern: AnsiString;
    const Flags: TReplaceFlags ): AnsiString; stdcall export;
var
    card: Cardinal;
    len1, len2, len3: Integer;
begin
    if rfReplaceAll in Flags then card := card or (1 shl 0);
    if rfIgnoreCase in Flags then card := card or (1 shl 1);
    
    len1 := Length(PAnsiChar(S));
    len2 := Length(PAnsiChar(oldPattern));
    len3 := Length(PAnsiChar(newPattern));
    
    ReplaceText(
        PAnsiChar(S),
        PAnsiChar(oldPattern),
        PAnsiChar(newPattern),
        card,
        len1,
        len2,
        len3
    );
end;

(*
function StringReplace_Ansi_A    (const S: AnsiString;    const oldPattern: AnsiString;    const newPattern: AnsiString;    Flags: TReplaceFlags; aCount: Integer): AnsiString;    stdcall export; [public, alias: 'StringReplace_Ansi_A'    ]; begin result := AnsiString   (StringReplace(AnsiString   (S),    AnsiString(oldPattern),    AnsiString(newPattern), Flags, aCount)); end;
function StringReplace_Wide_A    (const S: WideString;    const oldPattern: WideString;    const newPattern: WideString;    Flags: TReplaceFlags; aCount: Integer): WideString;    stdcall export; [public, alias: 'StringReplace_Wide_A'    ]; begin result := WideString   (StringReplace(WideString   (S),    WideString(oldPattern),    WideString(newPattern), Flags, aCount)); end;
function StringReplace_Unicode_A (const S: UnicodeString; const oldPattern: UnicodeString; const newPattern: UnicodeString; Flags: TReplaceFlags; aCount: Integer): UnicodeString; stdcall export; [public, alias: 'StringReplace_Unicode_A' ]; begin result := UnicodeString(StringReplace(UnicodeString(S), UnicodeString(oldPattern), UnicodeString(newPattern), Flags, aCount)); end;
 
function StringReplace_Ansi_B    (const S: AnsiString;    const oldPattern: AnsiString;    const newPattern: AnsiString;    Flags: TReplaceFlags): AnsiString;    stdcall export; [public, alias: 'StringReplace_Ansi_B'    ]; begin result :=    AnsiString(StringReplace(   AnsiString(S),    AnsiString(oldPattern),    AnsiString(newPattern), Flags)); end;
function StringReplace_Wide_B    (const S: WideString;    const oldPattern: WideString;    const newPattern: WideString;    Flags: TReplaceFlags): WideString;    stdcall export; [public, alias: 'StringReplace_Wide_B'    ]; begin result :=    WideString(StringReplace(   WideString(S),    WideString(oldPattern),    WideString(newPattern), Flags)); end;
function StringReplace_Unicode_B (const S: UnicodeString; const oldPattern: UnicodeString; const newPattern: UnicodeString; Flags: TReplaceFlags): UnicodeString; stdcall export; [public, alias: 'StringReplace_Unicode_B' ]; begin result := UnicodeString(StringReplace(UnicodeString(S), UnicodeString(oldPattern), UnicodeString(newPattern), Flags)); end;
*)

function WideStringReplace       (const S: WideString;    const oldPattern: WideString;    const newPattern: WideString;    Flags: TReplaceFlags): WideString;    stdcall; export; [public, alias: 'WideStringReplace'       ]; begin result := WideString(WideStringReplace(WideString   (S), WideString   (oldPattern), WideString   (newPattern), Flags)); end;

procedure FPC_move(const source; var dest; count: DWord); stdcall; [public, alias: 'FPC_move']; assembler; nostackframe;
asm
    mov    %r8, %rax
    sub    %rdx, %rcx            { rcx = src - dest }
    jz     .Lquit                { exit if src=dest }
    jnb    .L1                   { src>dest => forward move }

    add    %rcx, %rax            { rcx is negative => r8+rcx > 0 if regions overlap }
    jb     .Lback                { if no overlap, still do forward move }

.L1:
    cmp    $8, %r8
    jl     .Lless8f              { signed compare, negative count not allowed }
    test   $7, %dl
    je     .Ldestaligned

    test   $1, %dl               { align dest by moving first 1+2+4 bytes }
    je     .L2f
    mov    (%rcx,%rdx,1),%al
    dec    %r8
    mov    %al, (%rdx)
    add    $1, %rdx
.L2f:
    test   $2, %dl
    je     .L4f
    mov    (%rcx,%rdx,1),%ax
    sub    $2, %r8
    mov    %ax, (%rdx)
    add    $2, %rdx
.L4f:
    test   $4, %dl
    je     .Ldestaligned
    mov    (%rcx,%rdx,1),%eax
    sub    $4, %r8
    mov    %eax, (%rdx)
    add    $4, %rdx

.Ldestaligned:
    mov    %r8, %r9
    shr    $5, %r9
    jne    .Lmore32

.Ltail:
    mov    %r8, %r9
    shr    $3, %r9
    je     .Lless8f

    .balign 16
.Lloop8f:                             { max. 8 iterations }
    mov    (%rcx,%rdx,1),%rax
    mov    %rax, (%rdx)
    add    $8, %rdx
    dec    %r9
    jne    .Lloop8f
    and    $7, %r8

.Lless8f:
    test   %r8, %r8
    jle    .Lquit

    .balign 16
.Lloop1f:
    mov    (%rcx,%rdx,1),%al
    mov    %al,(%rdx)
    inc    %rdx
    dec    %r8
    jne    .Lloop1f
.Lquit:
    retq


.Lmore32:
    cmp    $0x2000, %r9          { this limit must be processor-specific (1/2 L2 cache size) }
    jnae   .Lloop32
    cmp    $0x1000, %rcx         { but don't bother bypassing cache if src and dest }
    jnb    .Lntloopf             { are close to each other}

    .balign 16
.Lloop32:
    add    $32,%rdx
    mov    -32(%rcx,%rdx,1),%rax
    mov    -24(%rcx,%rdx,1),%r10
    mov    %rax,-32(%rdx)
    mov    %r10,-24(%rdx)
    dec    %r9
    mov    -16(%rcx,%rdx,1),%rax
    mov    -8(%rcx,%rdx,1),%r10
    mov    %rax,-16(%rdx)
    mov    %r10,-8(%rdx)
    jne    .Lloop32

    and    $0x1f, %r8
    jmpq   .Ltail

.Lntloopf:
    mov    $32, %eax

    .balign 16
.Lpref:
    prefetchnta (%rcx,%rdx,1)
    prefetchnta 0x40(%rcx,%rdx,1)
    add    $0x80, %rdx
    dec    %eax
    jne    .Lpref

    sub    $0x1000, %rdx
    mov    $64, %eax

    .balign 16
.Loop64:
    add    $64, %rdx
    mov    -64(%rcx,%rdx,1), %r9
    mov    -56(%rcx,%rdx,1), %r10
    movnti %r9, -64(%rdx)
    movnti %r10, -56(%rdx)

    mov    -48(%rcx,%rdx,1), %r9
    mov    -40(%rcx,%rdx,1), %r10
    movnti %r9, -48(%rdx)
    movnti %r10, -40(%rdx)
    dec    %eax
    mov    -32(%rcx,%rdx,1), %r9
    mov    -24(%rcx,%rdx,1), %r10
    movnti %r9, -32(%rdx)
    movnti %r10, -24(%rdx)

    mov    -16(%rcx,%rdx,1), %r9
    mov    -8(%rcx,%rdx,1), %r10
    movnti %r9, -16(%rdx)
    movnti %r10, -8(%rdx)
    jne    .Loop64

    sub    $0x1000, %r8
    cmp    $0x1000, %r8
    jae    .Lntloopf

    mfence
    jmpq    .Ldestaligned        { go handle remaining bytes }

{ backwards move }
.Lback:
    add    %r8, %rdx             { points to the end of dest }
    cmp    $8, %r8
    jl     .Lless8b              { signed compare, negative count not allowed }
    test   $7, %dl
    je     .Ldestalignedb
    test   $1, %dl
    je     .L2b
    dec    %rdx
    mov    (%rcx,%rdx,1), %al
    dec    %r8
    mov    %al, (%rdx)
.L2b:
    test   $2, %dl
    je     .L4b
    sub    $2, %rdx
    mov    (%rcx,%rdx,1), %ax
    sub    $2, %r8
    mov    %ax, (%rdx)
.L4b:
    test   $4, %dl
    je     .Ldestalignedb
    sub    $4, %rdx
    mov    (%rcx,%rdx,1), %eax
    sub    $4, %r8
    mov    %eax, (%rdx)

.Ldestalignedb:
    mov    %r8, %r9
    shr    $5, %r9
    jne    .Lmore32b

.Ltailb:
    mov    %r8, %r9
    shr    $3, %r9
    je     .Lless8b

.Lloop8b:
    sub    $8, %rdx
    mov    (%rcx,%rdx,1), %rax
    dec    %r9
    mov    %rax, (%rdx)
    jne    .Lloop8b
    and    $7, %r8

.Lless8b:
    test   %r8, %r8
    jle    .Lquit2

    .balign 16
.Lsmallb:
    dec   %rdx
    mov   (%rcx,%rdx,1), %al
    dec   %r8
    mov   %al,(%rdx)
    jnz   .Lsmallb
.Lquit2:
    retq

.Lmore32b:
    cmp   $0x2000, %r9
    jnae  .Lloop32b
    cmp    $0xfffffffffffff000,%rcx
    jb     .Lntloopb

    .balign 16
.Lloop32b:
    sub    $32, %rdx
    mov    24(%rcx,%rdx,1), %rax
    mov    16(%rcx,%rdx,1), %r10
    mov    %rax, 24(%rdx)
    mov    %r10, 16(%rdx)
    dec    %r9
    mov    8(%rcx,%rdx,1),%rax
    mov    (%rcx,%rdx,1), %r10
    mov    %rax, 8(%rdx)
    mov    %r10, (%rdx)
    jne    .Lloop32b
    and    $0x1f, %r8
    jmpq   .Ltailb

.Lntloopb:
    mov    $32, %eax

    .balign 16
.Lprefb:
    sub    $0x80, %rdx
    prefetchnta (%rcx,%rdx,1)
    prefetchnta 0x40(%rcx,%rdx,1)
    dec    %eax
    jnz    .Lprefb

    add    $0x1000, %rdx
    mov    $0x40, %eax

    .balign 16
.Lloop64b:
    sub    $64, %rdx
    mov    56(%rcx,%rdx,1), %r9
    mov    48(%rcx,%rdx,1), %r10
    movnti %r9, 56(%rdx)
    movnti %r10, 48(%rdx)

    mov    40(%rcx,%rdx,1), %r9
    mov    32(%rcx,%rdx,1), %r10
    movnti %r9, 40(%rdx)
    movnti %r10, 32(%rdx)
    dec    %eax
    mov    24(%rcx,%rdx,1), %r9
    mov    16(%rcx,%rdx,1), %r10
    movnti %r9, 24(%rdx)
    movnti %r10, 16(%rdx)

    mov    8(%rcx,%rdx,1), %r9
    mov    (%rcx,%rdx,1), %r10
    movnti %r9, 8(%rdx)
    movnti %r10, (%rdx)
    jne    .Lloop64b

    sub    $0x1000, %r8
    cmp    $0x1000, %r8
    jae    .Lntloopb
    mfence
    jmpq   .Ldestalignedb
end;


class function TBooleanArray<T>.Create(NewLength: Integer): TArray<T>; stdcall;
var
  A: Array of T;
  I: Integer;
begin
  SetLength(A, NewLength);
  for I := 0 to High(A) do
  A[I] := Default(T);
  result := A;
end;

class function TCharArray<T>.Create(NewLength: Integer): TArray<T>; stdcall;
var
  A: Array of T;
  I: Integer;
begin
  SetLength(A, NewLength);
  for I := 0 to High(A) do
  A[I] := Default(T);
  result := A;
end;

class function TByteArray<T>.Create(NewLength: Integer): TArray<T>; stdcall;
var
  A: Array of T;
  I: Integer;
begin
  SetLength(A, NewLength);
  for I := 0 to High(A) do
  A[I] := Default(T);
  result := A;
end;

class function TWordArray<T>.Create(NewLength: Integer): TArray<T>; stdcall;
var
  A: Array of T;
  I: Integer;
begin
  SetLength(A, NewLength);
  for I := 0 to High(A) do
  A[I] := Default(T);
  result := A;
end;

class function TDWordArray<T>.Create(NewLength: Integer): TArray<T>; stdcall;
var
  A: Array of T;
  I: Integer;
begin
  SetLength(A, NewLength);
  for I := 0 to High(A) do
  A[I] := Default(T);
  result := A;
end;

class function TIntegerArray<T>.Create(NewLength: Integer): TArray<T>; stdcall;
var
  A: Array of T;
  I: Integer;
begin
  SetLength(A, NewLength);
  for I := 0 to High(A) do
  A[I] := Default(T);
  result := A;
end;

class function TStringArray<T>.Create(NewLength: Integer): TArray<T>; stdcall;
var
  A: Array of T;
  I: Integer;
begin
  SetLength(A, NewLength);
  for I := 0 to High(A) do
  A[I] := Default(T);
  result := A;
end;

constructor TWidgetSet.Create;
begin
  inherited Create;
end;

destructor TWidgetSet.Destroy;
begin
  inherited Destroy;
end;

constructor TRtl.Create;
begin
  inherited Create;
end;

destructor TRTL.Destroy;
begin
  inherited Destroy;
end;

function TRTL_Create: TRTL; stdcall; export;
begin
  WriteLn('xxxx');
  result := TRTL.Create;
end;

procedure TRTL_Destroy(AValue: TRTL); stdcall; export;
begin
  AValue.Free;
end;

//procedure AbstractError_E; stdcall;  [public, alias: 'AbstractError_E']; export; begin AbstractError; end;

exports
  Array_Boolean,
  Array_Char,
  Array_Byte,
  Array_Word,
  Array_DWord,
  Array_Integer,
  Array_String,
  
  SetLength_Array_Boolean,
  SetLength_Array_Char,
  SetLength_Array_Byte,
  SetLength_Array_Word,
  SetLength_Array_DWord,
  SetLength_Array_Integer,
  SetLength_Array_String,
  
  SetLength_String,
  SetLength_String_Ansi,
  SetLength_String_Wide,
  SetLength_String_Unicode,
  (*)
  StringReplace_Ansi_A,
  StringReplace_Wide_A,
  StringReplace_Unicode_A,
  
  StringReplace_Ansi_B,
  StringReplace_Wide_B,
  StringReplace_Unicode_B,
*)
  WideStringReplace,
  (*
  GetMem_PS,
  GetMem_S,
  
  FreeMem_PS,
  FreeMem_P,*)
  
  // New_P,
  // New_PC,
  
  // FillBool_A,
  // FillBool_B,
  // FillBool_C,
  // FillBool_D,
  // FillBool_E,
  //
  (*FillByte_A,
  FillByte_B,
  FillByte_C,
  FillByte_D,
  FillByte_E,
  //
  FillChar_A,
  FillChar_B,
  FillChar_C,
  FillChar_D,
  FillChar_E,
  //
  FillWord_A,
  FillWord_B,
  FillWord_C,
  FillWord_D,
  FillWord_E,
  //
  FillDWord_A,
  FillDWord_B,
  FillDWord_C,
  FillDWord_D,
  FillDWord_E,
  
  FPC_move,
  
  Abs_Int,
  Abs_Int64,
  Abs_VR,
  
  Frac_VR,
  Int_VR,
  Round_VR,
  Trunc_VR,*)
  
  //AbstractError_E,
  
  TRTL_Create,
  TRTL_Destroy;

end.
