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

uses Windows, global;

type
  TByteLookup = array[0..255] of Byte;
  PByteLookup = ^TByteLookup;
const
  bsr8bit: TByteLookup = (
  $ff,0,1,1,2,2,2,2,3,3,3,3,3,3,3,3,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,
	5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,
	6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,
	6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,
	7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,
	7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,
	7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,
	7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7
);

function GetBsr8bit: PByteLookup; stdcall; export;

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

//procedure FPC_move(const source; var dest; count: DWord); stdcall; export; assembler;

(*/
procedure GetMem_PS(p: Pointer; Size: PtrUInt);           stdcall; export;
function  GetMem_S (            Size: PtrUInt): Pointer;  stdcall; export;

procedure FreeMem_PS(p: Pointer; Size: PtrUInt);          stdcall; export;
function  FreeMem_P (p: Pointer               ): PtrUInt; stdcall; export;
*/
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
*/
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

(*
function StringReplace_Ansi_A    (const S: AnsiString;    const oldPattern: AnsiString;    const newPattern: AnsiString;    Flags: TReplaceFlags; aCount: Integer): AnsiString;    stdcall export;
function StringReplace_Wide_A    (const S: WideString;    const oldPattern: WideString;    const newPattern: WideString;    Flags: TReplaceFlags; aCount: Integer): WideString;    stdcall export;
function StringReplace_Unicode_A (const S: UnicodeString; const oldPattern: UnicodeString; const newPattern: UnicodeString; Flags: TReplaceFlags; aCount: Integer): UnicodeString; stdcall export;
*/

function StringReplace_A(
    S: AnsiString;
    oldPattern: AnsiString;
    newPattern: AnsiString;
    Flags: TReplaceFlags ): PDLLrequest; stdcall export;

(*/ 
function StringReplace_Wide_B    (const S: WideString;    const oldPattern: WideString;    const newPattern: WideString;    Flags: TReplaceFlags): WideString;    stdcall export;
function StringReplace_Unicode_B (const S: UnicodeString; const oldPattern: UnicodeString; const newPattern: UnicodeString; Flags: TReplaceFlags): UnicodeString; stdcall export;

function WideStringReplace       (const S:    WideString; const oldPattern:    WideString; const newPattern:    WideString; Flags: TReplaceFlags):    WideString; stdcall; export;
*/
//procedure AbstractError_E; stdcall; export;

implementation

const mfcfpc = 'mfcfpc.dll';

function GetBsr8bit: PByteLookup; stdcall; export;
begin
  result := @bsr8bit;
end;


(*
procedure GetMem_PS(p: Pointer; Size: PtrUInt);           stdcall; [public, alias: 'GetMem_PS'  ]; export; begin           GetMem(p,  Size); end;
function  GetMem_S (            Size: PtrUInt): Pointer;  stdcall; [public, alias: 'GetMem_S'   ]; export; begin result := GetMem(    Size); end;

//procedure FreeMem_PS(p: Pointer; Size: PtrUInt);          stdcall; [public, alias: 'FreeMem_PS' ]; export; begin                   FreeMem(p, Size) ; end;
function  FreeMem_P (p: Pointer               ): PtrUInt; stdcall; [public, alias: 'FreeMem_P'  ]; export; begin result := PtrUInt(FreeMem(p      )); end;
*/
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
function Abs_Int64(l: Int64  ): Int64;   stdcall; [public, alias: 'Abs_Int64']; export; begin result := Int64  (Abs(Int64  (l))); end;*/
//function Abs_VR   (l: ValReal): ValReal; stdcall; [public, alias: 'Abs_VR'   ]; export; begin result := ValReal(Abs(Double(l))); end;

(*
function Frac_VR (d: ValReal): ValReal; stdcall; [public, alias: 'Frac_VR'  ]; export; begin result := Frac (d); end;
function Int_VR  (d: ValReal): ValReal; stdcall; [public, alias: 'Int_VR'   ]; export; begin result := Int  (d); end;
function Round_VR(d: ValReal): ValReal; stdcall; [public, alias: 'Round_VR' ]; export; begin result := Round(d); end;
function Trunc_VR(d: ValReal): ValReal; stdcall; [public, alias: 'Trunc_VR' ]; export; begin result := Trunc(d); end;
*/

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



function ReplaceText(P: PDLLrequest): PDLLrequest; cdecl; external mfcfpc name 'fpc_ReplaceText';

//function ReplaceText(P: TDLLrequest): PAnsiChar; cdecl; external mfcfpc name 'fpc_ReplaceText';
(*/
 S: PAnsiChar;
    oldPattern: PAnsiChar;
    newPattern: PAnsiChar;
    Flags: Cardinal;
    lenS: Integer;
    lenOldPattern: Integer;
    lenNewPattern: Integer): PAnsiChar; cdecl;
    external mfcfpc name 'fpc_ReplaceText';*/

function StringReplace_A(
    S: AnsiString;
    oldPattern: AnsiString;
    newPattern: AnsiString;
    Flags: TReplaceFlags ): PDLLrequest; stdcall;
    [public, alias: 'StringReplace_A' ];
    export;
var
    flag: Cardinal;
    len1, len2, len3: Integer;
    temp: PAnsiChar;
    news: String;
    P: PDLLrequest;
begin
    if rfReplaceAll in Flags then flag := flag or (1 shl 0);
    if rfIgnoreCase in Flags then flag := flag or (1 shl 1);
    
    len1 := StrLen(PAnsiChar(S));
    len2 := StrLen(PAnsiChar(oldPattern));
    len3 := StrLen(PAnsiChar(newPattern));
    
    MessageBoxA(0,'xx  aaaaa xx','ee 000 eeee',0);
    
    GetMem(P, sizeof(P));
    
    P.version := RTLLib_Version;
    P.Error.ErrorCode := 0;
    
    P := ReplaceText(P);
    
    if P.Error.ErrorCode = 426 then
    MessageBoxA(0,'lllluuuulll','000000',0);
    
    FreeMem(P);
    (*
    temp := ReplaceText(
        PAnsiChar(S),
        PAnsiChar(oldPattern),
        PAnsiChar(newPattern),
        flag,
        len1,
        len2,
        len3
    );*/
    
    MessageBoxA(0, 'temp', 'abcd', 0);
    
(*/    news := PAnsiChar(GetMem(StrLen(temp)+1));
    if sizeof(news) < 9 then
    MessageBoxA(0,'ttttt','ccc',0);
    move(temp, news, sizeof(news));
    news[StrLen(temp)] := #0;*/
    
    result := @P;
end;

(*
function StringReplace_Ansi_A    (const S: AnsiString;    const oldPattern: AnsiString;    const newPattern: AnsiString;    Flags: TReplaceFlags; aCount: Integer): AnsiString;    stdcall export; [public, alias: 'StringReplace_Ansi_A'    ]; begin result := AnsiString   (StringReplace(AnsiString   (S),    AnsiString(oldPattern),    AnsiString(newPattern), Flags, aCount)); end;
function StringReplace_Wide_A    (const S: WideString;    const oldPattern: WideString;    const newPattern: WideString;    Flags: TReplaceFlags; aCount: Integer): WideString;    stdcall export; [public, alias: 'StringReplace_Wide_A'    ]; begin result := WideString   (StringReplace(WideString   (S),    WideString(oldPattern),    WideString(newPattern), Flags, aCount)); end;
function StringReplace_Unicode_A (const S: UnicodeString; const oldPattern: UnicodeString; const newPattern: UnicodeString; Flags: TReplaceFlags; aCount: Integer): UnicodeString; stdcall export; [public, alias: 'StringReplace_Unicode_A' ]; begin result := UnicodeString(StringReplace(UnicodeString(S), UnicodeString(oldPattern), UnicodeString(newPattern), Flags, aCount)); end;
 
function StringReplace_Ansi_B    (const S: AnsiString;    const oldPattern: AnsiString;    const newPattern: AnsiString;    Flags: TReplaceFlags): AnsiString;    stdcall export; [public, alias: 'StringReplace_Ansi_B'    ]; begin result :=    AnsiString(StringReplace(   AnsiString(S),    AnsiString(oldPattern),    AnsiString(newPattern), Flags)); end;
function StringReplace_Wide_B    (const S: WideString;    const oldPattern: WideString;    const newPattern: WideString;    Flags: TReplaceFlags): WideString;    stdcall export; [public, alias: 'StringReplace_Wide_B'    ]; begin result :=    WideString(StringReplace(   WideString(S),    WideString(oldPattern),    WideString(newPattern), Flags)); end;
function StringReplace_Unicode_B (const S: UnicodeString; const oldPattern: UnicodeString; const newPattern: UnicodeString; Flags: TReplaceFlags): UnicodeString; stdcall export; [public, alias: 'StringReplace_Unicode_B' ]; begin result := UnicodeString(StringReplace(UnicodeString(S), UnicodeString(oldPattern), UnicodeString(newPattern), Flags)); end;
*/

//function WideStringReplace       (const S: WideString;    const oldPattern: WideString;    const newPattern: WideString;    Flags: TReplaceFlags): WideString;    stdcall; export; [public, alias: 'WideStringReplace'       ]; begin result := WideString(WideStringReplace(WideString   (S), WideString   (oldPattern), WideString   (newPattern), Flags)); end;

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
  GetBsr8bit name 'GetBsr8bit',
  
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
  
  StringReplace_A,
  (*StringReplace_Wide_A,
  StringReplace_Unicode_A,
  
  StringReplace_Ansi_B,
  StringReplace_Wide_B,
  StringReplace_Unicode_B,
*/
  //WideStringReplace,
  (*
  GetMem_PS,
  GetMem_S,
  
  FreeMem_PS,
  FreeMem_P,*/
  
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
  Trunc_VR,*/
  
  //AbstractError_E,
  
  TRTL_Create,
  TRTL_Destroy;

end.
