{$mode delphi}
{$WARN IMPLICIT_STRING_CAST_LOSS OFF}
{$M-}
unit RTLUnit;

interface

uses TypInfo, SysUtils;

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

procedure GetMem_PS(p: Pointer; Size: PtrUInt);           stdcall; overload; export;
function  GetMem_S (            Size: PtrUInt): Pointer;  stdcall; overload; export;

procedure FreeMem_PS(p: Pointer; Size: PtrUInt);          stdcall; overload; export;
function  FreeMem_P (p: Pointer               ): PtrUInt; stdcall; overload; export;

function Abs_Int  (l: Integer): Integer; stdcall; overload; export;
function Abs_Int64(l: Int64  ): Int64;   stdcall; overload; export;
function Abs_VR   (l: ValReal): ValReal; stdcall; overload; export;

function Frac_VR (d: ValReal): ValReal; stdcall; export;
function Int_VR  (d: ValReal): ValReal; stdcall; export;
function Round_VR(d: ValReal): ValReal; stdcall; export;
function Trunc_VR(d: ValReal): ValReal; stdcall; export;

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

function StringReplace_String_A  (const S: String;        const oldPattern: String;        const newPattern: String;        Flags: TReplaceFlags; aCount: Integer): String;        stdcall export;
function StringReplace_Ansi_A    (const S: AnsiString;    const oldPattern: AnsiString;    const newPattern: AnsiString;    Flags: TReplaceFlags; aCount: Integer): AnsiString;    stdcall export;
function StringReplace_Wide_A    (const S: WideString;    const oldPattern: WideString;    const newPattern: WideString;    Flags: TReplaceFlags; aCount: Integer): WideString;    stdcall export;
function StringReplace_Unicode_A (const S: UnicodeString; const oldPattern: UnicodeString; const newPattern: UnicodeString; Flags: TReplaceFlags; aCount: Integer): UnicodeString; stdcall export;

function StringReplace_String_B  (const S:        String; const oldPattern:        String; const newPattern:        String; Flags: TReplaceFlags):        String; stdcall export;
function StringReplace_Ansi_B    (const S:    AnsiString; const oldPattern:    AnsiString; const newPattern:    AnsiString; Flags: TReplaceFlags):    AnsiString; stdcall export;
function StringReplace_Wide_B    (const S:    WideString; const oldPattern:    WideString; const newPattern:    WideString; Flags: TReplaceFlags):    WideString; stdcall export;
function StringReplace_Unicode_B (const S: UnicodeString; const oldPattern: UnicodeString; const newPattern: UnicodeString; Flags: TReplaceFlags): UnicodeString; stdcall export;
function WideStringReplace       (const S:    WideString; const oldPattern:    WideString; const newPattern:    WideString; Flags: TReplaceFlags):    WideString; stdcall; export;

implementation

procedure GetMem_PS(p: Pointer; Size: PtrUInt);           stdcall; [public, alias: 'GetMem_PS'  ]; export; begin           GetMem(p,  Size); end;
function  GetMem_S (            Size: PtrUInt): Pointer;  stdcall; [public, alias: 'GetMem_S'   ]; export; begin result := GetMem(    Size); end;

procedure FreeMem_PS(p: Pointer; Size: PtrUInt);          stdcall; [public, alias: 'GreeMem_PS' ]; export; begin           FreeMem(p, Size); end;
function  FreeMem_P (p: Pointer               ): PtrUInt; stdcall; [public, alias: 'FreeMem_P'  ]; export; begin result := FreeMem(p      ); end;

function Abs_Int  (l: Integer): Integer; stdcall; [public, alias: 'Abs_Int'  ]; export; begin result := Abs(l); end;
function Abs_Int64(l: Int64  ): Int64;   stdcall; [public, alias: 'Abs_Int64']; export; begin result := Abs(l); end;
function Abs_VR   (l: ValReal): ValReal; stdcall; [public, alias: 'Abs_VR'   ]; export; begin result := Abs(l); end;

function Frac_VR (d: ValReal): ValReal; stdcall; export; [public, alias: 'Frac_VR'  ]; export; begin result := Frac (d); end;
function Int_VR  (d: ValReal): ValReal; stdcall; export; [public, alias: 'Int_VR'   ]; export; begin result := Int  (d); end;
function Round_VR(d: ValReal): ValReal; stdcall; export; [public, alias: 'Round_VR' ]; export; begin result := Round(d); end;
function Trunc_VR(d: ValReal): ValReal; stdcall; export; [public, alias: 'Trunc_VR' ]; export; begin result := Trunc(d); end;


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

function StringReplace_String_A  (const S: String;        const oldPattern: String;        const newPattern: String;        Flags: TReplaceFlags; aCount: Integer): String;        stdcall export; [public, alias: 'StringReplace_String_A'  ]; begin result :=     String   (StringReplace(    String   (S),        String(oldPattern),        String(newPattern), Flags, aCount)); end;
function StringReplace_Ansi_A    (const S: AnsiString;    const oldPattern: AnsiString;    const newPattern: AnsiString;    Flags: TReplaceFlags; aCount: Integer): AnsiString;    stdcall export; [public, alias: 'StringReplace_Ansi_A'    ]; begin result := AnsiString   (StringReplace(AnsiString   (S),    AnsiString(oldPattern),    AnsiString(newPattern), Flags, aCount)); end;
function StringReplace_Wide_A    (const S: WideString;    const oldPattern: WideString;    const newPattern: WideString;    Flags: TReplaceFlags; aCount: Integer): WideString;    stdcall export; [public, alias: 'StringReplace_Wide_A'    ]; begin result := WideString   (StringReplace(WideString   (S),    WideString(oldPattern),    WideString(newPattern), Flags, aCount)); end;
function StringReplace_Unicode_A (const S: UnicodeString; const oldPattern: UnicodeString; const newPattern: UnicodeString; Flags: TReplaceFlags; aCount: Integer): UnicodeString; stdcall export; [public, alias: 'StringReplace_Unicode_A' ]; begin result := UnicodeString(StringReplace(UnicodeString(S), UnicodeString(oldPattern), UnicodeString(newPattern), Flags, aCount)); end;

function StringReplace_String_B  (const S: String;        const oldPattern: String;        const newPattern: String;        Flags: TReplaceFlags): String;        stdcall; export; [public, alias: 'StringReplace_String_B'  ]; begin result :=     String   (StringReplace (    String   (S),     String   (oldPattern),     String   (newPattern), Flags)); end;
function StringReplace_Ansi_B    (const S: AnsiString;    const oldPattern: AnsiString;    const newPattern: AnsiString;    Flags: TReplaceFlags): AnsiString;    stdcall; export; [public, alias: 'StringReplace_Ansi_B'    ]; begin result := AnsiString   (StringReplace (AnsiString   (S), AnsiString   (oldPattern), AnsiString   (newPattern), Flags)); end;
function StringReplace_Wide_B    (const S: WideString;    const oldPattern: WideString;    const newPattern: WideString;    Flags: TReplaceFlags): WideString;    stdcall; export; [public, alias: 'StringReplace_Wide_B'    ]; begin result := WideString   (StringReplace (WideString   (S), WideString   (oldPattern), WideString   (newPattern), Flags)); end;
function StringReplace_Unicode_B (const S: UnicodeString; const oldPattern: UnicodeString; const newPattern: UnicodeString; Flags: TReplaceFlags): UnicodeString; stdcall; export; [public, alias: 'StringReplace_Unicode_B' ]; begin result := UnicodeString(StringReplace (UnicodeString(S), UnicodeString(oldPattern), UnicodeString(newPattern), Flags)); end;
function WideStringReplace       (const S: WideString;    const oldPattern: WideString;    const newPattern: WideString;    Flags: TReplaceFlags): WideString;    stdcall; export; [public, alias: 'WideStringReplace'       ]; begin result := WideString(WideStringReplace(WideString   (S), WideString   (oldPattern), WideString   (newPattern), Flags)); end;

class function TBooleanArray<T>.Create(NewLength: Integer): TArray<T>;
var
  A: Array of T;
  I: Integer;
begin
  SetLength(A, NewLength);
  for I := 0 to High(A) do
  A[I] := Default(T);
  result := A;
end;

class function TCharArray<T>.Create(NewLength: Integer): TArray<T>;
var
  A: Array of T;
  I: Integer;
begin
  SetLength(A, NewLength);
  for I := 0 to High(A) do
  A[I] := Default(T);
  result := A;
end;

class function TByteArray<T>.Create(NewLength: Integer): TArray<T>;
var
  A: Array of T;
  I: Integer;
begin
  SetLength(A, NewLength);
  for I := 0 to High(A) do
  A[I] := Default(T);
  result := A;
end;

class function TWordArray<T>.Create(NewLength: Integer): TArray<T>;
var
  A: Array of T;
  I: Integer;
begin
  SetLength(A, NewLength);
  for I := 0 to High(A) do
  A[I] := Default(T);
  result := A;
end;

class function TDWordArray<T>.Create(NewLength: Integer): TArray<T>;
var
  A: Array of T;
  I: Integer;
begin
  SetLength(A, NewLength);
  for I := 0 to High(A) do
  A[I] := Default(T);
  result := A;
end;

class function TIntegerArray<T>.Create(NewLength: Integer): TArray<T>;
var
  A: Array of T;
  I: Integer;
begin
  SetLength(A, NewLength);
  for I := 0 to High(A) do
  A[I] := Default(T);
  result := A;
end;

class function TStringArray<T>.Create(NewLength: Integer): TArray<T>;
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
  
  StringReplace_String_A,
  StringReplace_Ansi_A,
  StringReplace_Wide_A,
  StringReplace_Unicode_A,
  
  StringReplace_String_B,
  StringReplace_Ansi_B,
  StringReplace_Wide_B,
  StringReplace_Unicode_B,

  WideStringReplace,
  
  GetMem_PS,
  GetMem_S,
  
  FreeMem_PS,
  FreeMem_P,
  
  Abs_Int,
  Abs_Int64,
  Abs_VR,
  
  Frac_VR,
  Int_VR,
  Round_VR,
  Trunc_VR,
  
  TRTL_Create,
  TRTL_Destroy;

end.
