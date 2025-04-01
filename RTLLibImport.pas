{$mode delphi}
{$WARN IMPLICIT_STRING_CAST_LOSS OFF}
{$M-}
{$linklib rtllib_dll}

unit RTLLibImport;

interface

uses Windows;

const rtllib = 'rtllib.dll';

type
  TReplaceFlags = set of (
    rfReplaceAll,
    rfIgnoreCase
  );

type
  TRTL = class(TObject)
  private
    FOwner: TRTL;
  public
    constructor Create;
    destructor Destroy; override;
  end;

function TRTL_Create: TRTL; stdcall; external rtllib;
procedure TRTL_Destroy(AValue: TRTL); stdcall; external rtllib;

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

procedure GetMem (p: Pointer; Size: PtrUInt);          stdcall; overload; external rtllib name 'GetMem_PS';
function  GetMem (            Size: PtrUInt): Pointer; stdcall; overload; external rtllib name 'GetMem_S';

procedure FreeMem(p: Pointer; Size: PtrUInt);          stdcall; overload; external rtllib name 'FreeMem_PS';
function  FreeMem(p: Pointer               ): PtrUInt; stdcall; overload; external rtllib name 'FreeMem_P';

//procedure New(var P: Pointer               ); stdcall; overload; external rtllib name 'New_P';
//procedure New(var P: Pointer; Size: Integer); stdcall; overload; external rtllib name 'New_PC';

function Abs  (l: Integer): Integer; stdcall; overload; external rtllib name 'Abs_Int';
function Abs  (l: Int64  ): Int64;   stdcall; overload; external rtllib name 'Abs_Int64';
function Abs  (l: ValReal): ValReal; stdcall; overload; external rtllib name 'Abs_VR';

function Frac (d: ValReal): ValReal; stdcall; external rtllib name 'Frac_VR';
function Int  (d: ValReal): ValReal; stdcall; external rtllib name 'Int_VR';
function Round(d: ValReal): ValReal; stdcall; external rtllib name 'Round_VR';
function Trunc(d: ValReal): ValReal; stdcall; external rtllib name 'Trunc_VR';

procedure SetLength(var A: TArray<Boolean>; NewLength: Integer); stdcall; overload; external rtllib name 'SetLength_Array_Boolean';
procedure SetLength(var A: TArray<Char   >; NewLength: Integer); stdcall; overload; external rtllib name 'SetLength_Array_Char';
procedure SetLength(var A: TArray<Byte   >; NewLength: Integer); stdcall; overload; external rtllib name 'SetLength_Array_Byte';
procedure SetLength(var A: TArray<Word   >; NewLength: Integer); stdcall; overload; external rtllib name 'SetLength_Array_Word';
procedure SetLength(var A: TArray<DWord  >; NewLength: Integer); stdcall; overload; external rtllib name 'SetLength_Array_DWord';
procedure SetLength(var A: TArray<Integer>; NewLength: Integer); stdcall; overload; external rtllib name 'SetLength_Array_Integer';
procedure SetLength(var A: TArray<String >; NewLength: Integer); stdcall; overload; external rtllib name 'SetLength_Array_String';

procedure SetLength(var S: AnsiString;    NewLength: Integer); stdcall; overload; external rtllib name 'SetLength_String_Ansi';
procedure SetLength(var S: WideString;    NewLength: Integer); stdcall; overload; external rtllib name 'SetLength_String_Wide';
procedure SetLength(var S: UnicodeString; NewLength: Integer); stdcall; overload; external rtllib name 'SetLength_String_Unicode';

procedure Array_Boolean (NewLength: Integer; var A: T_Array_Boolean); stdcall; external rtllib name 'Array_Boolean';
procedure Array_Char    (NewLength: Integer; var A: T_Array_Char   ); stdcall; external rtllib name 'Array_Char';
procedure Array_Byte    (NewLength: Integer; var A: T_Array_Byte   ); stdcall; external rtllib name 'Array_Byte';
procedure Array_Word    (NewLength: Integer; var A: T_Array_Word   ); stdcall; external rtllib name 'Array_Word';
procedure Array_DWord   (NewLength: Integer; var A: T_Array_DWord  ); stdcall; external rtllib name 'Array_DWord';
procedure Array_Integer (NewLength: Integer; var A: T_Array_Integer); stdcall; external rtllib name 'Array_Integer';
procedure Array_String  (NewLength: Integer; var A: T_Array_String ); stdcall; external rtllib name 'Array_String';

function StringReplace_String_A  (const S: String;        const oldPattern: String;        const newPattern: String;        Flags: TReplaceFlags; out aCount: Integer): String;        stdcall; overload; external rtllib name 'StringReplace_String_A';
function StringReplace_Ansi_A    (const S: AnsiString;    const oldPattern: AnsiString;    const newPattern: AnsiString;    Flags: TReplaceFlags; out aCount: Integer): AnsiString;    stdcall; overload; external rtllib name 'StringReplace_Ansi_A';
function StringReplace_Wide_A    (const S: WideString;    const oldPattern: WideString;    const newPattern: WideString;    Flags: TReplaceFlags; out aCount: Integer): WideString;    stdcall; overload; external rtllib name 'StringReplace_Wide_A';
function StringReplace_Unicode_A (const S: UnicodeString; const oldPattern: UnicodeString; const newPattern: UnicodeString; Flags: TReplaceFlags; out aCount: Integer): UnicodeString; stdcall; overload; external rtllib name 'StringReplace_Unicode_A';

function StringReplace_String_B  (const S: String;        const oldPattern: String;        const newPattern: String;        Flags: TReplaceFlags): String;        stdcall; overload; external rtllib name 'StringReplace_String_B';
function StringReplace_Ansi_B    (const S: AnsiString;    const oldPattern: AnsiString;    const newPattern: AnsiString;    Flags: TReplaceFlags): AnsiString;    stdcall; overload; external rtllib name 'StringReplace_Ansi_B';
function StringReplace_Wide_B    (const S: WideString;    const oldPattern: WideString;    const newPattern: WideString;    Flags: TReplaceFlags): WideString;    stdcall; overload; external rtllib name 'StringReplace_Wide_B';
function StringReplace_Unicode_B (const S: UnicodeString; const oldPattern: UnicodeString; const newPattern: UnicodeString; Flags: TReplaceFlags): UnicodeString; stdcall; overload; external rtllib name 'StringReplace_Unicode_B';

function WideStringReplace(const S: WideString; const oldPattern: WideString; const newPattern: WideString; Flags: TReplaceFlags): WideString; stdcall; external rtllib name 'WideStringReplace';

implementation

class function TBooleanArray<T>.Create(NewLength: Integer): TArray<T>; stdcall;
var
  A: Array of T;
begin
  Array_Boolean(NewLength, A);
  result := A;
end;

class function TCharArray<T>.Create(NewLength: Integer): TArray<T>; stdcall;
var
  A: Array of T;
begin
  Array_Char(NewLength, A);
  result := A;
end;

class function TByteArray<T>.Create(NewLength: Integer): TArray<T>; stdcall;
var
  A: Array of T;
  I: Integer;
begin
  Array_Byte(NewLength, A);
  result := A;
end;

class function TWordArray<T>.Create(NewLength: Integer): TArray<T>; stdcall;
var
  A: Array of T;
begin
  Array_Word(NewLength, A);
  result := A;
end;

class function TDWordArray<T>.Create(NewLength: Integer): TArray<T>; stdcall;
var
  A: Array of T;
begin
  Array_DWord(NewLength, A);
  result := A;
end;

class function TIntegerArray<T>.Create(NewLength: Integer): TArray<T>; stdcall;
var
  A: Array of T;
begin
  Array_Integer(NewLength, A);
  result := A;
end;

class function TStringArray<T>.Create(NewLength: Integer): TArray<T>; stdcall;
var
  A: Array of T;
begin
  Array_String(NewLength, A);
  result := A;
end;

constructor TRTL.Create;
begin
  FOwner := TRTL_Create;

end;

destructor TRTL.Destroy;
begin
  TRTL_Destroy(FOwner);
end;

begin
end.
