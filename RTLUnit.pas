{$mode delphi}
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

implementation

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

  TRTL_Create,
  TRTL_Destroy;

end.
