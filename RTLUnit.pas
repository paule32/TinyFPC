{$mode delphi}
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
  TArray<T> = Array of T;
  TArrayHelper<T> = class
    class function Create(NewLength: Integer): TArray<T>;
  end;

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

class function TArrayHelper<T>.Create(NewLength: Integer): TArray<T>;
var
  A: Array of T;
begin
  SetLength(A, NewLength);
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
