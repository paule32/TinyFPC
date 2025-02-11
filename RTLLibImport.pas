{$mode delphi}
{$linklib rtllib_dll}

unit RTLLibImport;

interface

uses Windows;

const rtllib = 'rtllib.dll';

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

procedure SetLength(var A: Array of Boolean; NewLength: Integer); stdcall; overload; external rtllib name 'SetLength_Array_Boolean';
procedure SetLength(var A: Array of Char;    NewLength: Integer); stdcall; overload; external rtllib name 'SetLength_Array_Char';
procedure SetLength(var A: Array of Byte;    NewLength: Integer); stdcall; overload; external rtllib name 'SetLength_Array_Byte';
procedure SetLength(var A: Array of Word;    NewLength: Integer); stdcall; overload; external rtllib name 'SetLength_Array_Word';
procedure SetLength(var A: Array of DWord;   NewLength: Integer); stdcall; overload; external rtllib name 'SetLength_Array_DWord';
procedure SetLength(var A: Array of Integer; NewLength: Integer); stdcall; overload; external rtllib name 'SetLength_Array_Integer';
procedure SetLength(var A: Array of String;  NewLength: Integer); stdcall; overload; external rtllib name 'SetLength_Array_String';

procedure SetLength(var S: String;        NewLength: Integer); stdcall; overload; external rtllib name 'SetLength_String';
procedure SetLength(var S: AnsiString;    NewLength: Integer); stdcall; overload; external rtllib name 'SetLength_String_Ansi';
procedure SetLength(var S: WideString;    NewLength: Integer); stdcall; overload; external rtllib name 'SetLength_String_Wide';
procedure SetLength(var S: UnicodeString; NewLength: Integer); stdcall; overload; external rtllib name 'SetLength_String_Unicode';

implementation

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
