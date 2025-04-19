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

uses Windows, global, SysUtils, QApplicationPascalExport;

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

function StringReplace_A(
    S: AnsiString;
    oldPattern: AnsiString;
    newPattern: AnsiString;
    Flags: TReplaceFlags ): PDLLrequest; stdcall export;

implementation

const mfcfpc = 'mfcfpc.dll';

function GetBsr8bit: PByteLookup; stdcall; export;
begin
  result := @bsr8bit;
end;

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
    external mfcfpc name 'fpc_ReplaceText';*)

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
    );*)
    
    MessageBoxA(0, 'temp', 'abcd', 0);
    
    result := @P;
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
  GetBsr8bit            name 'GetBsr8bit',
  CommandLineToArgvA    name 'CommandLineToArgvA',
  
  fpchandleerror        name 'fpchandleerror',
  fpcdivint64           name 'fpcdivint64',
  fpcdivqword           name 'fpcdivqword',
  
  fpcsetjmp          name 'fpcsetjmp',
  fpclongjmp         name 'fpclongjmp',
  
  fpcdynarraysetlength    name 'fpcdynarraysetlength',
  
  newansistring           name 'newansistring',
  newwidestring           name 'newwidestring',
  
  fpctruelyansistrunique  name 'fpctruelyansistrunique',
  
  fpcshortstrconcat     name 'fpcshortstrconcat',
  fpcshortstrtoansistr  name 'fpcshortstrtoansistr',
  fpcshortstrcopy       name 'fpcshortstrcopy',
  
  fpcansistrassign        name 'fpcansistrassign',
  fpcansistrsetlength     name 'fpcansistrsetlength',
  fpcansistrdecrref       name 'fpcansistrdecrref',
  fpcansistrcopy          name 'fpcansistrcopy',
  fpcansistrcompareequal  name 'fpcansistrcompareequal',
  
  fpcpwidechartoansistr name 'fpcpwidechartoansistr',
  fpcwidestrassign      name 'fpcwidestrassign',
  fpcpwidechartowidestr name 'fpcpwidechartowidestr',
  fpcwidestrincrref     name 'fpcwidestrincrref',
  fpcpchartoansistr     name 'fpcpchartoansistr',
  
  makeuniqueansistring  name 'makeuniqueansistring',
  
  UIntToStrA            name 'UIntToStrA',
  
  itoa_exp              name 'itoa_exp',
  atoi_exp              name 'atoi_exp',
  
  PushException_              name 'PushException_',
  signals_exception_handler_  name 'signals_exception_handler_',
  fpc_popobjectstack_         name 'fpc_popobjectstack_',

  StringReplace_A  name 'StringReplace_A',
  
  QApplication_Create   name 'QApplication_Create',
  QApplication_Destroy  name 'QApplication_Destroy',

  TRTL_Create,
  TRTL_Destroy;

end.
