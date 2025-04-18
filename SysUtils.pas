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
unit SysUtils;

interface
uses global;

{$ifdef DLLEXPORT}
function  IntToStr(Value: Integer): PChar; stdcall; export; overload;
function  IntToStr(Value:  Int64): PChar; stdcall; export; overload;

function  UIntToStr(Value: UInt64): AnsiString; stdcall; export;
function  StrAlloc(Size: Cardinal): PChar; stdcall; export;
procedure StrDispose(P: PChar); stdcall; export;

function  StrCopy(var Dest: PChar; Source: PChar): PChar; stdcall; export;
function  StrCat (var Dest: PChar; Source: PChar): PChar; stdcall; export;

function UIntToStrA(Value: UInt64): AnsiString; stdcall; export;

function StrCopy_(var Dest: PChar; Source: PChar): PChar; stdcall; export;
function StrCat_ (var Dest: PChar; Source: PChar): PChar; stdcall; export;

{ Ansi-Version }
function StrPas(P: PAnsiChar): AnsiString; overload;

{ Unicode-Version }
//function StrPas(P: PChar): UnicodeString; overload;

function _itoa  (Value: Integer; Buffer: PAnsiChar; Radix: Integer): PAnsiChar; cdecl; external 'msvcrt.dll' name '_itoa';
function _i64toa(Value:   Int64; Buffer: PAnsiChar; Radix: Integer): PAnsiChar; cdecl; external 'msvcrt.dll' name '_i64toa';
function _i64tow(Value:   Int64; Buffer: PWideChar; Radix: Integer): PWideChar; cdecl; external 'msvcrt.dll' name '_i64tow';

//Function fpc_chararray_to_ansistr(const arr: array of char; zerobased: boolean = true): ansistring; compilerproc;

{$endif DLLEXPORT}
{$ifdef DLLIMPORT}
function  IntToStr(Value: Integer): PChar; stdcall; overload; external RTLDLL;
function  IntToStr(Value:  Int64): PChar; stdcall; overload; external RTLDLL;

function  UIntToStr(Value: UInt64): AnsiString; stdcall; external RTLDLL;
function  StrAlloc(Size: Cardinal): PChar; stdcall; external RTLDLL;
procedure StrDispose(P: PChar); stdcall; external RTLDLL;
function  StrCopy(var Dest: PChar; Source: PChar): PChar; stdcall; external RTLDLL;
function  StrCat (var Dest: PChar; Source: PChar): PChar; stdcall; external RTLDLL;

function UIntToStrA(Value: UInt64): AnsiString; stdcall; external RTLDLL;
function StrCopy_(var Dest: PChar; Source: PChar): PChar; stdcall; external RTLDLL;
function StrCat_ (var Dest: PChar; Source: PChar): PChar; stdcall; external RTLDLL;

Function fpc_chararray_to_ansistr(const arr: array of char; zerobased: boolean = true): ansistring; compilerproc;
{$endif DLLIMPORT}
implementation

procedure fpc_pchar_ansistr_intern_charmove(const src: pchar; const srcindex: sizeint; var dst: rawbytestring; const dstindex, len: sizeint); {$ifdef FPC_HAS_CPSTRING}rtlproc;{$endif} {$ifdef SYSTEMINLINE}inline;{$endif}
begin
  move(src[srcindex],pbyte(pointer(dst))[dstindex],len);
end;

procedure fpc_pchar_pchar_intern_charmove(const src: pchar; const srcindex: sizeint; const dst: pchar; const dstindex, len: sizeint); {$ifdef FPC_HAS_CPSTRING}rtlproc;{$endif} {$ifdef SYSTEMINLINE}inline;{$endif}
begin
  move(src[srcindex],dst[dstindex],len);
end;

Procedure SetString(out S : AnsiString; Buf : PAnsiChar; Len : SizeInt);
begin
  SetLength(S,Len);
  If (Buf<>Nil) then
    fpc_pchar_ansistr_intern_charmove(Buf,0,S,0,Len);
end;

function StrPas(P: PAnsiChar): AnsiString;
var
  Start: PAnsiChar;
  Len: Integer;
begin
  if P = nil then
    Exit('');
  Start := P;
  { Länge bis zum terminierenden #0 ermitteln }
  while P^ <> #0 do
    Inc(P);
  Len := P - Start;
  { AnsiString mit genau Len Zeichen anlegen }
  SetString(Result, Start, Len);
end;

{ 1) Null‑terminiertes statisches oder dynamisches Array }
function CharArrToAnsiStr(const A: array of AnsiChar): AnsiString; overload;
var
  len : Integer;
begin
  result := '';
  if Length(A) = 0 then
  begin
    MessageBoxA(0,'Error: Char Array empty.', 'Error', 0);
    exit;
  end;
  len := 0;
  repeat
    result := result + A[len];
    inc(len);
  until len = Length(A);
  
  //Result := StrPas(PAnsiChar(@A[0]));
end;

{ 2) Statisches oder dynamisches Array mit bekannter Länge }
function CharArrToAnsiStr(const A: array of AnsiChar; BufLen: Integer): AnsiString; overload;
begin
  if (BufLen <= 0) or (Length(A) = 0) then
    Exit('');
  if BufLen > Length(A) then
    BufLen := Length(A);
  SetString(Result, PAnsiChar(@A[0]), BufLen);
end;

{ 3) PAnsiChar + Länge }
function CharArrToAnsiStr(P: PAnsiChar; BufLen: Integer): AnsiString; overload;
begin
  if (P = nil) or (BufLen <= 0) then
    Exit('');
  SetString(Result, P, BufLen);
end;

function StrAlloc(Size: Cardinal): PChar; stdcall; export;
begin
  GetMem(result, Size + 1); // +1 für Nullterminator
  result[0] := #0;          // sicherstellen, dass String leer initialisiert ist
end;

procedure StrDispose(P: PChar); stdcall; export;
begin
  if P <> nil then
    FreeMem(P);
end;

{$ifdef DLLEXPORT}
function StrCopy_(var Dest: PChar; Source: PChar): PChar; stdcall; export;
var
  I : Integer;
  L : Integer;
begin
  L      := StrLen(Source);
  Dest   := StrAlloc(L + 1);
  
  for I := 0 to L - 1 do
  Dest  [i] := Source[i];
  Dest  [L] := #0;
  
  Exit(Dest);
end;
function StrCopy(var Dest: PChar; Source: PChar): PChar; stdcall;
begin
  if Dest = nil then
  Dest := StrAlloc(StrLen(Source) + 1);
  StrCopy_(Dest, Source);
  Exit(Dest);
end;
{$endif DLLEXPORT}
{$ifdef DLLIMPORT}
function StrCopy_(Dest: PChar; Source: PChar): PChar; stdcall; external RTLDLL;
function StrCopy(Dest: PChar; Source: PChar): PChar; stdcall;
begin
MessageBoxA(0, Source, '4444', 0);
  result := StrCopy_(Dest, Source);
end;
{$endif DLLIMPORT}

{$ifdef DLLEXPORT}
function StrCat_(var Dest: PChar; Source: PChar): PChar; stdcall; export;
var
  D       : PChar;
  L, I, J : Integer;
begin
  L := StrLen(Dest) + StrLen(Source) + 1;
  D := StrAlloc(L);
  
  for i := 0 to StrLen(Dest) - 1 do
  D[i] := Dest[i];
  
  for J := 0 to StrLen(Source) - 1 do
  D[i + j + 1] := Source[J];
  D[i + J + 2] := #0;
  
  Dest := D;
  Exit(D);
end;
function StrCat(var Dest: PChar; Source: PChar): PChar; stdcall;
begin
  StrCat_(Dest, Source);
  Exit(Dest);
end;
{$endif DLLEXPORT}
{$ifdef DLLIMPORT}
function StrCat_(var Dest: PChar; Source: PChar): PChar; stdcall; external RTLDLL;
function StrCat (var Dest: PChar; Source: PChar): PChar; stdcall;
begin
  if Dest = nil then
  begin
    MessageBoxA(0,'Error: StrCat Dest not initialized.','Error',0);
    Exit(mil);
  end;
  Exit(StrCat_(Dest, Source));
end;
{$endif DLLIMPORT}

{$ifdef DLLEXPORT}
function IntToStrCRT32(Value: Integer): PChar; stdcall; export;
var
  Buffer : array[0..16] of Char;
  I      : Integer;
begin
  result := StrAlloc(16);
  _itoa(Value, Buffer, 10);
  i := 0;
  repeat
    result[i] := Buffer[i];
    inc(i);
  until i = Length(Buffer);
  result[16] := #0;
end;
function IntToStrCRT64(Value: Int64): PChar; stdcall; export;
var
  Buffer : array[0..32] of AnsiChar;
  I      : Integer;
begin
  result := StrAlloc(32);
  _i64toa(Value, Buffer, 10);
  i := 0;
  repeat
    result[i] := Buffer[i];
    inc(i);
  until i = Length(Buffer);
  result[32] := #0;
end;

function IntToStr(Value: Integer): PChar; stdcall; overload; begin result := IntToStrCRT32(Value); end;
function IntToStr(Value:   Int64): PChar; stdcall; overload; begin result := IntToStrCRT64(Value); end;
{$endif DLLEXPORT}
{$ifdef DLLIMPORT}
function IntToStrCRT32(Value: Integer): PChar; stdcall; external RTLDLL;
function IntToStrCRT64(Value:   Int64): PChar; stdcall; external RTLDLL;

function IntToStr(Value: Integer): PChar; overload; stdcall; begin result := IntToStrCRT32(Value); end;
function IntToStr(Value:   Int64): PChar; overload; stdcall; begin result := IntToStrCRT64(Value); end;
{$endif DLLIMPORT}


{$ifdef DLLEXPORT}
function UIntToStrA(Value: UInt64): AnsiString; stdcall; export;
var
  temp: array[0..31] of Char;
  i: Integer;
begin
  i := High(temp);
  temp[i] := #0;
  Dec(i);

  repeat
    temp[i] := Char(Ord('0') + (Value mod 10));
    Value := Value div 10;
    Dec(i);
  until Value = 0;

  Result := AnsiString(@temp[i + 1]);
end;
function UIntToStr(Value: UInt64): AnsiString; stdcall;
begin
  result := UIntToStrA(Value);
end;
{$endif DLLEXPORT}
{$ifdef DLLIMPORT}
function UIntToStrA(Value: UInt64): AnsiString; stdcall; external RTLDLL;
function UIntToStr(Value: UInt64): AnsiString; stdcall;
begin
  result := UIntToStrA(Value);
end;
{$endif DLLIMPORT}


{$ifdef DLLEXPORT}
exports
  UIntToStr  name 'UIntToStr',
  StrAlloc   name 'StrAlloc',
  StrCopy    name 'StrCopy',
  StrCat     name 'StrCat',
  StrCat_    name 'StrCat_',
  StrDispose name 'StrDispose'
  ;
{$endif DLLEXPORT}

end.
