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
function  IntToStr(Value:  Int64): PChar; stdcall; export;
function  UIntToStr(Value: UInt64): AnsiString; stdcall; export;
function  StrAlloc(Size: Cardinal): PChar; stdcall; export;
procedure StrDispose(P: PChar); stdcall; export;
function  StrCopy(Dest: PChar; Source: PChar): PChar; stdcall; export;
function  StrCat(Dest: PChar; Source: PChar): PChar; stdcall; export;
{$endif DLLEXPORT}
{$ifdef DLLIMPORT}
function  IntToStr(Value:  Int64): PChar; stdcall; external RTLDLL;
function  UIntToStr(Value: UInt64): AnsiString; stdcall; external RTLDLL;
function  StrAlloc(Size: Cardinal): PChar; stdcall; external RTLDLL;
procedure StrDispose(P: PChar); stdcall; external RTLDLL;
function  StrCopy(Dest: PChar; Source: PChar): PChar; stdcall; external RTLDLL;
function  StrCat(Dest: PChar; Source: PChar): PChar; stdcall; external RTLDLL;
{$endif DLLIMPORT}
implementation

{$ifdef DLLEXPORT}
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

function StrCopy(Dest: PChar; Source: PChar): PChar; stdcall; export;
begin
  Result := Dest;
  while Source^ <> #0 do
  begin
    Dest^ := Source^;
    Inc(Dest);
    Inc(Source);
  end;
  Dest^ := #0;  // Nullterminator setzen
end;

function StrCat(Dest: PChar; Source: PChar): PChar; stdcall; export;
var
  D: PChar;
begin
  D := Dest;
  // Gehe ans Ende von Dest
  while D^ <> #0 do
    Inc(D);

  // Hänge Source an
  while Source^ <> #0 do
  begin
    D^ := Source^;
    Inc(D);
    Inc(Source);
  end;

  D^ := #0; // Nullterminator setzen
  Result := Dest;
end;

function IntToStrPChar(Value: Int64; Buffer: PChar; BufLen: Integer): PChar;
var
  i: Integer;
  isNegative: Boolean;
  absValue: UInt64;
begin
  if Buffer = nil then
    Exit(nil);

  i := BufLen - 1;
  Buffer[i] := #0;
  Dec(i);

  if Value < 0 then
  begin
    isNegative := True;
    absValue := UInt64(-Value);
  end
  else
  begin
    isNegative := False;
    absValue := UInt64(Value);
  end;

  repeat
    if i < 0 then Exit(nil); // Buffer zu klein
    Buffer[i] := Char(Ord('0') + (absValue mod 10));
    absValue := absValue div 10;
    Dec(i);
  until absValue = 0;

  if isNegative then
  begin
    if i < 0 then Exit(nil);
    Buffer[i] := '-';
    Dec(i);
  end;

  Result := @Buffer[i + 1];
end;

function IntToStr(Value: Int64): PChar; stdcall; export;
var
  Buffer: array[0..63] of Char;
  P: PChar;
begin
  P := IntToStrPChar(Value, Buffer, Length(Buffer));
  if P = nil then
  begin
    MessageBoxA(0,'IntToStr(): buffer = nil', 'Error', 0);
    result := nil;
    Exit;
  end;
  result := P;
end;

function UIntToStr(Value: UInt64): AnsiString; stdcall; export;
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

exports
  IntToStr   name 'IntToStr',
  UIntToStr  name 'UIntToStr',
  StrAlloc   name 'StrAlloc',
  StrCat     name 'StrCat',
  StrCopy    name 'StrCopy',
  StrDispose name 'StrDispose'
  ;
{$endif DLLEXPORT}

end.
