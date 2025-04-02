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

{$if defined(codeh) and defined(codei)}
{$FATAL interface and implementation at the same time not possible}
{$endif}

{$ifdef codeh}

function fpc_get_input: PText; compilerproc;
function fpc_get_output: PText; compilerproc;

procedure fpc_write_end(var f: Text); compilerproc;
procedure fpc_writeln_end(var f: Text); compilerproc;

// char, widechar, short, ansi, wide, unicode
procedure fpc_write_text_char(len: LongInt; var t: Text; c : AnsiChar); compilerproc;
procedure fpc_write_text_widechar(len: LongInt; var t: Text; c: WideChar); compilerproc;
procedure fpc_write_text_shortstr(len: LongInt; var f: Text; const s: ShortString); compilerproc;
procedure fpc_write_text_ansistr(len: LongInt; var f: Text; const s: RawByteString); compilerproc;
procedure fpc_write_text_widestr(len: LongInt; var f: Text; const s: WideString); compilerproc;
procedure fpc_write_text_unicodestr(len: LongInt; var f: Text; const s: UnicodeString); compilerproc;

//procedure fpc_write_text_unicodestr(len :longint; var f: Text; const s: UnicodeString); compilerproc;

// pchar
// no idea what is the use of fpc_write_text_pchar_as_array
//procedure fpc_write_text_pchar_as_array(len: longint; var f: Text; const s: array of ansichar; zerobased: boolean = true); compilerproc;
//procedure fpc_write_text_pchar_as_pointer(len: longint; var f: Text; p: pansichar); compilerproc;

// numeric
procedure fpc_write_text_sint(len: longint; var t: Text; l: ValSInt); compilerproc;
procedure fpc_write_text_uint(len: longint; var t: Text; l: ValUInt); compilerproc;
//procedure fpc_write_text_float(rt, fixkomma, len: longint; var t: Text; r: ValReal); compilerproc;
//procedure fpc_write_text_currency(fixkomma, len: longint; var t: Text; c: Currency); compilerproc;
procedure fpc_write_text_qword(len: longint; var t: Text; q: qword); compilerproc;
//procedure fpc_write_text_int64(len: longint; var t: Text; i: int64); compilerproc;

// others
//procedure fpc_write_text_enum(typinfo, ord2strindex: pointer; len: sizeint; var t: Text; ordinal: longint); compilerproc;
procedure fpc_write_text_boolean(len: longint; var t: Text; b: boolean); compilerproc;

// ### read
procedure fpc_read_end(var f: Text); compilerproc;
procedure fpc_readln_end(var f: Text); compilerproc;

// char, widechar, short, ansi, wide, unicode
//procedure fpc_read_text_char(var f: Text; out c: AnsiChar); compilerproc;
//procedure fpc_read_text_widechar(var f: Text; out wc: WideChar); compilerproc;
//procedure fpc_read_text_shortstr(var f: Text; out s: ShortString; cp: TSystemCodePage); compilerproc;
procedure fpc_read_text_ansistr(var f: Text; out s: RawByteString; cp: TSystemCodePage); compilerproc;
//procedure fpc_read_text_widestr(var f: Text; out ws: WideString); compilerproc;
//procedure fpc_read_text_unicodestr(var f: Text; out us: UnicodeString); compilerproc;

// pchar
//procedure fpc_read_text_pchar_as_pointer(var f: Text; const s: PAnsiChar); compilerproc;
//procedure fpc_read_text_pchar_as_array(var f: Text; out s: array of AnsiChar; zerobased: boolean = false); compilerproc;

// numeric
//procedure fpc_read_text_sint(var f: Text; out l: ValSInt); compilerproc;
//procedure fpc_read_text_uint(var f: Text; out u: ValUInt); compilerproc;
//procedure fpc_read_text_float(var f : Text; out v: ValReal); compilerproc;
//procedure fpc_read_text_currency(var f: Text; out v: Currency); compilerproc;
{$ifndef CPU64}
//procedure fpc_read_text_qword(var f: Text; out q: qword); compilerproc;
//procedure fpc_read_text_int64(var f: Text; out i: int64); compilerproc;
{$endif}

// others
// @@todo: check out how it works with this enum at curretly used compiler versus trunk
//{$ifdef VER3_2}
//procedure fpc_read_text_enum(str2ordindex:pointer; var t: Text; out ordinal: longint); compilerproc;
//{$else VER3_2}
//procedure fpc_read_text_enum_longint(str2ordindex: pointer; var t: Text; out ordinal: longint); compilerproc;
//procedure fpc_read_text_enum_smallint(str2ordindex: pointer; var t: Text; out ordinal: smallint); compilerproc;
//procedure fpc_read_text_enum_shortint(str2ordindex: pointer; var t: Text; out ordinal: shortint); compilerproc;
//{$endif}

{$endif}

{$ifdef codei}

function fpc_get_input: PText; compilerproc;
begin
  result := nil;
end;

function fpc_get_output: PText; compilerproc;
begin
  result := nil;
end;

procedure fpc_write_end(var f: Text); compilerproc;
begin
end;

procedure fpc_writeln_end(var f: Text); compilerproc;
var
  s: ansistring;
begin
  s := LineEnding;
  WriteConsoleA(StdOut, @s[1], length(s), nil, nil);
end;

procedure fpc_write_text_char(len: LongInt; var t: Text; c: AnsiChar); [public, alias: 'FPC_WRITE_TEXT_CHAR']; compilerproc;
begin
  WriteConsoleA(StdOut, @c, 1, nil, nil);
end;

procedure fpc_write_text_widechar(len: LongInt; var t: Text; c: WideChar); compilerproc;
begin
  WriteConsoleW(StdOut, @c, 1, nil, nil);
end;

procedure fpc_write_text_shortstr(len: LongInt; var f: Text; const s: ShortString); [public, alias: 'FPC_WRITE_TEXT_SHORTSTR']; compilerproc;
begin
  WriteConsoleA(StdOut, @s[1], ord(s[0]), nil, nil);
end;

// leave it for later for having fun with TextRec
//const
//  fmInput  = $D7B1;
//  fmOutput = $D7B2;

procedure fpc_write_text_ansistr(len: LongInt; var f: Text; const s: RawByteString); [public, alias: 'FPC_WRITE_TEXT_ANSISTR']; compilerproc;
begin
  WriteConsoleA(StdOut, @s[1], length(s), nil, nil);
end;

procedure fpc_write_text_widestr(len: LongInt; var f: Text; const s: WideString); compilerproc;
begin
  if pointer(s) = nil then exit;
  WriteConsoleW(StdOut, @s[1], length(s), nil, nil);
end;

procedure fpc_write_text_unicodestr(len: LongInt; var f: Text; const s: UnicodeString); compilerproc;
begin
  if pointer(s) = nil then exit;
  WriteConsoleW(StdOut, @s[1], length(s), nil, nil);
end;

procedure fpc_read_end(var f: Text); [public, alias: 'FPC_READ_END']; compilerproc;
begin
end;

procedure int_write_to_console(s: AnsiString);
var
  d: dword;
begin
  WriteConsoleA(StdOut, @s[1], length(s), @d, nil);
end;

procedure int_read_from_console(out r: AnsiString);
var
  c: char;
  d: dword;
  s: ansistring;
begin
  s := '';
  while true do begin
    ReadConsoleA(StdIn, @c, 1, @d, nil);
    if c = #13 then begin
      //ReadConsoleA(StdIn, @c, 1, @d, nil); // read #10?
      break;
    end;
    s := s + c;
  end;
  r := s;
  int_write_to_console(#13#10);
end;

procedure fpc_readln_end(var f: Text); [public, alias: 'FPC_READLN_END']; compilerproc;
begin
  // how to wait for #13 on "readln()" without args called?
  //wait_for_enter;
  //TextRec(f).UserData[0] := 0;
  //TextRec(f).UserData[0] := 0;
  //TextRec(f).UserData[0] := 0;
  //TextRec(f).UserData[0] := 0;
end;

procedure fpc_read_text_shortstr(var f: Text; out s: ShortString; cp: TSystemCodePage); [public, alias: 'FPC_READ_TEXT_SHORTSTR']; compilerproc;
var
  t: ansistring;
begin
  int_read_from_console(t);
  s := t;
end;

procedure fpc_read_text_ansistr(var f: Text; out s: RawByteString; cp: TSystemCodePage); [public, alias: 'FPC_READ_TEXT_ANSISTR']; compilerproc;
var
  t: ansistring;
begin
  int_read_from_console(t);
  s := t;
end;

procedure fpc_write_text_sint(len: longint; var t: Text; l: ValSInt); compilerproc;
var
  s: ansistring;
begin
  s := itoa(l);
  WriteConsoleA(StdOut, @s[1], length(s), nil, nil);
end;

procedure fpc_write_text_uint(len: longint; var t: Text; l: ValUInt); compilerproc;
var
  s: ansistring;
begin
  s := itoa(l);
  WriteConsoleA(StdOut, @s[1], length(s), nil, nil);
end;

//procedure fpc_write_text_float(rt, fixkomma, len: longint; var t: Text; r: ValReal); compilerproc;
//procedure fpc_write_text_currency(fixkomma, len: longint; var t: Text; c: Currency); compilerproc;
procedure fpc_write_text_qword(len: longint; var t: Text; q: qword); compilerproc;
var
  s: ansistring;
begin
  s := itoa(q); // @@todo: itoa64 / signed / unsigned
  WriteConsoleA(StdOut, @s[1], length(s), nil, nil);
end;

procedure fpc_write_text_boolean(len: longint; var t: Text; b: boolean); compilerproc;
var
  s: ansistring;
begin
  // @@todo: alias for WriteConsoleA that will handle "len" (blanks)
  if b then s := 'TRUE' else s := 'FALSE';
  WriteConsoleA(StdOut, @s[1], length(s), nil, nil);
end;

{$endif}

