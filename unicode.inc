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

{ Copyright (c) 2024 fibodevy https://github.com/fibodevy | License: LGPL }

{$if defined(codeh) and defined(codei)}
{$FATAL interface and implementation at the same time not possible}
{$endif}

{$ifdef codeh}

// decode utf-8 to unicode codepoint
function utf8decode(var cur: pansichar; curend: pansichar; out codepoint: dword; out size: byte): boolean;
// encode unicode codepoint to utf-8
function utf8encode(codepoint: dword): ansistring;
// decode utf-16 to unicode codepoint
function utf16decode(var cur: pwidechar; curend: pwidechar; out codepoint: dword; out size: byte): boolean;
// encode unicode codepoint to utf-16
function utf16encode(codepoint: dword): unicodestring;
// convert utf-8 ansistring to utf-16 unicodestring
function utf8toutf16(utf8: ansistring): unicodestring;
// convert utf-8 ansichar to utf-16 unicodestring
function utf8toutf16(utf8: pansichar): unicodestring;
// convert utf-16 unicodestring to utf-8 ansistring
function utf16toutf8(utf16: unicodestring): ansistring;
// convert utf-16 unicodechar to utf-8 ansistring
function utf16toutf8(utf16: pwidechar): ansistring;
// check if a unicode codepoint represents a newline
function is_codepoint_newline(u: dword): boolean;
// get the length of a utf-8 string
function utf8len(const utf8: pansichar): sizeint;

{$endif}

{$ifdef codei}

function utf8decode(var cur: pansichar; curend: pansichar; out codepoint: dword; out size: byte): boolean; inline;
begin
  if (curend <> nil) and not (cur < curend) then exit(false);
  if cur^ = #0 then exit(false);
  if (byte(cur^) and $f0 = $f0) then begin
    // 4 bytes
    if (curend <> nil) and (cur+4 > curend) then exit(false);
    codepoint := ((byte(cur^) and $07) shl 18)
      or ((byte((cur+1)^) and $3f) shl 12)
      or ((byte((cur+2)^) and $3f) shl 6)
      or ((byte((cur+3)^) and $3f));
    size := 4;
  end
  else if (byte(cur^) and $e0 = $e0) then begin
    // 3 bytes
    if (curend <> nil) and (cur+3 > curend) then exit(false);
    codepoint := ((byte(cur^) and $0f) shl 12)
      or ((byte((cur+1)^) and $3f) shl 6)
      or ((byte((cur+2)^) and $3f));
    size := 3;
  end
  else if (byte(cur^) and $c0 = $c0) then begin
    // 2 bytes
    if (curend <> nil) and (cur+2 > curend) then exit(false);
    codepoint := ((byte(cur^) and $1f) shl 6)
      or ((byte((cur+1)^) and $3f));
    size := 2;
  end
  else begin
    // 1 byte
    codepoint := byte(cur^);
    size := 1;
  end;
  inc(cur, size);
  result := true;
end;

function utf8encode(codepoint: dword): ansistring; inline;
begin
  if codepoint <= $7F then
    result := ansichar(codepoint)
  else if codepoint <= $7ff then begin
    result := ansichar($c0 or (codepoint shr 6));
    result := result + ansichar($80 or (codepoint and $3f));
  end
  else if codepoint <= $ffff then begin
    result := ansichar($e0 or (codepoint shr 12));
    result := result + ansichar($80 or ((codepoint shr 6) and $3f));
    result := result + ansichar($80 or (codepoint and $3f));
  end
  else begin
    result := ansichar($f0 or (codepoint shr 18));
    result := result + ansichar($80 or ((codepoint shr 12) and $3f));
    result := result + ansichar($80 or ((codepoint shr 6) and $3f));
    result := result + ansichar($80 or (codepoint and $3f));
  end;
end;

function utf16decode(var cur: pwidechar; curend: pwidechar; out codepoint: dword; out size: byte): boolean; inline;
begin
  if (curend <> nil) and not (cur < curend) then exit(false);
  if cur^ = #0 then exit(false);
  codepoint := dword(cur^);
  size := 1;
  // surrogate?
  if (codepoint >= $d800) and (codepoint <= $dbff) then begin
    if (curend = nil) or ((cur+1 <= curend) and (dword((cur+1)^) >= $dc00) and (dword((cur+1)^) <= $dfff)) then begin
      codepoint := ((codepoint-$d800) shl 10)+(dword((cur+1)^)-$dc00)+$10000;
      size := 2;
    end;
  end;
  inc(cur, size);
  result := true;
end;

function utf16encode(codepoint: dword): unicodestring; inline;
begin
  if codepoint > $ffff then begin
    result := widechar($d800+((codepoint-$10000) shr 10));
    result := result + widechar($dc00+((codepoint-$10000) and $3ff));
  end else
    result := widechar(codepoint);
end;

function utf8toutf16(utf8: ansistring): unicodestring;
var
  i: integer;
  cur, curend: pansichar;
  codepoint: dword;
  size: byte;
begin
  curend := @utf8[1]+length(utf8);
  result := '';
  i := 1;
  while i <= length(utf8) do begin
    cur := @utf8[i];
    if utf8decode(cur, curend, codepoint, size) then begin
      //writeln('codepoint = ', codepoint, ' | size = ', size);
      result := result + utf16encode(codepoint);
      inc(i, size);
    end else begin
      result := result + '?';
      inc(cur);
      inc(i);
    end;
  end;
end;

function utf8toutf16(utf8: pansichar): unicodestring;
var
  i: integer;
  cur: pansichar;
  codepoint: dword;
  size: byte;
begin
  cur := utf8;
  result := '';
  i := 1;
  while utf8decode(cur, nil, codepoint, size) do begin
    //writeln('codepoint = ', codepoint, ' | size = ', size);
    result := result + utf16encode(codepoint);
    inc(i, size);
  end;
end;

function utf16toutf8(utf16: unicodestring): ansistring;
var
  i: integer;
  cur, curend: pwidechar;
  codepoint: dword;
  size: byte;
begin
  curend := @utf16[1]+(length(utf16)*2);
  result := '';
  i := 1;
  while i <= length(utf16) do begin
    cur := @utf16[i];
    if utf16decode(cur, curend, codepoint, size) then begin
      //writeln('codepoint = ', codepoint, ' | size = ', size);
      result := result + utf8encode(codepoint);
      inc(i, size);
    end else begin
      result := result + '?';
      inc(cur);
      inc(i);
    end;
  end;
end;

function utf16toutf8(utf16: pwidechar): ansistring;
var
  i: integer;
  cur: pwidechar;
  codepoint: dword;
  size: byte;
begin
  cur := utf16;
  result := '';
  i := 1;
  while utf16decode(cur, nil, codepoint, size) do begin
    //writeln('codepoint = ', codepoint, ' | size = ', size);
    result := result + utf8encode(codepoint);
    inc(i, size);
  end;
end;

function is_codepoint_newline(u: dword): boolean;
begin
  result := ((u <= 127) and ((u = 10) or (u = 13) or (u = $85))) or ((u = $2028) or (u = $2029));
end;

function utf8len(const utf8: pansichar): sizeint;
var
  cur: pansichar;
  codepoint: dword;
  size: byte;
begin
  cur := utf8;
  result := 0;
  while utf8decode(cur, nil, codepoint, size) do begin
    inc(result);
  end;
end;

{$endif}

