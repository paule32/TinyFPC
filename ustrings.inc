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

type
  punicoderec = ^tunicoderec;
  tunicoderec = record
    codepage: TSystemCodePage;
    elementsize: word;
    ref: SizeInt;
    len: SizeInt;
  end;

const
  ufirstoff = sizeof(tunicoderec);

procedure fpc_unicodestr_assign(var s1: pointer; s2: pointer); compilerproc;
procedure fpc_unicodestr_setlength(var s: unicodestring; l: sizeint); compilerproc;
procedure fpc_unicodestr_concat(var dests: unicodestring; const s1, s2: unicodestring); compilerproc;
procedure fpc_unicodestr_concat_multi(var dests: unicodestring; const sarr: array of unicodestring); compilerproc;
procedure fpc_unicodestr_incr_ref(s: pointer); compilerproc;
procedure fpc_unicodestr_decr_ref(var s: pointer); compilerproc;
function fpc_unicodestr_unique(var s: pointer): pointer; compilerproc;
function fpc_unicodestr_copy(const s: unicodestring; index, size: sizeint): unicodestring; compilerproc;
function fpc_uchar_to_unicodestr(const c: unicodechar): unicodestring; compilerproc;
function fpc_unicodestr_to_ansistr(const s2: unicodestring; cp : tsystemcodepage): ansistring; compilerproc;
function fpc_pwidechar_to_unicodestr(const p: pwidechar): unicodestring; compilerproc;
function fpc_widestr_to_unicodestr(const s2: widestring): unicodestring; compilerproc;

{$endif}

{$ifdef codei}

function newunicodestring(len: sizeint): pointer;
begin
  GetMem(result, len*2+sizeof(tunicoderec)+2); {2 for #0}
  punicoderec(result)^.len := len;
  punicoderec(result)^.ref := 1;
  punicoderec(result)^.codepage := DefaultUnicodeCodePage;
  punicoderec(result)^.elementsize := 2;
  inc(result, ufirstoff);
  PUnicodeChar(result)^ := #0;
end;

procedure fpc_unicodestr_assign(var s1: pointer; s2: pointer); compilerproc; [public, alias: 'FPC_UNICODESTR_ASSIGN'];
begin
  if s2 <> nil then
    if punicoderec(s2-ufirstoff)^.ref > 0 then
      inc(punicoderec(s2-ufirstoff)^.ref);
  fpc_unicodestr_decr_ref(s1);
  s1 := s2;
end;

procedure fpc_unicodestr_setlength(var s: unicodestring; l: sizeint); compilerproc; [public, alias: 'FPC_UNICODESTR_SETLENGTH'];
var
  u: unicodestring;
  n: sizeint;
begin
  if l = 0 then begin
    fpc_unicodestr_decr_ref(pointer(s));
    exit;
  end;
  pointer(u) := newunicodestring(l);
  if pointer(s) <> nil then begin
    n := length(s)+1; // inc terminator
    if n > l then n := l;
    move(pointer(s)^, pointer(u)^, n*2);
    fpc_unicodestr_decr_ref(pointer(s));
  end;
  s := u;
end;

procedure fpc_unicodestr_concat(var dests: unicodestring; const s1, s2: unicodestring); compilerproc;
var
  u: unicodestring;
begin
  if s1 = '' then begin
    dests := s2;
    exit;
  end;
  if s2 = '' then begin
    dests := s1;
    exit;
  end;
  setlength(u, length(s1)+length(s2));
  move(pointer(s1)^, pointer(u)^, length(s1)*2);
  move(pointer(s2)^, (pointer(u)+(length(s1)*2))^, length(s2)*2);
  dests := u;
end;

procedure fpc_unicodestr_concat_multi(var dests: unicodestring; const sarr: array of unicodestring); compilerproc;
var
  l, p: sizeint;
  i: integer;
begin
  l := 0;
  for i := 0 to high(sarr) do begin
    if (pointer(sarr[i]) <> nil) and (sarr[i] <> '') then l := l + length(sarr[i]);
  end;
  setlength(dests, l);
  p := 1;
  for i := 0 to high(sarr) do begin
    if (pointer(sarr[i]) <> nil) and (sarr[i] <> '') then begin
      move(pointer(sarr[i])^, pointer(@dests[p])^, length(sarr[i]));
      inc(p, length(sarr[i]));
    end;
  end;
end;

procedure fpc_unicodestr_incr_ref(s: pointer); compilerproc; [public, alias: 'FPC_UNICODESTR_INCR_REF'];
begin
  if (s = nil) or (punicoderec(s-ufirstoff)^.ref < 0) then exit;
  inc(punicoderec(s-ufirstoff)^.ref);
end;

procedure fpc_unicodestr_decr_ref(var s: pointer); compilerproc; [public, alias: 'FPC_UNICODESTR_DECR_REF'];
begin
  if (s = nil) or (punicoderec(s-ufirstoff)^.ref < 0) then exit;
  dec(punicoderec(s-ufirstoff)^.ref);
  if punicoderec(s-ufirstoff)^.ref <= 0 then FreeMem(s-ufirstoff);
  s := nil;
end;

function fpc_unicodestr_unique(var s: pointer): pointer; compilerproc; [public, alias: 'FPC_UNICODESTR_UNIQUE'];
var
  u: unicodestring;
begin
  pointer(u) := newunicodestring(punicoderec(s-ufirstoff)^.len);
  move(s^, pointer(u)^, punicoderec(s-ufirstoff)^.len);
  s := pointer(u);
end;

function fpc_unicodestr_copy(const s: unicodestring; index, size: sizeint): unicodestring; compilerproc;
begin
  dec(index);
  if index < 0 then index := 0;
  if (size > length(s)) or (size+index > length(s)) then size := length(s)-index;
  setlength(result, size);
  move((pointer(s)+(index*2))^, pointer(result)^, size*2);
end;

function fpc_uchar_to_unicodestr(const c: unicodechar): unicodestring; compilerproc;
begin
  setlength(result, 1);
  pwidechar(@result[1])^ := c;
end;

function fpc_unicodestr_to_ansistr(const s2: unicodestring; cp : tsystemcodepage): ansistring; compilerproc;
begin
  result := utf16toutf8(s2);
end;

function fpc_pwidechar_to_unicodestr(const p: pwidechar): unicodestring; compilerproc;
var
  s: sizeint;
begin
  if p = nil then exit('');
  s := IndexWord(p, -1, 0);
  setlength(result, s);
  move(p^, pointer(result)^, s*2);
end;

function fpc_widestr_to_unicodestr(const s2: widestring): unicodestring; compilerproc;
begin
  setlength(result, length(s2));
  move(pointer(s2)^, pointer(result)^, length(s2)*2);
end;

{$endif}

