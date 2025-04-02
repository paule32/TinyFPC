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

{$ifdef CPU64}
function AtomicExchange(var target: integer; val: integer): integer; assembler;
function AtomicExchange(var target: int64; val: int64): int64; assembler;
function AtomicIncrement(var target: integer): integer; assembler;
function AtomicIncrement(var target: int64): int64; assembler;
function AtomicDecrement(var target: integer): integer; assembler;
function AtomicDecrement(var target: int64): int64; assembler;
procedure inclocked(var target: integer); inline;
procedure inclocked(var target: int64); inline;
function declocked(var target: integer): boolean; inline;
function declocked(var target: int64): boolean; inline;
{$else}
function AtomicExchange(var target: integer; val: integer): integer; assembler;
function AtomicIncrement(var target: integer): integer; assembler;
function AtomicDecrement(var target: integer): integer; assembler;
procedure inclocked(var target: integer); inline;
function declocked(var target: integer): boolean; inline;
{$endif}

// create a language identifier from primary and sublanguage
function MAKELANGID(primary, sub: word): word; inline;
// find position of needle ansistring in haystack; search starts from offset
function pos(const needle: ansistring; const haystack: ansistring; offset: sizeint=1): sizeint;
// find position of needle unicodestring in haystack; search starts from offset
function pos(const needle: unicodestring; const haystack: unicodestring; offset: sizeint=1): sizeint;
// find position of needle widestring in haystack; search starts from offset
function pos(const needle: widestring; const haystack: widestring; offset: sizeint=1): sizeint;

{$endif}

{$ifdef codei}

{$asmmode intel}
{$ifdef CPU64}
function AtomicExchange(var target: integer; val: integer): integer; assembler;
asm
  mov    eax, val
  lock   xchg [target], eax
end;

function AtomicExchange(var target: int64; val: int64): int64; assembler;
asm
  mov    rax, val
  lock   xchg [target], rax
end;

function AtomicIncrement(var target: integer): integer; assembler;
asm
  mov eax, 1
  lock xadd [target], eax
  inc eax
end;

function AtomicIncrement(var target: int64): int64; assembler;
asm
  mov rax, 1
  lock xadd [target], rax
  inc rax
end;

function AtomicDecrement(var target: integer): integer; assembler;
asm
  mov eax, -1
  lock xadd [target], eax
  dec eax
end;

function AtomicDecrement(var target: int64): int64; assembler;
asm
  mov rax, -1
  lock xadd [target], rax
  dec rax
end;

procedure inclocked(var target: integer); inline;
begin
  if IsMultithread then AtomicIncrement(target) else inc(target);
end;

procedure inclocked(var target: int64); inline;
begin
  if IsMultithread then AtomicIncrement(target) else inc(target);
end;

function declocked(var target: integer): boolean; inline;
begin
  if IsMultithread then
    result := AtomicDecrement(target) = 0
  else begin
    dec(target);
    result := target = 0;
  end;
end;

function declocked(var target: int64): boolean; inline;
begin
  if IsMultithread then
    result := AtomicDecrement(target) = 0
  else begin
    dec(target);
    result := target = 0;
  end;
end;
{$else}

function AtomicExchange(var target: integer; val: integer): integer; assembler;
asm
  mov    eax, target
  xchg   [target], val
  mov    eax, val
end;

function AtomicIncrement(var target: integer): integer; assembler;
asm
  mov edx, 1
  xchg edx, eax
  lock xadd [edx], eax
  inc eax
end;

function AtomicDecrement(var target: integer): integer; assembler;
asm
  mov edx, -1
  xchg edx, eax
  lock xadd [edx], eax
  dec eax
end;

procedure inclocked(var target: integer); inline;
begin
  if IsMultithread then AtomicIncrement(target) else inc(target);
end;

function declocked(var target: integer): boolean; inline;
begin
  if IsMultithread then
    result := AtomicDecrement(target) = 0
  else begin
    dec(target);
    result := target = 0;
  end;
end;
{$endif}

function MAKELANGID(primary, sub: word): word; inline;
begin
  result := (primary and $3FF) or (sub shl 10);
end;

function pos(const needle: ansistring; const haystack: ansistring; offset: sizeint=1): sizeint;
var
  i: sizeint;
begin
  result := -1;
  for i := offset to length(haystack)-length(needle)+1 do
    if CompareByte(needle[1], haystack[i], length(needle)) = 0 then
      exit(i);
end;

function pos(const needle: unicodestring; const haystack: unicodestring; offset: sizeint=1): sizeint; inline; { inline for wide }
var
  i: sizeint;
begin
  result := -1;
  for i := offset to length(haystack)-length(needle)+1 do
    if CompareByte(needle[1], haystack[i], length(needle)) = 0 then
      exit(i);
end;

function pos(const needle: widestring; const haystack: widestring; offset: sizeint=1): sizeint;
begin
  result := pos(unicodestring(needle), unicodestring(haystack), offset);
end;

{$endif}

