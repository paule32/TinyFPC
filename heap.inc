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

// allocates a memory block of the specified size; returns a pointer to the block
function GetMem(size: ptruint): pointer;
// allocates memory and returns a pointer to the block; stores the pointer in p as an out parameter
function GetMem(out p: pointer; size: ptruint): pointer;
// frees the memory and returns the actual amount of memory released for reuse
function FreeMem(p: pointer): ptruint;
// frees the memory and returns the actual amount of memory released for reuse; size parameter is ignored
function FreeMemSize(p: pointer; {%H-}size: ptruint): ptruint;
// allocates a memory block of the specified size and zeroes it; returns a pointer to the block
function AllocMem(size: ptruint): pointer;
// resizes the memory block pointed to by p; returns a pointer to the new block
function ReAllocMem(var p: pointer; size: ptruint): pointer;
// returns the size of the allocated memory block pointed to by p (excluding the header size - should it?)
function MemSize(p: pointer): ptruint;

// --------------------------------------------------

// moves count bytes from src to dst
procedure Move(const src; var dst; count: ptruint);
// fills count bytes of memory starting at x with the specified value
procedure FillChar(var x; count: ptruint; value: ansichar);
// fills count bytes of memory starting at x with the specified value
procedure FillChar(var x; count: ptruint; value: byte);
// fills count bytes of memory starting at x with the specified value
procedure FillByte(var x; count: ptruint; value: byte);
// returns the index of the first occurrence of b in buf of length len; returns -1 if not found
function IndexByte(const buf; len: ptrint; b: byte): ptrint;
// returns the index of the first occurrence of b in buf of length len; returns -1 if not found
function IndexChar(const buf; len: ptrint; b: char): ptrint;
// returns the index of the first occurrence of b in buf of length len; returns -1 if not found
function IndexWord(const buf; len: ptrint; w: word): ptrint;
// compares len bytes between buf1 and buf2; returns the difference between the first differing bytes
function CompareByte(const buf1; const buf2; len: ptruint): ptrint;
// compares len characters between buf1 and buf2; returns the difference between the first differing characters
function CompareChar(const buf1; const buf2; len: ptruint): ptrint;

// --------------------------------------------------

function fpc_getmem(size: ptruint): pointer; compilerproc;
procedure fpc_freemem(p: pointer); compilerproc;

function xgetmem(size: ptruint): pointer; stdcall; external RTLDLL;
function xallocmem(size: ptruint): pointer; stdcall; external RTLDLL;
function xreallocmem(var p: pointer; size: QWord): pointer; external RTLDLL;
function xmemdiffat(const p1, p2: pointer; len: ptruint): ptruint; stdcall; external RTLDLL;

{$endif}

{$ifdef codei}

function GetMem(size: ptruint): pointer;
begin
  result := xgetmem(size);
end;

function GetMem(out p: pointer; size: ptruint): pointer;
begin
  result := xgetmem(size);
  p := result;
end;

function FreeMem(p: pointer): ptruint;
begin
  result := xfreemem(p);
end;

function FreeMemSize(p: pointer; size: ptruint): ptruint;
begin
  result := xfreemem(p);
end;

function AllocMem(size: ptruint): pointer;
begin
  result := xallocmem(size);
end;

function ReAllocMem(var p: pointer; size: ptruint): pointer;
begin
  result := xreallocmem(p, size);
end;

function MemSize(p: pointer): ptruint;
begin
  result := xmemsize(p);
end;

// --------------------------------------------------

procedure Move(const src; var dst; count: ptruint);
begin
  xmovemem(@src, @dst, count);
end;

procedure FillChar(var x; count: ptruint; value: ansichar);
begin
  xfillmem(@x, count, ord(value));
end;

procedure FillChar(var x; count: ptruint; value: byte);
begin
  xfillmem(@x, count, value);
end;

procedure FillByte(var x; count: ptruint; value: byte);
begin
  xfillmem(@x, count, value);
end;

function IndexByte(const buf; len: ptrint; b: byte): ptrint;
var
  i: ptrint;
begin
  result := -1;    
  i := 0;
  while (i < len) or (len = -1){len may not be given} do begin
    if pbyte(pointer(buf)+i)^ = b then exit(i);
    inc(i);
  end;
end;

function IndexChar(const buf; len: ptrint; b: char): ptrint;
var
  i: ptrint;
begin
  result := -1;
  i := 0;
  while (i < len) or (len = -1){len may not be given} do begin
    if pchar(pointer(buf)+i)^ = b then exit(i);
    inc(i);
  end;
end;

function IndexWord(const buf; len: ptrint; w: word): ptrint;
var
  i: ptrint;
begin
  result := -1;   
  i := 0;
  while (i < len) or (len = -1){len may not be given} do begin
    if pword(pointer(buf)+i)^ = w then exit(i div 2);
    inc(i, 2);
  end;
end;

function CompareByte(const buf1; const buf2; len: ptruint): ptrint;
var
  u: ptruint;
begin
  u := xmemdiffat(@buf1, @buf2, len);
  if u = len then result := 0 else result := pbyte(@buf1+u)^-pbyte(@buf2+u)^;
end;

function CompareChar(const buf1; const buf2; len: ptruint): ptrint;
begin
  result := CompareByte(buf1, buf2, len);
end;
                   
// --------------------------------------------------

function fpc_getmem(size: ptruint): pointer; compilerproc; [public, alias: 'FPC_GETMEM'];
begin
  result := GetMem(size);
end;

procedure fpc_freemem(p: pointer); compilerproc; [public, alias: 'FPC_FREEMEM'];
begin
  FreeMem(p);
end;

{$endif}

