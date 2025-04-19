// ---------------------------------------------------------------------------------------
// Copyright(c) 2025 Jens Kallup - paule32 & fibonacci
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

unit xmm;  

{$WARN 5026 off : Value parameter "$1" is assigned but never used}
{$WARN 3123 off : "$1" not yet supported inside inline procedure/function}    
{$WARN 6058 off : Call to subroutine "$1" marked as inline is not inlined}

{$mode ObjFPC}{$H+}
{$asmmode intel}

//{$define XMMDEBUG}

interface

// XMM Memory Manager; Windows only; uses Windows API to manage the actual memory    
// this is modified version of XMM for use in RTL as default memory manager
// author: fibodevy, 2024
// license: You are free to modify and redistribute the code with attribution to the original author (fibodevy) and a link to the source.

// all functions are self-contained since no external units are used
// this unit may be adapted to environments without Windows or other standard units

const
  // config
  XMMCHUNKSIZE     = 512; // size of each memory chunk in bytes
  XMMCHUNKFIXEDMEM = 4;   // fixed memory limit in MB
  XMMCHUNKCOUNT    = (1024*1024)*XMMCHUNKFIXEDMEM div XMMCHUNKSIZE; // total number of chunks based on fixed memory

const
  // winapi constants (as mentioned above, no Windows unit is used)
  MEM_COMMIT     = $00001000;
  PAGE_READWRITE = $04;
  MEM_RELEASE    = $00008000;

  // import from these
  KERNEL32 = 'kernel32.dll';
  NTDLL    = 'ntdll.dll';
  RTLDLL   = 'rtllib.dll';

function VirtualAlloc(lpAddress: Pointer; dwSize: SizeUInt; flAllocationType, flProtect: DWORD): Pointer; stdcall; external KERNEL32;
function VirtualFree(lpAddress: Pointer; dwSize: size_t; dwFreeType: DWORD): Boolean; stdcall; external KERNEL32;
procedure RtlFillMemory(dst: pointer; len: sizeuint; fill: integer); stdcall; external KERNEL32;
procedure RtlMoveMemory(dst, src: pointer; len: sizeuint); stdcall; external KERNEL32;
procedure RtlZeroMemory(dst: pointer; len: sizeuint); stdcall; external KERNEL32;
function RtlCompareMemory(src1, src2: pointer; len: sizeuint): sizeuint; stdcall; external NTDLL;

{$ifdef DLLEXPORT}
// allocates memory of given size
function xgetmem(size: ptruint): pointer; stdcall; export;
// allocates and zeroes memory of given size
function xallocmem(size: ptruint): pointer; stdcall; export;

// resizes the memory block at p
function xreallocmem_(var p: pointer; size: QWord): pointer; stdcall; export;
function xreallocmem(var p: pointer; size: QWord): pointer;

// clones the memory block at p
function xclone(const p: pointer): pointer; stdcall; export;
// returns size of memory block at p
function xmemsize(const p: pointer): ptruint; stdcall; export;
// returns the actual allocated size of the memory block, including header and OS alignment
function xmemrealsize(const p: pointer): ptruint; stdcall; export;
// returns the actual usable memory size, excluding metadata; the raw allocation size, not guaranteed for safe use by this pointer (but possible)
function xmemavailsize(const p: pointer): ptruint; stdcall; export;
// frees the memory block at p and returns the actual amount of memory released for reuse
function xfreemem(p: pointer): ptruint; stdcall; export;
// zeroes len bytes at p
function xzeromem(p: pointer; len: ptruint): ptruint; stdcall; export;
// moves len bytes from src to dst
function xmovemem(const src: pointer; dst: pointer; len: ptruint): ptruint; stdcall; export;

// fills len bytes at p with value v
function xfillmem(p: pointer; len: ptruint; v: byte): ptruint; stdcall; overload;
function xfillmem(p: pointer; len: ptruint; v: char): ptruint; stdcall; overload;

function xfillmem_a(p: pointer; len: ptruint; v: byte): ptruint; stdcall; export;
function xfillmem_b(p: pointer; len: ptruint; v: char): ptruint; stdcall; export;

// finds offset of first difference
function xmemdiffat(const p1, p2: pointer; len: ptruint): ptruint; stdcall; export;
// compares memory at p1 and p2; returns true if equal
function xmemcompare(const p1, p2: pointer; len: ptruint): boolean; stdcall; export;
// returns the number of free memory chunks
function xgetfreechunks: integer; stdcall; export;
// initializes the memory chunks pool (zeros all)
procedure xmminit; stdcall; export;
{$endif DLLEXPORT}

{$ifdef DLLIMPORT}
//function xgetmem(size: ptruint): pointer; stdcall; external RTLDLL;
//function xallocmem(size: ptruint): pointer; stdcall; external RTLDLL;

//function xreallocmem_(var p: pointer; size: QWord): pointer; stdcall; external RTLDLL;
//function xreallocmem(var p: pointer; size: QWord): pointer;

//function xclone(const p: pointer): pointer; stdcall; external RTLDLL;
function xmemsize(const p: pointer): ptruint; stdcall; external RTLDLL;
//function xmemrealsize(const p: pointer): ptruint; stdcall; external RTLDLL;
//function xmemavailsize(const p: pointer): ptruint; stdcall; external RTLDLL;
//function xfreemem(p: pointer): ptruint; stdcall; external RTLDLL;
//function xzeromem(p: pointer; len: ptruint): ptruint; stdcall; external RTLDLL;
function xmovemem(const src: pointer; dst: pointer; len: ptruint): ptruint; stdcall; external RTLDLL;

function xfillmem(p: pointer; len: ptruint; v: byte): ptruint; stdcall; overload;
function xfillmem(p: pointer; len: ptruint; v: char): ptruint; stdcall; overload;

//function xfillmem_a(p: pointer; len: ptruint; v: byte): ptruint; stdcall; external RTLDLL;
//function xfillmem_b(p: pointer; len: ptruint; v: char): ptruint; stdcall; external RTLDLL;

//function xmemdiffat(const p1, p2: pointer; len: ptruint): ptruint; stdcall; external RTLDLL;
//function xmemcompare(const p1, p2: pointer; len: ptruint): boolean; stdcall; external RTLDLL;
//function xgetfreechunks: integer; stdcall; external RTLDLL;
procedure xmminit_; stdcall; external RTLDLL;
{$endif DLLIMPORT}

implementation

type
  pxmemheader = ^txmemheader;
  pxmemchunk = ^txmemchunk;

  txmemheader = record
    wasallocated: boolean; // indicates if memory was allocated by the OS
    size: ptruint;         // size used by this block, not necessarily the full block size
    realsize: ptruint;     // actual block size from VirtualAlloc or small chunk (consider renaming to "availsize" for clarity)
    prev: pxmemchunk;      // pointer to previous chunk (likely to be removed due to no longer being used)
    next: pxmemchunk;      // pointer to next chunk (likely to be removed due to no longer being used)
  end;

  txmemchunk = record
    h: txmemheader;                        // header for MM metadata
    data: array[0..XMMCHUNKSIZE-1] of byte; // memory chunk of size XMMCHUNKSIZE
  end;

var
  xmemchunks: array[0..XMMCHUNKCOUNT-1] of txmemchunk;
  xmemchunks_critical_section: integer = 0;
  xmemchunks_init_done: boolean = false;

// swaps target with val; returns the old value of target
function _atomic32(var target: integer; val: integer): integer; assembler;
asm
  {$ifdef CPU64}
  mov    eax, val
  lock   xchg [target], eax
  {$else}
  mov    eax, target
  xchg   [target], val
  mov    eax, val
  {$endif}
end;

// enters a critical section; blocks other threads from accessing shared memory
procedure _xmemchunksbegin; inline;
begin
  while _atomic32(xmemchunks_critical_section, 1) <> 0 do; // consider adding a sleep to reduce CPU usage
end;

// exits critical section; releases lock for other threads to access shared memory
procedure _xmemchunksend; inline;
begin
  _atomic32(xmemchunks_critical_section, 0);
end;

// attempts to allocate a memory chunk of the specified size from the chunk pool;
// returns a pointer to the chunk or nil if no chunk is available
{$ifdef DLLEXPORT}
function _xgetmemchunk_(size: dword): pointer; stdcall; export;
var
  i: integer;
begin
  // critical section without try..finally for performance; cuts time in half
  result := nil;
  _xmemchunksbegin;

  // iterate through chunks; reliable and fast method
  if result = nil then begin
    for i := 0 to high(xmemchunks) do begin
      if xmemchunks[i].h.size = 0 then begin
        // free chunk found
        result := @xmemchunks[i].data[0];
        break;
      end;
    end;
  end;

  if result <> nil then begin
    // update chunk header with new size
    pxmemchunk(result-sizeof(txmemheader))^.h.size := size;
  end;

  // if no more chunks, return nil for fallback to standard GetMem
  _xmemchunksend;
end;
function _xgetmemchunk(size: dword): pointer; stdcall;
begin
  Exit(_xgetmemchunk_(size));
end;
{$endif DLLEXPORT}
{$ifdef DLLIMPORT}
function _xgetmemchunk_(size: dword): pointer; stdcall; external RTLDLL;
function _xgetmemchunk (size: dword): pointer; stdcall;
begin
  Exit(_xgetmemchunk_(size));
end;
{$endif DLLIMPORT}

{$ifdef DLLEXPORT}
function xgetmem_a(size: ptruint): pointer; stdcall; export;
begin
  {$ifdef XMMDEBUG}
  writeln('call to xgetmem(', size, ')');
  {$endif}

  result := nil;

  // try to get a small chunk from the memory pool
  if (size > 0) and (size <= XMMCHUNKSIZE) then begin
    result := _xgetmemchunk(size);
    if result <> nil then exit; // succeeded
  end;

  // if size is 0, no need to adjust it; header will handle allocation
  result := VirtualAlloc(nil, size+sizeof(txmemheader), MEM_COMMIT or MEM_RESERVE, PAGE_READWRITE);
  if result = nil then exit;

  // store allocation metadata in the header
  pxmemheader(result)^.size := size;
  {$ifdef CPU64}         
  pxmemheader(result)^.realsize := ((int64(size) div 4096)+1)*4096-sizeof(txmemheader); // align to 4096 bytes (OS alignment), subtract the header size
  {$else}
  // xmm.pas(174,37) Error: Unknown compilerproc "fpc_div_int64". Check if you use the correct run time library.
  pxmemheader(result)^.realsize := (((size) div 4096)+1)*4096-sizeof(txmemheader); // align to 4096 bytes (OS alignment), subtract the header size
  {$endif}
  pxmemheader(result)^.wasallocated := true; // memory allocated by OS; must be freed using VirtualFree
  pxmemheader(result)^.prev := nil; // not used currently; may be removed if it offers no performance gain
  pxmemheader(result)^.next := nil; // not used currently; may be removed if it offers no performance gain

  // move result pointer past the header
  result := result + sizeof(txmemheader);
end;
function xgetmem(size: ptruint): pointer; stdcall; export;
begin
  Exit(xgetmem_a(size));
end;
{$endif DLLEXPORT}
{$ifdef DLLIMPORT}
function xgetmem_a(size: ptruint): pointer; stdcall; external RTLDLL;
function xgetmem(size: ptruint): pointer; stdcall;
begin
  Exit(xgetmem_a(size));
end;
{$endif DLLIMPORT}

function xallocmem(size: ptruint): pointer; stdcall; export;
begin   
  {$ifdef XMMDEBUG}
  writeln('call to xallocmem(', size, ')');
  {$endif}
  result := xgetmem(size);
  if result = nil then exit;
  // zero out the allocated memory
  xfillmem(result, size, 0);
end;

{$ifdef DLLEXPORT}
function xreallocmem_(var p: pointer; size: QWord): pointer; stdcall; export;
var
  h: pxmemheader;
  n: ptruint;
begin
  {$ifdef XMMDEBUG}
  writeln('call to xreallocmem(', ptruint(p), ', ', size, ')');
  {$endif}

  result := nil;

  // free memory if size is 0
  if size = 0 then begin
    if p <> nil then xfreemem(p);
    p := nil;
    exit(nil);
  end;

  // allocate new memory if pointer is nil
  if p = nil then begin
    p := xgetmem(size);
    exit(p);
  end;

  // get the header (and the actual pointer, the one allocated by the OS)
  h := p-sizeof(txmemheader);

  // check if resizing is needed based on current size
  _xmemchunksbegin;
  if not h^.wasallocated then begin
    if size < XMMCHUNKSIZE then begin
      h^.size := size;
      result := p;
    end;
  end;
  _xmemchunksend;

  // will allocate new memory, but perhaps no need
  // use the actual memory size from the OS or the fixed chunk size
  if (result = nil) and (size < h^.realsize) then begin
    // only update the header with the new size
    h^.size := size;
    result := p;
  end;

  if result = nil then begin
    // allocate new memory and move old data
    result := xgetmem(size);
    if result <> nil then begin
      // find smaller of old and new size
      n := size;
      if pxmemheader(p)^.size < n then n := pxmemheader(p)^.size;
      // move memory to new location
      xmovemem(p, result, n);
    end;

    // free old memory and update pointer
    xfreemem(p);
    p := result;
  end;
end;
function xreallocmem(var p: pointer; size: QWord): pointer;
begin
  xreallocmem_(P, size);
  Exit(P);
end;
{$endif DLLEXPORT}
{$ifdef DLLIMPORT}
function xreallocmem_(var p: pointer; size: QWord): pointer; stdcall; external RTLDLL;
function xreallocmem (var p: pointer; size: QWord): pointer;
begin
  xreallocmem_(P, size);
  Exit(P);
end;
{$endif DLLIMPORT}

{$ifdef DLLEXPORT}
function xclone_(const p: pointer): pointer; stdcall; export;
var
  u: ptruint;
begin
  {$ifdef XMMDEBUG}
  writeln('call to xclone(', ptruint(p), ')');
  {$endif}
  u := xmemsize(p);
  result := xgetmem(u);
  xmovemem(p, result, u);
end;
function xclone(const p: pointer): pointer; stdcall;
begin
  Exit(xclone(p));
end;
{$endif DLLEXPORT}
{$ifdef DLLIMPORT}
function xclone_(const p: pointer): pointer; stdcall; external RTLDLL;
function xclone (const p: pointer): pointer; stdcall;
begin
  Exit(xclone(p));
end;
{$endif DLLIMPORT}

{$ifdef DLLEXPORT}
function xmemsize(const p: pointer): ptruint; stdcall; export;
begin
  {$ifdef XMMDEBUG}
  writeln('call to xmemsize(', ptruint(p), ')');
  {$endif}
  result := pxmemheader(p-sizeof(txmemheader))^.size;
end;
{$endif DLLEXPORT}

function xmemrealsize(const p: pointer): ptruint; stdcall; export;
begin
  {$ifdef XMMDEBUG}
  writeln('call to xmemrealsize(', ptruint(p), ')');
  {$endif}
  result := pxmemheader(p-sizeof(txmemheader))^.realsize+sizeof(txmemheader);
end;

function xmemavailsize(const p: pointer): ptruint; stdcall; export;
begin
  {$ifdef XMMDEBUG}
  writeln('call to xmemavailsize(', ptruint(p), ')');
  {$endif}
  result := pxmemheader(p-sizeof(txmemheader))^.realsize;
end;

{$ifdef DLLEXPORT}
function xfreemem_(p: pointer): ptruint; stdcall; export;
var
  h: pxmemheader;
begin
  {$ifdef XMMDEBUG}
  writeln('call to xfreemem(', ptruint(p), ')');
  {$endif}

  _xmemchunksbegin;

  // get the header; if wasallocated, it's the actual OS-allocated memory pointer
  h := p-sizeof(txmemheader);

  // return the realsize, which is the amount of memory available for reuse
  result := h^.realsize;
  // if allocated by the OS, add the header size as it's part of the total memory block
  if h^.wasallocated then result := result + sizeof(txmemheader);

  if h^.wasallocated then begin
    // release memory back to the OS; size doesn't matter with MEM_RELEASE
    VirtualFree(h, 0, MEM_RELEASE);
  end else begin
    // mark chunk as free for reuse
    h^.size := 0;
  end;

  _xmemchunksend;
end;
function xfreemem(p: pointer): ptruint; stdcall;
begin
  Exit(xfreemem_(p));
end;
{$endif DLLEXPORT}
{$ifdef DLLIMPORT}
function xfreemem_(p: pointer): ptruint; stdcall; external RTLDLL;
function xfreemem (p: pointer): ptruint; stdcall;
begin
  Exit(xfreemem_(p));
end;
{$endif DLLIMPORT}

function xzeromem(p: pointer; len: ptruint): ptruint; stdcall; export;
begin       
  {$ifdef XMMDEBUG}
  writeln('call to xzeromem(', ptruint(p), ', ', len, ')');
  {$endif}
  result := xfillmem(p, len, 0);
end;

{$ifdef DLLEXPORT}
function xmovemem(const src: pointer; dst: pointer; len: ptruint): ptruint; stdcall; export;
begin
  {$ifdef XMMDEBUG}
  writeln('call to xmovemem(', ptruint(src), ', ', ptruint(dst), ', ', len, ')');
  {$endif}
  RtlMoveMemory(dst, src, len);
  result := len;
end;
{$endif DLLEXPORT}

function xfillmem_a(p: pointer; len: ptruint; v: byte): ptruint; stdcall; [public, alias: 'xfillmem_a']; export;
begin   
  {$ifdef XMMDEBUG}
  writeln('call to xfillmem:1(', ptruint(p), ', ', len, ', ', v, ')');
  {$endif}
  RtlFillMemory(p, len, v);
  result := len;
end;

function xfillmem_b(p: pointer; len: ptruint; v: char): ptruint; stdcall; [public, alias: 'xfillmem_b']; export;
begin    
  {$ifdef XMMDEBUG}
  writeln('call to xfillmem:2(', ptruint(p), ', ', len, ', ', v, ')');
  {$endif}
  RtlFillMemory(p, len, ord(v));
  result := len;
end;

function xmemdiffat(const p1, p2: pointer; len: ptruint): ptruint; stdcall; export;
begin       
  {$ifdef XMMDEBUG}
  writeln('call to xmemdiffat(', ptruint(p1), ', ', ptruint(p2), ', ', len, ')');
  {$endif}
  result := RtlCompareMemory(p1, p2, len);
end;

function xmemcompare(const p1, p2: pointer; len: ptruint): boolean; stdcall; export;
begin
  {$ifdef XMMDEBUG}
  writeln('call to xmemcompare(', ptruint(p1), ', ', ptruint(p2), ', ', len, ')');
  {$endif}
  result := RtlCompareMemory(p1, p2, len) = len;
end;

{$ifdef DLLEXPORT}
function xgetfreechunks_: integer; stdcall; export;
var
  i: integer;
begin 
  {$ifdef XMMDEBUG}
  writeln('call to xgetfreechunks()');
  {$endif}     
  result := 0;
  _xmemchunksbegin;
  for i := 0 to high(xmemchunks) do if xmemchunks[i].h.size = 0 then result := result + 1;
  _xmemchunksend;
end;
function xgetfreechunks: integer; stdcall;
begin
  Exit(xgetfreechunks_);
end;
{$endif DLLEXPORT}
{$ifdef DLLIMPORT}
function xgetfreechunks_: integer; stdcall; external RTLDLL;
function xgetfreechunks: integer; stdcall;
begin
  Exit(xgetfreechunks_);
end;
{$endif DLLIMPORT}

{$ifdef DLLEXPORT}
procedure xmminit_; stdcall; export;
var
  i: integer;
begin
  {$ifdef XMMDEBUG}
  writeln('call to xchunksinit()');
  {$endif}

  // exit if initialization has already been done
  if xmemchunks_init_done then exit;

  // enter critical section to prevent concurrent access
  _xmemchunksbegin;

  // zero out the memory chunk array
  RtlFillMemory(@xmemchunks[0], sizeof(xmemchunks), 0);

  for i := low(xmemchunks) to high(xmemchunks) do begin
    // link the chunks and set their real sizes
    if i <> low(xmemchunks)  then xmemchunks[i].h.prev := @xmemchunks[i-1]; // set previous chunk pointer
    if i <> high(xmemchunks) then xmemchunks[i].h.next := @xmemchunks[i+1]; // set next chunk pointer

    // set realsize
    xmemchunks[i].h.realsize := sizeof(xmemchunks[i].data);
  end;

  // mark initialization as complete
  xmemchunks_init_done := true;

  // exit critical section
  _xmemchunksend;
end;
procedure xmminit; stdcall;
begin
  xmminit_;
end;
{$endif DLLEXPORT}
{$ifdef DLLIMPORT}
procedure xmminit ; stdcall;
begin
  xmminit_;
end;
{$endif DLLIMPORT}

function xfillmem(p: pointer; len: ptruint; v: byte): ptruint; stdcall; begin result := xfillmem_a(p, len, v); end;
function xfillmem(p: pointer; len: ptruint; v: char): ptruint; stdcall; begin result := xfillmem_b(p, len, v); end;

{$ifdef DLLEXPORT}
exports
  xreallocmem       name 'xreallocmem',
  xgetmem           name 'xgetmem',
  xallocmem         name 'xallocmem',
  xclone            name 'xclone',
  xmemsize          name 'xmemsize',
  xmemrealsize      name 'xmemrealsize',
  xmemavailsize     name 'xmemavailsize',
  xfreemem          name 'xfreemem',
  xzeromem          name 'xzeromem',
  xmovemem          name 'xmovemem',
  xfillmem_a        name 'xfillmem_a',
  xfillmem_b        name 'xfillmem_b',
  xmemdiffat        name 'xmemdiffat',
  xmemcompare       name 'xmemcompare',
  xgetfreechunks    name 'xgetfreechunks',
  xmminit_          name 'xmminit_';
{$endif DLLEXPORT}
initialization
  // initialization: prepares memory chunk pool for allocations (zeros all)
  xmminit;

end.
