{$if defined(codeh) and defined(codei)}
{$FATAL interface and implementation at the same time not possible}
{$endif}

{$ifdef codeh}

{$ifdef DLLEXPORT}
function  fpcshortstrtoansistr(const s2: ShortString; cp: TSystemCodePage): RawByteString; stdcall; export;
procedure fpcshortstrconcat(var dests: ShortString; const s1, s2: ShortString); stdcall; export;
function  fpcshortstrcopy(const s: ShortString; index: SizeInt; count: SizeInt): ShortString; stdcall; export;

procedure fpcansistrassign(var dests: pointer; s2: pointer); stdcall; export;
procedure fpcansistrsetlength(var S: rawbytestring; l: SizeInt; cp: TSystemCodePage); stdcall; export;
procedure fpcansistrdecrref(var s: pointer); stdcall; export;
function  fpcansistrcopy(const s: RawByteString; index, size: SizeInt): RawByteString; stdcall; export;

function  fpcpwidechartoansistr(const p: PWideChar; cp: TSystemCodePage): AnsiString; stdcall; export;
procedure fpcwidestrassign(var s1: pointer; s2: pointer); stdcall export;
function  fpcpwidechartowidestr(const p: PWideChar): WideString; stdcall; export;

function  fpctruelyansistrunique(var S: Pointer): Pointer; stdcall; export;
function  makeuniqueansistring(p: pointer): pointer; stdcall; export;

function fpcpchartoansistr(const p: PAnsiChar; cp: TSystemCodePage): RawByteString; stdcall; export;

function  itoa_exp(num: integer; base: integer=10): pansichar; stdcall; export;
function  atoi_exp(str: pansichar): integer; stdcall; export;

function newansistring(len: SizeInt): pointer; stdcall; export;
function newwidestring(len: SizeInt): pointer; stdcall; export;

{$endif DLLEXPORT}

// short
procedure fpc_shortstr_setlength(var s: ShortString; len: SizeInt); compilerproc;
function fpc_shortstr_to_ansistr(const s2: ShortString; cp: TSystemCodePage): RawByteString; stdcall; compilerproc;
procedure fpc_shortstr_to_shortstr(out res: ShortString; const sstr: ShortString); compilerproc;
function fpc_val_sint_shortstr(destsize: SizeInt; const s: ShortString; out code: ValSInt): ValSInt; compilerproc;
procedure fpc_shortstr_sint(v: ValSInt; len: SizeInt; out s: ShortString); compilerproc;
procedure fpc_ansistr_to_shortstr(out res: ShortString; const s2: RawByteString); compilerproc;
procedure fpc_shortstr_concat(var dests: ShortString; const s1, s2: ShortString); compilerproc;
function fpc_shortstr_copy(const s: ShortString; index: SizeInt; count: SizeInt): ShortString; compilerproc;

// AnsiString
procedure fpc_ansistr_assign(var dests: pointer; s2: pointer); compilerproc;
procedure fpc_ansistr_setlength(var s: RawByteString; l: SizeInt; cp: TSystemCodePage); compilerproc;
procedure fpc_ansistr_decr_ref(var s: pointer); compilerproc;
procedure fpc_ansistr_incr_ref(s: pointer); compilerproc;
procedure fpc_ansistr_concat(var dests: RawByteString; const s1, s2: RawByteString; cp: TSystemCodePage); compilerproc;
procedure fpc_ansistr_concat_multi(var dests: RawByteString; const sarr: array of RawByteString; cp: TSystemCodePage); compilerproc;
function fpc_ansistr_unique(var s: pointer): pointer; compilerproc;
procedure fpc_ansistr_sint(v: ValSInt; len: SizeInt; out s: RawByteString; cp: TSystemCodePage); compilerproc;
function fpc_char_to_ansistr(const c: AnsiChar; cp: TSystemCodePage): RawByteString; compilerproc;
function fpc_ansistr_compare_equal(const s1, s2: RawByteString): SizeInt; compilerproc;
function fpc_ansistr_copy(const s: RawByteString; index, size: SizeInt): RawByteString; compilerproc;

// WideString
procedure fpc_widestr_assign(var s1: pointer; s2: pointer); compilerproc;
procedure fpc_widestr_setlength(var s: WideString; l: sizeint); compilerproc;
procedure fpc_widestr_decr_ref(var s: pointer); compilerproc;
procedure fpc_widestr_incr_ref(var s: pointer); compilerproc;

// AnsiChar
procedure fpc_pchar_to_shortstr(out res: ShortString; p: PAnsiChar); compilerproc;
function fpc_pchar_to_ansistr(const p: PAnsiChar; cp: TSystemCodePage): RawByteString; compilerproc;

// WideChar
function fpc_pwidechar_to_widestr(const p: PWideChar): WideString; compilerproc;
function fpc_pwidechar_to_ansistr(const p: PWideChar; cp: TSystemCodePage): AnsiString; compilerproc;

// UnicodeChar
function fpc_uchar_to_ansistr(const c: UnicodeChar; cp: TSystemCodePage): AnsiString; compilerproc;

function strlen(p: pchar): dword;
// Error: Internal error 2013060101
//function strlen(s: shortstring): dword;
function strlen(s: ansistring): dword;

function new_ansistring(len: SizeInt): pointer; stdcall; export;
function new_widestring(len: SizeInt): pointer; stdcall; export;

function make_unique_ansistring(s: ansistring): ansistring;
function make_unique_ansistring(p: pointer): pointer;

// ansi to int
function atoi(str: pansichar): integer; stdcall;
// int to ansi
// for int64 fix: Error: Unknown compilerproc "fpc_mod_int64". Check if you use the correct run time library.
function itoa(num: integer; base: integer=10): pansichar; stdcall;

{$endif}

{$ifdef codei}

procedure fpc_shortstr_setlength(var s: ShortString; len: SizeInt); [public, alias: 'FPC_SHORTSTR_SETLENGTH']; compilerproc;
begin
  if len > 255 then len := 255;
  s[0] := chr(len);
end;

{$ifdef DLLEXPORT}
function fpcshortstrtoansistr(const s2: ShortString; cp: TSystemCodePage): RawByteString; stdcall; export;
var
  len: integer;
begin
  len := length(s2);
  setlength(result, len);
  move(s2[1], result[1], len);
end;
function fpc_shortstr_to_ansistr(const s2: ShortString; cp: TSystemCodePage): RawByteString; stdcall; compilerproc;
begin
  result := fpcshortstrtoansistr(s2,cp);
end;
{$endif DLLEXPORT}
{$ifdef DLLIMPORT}
function fpcshortstrtoansistr(const s2: ShortString; cp: TSystemCodePage): RawByteString; stdcall; external RTLDLL;
function fpc_shortstr_to_ansistr(const s2: ShortString; cp: TSystemCodePage): RawByteString; stdcall; compilerproc;
begin
  result := fpcshortstrtoansistr(s2,cp);
end;
{$endif DLLIMPORT}

procedure fpc_shortstr_to_shortstr(out res: ShortString; const sstr: ShortString); [public, alias: 'FPC_SHORTSTR_TO_SHORTSTR']; compilerproc;
var
  l: dword;
begin
  l := pbyte(@sstr[0])^;
  setlength(res, l);
  move(sstr[1], res[1], l);
end;

function fpc_val_sint_shortstr(destsize: SizeInt; const s: ShortString; out code: ValSInt): ValSInt; [public, alias: 'FPC_VAL_SINT_SHORTSTR']; compilerproc;
begin
  writeln('fpc_val_sint_shortstr');
end;

procedure fpc_shortstr_sint(v: ValSInt; len: SizeInt; out s: ShortString); [public, alias: 'FPC_SHORTSTR_SINT']; compilerproc;
begin
  writeln('fpc_shortstr_sint');
end;

procedure fpc_ansistr_to_shortstr(out res: ShortString; const s2: RawByteString); [public, alias: 'FPC_ANSISTR_TO_SHORTSTR']; compilerproc;
begin
  setlength(res, length(s2));
  move(s2[1], res[1], length(s2));
end;

{$ifdef DLLEXPORT}
procedure fpcshortstrconcat(var dests: ShortString; const s1, s2: ShortString); stdcall; export;
var
  len, cap, m: integer;
begin
  len := 0;
  cap := 255;
  // get first string
  move(s1[1], dests[1], length(s1));
  len := length(s1);
  // left capacity?
  dec(cap, length(s1));
  if cap > 0 then begin
    // get some (or whole) of another string
    m := length(s2);
    if m > cap then m := cap;
    move(s2[1], dests[length(s1)+1], m);
    inc(len, m);
  end;
  // and the new length is...
  dests[0] := chr(len);
end;
procedure fpc_shortstr_concat(var dests: ShortString; const s1, s2: ShortString); compilerproc;
begin
  fpcshortstrconcat(dests, s1, s2);
end;
{$endif DLLEXPORT}
{$ifdef DLLIMPORT}
procedure fpcshortstrconcat(var dests: ShortString; const s1, s2: ShortString); stdcall; external RTLDLL;
procedure fpc_shortstr_concat(var dests: ShortString; const s1, s2: ShortString); compilerproc;
begin
  fpcshortstrconcat(dests, s1, s2);
end;
{$endif DLLIMPORT}

{$ifdef DLLEXPORT}
function fpcshortstrcopy(const s: ShortString; index: SizeInt; count: SizeInt): ShortString; stdcall; export;
begin
  if count < 0 then count := 0;
  if index > 1 then dec(index) else index := 0;
  if index > length(s) then
    count := 0
  else if count > length(s)-index then
    count := length(s)-index;
  result[0] := chr(count);
  move(s[index+1], result[1], count);
end;
function fpc_shortstr_copy(const s: ShortString; index: SizeInt; count: SizeInt): ShortString; compilerproc;
begin
  result := fpcshortstrcopy(s, index, count);
end;
{$endif DLLEXPORT}
{$ifdef DLLIMPORT}
function fpcshortstrcopy(const s: ShortString; index: SizeInt; count: SizeInt): ShortString; stdcall; external RTLDLL;
function fpc_shortstr_copy(const s: ShortString; index: SizeInt; count: SizeInt): ShortString; compilerproc;
begin
  result := fpcshortstrcopy(s, index, count);
end;
{$endif DLLIMPORT}

{$ifdef DLLEXPORT}
procedure fpcansistrassign(var dests: pointer; s2: pointer); stdcall; export;
begin
  if dests = s2 then exit;
  if s2 <> nil then
    if PAnsiRec(s2-AnsiFirstOff)^.Ref > 0 then
      // @@todo locked
      inc(PAnsiRec(s2-AnsiFirstOff)^.Ref);
  // dec refcount of the old s1
  fpc_ansistr_decr_ref(dests);
  // point dests to s2
  dests := s2;
end;
procedure fpc_ansistr_assign(var dests: pointer; s2: pointer); [public, alias: 'FPC_ANSISTR_ASSIGN'];
begin
  fpcansistrassign(dests, s2);
end;
{$endif DLLEXPORT}
{$ifdef DLLIMPORT}
procedure fpcansistrassign(var dests: pointer; s2: pointer); stdcall; external RTLDLL;
procedure fpc_ansistr_assign(var dests: pointer; s2: pointer); [public, alias: 'FPC_ANSISTR_ASSIGN'];
begin
  fpcansistrassign(dests, s2);
end;
{$endif DLLIMPORT}

{$ifdef DLLEXPORT}
procedure fpcansistrsetlength(var S: rawbytestring; l: SizeInt; cp: TSystemCodePage); stdcall; export;
var
  Temp: Pointer;
  lens, lena: SizeInt;
begin
  if (l > 0) then begin
    if Pointer(S) = nil then begin
      Pointer(S) := new_ansistring(L);
      //cp:=TranslatePlaceholderCP(cp);
      cp := 0;
      PAnsiRec(Pointer(S) - AnsiFirstOff)^.CodePage := cp;
    end else if PAnsiRec(Pointer(S) - AnsiFirstOff)^.Ref = 1 then begin
      Temp := Pointer(s) - AnsiFirstOff;
          //lens:=MemSize(Temp);
      lens := 4;
      lena := AnsiFirstOff + L + sizeof(ansichar);
          { allow shrinking string if that saves at least half of current size }
      if (lena > lens) or ((lens > 32) and (lena <= SizeInt(SizeUint(lens) div 2))) then pointer(s) := xreallocmem(temp, 4) + AnsiFirstOff;
    end else begin
          { Reallocation is needed... }
      Temp := new_ansistring(4);
      PAnsiRec(Pointer(Temp) - AnsiFirstOff)^.CodePage := PAnsiRec(Pointer(S) - AnsiFirstOff)^.CodePage;
          { Also copy a trailing implicit #0 of the original string
            to the new larger string }
      lens := PAnsiRec(Pointer(S) - AnsiFirstOff)^.Len + 1;
      if l < lens then lens := l;
      Move(Pointer(S)^, Temp^, lens);
      fpc_ansistr_decr_ref(Pointer(s));
      Pointer(S) := Temp;
    end;
      { Force nil termination in case it gets shorter }
    pbyte(Pointer(S) + l)^ := 0;
    PAnsiRec(Pointer(S) - AnsiFirstOff)^.Len := l;
  end else  { length=0, deallocate the string }
    fpc_ansistr_decr_ref(Pointer(S));
end;
procedure fpc_ansistr_setlength(var S: rawbytestring; l: SizeInt; cp: TSystemCodePage); [public, alias: 'FPC_ANSISTR_SETLENGTH']; compilerproc;
begin
  fpcansistrsetlength(S, l, cp);
end;
{$endif DLLEXPORT}
{$ifdef DLLIMPORT}
procedure fpcansistrsetlength(var S: rawbytestring; l: SizeInt; cp: TSystemCodePage); stdcall; external RTLDLL;
procedure fpc_ansistr_setlength(var S: rawbytestring; l: SizeInt; cp: TSystemCodePage); [public, alias: 'FPC_ANSISTR_SETLENGTH']; compilerproc;
begin
  fpcansistrsetlength(S, l, cp);
end;
{$endif DLLIMPORT}

{$ifdef DLLEXPORT}
procedure fpcansistrdecrref(var s: pointer); stdcall; export;
var
  p: PAnsiRec;
begin
  if s = nil then exit;
  p := PAnsiRec(s-AnsiFirstOff);
  s := nil;
  if p^.ref < 0 then exit;
  //if declocked(p^.Ref) then FreeMem(p);
  //if InterlockedDecrement(p^.Ref) = 0 then FreeMem(p);
  // leave it here in case "lock" is bad again...
  dec(p^.Ref);
  if p^.Ref <= 0 then FreeMem(p);
end;
procedure fpc_ansistr_decr_ref(var s: pointer); [public , alias: 'FPC_ANSISTR_DECR_REF']; compilerproc;
begin
  fpcansistrdecrref(s);
end;
{$endif DLLEXPORT}
{$ifdef DLLIMPORT}
procedure fpcansistrdecrref(var s: pointer); stdcall; external RTLDLL;
procedure fpc_ansistr_decr_ref(var s: pointer); [public , alias: 'FPC_ANSISTR_DECR_REF']; compilerproc;
begin
  fpcansistrdecrref(s);
end;
{$endif DLLIMPORT}

// alias for internal use
procedure fpc_ansistr_decr_ref(var s: pointer); [external name 'FPC_ANSISTR_DECR_REF'];

procedure fpc_ansistr_incr_ref(s: pointer); [public, alias: 'FPC_ANSISTR_INCR_REF']; compilerproc;
begin
  if s = nil then exit;
  if PAnsiRec(s-AnsiFirstOff)^.Ref < 0 then exit;
  //InterlockedIncrement(PAnsiRec(s-AnsiFirstOff)^.Ref);
  inc(PAnsiRec(s-AnsiFirstOff)^.Ref);
  //inclocked(PAnsiRec(s-AnsiFirstOff)^.Ref);
end;

procedure fpc_ansistr_concat(var dests: RawByteString; const s1, s2: RawByteString; cp: TSystemCodePage); compilerproc;
begin
  pointer(dests) := new_ansistring(length(s1)+length(s2));
  move(s1[1], dests[1], length(s1));
  move(s2[1], dests[length(s1)+1], length(s2));
end;

procedure fpc_ansistr_concat_multi(var dests: RawByteString; const sarr: array of RawByteString; cp: TSystemCodePage); compilerproc;
begin
  writeln('fpc_ansistr_concat_multi');
end;

{$ifdef DLLEXPORT}
function fpctruelyansistrunique(var S: Pointer): Pointer; stdcall; export;
var
  SNew: Pointer;
  L: SizeInt;
begin
  L := PAnsiRec(Pointer(S) - AnsiFirstOff)^.len;
  SNew := new_ansistring(L);
  Move(Pointer(S)^, SNew^, L + 1);
  PAnsiRec(SNew - AnsiFirstOff)^.len := L;
  PAnsiRec(SNew - AnsiFirstOff)^.CodePage := PAnsiRec(Pointer(S) - AnsiFirstOff)^.CodePage;
  //fpc_ansistr_decr_ref(pointer(s));
  pointer(S) := SNew;
  pointer(result) := SNew;
end;
function fpc_truely_ansistr_unique(var S: Pointer): Pointer;
begin
  result := fpctruelyansistrunique(s);
end;
{$endif DLLEXPORT}
{$ifdef DLLIMPORT}
function fpctruelyansistrunique(var S: Pointer): Pointer; stdcall; external RTLDLL;
function fpc_truely_ansistr_unique(var S: Pointer): Pointer;
begin
  result := fpctruelyansistrunique(s);
end;
{$endif DLLIMPORT}

function fpc_ansistr_unique(var S: Pointer): Pointer; compilerproc; [public, alias: 'FPC_ANSISTR_UNIQUE'];
begin
  pointer(result) := pointer(s);
  if (result <> nil) and (PAnsiRec(result - AnsiFirstOff)^.Ref <> 1) then result := fpc_truely_ansistr_unique(s);
end;

procedure fpc_ansistr_sint(v: ValSInt; len: SizeInt; out s: RawByteString; cp: TSystemCodePage); [public, alias: 'FPC_ANSISTR_VALSINT']; compilerproc;
begin
  writeln('fpc_ansistr_sint');
end;

function fpc_char_to_ansistr(const c: AnsiChar; cp: TSystemCodePage): RawByteString; compilerproc;
begin
  pointer(result) := new_ansistring(1);
  move(c, result[1], 1);
end;

// 0 = equal
function fpc_ansistr_compare_equal(const s1, s2: RawByteString): SizeInt; compilerproc; [public, alias: 'FPC_ANSISTR_COMPARE_EQUAL'];
begin
  if pointer(s1) = pointer(s2) then exit(0);
  if (pointer(s1) = nil) or (pointer(s2) = nil) then exit(-1);
  if length(s1) <> length(s2) then exit(-1);
  result := CompareByte(s1[1], s2[1], length(s1));
end;

{$ifdef DLLEXPORT}
function fpcansistrcopy(const s: RawByteString; index, size: SizeInt): RawByteString; stdcall; export;
var
  ResultAddress : Pointer;
begin
  ResultAddress:=Nil;
  dec(index);
  if Index < 0 then
    Index := 0;
  { Check Size. Accounts for Zero-length S, the double check is needed because
    Size can be maxint and will get <0 when adding index }
  if (Size>Length(S)) or
     (Index+Size>Length(S)) then
   Size:=Length(S)-Index;
  If Size>0 then
   begin
     ResultAddress:=new_ansistring(size);
     if ResultAddress<>Nil then
      begin
        Move(Pointer(Pointer(S)+index)^,ResultAddress^,Size);
        PByte(ResultAddress+Size)^:=0;
        PAnsiRec(ResultAddress-AnsiFirstOff)^.Len:=Size;
        PAnsiRec(ResultAddress-AnsiFirstOff)^.CodePage:=PAnsiRec(Pointer(S)-AnsiFirstOff)^.CodePage;
      end;
   end;
   fpc_ansistr_decr_ref(Pointer(fpcansistrcopy));
  Pointer(fpcansistrCopy):=ResultAddress;
end;
//if count < 0 then count := 0;
//if index > 1 then dec(index) else index := 0;
//if index > length(s) then
//  count := 0
//else if count > length(s)-index then
//  count := length(s)-index;
//result[0] := chr(count);
//move(s[index+1], result[1], count);
function fpc_ansistr_copy(const s: RawByteString; index, size: SizeInt): RawByteString; compilerproc;
begin
  result := fpcansistrcopy(s, index, size);
end;
{$endif DLLEXPORT}
{$ifdef DLLIMPORT}
function fpcansistrcopy(const s: RawByteString; index, size: SizeInt): RawByteString; stdcall; external RTLDLL;
function fpc_ansistr_copy(const s: RawByteString; index, size: SizeInt): RawByteString; compilerproc;
begin
  result := fpcansistrcopy(s, index, size);
end;
{$endif DLLIMPORT}

{$ifdef DLLEXPORT}
procedure fpcwidestrassign(var s1: pointer; s2: pointer); stdcall export;
begin
  if s1 = s2 then exit;
  if s2 <> nil then begin
    setlength(widestring(s1), length(widestring(s2)));
    move(s2^, s1^, length(widestring(s1))+1*sizeof(WideChar));
  end else
    fpc_widestr_decr_ref(s1); // will set s1 to nil
end;
procedure fpc_widestr_assign(var s1: pointer; s2: pointer); [public, alias: 'FPC_WIDESTR_ASSIGN']; compilerproc;
begin
  fpcwidestrassign(s1, s2);
end;
{$endif DLLEXPORT}
{$ifdef DLLIMPORT}
procedure fpcwidestrassign(var s1: pointer; s2: pointer); stdcall external RTLDLL;
procedure fpc_widestr_assign(var s1: pointer; s2: pointer); [public, alias: 'FPC_WIDESTR_ASSIGN']; compilerproc;
begin
  fpcwidestrassign(s1, s2);
end;
{$endif DLLIMPORT}

procedure fpc_widestr_setlength(var s: WideString; l: sizeint); [public, alias: 'FPC_WIDESTR_SETLENGTH']; compilerproc;
begin
  pointer(s) := new_widestring(l);
end;

procedure fpc_widestr_decr_ref(var s: pointer); [public, alias: 'FPC_WIDESTR_DECR_REF']; compilerproc;
Begin
  if s = nil then exit;
  dec(s, WideFirstOff);
  xfreemem(s);
  s := nil;
end;

{ alias for internal use }
procedure fpc_widestr_decr_ref(var s: pointer); [external name 'FPC_WIDESTR_DECR_REF']; compilerproc;

procedure fpc_widestr_incr_ref(var s: pointer); [public, alias: 'FPC_WIDESTR_INCR_REF']; compilerproc;
var
  p : pointer;
Begin
  if s = nil then exit;
  p := new_widestring(length(WideString(s)));
  move(s^, p^, (length(WideString(s))+1)*sizeof(widechar));
  s := p;
end;

{ alias for internal use }
procedure fpc_widestr_incr_ref(var s: pointer); [external name 'FPC_WIDESTR_INCR_REF']; compilerproc;

{ ****************************************
  AnsiChar
  **************************************** }

procedure fpc_pchar_to_shortstr(out res: ShortString; p: PAnsiChar); [public, alias: 'FPC_PCHAR_TO_SHORTSTR']; compilerproc;
begin
  writeln('fpc_pchar_to_shortstr');
end;

{$ifdef DLLEXPORT}
function fpcpchartoansistr(const p: PAnsiChar; cp: TSystemCodePage): RawByteString; stdcall; export;
var
  l: dword;
begin
  l := strlen(p);
  pointer(result) := new_ansistring(l);
  move(p^, result[1], l);
end;
function fpc_pchar_to_ansistr(const p: PAnsiChar; cp: TSystemCodePage): RawByteString; compilerproc;
begin
  result := fpcpchartoansistr(p, cp);
end;
{$endif DLLEXPORT}
{$ifdef DLLIMPORT}
function fpcpchartoansistr(const p: PAnsiChar; cp: TSystemCodePage): RawByteString; stdcall; external RTLDLL;
function fpc_pchar_to_ansistr(const p: PAnsiChar; cp: TSystemCodePage): RawByteString; compilerproc;
begin
  result := fpcpchartoansistr(p, cp);
end;
{$endif DLLIMPORT}

{ ****************************************
  WideChar
  **************************************** }

{$ifdef DLLEXPORT}
function fpcpwidechartowidestr(const p: PWideChar): WideString; stdcall; export;
var
  len: SizeInt;
begin
  result := '';
  if p = nil then exit;
  len := IndexWord(p^, -1, 0);
  setlength(result, len);
  if len > 0 then move(p^, result[1], len*2);
end;
function fpc_pwidechar_to_widestr(const p: PWideChar): WideString; compilerproc;
begin
  result := fpcpwidechartowidestr(p);
end;
{$endif DLLEXPORT}
{$ifdef DLLIMPORT}
function fpcpwidechartowidestr(const p: PWideChar): WideString; stdcall; external RTLDLL;
function fpc_pwidechar_to_widestr(const p: PWideChar): WideString; compilerproc;
begin
  result := fpcpwidechartowidestr(p);
end;
{$endif DLLIMPORT}

{$ifdef DLLEXPORT}
function fpcpwidechartoansistr(const p: PWideChar; cp: TSystemCodePage): AnsiString; stdcall; export;
var
  len, i: SizeInt;
begin
  result := '';
  if p = nil then exit;
  len := IndexWord(p^, -1, 0);
  setlength(result, len);
  if len > 0 then begin
    for i := 0 to len-1 do begin
      // just take one byte
      result[1+i] := PAnsiChar(p+i)^;
    end;
  end;
end;
function fpc_pwidechar_to_ansistr(const p: PWideChar; cp: TSystemCodePage): AnsiString; compilerproc;
begin
  result := fpcpwidechartoansistr(p, cp);
end;
{$endif DLLEXPORT}
{$ifdef DLLIMPORT}
function fpcpwidechartoansistr(const p: PWideChar; cp: TSystemCodePage): AnsiString; stdcall; external RTLDLL;
function fpc_pwidechar_to_ansistr(const p: PWideChar; cp: TSystemCodePage): AnsiString; compilerproc;
begin
  result := fpcpwidechartoansistr(p, cp);
end;
{$endif DLLIMPORT}

{ ****************************************
  UnicodeChar
  **************************************** }

function fpc_uchar_to_ansistr(const c: UnicodeChar; cp: TSystemCodePage): AnsiString; compilerproc;
begin
  writeln('fpc_uchar_to_ansistr');
end;

{ ****************************************
  Others
  **************************************** }

function strlen(p: pchar): dword;
begin
  result := 0;
  if p = nil then exit;
  while (p+result)^ <> #0 do inc(result);
end;

// Error: Internal error 2013060101
//function strlen(s: shortstring): dword;
//begin
//  result := ord(s[0])
//  //result := pbyte(@s[0])^;
//end;

function strlen(s: ansistring): dword;
begin
  result := length(s);
end;

// init new string to length 0 and refcount 1
{$ifdef DLLEXPORT}
function newansistring(len: SizeInt): pointer; stdcall; export;
begin
  result := GetMem(len+(AnsiFirstOff+SizeOf(AnsiChar)));
  if result = nil then exit;
  PAnsiRec(result)^.len := len;
  PAnsiRec(result)^.ref := 1;
  PAnsiRec(result)^.codepage := DefaultSystemCodePage;
  PAnsiRec(result)^.elementsize := SizeOf(AnsiChar);
  inc(result, AnsiFirstOff);
  PAnsiChar(result)^ := #0; // null terminated
end;
function new_ansistring(len: SizeInt): pointer; stdcall; export;
begin
  result := newansistring(len);
end;
{$endif DLLEXPORT}
{$ifdef DLLIMPORT}
function newansistring(len: SizeInt): pointer; stdcall; external RTLDLL;
function new_ansistring(len: SizeInt): pointer; stdcall; export;
begin
  result := newansistring(len);
end;
{$endif DLLIMPORT}

// init new string to length 0 and refcount 1
{$ifdef DLLEXPORT}
function newwidestring(len: SizeInt): pointer; stdcall; export;
begin
  result := GetMem(len * sizeof(widechar) + WideRecLen);
  if result = nil then exit;
  PWideRec(result)^.Len := len * 2;
  PWideRec(result)^.First := #0;
  inc(result, WideFirstOff);
end;
function new_widestring(len: SizeInt): pointer; stdcall; export;
begin
  result := newwidestring(len);
end;
{$endif DLLEXPORT}
{$ifdef DLLIMPORT}
function newwidestring(len: SizeInt): pointer; stdcall; external RTLDLL;
function new_widestring(len: SizeInt): pointer; stdcall; export;
begin
  result := newwidestring(len);
end;
{$endif DLLIMPORT}

function make_unique_ansistring(s: ansistring): ansistring;
begin
  pointer(result) := new_ansistring(length(s));
  setlength(result, length(s));
  move(s[1], result[1], length(s));
end;

{$ifdef DLLEXPORT}
function makeuniqueansistring(p: pointer): pointer; stdcall; export;
var
  a: PAnsiRec;
  s: ansistring;
begin
  a := PAnsiRec(p-AnsiFirstOff);
  pointer(s) := new_ansistring(a^.len);
  move(a, s[1], a^.len);
  pointer(result) := pointer(a+AnsiFirstOff);
end;
function make_unique_ansistring(p: pointer): pointer;
begin
  result := makeuniqueansistring(p);
end;
{$endif DLLEXPORT}
{$ifdef DLLIMPORT}
function makeuniqueansistring(p: pointer): pointer; stdcall; external RTLDLL;
function make_unique_ansistring(p: pointer): pointer;
begin
  result := makeuniqueansistring(p);
end;
{$endif DLLIMPORT}

// ansi to int
{$ifdef DLLEXPORT}
function atoi_exp(str: pansichar): integer; stdcall export;
var
  i: integer;
  c: char;
begin
  result := 0;
  i := 0;
  while true do begin
    c := (str+i)^;
    if c = #0 then break;
    inc(i);
    result := result*10 + ord(c)-ord('0');
  end;
end;
function atoi(str: pansichar): integer; stdcall;
begin
  result := atoi_exp(str);
end;
{$endif DLLEXPORT}
{$ifdef DLLIMPORT}
function atoi_exp(str: pansichar): integer; stdcall; external RTLDLL;
function atoi(str: pansichar): integer; stdcall;
begin
  result := atoi_exp(str);
end;
{$endif DLLIMPORT}

// int to ansi
// could do int64 if: Error: Unknown compilerproc "fpc_mod_int64". Check if you use the correct run time library.
{$ifdef DLLEXPORT}
function itoa_exp(num: integer; base: integer=10): pansichar; stdcall; export;
procedure reverse(str: pansichar; len: integer);
var
  st, en: integer;
  tmp: ansichar;
begin
  st := 0;
  en := len-1;
  while st < en do begin
    tmp := str[st];
    str[st] := str[en];
    str[en] := tmp;
    inc(st);
    dec(en);
  end;
end;
var
  i, rem: integer;
  neg: boolean = false;
begin
  result := '0';
  // obvious 0
  if num = 0 then exit;
  // negative value
  if num < 0 then begin
    // handle only base 10 negative
    if base <> 10 then exit;
    neg := true;
    num := -num;
  end;
  // alloc mem, 32 bytes should be enough
  result := AllocMem(32);
  // do actual conversion
  i := 0;
  while num <> 0 do begin
    rem := num mod base;
    if rem > 9 then result[i] := chr((rem-10)+ord('a')) else result[i] := chr(rem+ord('0'));
    inc(i);
    num := num div base;
  end;
  // if negative
  if neg then begin
    result[i] := '-';
    inc(i);
  end;
  // null terminator
  result[i] := #0;
  // set length to only as much as needed
  ReAllocMem(result, 32);
  // reverse
  reverse(result, i);
end;
function itoa(num: integer; base: integer=10): pansichar; stdcall;
begin
  result := itoa_exp(num, base);
end;
{$endif DLLEXPORT}
{$ifdef DLLIMPORT}
function itoa_exp(num: integer; base: integer=10): pansichar; stdcall; external RTLDLL;
function itoa(num: integer; base: integer): pansichar; stdcall;
begin
  result := itoa_exp(num, base);
end;
{$endif DLLIMPORT}

{$endif}

