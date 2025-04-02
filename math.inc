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

//function  Lo(B: Byte):Byte;{$ifdef SYSTEMINLINE}inline;{$endif}
//function  Hi(b : Byte) : Byte;{$ifdef SYSTEMINLINE}inline;{$endif}
function  Lo(i : Integer) : byte;  [INTERNPROC: fpc_in_lo_Word];
function  Lo(w : Word) : byte;     [INTERNPROC: fpc_in_lo_Word];
//function  Lo(l : Longint) : Word;  [INTERNPROC: fpc_in_lo_long];
function  Lo(l : DWord) : Word;    [INTERNPROC: fpc_in_lo_long];
function  Lo(i : Int64) : DWord;   [INTERNPROC: fpc_in_lo_qword];
function  Lo(q : QWord) : DWord;   [INTERNPROC: fpc_in_lo_qword];
function  Hi(i : Integer) : byte;  [INTERNPROC: fpc_in_hi_Word];
function  Hi(w : Word) : byte;     [INTERNPROC: fpc_in_hi_Word];
//function  Hi(l : Longint) : Word;  [INTERNPROC: fpc_in_hi_long];
function  Hi(l : DWord) : Word;    [INTERNPROC: fpc_in_hi_long];
function  Hi(i : Int64) : DWord;   [INTERNPROC: fpc_in_hi_qword];
function  Hi(q : QWord) : DWord;   [INTERNPROC: fpc_in_hi_qword];

//function BsfByte(const avalue: byte): byte;{$ifdef SYSTEMINLINE}inline;{$endif}
function BsrByte(const avalue: byte): byte;{$ifdef SYSTEMINLINE}inline;{$endif}
//function BsfWord(const avalue: word): {$ifdef CPU16}byte{$else}cardinal{$endif};{$ifdef SYSTEMINLINE}inline;{$endif}
//function BsrWord(const avalue: word): {$ifdef CPU16}byte{$else}cardinal{$endif};{$ifdef SYSTEMINLINE}inline;{$endif}
//function BsfDWord(const avalue : dword): {$ifdef CPU16}byte{$else}cardinal{$endif};{$ifdef SYSTEMINLINE}inline;{$endif}
function BsrDWord(const avalue : dword): {$ifdef CPU16}byte{$else}cardinal{$endif};{$ifdef SYSTEMINLINE}inline;{$endif}
//function BsfQWord(const avalue : qword): {$ifdef CPU16}byte{$else}cardinal{$endif};{$ifdef SYSTEMINLINE}inline;{$endif}
function BsrQWord(const avalue: qword): cardinal; {$ifdef SYSTEMINLINE}inline;{$endif}

//function Pi: ValReal; [internproc: fpc_in_pi_real];
//function Abs(d: ValReal): ValReal; [internproc: fpc_in_abs_real];
//function Sqr(d: ValReal): ValReal; [internproc: fpc_in_sqr_real];
//function Sqrt(d: ValReal): ValReal; [internproc: fpc_in_sqrt_real];
//function ArcTan(d: ValReal): ValReal; [internproc: fpc_in_arctan_real];
//function Ln(d: ValReal): ValReal; [internproc: fpc_in_ln_real];
//function Sin(d: ValReal): ValReal; [internproc: fpc_in_sin_real];
//function Cos(d: ValReal): ValReal; [internproc: fpc_in_cos_real];
//function Exp(d: ValReal): ValReal; [internproc: fpc_in_exp_real];
//function Round(d: ValReal): int64; [internproc: fpc_in_round_real];
//function Frac(d: ValReal): ValReal; [internproc: fpc_in_frac_real];
//function Int(d: ValReal): ValReal; [internproc: fpc_in_int_real];
//function Trunc(d: ValReal): int64; [internproc: fpc_in_trunc_real];

function Abs(l: LongInt): LongInt; [internproc: fpc_in_abs_long];

