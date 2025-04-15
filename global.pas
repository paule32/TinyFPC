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
unit global;
interface
uses Windows;

// todo: versioning !
{$ifdef FPC}
const RTLLib_Version = 20250510;
{$endif}

const
  KERNEL32 = 'kenrel32.dll';
  RTLDLL   = 'rtllib.dll';

{$ifdef RELEASE}
function CommandLineToArgvA(CmdLine: PAnsiChar; var argc: Integer): PPAnsiChar; stdcall; external RTLDLL;
{$endif}

type
  TArray<T> = Array of T;

var
  QApplication_Counter: DWORD32;

type
  PDLLerror = ^TDLLerror;
  TDLLerror = record
    ErrorCode               : DWORD32;
    ErrorTextLength         : DWORD32;
    ErrorFromFunctionLength : DWORD32;
    ErrorFromLine           : DWORD32;
    ErrorText               : LPCSTR;
    ErrorTimeStamp          : Array [0..31] of Char;
    ErrorFromFunction       : LPCSTR;
  end;

type
  PDLLarguments = ^TDLLarguments;
  TDLLarguments = record
    ArgTypeName      : Array [0..64] of Char;
    ArgContentLength : DWORD32;
    ArgContent       : LPCSTR;
  end;
  
type
  PDLLargs = ^TDLLargs;
  TDLLargs = record
    ArgsCount : DWORD32;
    ArgsArray : Array of TDLLArguments;
  end;
  
type
  PDLLrequest = ^TDLLrequest;
  TDLLrequest = record
    Version : DWORD32;
    Error   : TDLLerror;
    Args    : TDLLargs;
  end;

implementation

end.
