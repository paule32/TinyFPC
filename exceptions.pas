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

unit Exceptions;

{$mode ObjFPC}{$H+}

interface  

uses Windows;

function translate_windows_error(code: integer): string;

type
  Exception = class(TObject)
    Message: string;
    Code: integer;
    constructor Create(const msg: string; const errcode: integer);
  end;

  EDivByZero       = class(Exception) end;  
  EOutOfMemory     = class(Exception) end;
  EAccessViolation = class(Exception) end;
  // @@todo: add more

const
  exception_classes: array[200..236] of TClass = (
    { 0   DivByZero           RangeError         StackOverflow     OutOfMemory/HeapOverflow InvalidPointerOperation  }
    EDivByZero, nil, nil, EOutOfMemory, nil,
    { 5   Overflow            Underflow          InvalidOp         ZeroDivide               209                      }
    nil, nil, nil, nil, nil,
    { 10  ObjectCheckError    AbstractError      ExternalException 213                      BusError                 }
    nil, nil, nil, nil, nil,               
    { 15  IntOverflow         AccessViolation    ControlC          Privilege                InvalidCast              }
    nil, EAccessViolation, nil, nil, nil,  
    { 20  InvalidVarCast      InvalidVarOp       DispatchError     VarArrayCreate           VarNotArray              }
    nil, nil, nil, nil, nil, 
    { 25  VarArrayBounds      226                AssertionFailed   IntfCastError            SafecallException        }
    nil, nil, nil, nil, nil,    
    { 30  230                 iconvError         NoThreadSupport   SigQuit                  MissingWStringManager    }
    nil, nil, nil, nil, nil,    
    { 35  NoDynLibsSupport    ThreadError                                                                            }
    nil, nil
  );

function FormatMessageA(dwFlags: DWORD; lpSource: LPCVOID; dwMessageId: DWORD; dwLanguageId: DWORD;
  lpBuffer: LPSTR; nSize: DWORD; Arguments: Pointer): DWORD; stdcall; external 'kernel32.dll';
function FormatMessageW(dwFlags: DWORD; lpSource: LPCVOID; dwMessageId: DWORD; dwLanguageId: DWORD;
  lpBuffer: LPWSTR; nSize: DWORD; Arguments: Pointer): DWORD; stdcall; external 'kernel32.dll';

const
  FORMAT_MESSAGE_ALLOCATE_BUFFER = $00000100;
  FORMAT_MESSAGE_IGNORE_INSERTS  = $00000200;
  FORMAT_MESSAGE_FROM_STRING     = $00000400;
  FORMAT_MESSAGE_FROM_HMODULE    = $00000800;
  FORMAT_MESSAGE_FROM_SYSTEM     = $00001000;
  FORMAT_MESSAGE_ARGUMENT_ARRAY  = $00002000;

implementation

function translate_windows_error(code: integer): string;
var
  len: DWord;
  buf: PAnsiChar;
begin
  len := FormatMessageA(FORMAT_MESSAGE_FROM_SYSTEM or FORMAT_MESSAGE_IGNORE_INSERTS or FORMAT_MESSAGE_ALLOCATE_BUFFER,
    nil,
    code,
    MAKELANGID(0{LANG_NEUTRAL}, 1{SUBLANG_DEFAULT}),
    @buf,
    0,
    nil);
  writeln('len = ', len);
  //exceptions.pas(78,3) Error: Unknown compilerproc "fpc_write_text_pchar_as_pointer". Check if you use the correct run time library.
  //writeln('msg = ', buf);
  writeln('msg = ', string(buf));
  //MessageBoxA(0, buf, buf, 0);
  //ExitProcess(0);
  result := buf;
  //FillChar(buf^, len, 0);
  //exceptions.pas(79,21) Error: Unknown compilerproc "fpc_pchar_length". Check if you use the correct run time library.
  //setlength(result, length(buf));
  //setlength(result, len);
  //move(buf^, result[1], length(result));
  writeln('result = ', result);
  LocalFree(HLOCAL(buf));
end;

constructor Exception.Create(const msg: string; const errcode: integer);
begin
  Message := msg;
  Code := errcode;
end;

function default_ExceptObjProc(code: LongInt; const rec: EXCEPTION_RECORD): Pointer;
begin
  result := TTestException.Create('@@todo');
end;

function default_ExceptClsProc(code: LongInt): Pointer;
begin
  if (code >= low(exception_classes)) and (code <= high(exception_classes)) then result := exception_classes[code] else result := nil;
end;

procedure default_ErrorProc(code: Longint; addr: Pointer; frame: Pointer);
begin
  raise TTestException.Create('@@todo');
end;

initialization
  ExceptObjProc := @default_ExceptObjProc;
  ExceptClsProc := @default_ExceptClsProc;
  ErrorProc := @default_ErrorProc;

end.
