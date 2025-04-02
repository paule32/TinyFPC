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

type
  HWND    = NativeUInt;
  HANDLE  = NativeUInt;
  HMODULE = NativeUInt;
  lParam  = NativeInt;
  wParam  = NativeInt;
  LPCSTR  = PChar;
  PCSTR   = PChar;
  LPCWSTR = PWideChar;
  PVOID   = Pointer;
  LPVOID  = Pointer;
  WINBOOL = LongBool;
  BOOL    = WINBOOL;
  LPDWORD = PDWORD;
  
const
  MEM_COMMIT     = $00001000;
  MEM_RESERVE    = $00002000;
  MEM_RESET      = $00080000;
  MEM_RESET_UNDO = $1000000;
  
  PAGE_EXECUTE           = $10;
  PAGE_EXECUTE_READ      = $20;
  PAGE_EXECUTE_READWRITE = $40;
  PAGE_EXECUTE_WRITECOPY = $80;
  PAGE_NOACCESS          = $01;
  PAGE_READONLY          = $02;
  PAGE_READWRITE         = $04;
  PAGE_WRITECOPY         = $08;
  
  PAGE_TARGETS_INVALID   = $40000000;
  PAGE_TARGETS_NO_UPDATE = $40000000;
  MEM_DECOMMIT = $00004000;
  MEM_RELEASE  = $00008000;
