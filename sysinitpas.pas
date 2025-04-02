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

unit sysinitpas;

{$mode ObjFPC}{$H+}

interface

const
  DLL_PROCESS_DETACH = 0;
  DLL_PROCESS_ATTACH = 1;
  DLL_THREAD_ATTACH  = 2;
  DLL_THREAD_DETACH  = 3;

type
  TEntryInformation = record
    InitFinalTable: Pointer;
    ThreadvarTablesTable: Pointer;
    ResourceStringTables: Pointer;
    ResStrInitTables: Pointer;
    ResLocation: Pointer;
    PascalMain: procedure;
    valgrind_used: boolean;
  end;

procedure PASCALMAIN; external name 'PASCALMAIN';
procedure fpc_do_exit; external name 'FPC_DO_EXIT';

var
  SysInstance: LongInt;
  DLLReason: DWord;
  DLLParam: PtrInt;
  ThreadvarTablesTable : record end; external name 'FPC_THREADVARTABLES';

implementation

const
  SysInitEntryInformation: TEntryInformation = (
    InitFinalTable:       nil;//@InitFinalTable;
    ThreadvarTablesTable: nil;//@ThreadvarTablesTable;
    ResourceStringTables: nil;//@ResourceStringTables;
    ResStrInitTables:     nil;//@ResStrInitTables;
    ResLocation:          nil;
    PascalMain:           @PASCALMAIN;
    valgrind_used:        false;
  );

// for internal use
// its here temporary I guess
procedure fpc_finalizeunits; [external name 'FPC_FINALIZEUNITS'];

function DLL_Entry(constref info: TEntryInformation): longbool; [public, alias: '_FPC_DLL_Entry'];
begin
  result := true;

  case DLLReason of
    DLL_PROCESS_DETACH: begin
      // release resources etc
      // and finalize units
      fpc_finalizeunits;
      // do not call internal_do_exit
    end;
    DLL_PROCESS_ATTACH: begin   
      // the result is ignored except on DLL_PROCESS_ATTACH
      // use try..except and return false on exception
      PASCALMAIN;
    end;
    DLL_THREAD_ATTACH: { @@todo };
    DLL_THREAD_DETACH: { @@todo };
  end;
end;

// Console APP
procedure _FPC_mainCRTStartup; stdcall; public name '_mainCRTStartup';
const
  ENABLE_ECHO_INPUT = $0004;
  ENABLE_LINE_INPUT = $0002;
begin
  is_console := true;

  system.StdIn := GetStdHandle(STD_INPUT_HANDLE);
  system.StdOut := GetStdHandle(STD_OUTPUT_HANDLE);
  system.StdErr := GetStdHandle(STD_ERROR_HANDLE);

  // initialize console; enables readln
  SetConsoleMode(StdIn, ENABLE_ECHO_INPUT or ENABLE_LINE_INPUT);

  PASCALMAIN;
	fpc_do_exit;
end;

// GUI APP
procedure _WinMainCRTStartup; stdcall; public name '_WinMainCRTStartup';
begin
  PASCALMAIN;
	fpc_do_exit;
end;

// DLL console
procedure _FPC_DLLMainCRTStartup(_hinstance: longint; _dllreason: dword; _dllparam: pointer); stdcall; public name '_DLLMainCRTStartup';
begin
  is_console := true;
  SysInstance := _hinstance;
  DLLReason := _dllreason;
  DLLParam := PtrUInt(_dllparam);

  system.StdIn := GetStdHandle(STD_INPUT_HANDLE);
  system.StdOut := GetStdHandle(STD_OUTPUT_HANDLE);
  system.StdErr := GetStdHandle(STD_ERROR_HANDLE);

  DLL_Entry(SysInitEntryInformation);
end;

// DLL non-cosole
procedure _FPC_DLLWinMainCRTStartup(_hinstance: longint; _dllreason: dword; _dllparam: pointer); stdcall; public name '_DLLWinMainCRTStartup';
begin
  SysInstance := _hinstance;
  DLLReason := _dllreason;
  DLLParam := PtrUInt(_dllparam);

  DLL_Entry(SysInitEntryInformation);
end;

end.
