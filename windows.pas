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

unit Windows;

{$mode ObjFPC}{$H+}

interface

type
  PHANDLE = ^HANDLE;
  LONG = Longint;
  PLONG = ^LONG;
  HFILE = THandle;
  LPCVOID = Pointer;
  PLARGE_INTEGER = ^LARGE_INTEGER;
  LPSTR = PAnsiChar;
  LPWSTR = PWideChar;
  LPBYTE = ^Byte;
  LONG_PTR = PtrInt;
  UINT_PTR = PtrUInt;
  VOID = Pointer;
  HMENU = THandle;
  HINSTANCE = THandle;
  HLOCAL = THandle;

  LPSECURITY_ATTRIBUTES = ^SECURITY_ATTRIBUTES;
  SECURITY_ATTRIBUTES = record
    nLength: DWORD;
    lpSecurityDescriptor: Pointer;
    bInheritHandle: BOOL;
  end;
             
  LPOVERLAPPED = ^OVERLAPPED;
  OVERLAPPED = record
    Internal: ULONG_PTR;
    InternalHigh: ULONG_PTR;
    Offset: DWORD;
    OffsetHigh: DWORD;
    hEvent: THandle;
  end;

  POINT = record
    x: LONG;
    y: LONG;
  end;
       
  LPMSG = ^MSG;
  MSG = record
    hwnd: HWND;
    message: UINT;
    wParam: WPARAM;
    lParam: LPARAM;
    time: DWORD;
    pt: POINT;
  end;

  TIMERPROC = procedure(hWnd: HWND; uMsg: UINT; idEvent: UINT_PTR; dwTime: DWORD); stdcall;

  STARTUPINFOA = record
    cb: DWORD;
    lpReserved: LPSTR;
    lpDesktop: LPSTR;
    lpTitle: LPSTR;
    dwX: DWORD;
    dwY: DWORD;
    dwXSize: DWORD;
    dwYSize: DWORD;
    dwXCountChars: DWORD;
    dwYCountChars: DWORD;
    dwFillAttribute: DWORD;
    dwFlags: DWORD;
    wShowWindow: WORD;
    cbReserved2: WORD;
    lpReserved2: LPBYTE;
    hStdInput: HANDLE;
    hStdOutput: HANDLE;
    hStdError: HANDLE;
  end;

  STARTUPINFOW = record
    cb: DWORD;
    lpReserved: LPWSTR;
    lpDesktop: LPWSTR;
    lpTitle: LPWSTR;
    dwX: DWORD;
    dwY: DWORD;
    dwXSize: DWORD;
    dwYSize: DWORD;
    dwXCountChars: DWORD;
    dwYCountChars: DWORD;
    dwFillAttribute: DWORD;
    dwFlags: DWORD;
    wShowWindow: WORD;
    cbReserved2: WORD;
    lpReserved2: LPBYTE;
    hStdInput: HANDLE;
    hStdOutput: HANDLE;
    hStdError: HANDLE;
  end;

  PPROCESS_INFORMATION = ^PROCESS_INFORMATION;
  PROCESS_INFORMATION = record
    hProcess: HANDLE;
    hThread: HANDLE;
    dwProcessId: DWORD;
    dwThreadId: DWORD;
  end;

  LPTHREAD_START_ROUTINE = function(lpParameter: LPVOID): DWORD; stdcall;
  LPVOID = Pointer;

  LRESULT = LONG_PTR;
  RECT = record
    left: LONG;
    top: LONG;
    right: LONG;
    bottom: LONG;
  end;

  HGLOBAL = THandle;
  LARGE_INTEGER = record
    case Integer of
      0: (LowPart: DWORD; HighPart: LONG);
      1: (QuadPart: Int64);
  end;

  PRTL_CRITICAL_SECTION = ^RTL_CRITICAL_SECTION;
  RTL_CRITICAL_SECTION = record
    DebugInfo: Pointer;
    LockCount: LongInt;
    RecursionCount: LongInt;
    OwningThread: THandle;
    LockSemaphore: THandle;
    SpinCount: ULONG_PTR;
  end;

  PSYSTEMTIME = ^SYSTEMTIME;
  SYSTEMTIME = record
    wYear: WORD;
    wMonth: WORD;
    wDayOfWeek: WORD;
    wDay: WORD;
    wHour: WORD;
    wMinute: WORD;
    wSecond: WORD;
    wMilliseconds: WORD;
  end;

  PFILETIME = ^FILETIME;
  FILETIME = record
    dwLowDateTime: DWORD;
    dwHighDateTime: DWORD;
  end;

const
  INVALID_HANDLE_VALUE    = THandle(-1);
  INVALID_FILE_ATTRIBUTES = DWORD(-1);

  // Constants for File Access
  GENERIC_READ = $80000000;
  GENERIC_WRITE = $40000000;
  GENERIC_EXECUTE = $20000000;
  GENERIC_ALL = $10000000;

  // Constants for File Share Mode
  FILE_SHARE_READ = $00000001;
  FILE_SHARE_WRITE = $00000002;
  FILE_SHARE_DELETE = $00000004;

  // Constants for Creation Disposition
  CREATE_NEW = 1;
  CREATE_ALWAYS = 2;
  OPEN_EXISTING = 3;
  OPEN_ALWAYS = 4;
  TRUNCATE_EXISTING = 5;

  // Constants for File Attributes
  FILE_ATTRIBUTE_READONLY = $00000001;
  FILE_ATTRIBUTE_HIDDEN = $00000002;
  FILE_ATTRIBUTE_SYSTEM = $00000004;
  FILE_ATTRIBUTE_DIRECTORY = $00000010;
  FILE_ATTRIBUTE_ARCHIVE = $00000020;
  FILE_ATTRIBUTE_NORMAL = $00000080;
  FILE_ATTRIBUTE_TEMPORARY = $00000100;

  FILE_BEGIN   = 0;
  FILE_CURRENT = 1;
  FILE_END     = 2;

  // Wait Constants
  WAIT_OBJECT_0 = $00000000;
  WAIT_ABANDONED = $00000080;
  WAIT_TIMEOUT = $00000102;
  WAIT_FAILED = DWORD(-1);

  // ShowWindow Constants
  SW_HIDE = 0;
  SW_SHOWNORMAL = 1;
  SW_SHOWMINIMIZED = 2;
  SW_SHOWMAXIMIZED = 3;
  SW_SHOWNOACTIVATE = 4;
  SW_SHOW = 5;
  SW_MINIMIZE = 6;
  SW_SHOWMINNOACTIVE = 7;
  SW_SHOWNA = 8;
  SW_RESTORE = 9;
  SW_SHOWDEFAULT = 10;
  SW_FORCEMINIMIZE = 11;   const

  // Button definitions
  MB_OK = $00000000;
  MB_OKCANCEL = $00000001;
  MB_ABORTRETRYIGNORE = $00000002;
  MB_YESNOCANCEL = $00000003;
  MB_YESNO = $00000004;
  MB_RETRYCANCEL = $00000005;

  // Icon definitions
  MB_ICONHAND = $00000010;
  MB_ICONQUESTION = $00000020;
  MB_ICONEXCLAMATION = $00000030;
  MB_ICONASTERISK = $00000040;

  // Additional combinations
  MB_ICONERROR = MB_ICONHAND;
  MB_ICONWARNING = MB_ICONEXCLAMATION;
  MB_ICONINFORMATION = MB_ICONASTERISK;

  // Allocation types
  MEM_COMMIT     = $00001000;
  MEM_RESERVE    = $00002000;
  MEM_DECOMMIT   = $00004000;
  MEM_RELEASE    = $00008000;
  MEM_RESET      = $00080000;
  MEM_TOP_DOWN   = $00100000;
  MEM_WRITE_WATCH = $00200000;
  MEM_PHYSICAL   = $00400000;

  // Memory protection options
  PAGE_NOACCESS          = $01;
  PAGE_READONLY          = $02;
  PAGE_READWRITE         = $04;
  PAGE_WRITECOPY         = $08;
  PAGE_EXECUTE           = $10;
  PAGE_EXECUTE_READ      = $20;
  PAGE_EXECUTE_READWRITE = $40;
  PAGE_EXECUTE_WRITECOPY = $80;
  PAGE_GUARD             = $100;
  PAGE_NOCACHE           = $200;
  PAGE_WRITECOMBINE      = $400;

procedure Sleep(dwMilliseconds: DWORD); stdcall; external 'kernel32.dll';
function GetTickCount: DWORD; stdcall; external 'kernel32.dll';
function Beep(dwFreq, dwDuration: DWORD): BOOL; stdcall; external 'kernel32.dll';
function MessageBoxA(hWnd: HWND; lpText, lpCaption: LPCSTR; uType: UINT): Integer; stdcall; external 'user32.dll' name 'MessageBoxA';
function MessageBoxW(hWnd: HWND; lpText, lpCaption: LPCWSTR; uType: UINT): Integer; stdcall; external 'user32.dll' name 'MessageBoxW';

function CreateMutexA(lpMutexAttributes: LPSECURITY_ATTRIBUTES; bInitialOwner: BOOL; lpName: LPCSTR): HANDLE; stdcall; external 'kernel32.dll' name 'CreateMutexA';
function CreateMutexW(lpMutexAttributes: LPSECURITY_ATTRIBUTES; bInitialOwner: BOOL; lpName: LPCWSTR): HANDLE; stdcall; external 'kernel32.dll' name 'CreateMutexW';
function OpenMutexA(dwDesiredAccess: DWORD; bInheritHandle: BOOL; lpName: LPCSTR): HANDLE; stdcall; external 'kernel32.dll' name 'OpenMutexA';
function OpenMutexW(dwDesiredAccess: DWORD; bInheritHandle: BOOL; lpName: LPCWSTR): HANDLE; stdcall; external 'kernel32.dll' name 'OpenMutexW';
function ReleaseMutex(hMutex: HANDLE): BOOL; stdcall; external 'kernel32.dll';
function WaitForSingleObject(hHandle: HANDLE; dwMilliseconds: DWORD): DWORD; stdcall; external 'kernel32.dll';
function WaitForMultipleObjects(nCount: DWORD; lpHandles: PHANDLE; bWaitAll: BOOL; dwMilliseconds: DWORD): DWORD; stdcall; external 'kernel32.dll';
function CloseHandle(hObject: HANDLE): BOOL; stdcall; external 'kernel32.dll';

function CreateFileA(lpFileName: LPCSTR; dwDesiredAccess, dwShareMode: DWORD;
  lpSecurityAttributes: LPSECURITY_ATTRIBUTES; dwCreationDisposition, dwFlagsAndAttributes: DWORD;
  hTemplateFile: HANDLE): HANDLE; stdcall; external 'kernel32.dll' name 'CreateFileA';
function CreateFileW(lpFileName: LPCWSTR; dwDesiredAccess, dwShareMode: DWORD;
  lpSecurityAttributes: LPSECURITY_ATTRIBUTES; dwCreationDisposition, dwFlagsAndAttributes: DWORD;
  hTemplateFile: HANDLE): HANDLE; stdcall; external 'kernel32.dll' name 'CreateFileW';
function WriteFile(hFile: HANDLE; lpBuffer: LPCVOID; nNumberOfBytesToWrite: DWORD;
  lpNumberOfBytesWritten: PDWORD; lpOverlapped_: LPOVERLAPPED): BOOL; stdcall; external 'kernel32.dll';
function ReadFile(hFile: HANDLE; lpBuffer: LPVOID; nNumberOfBytesToRead: DWORD;
  lpNumberOfBytesRead: PDWORD; lpOverlapped_: LPOVERLAPPED): BOOL; stdcall; external 'kernel32.dll';
function DeleteFileA(lpFileName: LPCSTR): BOOL; stdcall; external 'kernel32.dll' name 'DeleteFileA';
function DeleteFileW(lpFileName: LPCWSTR): BOOL; stdcall; external 'kernel32.dll' name 'DeleteFileW';
function SetFilePointer(hFile: HANDLE; lDistanceToMove: LONG;
  lpDistanceToMoveHigh: PLONG; dwMoveMethod: DWORD): DWORD; stdcall; external 'kernel32.dll';
function _lopen(lpPathName: LPCSTR; iReadWrite: Integer): HFILE; stdcall; external 'kernel32.dll' name '_lopen';
function _lread(hFile: HFILE; lpBuffer: LPVOID; uBytes: UINT): UINT; stdcall; external 'kernel32.dll' name '_lread';
function _lwrite(hFile: HFILE; lpBuffer: LPCVOID; uBytes: UINT): UINT; stdcall; external 'kernel32.dll' name '_lwrite';
function _lclose(hFile: HFILE): HFILE; stdcall; external 'kernel32.dll' name '_lclose';
function GetFileSize(hFile: HANDLE; lpFileSizeHigh: LPDWORD): DWORD; stdcall; external 'kernel32.dll';
function GetFileSizeEx(hFile: HANDLE; lpFileSize: PInt64): BOOL; stdcall; external 'kernel32.dll';

function CreateProcessA(lpApplicationName: LPCSTR; lpCommandLine: LPSTR; lpProcessAttributes, lpThreadAttributes: LPSECURITY_ATTRIBUTES;
  bInheritHandles: BOOL; dwCreationFlags: DWORD; lpEnvironment: LPVOID; lpCurrentDirectory: LPCSTR;
  const lpStartupInfo: STARTUPINFOA; lpProcessInformation: PPROCESS_INFORMATION): BOOL; stdcall; external 'kernel32.dll' name 'CreateProcessA';
function CreateProcessW(lpApplicationName: LPCWSTR; lpCommandLine: LPWSTR; lpProcessAttributes, lpThreadAttributes: LPSECURITY_ATTRIBUTES;
  bInheritHandles: BOOL; dwCreationFlags: DWORD; lpEnvironment: LPVOID; lpCurrentDirectory: LPCWSTR;
  const lpStartupInfo: STARTUPINFOW; var lpProcessInformation: PROCESS_INFORMATION): BOOL; stdcall; external 'kernel32.dll' name 'CreateProcessW';

function TerminateProcess(hProcess: HANDLE; uExitCode: UINT): BOOL; stdcall; external 'kernel32.dll';
function ExitProcess(uExitCode: UINT): VOID; stdcall; external 'kernel32.dll';
function GetCurrentProcess: HANDLE; stdcall; external 'kernel32.dll';
function OpenProcess(dwDesiredAccess: DWORD; bInheritHandle: BOOL; dwProcessId: DWORD): HANDLE; stdcall; external 'kernel32.dll';
function CreateThread(lpThreadAttributes: LPSECURITY_ATTRIBUTES; dwStackSize: SIZE_T; lpStartAddress: LPTHREAD_START_ROUTINE;
  lpParameter: LPVOID; dwCreationFlags: DWORD; var lpThreadId: DWORD): HANDLE; stdcall; external 'kernel32.dll';
function TerminateThread(hThread: HANDLE; dwExitCode: DWORD): BOOL; stdcall; external 'kernel32.dll';
function ExitThread(dwExitCode: DWORD): VOID; stdcall; external 'kernel32.dll';
function GetCurrentThread: HANDLE; stdcall; external 'kernel32.dll';
function SuspendThread(hThread: HANDLE): DWORD; stdcall; external 'kernel32.dll';
function ResumeThread(hThread: HANDLE): DWORD; stdcall; external 'kernel32.dll';

function CreateWindowExA(dwExStyle: DWORD; lpClassName, lpWindowName: LPCSTR; dwStyle: DWORD;
  X, Y, nWidth, nHeight: Integer; hWndParent: HWND; hMenu_: HMENU;
  hInstance_: HINSTANCE; lpParam: LPVOID): HWND; stdcall; external 'user32.dll' name 'CreateWindowExA';
function CreateWindowExW(dwExStyle: DWORD; lpClassName, lpWindowName: LPCWSTR; dwStyle: DWORD;
  X, Y, nWidth, nHeight: Integer; hWndParent: HWND; hMenu_: HMENU;
  hInstance_: HINSTANCE; lpParam: LPVOID): HWND; stdcall; external 'user32.dll' name 'CreateWindowExW';
function DestroyWindow(hWnd: HWND): BOOL; stdcall; external 'user32.dll';
function ShowWindow(hWnd: HWND; nCmdShow: Integer): BOOL; stdcall; external 'user32.dll';
function UpdateWindow(hWnd: HWND): BOOL; stdcall; external 'user32.dll';
function SetWindowTextA(hWnd: HWND; lpString: LPCSTR): BOOL; stdcall; external 'user32.dll' name 'SetWindowTextA';
function SetWindowTextW(hWnd: HWND; lpString: LPCWSTR): BOOL; stdcall; external 'user32.dll' name 'SetWindowTextW';
function GetWindowTextA(hWnd: HWND; lpString: LPSTR; nMaxCount: Integer): Integer; stdcall; external 'user32.dll' name 'GetWindowTextA';
function GetWindowTextW(hWnd: HWND; lpString: LPWSTR; nMaxCount: Integer): Integer; stdcall; external 'user32.dll' name 'GetWindowTextW';
function CloseWindow(hWnd: HWND): BOOL; stdcall; external 'user32.dll';
function PostQuitMessage(nExitCode: Integer): VOID; stdcall; external 'user32.dll';
function DefWindowProcA(hWnd: HWND; Msg: UINT; wParam: WPARAM; lParam: LPARAM): LRESULT; stdcall; external 'user32.dll' name 'DefWindowProcA';
function DefWindowProcW(hWnd: HWND; Msg: UINT; wParam: WPARAM; lParam: LPARAM): LRESULT; stdcall; external 'user32.dll' name 'DefWindowProcW';
function GetMessageA(lpMsg_: LPMSG; hWnd: HWND; wMsgFilterMin, wMsgFilterMax: UINT): BOOL; stdcall; external 'user32.dll' name 'GetMessageA';
function GetMessageW(lpMsg_: LPMSG; hWnd: HWND; wMsgFilterMin, wMsgFilterMax: UINT): BOOL; stdcall; external 'user32.dll' name 'GetMessageW';
function DispatchMessageA(const lpMsg: MSG): LRESULT; stdcall; external 'user32.dll' name 'DispatchMessageA';
function DispatchMessageW(const lpMsg: MSG): LRESULT; stdcall; external 'user32.dll' name 'DispatchMessageW';
function TranslateMessage(const lpMsg: MSG): BOOL; stdcall; external 'user32.dll';
function SendMessageA(hWnd: HWND; Msg: UINT; wParam: WPARAM; lParam: LPARAM): LRESULT; stdcall; external 'user32.dll' name 'SendMessageA';
function SendMessageW(hWnd: HWND; Msg: UINT; wParam: WPARAM; lParam: LPARAM): LRESULT; stdcall; external 'user32.dll' name 'SendMessageW';
function PostMessageA(hWnd: HWND; Msg: UINT; wParam: WPARAM; lParam: LPARAM): BOOL; stdcall; external 'user32.dll' name 'PostMessageA';
function PostMessageW(hWnd: HWND; Msg: UINT; wParam: WPARAM; lParam: LPARAM): BOOL; stdcall; external 'user32.dll' name 'PostMessageW';
function GetWindowRect(hWnd: HWND; var lpRect: RECT): BOOL; stdcall; external 'user32.dll';
function GetClientRect(hWnd: HWND; var lpRect: RECT): BOOL; stdcall; external 'user32.dll';
function MoveWindow(hWnd: HWND; X, Y, nWidth, nHeight: Integer; bRepaint: BOOL): BOOL; stdcall; external 'user32.dll';

function GlobalAlloc(uFlags: UINT; dwBytes: SIZE_T): HGLOBAL; stdcall; external 'kernel32.dll';
function GlobalFree(hMem: HGLOBAL): HGLOBAL; stdcall; external 'kernel32.dll';
function GlobalLock(hMem: HGLOBAL): LPVOID; stdcall; external 'kernel32.dll';
function GlobalUnlock(hMem: HGLOBAL): BOOL; stdcall; external 'kernel32.dll';

function SetWindowPos(hWnd: HWND; hWndInsertAfter: HWND; X, Y, cx, cy: Integer; uFlags: UINT): BOOL; stdcall; external 'user32.dll';
function GetForegroundWindow: HWND; stdcall; external 'user32.dll';

function CreateEventA(lpEventAttributes: LPSECURITY_ATTRIBUTES; bManualReset, bInitialState: BOOL; lpName: LPCSTR): HANDLE; stdcall; external 'kernel32.dll' name 'CreateEventA';
function CreateEventW(lpEventAttributes: LPSECURITY_ATTRIBUTES; bManualReset, bInitialState: BOOL; lpName: LPCWSTR): HANDLE; stdcall; external 'kernel32.dll' name 'CreateEventW';
function SetEvent(hEvent: HANDLE): BOOL; stdcall; external 'kernel32.dll';
function ResetEvent(hEvent: HANDLE): BOOL; stdcall; external 'kernel32.dll';

function SetTimer(hWnd: HWND; nIDEvent: UINT_PTR; uElapse: UINT; lpTimerFunc: TIMERPROC): UINT_PTR; stdcall; external 'user32.dll';
function KillTimer(hWnd: HWND; uIDEvent: UINT_PTR): BOOL; stdcall; external 'user32.dll';

function CreateDirectoryA(lpPathName: LPCSTR; lpSecurityAttributes: LPSECURITY_ATTRIBUTES): BOOL; stdcall; external 'kernel32.dll' name 'CreateDirectoryA';
function CreateDirectoryW(lpPathName: LPCWSTR; lpSecurityAttributes: LPSECURITY_ATTRIBUTES): BOOL; stdcall; external 'kernel32.dll' name 'CreateDirectoryW';
function RemoveDirectoryA(lpPathName: LPCSTR): BOOL; stdcall; external 'kernel32.dll' name 'RemoveDirectoryA';
function RemoveDirectoryW(lpPathName: LPCWSTR): BOOL; stdcall; external 'kernel32.dll' name 'RemoveDirectoryW';

function InterlockedIncrement(Addend: PLongInt): LongInt; stdcall; external 'kernel32.dll';
function InterlockedDecrement(Addend: PLongInt): LongInt; stdcall; external 'kernel32.dll';
function InterlockedExchange(Target: PLongInt; Value: LongInt): LongInt; stdcall; external 'kernel32.dll';
function InterlockedExchangeAdd(Addend: PLongInt; Value: LongInt): LongInt; stdcall; external 'kernel32.dll';
function InterlockedCompareExchange(Destination: PLongInt; Exchange: LongInt; Comperand: LongInt): LongInt; stdcall; external 'kernel32.dll';

procedure InitializeCriticalSection(lpCriticalSection: PRTL_CRITICAL_SECTION); stdcall; external 'kernel32.dll';
procedure EnterCriticalSection(lpCriticalSection: PRTL_CRITICAL_SECTION); stdcall; external 'kernel32.dll';
procedure LeaveCriticalSection(lpCriticalSection: PRTL_CRITICAL_SECTION); stdcall; external 'kernel32.dll';
procedure DeleteCriticalSection(lpCriticalSection: PRTL_CRITICAL_SECTION); stdcall; external 'kernel32.dll';
function TryEnterCriticalSection(lpCriticalSection: PRTL_CRITICAL_SECTION): BOOL; stdcall; external 'kernel32.dll';

function VirtualAlloc(lpAddress: LPVOID; dwSize: SIZE_T; flAllocationType: DWORD; flProtect: DWORD): LPVOID; stdcall; external 'kernel32.dll';
function VirtualFree(lpAddress: LPVOID; dwSize: SIZE_T; dwFreeType: DWORD): BOOL; stdcall; external 'kernel32.dll';
function VirtualProtect(lpAddress: LPVOID; dwSize: SIZE_T; flNewProtect: DWORD; lpflOldProtect: PDWORD): BOOL; stdcall; external 'kernel32.dll';
function VirtualLock(lpAddress: LPVOID; dwSize: SIZE_T): BOOL; stdcall; external 'kernel32.dll';
function VirtualUnlock(lpAddress: LPVOID; dwSize: SIZE_T): BOOL; stdcall; external 'kernel32.dll';   
function LocalFree(hMem: HLOCAL): HLOCAL; stdcall; external 'kernel32.dll';

function SetFilePointerEx(hFile: THandle; liDistanceToMove: Int64; lpNewFilePointer: PInt64; dwMoveMethod: DWORD): BOOL; stdcall; external 'kernel32.dll';
function FlushFileBuffers(hFile: THandle): BOOL; stdcall; external 'kernel32.dll';
procedure GetSystemTime(lpSystemTime: PSYSTEMTIME); stdcall; external 'kernel32.dll';
function SystemTimeToFileTime(lpSystemTime: PSYSTEMTIME; lpFileTime: PFILETIME): BOOL; stdcall; external 'kernel32.dll';
function SetFileTime(hFile: THandle; lpCreationTime: PFILETIME; lpLastAccessTime: PFILETIME; lpLastWriteTime: PFILETIME): BOOL; stdcall; external 'kernel32.dll';
function GetFileAttributesA(lpFileName: LPCSTR): DWORD; stdcall; external 'kernel32.dll';
function GetFileAttributesW(lpFileName: LPCWSTR): DWORD; stdcall; external 'kernel32.dll';
function GetCurrentDirectoryW(nBufferLength: DWORD; lpBuffer: LPWSTR): DWORD; stdcall; external 'kernel32.dll';
function GetCommandLineW: PWideChar; stdcall; external 'kernel32.dll';
function CommandLineToArgvW(lpCmdLine: PWideChar; var pNumArgs: Integer): PPWideChar; stdcall; external 'shell32.dll';

implementation

end.
