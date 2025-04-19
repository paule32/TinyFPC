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
unit QApplicationPascalExport;

interface
uses Windows, SysUtils, QObjectPascalExport;

function GetCommandLineA: LPSTR; stdcall; external kernel32;
function CommandLineToArgvA(CmdLine: PAnsiChar; var argc: Integer): PPAnsiChar; stdcall; export;

function StrAlloc(Size: Cardinal): PChar; stdcall; external RTLDLL;

type
  (**
   * \class   QApplication
   * \brief   The QApplication class manages the GUI application's control flow and main settings.
   * \details QApplication specializes QGuiApplication with some functionality needed for
   *          QWidget-based applications. It handles widget specific initialization, and
   *          finalization.
   *
   *          For any GUI application using Qt, there is precisely one QApplication object,
   *          no matter whether the application has 0, 1, 2 or more windows at any given
   *          time.
   *          For non-QWidget based Qt applications, use QGuiApplication instead, as it does
   *          not depend on the QtWidgets library.
   *
   *          Some GUI applications provide a special batch mode ie. provide command line
   *          arguments for executing tasks without manual intervention. In such non-GUI mode,
   *          it is often sufficient to instantiate a plain QCoreApplication to avoid unnecessarily
   *          initializing resources needed for a graphical user interface.
   *
   *          The following example shows how to dynamically create an appropriate type of
   *          application instance:
   *)
  QApplication = class(QObject)
  public
    (**
     * \brief This is the Pascal constructor for class QApplication.
     *)
    constructor Create(ArgCount: Integer; Args: PPChar); overload;
    constructor Create; overload;
    
    (**
     * \brief This ist the Pascal destructor for class QApplication.
     *)
    destructor Destroy;
  end;

function  QApplication_Create: Pointer; stdcall; export;  
procedure QApplication_Destroy(P: QApplication); stdcall; export;

implementation

function CommandLineToArgvA(CmdLine: PAnsiChar; var argc: Integer): PPAnsiChar;
const
  GMEM_FIXED = $0000;
var
  argv: PPAnsiChar;
  _argv: PAnsiChar;
  len: Cardinal;
  i, j: Cardinal;
  a: AnsiChar;
  in_QM, in_TEXT, in_SPACE: Boolean;
  allocSize: Cardinal;
begin
  len := StrLen(CmdLine);
  // Speichergröße berechnen (Pointer-Array + Stringblock)
  allocSize := ((len + 2) div 2) * SizeOf(Pointer) + SizeOf(Pointer);

  argv := PPAnsiChar(GlobalAlloc(GMEM_FIXED, allocSize + (len + 2) * SizeOf(AnsiChar)));
  if argv = nil then
  begin
    argc := 0;
    Result := nil;
    Exit;
  end;

  _argv := PAnsiChar(PByte(argv) + allocSize);

  argc := 0;
  argv[argc] := _argv;

  in_QM := False;
  in_TEXT := False;
  in_SPACE := True;
  i := 0;
  j := 0;

  while CmdLine[i] <> #0 do
  begin
    a := CmdLine[i];
    if in_QM then
    begin
      if a = '"' then
        in_QM := False
      else
      begin
        _argv[j] := a;
        Inc(j);
      end;
    end
    else
    begin
      case a of
        '"':
        begin
          in_QM := True;
          in_TEXT := True;
          if in_SPACE then
          begin
            argv[argc] := @_argv[j];
            Inc(argc);
          end;
          in_SPACE := False;
        end;
        ' ', #9, #10, #13:
        begin
          if in_TEXT then
          begin
            _argv[j] := #0;
            Inc(j);
          end;
          in_TEXT := False;
          in_SPACE := True;
        end;
      else
        in_TEXT := True;
        if in_SPACE then
        begin
          argv[argc] := @_argv[j];
          Inc(argc);
        end;
        _argv[j] := a;
        Inc(j);
        in_SPACE := False;
      end;
    end;
    Inc(i);
  end;

  _argv[j] := #0;
  argv[argc] := nil;

  Result := argv;
end;

procedure dummy; assembler; [public, alias: 'dummyQ'];
asm
call QApplication_Create
call QApplication_Destroy
end;

(**
 * \brief CTOR Create of QApplication
 * \param ArgCount - Integer
 * \param Args     - Array of String
 *)
constructor QApplication.Create(
  ArgCount: Integer;
  Args: PPChar);
begin
  inherited Create;
end;

constructor QApplication.Create;
begin
  inherited Create;
end;

destructor QApplication.Destroy;
begin
  inherited Destroy;
end;

function  QApplication_Create: Pointer; stdcall; export;
var
  cmdline   : PAnsiChar;
  Args      : PPAnsiChar;
  ArgsCount : Integer;
  S, R      : PChar;
  TotalLen  : Integer;
  
  P : Pointer;
  I : Integer;
  A : Array of String;
begin
  result  := nil;
  CmdLine := GetCommandLineA;
  Args    := CommandLineToArgvA(CmdLine, ArgsCount);
  
  if Args = nil then
  begin
    MessageBoxA(0,'Error: can not parse command line.', 'Error', MB_OK);
    ExitProcess(1);
  end;
      
  TotalLen := 128;
  
  R := StrAlloc(TotalLen);
  S := StrAlloc(16);
  
  //StrCopy(S, IntToStr(ArgsCount));
  
  StrCopy(R, 'Count of Parameters: ');
  StrCopy(S, IntToStr(ArgsCount));
  
  MessageBoxA(0, R, '222  11  222', 0);
  MessageBoxA(0, S, '222  11  222', 0);
  
  StrCat (R, S);
  
  MessageBoxA(0, R, '222  11  222', 0);
  
  StrDispose(R);
  
  ExitProcess(0);
  //P := QApplication.Create;(ParamCount, A));
  
  result := nil;
end;

procedure QApplication_Destroy(P: QApplication); stdcall; export;
begin
  P.Free;
end;

initialization
dummy

end.
