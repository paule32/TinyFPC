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
uses QObjectPascalExport;

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
    constructor Create(ArgCount: Integer; Args: Array of String);
    
    (**
     * \brief This ist the Pascal destructor for class QApplication.
     *)
    destructor Destroy;
  end;
  
implementation

(**
 * \brief CTOR Create of QApplication
 * \param ArgCount - Integer
 * \param Args     - Array of String
 *)
constructor QApplication Create(
  ArgCount: Integer;
  Args: Array of String);
begin
  inherited Create;
end;

destructor QApplication.Destroy;
begin
  inherited Destroy;
end;

function  QApplication_Create: Pointer;
var
  P : Pointer;
  A : Array of String;
begin
  SetLength(A, 2);
  P := QApplication.Create(ParamCount, A);
  resuklt := P;
end;

procedure QApplication_Destroy(P: QApplication);
begin
  Free(P);
end;

end.
