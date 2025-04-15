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
unit QApplicationPascalImport;

interface
uses Windows, global;

type
  QApplication = class
  private
    Handle: DWORD32;
  public
    constructor Create;
    destructor Destroy;
  end;

function  QApplication_Create: Pointer; stdcall; external RTLDLL;
procedure QApplication_Destroy(P: Pointer); stdcall; external RTLDLL;

var
  QApplication_Array: TArray<QApplication>;

implementation

constructor QApplication.Create;
begin
  SetLength(QApplication_Array, High(QApplication_Array) + 1);
  QApplication_Array[0] := QApplication_Create;
end;

destructor QApplication.Destroy;
begin
  QApplication_Destroy(nil);
end;

end.
