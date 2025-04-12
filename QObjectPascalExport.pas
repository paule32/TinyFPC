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
unit QObjectPascalExport;

interface

type
  (**
   * \class   QObject
   * \brief   The QObject class is the base class of all Qt objects
   * \details QObject is the heart of the Qt Object Model.
   *          The central feature in this model is a very powerful mechanism for seamless
   *          object communication called signals and slots.
   *          You can connect a signal to a slot with connect() and destroy the connection
   *          with disconnect(). To avoid never ending notification loops you can temporarily
   *          block signals with blockSignals(). The protected functions connectNotify() and
   *          disconnectNotify() make it possible to track connections.
   *)
  QObject = class
  public
    (**
     * \brief this is the CTOR of class QObject
     *)
    constructor Create;
    
    (**
     * \brief This is the DTOR of class QObject
     *)
    destructor Destroy;
    
    (**
     * \brief This is a member of QObject
     *)
    procedure test;
  end;
  
implementation

constructor QObject.Create;
begin
end;

destructor QObject.Destroy;
begin
end;

end.
