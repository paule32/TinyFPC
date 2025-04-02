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

{$if defined(codeh) and defined(codei)}
{$FATAL interface and implementation at the same time not possible}
{$endif}

{$ifdef codeh}

const
   vtInteger       = 0;
   vtBoolean       = 1;
   vtChar          = 2;
{$ifndef FPUNONE}
   vtExtended      = 3;
{$endif}
   vtString        = 4;
   vtPointer       = 5;
   vtPChar         = 6;
   vtObject        = 7;
   vtClass         = 8;
   vtWideChar      = 9;
   vtPWideChar     = 10;
   vtAnsiString    = 11;
   vtCurrency      = 12;
   vtVariant       = 13;
   vtInterface     = 14;
   vtWideString    = 15;
   vtInt64         = 16;
   vtQWord         = 17;
   vtUnicodeString = 18;

type
  PVarRec = ^TVarRec;
  TVarRec = record
    case VType: sizeint of
      vtInteger: (VInteger: longint);
      vtBoolean: (VBoolean: boolean);
      vtChar: (VChar: char);
      vtWideChar: (VWideChar: widechar);
      {$ifndef FPUNONE}
      vtExtended: (VExtended: PExtended);
      {$endif}
      vtString: (VString: pshortstring);
      vtPointer: (VPointer: Pointer);
      vtPChar: (VPChar: pansichar);
      vtObject: (VObject: TObject);
      vtClass: (VClass: TClass);
      vtPWideChar: (VPWideChar: pwidechar);
      vtAnsiString: (VAnsiString: Pointer);
      vtCurrency: (VCurrency: PCurrency);
      vtVariant: (VVariant: PVariant);
      vtInterface: (VInterface: Pointer);
      vtWideString: (VWideString: Pointer);
      vtInt64: (VInt64: PInt64);
      vtUnicodeString: (VUnicodeString: Pointer);
      vtQWord: (VQWord: PQWord);
  end;

{$endif}

{$ifdef codei}

{$endif}

