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

// mfcFPC.cpp: Definiert die Initialisierungsroutinen für die DLL.
//

#include "pch.h"
#include "framework.h"
#include "mfcFPC.h"

#ifdef _DEBUG
#define new DEBUG_NEW
#endif

//
//TODO: Wenn diese DLL dynamisch mit MFC-DLLs verknüpft ist,
//		muss für alle aus dieser DLL exportierten Funktionen, die in
//		MFC aufgerufen werden, das AFX_MANAGE_STATE-Makro
//		am Anfang der Funktion hinzugefügt werden.
//
//		Beispiel:
//
//		extern "C" BOOL PASCAL EXPORT ExportedFunction()
//		{
//			AFX_MANAGE_STATE(AfxGetStaticModuleState());
//			// Hier normaler Funktionsrumpf
//		}
//
//		Es ist sehr wichtig, dass dieses Makro in jeder Funktion
//		vor allen MFC-Aufrufen angezeigt wird.  Dies bedeutet,
//		dass es als erste Anweisung innerhalb der
//		Funktion angezeigt werden muss, sogar vor jeglichen Deklarationen von Objektvariablen,
//		da ihre Konstruktoren Aufrufe in die MFC-DLL generieren
//		könnten.
//
//		Siehe Technische Hinweise für MFC 33 und 58 für weitere
//		Details.
//

# include "fpcDLL.h"
// CmfcFPCrtl

BEGIN_MESSAGE_MAP(CmfcFPCrtl, CWinApp)
END_MESSAGE_MAP()


// CmfcFPCrtl-Erstellung

CmfcFPCrtl::CmfcFPCrtl()
{
	// TODO: Hier Code zur Konstruktion einfügen
	// Alle wichtigen Initialisierungen in InitInstance positionieren
}


// Das einzige CmfcFPCrtl-Objekt

CmfcFPCrtl theRtl;

// CmfcFPCApp-Initialisierung
BOOL CmfcFPCrtl::InitInstance()
{
	CWinApp::InitInstance();

	if (!AfxSocketInit()) {
		AfxMessageBox(IDP_SOCKETS_INIT_FAILED);
		return FALSE;
	}

	return TRUE;
}
