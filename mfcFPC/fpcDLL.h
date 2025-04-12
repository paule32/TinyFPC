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
#pragma once
/**
 * \file fpcDLL.h
 * \brief This File is the prototyp File for the miniFPC/RTL Project.
 * 
 * \autor \a_currentAuthors
 */

#ifndef FPCDLL_H_
# define FPCDLL_H_

// Export-Makro definieren
#ifdef FPCDLL_EXPORTS
# define FPCDLL_API __declspec(dllexport)
#else
# define FPCDLL_API __declspec(dllimport)
#endif

# include "mfcFPC.h"

#ifdef __cplusplus
extern "C" {
#endif

/**
 * \struct TDLLerror
 * \cond   english
 * \brief  The following structure is the main struct for exchange errors from C++
 *         and FPC.
 *
 * This structure is used for the Exchange of Error Codes, Messages, and Debug
 * Informations from C++ to FPC, and FPC to C++.
 * \endcond
 * \cond   german
 * \brief  Die folgende Struktur dient dem Austausch von Fehler und Fehler-Codes
 *         von C++ nach FPC.
 * 
 * Diese Struktur wird verwendet, um den Austausch von Fehler-Codes Informationen
 * von C++ nach FPC, und FPC nach C++ zu handhaben.
 * \endcond
 */
struct TDLLerror {
	/**
	 * \cond english
	 * \brief The Error Code of current Error.
	 * \endcond
	 * \cond german
	 * \brief Der aktuelle Fehler-Code.
	 * \endcond
     */
	DWORD32 ErrorCode;

	/**
	 * \cond english
	 * \brief The Length for ErrorText
	 * \endcond
	 * \cond german
	 * \brief Die Länge für den Fehler-Code.
	 * \endcond
	 */
	DWORD32 ErrorTextLength;

	/**
	 * \cond english
	 * \brief The Length for the Function Name.
	 * \endcond
	 * \cond german
	 * \brief Die Länge für den Funktion-Namen.
	 * \endcond
	 */
	DWORD32 ErrorFromFunctionLength;

	/**
	 * \cond english
	 * \brief The Line of Error occurency.
	 * \endcond
	 * \cond german
	 * \brief Die Zeile den Fehlers
	 * \endcond
	 */
	DWORD32 ErrorFromLine;

	/**
	 * \cond english
	 * \brief The Line of Error occurency.
	 * \endcond
	 * \cond german
	 * \brief Der zugeordnete Fehler-Text.
	 * \endcond
	 */
	LPCSTR  ErrorText;

	/**
	 * \cond english
	 * \brief The Line of Error occurency.
	 * \endcond
	 * \cond german
	 * \brief Der Zeit-Stempel für den Fehler.
	 * \endcond
	 */
	LPCSTR  ErrorTimeStamp[32];

	/**
	 * \cond english
	 * \brief This is the Function name in which the Error occur
	 * \endcond
	 * \cond german
	 * \brief Dies ist der Funktion (Name) in welch der Fehler ausgelöst wurde.
	 * \endcond
	 */
	LPCSTR  ErrorFromFunction;
};

/**
 * \struct TDLLargs
 * \cond   english
 * \brief  The following structure is the main struct for exchange arguments from FPC
 *         and C++
 * 
 * This structure is used for Exchange Function Arguments from FPC to C++,
 * and C++ to FPC.
 * \endcond
 * \cond   german
 * \brief  Die folgende Struktur dient zum austauschen von Argumenten von FPC und C++
 * 
 * Diese Struktur wird verwendet, um den Austausch von Funktion-Arguemten von FPC nach C++
 * und C++ nach FPC zu bewerkstelligen.
 * \endcond
 */
struct TDLLargs {
	/**
	 * \cond english
 	 * \brief This field holds the numbers of Arguments
	 * \endcond
	 * \cond german
	 * \brief Dieses Feld bestimmt die Anzahl der übergebenen Argumente
	 * \endcond
	 */
	DWORD32 ArgsCount;

	/**
	 * \cond english
	 * \brief This field represents the mangled Argument Names
	 * \endcond
	 * \cond german
	 * \brief Dieses Feld entspricht den Argumenten-Typ
	 * \endcond
	 */
	LPCSTR  ArgsString;
};

/**
 * \struct TDLLrequest
 * \cond   english
 * \brief  The following structure is the main struct for exchange arguments from FPC
 *         and C++
 * \endcond
 */
struct TDLLrequest {
	DWORD32 version;
	struct  TDLLerror Error;
	struct  TDLLargs  Args;
};
extern std::string result_string;

/**
 * \enum   TReplaceFlags
 * \cond   english
 * \brief  Flags for StringReplace function.
 * \detail TReplaceFlags determines the behaviour of the \ref StringReplace function.
 * \endcond
 */
enum TReplaceFlags {
	rfReplaceAll = 1 << 0,	///< \cond english Replace all occurences of the search string witht the replacement string \endcond
	rfIgnoreCase = 1 << 1	///< \cond english Search case insensive \endcond
};

FPCDLL_API TDLLrequest * __cdecl fpc_ReplaceText(TDLLrequest * P);
#if 0
	LPCSTR  S,
	LPCSTR  oldPattern,
	LPCSTR  newPattern,
	DWORD32 flags,
	DWORD32 lenS,
	DWORD32 lenOldPattern,
	DWORD32 lenNewPattern);
#endif

FPCDLL_API LPCSTR __cdecl
StringReplace(
	LPCSTR  origin,
	LPCSTR  oldPattern,
	LPCSTR  newPattern,
	DWORD32 flags);

#ifdef __cplusplus
};
#endif

#endif  // FPCDLL_H_
