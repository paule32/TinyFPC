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
/**
 * \file fpcDLL.h
 * \brief This File is the prototyp File for the miniFPC/RTL Project.
 * 
 * \autor \a_currentAuthors
 */
#pragma once

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
 */
struct TDLLerror {
	uint32_t ErrorCode;                 ///< \cond english The Error Code of current Error   \endcond
	uint32_t ErrorTextLength;           ///< \cond english The Length for ErrorText          \endcond
	uint32_t ErrorFromFunctionLength;   ///< \cond english The Length for the Function Name  \endcond
	uint32_t ErrorFromLine;             ///< \cond english The Line of Error occurency       \endcond
	char*    ErrorText;                 ///< \cond english The Error Text                    \endcond
	char*    ErrorTimeStamp[32];        ///< \cond english A Time stamp for the Error        \endcond
	char*    ErrorFromFunction;         ///< \cond english This is the Function name in which the Error occur \endcond
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
 */
struct TDLLargs {
	uint32_t ArgsCount;                 ///< \cond english This field holds the numbers of Arguments        \endcond
	char*    ArgsString;				///< \cond english This field represents the mangled Argument Names \endcond
};

/**
 * \struct TDLLrequest
 * \cond   english
 * \brief  The following structure is the main struct for exchange arguments from FPC
 *         and C++
 * \endcond
 */
struct TDLLrequest {
	struct TDLLerror Error;
	struct TDLLargs  Args;
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

FPCDLL_API LPCSTR __cdecl
fpc_ReplaceText(
	LPVOID  Request,
	LPCSTR  S,
	LPCSTR  oldPattern,
	LPCSTR  newPattern,
	DWORD32 flags,
	DWORD32 lenS,
	DWORD32 lenOldPattern,
	DWORD32 lenNewPattern);

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
