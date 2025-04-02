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

#ifndef FPCDLL_H_
# define FPCDLL_H_

// Export-Makro definieren
#ifdef FPCDLL_EXPORTS
# define FPCDLL_API __declspec(dllexport)
#else
# define FPCDLL_API __declspec(dllimport)
#endif

#ifdef __cplusplus
extern "C" {
#endif

// \brief Flags for StringReplace function.
// \detail TReplaceFlags determines the behaviour of the \ref StringReplace function.
enum TReplaceFlags {
	rfReplaceAll = 1 << 0,	//! Replace all occurences of the search string witht the replacement string
	rfIgnoreCase = 1 << 1	//! Search case insensive
};

char*
StringReplace(
	char* origin,
	char* oldPattern,
	char* newPattern,
	uint32_t flags);

#ifdef __cplusplus
};
#endif

#endif  // FPCDLL_H_
