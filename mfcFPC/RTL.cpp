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

# include "pch.h"

# define FPCDLL_EXPORTS
# include "fpcDLL.h"

// ---------------------------------------------------------------------------------------
// internal helper members ...
// ---------------------------------------------------------------------------------------
std::string result_string;

static std::string __cdecl
replaceSubstring(
	const std::string str,
	const std::string& oldPattern,
	const std::string& newPattern)
{
	std::string result = str;
	size_t index = 0;

	while ((index = result.find(oldPattern, index)) != std::string::npos) {
		result.replace(index, oldPattern.length(), newPattern);
		index += newPattern.length();
	}
	return result;
}

// ---------------------------------------------------------------------------------------
// internal section
// ---------------------------------------------------------------------------------------
extern "C" FPCDLL_API TDLLrequest* __cdecl
fpc_ReplaceText(
	TDLLrequest * P) {

	MessageBoxA(0, "1111111", "errrrr", 0);
	if (nullptr == P) {
		MessageBoxA(0, "erro", "errrrr", 0);
	}
	MessageBoxA(0, "222222", "errrrr", 0);
	if (P->version == 231) {
		MessageBoxA(0, "231", "ok", 0);
		if (P->Error.ErrorCode == 114422) {
			MessageBoxA(0, "ggHHHHHCDSGDFG", "22121212", 0);
		}
		P->Error.ErrorCode = 427;
	}
	MessageBoxA(0, "333333", "errrrr", 0);
	return P;
}

// ---------------------------------------------------------------------------------------
// global section
// ---------------------------------------------------------------------------------------

// \brief StringReplace
FPCDLL_API LPCSTR __cdecl
StringReplace(
	LPCSTR  origin,
	LPCSTR  oldPattern,
	LPCSTR  newPattern,
	DWORD32 flags)
{
	using std::string;
	using std::regex;

	string input = origin;
	auto  buffer = new char[input.length() + 1];
	if (buffer) {
		strcpy_s(buffer, input.length() + 1, input.c_str());
		MessageBoxA(0, buffer, input.c_str(), 0);
	}	else {
		MessageBoxA(0, "error ufo", "error mufo", 0);
		throw string("StringReplace: could not allocate memory.");
	}

	return (char*)buffer;
}
