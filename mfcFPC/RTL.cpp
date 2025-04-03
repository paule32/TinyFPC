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

#ifdef __cplusplus
extern "C" {
#endif

// ---------------------------------------------------------------------------------------
// internal helper members ...
// ---------------------------------------------------------------------------------------
std::vector<std::string> global_str;
std::string result_str;

std::string __cdecl
replaceSubstring(
	const std::string str,
	const std::string& oldPattern,
	const std::string& newPattern) {

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
FPCDLL_API LPCSTR __cdecl
fpc_ReplaceText(
	LPCSTR  S,
	LPCSTR  oldPattern,
	LPCSTR  newPattern,
	DWORD32 flags,
	DWORD32 lenS,
	DWORD32 lenOldPattern,
	DWORD32 lenNewPattern) {

	std::string safe_S  (S, lenS);             // sichere Kopie mit Länge
	std::string safe_Old(oldPattern, lenOldPattern);
	std::string safe_New(newPattern, lenNewPattern);

	// Ersetzen
	global_str.push_back(replaceSubstring(safe_S, safe_Old, safe_New));
	result_str = global_str.back();
	global_str.pop_back();
	MessageBoxA(0, LPCSTR(result_str.c_str()), LPCSTR("---- 00 ---"), 0);
	return LPCSTR(result_str.c_str());
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

#ifdef __cplusplus
};
#endif
