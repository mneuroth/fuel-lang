/*
* FUEL(isp) is a fast usable embeddable lisp interpreter.
*
* Copyright (c) 2016 Michael Neuroth
*
* Permission is hereby granted, free of charge, to any person obtaining
* a copy of this software and associated documentation files (the "Software"),
* to deal in the Software without restriction, including without limitation
* the rights to use, copy, modify, merge, publish, distribute, sublicense,
* and/or sell copies of the Software, and to permit persons to whom the
* Software is furnished to do so, subject to the following conditions:
*
* The above copyright notice and this permission notice shall be included
* in all copies or substantial portions of the Software.
*
* THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS
* OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
* FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL
* THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR
* OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE,
* ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR
* OTHER DEALINGS IN THE SOFTWARE.
*
* */

#ifndef _CSSTRING_H
#define _CSSTRING_H

#include <string.h>
#include <string>
#include <vector>

#include <stdlib.h>

namespace CppLisp
{
	//
	// Class to wrap the stl string class with functions of the C# string class
	//
	class string : public std::string
	{
	public:
		string();
		string(const char * txt);
		string(char ch);
		string(const string & txt);
		string(const std::string & txt);

		inline size_t Length() const
		{
			return size();
		}

		inline bool Equals(const string & txt)
		{
			return *this == txt;
		}

		inline bool StartsWith(const string & txt) const
		{
			return compare(0, txt.length(), txt) == 0;
		}

		inline string Substring(size_t offs, size_t length = npos) const
		{
			return string(substr(offs, length));
		}

        inline size_t IndexOf(const string & txt, const string & /*arg*//*StringComparison.InvariantCulture*/) const
		{
            return find(txt);
		}

		inline size_t IndexOf(const string & txt, size_t offset) const
		{
			return find(txt, offset);
		}

		inline void Append(const string & txt)
		{
			append(txt);
		}

		inline bool Contains(const string & txt) const
		{
			return IndexOf(txt, "invariant_culture") != string::npos;
		}

		inline size_t LastIndexOf(const string & text, const string & /*compareMethod*/)
		{
			return find_last_of(text);
		}

		string Trim() const;
		string ToUpper() const;
		string ToLower() const;
		bool EndsWith(const string & txt);
		std::vector<string> Split(const string & seperator) const;

		static bool IsNullOrEmpty(const string & txt);
		static string ReplaceIn(string temp, const string & findText, const string & arg);
		static string ReplaceInWithFill(string temp, const string & findText, const string & arg);
		static string Format(const string & txt, const string & arg1, const string & arg2 = "", const string & arg3 = "", const string & arg4 = "", const string & arg5 = "");
		static int CompareOrdinal(const string & a, const string & b);

		const static string Empty;
	};
}

#endif
