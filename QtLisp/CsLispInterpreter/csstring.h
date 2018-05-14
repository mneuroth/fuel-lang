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
#include <cctype>
#include <functional>
#include <memory>
#include <algorithm>

namespace CsLisp
{
	class string : public std::string
	{
	public:
		string()
			: std::string()
		{
		}

		string(const char * txt)
			: std::string(txt)
		{			
		}

		string(char ch)
		{
			*this += ch;
		}

		string(const string & txt)
			: std::string(txt)
		{
		}

		string(const std::string & txt)
			: std::string(txt)
		{
		}

//		string& operator+(const string & other)
//		{
//			append(other);
//			return *this;
//		}

//		string& operator=(const string & other)
//		{
//// TODO
//			return *this;
//		}
//
//		string& operator=(const char * other)
//		{
//// TODO
//			return *this;
//		}

		int Length() const
		{
			return size();
		}

		bool Equals(const string & txt)
		{
			return *this == txt;
		}

		string ToUpper()
		{
			string temp = *this;
			std::transform(temp.begin(), temp.end(), temp.begin(), std::ptr_fun<int, int>(std::toupper));
			return temp;
		}

		bool StartsWith(const string & txt) const
		{
			return compare(0, txt.length(), txt) == 0;
		}

		string Substring(int offs, int length = npos)
		{
			return string(substr(offs, length));
		}

		int IndexOf(const string & txt, const string & arg/*StringComparison.InvariantCulture*/)
		{
			return (*this).find_first_of(txt);
		}

		static string Format(const string & txt, const string & arg1, const string & arg2 = "", const string & arg3 = "", const string & arg4 = "", const string & arg5 = "")
		{
// TODO
			//string s = std::str(std::format("%2% %2% %1%\n") % "world" % "hello");
			//std::string
			return txt;
		}

		static bool IsNullOrEmpty(const string & txt)
		{
			return txt.size() == 0;
		}

		int Length()
		{
			return size();
		}

		const static string Empty;
	};

	class String
	{
	public:
		static int CompareOrdinal(const string & a, const string & b)
		{
			return strcmp(a.c_str(), b.c_str());
		}
	};
	

//	inline CsLisp::string operator+(const CsLisp::string & s1, const CsLisp::string & s2)
//	{
//		CsLisp::string s = s1;
//		s.append(s2);
//		return s;
//	}
}

#endif
