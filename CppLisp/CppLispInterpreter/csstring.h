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

#if defined( ARDUINO_ARCH_ESP32 )
#define _GLIBCXX_USE_C99
#endif

#include <string.h>
#include <string>
#include <vector>
#include <algorithm>

#include <stdlib.h>

#ifdef _MSC_VER
#ifdef _EXPORTING
#define DLLEXPORT __declspec(dllexport)
#else
#define DLLEXPORT __declspec(dllimport)
#endif
#else
#define DLLEXPORT
#endif

//#if defined( __ANDROID__ )
//
//#include <string>
//#include <sstream>
//
//namespace std
//{
//    template <typename T>
//    inline std::string to_string(T value)
//    {
//        std::ostringstream os ;
//        os << value ;
//        return os.str() ;
//    }
//
//    inline int stoi(const string & s, size_t * errPos = 0, int base = 10)
//    {
//        char * p;
//        const char * val = s.c_str();
//        long ret = strtol(val, &p, base);
//        if( errPos != 0 )
//        {
//            *errPos = (size_t)(p - val);
//        }
//        return (int)ret;
//    }
//
//    inline double stod(const string & s, size_t * errPos = 0)
//    {
//        char * p;
//        const char * val = s.c_str();
//        double ret = strtod(val, &p);
//        if( errPos != 0 )
//        {
//            *errPos = (size_t)(p - val);
//        }
//        return ret;
//    }
//}
//#endif

#define NULL_STRING "##<NULL>##"

namespace CppLisp
{
	inline std::string & do_replace(std::string & s, const std::string & searchTxt, const std::string & replaceTxt)
	{
		if (!searchTxt.empty())
		{
			for (size_t pos = 0; (pos = s.find(searchTxt, pos)) != std::string::npos; pos += replaceTxt.size())
			{
				s.replace(pos, searchTxt.size(), replaceTxt);
			}
		}
		return s;
	}

	//
	// Class to wrap the stl string class with functions of the C# string class
	//
	class DLLEXPORT string : public std::string
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

		inline bool Equals(const string & txt) const
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

		inline string Replace(const string & searchTxt, const string & replaceTxt) const
		{
			std::string s = *this;
			return do_replace(s, searchTxt.c_str(), replaceTxt.c_str());
		}
		
		inline size_t IndexOf(const string & txt, const string & /*arg*//*StringComparison.InvariantCulture*/) const
		{
            return find(txt);
		}

		inline size_t IndexOf(const string & txt, size_t offset = 0) const
		{
			return find(txt, offset);
		}

		inline size_t IndexOf(const string & txt, size_t offset, size_t count) const
		{
			return find(txt.c_str(), offset, count);
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
		static string ReplaceIn(const string & temp, const string & findText, const string & arg, bool & found);
		static string ReplaceInWithFill(const string & temp, const string & findText, const string & arg, bool & found);
		static string Format(const string & txt, const string & arg1 = NULL_STRING, const string & arg2 = NULL_STRING, const string & arg3 = NULL_STRING, const string & arg4 = NULL_STRING, const string & arg5 = NULL_STRING);
		static int CompareOrdinal(const string & a, const string & b);

		const static string Empty;
	};
}

#endif
