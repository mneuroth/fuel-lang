#ifndef _CSSTRING_H
#define _CSSTRING_H

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

		bool StartsWith(const string & txt)
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

		static string Format(const string & txt, const string & args)
		{
// TODO
			//string s = std::str(std::format("%2% %2% %1%\n") % "world" % "hello");
			//std::strin
			return txt;
		}

		int Length()
		{
			return size();
		}

		const static string Empty;
	};

//	inline CsLisp::string operator+(const CsLisp::string & s1, const CsLisp::string & s2)
//	{
//		CsLisp::string s = s1;
//		s.append(s2);
//		return s;
//	}
}

#endif