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
#include <vector>

#include <stdlib.h>

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

		size_t Length() const
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

		string Substring(size_t offs, size_t length = npos) const
		{
			return string(substr(offs, length));
		}

        size_t IndexOf(const string & txt, const string & /*arg*//*StringComparison.InvariantCulture*/) const
		{
            return find(txt);
		}

		static string ReplaceIn(string temp, const string & findText, const string & arg)
		{
			auto pos = temp.find(findText);
			if (pos != std::string::npos)
			{
				temp = temp.replace(pos, findText.size(), arg);
			}
			return temp;
		}

		static string ReplaceInWithFill(string temp, const string & findText, const string & arg)
		{
			auto pos = temp.find(findText);
			if (pos != std::string::npos)
			{
				auto pos2 = temp.find("}", pos + 1);
				if (pos2 != std::string::npos)
				{
					string len = temp.substr(pos + 3, pos2 - pos - findText.size());
					int l = std::stoi(len);
					if (l > 0)
					{
						int count = l - (int)arg.size();
						temp = temp.replace(pos, pos2 - pos + 1, std::string(count > 0 ? count : 0, ' ') + arg);
					}
					else
					{
						int count = abs(l) - (int)arg.size();
						temp = temp.replace(pos, pos2 - pos + 1, arg + std::string(count > 0 ? count : 0, ' '));
					}
				}
			}
			return temp;
		}

		static string Format(const string & txt, const string & arg1, const string & arg2 = "", const string & arg3 = "", const string & arg4 = "", const string & arg5 = "")
		{
			string temp = txt;
			temp = ReplaceIn(temp, "{0}", arg1);
			temp = ReplaceInWithFill(temp, "{0,", arg1);
			temp = ReplaceIn(temp, "{1}", arg2);
			temp = ReplaceInWithFill(temp, "{1,", arg2);
			temp = ReplaceIn(temp, "{2}", arg3);
			temp = ReplaceInWithFill(temp, "{2,", arg3);
			temp = ReplaceIn(temp, "{3}", arg4);
			temp = ReplaceInWithFill(temp, "{3,", arg4);
			temp = ReplaceIn(temp, "{4}", arg5);
			temp = ReplaceInWithFill(temp, "{4,", arg5);
			return temp;
		}

		static bool IsNullOrEmpty(const string & txt)
		{
			return txt.size() == 0;
		}

		size_t Length()
		{
			return size();
		}

		void Append(const string & txt)
		{
			append(txt);
		}

		bool Contains(const string & txt) const
		{
			return IndexOf(txt, "invariant_culture") != string::npos;
		}

		bool EndsWith(const string & txt)
		{
			if (txt.size() > size())
			{
				return false;
			}
			return std::equal(txt.rbegin(), txt.rend(), rbegin());
		}

		string Trim() const
		{
			// https://stackoverflow.com/questions/216823/whats-the-best-way-to-trim-stdstring
			string s = *this;
			s.erase(s.begin(), std::find_if_not(s.begin(), s.end(), [](char c) { return std::isspace(c); }));
			s.erase(std::find_if_not(s.rbegin(), s.rend(), [](char c) { return std::isspace(c); }).base(), s.end());
			return s;
		}

		std::vector<string> Split(const string & seperator) const
		{
			// https://stackoverflow.com/questions/5167625/splitting-a-c-stdstring-using-tokens-e-g
			string s = *this;
			std::vector<string> output;

			std::string::size_type prev_pos = 0, pos = 0;
			while ((pos = s.find(seperator, pos)) != std::string::npos)
			{
				std::string substring(s.substr(prev_pos, pos - prev_pos));

				output.push_back(substring);

				prev_pos = ++pos;
			}

			output.push_back(s.substr(prev_pos, pos - prev_pos)); // Last word

			return output;
		}

        size_t LastIndexOf(const string & text, const string & /*compareMethod*/)
		{
			return find_last_of(text);
		}

		static int CompareOrdinal(const string & a, const string & b)
		{
			return strcmp(a.c_str(), b.c_str());
		}

		const static string Empty;
	};
}

#endif
