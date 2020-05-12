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

#include "csstring.h"
#include "csexception.h"

#include <cctype>
#include <algorithm>
#include <functional>

namespace CppLisp
{
	string::string()
		: std::string()
	{
	}

	string::string(const char * txt)
		: std::string(txt)
	{
	}

	string::string(char ch)
	{
		*this += ch;
	}

	string::string(const string & txt)
		: std::string(txt)
	{
	}

	string::string(const std::string & txt)
		: std::string(txt)
	{
	}

	bool string::EndsWith(const string & txt)
	{
		if (txt.size() > size())
		{
			return false;
		}
		return std::equal(txt.rbegin(), txt.rend(), rbegin());
	}

	string string::ToUpper() const
	{
		string temp = *this;
		std::transform(temp.begin(), temp.end(), temp.begin(), std::ptr_fun<int, int>(std::toupper));
		return temp;
	}

	string string::ToLower() const
	{
		string temp = *this;
		std::transform(temp.begin(), temp.end(), temp.begin(), std::ptr_fun<int, int>(std::tolower));
		return temp;
	}

	string string::Trim() const
	{
		// https://stackoverflow.com/questions/216823/whats-the-best-way-to-trim-stdstring
		string s = *this;
		s.erase(s.begin(), std::find_if_not(s.begin(), s.end(), [](char c) { return std::isspace(c); }));
		s.erase(std::find_if_not(s.rbegin(), s.rend(), [](char c) { return std::isspace(c); }).base(), s.end());
		return s;
	}

	std::vector<string> string::Split(const string & seperator) const
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

	bool string::IsNullOrEmpty(const string & txt)
	{
		return txt.size() == 0;
	}

	string string::ReplaceIn(const string & temp, const string & findText, const string & arg, bool & found)
	{
		auto pos = temp.find(findText);
		string ret = temp;
		if (pos != std::string::npos)
		{
			ret = ret.replace(pos, findText.size(), arg);
			found = true;
		}
		else
		{
			found = false;
		}
		return ret;
	}

	string string::ReplaceInWithFill(const string & temp, const string & findText, const string & arg, bool & found)
	{
		found = false;
		string ret = temp;
		auto pos = ret.find(findText);
		if (pos != std::string::npos)
		{
			auto pos2 = ret.find("}", pos + 1);
			if (pos2 != std::string::npos)
			{
				string len = ret.substr(pos + 3, pos2 - pos - findText.size());
				int l = std::stoi(len);
				if (l > 0)
				{
					int count = l - (int)arg.size();
					ret = ret.replace(pos, pos2 - pos + 1, std::string(count > 0 ? count : 0, ' ') + arg);
					found = true;
				}
				else
				{
					int count = abs(l) - (int)arg.size();
					ret = ret.replace(pos, pos2 - pos + 1, arg + std::string(count > 0 ? count : 0, ' '));
					found = true;
				}
			}
		}
		return ret;
	}

	string string::Format(const string & txt, const string & arg1, const string & arg2, const string & arg3, const string & arg4, const string & arg5)
	{
		const string ERR_MSG = "Not enough items for format string: " + txt;
		bool found;
		string temp = txt;
		temp = ReplaceIn(temp, "{0}", arg1, found);
		if (found && arg1 == NULL_STRING)
		{
			throw LispExceptionBase(ERR_MSG);
		}
		temp = ReplaceInWithFill(temp, "{0,", arg1, found);
		if (found && arg1 == NULL_STRING)
		{
			throw LispExceptionBase(ERR_MSG);
		}
		temp = ReplaceIn(temp, "{1}", arg2, found);
		if (found && arg2 == NULL_STRING)
		{
			throw LispExceptionBase(ERR_MSG);
		}
		temp = ReplaceInWithFill(temp, "{1,", arg2, found);
		if (found && arg2 == NULL_STRING)
		{
			throw LispExceptionBase(ERR_MSG);
		}
		temp = ReplaceIn(temp, "{2}", arg3, found);
		if (found && arg3 == NULL_STRING)
		{
			throw LispExceptionBase(ERR_MSG);
		}
		temp = ReplaceInWithFill(temp, "{2,", arg3, found);
		if (found && arg3 == NULL_STRING)
		{
			throw LispExceptionBase(ERR_MSG);
		}
		temp = ReplaceIn(temp, "{3}", arg4, found);
		if (found && arg4 == NULL_STRING)
		{
			throw LispExceptionBase(ERR_MSG);
		}
		temp = ReplaceInWithFill(temp, "{3,", arg4, found);
		if (found && arg4 == NULL_STRING)
		{
			throw LispExceptionBase(ERR_MSG);
		}
		temp = ReplaceIn(temp, "{4}", arg5, found);
		if (found && arg5 == NULL_STRING)
		{
			throw LispExceptionBase(ERR_MSG);
		}
		temp = ReplaceInWithFill(temp, "{4,", arg5, found);
		if (found && arg5 == NULL_STRING)
		{
			throw LispExceptionBase(ERR_MSG);
		}
		return temp;
	}

    int string::CompareOrdinal(const string & a, const string & b)
	{
		return strcmp(a.c_str(), b.c_str());
	}
}
