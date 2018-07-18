
#include "csstring.h"

namespace CsLisp
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

	string string::ReplaceIn(string temp, const string & findText, const string & arg)
	{
		auto pos = temp.find(findText);
		if (pos != std::string::npos)
		{
			temp = temp.replace(pos, findText.size(), arg);
		}
		return temp;
	}

	string string::ReplaceInWithFill(string temp, const string & findText, const string & arg)
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

	string string::Format(const string & txt, const string & arg1, const string & arg2, const string & arg3, const string & arg4, const string & arg5)
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

    int string::CompareOrdinal(const string & a, const string & b)
	{
		return strcmp(a.c_str(), b.c_str());
	}
}