
#include "cstypes.h"

#include <iostream>

namespace CsLisp
{
	template <class T>
	string IEnumerable<T>::DumpList() const 
	{
		string ret = "(";
		for (var elem : *this)
		{
			ret += elem->ToString();
			ret += ",";
		}
		return ret + ")";
	}

	void TextWriter::Write(const string & txt)
	{
		if (m_bToString)
		{
			m_sText += txt;
		}
		std::cout << txt;
	}

	void TextWriter::WriteLine()
	{
		if (m_bToString)
		{
			m_sText += "\n";
		}
		std::cout << std::endl;
	}

	void TextWriter::WriteLine(const string & txt)
	{
		if (m_bToString)
		{
			m_sText += txt + "\n";
		}
		std::cout << txt << std::endl;
	}

	void TextWriter::WriteLine(const string & txt, const string & txt1)
	{
		string temp = string::Format(txt, txt1);
		if (m_bToString)
		{
			m_sText += temp + "\n";
		}
		std::cout << temp << std::endl;
	}

	void TextWriter::WriteLine(const string & txt, const string & txt1, const string & txt2)
	{
		string temp = string::Format(txt, txt1, txt2);
		if (m_bToString)
		{
			m_sText += temp + "\n";
		}
		std::cout << temp << std::endl;
	}

	void TextWriter::WriteLine(const string & txt, const string & txt1, const string & txt2, const string & txt3)
	{
		string temp = string::Format(txt, txt1, txt2, txt3);
		if (m_bToString)
		{
			m_sText += temp + "\n";
		}
		std::cout << temp << std::endl;
	}

	void TextWriter::WriteLine(const string & txt, const string & txt1, const string & txt2, const string & txt3, const string & txt4)
	{
		string temp = string::Format(txt, txt1, txt2, txt3, txt4);
		if (m_bToString)
		{
			m_sText += temp + "\n";
		}
		std::cout << temp << std::endl;
	}

	string TextReader::ReadLine()
	{
		string input;
		if (m_bFromString)
		{
			if (m_aCurrentPos != m_aAllLines.end())
			{
				input = *m_aCurrentPos;
				m_aCurrentPos++;
			}
		}
		else
		{
			std::getline(std::cin, input);
		}
		return input;
	}
}
