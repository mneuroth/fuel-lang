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

#include "cstypes.h"

#include <iostream>

namespace CppLisp
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

	void TextWriter::Flush() 
	{
		std::cout.flush();
	}

	TextWriter::TextWriter(bool bToString)
		: m_bToString(bToString)
	{
	}

	TextReader::TextReader(const string & txt)
	{
		m_bFromString = !string::IsNullOrEmpty(txt);
		SetContent(txt);
	}

	void TextReader::SetContent(const string & txt)
	{
		m_sText = txt;
		m_aAllLines = txt.Split("\n");
		m_aCurrentPos = m_aAllLines.begin();
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

	string LispFunctionWrapper::GetFormatedDoc() const
	{
		const string separator = "\n\n";
		const string splitter = "-------------------------------------------------" + separator;
		return GetFormatedHelpString(separator, splitter);
	}

	string LispFunctionWrapper::GetHtmlFormatedDoc() const
	{
		const string separator = "<br><br>";
		const string splitter = string("<hr>") + string("<br>");
		return GetFormatedHelpString(separator, splitter, [](const string & s) -> string { return "<b>" + s + "</b>"; }, [](const string & s) -> string { return "<code>" + s + "</code>"; });
	}

	string LispFunctionWrapper::GetFormatedHelpString(const string & separator, const string & splitter, std::function<string(const string &)> nameDecorator, std::function<string(const string &)> syntaxDecorator) const
	{
		if (nameDecorator == null)
		{
			nameDecorator = [](const string & s) -> string { return s; };
		}
		if (syntaxDecorator == null)
		{
			syntaxDecorator = [](const string & s) -> string { return s; };
		}
		string name = "???";
		string signature = (!string::IsNullOrEmpty(Signature) ? Signature : string::Empty);
		if (signature.size() > 0 && signature.StartsWith("("))
		{
			size_t len = signature.IndexOf(" ", "StringComparison.Ordinal");
			// process commands like: (doc)
			if (len == (size_t)-1)
			{
				len = signature.IndexOf(")", "StringComparison.Ordinal") - 1;
			}
			name = nameDecorator(signature.Substring(1, len));
		}
		name += IsSpecialForm() ? " [special form]" : string::Empty;
		name += separator;
		string syntax = syntaxDecorator("Syntax: " + signature) + separator;
		string doc = (!string::IsNullOrEmpty(Documentation) ? Documentation : "<not available>");
		doc += separator;
		return splitter + name + syntax + doc + "\n";
	}

}
