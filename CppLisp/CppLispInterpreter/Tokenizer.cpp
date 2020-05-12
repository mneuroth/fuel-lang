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

#include "Tokenizer.h"
#include "Exception.h"

namespace CppLisp
{
	IEnumerable<std::shared_ptr<LispToken>> LispTokenizer::Tokenize(const string & code, size_t offset)
	{
		IEnumerable<std::shared_ptr<LispToken>> tokens; // = new List<LispToken>();
		string currentToken = string::Empty;
		size_t currentTokenStartPos = 0;
		size_t lineCount = 1;
		bool isInString = false;
		bool isInSymbol = false;
		bool wasLastBackslash = false;

		/*Action<string, int, int>*/
		std::function<void(const string &, size_t, size_t)> addToken = [&tokens, offset, &isInSymbol, &isInString, &currentToken, &currentTokenStartPos](const string & currentTok, size_t pos, size_t line)
		{
			tokens.Add(std::make_shared<LispToken>(currentTok, currentTokenStartPos - offset, pos - offset, line));
			isInSymbol = false;
			isInString = false;
			currentToken = string::Empty;
			currentTokenStartPos = pos + 1;
		};

		for (size_t i = 0; i < code.Length(); i++)
		{
			char ch = code[i];
			if (Char_IsWhiteSpace(ch))
			{
				if (isInString)
				{
					currentToken += ch;
				}
				else if (isInSymbol)
				{
					addToken(currentToken, i, lineCount);
				}
				wasLastBackslash = false;
				if (ch == '\n')
				{
					lineCount++;
				}
			}
			else if (ch == '\\')
			{
				if (wasLastBackslash)
				{
					currentToken += ch;
					wasLastBackslash = false;
				}
				else
				{
					wasLastBackslash = true;
				}
			}
			else if (ch == '(' || ch == ')')
			{
				if (isInString)
				{
					currentToken += ch;
				}
				else if (isInSymbol)
				{
					addToken(currentToken, i, lineCount);
					addToken(string::Empty + string(ch), i, lineCount);
				}
				else
				{
					addToken(string::Empty + string(ch), i, lineCount);
				}
				wasLastBackslash = false;
			}
			else if (ch == ';')
			{
				if (isInString)
				{
					currentToken += ch;
				}
				else if (isInSymbol)
				{
					addToken(currentToken, i, lineCount);
					i = ProcessComment(code, i, lineCount, ch, addToken);
				}
				else
				{
					i = ProcessComment(code, i, lineCount, ch, addToken);
				}
				wasLastBackslash = false;
				// comment ends always with new line
				lineCount++;
			}
			else if (ch == '\'' || ch == '`' || ch == ',')
			{
				if (isInString)
				{
					currentToken += ch;
				}
				else
				{
					if (code[i + 1] == '@')
					{
						// process unquotesplicing
						string s = string::Empty;
						s += ch;
						i++;
						s += code[i];
						addToken(s, i, lineCount);
					}
					else
					{
						addToken(string::Empty + string(ch), i, lineCount);
					}
				}
				wasLastBackslash = false;
			}
			else if (ch == '"')
			{
				if (wasLastBackslash)
				{
					currentToken += ch;
				}
				else if (isInString)
				{
					// finish string
					addToken(string("\"") + currentToken + string("\""), i, lineCount);
				}
				else
				{
					// start string
					isInString = true;
					currentToken = string::Empty;
				}
				wasLastBackslash = false;
			}
			else
			{
				if (!isInSymbol && !isInString)
				{
					isInSymbol = true;
				}
				if (wasLastBackslash)
				{
					ch = ProcessCharAfterBackslash(ch);
				}
				currentToken += ch;
				wasLastBackslash = false;
			}
		}
		if (currentToken != string::Empty)
		{
			addToken(currentToken, -1, lineCount);
		}
		return tokens;
	}

	char LispTokenizer::ProcessCharAfterBackslash(char ch)
	{
		switch (ch)
		{
		case 'n':
			return '\n';
		case 'r':
			return '\r';
		case 't':
			return '\t';
		case '\\':
			return '\\';
		}
		throw LispExceptionBase(string::Format(string("Invalid character after backslash {0}"), string(ch)));
	}

	size_t LispTokenizer::ProcessComment(const string & code, size_t i, size_t lineCount, char ch, /*Action<string, size_t, size_t>*/std::function<void(const string &, size_t, size_t)> addToken)
	{
		size_t newIndex;
		string comment = string::Empty + string(ch) + GetRestOfLine(code, i + 1, /*out*/ newIndex);
		addToken(comment, i, lineCount);
		i = newIndex;
		return i;
	}

	string LispTokenizer::GetRestOfLine(const string & code, size_t i, /*out*/ size_t & newIndex)
	{
		string rest = code.Substring(i);
		size_t pos = rest.IndexOf("\n", "StringComparison.InvariantCulture");
		if (pos > 0)
		{
			newIndex = i + pos;
			return rest.Substring(0, pos + 1);
		}
		newIndex = i + rest.Length() - 1;
		return rest;
	}
}