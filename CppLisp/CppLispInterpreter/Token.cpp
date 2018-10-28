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

#include "Token.h"
#include <string>

const CppLisp::string CppLisp::LispToken::StringStart = "\"";
const CppLisp::string CppLisp::LispToken::QuoteConst = "'";
const CppLisp::string CppLisp::LispToken::Quasiquote = "`";
const CppLisp::string CppLisp::LispToken::Unquote = ",";
const CppLisp::string CppLisp::LispToken::Unquotesplicing = ",@";
const CppLisp::string CppLisp::LispToken::NilConst = "NIL";

const CppLisp::string CppLisp::string::Empty = "";

namespace CppLisp
{
	bool Int32_TryParse(const string & txt, size_t & outValue)
	{
		try
		{
			size_t errPos;
			outValue = std::stoi(std::string(txt), &errPos, 10);
			if (errPos < txt.length())
			{
				return false;
			}
			return true;
		}
		catch (const std::invalid_argument &)
		{
			return false;
		}
	}

	bool Double_TryParse(const string & txt, const string & /*NumberStyles_Any*/, const string & /*CultureInfo_InvariantCulture*/, double & doubleValue)
	{
		try
		{
			size_t errPos;
			doubleValue = std::stod(txt, &errPos);
			if (errPos < txt.length())
			{
				return false;
			}
			return true;
		}
		catch (const std::invalid_argument &)
		{
			return false;
		}
	}

	LispToken::LispToken(const string & text, size_t start, size_t stop, size_t lineNo)
	{
		size_t intValue;
		double doubleValue;

		StartPos = start;
		StopPos = stop;
		LineNo = lineNo;
		Value = std::make_shared<object>(text);

		if (text.StartsWith(StringStart))
		{
			Type = /*LispTokenType::*/String;
			Value = std::make_shared<object>(text.Substring(1, text.Length() - 2));
		}
		else if (text == QuoteConst)
		{
			Type = /*LispTokenType::*/Quote;
		}
		else if (text == Quasiquote)
		{
			Type = /*LispTokenType::*/QuasiQuote;
		}
		else if (text == Unquote)
		{
			Type = /*LispTokenType::*/UnQuote;
		}
		else if (text == Unquotesplicing)
		{
			Type = /*LispTokenType::*/UnQuoteSplicing;
		}
		else if (text == "(")
		{
			Type = /*LispTokenType::*/ListStart;
		}
		else if (text == ")")
		{
			Type = /*LispTokenType::*/ListEnd;
		}
		else if (Int32_TryParse(text, /*out*/ intValue))
		{
			Type = /*LispTokenType::*/Int;
			Value = std::make_shared<object>((int)intValue);
		}
		else if (Double_TryParse(text, "NumberStyles.Any", "CultureInfo.InvariantCulture", /*out*/ doubleValue))
		{
			Type = /*LispTokenType::*/Double;
			Value = std::make_shared<object>(doubleValue);
		}
		else if (text.Equals("true") || text.Equals("#t"))
		{
			Type = /*LispTokenType::*/True;
			Value = std::make_shared<object>(true);
		}
		else if (text.Equals("false") || text.Equals("#f"))
		{
			Type = /*LispTokenType::*/False;
			Value = std::make_shared<object>(false);
		}
		else if (text.ToUpper().Equals(NilConst))
		{
			Type = /*LispTokenType::*/Nil;
			Value = std::make_shared<object>(null);
		}
		else if (text.StartsWith(";"))
		{
			Type = /*LispTokenType::*/Comment;
			Value = std::make_shared<object>(text);
		}
		else
		{
			Type = /*LispTokenType::*/Symbol;
			Value = std::make_shared<object>(text);
		}
	}

	bool LispToken::operator ==(const LispToken & other) const
	{
		bool isEqual = Type == other.Type &&
			ToString() == other.ToString() &&
			StartPos == other.StartPos &&
			StopPos == other.StopPos &&
			LineNo == other.LineNo;
		return isEqual;
	}

	string LispToken::ToString() const
	{
		if (Type == /*LispTokenType::*/Nil)
		{
			return NilConst;
		}
		return Value->ToString();
	}
}
