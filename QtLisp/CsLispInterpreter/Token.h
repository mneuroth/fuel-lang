#ifndef _TOKEN_H
#define _TOKEN_H

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

#include <string>
#include <cctype>
#include <functional>
#include <algorithm>
#include <stdexcept>

#define null 0

namespace CsLisp
{
	class LispToken;
	class object;

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

		string& operator+(const string & other)
		{
			append(other);
			return *this;
		}

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

	inline CsLisp::string operator+(const CsLisp::string & s1, const CsLisp::string & s2)
	{
		CsLisp::string s = s1;
		s.append(s2);
		return s;
	}

	// variant object
	class object
	{
	private:
		string m_sValue;

	public:
		object()
			: m_sValue("?")
		{
		}

		object& operator=(const string & other)
		{
			m_sValue = other;
			return *this;
		}

		object& operator=(int other)
		{
			m_sValue = std::to_string(other);
			return *this;
		}

		object& operator=(double other)
		{
			m_sValue = std::to_string(other);
			return *this;
		}

		operator int()
		{
			return stoi(m_sValue);
		}

		operator double()
		{
			return stod(m_sValue);
		}

		operator bool()
		{
			return stoi(m_sValue) == 1;
		}

		string ToString() const
		{
			return m_sValue;
		}
	};

	inline bool Int32_TryParse(const string & txt, int & outValue)
	{
		try
		{
			size_t errPos;
			outValue = std::stoi(txt, &errPos);
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

	inline bool Double_TryParse(const string & txt, const string & NumberStyles_Any, const string & CultureInfo_InvariantCulture, double & doubleValue)
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

	inline bool Char_IsWhiteSpace(char ch)
	{
		return isspace(ch) != 0;
	}

	/// <summary>
	/// Type for lisp tokens.
	/// </summary>
	enum LispTokenType
	{
		ListStart = 0,
		ListEnd = 1,
		Symbol = 2,
		String = 3,
		Int = 4,
		Double = 5,
		Quote = 6,
		QuasiQuote = 7,
		UnQuote = 8,
		UnQuoteSplicing = 9,
		True = 10,
		False = 11,
		Comment = 12,
		Nil = 13,
	};

	/// <summary>
	/// Interface for a lisp token.
	/// </summary>
	/// <remark>
	/// This interface is intended to remove the dependency to the LispToken class in the future.
	/// </remark>
	/*public interface*/class ILispTokenInterface
	{
	public:
		///// <summary>
		///// Gets the type of the token.
		///// </summary>
		///// <value>
		///// The type.
		///// </value>
		//LispTokenType Type; //{ get; }

		///// <summary>
		///// Gets the value.
		///// </summary>
		///// <value>
		///// The value.
		///// </value>
		//object Value; // { get; }

		///// <summary>
		///// Gets the start position of the token.
		///// </summary>
		///// <value>
		///// The start position.
		///// </value>
		//int StartPos; // { get; }
	};

	/// <summary>
	/// Lisp token.
	/// </summary>
	/*public*/ class LispToken : public ILispTokenInterface
	{
	public:
		//#region constants

		/*public*/ const static string StringStart;
		/*public*/ const static string QuoteConst;
		/*public*/ const static string Quasiquote;
		/*public*/ const static string Unquote;
		/*public*/ const static string Unquotesplicing;
		/*public*/ const static string NilConst;

		//#endregion

		//#region properties

		/// <summary>
		/// Gets the type of the token.
		/// </summary>
		/// <value>
		/// The type.
		/// </value>
		/*public*/ LispTokenType Type; // { get; private set; }

		/// <summary>
		/// Gets the value.
		/// </summary>
		/// <value>
		/// The value.
		/// </value>
		/*public*/ object Value; // { get; private set; }

		/// <summary>
		/// Gets or sets the start position of the token.
		/// </summary>
		/// <value>
		/// The start position.
		/// </value>
		/*public*/ int StartPos; // { get; set; }

		/// <summary>
		/// Gets or sets the stop position of the token.
		/// </summary>
		/// <value>
		/// The stop position.
		/// </value>
		/*public*/ int StopPos; // { get; set; }

		/// <summary>
		/// Gets or sets the line no of the token.
		/// </summary>
		/// <value>
		/// The line no.
		/// </value>
		/*public*/ int LineNo; // { get; set; }

		//#endregion

		//#region constructor

		/// <summary>
		/// Initializes a new instance of the <see cref="LispToken"/> class.
		/// </summary>
		/// <param name="text">The text of the token.</param>
		/// <param name="start">The start position.</param>
		/// <param name="stop">The stop position.</param>
		/// <param name="lineNo">The line no.</param>
		/*public*/ LispToken(string text, int start, int stop, int lineNo)
		{
			int intValue;
			double doubleValue;

			StartPos = start;
			StopPos = stop;
			LineNo = lineNo;
			Value = text;

			if (text.StartsWith(StringStart))
			{
				Type = /*LispTokenType::*/String;
				Value = text.Substring(1, text.Length() - 2);
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
				Value = intValue;
			}
			else if (Double_TryParse(text, "NumberStyles.Any", "CultureInfo.InvariantCulture", /*out*/ doubleValue))
			{
				Type = /*LispTokenType::*/Double;
				Value = doubleValue;
			}
			else if (text.Equals("true") || text.Equals("#t"))
			{
				Type = /*LispTokenType::*/True;
				Value = true;
			}
			else if (text.Equals("false") || text.Equals("#f"))
			{
				Type = /*LispTokenType::*/False;
				Value = false;
			}
			else if (text.ToUpper().Equals(NilConst))
			{
				Type = /*LispTokenType::*/Nil;
				Value = null;
			}
			else if (text.StartsWith(";"))
			{
				Type = /*LispTokenType::*/Comment;
				Value = text;
			}
			else
			{
				Type = /*LispTokenType::*/Symbol;
				Value = text;
			}
		}

		//#endregion

		//#region public methods

		/// <summary>
		/// Returns a <see cref="System.String" /> that represents this instance.
		/// </summary>
		/// <returns>
		/// A <see cref="System.String" /> that represents this instance.
		/// </returns>
		/*public override*/ string ToString() const
		{
			if (Type == /*LispTokenType::*/Nil)
			{
				return NilConst;
			}
			return Value.ToString();
		}

		//#endregion
	};
}

#endif
