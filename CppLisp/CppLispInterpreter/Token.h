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

#ifndef _TOKEN_H
#define _TOKEN_H

#include "csobject.h"
#include "cstypes.h"
#include "csstring.h"

namespace CsLisp
{
	class LispToken;

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

	bool Int32_TryParse(const string & txt, size_t & outValue);

	bool Double_TryParse(const string & txt, const string & /*NumberStyles_Any*/, const string & /*CultureInfo_InvariantCulture*/, double & doubleValue);

	inline bool Char_IsWhiteSpace(char ch)
	{
		return isspace(ch) != 0;
	}

	// **********************************************************************
	/// <summary>
	/// Lisp token.
	/// </summary>
	/*public*/ class LispToken
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
		/*public*/ std::shared_ptr<object> Value; // { get; private set; }

		/// <summary>
		/// Gets or sets the start position of the token.
		/// </summary>
		/// <value>
		/// The start position.
		/// </value>
		/*public*/ size_t StartPos; // { get; set; }

		/// <summary>
		/// Gets or sets the stop position of the token.
		/// </summary>
		/// <value>
		/// The stop position.
		/// </value>
		/*public*/ size_t StopPos; // { get; set; }

		/// <summary>
		/// Gets or sets the line no of the token.
		/// </summary>
		/// <value>
		/// The line no.
		/// </value>
		/*public*/ size_t LineNo; // { get; set; }

		//#endregion

		//#region constructor

		/// <summary>
		/// Initializes a new instance of the <see cref="LispToken"/> class.
		/// </summary>
		/// <param name="text">The text of the token.</param>
		/// <param name="start">The start position.</param>
		/// <param name="stop">The stop position.</param>
		/// <param name="lineNo">The line no.</param>
		/*public*/ LispToken(string text, size_t start, size_t stop, size_t lineNo);

		//#endregion

		bool operator ==(const LispToken & other) const;

		//#region public methods

		/// <summary>
		/// Returns a <see cref="System.String" /> that represents this instance.
		/// </summary>
		/// <returns>
		/// A <see cref="System.String" /> that represents this instance.
		/// </returns>
		/*public override*/ string ToString() const;

		//#endregion
	};
}

#endif
