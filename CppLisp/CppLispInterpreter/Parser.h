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

#ifndef _LISP_PARSER_H
#define _LISP_PARSER_H

#include "Token.h"
#include "Scope.h"

namespace CppLisp
{
	/// <summary>
	/// The FUEL lisp parser.
	/// </summary>
    /*public*/ class LispParser
	{
		//#region constants
			
		const static string BracketsOutOfBalance;
		const static string BracketsOutOfBalanceOrUnexpectedScriptCode;
		const static string UnexpectedToken;

		//#endregion

	public:
		//#region public static methods
	
		/// <summary>
		/// Parses the specified code.
		/// string ==&gt; List(Tokens) ==&gt; List(object) mit object == List(object), LispVariant(string, int, double, and ==&gt; for unquoting Symbols)
		/// </summary>
		/// <param name="code">The code.</param>
		/// <param name="offset">The position offset.</param>
		/// <param name="scope">The scope.</param>
		/// <returns>Abstract syntax tree as container</returns>
		/*public*/ static std::shared_ptr<object> Parse(string code, size_t offset = 0, std::shared_ptr<LispScope> scope = null);

		//#endregion

	private:
		//#region private methods

		/*private*/ static size_t ParseTokens(string moduleName, std::vector<std::shared_ptr<LispToken>> tokens, size_t startIndex, /*ref*/ std::shared_ptr<object> & parseResult, bool isToplevel);

		/*private*/ static bool OnlyCommentTokensFrom(const std::vector<std::shared_ptr<LispToken>> & tokens, size_t i);

		//#endregion
	};
}

#endif
