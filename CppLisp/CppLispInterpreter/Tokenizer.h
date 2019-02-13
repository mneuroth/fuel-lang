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

#ifndef _TOKENIZER_H
#define _TOKENIZER_H

#include "Token.h"
#include "cstypes.h"

namespace CppLisp
{
	/// <summary>
	/// The FUEL lisp tokenizer
	/// </summary>
	/*public*/ class DLLEXPORT LispTokenizer
	{
	public:
		//#region public static methods

		/// <summary>
		/// Tokenizes the specified code.
		/// </summary>
		/// <param name="code">The code.</param>
		/// <param name="offset">The position offset (decorated code).</param>
		/// <returns>Container with tokens</returns>
		/*public*/ static IEnumerable<std::shared_ptr<LispToken>> Tokenize(const string & code, size_t offset = 0);

		//#endregion

	private:
		//#region private static methods
	
		/*private*/ static char ProcessCharAfterBackslash(char ch);

		/*private*/ static size_t ProcessComment(const string & code, size_t i, size_t lineCount, char ch, /*Action<string, size_t, size_t>*/std::function<void(const string &, size_t, size_t)> addToken);

		/*private*/ static string GetRestOfLine(const string & code, size_t i, /*out*/ size_t & newIndex);

		//#endregion
	};
}

#endif
