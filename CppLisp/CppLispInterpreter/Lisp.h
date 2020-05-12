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

/*
*          Source Code     "(do (def a 42) (print a))"
*
*        ---------------
*        |  Tokenizer  |
*        ---------------
*
*        List<LispTokens>
*
*        ---------------
*        |    Parser   |
*        ---------------
*
*        AST = List<object> object=LispVariant|List<object>
* using
*        -----------------
*    --- | Expand Macros |
*    |   -----------------
*    |
*    |   AST = List<object> object=LispVariant|List<object>
*    |
*    |   --------------------           ---------------             --------
*    --> | Interpreter/Eval |   --->    | Environment |     --->    | .NET |
*        --------------------           ---------------             --------
*
*/

#ifndef _LISP_H
#define _LISP_H

//using System;
//using System.Collections.Generic;

#include <iostream>

#include "cstypes.h"
#include "Parser.h"
#include "Interpreter.h"

namespace CppLisp
{
	/// <summary>
	/// The FUEL lisp interpreter.
	/// </summary>
	/*public*/ class DLLEXPORT Lisp
	{
	public:
		//#region constants

		/*public*/ static const string ProgramName;

		/*public*/ static const string Name;
		/*public*/ static const string Version;
		/*public*/ static const string Date;
		/*public*/ static const string Copyright;

		/*public*/ static const string Platform;

		/*public*/ static const string License;
		/*public*/ static const string LicenseUrl;

		/*public*/ static const string Info;

		//#endregion

		//#region evaluation

		static string GetCompilerInfo();
		
		/// <summary>
		/// Evals the specified lisp code.
		/// An exception may occure if the lisp code is invalid.
		/// </summary>
		/// <param name="lispCode">The lisp code.</param>
		/// <param name="scope">The scope.</param>
		/// <param name="moduleName">The module name and path.</param>
		/// <param name="tracing">if set to <c>true</c> [tracing].</param>
		/// <param name="nativeItems">The dictionary with native items.</param>
		/// <returns>The result of the script evaluation</returns>
		/*public*/ static std::shared_ptr<LispVariant> Eval(const string & lispCode, std::shared_ptr<LispScope> scope = 0/*= null*/, const string & moduleName = "test"/*= null*/, bool tracing = false/*, Dictionary<string, object> nativeItems = null*/, std::shared_ptr<TextWriter> outp = null, std::shared_ptr<TextReader> inp = null, bool onlyMacroExpand = false);

		/// <summary>
		/// Evals the specified lisp code.
		/// All exceptions will be filtered and an error value will be returned.
		/// </summary>
		/// <param name="lispCode">The lisp code.</param>
		/// <param name="moduleName">The current module name.</param>
		/// <param name="verboseErrorOutput">if set to <c>true</c> [verbose error output].</param>
		/// <param name="tracing">if set to <c>true</c> [tracing].</param>
        /// <param name="onlyMacroExpand">if set to <c>true</c> [macro expanding].</param>
		/// <returns>The result</returns>
		/*public*/ static std::shared_ptr<LispVariant> SaveEval(const string & lispCode, const string & moduleName = /* null*/ "main", bool verboseErrorOutput = false, bool tracing = false, std::shared_ptr<TextWriter> outp = null, std::shared_ptr<TextReader> inp = null, bool onlyMacroExpand = false);

		//#endregion

	private:
		//#region private methods

		/*private*/ static void RegisterNativeObjects(/*Dictionary<string, object> nativeItems,*/ LispScope & /*currentScope*/);

		//#endregion
	};
}

#endif
