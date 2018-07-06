#ifndef _LISP_H
#define _LISP_H

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

//using System;
//using System.Collections.Generic;

#include <iostream>

#include "cstypes.h"
#include "Parser.h"
#include "Interpreter.h"

namespace CsLisp
{
// TODO --> move into CsUtils.cpp
	/*public*/ static string DecorateWithBlock(string code, size_t & offset)
	{
		const string block = "(do ";
		offset = block.Length();
		return block + code + "\n)";
	}

	/// <summary>
	/// The FUEL lisp interpreter.
	/// </summary>
	/*public*/ class Lisp
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
		/*public*/ static std::shared_ptr<LispVariant> Eval(const string & lispCode, std::shared_ptr<LispScope> scope = 0/*= null*/, const string & moduleName = "test"/*= null*/, bool tracing = false/*, Dictionary<string, object> nativeItems = null*/, string * pRedirectToString = 0)
		{
			// first create global scope, needed for macro expanding
			var currentScope = scope==null ? LispEnvironment::CreateDefaultScope(pRedirectToString != 0) : scope;
			currentScope->ModuleName = moduleName;
			currentScope->Tracing = tracing;
			RegisterNativeObjects(/*nativeItems,*/ *currentScope);
			size_t offset = 0;
			string code = /*LispUtils.*/DecorateWithBlock(lispCode, /*out*/ offset);
			var ast = LispParser::Parse(code, offset, currentScope);
#ifdef ENABLE_COMPILE_TIME_MACROS 
			var expandedAst = std::make_shared<object>(*(LispInterpreter::ExpandMacros(std::make_shared<object>(*ast), currentScope)));
#else
			var expandedAst = std::make_shared<object>(*ast);
#endif
			var result = LispInterpreter::EvalAst(expandedAst, currentScope);
			if (pRedirectToString != 0)
			{
				*pRedirectToString = currentScope->Output.GetContent();
			}
			return result;
		}

		/// <summary>
		/// Evals the specified lisp code.
		/// All exceptions will be filtered and an error value will be returned.
		/// </summary>
		/// <param name="lispCode">The lisp code.</param>
		/// <param name="moduleName">The current module name.</param>
		/// <param name="verboseErrorOutput">if set to <c>true</c> [verbose error output].</param>
		/// <param name="tracing">if set to <c>true</c> [tracing].</param>
		/// <returns>The result</returns>
		/*public*/ static std::shared_ptr<LispVariant> SaveEval(const string & lispCode, const string & moduleName = /* null*/ "main", bool verboseErrorOutput = false, bool tracing = false, string * pRedirectToString = 0)
		{
			std::shared_ptr<LispVariant> result;
			try
			{
				result = Eval(lispCode, /*scope:*/ null, /*moduleName :*/ moduleName, /*tracing :*/ tracing, /*redirectToString*/ pRedirectToString);
			}
			catch (LispException exc)
			{
				/*Console.WriteLine*/ //std::cout << string::Format("\nError executing script.\n\n{0} --> line={1} start={2} stop={3} module={4}", exc.Message, exc.Data[LispUtils.LineNo], exc.Data[LispUtils.StartPos], exc.Data[LispUtils.StopPos], exc.Data[LispUtils.ModuleName]) << std::endl;
				string errMsg = string::Format("\nError executing script.\n\n{0} --> line={1} start={2} stop={3} module={4}", exc.Message/*, exc.Data["blub!"].ToString()*//*, exc.Data[LispUtils.LineNo], exc.Data[LispUtils.StartPos], exc.Data[LispUtils.StopPos], exc.Data[LispUtils.ModuleName]*/);
				std::cout << errMsg << std::endl;
				if (pRedirectToString != 0)
				{
					*pRedirectToString += errMsg + "\n";
				}
// TODO --> implement stack trace for exception
//				var stackInfo = exc.Data[LispUtils.StackInfo];
//				Console.WriteLine("\nCallstack:\n{0}", stackInfo != null ? stackInfo : "<not available>");                if (verboseErrorOutput)
//				{
//					Console.WriteLine("\nNative callstack:");
//					Console.WriteLine("Exception in eval(): {0} \ndata={1}", exc, exc.Data);
//				}
				result = LispVariant::CreateErrorValue(exc.Message);
			}
			return result;
		}

		//#endregion

	private:
		//#region private methods

		/*private*/ static void RegisterNativeObjects(/*Dictionary<string, object> nativeItems,*/ LispScope & currentScope)
		{
// TODO --> implement native objects
//			if (nativeItems != null)
//			{
//				foreach(KeyValuePair<string, object> item in nativeItems)
//				{
//					currentScope[item.Key] = new LispVariant(LispType.NativeObject, item.Value);
//				}
//			}
		}

		//#endregion
	};
}

#endif