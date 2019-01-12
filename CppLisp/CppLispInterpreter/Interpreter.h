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

#ifndef _LISP_INTERPRETER_H
#define _LISP_INTERPRETER_H

//using System;
//using System.Collections.Generic;
//using System.Linq;

#include "csobject.h"
#include "cstypes.h"
#include "Scope.h"

namespace CppLisp
{
	// **********************************************************************
	class LispBreakpointPosition : public Tuple3<size_t, size_t, size_t>
	{
	public:
		/*public*/ LispBreakpointPosition(size_t start, size_t stop, size_t lineNumber)
			: Tuple3<size_t, size_t, size_t>(start, stop, lineNumber)
		{
		}
	};

	// **********************************************************************
	/// <summary>
    /// The FUEL lisp interpreter.
    /// </summary>
    /*public*/ class LispInterpreter
    {
	public:
        //#region public methods

        /// <summary>
        /// Resolves the items of the ast in the given scope.
        /// </summary>
        /// <param name="scope"></param>
        /// <param name="astAsList"></param>
        /// <param name="compile"></param>
        /// <returns></returns>
		/*public*/ static std::shared_ptr<IEnumerable<std::shared_ptr<object>>> ResolveArgsInScopes(std::shared_ptr<LispScope> scope, std::shared_ptr<IEnumerable<std::shared_ptr<object>>> astAsList, bool compile);

		/// <summary>
        /// Evaluates the given ast.
        /// </summary>
        /// <param name="ast">The ast.</param>
        /// <param name="scope">The scope.</param>
        /// <returns>The result of ast evaluation.</returns>
        /// <exception cref="System.Exception">Unexpected macro modus!</exception>
		/*public*/ static std::shared_ptr<LispVariant> EvalAst(std::shared_ptr<object> ast, std::shared_ptr<LispScope> scope);

#ifdef ENABLE_COMPILE_TIME_MACROS 

		/*public*/ static std::shared_ptr<object> ExpandMacros(std::shared_ptr<object> ast, std::shared_ptr<LispScope> globalScope);
		/*private*/ static std::shared_ptr<object> ExpandMacros(std::shared_ptr<object> ast, std::shared_ptr<LispScope> globalScope, /*ref*/ bool & anyMacroReplaced);

#endif

        //#endregion

	private:

        //#region private methods

        /// <summary>
        /// Get information about position in sourcecode for
        /// given item of the ast.
        /// </summary>
        /// <param name="item"></param>
        /// <returns></returns>
		/*private*/ static LispBreakpointPosition GetPosInfo(std::shared_ptr<object> item);

		/*private*/ static std::shared_ptr<IEnumerable<std::shared_ptr<object>>> ReplaceSymbolWithValueInExpression(const LispVariant & symbol, std::shared_ptr<object> symbolValue, std::shared_ptr<IEnumerable<std::shared_ptr<object>>> expression, bool macroArgsReplace, /*ref*/ bool & replacedAnything);
		/*private*/ static std::shared_ptr<IEnumerable<std::shared_ptr<object>>> ReplaceFormalArgumentsInExpression(std::shared_ptr<IEnumerable<std::shared_ptr<object>>> formalArguments, std::shared_ptr<IEnumerable<std::shared_ptr<object>>> astAsList, std::shared_ptr<IEnumerable<std::shared_ptr<object>>> expression, /*ref*/ bool & anyMacroReplaced);
		/*private*/ static bool IsSymbol(std::shared_ptr<object> elem);

        //#endregion
	};
}

#endif
