#ifndef _ENVIRONMENT_H
#define _ENVIRONMENT_H

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

//using System;
//using System.Collections.Generic;
//using System.IO;
//using System.Linq;
//using System.Reflection;
//using System.Text;

#include "cstypes.h"
#include "csstring.h"
#include "csobject.h"

extern std::string ReadFileOrEmptyString(const std::string & fileName);

namespace CsLisp
{
	class LispScope;
	class LispBreakpointPosition;

	extern string LispUtils_LibraryPath;

	// **********************************************************************
	/// <summary>
	/// Class to hold informations about macro expansions at compile time.
	/// </summary>
	class LispMacroCompileTimeExpand
	{
	public:
		LispMacroCompileTimeExpand(std::shared_ptr<IEnumerable<std::shared_ptr<object>>> parameters, std::shared_ptr<IEnumerable<std::shared_ptr<object>>> expression)
		{
			FormalArguments = parameters;
			Expression = expression;
		}

		std::shared_ptr<IEnumerable<std::shared_ptr<object>>> FormalArguments;

		std::shared_ptr<IEnumerable<std::shared_ptr<object>>> Expression;
	};

	// **********************************************************************
	/// <summary>
	/// Class to hold informations about macro expansions at run time.
	/// </summary>
	class LispMacroRuntimeEvaluate : public LispMacroCompileTimeExpand
	{
	public:
		LispMacroRuntimeEvaluate(std::shared_ptr<IEnumerable<std::shared_ptr<object>>> parameters, std::shared_ptr<IEnumerable<std::shared_ptr<object>>> expression)
			: LispMacroCompileTimeExpand(parameters, expression)
		{
		}
	};

	// **********************************************************************
	/// <summary>
	/// The runtime environment for the FUEL lisp interpreter.
	/// </summary>
	class LispEnvironment
	{
	public:
		// constants
		const static string MetaTag;
		const static string EvalStrTag;

		const static string Macros;
		const static string Modules;

		const static string Apply;
		const static string Eval;
		const static string EvalStr;
		const static string Quote;
		const static string Quasiquote;

		const static string Sym;
		const static string Str;

		// methods
		static bool IsInModules(const string & funcName, std::shared_ptr<LispScope> scope);
		static bool IsMacro(std::shared_ptr<object> funcName, std::shared_ptr<LispScope> scope);
		static bool IsExpression(std::shared_ptr<object> item);
		static bool FindFunctionInModules(const string & funcName, std::shared_ptr<LispScope> scope, std::shared_ptr<object> & foundValue);

		static std::shared_ptr<LispScope> CreateDefaultScope();

		static std::shared_ptr<object> QueryItem(std::shared_ptr<object> funcName, std::shared_ptr<LispScope> scope, string key);
		static std::shared_ptr<object> GetFunctionInModules(const string & funcName, std::shared_ptr<LispScope> scope);
		static std::shared_ptr<object> GetMacro(std::shared_ptr<object> funcName, std::shared_ptr<LispScope> scope);
		static std::shared_ptr<IEnumerable<std::shared_ptr<object>>> GetExpression(std::shared_ptr<object> item);
		static std::shared_ptr<IEnumerable<std::shared_ptr<object>>> CheckForList(const string & functionName, std::shared_ptr<object> listObj, std::shared_ptr<LispScope> scope);
	};
}

#endif