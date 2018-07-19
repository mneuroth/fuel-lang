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

#ifndef _LISP_SCOPE_H
#define _LISP_SCOPE_H

 //using System;
//using System.Collections.Generic;
//using System.Linq;
//using System.IO;

#include "cstypes.h"
#include "csstring.h"
#include "csobject.h"
#include "Token.h"
#include "Variant.h"
#include "Environment.h"
#include "DebuggerInterface.h"

#include <algorithm>
#include <memory>
#include <list>

namespace CsLisp
{
    /// <summary>
    /// The lisp runtime scope. That is something like a stack item.
    /// </summary>
    /*public*/ class LispScope : public Dictionary<string, std::shared_ptr<object>>, public std::enable_shared_from_this<LispScope>
    {
	private:

		// disable assignment operator
		LispScope & operator=(const LispScope & other);

	public:
        //#region debugging support

        /// <summary>
        /// Gets and sets the debuging modus.
        /// </summary>
		/*public*/ ILispDebugger * Debugger; // { get; set; }

        /// <summary>
        /// Gets and sets the tracing modus.
        /// </summary>
		/*public*/ bool Tracing; // { get; set; }

        /// <summary>
        /// Gets and sets all tokens of the current script,
        /// used for debugging purpose and for showing the 
        /// position of an error.
        /// </summary>
		/*public*/ IEnumerable<std::shared_ptr<LispToken>> Tokens; // { get; set; }

        /// <summary>
        /// Gets and sets the next and previous scope,
        /// used for debugging purpose to show the 
        /// call stack
        /// </summary>
		/*public*/ std::shared_ptr<LispScope> Next; // { get; private set; }
		/*public*/ std::shared_ptr<LispScope> Previous; // { get; set; }

        /// <summary>
        /// Gets or sets the scope chain to implement closures.
        /// </summary>
        /// <value>
        /// The closure chain.
        /// </value>
		/*public*/ std::shared_ptr<LispScope> ClosureChain; // { get; set; }

        /// <summary>
        /// Gets or sets the current module name and path.
        /// </summary>
        /// <value>
        /// The module name and path.
        /// </value>       
		/*public*/ string ModuleName; // { get; set; }

		/// <summary>
		/// Gets or sets the flat which indicates that an eval function is executed.
		/// This is needed for debugging.
		/// </summary>
		/*public*/ bool IsInEval; // { get; set; }

        /// <summary>
        /// Gets or sets the current token.
        /// </summary>
		/*public*/ std::shared_ptr<LispToken> CurrentToken; // { get; set; }

        /*public*/ inline size_t CurrentLineNo() const
        {
            //get
            //{
                return CurrentToken ? CurrentToken->LineNo : -1;
            //}
        }

        /// <summary>
        /// Gets or sets user data.
        /// Needed for debugging support --> set function name to LispScope
        /// </summary>
        /// <value> The user data. </value>
		/*public*/ std::shared_ptr<object> UserData; // { get; set; }

        /// <summary>
        /// Gets or sets the user documentation information.
        /// </summary>
        /// <value>The user documentation.</value>
		/*public*/ std::shared_ptr<Tuple<string, string>> UserDoc; // { get; set; }

        //#endregion

        //#region properties

        /// <summary>
        /// Gets the name of this scope.
        /// </summary>
        /// <value> The name. </value>
		/*public*/ string Name; // { get; private set; }

        /// <summary>
        /// Gets the global scope.
        /// </summary>
        /// <value> The global scope. </value>
		/*public*/ std::shared_ptr<LispScope> GlobalScope; // { get; private set; }

        /// <summary>
        /// Gets the output stream.
        /// </summary>
        /// <value>
        /// The output.
        /// </value>
		/*public*/ std::shared_ptr<TextWriter> Output; // { get; set; }

        /// <summary>
        /// Gets the input stream.
        /// </summary>
        /// <value>
        /// The input.
        /// </value>
		/*public*/ std::shared_ptr<TextReader> Input; // { get; set; }

        //#endregion

        //#region constructor

        /// <summary>
        /// Initializes a new instance of the <see cref="LispScope"/> class.
        /// </summary>
        /// <param name="fcnName">Name of the FCN.</param>
        /// <param name="globalScope">The global scope.</param>
        /// <param name="moduleName">The current module name for the scope.</param>
		/*public*/ LispScope(string fcnName = string::Empty, std::shared_ptr<LispScope> globalScope = null, std::shared_ptr<string> moduleName = null, std::shared_ptr<TextWriter> outp = null, std::shared_ptr<TextReader> inp = null);

		inline void PrivateInitForCpp(std::shared_ptr<LispScope> globalScope = null)
		{
			GlobalScope = globalScope != null ? globalScope : shared_from_this();
		}

        //#endregion

        //#region public methods

        /*public*/ inline void PushNextScope(std::shared_ptr<LispScope> nextScope)
        {
            Next = nextScope;
            nextScope->Previous = shared_from_this();
        }

        /*public*/ inline void PopNextScope()
        {
            Next->Previous = null;
            Next = null;
        }

        /// <summary>
        /// Determines whether the given name is available in the closure chain.
        /// </summary>
        /// <param name="name">The name.</param>
        /// <param name="closureScopeFound">The closure scope found.</param>
        /// <returns>True if name was found.</returns>
		/*public*/ bool IsInClosureChain(string name, /*out*/ std::shared_ptr<LispScope> & closureScopeFound);

        /// <summary>
        /// Resolves the given element in this scope.
        /// </summary>
        /// <param name="elem">The element.</param>
        /// <returns>Resolved value or null</returns>
		/*public*/ std::shared_ptr<object> ResolveInScopes(std::shared_ptr<object> elem);

        /// <summary>
        /// Searches the given symbol in the scope environment and 
        /// sets the value if found.
        /// Throws an exception if the symbol is not found in the scope 
        /// environment.
        /// </summary>
        /// <param name="symbolName">Name of the symbol.</param>
        /// <param name="value">The value.</param>
        /// <exception cref="LispException">Symbol  + symbolName +  not found</exception>
		/*public*/ void SetInScopes(const string & symbolName, std::shared_ptr<object> value);

		/*public*/ std::shared_ptr<LispToken> GetPreviousToken(std::shared_ptr<LispToken> token);

		/*public*/ int GetCallStackSize() const;

		/*public*/ void DumpStack(int currentLevel = -1);

		/*public*/ string DumpStackToString(int currentLevel = -1);

        /*public*/ inline void DumpVars()
        {
			Dump([](const LispVariant & v) -> bool { return !v.IsFunction() || (v.IsFunction() && !v.FunctionValue().IsBuiltin()); });
        }

		/*public*/ void DumpFunctions();

        /*public*/ inline void DumpMacros()
        {
			ProcessMetaScope(LispEnvironment::Macros, [this](KeyValuePair<string, std::shared_ptr<object>> macro) -> void { Output->WriteLine(macro.Key); });
        }

        /*public*/ inline void DumpBuiltinFunctions()
        {
			Dump([](const LispVariant & v) -> bool { return v.IsFunction() && v.FunctionValue().IsBuiltin(); });
        }

        /*public*/ inline void DumpBuiltinFunctionsHelp()
        {
			Dump([](const LispVariant & v) -> bool { return v.IsFunction() && v.FunctionValue().IsBuiltin(); }, [](const LispVariant & v) -> string { return v.FunctionValue().Documentation; }, /*showHelp:*/ true);
        }

        /*public*/ inline void DumpBuiltinFunctionsHelpFormated()
        {
			Dump([](const LispVariant & v) -> bool { return v.IsFunction() && v.FunctionValue().IsBuiltin(); }, null, /*showHelp:*/false, /*sort:*/ true, /*format:*/ [](const LispVariant & v) -> string { return v.FunctionValue().GetFormatedDoc(); });
        }

		/*public*/ void DumpBuiltinFunctionsHelpHtmlFormated();

        /*public*/ inline void DumpModules()
        {
			ProcessMetaScope(LispEnvironment::Modules, [this](KeyValuePair<string, std::shared_ptr<object>> mod) -> void { return Output->WriteLine(mod.Key); });
        }

		/*public*/ string GetFunctionsHelpFormated(const string & functionName, /*Func<string, string, bool>*/std::function<bool(string, string)> select = null);

        //#endregion
	
	private:
        //#region private methods

		/*private*/ void ProcessMetaScope(string metaScope, /*Action<KeyValuePair<string, std::shared_ptr<object>>>*/std::function<void(KeyValuePair<string, std::shared_ptr<object>>)> action);

		/*private*/ void Dump(std::function<bool(const LispVariant &)>/*Func<LispVariant, bool>*/ select, std::function<string(const LispVariant &)>/*Func<LispVariant, string>*/ show = null, bool showHelp = false, bool sort = false, std::function<string(const LispVariant &)>/*Func<LispVariant, string>*/ format = null);

        //#endregion
	};
}

#endif
