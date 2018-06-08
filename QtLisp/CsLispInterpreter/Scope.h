#ifndef _LISP_SCOPE_H
#define _LISP_SCOPE_H

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
//using System.Linq;
//using System.IO;

#include "cstypes.h"
#include "csstring.h"
#include "csobject.h"
#include "Token.h"
#include "Variant.h"

#include <memory>
#include <list>

namespace CsLisp
{
	class LispScope;
	class LispBreakpointPosition;

	class ILispDebugger
	{
	public:
		void InteractiveLoop(LispScope & initialTopScope /*= null*/, std::shared_ptr<IEnumerable<std::shared_ptr<object>>> currentAst = null, bool startedFromMain = false, bool tracing = false)
		{
// TODO: only dummy impl !
		}

		bool NeedsBreak(LispScope & scope, LispBreakpointPosition & posInfosOfCurrentAstItem)
		{
// TODO: only dummy impl !
			return false;
		}
	};

	class LispEnvironment
	{
	public:
		static bool IsInModules(const string & name, std::shared_ptr<LispScope> globalScope)
		{
// TODO --> IsInModules realisieren
			return false;
		}

		/*public*/ static bool IsMacro(std::shared_ptr<object> funcName, std::shared_ptr<LispScope> scope)
		{
			return false; // ExistsItem(funcName, scope, Macros);
		}

		/*public*/ static bool IsExpression(std::shared_ptr<object> item)
		{
			return (item->IsLispVariant() /*is LispVariant*/ && item->IsList()) ||
				(item->IsIEnumerableOfObject());
		}

		/*public*/ static std::shared_ptr<IEnumerable<std::shared_ptr<object>>> GetExpression(std::shared_ptr<object> item)
		{
			if (item->IsLispVariant() /*is LispVariant*/ && item->IsList()/*((LispVariant)item).IsList*/)
			{
				return item->ToLispVariant()->ListValue(); // ((LispVariant)item).ListValue;
			}
			if (item->IsIEnumerableOfObject() /*is IEnumerable<object>*/)
			{
				return item->ToList(); // (IEnumerable<object>)item;
			}
			return std::make_shared<IEnumerable<std::shared_ptr<object>>>(); // new List<object>();
		}

		static std::shared_ptr<object>  GetFunctionInModules(const string & name, std::shared_ptr<LispScope> globalScope)
		{
// TODO --> GetFunctionInModules realisieren
			return null;
		}

		/*public*/ static std::shared_ptr<object> GetMacro(std::shared_ptr<object> funcName, std::shared_ptr<LispScope> scope)
		{
			return QueryItem(funcName, scope, Macros);
		}

		/*private*/ static std::shared_ptr<object> QueryItem(std::shared_ptr<object> funcName, std::shared_ptr<LispScope> scope, string key);


		static string Macros;
		static string Modules;
		static string MetaTag;

		const static string Quote;
		const static string Quasiquote;
	};

    /// <summary>
    /// The lisp runtime scope. That is something like a stack item.
    /// </summary>
    /*public*/ class LispScope : public Dictionary<string, std::shared_ptr<object>>, public std::enable_shared_from_this<LispScope>
    {
	public:
        //#region debugging support

        /// <summary>
        /// Gets and sets the debuging modus.
        /// </summary>
		/*public*/ std::shared_ptr<ILispDebugger> Debugger; // { get; set; }

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
        /// Gets or sets the current token.
        /// </summary>
		/*public*/ std::shared_ptr<LispToken> CurrentToken; // { get; set; }

        /*public*/ int CurrentLineNo() const
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
		/*public*/ object UserData; // { get; set; }

        /// <summary>
        /// Gets or sets the user documentation information.
        /// </summary>
        /// <value>The user documentation.</value>
		/*public*/ Tuple<string, string> UserDoc; // { get; set; }

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
		/*public*/ TextWriter Output; // { get; set; }

        /// <summary>
        /// Gets the input stream.
        /// </summary>
        /// <value>
        /// The input.
        /// </value>
		/*public*/ TextReader Input; // { get; set; }

        //#endregion

        //#region constructor

        /// <summary>
        /// Initializes a new instance of the <see cref="LispScope"/> class.
        /// </summary>
        /// <param name="fcnName">Name of the FCN.</param>
        /// <param name="globalScope">The global scope.</param>
        /// <param name="moduleName">The current module name for the scope.</param>
        /*public*/ LispScope(string fcnName, std::shared_ptr<LispScope> globalScope = null, std::shared_ptr<string> moduleName = null)
        {
            Name = fcnName;
			ModuleName = moduleName ? *moduleName : string::Empty;
			//GlobalScope = globalScope != null ? globalScope : shared_from_this();
			if (ModuleName == string::Empty && globalScope != null)
            {
                ModuleName = globalScope->ModuleName;
            }
            CurrentToken = null;
// TODO --> Console.In/Out umleiten realisieren !
//            Input = Console.In;
//            Output = Console.Out;
        }

        /// <summary>
        /// Initializes a new instance of the <see cref="LispScope"/> class.
        /// </summary>
        /// <remarks>Needed for compiler module and .NET 3.5</remarks>
        /*public*/ LispScope()
            : LispScope(string::Empty)
        {
        }

		void PrivateInitForCpp(std::shared_ptr<LispScope> globalScope = null)
		{
			GlobalScope = globalScope != null ? globalScope : shared_from_this();
		}

        //#endregion

        //#region public methods

        /*public*/ void PushNextScope(std::shared_ptr<LispScope> nextScope)
        {
            Next = nextScope;
            nextScope->Previous = shared_from_this();
        }

        /*public*/ void PopNextScope()
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
        /*public*/ bool IsInClosureChain(string name, /*out*/ std::shared_ptr<LispScope> closureScopeFound)
        {
            closureScopeFound = null;
            if (ClosureChain != null)
            {
                if (ClosureChain->ContainsKey(name))
                {
                    closureScopeFound = ClosureChain;
                    return true;
                }
                return ClosureChain->IsInClosureChain(name, closureScopeFound);
            }
            return false;
        }

        /// <summary>
        /// Resolves the given element in this scope.
        /// </summary>
        /// <param name="elem">The element.</param>
        /// <returns>Resolved value or null</returns>
        /*public*/ std::shared_ptr<object> ResolveInScopes(std::shared_ptr<object> elem)
        {
			std::shared_ptr<object> result;

            var name = elem->ToString();
            std::shared_ptr<LispScope> foundClosureScope;
            // first try to resolve in this scope
            if (ContainsKey(name))
            {
                result = (*this)[name];
            }
            // then try to resolve in closure chain scope(s)
            else if (IsInClosureChain(name, foundClosureScope))
            {
                result = (*foundClosureScope)[name];
            }
            // then try to resolve in global scope
            else if (GlobalScope != null &&
                     GlobalScope->ContainsKey(name))
            {
                result = (*GlobalScope)[name];
            }
            // then try to resolve in scope of loaded modules
            else if (LispEnvironment::IsInModules(name, GlobalScope))
            {
                result = LispEnvironment::GetFunctionInModules(name, GlobalScope);
            }
            else
            {
                result = elem;
            }

            return result;
        }

        /// <summary>
        /// Searches the given symbol in the scope environment and 
        /// sets the value if found.
        /// Throws an exception if the symbol is not found in the scope 
        /// environment.
        /// </summary>
        /// <param name="symbolName">Name of the symbol.</param>
        /// <param name="value">The value.</param>
        /// <exception cref="LispException">Symbol  + symbolName +  not found</exception>
        /*public*/ void SetInScopes(string symbolName, std::shared_ptr<object> value)
        {
			std::shared_ptr<LispScope> foundClosureScope;
            if (!string::IsNullOrEmpty(symbolName) && ContainsKey(symbolName))
            {
                (*this)[symbolName] = value;
            }
            else if (!string::IsNullOrEmpty(symbolName) && IsInClosureChain(symbolName, foundClosureScope))
            {
                (*foundClosureScope)[symbolName] = value;
            }
            else if (!string::IsNullOrEmpty(symbolName) && GlobalScope != null && GlobalScope->ContainsKey(symbolName))
            {
                (*GlobalScope)[symbolName] = value;
            }
            else
            {
                throw new LispException("Symbol " + symbolName + " not found", this);
            }
        }

        /*public*/ std::shared_ptr<LispToken> GetPreviousToken(std::shared_ptr<LispToken> token)
        {
			std::shared_ptr<LispToken> previous = null;
            //if (Tokens)
            {
                //foreach (var item in Tokens)
				for(std::shared_ptr<LispToken> item : Tokens)
                {
                    if (*item == *token)
                    {
                        return previous;
                    }
                    previous = item;
                }
            }
            return null;
        }

        /*public*/ int GetCallStackSize() const
        {
			std::shared_ptr<const LispScope> current = shared_from_this();
            int i = 0;
            do
            {
                current = current->Previous;
                i++;
            } while (current != null);
            return i;
        }

        /*public*/ void DumpStack(int currentLevel = -1)
        {
            string stackInfo = DumpStackToString(currentLevel);
            Output.WriteLine(stackInfo);
        }

        /*public*/ string DumpStackToString(int currentLevel = -1)
        {
            string ret = string::Empty;
			std::shared_ptr<LispScope> current = shared_from_this();
            int i = GetCallStackSize();
            do
            {
                string currentItem = currentLevel == i ? "-->" : "   ";

                ret = string::Format("{0,3}{1,5} name={2,-35} lineno={3,-4} module={4}\n", currentItem, i, current->Name, current->CurrentLineNo(), current->ModuleName) + ret;
                current = current->Previous;
                i--;
            } while (current != null);
            return ret;
        }

        /*public*/ void DumpVars()
        {
			Dump([](const LispVariant & v) -> bool { return !v.IsFunction() || (v.IsFunction() && !v.FunctionValue().IsBuiltin()); });
        }

        /*public*/ void DumpFunctions()
        {
			Dump([](const LispVariant & v)-> bool { v.IsFunction(); }, [](const LispVariant & v) -> string { " : module=" + v.FunctionValue().ModuleName; });

            ProcessMetaScope(LispEnvironment::Modules, [](KeyValuePair<string, std::shared_ptr<object>> modu) -> void
            {
				//var mod = module.Value as LispScope;
				if (modu.Value->IsLispScope())
				{
					var mod = modu.Value->ToLispScope();
					mod->DumpFunctions();
				}
            });
        }

        /*public*/ void DumpMacros()
        {
			ProcessMetaScope(LispEnvironment::Macros, [this](KeyValuePair<string, std::shared_ptr<object>> macro) -> void { Output.WriteLine(macro.Key); });
        }

        /*public*/ void DumpBuiltinFunctions()
        {
			Dump([](const LispVariant & v) -> bool { return v.IsFunction() && v.FunctionValue().IsBuiltin(); });
        }

        /*public*/ void DumpBuiltinFunctionsHelp()
        {
			Dump([](const LispVariant & v) -> bool { return v.IsFunction() && v.FunctionValue().IsBuiltin(); }, [](const LispVariant & v) -> string { return v.FunctionValue().Documentation; }, /*showHelp:*/ true);
        }

        /*public*/ void DumpBuiltinFunctionsHelpFormated()
        {
			Dump([](const LispVariant & v) -> bool { return v.IsFunction() && v.FunctionValue().IsBuiltin(); }, null, /*showHelp:*/false, /*sort:*/ true, /*format:*/ [](const LispVariant & v) -> string { return v.FunctionValue().GetFormatedDoc(); });
        }

        /*public*/ void DumpBuiltinFunctionsHelpHtmlFormated()
        {
            Output.WriteLine("<html>");
            Output.WriteLine("<head>");
            Output.WriteLine("<title>");
            Output.WriteLine("Documentation of fuel language");
            Output.WriteLine("</title>");
            Output.WriteLine("</head>");
            Output.WriteLine("<h2>Documentation of builtin functions of the fuel language:</h2>");
            Output.WriteLine("<body>");
			Dump([](const LispVariant & v) -> bool { return v.IsFunction() && v.FunctionValue().IsBuiltin(); }, null, /*showHelp:*/false, /*sort:*/ true, /*format:*/[](const LispVariant & v) -> string { return v.FunctionValue().GetHtmlFormatedDoc(); });
            Output.WriteLine("</body>");
            Output.WriteLine("</html>");
        }

        /*public*/ void DumpModules()
        {
			ProcessMetaScope(LispEnvironment::Modules, [this](KeyValuePair<string, std::shared_ptr<object>> mod) -> void { return Output.WriteLine(mod.Key); });
        }

        /*public*/ string GetFunctionsHelpFormated(string functionName, /*Func<string, string, bool>*/std::function<bool(string, string)> select = null)
        {
            string result = string::Empty;
            //foreach (var key in Keys)
			for(const string & key : GetKeys())
            {
                if (select != null)
                {
                    if (select(key, functionName))
                    {
                        var value = /*(LispVariant)*/(*this)[key]->ToLispVariant();
                        result += value->FunctionValue().GetFormatedDoc();                        
                    }
                }
                else if (key.StartsWith(functionName))
                {
                    var value = /*(LispVariant)*/(*this)[key]->ToLispVariant();
                    result += value->FunctionValue().GetFormatedDoc();
                }
            }
            return result;
        }

        //#endregion

        //#region private methods

		/*private*/ void ProcessMetaScope(string metaScope, /*Action<KeyValuePair<string, std::shared_ptr<object>>>*/std::function<void(KeyValuePair<string, std::shared_ptr<object>>)> action)
        {
            if (ContainsKey(metaScope))
            {
				var items = (*this)[metaScope]; // as LispScope;
                if (items->IsLispScope())
                {
                    for (/*KeyValuePair<string, std::shared_ptr<object>>*/auto const & item : *(items->ToLispScope()))
                    {						
						KeyValuePair<string, std::shared_ptr<object>> temp(item.first, item.second);
                        action(temp);
                    }
                }
            }
        }

		/*private*/ void Dump(std::function<bool(const LispVariant &)>/*Func<LispVariant, bool>*/ select, std::function<string(const LispVariant &)>/*Func<LispVariant, string>*/ show = null, bool showHelp = false, bool sort = false, std::function<string(const LispVariant &)>/*Func<LispVariant, string>*/ format = null)
        {
			IEnumerable<LispScope::key_type> keys = GetKeys();
            if (sort)
            {
                keys.sort();                
            }
            for (var key : keys)
            {
                if (!key.StartsWith(LispEnvironment::MetaTag))
                {
                    var value = /*(LispVariant)*/(*this)[key]->ToLispVariant();
                    if (select(*value))
                    {
                        if (format != null)
                        {
                            Output.WriteLine("{0}", format(*value));
                        }
                        else
                        {
                            string info = show != null ? show(*value) : string::Empty;
                            if (showHelp)
                            {
                                Output.WriteLine("{0,20} --> {1}", key, value->FunctionValue().Signature);
                                if (!string::IsNullOrEmpty(info))
                                {
                                    Output.WriteLine("{0,20}     {1}", "", info);
                                }
                            }
                            else
                            {
                                Output.WriteLine("{0,20} --> {1,-40} : {2} {3}", key, value->ToStringDebugger(), value->TypeString(), info);
                            }                            
                        }
                    }
                }
            }
        }

        //#endregion
	};
}

#endif