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

using System;
using System.Collections.Generic;
using System.Linq;
using System.IO;

namespace CsLisp
{
    /// <summary>
    /// The lisp runtime scope. That is something like a stack item.
    /// </summary>
    public class LispScope : Dictionary<string, object>
    {
        #region debugging support

        /// <summary>
        /// Gets and sets the debuging modus.
        /// </summary>
        public ILispDebugger Debugger { get; set; }

        /// <summary>
        /// Gets and sets the tracing modus.
        /// </summary>
        public bool Tracing { get; set; }

        /// <summary>
        /// Gets and sets all tokens of the current script,
        /// used for debugging purpose and for showing the 
        /// position of an error.
        /// </summary>
        public IList<LispToken> Tokens { get; set; }

        /// <summary>
        /// Gets and sets the next and previous scope,
        /// used for debugging purpose to show the 
        /// call stack
        /// </summary>
        public LispScope Next { get; private set; }
        public LispScope Previous { get; set; }

        /// <summary>
        /// Gets or sets the scope chain to implement closures.
        /// </summary>
        /// <value>
        /// The closure chain.
        /// </value>
        public LispScope ClosureChain { get; set; }

        /// <summary>
        /// Gets or sets the current module name and path.
        /// </summary>
        /// <value>
        /// The module name and path.
        /// </value>       
        public string ModuleName { get; set; }

        /// <summary>
        /// Gets or sets the current token.
        /// </summary>
        public LispToken CurrentToken { get; set; }

        public int CurrentLineNo
        {
            get
            {
                return CurrentToken != null ? CurrentToken.LineNo : -1;
            }
        }

        /// <summary>
        /// Gets or sets user data.
        /// Needed for debugging support --> set function name to LispScope
        /// </summary>
        /// <value> The user data. </value>
        public object UserData { get; set; }

        /// <summary>
        /// Gets or sets the user documentation information.
        /// </summary>
        /// <value>The user documentation.</value>
        public Tuple<string, string> UserDoc { get; set; }

        #endregion

        #region properties

        /// <summary>
        /// Gets the name of this scope.
        /// </summary>
        /// <value> The name. </value>
        public string Name { get; private set; }

        /// <summary>
        /// Gets the global scope.
        /// </summary>
        /// <value> The global scope. </value>
        public LispScope GlobalScope { get; private set; }

        /// <summary>
        /// Gets the output stream.
        /// </summary>
        /// <value>
        /// The output.
        /// </value>
        public TextWriter Output { get; set; }

        /// <summary>
        /// Gets the input stream.
        /// </summary>
        /// <value>
        /// The input.
        /// </value>
        public TextReader Input { get; set; }

        #endregion

        #region constructor

        /// <summary>
        /// Initializes a new instance of the <see cref="LispScope"/> class.
        /// </summary>
        /// <param name="fcnName">Name of the FCN.</param>
        /// <param name="globalScope">The global scope.</param>
        /// <param name="moduleName">The current module name for the scope.</param>
        public LispScope(string fcnName, LispScope globalScope = null, string moduleName = null)
        {
            Name = fcnName;
            GlobalScope = globalScope ?? this;
            ModuleName = moduleName;
            if (ModuleName == null && globalScope != null)
            {
                ModuleName = globalScope.ModuleName;
            }
            CurrentToken = null;
            Input = Console.In;
            Output = Console.Out;
        }

        /// <summary>
        /// Initializes a new instance of the <see cref="LispScope"/> class.
        /// </summary>
        /// <remarks>Needed for compiler module and .NET 3.5</remarks>
        public LispScope()
            : this(string.Empty)
        {
        }

        #endregion

        #region public methods

        public void PushNextScope(LispScope nextScope)
        {
            Next = nextScope;
            nextScope.Previous = this;
        }

        public void PopNextScope()
        {
            Next.Previous = null;
            Next = null;
        }

        /// <summary>
        /// Determines whether the given name is available in the closure chain.
        /// </summary>
        /// <param name="name">The name.</param>
        /// <param name="closureScopeFound">The closure scope found.</param>
        /// <returns>True if name was found.</returns>
        public bool IsInClosureChain(string name, out LispScope closureScopeFound)
        {
            closureScopeFound = null;
            if (ClosureChain != null)
            {
                if (ClosureChain.ContainsKey(name))
                {
                    closureScopeFound = ClosureChain;
                    return true;
                }
                return ClosureChain.IsInClosureChain(name, out closureScopeFound);
            }
            return false;
        }

        /// <summary>
        /// Resolves the given element in this scope.
        /// </summary>
        /// <param name="elem">The element.</param>
        /// <returns>Resolved value or null</returns>
        public object ResolveInScopes(object elem)
        {
            object result;

            var name = elem.ToString();
            LispScope foundClosureScope;
            // first try to resolve in this scope
            if (ContainsKey(name))
            {
                result = this[name];
            }
            // then try to resolve in closure chain scope(s)
            else if (IsInClosureChain(name, out foundClosureScope))
            {
                result = foundClosureScope[name];
            }
            // then try to resolve in global scope
            else if (GlobalScope != null &&
                     GlobalScope.ContainsKey(name))
            {
                result = GlobalScope[name];
            }
            // then try to resolve in scope of loaded modules
            else if (LispEnvironment.IsInModules(name, GlobalScope))
            {
                result = LispEnvironment.GetFunctionInModules(name, GlobalScope);
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
        public void SetInScopes(string symbolName, object value)
        {
            LispScope foundClosureScope;
            if (symbolName != null && ContainsKey(symbolName))
            {
                this[symbolName] = value;
            }
            else if (symbolName != null && IsInClosureChain(symbolName, out foundClosureScope))
            {
                foundClosureScope[symbolName] = value;
            }
            else if (symbolName != null && GlobalScope != null && GlobalScope.ContainsKey(symbolName))
            {
                GlobalScope[symbolName] = value;
            }
            else
            {
                throw new LispException("Symbol " + symbolName + " not found", this);
            }
        }

        public LispToken GetPreviousToken(LispToken token)
        {
            LispToken previous = null;
            if (Tokens != null)
            {
                foreach (var item in Tokens)
                {
                    if (item == token)
                    {
                        return previous;
                    }
                    previous = item;
                }
            }
            return null;
        }

        public int GetCallStackSize()
        {
            LispScope current = this;
            int i = 0;
            do
            {
                current = current.Previous;
                i++;
            } while (current != null);
            return i;
        }

        public void DumpStack(int currentLevel = -1)
        {
            string stackInfo = DumpStackToString(currentLevel);
            Output.WriteLine(stackInfo);
        }

        public string DumpStackToString(int currentLevel = -1)
        {
            string ret = string.Empty;
            LispScope current = this;
            int i = GetCallStackSize();
            do
            {
                string currentItem = currentLevel == i ? "-->" : "   ";

                ret = string.Format("{0,3}{1,5} name={2,-35} lineno={3,-4} module={4}\n", currentItem, i, current.Name, current.CurrentLineNo, current.ModuleName) + ret;
                current = current.Previous;
                i--;
            } while (current != null);
            return ret;
        }

        public void DumpVars()
        {
            Dump(v => !v.IsFunction || (v.IsFunction && !v.FunctionValue.IsBuiltin));
        }

        public void DumpFunctions()
        {
            Dump(v => v.IsFunction, v => " : module=" + v.FunctionValue.ModuleName);

            ProcessMetaScope(LispEnvironment.Modules, module =>
            {
                var mod = module.Value as LispScope;
                if (mod != null)
                {
                    mod.DumpFunctions();
                }
            });
        }

        public void DumpMacros()
        {
            ProcessMetaScope(LispEnvironment.Macros, macro => Output.WriteLine(macro.Key));
        }

        public void DumpBuiltinFunctions()
        {
            Dump(v => v.IsFunction && v.FunctionValue.IsBuiltin);
        }

        public void DumpBuiltinFunctionsHelp()
        {
            Dump(v => v.IsFunction && v.FunctionValue.IsBuiltin, v => v.FunctionValue.Documentation, showHelp: true);
        }

        public void DumpBuiltinFunctionsHelpFormated()
        {
            Dump(v => v.IsFunction && v.FunctionValue.IsBuiltin, sort: true, format: v => v.FunctionValue.FormatedDoc);
        }

        public void DumpBuiltinFunctionsHelpHtmlFormated()
        {
            Output.WriteLine("<html>");
            Output.WriteLine("<head>");
            Output.WriteLine("<title>");
            Output.WriteLine("Documentation of fuel language");
            Output.WriteLine("</title>");
            Output.WriteLine("</head>");
            Output.WriteLine("<h2>Documentation of builtin functions of the fuel language:</h2>");
            Output.WriteLine("<body>");
            Dump(v => v.IsFunction && v.FunctionValue.IsBuiltin, sort: true, format: v => v.FunctionValue.HtmlFormatedDoc);
            Output.WriteLine("</body>");
            Output.WriteLine("</html>");
        }

        public void DumpModules()
        {
            ProcessMetaScope(LispEnvironment.Modules, module => Output.WriteLine(module.Key));
        }

        public string GetFunctionsHelpFormated(string functionName, Func<string, string, bool> select = null)
        {
            string result = string.Empty;
            foreach (var key in Keys)
            {
                if (select != null)
                {
                    if (select(key, functionName))
                    {
                        var value = (LispVariant)this[key];
                        result += value.FunctionValue.FormatedDoc;                        
                    }
                }
                else if (key.StartsWith(functionName))
                {
                    var value = (LispVariant)this[key];
                    result += value.FunctionValue.FormatedDoc;
                }
            }
            return result;
        }

        #endregion

        #region private methods

        private void ProcessMetaScope(string metaScope, Action<KeyValuePair<string, object>> action)
        {
            if (ContainsKey(metaScope))
            {
                var items = this[metaScope] as LispScope;
                if (items != null)
                {
                    foreach (KeyValuePair<string, object> item in items)
                    {
                        action(item);
                    }
                }
            }
        }

        private void Dump(Func<LispVariant, bool> select, Func<LispVariant, string> show = null, bool showHelp = false, bool sort = false, Func<LispVariant, string> format = null)
        {
            var keys = Keys.ToList();
            if (sort)
            {
                keys.Sort();                
            }
            foreach (var key in keys)
            {
                if (!key.StartsWith(LispEnvironment.MetaTag))
                {
                    var value = (LispVariant)this[key];
                    if (select(value))
                    {
                        if (format != null)
                        {
                            Output.WriteLine("{0}", format(value));
                        }
                        else
                        {
                            string info = show != null ? show(value) : string.Empty;
                            if (showHelp)
                            {
                                Output.WriteLine("{0,20} --> {1}", key, value.FunctionValue.Signature);
                                if (!string.IsNullOrEmpty(info))
                                {
                                    Output.WriteLine("{0,20}     {1}", "", info);
                                }
                            }
                            else
                            {
                                Output.WriteLine("{0,20} --> {1,-40} : {2} {3}", key, value.ToStringDebugger(), value.TypeString, info);
                            }                            
                        }
                    }
                }
            }
        }

        #endregion
    }
}
