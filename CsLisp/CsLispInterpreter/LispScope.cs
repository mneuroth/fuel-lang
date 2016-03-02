using System;
using System.Collections.Generic;
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
        public LispScope(string fcnName = "", LispScope globalScope = null, string moduleName = null)
        {
            Name = fcnName;
            GlobalScope = globalScope ?? this;
            ModuleName = moduleName;
            CurrentToken = null;
            Input = Console.In;
            Output = Console.Out;
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
        /// Resolves the items of the ast in this scope.
        /// </summary>
        /// <param name="elem">The elem.</param>
        /// <returns></returns>
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

                ret += string.Format("{0,3}{1,5} {2} lineno={3} module={4}\n", currentItem, i, current.Name, current.CurrentLineNo, current.ModuleName);
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
            // TODO --> dump also module functions and maybe macros !?
        }

        public void DumpBuiltinFunctions()
        {
            Dump(v => v.IsFunction && v.FunctionValue.IsBuiltin);
        }

        public void DumpModules()
        {
            // TODO --> ueber alle funktionen gehen und alle module namen sammeln und anzeigen
            Output.WriteLine(this[LispEnvironment.Modules]);
        }

        #endregion

        #region private methods

        private void Dump(Func<LispVariant, bool> select, Func<LispVariant, string> show = null)
        {
            foreach (var key in Keys)
            {
                if (!key.StartsWith(LispEnvironment.MetaTag))
                {
                    var value = (LispVariant)this[key];
                    if (select(value))
                    {
                        string info = show != null ? show(value) : string.Empty;
                        Output.WriteLine("{0,20} --> {1,-40} : {2} {3}", key, value, value.Type, info);
                    }
                }
            }
            // TODO --> hier ggf. die Module-Funktionen auch loopen...
        }

        #endregion
    }
}
