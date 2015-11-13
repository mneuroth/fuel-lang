using System;
using System.Collections.Generic;
using System.Linq;
using System.Reflection;
using System.Text;

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
        /// Has program execution finished?
        /// </summary>
        public bool Finished { get; set; }

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

        #endregion

        #region constructor

        /// <summary>
        /// Initializes a new instance of the <see cref="LispScope"/> class.
        /// </summary>
        /// <param name="fcnName">Name of the FCN.</param>
        /// <param name="globalScope">The global scope.</param>
        public LispScope(string fcnName = "", LispScope globalScope = null)
        {
            Name = fcnName;
            GlobalScope = globalScope ?? this;
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
            LispScope current = this;
            int i = GetCallStackSize();
            do
            {
                string currentItem = currentLevel == i ? "-->" : "   ";
                Console.WriteLine("{0,3}{1,5} {2}", currentItem, i, current.Name);
                current = current.Previous;
                i--;
            } while (current != null);
        }

        public void DumpVars()
        {
            Dump(v => !v.IsFunction || (v.IsFunction && !v.FunctionValue.IsBuiltin));
        }

        public void DumpFunctions()
        {
            Dump(v => v.IsFunction && v.FunctionValue.IsBuiltin);
        }

        #endregion

        #region private methods

        private void Dump(Func<LispVariant, bool> select)
        {
            foreach (var key in Keys)
            {
                if (!key.StartsWith(LispEnvironment.MetaTag))
                {
                    var value = (LispVariant)this[key];
                    if (select(value))
                    {
                        Console.WriteLine("{0,15} --> {1,-45} : {2}", key, value, value.Type);
                    }
                }
            }
        }

        #endregion
    }

    /// <summary>
    /// Wrapper for an environment function or special form.
    /// </summary>
    public struct LispFunctionWrapper
    {
        #region properties

        public Func<object[], LispScope, LispVariant> Function { get; private set; }

        public string Signature { get; private set; }

        public bool IsBuiltin { get; private set; }

        public bool IsSpecialForm { get; private set; }

        public bool IsEvalInExpand { get; private set; }

        #endregion

        #region constroctor(s)

        public LispFunctionWrapper(Func<object[], LispScope, LispVariant> func, string signature, bool isBuiltin, bool isSpecialForm = false, bool isEvalInExpand = false)
            : this()
        {
            Function = func;
            Signature = signature;
            IsBuiltin = isBuiltin;
            IsSpecialForm = isSpecialForm;
            IsEvalInExpand = isEvalInExpand;
        }

        public LispFunctionWrapper(Func<LispVariant, LispScope, LispVariant> func, string signature, bool isBuiltin, bool isSpecialForm = false, bool isEvalInExpand = false)
            : this((args, scope) => func((LispVariant)(args[0]), scope), signature, isBuiltin, isSpecialForm, isEvalInExpand)
        {
        }

        public LispFunctionWrapper(Func<LispVariant, LispVariant, LispScope, LispVariant> func, string signature, bool isBuiltin, bool isSpecialForm = false, bool isEvalInExpand = false)
            : this((args, scope) => func((LispVariant)(args[0]), (LispVariant)(args[1]), scope), signature, isBuiltin, isSpecialForm, isEvalInExpand)
        {
        }

        public LispFunctionWrapper(Func<LispVariant, LispVariant, LispVariant, LispScope, LispVariant> func, string signature, bool isBuiltin, bool isSpecialForm = false, bool isEvalInExpand = false)
            : this((args, scope) => func((LispVariant)(args[0]), (LispVariant)(args[1]), (LispVariant)(args[2]), scope), signature, isBuiltin, isSpecialForm, isEvalInExpand)
        {
        }

        public LispFunctionWrapper(Func<LispVariant, LispVariant, LispVariant, LispVariant, LispScope, LispVariant> func, string signature, bool isBuiltin, bool isSpecialForm = false, bool isEvalInExpand = false)
            : this((args, scope) => func((LispVariant)(args[0]), (LispVariant)(args[1]), (LispVariant)(args[2]), (LispVariant)(args[3]), scope), signature, isBuiltin, isSpecialForm, isEvalInExpand)
        {
        }

        public LispFunctionWrapper(Func<LispVariant, LispVariant, LispVariant, LispVariant, LispVariant, LispScope, LispVariant> func, string signature, bool isBuiltin, bool isSpecialForm = false, bool isEvalInExpand = false)
            : this((args, scope) => func((LispVariant)(args[0]), (LispVariant)(args[1]), (LispVariant)(args[2]), (LispVariant)(args[3]), (LispVariant)(args[4]), scope), signature, isBuiltin, isSpecialForm, isEvalInExpand)
        {
        }

        public LispFunctionWrapper(Func<LispVariant, LispVariant, LispVariant, LispVariant, LispVariant, LispVariant, LispScope, LispVariant> func, string signature, bool isBuiltin, bool isSpecialForm = false, bool isEvalInExpand = false)
            : this((args, scope) => func((LispVariant)(args[0]), (LispVariant)(args[1]), (LispVariant)(args[2]), (LispVariant)(args[3]), (LispVariant)(args[4]), (LispVariant)(args[5]), scope), signature, isBuiltin, isSpecialForm, isEvalInExpand)
        {
        }

        public LispFunctionWrapper(Func<LispVariant, LispVariant, LispVariant, LispVariant, LispVariant, LispVariant, LispVariant, LispScope, LispVariant> func, string signature, bool isBuiltin, bool isSpecialForm = false, bool isEvalInExpand = false)
            : this((args, scope) => func((LispVariant)(args[0]), (LispVariant)(args[1]), (LispVariant)(args[2]), (LispVariant)(args[3]), (LispVariant)(args[4]), (LispVariant)(args[5]), (LispVariant)(args[6]), scope), signature, isBuiltin, isSpecialForm, isEvalInExpand)
        {
        }

        public LispFunctionWrapper(Func<LispVariant, LispVariant, LispVariant, LispVariant, LispVariant, LispVariant, LispVariant, LispVariant, LispScope, LispVariant> func, string signature, bool isBuiltin, bool isSpecialForm = false, bool isEvalInExpand = false)
            : this((args, scope) => func((LispVariant)(args[0]), (LispVariant)(args[1]), (LispVariant)(args[2]), (LispVariant)(args[3]), (LispVariant)(args[4]), (LispVariant)(args[5]), (LispVariant)(args[6]), (LispVariant)(args[7]), scope), signature, isBuiltin, isSpecialForm, isEvalInExpand)
        {
        }

        public LispFunctionWrapper(Func<LispVariant, LispVariant, LispVariant, LispVariant, LispVariant, LispVariant, LispVariant, LispVariant, LispVariant, LispScope, LispVariant> func, string signature, bool isBuiltin, bool isSpecialForm = false, bool isEvalInExpand = false)
            : this((args, scope) => func((LispVariant)(args[0]), (LispVariant)(args[1]), (LispVariant)(args[2]), (LispVariant)(args[3]), (LispVariant)(args[4]), (LispVariant)(args[5]), (LispVariant)(args[6]), (LispVariant)(args[7]), (LispVariant)(args[8]), scope), signature, isBuiltin, isSpecialForm, isEvalInExpand)
        {
        }

        public LispFunctionWrapper(Func<LispVariant, LispVariant, LispVariant, LispVariant, LispVariant, LispVariant, LispVariant, LispVariant, LispVariant, LispVariant, LispScope, LispVariant> func, string signature, bool isBuiltin, bool isSpecialForm = false, bool isEvalInExpand = false)
            : this((args, scope) => func((LispVariant)(args[0]), (LispVariant)(args[1]), (LispVariant)(args[2]), (LispVariant)(args[3]), (LispVariant)(args[4]), (LispVariant)(args[5]), (LispVariant)(args[6]), (LispVariant)(args[7]), (LispVariant)(args[8]), (LispVariant)(args[9]), scope), signature, isBuiltin, isSpecialForm, isEvalInExpand)
        {
        }

        #endregion
    }

    /// <summary>
    /// The runtime environment for the FUEL lisp interpreter.
    /// </summary>
    public class LispEnvironment
    {
        #region constants

        public const string MetaTag = "###";

        private const string MainScope = "<main>";
        private const string AnonymousScope = "<anonymous>";

        private const string If = "if";
        private const string While = "while";
        private const string Do = "do";
        private const string Fn = "fn";
        private const string Def = "def";
        private const string Gdef = "gdef";
        private const string Setf = "setf";
        private const string Defn = "defn";
        private const string Gdefn = "gdefn";
        private const string MapFcn = "map";
        private const string ReduceFcn = "reduce";
        private const string DefineMacro = "define-macro";
        //private const string DefineMacroExpand = "define-macro-expand";
        private const string Lambda = "lambda";
        private const string Macros = MetaTag + "macros" + MetaTag;
        private const string Tracebuffer = MetaTag + "tracebuffer" + MetaTag;
        private const string Traceon = MetaTag + "traceon" + MetaTag;
            
        public const string Apply = "apply";
        public const string Quote = "quote";
        public const string Quasiquote = "quasiquote";

        private const string BadArgumentCount = "bad argument count in ";

        #endregion

        #region public methods of environment

        public static bool IsMacro(object funcName, LispScope globalScope)
        {
            if (globalScope != null &&
                globalScope.ContainsKey(Macros))
            {
                return ((LispScope)globalScope[Macros]).ContainsKey(funcName.ToString());
            }
            return false;
        }

        public static object GetMacro(object funcName, LispScope globalScope)
        {
            if (globalScope != null &&
                globalScope.ContainsKey(Macros) &&
                ((LispScope)globalScope[Macros]).ContainsKey(funcName.ToString()))
            {
                return ((LispScope)globalScope[Macros])[funcName.ToString()];
            }
            return null;
        }

        public static LispScope CreateDefaultScope()
        {
            var scope = new LispScope(MainScope);

            // meta information fields
            scope[Macros] = new LispScope(Macros, scope);
            scope[Tracebuffer] = new StringBuilder();
            scope[Traceon] = false;

            // infrastructure functions
            scope["help"] = CreateFunction(Help);
            scope["break"] = CreateFunction(Break);
            scope["vars"] = CreateFunction(Vars);
            scope["trace"] = CreateFunction(TracePrint);
            scope["gettrace"] = CreateFunction(GetTracePrint);

            // access to .NET
            scope["native-methods"] = CreateFunction(GetNativeMethods, "(native-methods native-obj|class-name)");
            scope["call"] = CreateFunction(CallNative, "(call native-obj|class-name [method-name [args...]]|[args...])");    // call native function
            // Macro: (register-native full-class-name lisp-name) --> erzeugt konstruktoren und zugriffsmethoden fuer klasse
            // --> (lisp-name-create args)
            // --> (lisp-name-method obj args)

            // interpreter functions
            scope["type"] = CreateFunction(GetType);
            scope["nop"] = CreateFunction(Nop);
            scope["print"] = CreateFunction(Print);

            scope["string"] = CreateFunction(Addition);
            scope["add"] = CreateFunction(Addition);
            scope["+"] = CreateFunction(Addition);
            scope["minus"] = CreateFunction(Subtraction);
            scope["-"] = CreateFunction(Subtraction);
            scope["mul"] = CreateFunction(Multiplication);
            scope["*"] = CreateFunction(Multiplication);
            scope["div"] = CreateFunction(Division);
            scope["/"] = CreateFunction(Division);

            scope["<"] = CreateFunction(LessTest);
            scope[">"] = CreateFunction(GreaterTest);
            scope["<="] = CreateFunction(LessEqualTest);
            scope[">="] = CreateFunction(GreaterEqualTest);

            scope["equal"] = CreateFunction(EqualTest);
            scope["="] = CreateFunction(EqualTest);
            scope["=="] = CreateFunction(EqualTest);

            scope["not"] = CreateFunction(Not);
            scope["!"] = CreateFunction(Not);

            scope["list"] = CreateFunction(CreateList);
            scope[MapFcn] = CreateFunction(Map, "(map func list)");
            scope[ReduceFcn] = CreateFunction(Reduce, "(reduce func list initial)");
            scope["len"] = CreateFunction(Length);
            scope["first"] = CreateFunction(First);
            scope["rest"] = CreateFunction(Rest);
            scope["nth"] = CreateFunction(Nth);
            scope["sym"] = CreateFunction(Sym);

            scope[Apply] = CreateFunction(ApplyFcn);

            // special forms
            scope["and"] = CreateFunction(and_form, isSpecialForm: true);
            scope["or"] = CreateFunction(or_form, isSpecialForm: true);
            scope[Def] = CreateFunction(def_form, isSpecialForm: true);
            scope[Gdef] = CreateFunction(gdef_form, isSpecialForm: true);
            scope[Setf] = CreateFunction(setf_form, isSpecialForm: true);
            scope[DefineMacro] = CreateFunction(definemacro_form, isSpecialForm: true, isEvalInExpand: true);
            //scope[DefineMacroExpand] = CreateFunction(definemacroexpand_form, isSpecialForm: true, isEvalInExpand: true);
            scope[Quote] = CreateFunction(quote_form, isSpecialForm: true);
            scope[Quasiquote] = CreateFunction(quasiquote_form, isSpecialForm: true);
            scope[If] = CreateFunction(if_form, isSpecialForm: true);
            scope[While] = CreateFunction(while_form, isSpecialForm: true);
            scope[Do] = CreateFunction(do_form, isSpecialForm: true);
            scope["begin"] = CreateFunction(do_form, isSpecialForm: true);
            scope[Lambda] = CreateFunction(fn_form, isSpecialForm: true);
            scope[Fn] = CreateFunction(fn_form, isSpecialForm: true);
            scope[Defn] = CreateFunction(defn_form, isSpecialForm: true);
            scope[Gdefn] = CreateFunction(gdefn_form, isSpecialForm: true);

            return scope;
        }

        #endregion

        #region functions: infrastructure and debugging

        private static LispVariant Help(object[] args, LispScope scope)
        {
            var helpText = new StringBuilder();
            helpText.Append("available functions:\n");
            foreach (var cmd in scope.Keys)
            {                
                string s = cmd+"\n";
                helpText.Append(s);
            }
            Console.WriteLine(helpText.ToString());
            return new LispVariant(helpText.ToString());
        }

        private static LispVariant Break(object[] args, LispScope scope)
        {
            Console.WriteLine("break -> call stack:");
            scope.DumpStack(scope.GetCallStackSize());
            var debugger = scope.GlobalScope.Debugger;
            if (debugger != null)
            {
                debugger.InteractiveLoop(initialTopScope: scope);
            }
            else
            {
                Console.WriteLine("Warning: can not break, because no debugger support availabe!");
            }
            return new LispVariant();
        }

        private static LispVariant Vars(object[] args, LispScope scope)
        {
            Console.WriteLine("variables:");
            scope.DumpVars();
            return new LispVariant();
        }

        private static LispVariant TracePrint(object[] args, LispScope scope)
        {
            var status = (LispVariant)args[0];
            scope[Traceon] = status.BoolValue;
            return new LispVariant(status.BoolValue);
        }

        private static LispVariant GetTracePrint(object[] args, LispScope scope)
        {
            var buffer = (StringBuilder)scope[Tracebuffer];
            return new LispVariant(buffer.ToString());
        }

        #endregion

        #region functions

        public static LispVariant Nop(object[] args, LispScope scope)
        {
            return new LispVariant();
        }

        public static LispVariant GetType(object[] args, LispScope scope)
        {
            CheckArgs("type", 1, args, scope);

            var item = ((LispVariant)args[0]);
            return new LispVariant((int)item.Type);
        }

        public static LispVariant Print(object[] args, LispScope scope)
        {
            var text = String.Empty;
            foreach (var item in args)
            {
                if (text.Length > 0)
                {
                    text += " ";
                }
                text += item;
            }
            if (scope.ContainsKey(Traceon) && (bool)scope[Traceon])
            {
                var buffer = (StringBuilder)scope[Tracebuffer];
                buffer.Append(text);
            }
            Console.WriteLine(text);
            return new LispVariant(text);
        }

        public static LispVariant Addition(object[] args, LispScope scope)
        {
            return ArithmetricOperation(args, (l, r) => l + r);
        }

        public static LispVariant Subtraction(object[] args, LispScope scope)
        {
            return ArithmetricOperation(args, (l, r) => l - r);
        }

        public static LispVariant Multiplication(object[] args, LispScope scope)
        {
            return ArithmetricOperation(args, (l, r) => l * r);
        }

        public static LispVariant Division(object[] args, LispScope scope)
        {
            return ArithmetricOperation(args, (l, r) => l / r);
        }

        public static LispVariant Not(object[] args, LispScope scope)
        {
            CheckArgs("not", 1, args, scope);

            var arg1 = (LispVariant)args[0];
            return new LispVariant(!arg1.BoolValue);
        }

        public static LispVariant LessTest(object[] args, LispScope scope)
        {
            return CompareOperation(args, (l, r) => new LispVariant(l < r), scope);
        }

        public static LispVariant GreaterTest(object[] args, LispScope scope)
        {
            return CompareOperation(args, (l, r) => new LispVariant(l > r), scope);
        }

        public static LispVariant LessEqualTest(object[] args, LispScope scope)
        {
            return CompareOperation(args, (l, r) => new LispVariant(l <= r), scope);
        }

        public static LispVariant GreaterEqualTest(object[] args, LispScope scope)
        {
            return CompareOperation(args, (l, r) => new LispVariant(l >= r), scope);
        }

        public static LispVariant EqualTest(object[] args, LispScope scope)
        {
            return CompareOperation(args, (l, r) => new LispVariant(LispVariant.EqualOp(l, r)), scope);
        }

        public static LispVariant CreateList(object[] args, LispScope scope)
        {
            var result = new LispVariant(LispType.List, new List<object>());
            foreach (var arg in args)
            {
                result.Add(arg);
            }
            return result;
        }

        public static LispVariant Map(object[] args, LispScope scope)
        {
            CheckArgs(MapFcn, 2, args, scope);

            var function = CheckForFunction(MapFcn, args[0], scope).FunctionValue;
            var elements = CheckForList(MapFcn, args[1], scope);

            var result = new LispVariant(LispType.List, new List<object>());
            foreach (var elem in elements)
            {
                // call for every element the given function (args[0])
                result.Add(function.Function(new[] {elem}, scope));
            }
            return result;
        }

        public static LispVariant Reduce(object[] args, LispScope scope)
        {
            CheckArgs(ReduceFcn, 3, args, scope);

            var function = CheckForFunction(ReduceFcn, args[0], scope).FunctionValue;
            var elements = CheckForList(ReduceFcn, args[1], scope);

            var start = (LispVariant)args[2];
            var result = new LispVariant(start);
            foreach (var elem in elements)
            {
                // call for every element the given function (args[0])
                result = function.Function(new[] { elem, result }, scope);
            }
            return result;
        }

        public static LispVariant Length(object[] args, LispScope scope)
        {
            CheckArgs("len", 1, args, scope);

            var elements = ((LispVariant)args[0]).ListValue;
            return new LispVariant(elements.Count());
        }

        public static LispVariant First(object[] args, LispScope scope)
        {
            CheckArgs("first", 1, args, scope);

            var elements = ((LispVariant)args[0]).ListValue;
            return new LispVariant(elements.First());
        }

        public static LispVariant Rest(object[] args, LispScope scope)
        {
            CheckArgs("rest", 1, args, scope);

            var elements = ((LispVariant)args[0]).ListValue;
            return new LispVariant(elements.Skip(1));
        }

        public static LispVariant Nth(object[] args, LispScope scope)
        {
            CheckArgs("nth", 2, args, scope);

            var index = ((LispVariant)args[0]).IntValue;
            var elements = ((LispVariant)args[1]).ListValue;
            return new LispVariant(elements.ElementAt(index));
        }

        public static LispVariant Sym(object[] args, LispScope scope)
        {
            CheckArgs("sym", 1, args, scope);

            var symbol = ((LispVariant)args[0]).ToString();
            return new LispVariant(LispType.Symbol, symbol);
        }

        // used also for processing macros !
        public static LispVariant ApplyFcn(object[] args, LispScope scope)
        {
            var fcn = (LispVariant)args[0];

            var evaluatedArgs = new object[args.Length - 1];
            for (int i = 0; i < evaluatedArgs.Length; i++)
            {
                evaluatedArgs[i] = LispInterpreter.EvalAst(args[i + 1], scope);
            }

            var result = fcn.FunctionValue.Function(evaluatedArgs, scope);
            return result;
        }

        #endregion

        #region special forms

        private static LispVariant bool_operation_form(object[] args, LispScope scope, Func<bool, bool, bool> func, bool initial)
        {
            var result = initial;
            foreach (var arg in args)
            {
                bool value = LispInterpreter.EvalAst(arg, scope).BoolValue;
                result = func(result, value);
                if (!result)
                {
                    break;
                }
            }
            return new LispVariant(result);
        }

        public static LispVariant and_form(object[] args, LispScope scope)
        {
            return bool_operation_form(args, scope, (r, v) => r && v, true);
        }

        public static LispVariant or_form(object[] args, LispScope scope)
        {
            return bool_operation_form(args, scope, (r, v) => r || v, false);
        }

        public static LispVariant def_form(object[] args, LispScope scope)
        {
            return def_form_helper(args, scope, Def, scope);
        }

        public static LispVariant gdef_form(object[] args, LispScope scope)
        {
            return def_form_helper(args, scope, Gdef, scope.GlobalScope);
        }

        public static LispVariant setf_form(object[] args, LispScope scope)
        {
            CheckArgs(Setf, 2, args, scope);

            var symbol = EvalArgIfNeeded(args[0], scope);
            var symbolName = symbol != null ? symbol.ToString() : null;
            var value = LispInterpreter.EvalAst(args[1], scope);
            if (symbolName != null && scope.ContainsKey(symbolName))
            {
                scope[symbolName] = value;
            }
            else if (symbolName != null && scope.GlobalScope.ContainsKey(symbolName))
            {
                scope.GlobalScope[symbolName] = value;
            }
            else
            {
                throw new LispException("Symbol " + symbolName + " not found" + GetPositionOfPreviousTokenForSymbol(symbol, scope), symbol.Token.LineNo);
            }
            return value;
        }

        // ??? (define-macro name (arg1 arg2 ...) (block ... ))
        // (define-macro name (lambda (arg1 arg2 ...) (block ... )))
        private static LispVariant definemacro_form(object[] args, LispScope scope)
        {
            CheckArgs(DefineMacro, 2, args, scope);

            var macros = scope.GlobalScope[Macros] as LispScope;
            if (macros != null)
            {
                macros[args[0].ToString()] = args[1];
            }

            return new LispVariant();
        }

// TODO --> implementieren --> dynamisch code erzeugen und in den Ast einhaengen !
        //private static LispVariant definemacroexpand_form(object[] args, LispScope scope)
        //{
        //    CheckArgs(DefineMacroExpand, 2, args, scope);

        //    var macros = scope.GlobalScope[Macros] as LispScope;
        //    if (macros != null)
        //    {
        //        macros[args[0].ToString()] = args[1];
        //    }

        //    return new LispVariant();
        //}

        public static LispVariant quote_form(object[] args, LispScope scope)
        {
            CheckArgs("quote", 1, args, scope);

            return new LispVariant(args[0]);
        }

        public static LispVariant quasiquote_form(object[] args, LispScope scope)
        {
            CheckArgs("quasiquote", 1, args, scope);

            // unquote elements of list if needed
            var lst = (IEnumerable<object>)args[0];
            var ret = new List<object>();
            foreach (var elem in lst)
            {
                object item = UnQuoteIfNeeded(elem, scope);
                // process unquotesplicing
                IEnumerable<object> sublst = ToEnumerable(item);
                if (sublst != null)
                {
                    foreach (var subitem in sublst)
                    {
                        ret.Add(subitem);
                    }
                }
                else
                {
                    ret.Add(item);
                }
            }
            return new LispVariant(ret);
        }

        public static LispVariant if_form(object[] args, LispScope scope)
        {
            CheckArgs("if", 3, args, scope);

            var passed = LispInterpreter.EvalAst(args[0], scope).BoolValue;
            return LispInterpreter.EvalAst(passed ? args[1] : args[2], scope);
        }

        public static LispVariant while_form(object[] args, LispScope scope)
        {
            CheckArgs("while", 2, args, scope);

            var result = new LispVariant();
            var condition = LispInterpreter.EvalAst(args[0], scope);
            while (condition.ToBool())
            {
                result = LispInterpreter.EvalAst(args[1], scope);
                condition = LispInterpreter.EvalAst(args[0], scope);
            }
            return result;
        }

        public static LispVariant do_form(object[] args, LispScope scope)
        {
            var result = new LispVariant();

            foreach (var statement in args)
            {
                if (!(statement is IEnumerable<object>))
                {
                    throw new LispException("List expected in do in " + GetPositionOfToken(((LispVariant)statement).Token));
                }
                result = LispInterpreter.EvalAst(statement, scope);
            }

            return result;
        }

        public static LispVariant fn_form(object[] args, LispScope scope)
        {
            var name = scope.UserData != null ? scope.UserData.ToString() : AnonymousScope;

            Func<object[], LispScope, LispVariant> fcn =
                (localArgs, localScope) =>
                {
                    var childScope = new LispScope(name, localScope.GlobalScope);
                    localScope.PushNextScope(childScope);

                    var i = 0;
                    foreach (var arg in (IEnumerable<object>)args[0])
                    {
                        childScope[arg.ToString()] = localArgs[i];
                        i++;
                    }

                    // save the current call stack to resolve variables in closures
                    childScope.ClosureChain = scope;

                    var ret = LispInterpreter.EvalAst(args[1], childScope);
                    localScope.PopNextScope();
                    return ret;
                };

            return new LispVariant(CreateFunction(fcn, isBuiltin: false));
        }

        public static LispVariant defn_form(object[] args, LispScope scope)
        {
            return defn_form_helper(args, scope, Def);
        }

        public static LispVariant gdefn_form(object[] args, LispScope scope)
        {
            return defn_form_helper(args, scope, Gdef);
        }

        #endregion

        #region internal methods

        internal static Tuple<int, int> GetPosInfo(LispToken token)
        {
            if (token != null)
            {
                return new Tuple<int, int>(token.StartPos, token.LineNo);
            }
            return new Tuple<int, int>(-1, -1);
        }

        #endregion

        #region private methods

        // (call class-name [args...])
        // or
        // (call native-obj full-method-name [args...])
        private static LispVariant CallNative(object[] args, LispScope scope)
        {
            var nativeObjOrClassName = ((LispVariant)args[0]);

            Type nativeClass;
            if (nativeObjOrClassName.IsString || nativeObjOrClassName.IsSymbol)
            {
                // constructor call
                var callArgs = new object[args.Length - 1];
                if (args.Length > 1)
                {
                    Array.Copy(args, 1, callArgs, 0, args.Length - 1);
                }

                nativeClass = Type.GetType(nativeObjOrClassName.ToString());

                if (nativeClass != null)
                {
                    ConstructorInfo constructor = nativeClass.GetConstructor(new Type[0]);
                    //TODO: ConstructorInfo[] constructors = nativeClass.GetConstructors();

                    if (constructor != null)
                    {
                        object result = constructor.Invoke(callArgs);
                        return new LispVariant(LispType.NativeObject, result);
                    }
                }
                throw new LispException("Bad constructor for class " + nativeObjOrClassName, nativeObjOrClassName.Token.LineNo);
            }
            else
            {
                // method call
                var methodName = args.Length > 1 ? args[1].ToString() : string.Empty;
                var callArgs = new object[args.Length > 1 ? args.Length - 2 : 0];
                if (args.Length > 2)
                {
                    Array.Copy(args, 2, callArgs, 0, args.Length - 2);
                }

                nativeClass = nativeObjOrClassName.NativeObjectValue.GetType();

                MethodInfo method = nativeClass.GetMethod(methodName);

                if (method != null)
                {
                    ParameterInfo[] parameterInfos = method.GetParameters();
                    object result = method.Invoke(nativeObjOrClassName.NativeObjectValue, ConvertAllToNative(callArgs, parameterInfos));
                    return new LispVariant(result);
                }
                else
                {
                    PropertyInfo property = nativeClass.GetProperty(methodName);
                    if (property != null)
                    {
                        object result = property.GetValue(nativeObjOrClassName.NativeObjectValue, null);
                        return new LispVariant(result);
                    }
                }
                throw new LispException("Bad method for class " + methodName, nativeObjOrClassName.Token.LineNo);
            }
        }

        static private LispVariant GetNativeMethods(object[] args, LispScope scope)
        {
            var nativeObjOrClassName = ((LispVariant)args[0]);

            Type nativeClass = null;
            if (nativeObjOrClassName.IsString || nativeObjOrClassName.IsSymbol)
            {
                nativeClass = Type.GetType(nativeObjOrClassName.ToString());
            }
            else
            {
                nativeClass = nativeObjOrClassName.NativeObjectValue.GetType();
            }

            MethodInfo[] methods = nativeClass.GetMethods();
            var result = methods.Where(elem => elem.IsPublic).Select(elem => elem.Name).ToList();

            return new LispVariant(result);
        }

        static private object[] ConvertAllToNative(object[] lstLispVariants, ParameterInfo[] parameterInfos)
        {
            var result = new object[lstLispVariants.Length];
            for (var i = 0; i < parameterInfos.Length; i++)
            {
                result[i] = ConvertToNative(lstLispVariants[i], parameterInfos[i]);
            }
            return result;
        }

        static private object ConvertToNative(object value, ParameterInfo parameterInfo)
        {
            var lispVariant = (LispVariant)value;
            if (parameterInfo.ParameterType.Name == "Int32")
            {
                return lispVariant.ToInt();
            }
            if (parameterInfo.ParameterType.Name == "Double")
            {
                return lispVariant.ToDouble();
            }
            if (parameterInfo.ParameterType.Name == "Bool")
            {
                return lispVariant.ToBool();
            }
            if (parameterInfo.ParameterType.Name == "String")
            {
                return lispVariant.ToString();
            }
            return value;
        }

        private static object UnQuoteIfNeeded(object item, LispScope scope)
        {
            var value = item as LispVariant;
            if (value != null && (value.IsUnQuoted == LispUnQuoteModus.UnQuote || value.IsUnQuoted == LispUnQuoteModus.UnQuoteSplicing))
            {
                return scope[value.StringValue];
            }
            return item;
        }

        private static LispVariant ArithmetricOperation(IEnumerable<object> args, Func<LispVariant, LispVariant, LispVariant> op)
        {
            LispVariant result = null;
            foreach (var elem in args)
            {
                if (result == null)
                {
                    result = new LispVariant(elem);
                }
                else
                {
                    result = op(result, new LispVariant(elem));
                }
            }
            return result;
        }

        private static LispVariant CompareOperation(object[] args, Func<LispVariant, LispVariant, LispVariant> op, LispScope scope)
        {
            CheckArgs("compare-op", 2, args, scope);

            var arg1 = (LispVariant)args[0];
            var arg2 = (LispVariant)args[1];
            LispVariant result = op(arg1, arg2);
            return result;
        }

        private static object CreateFunction(Func<object[], LispScope, LispVariant> func, string signature = null, bool isBuiltin = true, bool isSpecialForm = false, bool isEvalInExpand = false)
        {
            return new LispVariant(new LispFunctionWrapper(func, signature, isBuiltin, isSpecialForm, isEvalInExpand));
        }

        private static string GetPositionOfPreviousTokenForSymbol(object symbol, LispScope scope)
        {
            var sym = (LispVariant)symbol;
            var prevToken = scope.GetPreviousToken(sym.Token);
            return GetPositionOfToken(prevToken);
        }

        private static string GetPositionOfToken(LispToken token)
        {
            var infos = GetPosInfo(token);
            return " (total-pos=" + infos.Item1 + " line=" + infos.Item2 + ")";
        }

        private static void CheckArgs(string name, int count, object[] args, LispScope scope)
        {
            if (args.Length != count)
            {
                throw new LispException(BadArgumentCount + name + GetPositionOfPreviousTokenForSymbol(args[0], scope));
            }
        }

        private static LispVariant CheckForFunction(string functionName, object arg0, LispScope scope)
        {
            var function = (LispVariant)arg0;
            if (!function.IsFunction)
            {
                throw new LispException("no function in " + functionName + GetPositionOfPreviousTokenForSymbol(arg0, scope), function.Token.LineNo);
            }
            return function;
        }

        private static IEnumerable<object> CheckForList(string functionName, object arg0, LispScope scope)
        {
            if (arg0 is object[])
            {
                return (IEnumerable<object>)arg0;
            }
            var function = (LispVariant)arg0;
            if (!function.IsList)
            {
                throw new LispException("no list in " + functionName + GetPositionOfPreviousTokenForSymbol(arg0, scope), function.Token.LineNo);
            }
            return function.ListValue;
        }

        private static IEnumerable<object> ToEnumerable(object obj)
        {
            var enumerable = obj as IEnumerable<object>;
            if (enumerable != null)
            {
                return enumerable;
            }
            var variant = obj as LispVariant;
            return variant != null && variant.IsList ? variant.ListValue : null;
        }

        private static LispVariant defn_form_helper(object[] args, LispScope scope, string name)
        {
            CheckArgs(name, 3, args, scope);

            var fn = ((LispVariant)scope.GlobalScope[Fn]).FunctionValue;
            scope.UserData = args[0].ToString();
            var resFn = fn.Function(new[] { args[1], args[2] }, scope);
            scope.UserData = null;

            var defFcn = ((LispVariant)scope.GlobalScope[name]).FunctionValue;
            return defFcn.Function(new[] { args[0], resFn }, scope);
        }

        private static LispVariant def_form_helper(object[] args, LispScope scope, string name, LispScope scopeToSet)
        {
            CheckArgs(name, 2, args, scope);

            var symbol = EvalArgIfNeeded(args[0], scope);
            if (!symbol.IsSymbol)
            {
                throw new LispException("Symbol expected" + GetPositionOfPreviousTokenForSymbol(symbol, scope), symbol.Token.LineNo);
            }
            scopeToSet[symbol.ToString()] = LispInterpreter.EvalAst(args[1], scope);
            return new LispVariant(args[1]);
        }

        private static LispVariant EvalArgIfNeeded(object arg, LispScope scope)
        {
            return (arg is IEnumerable<object>) ? LispInterpreter.EvalAst(arg, scope) : (LispVariant)arg;
        }

        #endregion
    }
}
