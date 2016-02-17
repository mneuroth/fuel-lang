using System;
using System.Collections.Generic;
using System.IO;
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
        /// Gets and sets the tracing modus.
        /// </summary>
        public bool Tracing { get; set; }

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
        /// Gets or sets the current module name and path.
        /// </summary>
        /// <value>
        /// The module name and path.
        /// </value>
        public string ModuleName { get; set; }

        /// <summary>
        /// Gets or sets the current line number.
        /// </summary>
        public int LineNumber { get; set; }

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
        /// <param name="moduleName">The current module name for the scope.</param>
        public LispScope(string fcnName = "", LispScope globalScope = null, string moduleName = null)
        {
            Name = fcnName;
            GlobalScope = globalScope ?? this;
            ModuleName = moduleName;
            LineNumber = -1;
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
            string stackInfo = DumpStackToString(currentLevel);
            Console.WriteLine(stackInfo);
        }

        public string DumpStackToString(int currentLevel = -1)
        {
            string ret = string.Empty;
            LispScope current = this;
            int i = GetCallStackSize();
            do
            {
                string currentItem = currentLevel == i ? "-->" : "   ";

                ret += string.Format("{0,3}{1,5} {2} lineno={3} module={4}\n", currentItem, i, current.Name, current.LineNumber, current.ModuleName);
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
            Dump(v => v.IsFunction && v.FunctionValue.IsBuiltin);
// TODO --> dump also module functions and maybe macros !?
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
    /// Class to hold informations about macro expansions
    /// </summary>
    public class LispMacroExpand : Tuple<IEnumerable<object>, IEnumerable<object>>
    {
        public IEnumerable<object> FormalParameters
        {
            get
            {
                return Item1;
            }
        }

        public IEnumerable<object> Expression
        {
            get
            {
                return Item2;
            }
        }

        public LispMacroExpand(IEnumerable<object> parameters, IEnumerable<object> expression)
            : base(parameters, expression)
        {     
        }
    }

    /// <summary>
    /// Wrapper for an environment function or special form.
    /// </summary>
    public struct LispFunctionWrapper
    {
        #region properties

        public Func<object[], LispScope, LispVariant> Function { get; private set; }

        public string Signature { get; private set; }

        public string ModuleName { get; private set; }

        public bool IsBuiltin { get; private set; }

        public bool IsSpecialForm { get; private set; }

        public bool IsEvalInExpand { get; private set; }

        #endregion

        #region constroctor(s)

        public LispFunctionWrapper(Func<object[], LispScope, LispVariant> func, string signature, bool isBuiltin, bool isSpecialForm = false, bool isEvalInExpand = false, string moduleName = null)
            : this()
        {
            Function = func;
            Signature = signature;
            ModuleName = ModuleName;
            IsBuiltin = isBuiltin;
            IsSpecialForm = isSpecialForm;
            IsEvalInExpand = isEvalInExpand;
        }

        public LispFunctionWrapper(Func<LispVariant, LispScope, LispVariant> func, string signature, bool isBuiltin, bool isSpecialForm = false, bool isEvalInExpand = false, string moduleName = null)
            : this((args, scope) => func((LispVariant)(args[0]), scope), signature, isBuiltin, isSpecialForm, isEvalInExpand)
        {
        }

        public LispFunctionWrapper(Func<LispVariant, LispVariant, LispScope, LispVariant> func, string signature, bool isBuiltin, bool isSpecialForm = false, bool isEvalInExpand = false, string moduleName = null)
            : this((args, scope) => func((LispVariant)(args[0]), (LispVariant)(args[1]), scope), signature, isBuiltin, isSpecialForm, isEvalInExpand)
        {
        }

        public LispFunctionWrapper(Func<LispVariant, LispVariant, LispVariant, LispScope, LispVariant> func, string signature, bool isBuiltin, bool isSpecialForm = false, bool isEvalInExpand = false, string moduleName = null)
            : this((args, scope) => func((LispVariant)(args[0]), (LispVariant)(args[1]), (LispVariant)(args[2]), scope), signature, isBuiltin, isSpecialForm, isEvalInExpand)
        {
        }

        public LispFunctionWrapper(Func<LispVariant, LispVariant, LispVariant, LispVariant, LispScope, LispVariant> func, string signature, bool isBuiltin, bool isSpecialForm = false, bool isEvalInExpand = false, string moduleName = null)
            : this((args, scope) => func((LispVariant)(args[0]), (LispVariant)(args[1]), (LispVariant)(args[2]), (LispVariant)(args[3]), scope), signature, isBuiltin, isSpecialForm, isEvalInExpand, moduleName)
        {
        }

        public LispFunctionWrapper(Func<LispVariant, LispVariant, LispVariant, LispVariant, LispVariant, LispScope, LispVariant> func, string signature, bool isBuiltin, bool isSpecialForm = false, bool isEvalInExpand = false, string moduleName = null)
            : this((args, scope) => func((LispVariant)(args[0]), (LispVariant)(args[1]), (LispVariant)(args[2]), (LispVariant)(args[3]), (LispVariant)(args[4]), scope), signature, isBuiltin, isSpecialForm, isEvalInExpand, moduleName)
        {
        }

        public LispFunctionWrapper(Func<LispVariant, LispVariant, LispVariant, LispVariant, LispVariant, LispVariant, LispScope, LispVariant> func, string signature, bool isBuiltin, bool isSpecialForm = false, bool isEvalInExpand = false, string moduleName = null)
            : this((args, scope) => func((LispVariant)(args[0]), (LispVariant)(args[1]), (LispVariant)(args[2]), (LispVariant)(args[3]), (LispVariant)(args[4]), (LispVariant)(args[5]), scope), signature, isBuiltin, isSpecialForm, isEvalInExpand, moduleName)
        {
        }

        public LispFunctionWrapper(Func<LispVariant, LispVariant, LispVariant, LispVariant, LispVariant, LispVariant, LispVariant, LispScope, LispVariant> func, string signature, bool isBuiltin, bool isSpecialForm = false, bool isEvalInExpand = false, string moduleName = null)
            : this((args, scope) => func((LispVariant)(args[0]), (LispVariant)(args[1]), (LispVariant)(args[2]), (LispVariant)(args[3]), (LispVariant)(args[4]), (LispVariant)(args[5]), (LispVariant)(args[6]), scope), signature, isBuiltin, isSpecialForm, isEvalInExpand, moduleName)
        {
        }

        public LispFunctionWrapper(Func<LispVariant, LispVariant, LispVariant, LispVariant, LispVariant, LispVariant, LispVariant, LispVariant, LispScope, LispVariant> func, string signature, bool isBuiltin, bool isSpecialForm = false, bool isEvalInExpand = false, string moduleName = null)
            : this((args, scope) => func((LispVariant)(args[0]), (LispVariant)(args[1]), (LispVariant)(args[2]), (LispVariant)(args[3]), (LispVariant)(args[4]), (LispVariant)(args[5]), (LispVariant)(args[6]), (LispVariant)(args[7]), scope), signature, isBuiltin, isSpecialForm, isEvalInExpand, moduleName)
        {
        }

        public LispFunctionWrapper(Func<LispVariant, LispVariant, LispVariant, LispVariant, LispVariant, LispVariant, LispVariant, LispVariant, LispVariant, LispScope, LispVariant> func, string signature, bool isBuiltin, bool isSpecialForm = false, bool isEvalInExpand = false, string moduleName = null)
            : this((args, scope) => func((LispVariant)(args[0]), (LispVariant)(args[1]), (LispVariant)(args[2]), (LispVariant)(args[3]), (LispVariant)(args[4]), (LispVariant)(args[5]), (LispVariant)(args[6]), (LispVariant)(args[7]), (LispVariant)(args[8]), scope), signature, isBuiltin, isSpecialForm, isEvalInExpand, moduleName)
        {
        }

        public LispFunctionWrapper(Func<LispVariant, LispVariant, LispVariant, LispVariant, LispVariant, LispVariant, LispVariant, LispVariant, LispVariant, LispVariant, LispScope, LispVariant> func, string signature, bool isBuiltin, bool isSpecialForm = false, bool isEvalInExpand = false, string moduleName = null)
            : this((args, scope) => func((LispVariant)(args[0]), (LispVariant)(args[1]), (LispVariant)(args[2]), (LispVariant)(args[3]), (LispVariant)(args[4]), (LispVariant)(args[5]), (LispVariant)(args[6]), (LispVariant)(args[7]), (LispVariant)(args[8]), (LispVariant)(args[9]), scope), signature, isBuiltin, isSpecialForm, isEvalInExpand, moduleName)
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
        private const string DefineMacroExpand = "define-macro-expand";
        private const string Lambda = "lambda";
        private const string Modules = MetaTag + "modules" + MetaTag;
        private const string Macros = MetaTag + "macros" + MetaTag;
        private const string Tracebuffer = MetaTag + "tracebuffer" + MetaTag;
        private const string Traceon = MetaTag + "traceon" + MetaTag;
            
        public const string Apply = "apply";
        public const string Eval = "eval";
        public const string EvalStr = "evalstr";
        public const string Quote = "quote";
        public const string Quasiquote = "quasiquote";

        public const string Sym = "sym";
        public const string Str = "str";

        private const string BadArgumentCount = "bad argument count in ";

        #endregion

        #region public methods of environment

        public static bool IsInModules(object funcName, LispScope globalScope)
        {
            return ExistsItem(funcName, globalScope, Modules);
        }

        public static object GetFunctionInModules(object funcName, LispScope globalScope)
        {
            return QueryItem(funcName, globalScope, Modules);            
        }

        public static bool IsMacro(object funcName, LispScope globalScope)
        {
            return ExistsItem(funcName, globalScope, Macros);
        }

        public static object GetMacro(object funcName, LispScope globalScope)
        {
            return QueryItem(funcName, globalScope, Macros);
        }

        public static bool IsExpression(object item)
        {
            return (item is LispVariant && ((LispVariant)item).IsList) ||
                   (item is IEnumerable<object>);
        }

        public static IEnumerable<object> GetExpression(object item)
        {
            if (item is LispVariant && ((LispVariant)item).IsList)
            {
                return ((LispVariant)item).ListValue;
            }
            if (item is IEnumerable<object>)
            {
                return (IEnumerable<object>)item;
            }
            return new List<object>();
        }

        public static LispScope CreateDefaultScope()
        {
            var scope = new LispScope(MainScope);

            // meta information fields
            scope[Modules] = new LispScope(Modules, scope);
            scope[Macros] = new LispScope(Macros, scope);
            scope[Tracebuffer] = new StringBuilder();
            scope[Traceon] = false;

            // infrastructure functions
            scope["fuel"] = CreateFunction(Fuel);
            scope["copyright"] = CreateFunction(Copyright);
            scope["help"] = CreateFunction(Help);
            scope["break"] = CreateFunction(Break);
            scope["vars"] = CreateFunction(Vars);
            scope["trace"] = CreateFunction(TracePrint);
            scope["gettrace"] = CreateFunction(GetTracePrint);
            scope["import"] = CreateFunction(Import);

            // access to .NET
            scope["native-methods"] = CreateFunction(GetNativeMethods, "(native-methods native-obj|class-name) -> (method-name, argument-count");
            scope["call"] = CreateFunction(CallNative, "(call native-obj|class-name [method-name [args...]]|[args...])");    // call native function
            // Macro: (register-native full-class-name lisp-name) --> erzeugt konstruktoren und zugriffsmethoden fuer klasse
            // --> (lisp-name-create args)
            // --> (lisp-name-method obj args)

            // interpreter functions
            scope["type"] = CreateFunction(GetType);
            scope["typestr"] = CreateFunction(GetTypeString);
            scope["nop"] = CreateFunction(Nop);
            scope["return"] = CreateFunction(Return);
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
            scope["cons"] = CreateFunction(Cons);
            scope["len"] = CreateFunction(Length);
            scope["first"] = CreateFunction(First);
            scope["car"] = CreateFunction(First);
            scope["rest"] = CreateFunction(Rest);
            scope["cdr"] = CreateFunction(Rest);
            scope["nth"] = CreateFunction(Nth);
            scope["append"] = CreateFunction(Append);
            scope[Sym] = CreateFunction(Symbol);
            scope[Str] = CreateFunction(ConvertToString);

            scope[Apply] = CreateFunction(ApplyFcn);
            scope[Eval] = CreateFunction(EvalFcn);
            scope[EvalStr] = CreateFunction(EvalStrFcn);

            // special forms
            scope["and"] = CreateFunction(and_form, isSpecialForm: true);
            scope["or"] = CreateFunction(or_form, isSpecialForm: true);
            scope[Def] = CreateFunction(def_form, isSpecialForm: true);
            scope[Gdef] = CreateFunction(gdef_form, isSpecialForm: true);
            scope[Setf] = CreateFunction(setf_form, isSpecialForm: true);
            scope[DefineMacro] = CreateFunction(definemacro_form, isSpecialForm: true, isEvalInExpand: true);
            scope["macro-expand"] = CreateFunction(macroexpand_form, isSpecialForm: true, isEvalInExpand: true);
            scope["macro-expand-flat"] = CreateFunction(macroexpandflat_form, isSpecialForm: true, isEvalInExpand: true);
            scope[DefineMacroExpand] = CreateFunction(definemacroexpand_form, isSpecialForm: true, isEvalInExpand: true);
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

        private static LispVariant Fuel(object[] args, LispScope scope)
        {
            var text = new StringBuilder();
            text.Append(string.Format("fuel version {0} from {1}", Lisp.Version, Lisp.Date));
            return new LispVariant(text.ToString());
        }

        private static LispVariant Copyright(object[] args, LispScope scope)
        {
            var text = new StringBuilder();
            text.Append(string.Format("Copyright: {0} {1}", Lisp.License, Lisp.LicenseUrl));
            return new LispVariant(text.ToString());
        }

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

        private static LispVariant Import(object[] args, LispScope scope)
        {
            LispVariant result = new LispVariant();

            foreach (var module in args)
            {
                string code = string.Empty;
                string orgModuleFileName = ((LispVariant)module).StringValue;
                string fileName = orgModuleFileName;
                if (!File.Exists(fileName))
                {
                    // try default path Library\modulename.fuel
                    fileName = "." + Path.DirectorySeparatorChar + "Library" + Path.DirectorySeparatorChar + orgModuleFileName;
                    fileName = AddFileExtensionIfNeeded(fileName);
                    if (!File.Exists(fileName))
                    {
                        fileName = AppDomain.CurrentDomain.BaseDirectory + Path.DirectorySeparatorChar + "Library" + Path.DirectorySeparatorChar + orgModuleFileName;
                        fileName = AddFileExtensionIfNeeded(fileName);
                    }
                }
                if (File.Exists(fileName))
                {
                    code = File.ReadAllText(fileName);
                }
                else
                {
                    Console.WriteLine("WARNING: Library {0} not found! Tried path {1}", orgModuleFileName, fileName);
                }
                if (!string.IsNullOrEmpty(code))
                {
                    var importScope = new LispScope("import "+fileName, scope.GlobalScope, fileName);
                    scope.PushNextScope(importScope);

                    result = Lisp.Eval(code, importScope, fileName, updateFinishedFlag: false);

                    // merge new module into modules dictionary
                    importScope.ToList().ForEach(x => ((LispScope)scope.GlobalScope[Modules]).Add(x.Key, x.Value));

                    scope.PopNextScope();

                }
            }
            return result;
        }

        private static string AddFileExtensionIfNeeded(string fileName)
        {
            const string EXTENSION = ".fuel";

            if (!fileName.EndsWith(EXTENSION))
            {
                fileName += EXTENSION;
            }
            return fileName;
        }

        #endregion

        #region functions

        public static LispVariant Nop(object[] args, LispScope scope)
        {
            return new LispVariant();
        }

        public static LispVariant Return(object[] args, LispScope scope)
        {
            return new LispVariant(args[0]);
        }

        public static LispVariant GetType(object[] args, LispScope scope)
        {
            CheckArgs("type", 1, args, scope);

            var item = ((LispVariant)args[0]);
            return new LispVariant((int)item.Type);
        }

        public static LispVariant GetTypeString(object[] args, LispScope scope)
        {
            CheckArgs("typestr", 1, args, scope);

            var item = ((LispVariant)args[0]);
            return new LispVariant(item.TypeString);
        }

        public static LispVariant Print(object[] args, LispScope scope)
        {
            var text = GetStringRepresentation(args, scope);
            Console.WriteLine(text);
            return new LispVariant(text);
        }

        //public static LispVariant StringFcn(object[] args, LispScope scope)
        //{
        //    var text = GetStringRepresentation(args, scope, string.Empty);
        //    return new LispVariant(text);
        //}

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

        public static LispVariant Cons(object[] args, LispScope scope)
        {
            var result = new LispVariant(LispType.List, new List<object>());
            if (args.Length > 0)
            {
                result.Add(args[0]);                
            }
            if (args.Length > 1)
            {
                var item2 = (LispVariant)args[1];
                if (item2.IsList)
                {
                    foreach (var item in item2.ListValue)
                    {
                        result.Add(item);
                    }
                }
                else
                {
                    result.Add(args[1]);                    
                }
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

        public static LispVariant Append(object[] args, LispScope scope)
        {
            var result = new LispVariant(LispType.List, new List<object>());
            foreach (var listElement in args)
            {
                var lst = ((LispVariant)listElement).ListValue;
                foreach (var item in lst)
                {
                    result.Add(item);                    
                }
            }
            return result;
        }

        public static LispVariant Symbol(object[] args, LispScope scope)
        {
            CheckArgs(Sym, 1, args, scope);

            var symbol = args[0].ToString();
            return new LispVariant(LispType.Symbol, symbol);
        }

        public static LispVariant ConvertToString(object[] args, LispScope scope)
        {
            CheckArgs(Str, 1, args, scope);

            var value = args[0].ToString();
            return new LispVariant(LispType.String, value);
        }

        // used also for processing macros !
        public static LispVariant ApplyFcn(object[] args, LispScope scope)
        {
            CheckArgs(Apply, 2, args, scope);

            var fcn = LispInterpreter.EvalAst(args[0], scope);
            if (fcn.IsList)
            {
                fcn = LispInterpreter.EvalAst(fcn, scope);
                if (!fcn.IsFunction)
                {
                    return fcn;
                }
            }

            var arguments = (LispVariant)args[1];

            if (arguments.IsList)
            {
                var argumentsArray = arguments.ListValue.ToArray();
                //var evaluatedArgs = new object[argumentsArray.Length];
                //for (int i = 0; i < evaluatedArgs.Length; i++)
                //{
                //    // apply is no special form --> arguments are already evaluated !
                //    evaluatedArgs[i] = argumentsArray[i]; // old: LispInterpreter.EvalAst(argumentsArray[i], scope);
                //}

                //var result = fcn.FunctionValue.Function(evaluatedArgs, scope);
                var result = fcn.FunctionValue.Function(argumentsArray, scope);
                return result;
            }

            throw new LispException("expected list as arguments in apply");
        }

        public static LispVariant EvalFcn(object[] args, LispScope scope)
        {
            CheckArgs("eval", 1, args, scope);

            // convert LispVariant.List --> object[] needed for evaluation
            var variant = (LispVariant)args[0];
            object[] code = variant.ListValue.ToArray();
            var result = LispInterpreter.EvalAst(code, scope);
            return result;
        }

        public static LispVariant EvalStrFcn(object[] args, LispScope scope)
        {
            CheckArgs("evalstr", 1, args, scope);

            var variant = (LispVariant)args[0];
            var result = Lisp.Eval(variant.Value.ToString(), scope);
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

            return null;
        }

// TODO --> implementieren --> dynamisch code erzeugen und in den Ast einhaengen !
        // (define-macro-expand name (args) (expression))
        private static LispVariant definemacroexpand_form(object[] args, LispScope scope)
        {
            CheckArgs(DefineMacroExpand, 3, args, scope);

            object result = null;
            var macros = scope.GlobalScope[Macros] as LispScope;
            if (macros != null)
            {
                // allow macros in macros --> recursive call for ExpandMacros()
                result = LispInterpreter.ExpandMacros(GetExpression(args[2]), scope);
                macros[args[0].ToString()] = new LispMacroExpand(GetExpression(args[1]), result as IEnumerable<object>);
            }

            List<object> ret = new List<object>() { args[0], args[1], result };
            //return ret;
            //return new LispVariant(ret);
            return null; // TODO gulp working replace macro new LispVariant(result);
        }

// TODO --> ggf. entfernen
        private static LispVariant macroexpand_form(object[] args, LispScope scope)
        {
            CheckArgs(DefineMacro, 1, args, scope);

            return LispInterpreter.EvalAst(args[0], scope);
        }

// TODO --> ggf. entfernen
        private static LispVariant macroexpandflat_form(object[] args, LispScope scope)
        {
            var result = macroexpand_form(args, scope);

            //if (result is IEnumerable<object>)
            {
                var resultList = new List<object>();
                foreach (var item in GetExpression(result))
                {
                    resultList.Add(item);
                }
                return new LispVariant(resultList.ToArray());
            }

            //return result;
        }

        public static LispVariant quote_form(object[] args, LispScope scope)
        {
            CheckArgs(Quote, 1, args, scope);

            return new LispVariant(args[0]);
        }

        public static LispVariant quasiquote_form(object[] args, LispScope scope)
        {
            CheckArgs(Quasiquote, 1, args, scope);

            // unquote elements of list if needed
            var lst = GetExpression(args[0]);
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
            CheckArgs(If, 3, args, scope);

            var passed = LispInterpreter.EvalAst(args[0], scope).BoolValue;
            return LispInterpreter.EvalAst(passed ? args[1] : args[2], scope);
        }

        public static LispVariant while_form(object[] args, LispScope scope)
        {
            CheckArgs(While, 2, args, scope);

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
            string name = (string)scope.UserData;
            string moduleName = scope.ModuleName;
            
            Func<object[], LispScope, LispVariant> fcn =
                (localArgs, localScope) =>
                {
                    var childScope = new LispScope(name, localScope.GlobalScope, moduleName);
                    localScope.PushNextScope(childScope);

                    var i = 0;
                    IEnumerable<object> argsEnum = args[0] is LispVariant ? ((LispVariant)args[0]).ListValue : GetExpression(args[0]);
                    foreach (var arg in argsEnum)
                    {
                        childScope[arg.ToString()] = localArgs[i];
                        i++;
                    }

                    // save the current call stack to resolve variables in closures
                    childScope.ClosureChain = scope;

                    LispVariant ret = null;
                    try
                    {
                        ret = LispInterpreter.EvalAst(args[1], childScope);
                    }
                    catch (LispStopDebuggerException)
                    {
                        // forward a debugger stop exception to stop the debugger loop
                        throw;
                    }
                    catch (Exception ex)
                    {
                        // add the stack info and module name to the data of the exception
                        ex.Data[LispUtils.StackInfo] = childScope.DumpStackToString();
                        ex.Data[LispUtils.ModuleName] = childScope.ModuleName;

                        var debugger = scope.GlobalScope.Debugger;
                        if (debugger != null)
                        {
                            Console.WriteLine(ex);

                            debugger.InteractiveLoop(initialTopScope: childScope, currentAst: (IList<object>)(args[1]) /*new List<object> { info.Item2 }*/ );
                        }

                        throw;
                    }
                    localScope.PopNextScope();
                    return ret;
                };

            return new LispVariant(CreateFunction(fcn, isBuiltin: false, moduleName: scope.ModuleName));
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
                throw new LispException("Bad method for class " + methodName, nativeObjOrClassName.Token!=null ? nativeObjOrClassName.Token.LineNo : -1);
            }
        }

        static private LispVariant GetNativeMethods(object[] args, LispScope scope)
        {
            var nativeObjOrClassName = ((LispVariant)args[0]);

            Type nativeClass;
            if (nativeObjOrClassName.IsString || nativeObjOrClassName.IsSymbol)
            {
                nativeClass = Type.GetType(nativeObjOrClassName.ToString());
            }
            else
            {
                nativeClass = nativeObjOrClassName.NativeObjectValue.GetType();
            }

            MethodInfo[] methods = nativeClass.GetMethods();
            var result = methods.Where(elem => elem.IsPublic).Select(elem => new List<object> { elem.Name, elem.GetParameters().Count() }).ToList();

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

        private static object CreateFunction(Func<object[], LispScope, LispVariant> func, string signature = null, bool isBuiltin = true, bool isSpecialForm = false, bool isEvalInExpand = false, string moduleName = null)
        {
            return new LispVariant(new LispFunctionWrapper(func, signature, isBuiltin, isSpecialForm, isEvalInExpand, moduleName));
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
                return GetExpression(arg0);
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

        private static LispToken GetTokenFrom(object item)
        {
            if (item is LispVariant)
            {
                return ((LispVariant)item).Token;
            }
            return null;
        }

        private static LispVariant defn_form_helper(object[] args, LispScope scope, string name)
        {
            CheckArgs(name, 3, args, scope);

            var fn = ((LispVariant)scope.GlobalScope[Fn]).FunctionValue;
            scope.UserData = EvalArgIfNeeded(args[0], scope).ToString();
            var resFn = fn.Function(new[] { args[1], args[2] }, scope);
            scope.UserData = null;

            var defFcn = ((LispVariant)scope.GlobalScope[name]).FunctionValue;
            return defFcn.Function(new[] { args[0], resFn }, scope);
        }

        private static LispVariant def_form_helper(object[] args, LispScope scope, string name, LispScope scopeToSet)
        {
            CheckArgs(name, 2, args, scope);

            var symbol = EvalArgIfNeeded(args[0], scope);
            if (!(symbol.IsSymbol || symbol.IsString))
            {
                throw new LispException("Symbol expected" + GetPositionOfPreviousTokenForSymbol(symbol, scope), symbol.Token.LineNo);
            }
            var value = LispInterpreter.EvalAst(args[1], scope);
            scopeToSet[symbol.ToString()] = value;
            return new LispVariant(value);
        }

        private static LispVariant EvalArgIfNeeded(object arg, LispScope scope)
        {
            return (arg is IEnumerable<object>) ? LispInterpreter.EvalAst(arg, scope) : (LispVariant)arg;
        }

        private static string GetStringRepresentation(object[] args, LispScope scope, string separator = " ")
        {
            var text = String.Empty;
            foreach (var item in args)
            {
                if (text.Length > 0)
                {
                    text += separator;
                }
                text += item;
            }
            if (scope.ContainsKey(Traceon) && (bool)scope[Traceon])
            {
                var buffer = (StringBuilder)scope[Tracebuffer];
                buffer.Append(text);
            }
            return text;
        }

        private static object QueryItem(object funcName, LispScope globalScope, string key)
        {
            if (globalScope != null &&
                globalScope.ContainsKey(key) &&
                ((LispScope)globalScope[key]).ContainsKey(funcName.ToString()))
            {
                return ((LispScope)globalScope[key])[funcName.ToString()];
            }
            return null;
        }

        private static bool ExistsItem(object funcName, LispScope globalScope, string key)
        {
            if (globalScope != null &&
                globalScope.ContainsKey(key))
            {
                return ((LispScope)globalScope[key]).ContainsKey(funcName.ToString());
            }
            return false;
        }

        #endregion
    }
}
