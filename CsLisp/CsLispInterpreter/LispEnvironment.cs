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
using System.IO;
using System.Linq;
using System.Reflection;
using System.Text;

namespace CsLisp
{
    /// <summary>
    /// Class to hold informations about macro expansions at compile time.
    /// </summary>
    internal class LispMacroCompileTimeExpand : Tuple<IEnumerable<object>, IEnumerable<object>>
    {
        public IEnumerable<object> FormalArguments
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

        public LispMacroCompileTimeExpand(IEnumerable<object> parameters, IEnumerable<object> expression)
            : base(parameters, expression)
        {     
        }
    }

    /// <summary>
    /// Class to hold informations about macro expansions at run time.
    /// </summary>
    internal class LispMacroRuntimeEvaluate : LispMacroCompileTimeExpand
    {
        public LispMacroRuntimeEvaluate(object parameters, object expression)
            : base((IEnumerable<object>)parameters, (IEnumerable<object>)expression)
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

        public string Documentation { get; private set; }

        public string ModuleName { get; private set; }

        public bool IsBuiltin { get; private set; }

        public bool IsSpecialForm { get; private set; }

        public bool IsEvalInExpand { get; private set; }

        public string FormatedDoc
        {
            get
            {
                const string separator = "\n\n";
                const string splitter = "-------------------------------------------------" + separator;
                string name = "???";
                string signature = (Signature != null ? Signature : string.Empty);
                if (signature.Length > 0 && signature.StartsWith("("))
                {
                    name = signature.Substring(1, signature.IndexOf(" "));
                }
                name += separator;
                string syntax = "Syntax: " + signature + separator;
                string doc = (Documentation != null ? Documentation : "<not available>");
                doc += separator;
                return splitter + name + syntax + doc;
            }
        }

        #endregion

        #region constroctor(s)

        public LispFunctionWrapper(Func<object[], LispScope, LispVariant> func, string signature, string documentation, bool isBuiltin, bool isSpecialForm = false, bool isEvalInExpand = false, string moduleName = null)
            : this()
        {
            Function = func;
            Signature = signature;
            Documentation = documentation;
            ModuleName = moduleName;
            IsBuiltin = isBuiltin;
            IsSpecialForm = isSpecialForm;
            IsEvalInExpand = isEvalInExpand;
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
        public const string Builtin = "<builtin>";

        private const string MainScope = "<main>";

        private const string If = "if";
        private const string While = "while";
        private const string Begin = "begin";
        private const string Do = "do";
        private const string Or = "or";
        private const string And = "and";
        private const string Fn = "fn";
        private const string Def = "def";
        private const string Gdef = "gdef";
        private const string Setf = "setf";
        private const string Defn = "defn";
        private const string Gdefn = "gdefn";
        private const string MapFcn = "map";
        private const string ReduceFcn = "reduce";
        private const string DefineMacro = "define-macro";      // == define-macro-eval
        private const string DefineMacroEval = "define-macro-eval";
#if ENABLE_COMPILE_TIME_MACROS 
        private const string DefineMacroExpand = "define-macro-expand";
#endif
        private const string Lambda = "lambda";
        private const string Tracebuffer = MetaTag + "tracebuffer" + MetaTag;
        private const string Traceon = MetaTag + "traceon" + MetaTag;
        internal const string ArgsMeta = MetaTag + "args" + MetaTag;
        private const string AdditionalArgs = "_additionalArgs";

        public const string Macros = MetaTag + "macros" + MetaTag;
        public const string Modules = MetaTag + "modules" + MetaTag;

        private const string ArgsCount = "argscount";
        private const string Args = "args";
        public const string Apply = "apply";
        public const string Eval = "eval";
        public const string EvalStr = "evalstr";
        public const string Quote = "quote";
        public const string Quasiquote = "quasiquote";

        public const string Sym = "sym";
        public const string Str = "str";

        #endregion

        #region public methods of environment

        public static bool IsInModules(string funcName, LispScope scope)
        {
            object value;
            return FindFunctionInModules(funcName, scope, out value);
        }

        public static object GetFunctionInModules(string funcName, LispScope scope)
        {
            object result;
            FindFunctionInModules(funcName, scope, out result);
            return result;         
        }

        public static bool IsMacro(object funcName, LispScope scope)
        {
            return ExistsItem(funcName, scope, Macros);
        }

        public static object GetMacro(object funcName, LispScope scope)
        {
            return QueryItem(funcName, scope, Macros);
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
            scope["doc"] = CreateFunction(Documentation);
            scope["break"] = CreateFunction(Break);
            scope["vars"] = CreateFunction(Vars);
            scope["trace"] = CreateFunction(TracePrint);
            scope["gettrace"] = CreateFunction(GetTracePrint);
            scope["import"] = CreateFunction(Import);

            // access to .NET
            scope["native-methods"] = CreateFunction(GetNativeMethods, "(native-methods native-obj|class-name) -> (method-name, argument-count, is-static, net-method-name)");
            scope["native-fields"] = CreateFunction(GetNativeFields, "(native-fields native-obj|class-name) -> (property-name)");
            scope["field"] = CreateFunction(CallField, "(field native-obj|class-name field-name)");    // access native field
            scope["call"] = CreateFunction(CallNative, "(call native-obj|class-name [method-name [args...]]|[args...])");    // call native function
            scope["call-static"] = CreateFunction(CallStaticNative, "(call-static class-name method-name [args...])");    // call native static function
            // Macro: (register-native full-class-name lisp-name) --> erzeugt konstruktoren und zugriffsmethoden fuer klasse
            // --> (lisp-name-create args)
            // --> (lisp-name-method obj args)

            // interpreter functions
            scope["type"] = CreateFunction(GetType);
            scope["typestr"] = CreateFunction(GetTypeString);
            scope["nop"] = CreateFunction(Nop);
            scope["return"] = CreateFunction(Return);
            scope["print"] = CreateFunction(Print);
            scope["println"] = CreateFunction(PrintLn);

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
            scope["!="] = CreateFunction(NotEqualTest);

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

            scope[ArgsCount] = CreateFunction(ArgsCountFcn);
            scope[Args] = CreateFunction(ArgsFcn);
            scope[Apply] = CreateFunction(ApplyFcn);
            scope[Eval] = CreateFunction(EvalFcn);
            scope[EvalStr] = CreateFunction(EvalStrFcn);

            // special forms
            scope[And] = CreateFunction(and_form, isSpecialForm: true);
            scope[Or] = CreateFunction(or_form, isSpecialForm: true);
            scope[Def] = CreateFunction(def_form, isSpecialForm: true);
            scope[Gdef] = CreateFunction(gdef_form, isSpecialForm: true);
            scope[Setf] = CreateFunction(setf_form, isSpecialForm: true);
            // macros are:
            // a special form to control evaluation of function parameters inside the macro code
            // there are two options possible:
            //  - run time evaluation of macros
            //  - compile time replacement/expanding of macros
            scope[DefineMacro] = CreateFunction(definemacroevaluate_form, isSpecialForm: true);
            // run time evaluation for macros: 
            scope[DefineMacroEval] = CreateFunction(definemacroevaluate_form, isSpecialForm: true);
#if ENABLE_COMPILE_TIME_MACROS 
            // compile time expand for macros:
            scope[DefineMacroExpand] = CreateFunction(definemacroexpand_form, isSpecialForm: true, isEvalInExpand: true);
#endif

            scope[Quote] = CreateFunction(quote_form, isSpecialForm: true);
            scope[Quasiquote] = CreateFunction(quasiquote_form, isSpecialForm: true);
            scope[If] = CreateFunction(if_form, "(if cond then-block [else-block])", isSpecialForm: true);
            scope[While] = CreateFunction(while_form, "(while cond block)", isSpecialForm: true);
            scope[Do] = CreateFunction(do_form, "(do statement1 statement2 ...)", isSpecialForm: true);
            scope[Begin] = CreateFunction(do_form, "see: do", isSpecialForm: true);
            scope[Lambda] = CreateFunction(fn_form, "(lambda (arguments) block)", "defines a lambda function", isSpecialForm: true);
            scope[Fn] = CreateFunction(fn_form, "(fn (arguments) block)", isSpecialForm: true);
            scope[Defn] = CreateFunction(defn_form, "(defn name (args) block)", "defines a function", isSpecialForm: true);
            scope[Gdefn] = CreateFunction(gdefn_form, "(gdefn name (args) block)", "function defined in global scope", isSpecialForm: true);

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
            scope.GlobalScope.Output.WriteLine(helpText.ToString());
            return new LispVariant(helpText.ToString());
        }

        private static LispVariant Documentation(object[] args, LispScope scope)
        {
            var text = new StringBuilder();
            var tempOutputWriter = scope.GlobalScope.Output;
            scope.GlobalScope.Output = new StringWriter(text);
            scope.GlobalScope.DumpBuiltinFunctionsHelpFormated();
            scope.GlobalScope.Output = tempOutputWriter;
            return new LispVariant(text.ToString());
        }        

        private static LispVariant Break(object[] args, LispScope scope)
        {
            scope.GlobalScope.Output.WriteLine("break -> call stack:");
            scope.DumpStack(scope.GetCallStackSize());
            var debugger = scope.GlobalScope.Debugger;
            if (debugger != null)
            {
                debugger.InteractiveLoop(initialTopScope: scope);
            }
            else
            {
                scope.GlobalScope.Output.WriteLine("Warning: can not break, because no debugger support availabe!");
            }
            return new LispVariant();
        }

        private static LispVariant Vars(object[] args, LispScope scope)
        {
            scope.GlobalScope.Output.WriteLine("variables:");
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
                    // try the given library path (if available)
                    fileName = LispUtils.LibraryPath + Path.DirectorySeparatorChar + orgModuleFileName;
                    fileName = AddFileExtensionIfNeeded(fileName);
                    if (!File.Exists(fileName)) 
                    {
                        // try default path .\Library\modulename.fuel
                        fileName = "." + Path.DirectorySeparatorChar + "Library" + Path.DirectorySeparatorChar + orgModuleFileName;
                        fileName = AddFileExtensionIfNeeded(fileName);
                        if (!File.Exists(fileName))
                        {
                            // try default path <fuel.exe-path>\Library\modulename.fuel
                            fileName = AppDomain.CurrentDomain.BaseDirectory + Path.DirectorySeparatorChar + "Library" + Path.DirectorySeparatorChar + orgModuleFileName;
                            fileName = AddFileExtensionIfNeeded(fileName);
                            if (!File.Exists(fileName))
                            {
                                // try environment variable FUELPATH
                                string envPath = Environment.GetEnvironmentVariable("FUELPATH");
                                if (envPath != null) 
                                {
                                    fileName = envPath + Path.DirectorySeparatorChar + orgModuleFileName;
                                    fileName = AddFileExtensionIfNeeded(fileName);
                                }
                            }
                        }
                    }
                }
                if (File.Exists(fileName))
                {
                    code = File.ReadAllText(fileName);
                }
                else
                {
                    scope.GlobalScope.Output.WriteLine("WARNING: Library {0} not found! Tried path {1}", orgModuleFileName, fileName);
                }
                if (!string.IsNullOrEmpty(code))
                {
                    var importScope = new LispScope("import "+fileName, scope.GlobalScope, fileName);
                    scope.PushNextScope(importScope);

                    result = Lisp.Eval(code, importScope, fileName);

                    // add new module to modules scope
                    ((LispScope)scope.GlobalScope[Modules]).Add(fileName, importScope);

                    scope.PopNextScope();

                }
            }
            return result;
        }

        private static string AddFileExtensionIfNeeded(string fileName)
        {
            const string extension = ".fuel";

            if (!fileName.EndsWith(extension))
            {
                fileName += extension;
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
            scope.GlobalScope.Output.Write(text);
            return new LispVariant(text);
        }

        public static LispVariant PrintLn(object[] args, LispScope scope)
        {
            var text = GetStringRepresentation(args, scope);
            scope.GlobalScope.Output.WriteLine(text);
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

        public static LispVariant NotEqualTest(object[] args, LispScope scope)
        {
            return CompareOperation(args, (l, r) => new LispVariant(!LispVariant.EqualOp(l, r)), scope);
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
            // convert native object into a readable form
            // used for: (println (str nativeLst))
            if (args[0] is LispVariant)
            {
                var variant = (LispVariant)args[0];
                if (variant.IsNativeObject)
                {
                    value = variant.NativeObjectStringRepresentation;
                }                
            }
            return new LispVariant(LispType.String, value);
        }

        public static LispVariant ArgsCountFcn(object[] args, LispScope scope)
        {
            CheckArgs(Apply, 0, args, scope);

            return new LispVariant(((LispVariant)scope[ArgsMeta]).ListValue.Count());
        }

        public static LispVariant ArgsFcn(object[] args, LispScope scope)
        {
            CheckArgs(Apply, 1, args, scope);

            var index = ((LispVariant) args[0]).IntValue;
            var array = ((LispVariant) scope[ArgsMeta]).ListValue.ToArray();
            if (index >= 0 && index < array.Length)
            {
                return new LispVariant(array[index]);                
            }
            throw new LispException(string.Format("Index out of range in args function (index={0} max={1})", index, array.Length));
        }

        public static LispVariant ApplyFcn(object[] args, LispScope scope)
        {
            CheckArgs(Apply, 2, args, scope);

            var fcn = LispInterpreter.EvalAst(args[0], scope);

            var arguments = (LispVariant)args[1];

            if (arguments.IsList)
            {
                var argumentsArray = arguments.ListValue.ToArray();
                var result = fcn.FunctionValue.Function(argumentsArray, scope);
                return result;
            }

            throw new LispException("Expected list as arguments in apply", scope);
        }

        public static LispVariant EvalFcn(object[] args, LispScope scope)
        {
            CheckArgs("eval", 1, args, scope);

            LispVariant result;
            // convert LispVariant.List --> object[] needed for evaluation
            var variant = (LispVariant)args[0];
            if (variant.IsList)
            {
                object[] code = variant.ListValue.ToArray();
                result = LispInterpreter.EvalAst(code, scope);
            }
            else
            {
                // if a single value is given for evaluation --> just return value !
                result = variant;
            }
            return result;
        }

        public static LispVariant EvalStrFcn(object[] args, LispScope scope)
        {
            CheckArgs("evalstr", 1, args, scope);

            var variant = (LispVariant)args[0];
            var result = Lisp.Eval(variant.Value.ToString(), scope, scope.ModuleName);
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
            scope.SetInScopes(symbolName, value);
            return value;
        }

        private static LispVariant definemacroevaluate_form(object[] args, LispScope scope)
        {
            CheckArgs(DefineMacroEval, 3, args, scope);

            var macros = scope.GlobalScope[Macros] as LispScope;
            if (macros != null)
            {
                macros[args[0].ToString()] = new LispMacroRuntimeEvaluate(args[1], args[2]);
            }

            return null;
        }

#if ENABLE_COMPILE_TIME_MACROS 

        // (define-macro-expand name (args) (expression))
        private static LispVariant definemacroexpand_form(object[] args, LispScope scope)
        {
            CheckArgs(DefineMacroExpand, 3, args, scope);

            var macros = scope.GlobalScope[Macros] as LispScope;
            if (macros != null)
            {
                // allow macros in macros --> recursive call for ExpandMacros()
                object result = LispInterpreter.ExpandMacros(GetExpression(args[2]), scope);
                macros[args[0].ToString()] = new LispMacroCompileTimeExpand(GetExpression(args[1]), result as IEnumerable<object>);
            }

            return null;
        }

#endif

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
            if (!(args.Length == 2 || args.Length == 3))
            {
                // throw exception
                CheckArgs(If, -1, args, scope);                
            }

            var passed = LispInterpreter.EvalAst(args[0], scope).BoolValue;
            var elseCode = args.Length > 2 ? args[2] : null;
            return LispInterpreter.EvalAst(passed ? args[1] : elseCode, scope);
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
                    throw new LispException("List expected in do", ((LispVariant)statement).Token, scope.ModuleName, scope.DumpStackToString());
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

                    // add formal arguments to current scope
                    var i = 0;
                    var formalArgs = (args[0] is LispVariant ? ((LispVariant)args[0]).ListValue : GetExpression(args[0])).ToArray();
                    foreach (var arg in formalArgs)
                    {
                        childScope[arg.ToString()] = localArgs[i];
                        i++;
                    }

                    // support args function for accessing all given parameters
                    childScope[ArgsMeta] = new LispVariant(localArgs);
                    int formalArgsCount = formalArgs.Count();
                    if (localArgs.Length > formalArgsCount)
                    {
                        var additionalArgs = new object[localArgs.Length - formalArgsCount];
                        for (int n = 0; n < localArgs.Length - formalArgsCount; n++)
                        {
                            additionalArgs[n] = localArgs[n + formalArgsCount];
                        }
                        childScope[AdditionalArgs] = new LispVariant(additionalArgs);
                    }

                    // save the current call stack to resolve variables in closures
                    childScope.ClosureChain = scope;

                    LispVariant ret;
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
                        ex.AddModuleNameAndStackInfos(childScope.ModuleName, childScope.DumpStackToString());
                        ex.AddTokenInfos(childScope.CurrentToken);

                        var debugger = scope.GlobalScope.Debugger;
                        if (debugger != null)
                        {
                            scope.GlobalScope.Output.WriteLine(ex);

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

        #region private methods

        private static LispVariant CallStaticNative(object[] args, LispScope scope)
        {
            var className = ((LispVariant)args[0]);
            var methodName = args.Length > 1 ? args[1].ToString() : string.Empty;

            if (className.IsString || className.IsSymbol)
            {
                var callArgs = GetCallArgs(args);

                Type nativeClass = Type.GetType(className.ToString());
                if (nativeClass != null)
                {
                    MethodInfo method;
                    try
                    {
                        method = nativeClass.GetMethod(methodName);                        
                    }
                    catch(AmbiguousMatchException)
                    {
                        // this exception should not happen, becuase
                        // overloaded method will be handled in native-methods 
                        // in this functions all overloaded methods get a unique name
                        // i. e. Math-Abs --> Math-Abs-1, Math-Abs-2, ...
                        method = null;
                    }
                    if (method != null)
                    {
                        ParameterInfo[] parameterInfos = method.GetParameters();
                        object result = method.Invoke(null, ConvertAllToNative(callArgs, parameterInfos));
                        return new LispVariant(result);
                    }                    
                }
            }
            throw new LispException("Bad static method " + methodName + " for class " + className, scope);                
        }

        private static Type[] GetTypes(object[] objects)
        {
            return objects.Select(o =>
            {
                if (o is LispVariant)
                {
                    return ((LispVariant) o).Value.GetType();
                }
                return o.GetType();
            }).ToArray();
        }

        private static LispVariant CallField(object[] args, LispScope scope)
        {
            var className = ((LispVariant)args[0]);
            var fieldName = args.Length > 1 ? args[1].ToString() : string.Empty;

            if (className.IsString || className.IsSymbol)
            {
                Type nativeClass = Type.GetType(className.ToString());
                if (nativeClass != null)
                {
                    FieldInfo field = nativeClass.GetField(fieldName);
                    if (field != null)
                    {
                        object result = field.GetValue(null);
                        return new LispVariant(result);
                    }
                }
            }
            throw new LispException("Bad field " + fieldName + " for class " + className, scope);
        }

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
                    var argTypes = GetTypes(callArgs);
                    ConstructorInfo constructor = nativeClass.GetConstructor(argTypes);

                    // if no default constructor is found try to resolve a constructor with arguments
                    if (constructor == null)
                    {
                        var additionalArgs = scope.GetAdditionalArgs();
                        constructor = nativeClass.GetConstructor(GetTypes(additionalArgs));
                        // copy additional args into callArgs for constructor call
                        callArgs = new object[additionalArgs.Length];
                        for (int j = 0; j < callArgs.Length; j++)
                        {
                            callArgs[j] = ((LispVariant) (additionalArgs[j])).Value;
                        }
                    }
                    
                    // optional improvment: get all constructors and try to resolve:
                    // ConstructorInfo[] constructors = nativeClass.GetConstructors();

                    if (constructor != null)
                    {
                        object result = constructor.Invoke(callArgs);
                        return new LispVariant(LispType.NativeObject, result);
                    }
                }
                throw new LispException("Bad constructor for class " + nativeObjOrClassName, scope);
            }
            else
            {
                // method call
                var methodName = args.Length > 1 ? args[1].ToString() : string.Empty;
                var callArgs = GetCallArgs(args);

                nativeClass = nativeObjOrClassName.NativeObjectValue.GetType();
                MethodInfo method;
                try
                {
                    method = nativeClass.GetMethod(methodName);
                }
                catch (AmbiguousMatchException)
                {
                    // process overloaded methods, try to resolve method via types of given arguments
                    // example: List-Sort
                    var callArgsTypes = GetTypes(callArgs);
                    method = nativeClass.GetMethod(methodName, callArgsTypes);
                }
                if (method != null)
                {
                    ParameterInfo[] parameterInfos = method.GetParameters();
                    object result = method.Invoke(nativeObjOrClassName.NativeObjectValue, ConvertAllToNative(callArgs, parameterInfos));
                    return new LispVariant(result);
                }
                // try to resolve as property (really needed here?)
                //PropertyInfo property = nativeClass.GetProperty(methodName);
                //if (property != null)
                //{
                //    object result = property.GetValue(nativeObjOrClassName.NativeObjectValue, null);
                //    return new LispVariant(result);
                //}
                throw new LispException("Bad method for class " + methodName, scope);
            }
        }

        static private LispVariant GetNativeFields(object[] args, LispScope scope)
        {
            var nativeClass = GetNativeClass(args[0]);

            var properties = nativeClass.GetFields();

            var result = properties.Select(elem => elem.Name ).ToList();

            return new LispVariant(result);
        }

        static private LispVariant GetNativeMethods(object[] args, LispScope scope)
        {
            var nativeClass = GetNativeClass(args[0]);

            MethodInfo[] methods = nativeClass.GetMethods();
            List<List<object>> result = methods.Where(elem => elem.IsPublic).Select(elem => new List<object> { elem.Name, elem.GetParameters().Count(), elem.IsStatic, elem.Name }).ToList();

            // do name mangling for overloaded methods --> add number for method with same names 
            var newResult = new List<List<object>>();
            var methodNames = new Dictionary<string, int>();
            foreach (List<object> methodInfo in result)
            {
                var methodName = (string)methodInfo.ElementAt(0);
                if (methodNames.ContainsKey(methodName))
                {
                    methodNames[methodName] += 1;
                    newResult.Add(new List<object> { string.Format("{0}-{1}", methodInfo[0], methodNames[methodName]), methodInfo[1], methodInfo[2], methodName });
                }
                else
                {
                    methodNames[methodName] = 1;
                    newResult.Add(methodInfo);
                }
            }

            return new LispVariant(newResult);
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

        private static object CreateFunction(Func<object[], LispScope, LispVariant> func, string signature = null, string documentation = null, bool isBuiltin = true, bool isSpecialForm = false, bool isEvalInExpand = false, string moduleName = Builtin)
        {
            return new LispVariant(new LispFunctionWrapper(func, signature, documentation, isBuiltin, isSpecialForm, isEvalInExpand, moduleName));
        }

        private static void CheckArgs(string name, int count, object[] args, LispScope scope)
        {
            if (count < 0 || args.Length != count)
            {
                throw new LispException(string.Format("Bad argument count in {0}, has {1} expected {2}", name, args.Length, count), scope);
            }
        }

        private static LispVariant CheckForFunction(string functionName, object arg0, LispScope scope)
        {
            var function = (LispVariant)arg0;
            if (!function.IsFunction)
            {
                throw new LispException("No function in " + functionName, scope);
            }
            return function;
        }

        private static IEnumerable<object> CheckForList(string functionName, object listObj, LispScope scope)
        {
            if (listObj is object[])
            {
                return GetExpression(listObj);
            }
            var value = (LispVariant)listObj;
            if (value.IsNativeObject && (value.Value is IEnumerable<object>))
            {
                return (IEnumerable<object>)value.Value;
            }
            if (!value.IsList)
            {
                throw new LispException("No list in " + functionName, scope.GetPreviousToken(((LispVariant)listObj).Token), scope.ModuleName, scope.DumpStackToString());
            }
            return value.ListValue;
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
                throw new LispException("Symbol expected", scope);
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

        private static object QueryItem(object funcName, LispScope scope, string key)
        {
            if (scope != null &&
                scope.ContainsKey(key) &&
                ((LispScope)scope[key]).ContainsKey(funcName.ToString()))
            {
                return ((LispScope)scope[key])[funcName.ToString()];
            }
            return null;
        }

        private static bool ExistsItem(object funcName, LispScope scope, string key)
        {
            if (scope != null &&
                scope.ContainsKey(key))
            {
                return ((LispScope)scope[key]).ContainsKey(funcName.ToString());
            }
            return false;
        }

        private static bool FindFunctionInModules(string funcName, LispScope scope, out object foundValue)
        {
            foundValue = null;
            var importedModules = (LispScope)scope.GlobalScope[Modules];
            foreach (KeyValuePair<string, object> kv in importedModules)
            {
                var module = (LispScope)kv.Value;
                if (module.ContainsKey(funcName))
                {
                    foundValue = module[funcName];
                    return true;
                }
            }
            return false;
        }

        private static object[] GetCallArgs(object[] args)
        {
            var callArgs = new object[args.Length > 1 ? args.Length - 2 : 0];
            if (args.Length > 2)
            {
                Array.Copy(args, 2, callArgs, 0, args.Length - 2);
            }
            return callArgs;
        }

        static private Type GetNativeClass(object item)
        {
            var nativeObjOrClassName = ((LispVariant)item);

            Type nativeClass;
            if (nativeObjOrClassName.IsString || nativeObjOrClassName.IsSymbol)
            {
                nativeClass = Type.GetType(nativeObjOrClassName.ToString());
            }
            else
            {
                nativeClass = nativeObjOrClassName.NativeObjectValue.GetType();
            }

            return nativeClass;
        }

        #endregion
    }
}
