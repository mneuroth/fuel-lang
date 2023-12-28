/*
 * FUEL(isp) is a fast usable embeddable lisp interpreter.
 *
 * Copyright (c) 2023 Michael Neuroth
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

package;

import haxe.Exception;

using StringTools;

using LispUtils;
using LispVariant;
using LispVariant.OpLispVariant;
using LispToken.LispTokenType;

/// <summary>
/// Class to hold informations about macro expansions at compile time.
/// </summary>
/*internal*/ class LispMacroCompileTimeExpand //: Tuple<IEnumerable<object>, IEnumerable<object>>
{
    public var FormalArguments:Array<Dynamic>;
    // public IEnumerable<object> FormalArguments
    // {
    //     get
    //     {
    //         return Item1;
    //     }
    // }

    public var Expression:Array<Dynamic>;
    // public IEnumerable<object> Expression
    // {
    //     get
    //     {
    //         return Item2;
    //     }
    // }

    public function new(/*IEnumerable<object>*/ parameters:Array<Dynamic>, /*IEnumerable<object>*/ expression:Array<Dynamic>)
        //: base(parameters, expression)
    {
        FormalArguments = parameters;
        Expression = expression;     
    }
}

/// <summary>
/// Class to hold informations about macro expansions at run time.
/// </summary>
/*internal*/ class LispMacroRuntimeEvaluate extends LispMacroCompileTimeExpand
{
    public function new(/*object*/ parameters:Dynamic, /*object*/ expression:Dynamic)
        //: base((IEnumerable<object>)parameters, (IEnumerable<object>)expression)
    {
        super(parameters, expression);
    }
}

class LispEnvironment {
    public /*const*/static var MetaTag = "###";
    public /*const*/static var Builtin = "<builtin>";
    public /*const*/static var EvalStrTag = "<evalstr>:";

    private /*const*/static var MainScope = "<main>";

    private /*const*/static var If = "if";
    private /*const*/static var While = "while";
    private /*const*/static var Begin = "begin";
    private /*const*/static var Do = "do";
    private /*const*/static var Or = "or";
    private /*const*/static var And = "and";
    private /*const*/static var Fn = "fn";
    private /*const*/static var Def = "def";
    private /*const*/static var Setf = "setf";
    private /*const*/static var Defn = "defn";
    private /*const*/static var Gdef = "gdef";
    private /*const*/static var Gdefn = "gdefn";
    private /*const*/static var MapFcn = "map";
    private /*const*/static var ReduceFcn = "reduce";
    private /*const*/static var DefineMacro = "define-macro";      // == define-macro-eval
    private /*const*/static var DefineMacroEval = "define-macro-eval";
#if ENABLE_COMPILE_TIME_MACROS 
    private /*const*/static var DefineMacroExpand = "define-macro-expand";
#end

    public /*const*/static var Lambda = "lambda";
    private /*const*/static var Tracebuffer = MetaTag + "tracebuffer" + MetaTag;
    private /*const*/static var Traceon = MetaTag + "traceon" + MetaTag;
    
    public /*const*/static var ArgsMeta = MetaTag + "args" + MetaTag;
    public /*const*/static var AdditionalArgs = "_additionalArgs";

    public /*const*/static var Macros = MetaTag + "macros" + MetaTag;
    public /*const*/static var Modules = MetaTag + "modules" + MetaTag;

    private /*const*/static var ArgsCount = "argscount";
    private /*const*/static var Args = "args";
    private /*const*/static var Arg = "arg";
    public /*const*/static var Apply = "apply";
    public /*const*/static var Eval = "eval";
    public /*const*/static var EvalStr = "evalstr";
    public /*const*/static var Quote = "quote";
    public /*const*/static var Quasiquote = "quasiquote";
    public /*const*/static var UnQuote = "_unquote";
    public /*const*/static var UnQuoteSplicing = "_unquotesplicing";

    public /*const*/static var Sym = "sym";
    public /*const*/static var Str = "str";

    public /*const*/static var FuelVersion = "v0.99.4";
    public /*const*/static var FuelDate = "11.11.2023";

    public static function FuelFuncWrapper0<TResult>(/*object[]*/ args:Array<Dynamic>, scope:LispScope, name:String, /*Func<TResult>*/ func:Dynamic):LispVariant
    {
        CheckArgs(name, 0, args, scope);

        var result = func();

        //var tempResult:LispVariant = cast(result, LispVariant);
        //return tempResult!=null ? tempResult : LispVariant.forValue(result);
        return LispVariant.forValue(result);
    }

    public static function FuelFuncWrapper1/*<T1, TResult>*/(/*object[]*/ args:Array<Dynamic>, scope:LispScope, name:String, /*Func<T1, TResult>*/ func:Dynamic):LispVariant
    {
        CheckArgs(name, 1, args, scope);

        var arg1 = /*(T1)*/cast(args[0], LispVariant);
        var result = func(arg1);

        //var tempResult:LispVariant = cast(result, LispVariant);
        //return tempResult!=null ? tempResult : LispVariant.forValue(result);
        return LispVariant.forValue(result);
    }

    public static function FuelFuncWrapper2/*<T1, T2, TResult>*/(/*object[]*/ args:Array<Dynamic>, scope:LispScope, name:String, /*Func<T1, T2, TResult>*/ func:Dynamic):LispVariant
    {
        CheckArgs(name, 2, args, scope);

        var arg1 = /*(T1)*/cast(args[0], LispVariant);
        var arg2 = /*(T2)*/cast(args[1], LispVariant);
        var result = func(arg1, arg2);

        //var tempResult:LispVariant = cast(result, LispVariant);
        //return tempResult!=null ? tempResult : LispVariant.forValue(result);
        return LispVariant.forValue(result);
    }

    public static function FuelFuncWrapper3/*<T1, T2, T3, TResult>*/(/*object[]*/ args:Array<Dynamic>, scope:LispScope, name:String, /*Func<T1, T2, T3, TResult>*/ func:Dynamic):LispVariant
    {
        CheckArgs(name, 3, args, scope);

        var arg1 = /*(T1)*/cast(args[0], LispVariant);
        var arg2 = /*(T2)*/cast(args[1], LispVariant);
        var arg3 = /*(T3)*/cast(args[2], LispVariant);
        var result = func(arg1, arg2, arg3);

        //LispVariant tempResult = result as LispVariant;
        //return tempResult ?? new LispVariant(result);
        return LispVariant.forValue(result);
    }

    public static function CreateDefaultScope():LispScope {
        var scope = LispScope.forFunction(MainScope);

        scope.set(Modules, LispScope.forFunction(Modules, scope));
        scope.set(Macros, LispScope.forFunction(Macros, scope));
        scope.set(Tracebuffer, "");
        scope.set(Traceon, false);

        scope.set("fuel", CreateFunction(Fuel, "(fuel)", "Returns and shows information about the fuel language."));
        scope.set("copyright", CreateFunction(Copyright, "(copyright)", "Returns and shows the copyright of the fuel language."));
        scope.set("help", CreateFunction(Help, "(help)", "Returns and shows the available builtin functions."));
        scope.set("doc", CreateFunction(Documentation, "(doc functionname ...)", "Returns and shows the documentation of all builtin functions or for the given function name(s)."));
        scope.set("searchdoc", CreateFunction(SearchDocumentation, "(searchdoc name ...)", "Returns and shows the documentation of functions containing name(s)."));
        scope.set("htmldoc", CreateFunction(HtmlDocumentation, "(htmldoc)", "Returns and shows the documentation of all builtin functions in html format."));
        scope.set("break", CreateFunction(Break, "(break)", "Sets a breakpoint in the code."));
        scope.set("vars", CreateFunction(Vars, "(vars)", "Returns a dump of all variables."));
        scope.set("delvar", CreateFunction(DelVar, "(vars)", "(delvar name)", "Deletes a local variable with the given name and returns a success flag."));
        scope.set("need-l-value", CreateFunction(NeedLValue, "(need-l-value)", "Returns #t if a l-value is needed as return value of the current function."));
        scope.set("trace", CreateFunction(TracePrint, "(trace value)", "Switches the trace modus on or off."));
        scope.set("gettrace", CreateFunction(GetTracePrint, "(gettrace)", "Returns the trace output."));
        scope.set("import", CreateFunction(Import, "(import module1 ...)", "Imports modules with fuel code."));
        scope.set("tickcount", CreateFunction(CurrentTickCount, "(tickcount)", "Returns the current tick count in milliseconds, can be used to measure times."));
        scope.set("sleep", CreateFunction(Sleep, "(sleep time-in-ms)", "Sleeps the given number of milliseconds."));
        scope.set("date-time", CreateFunction(Datetime, "(date-time)", "Returns a list with informations about the current date and time: (year month day hours minutes seconds)."));
        scope.set("platform", CreateFunction(Platform, "(platform)", "Returns a list with informations about the current platform: (operating_system runtime_environment register_size)."));

        // interpreter functions
        scope.set("type", CreateFunction(GetType, "(type expr)", "Returns the type id of the value of the expression."));
        scope.set("typestr", CreateFunction(GetTypeString, "(typestr expr)", "Returns a readable string representing the type of the value of the expression."));
        scope.set("nop", CreateFunction(Nop, "(nop)", "Does nothing (no operation)."));
        scope.set("return", CreateFunction(Return, "(return expr)", "Returns the value of the expression and quits the function."));
        scope.set("print", CreateFunction(Print, "(print expr1 expr2 ...)", "Prints the values of the given expressions on the console."));
        scope.set("println", CreateFunction(PrintLn, "(readline)", "Reads a line from the console input."));
        scope.set("format", CreateFunction(Format, "(format format-str expr1 expr2 ...)", "Formats the content of the format string with the values of the given expressions and returns a string."));
        scope.set("flush", CreateFunction(Flush, "(flush)", "Flushes the output to the console."));
        scope.set("readline", CreateFunction(ReadLine, "(println expr1 expr2 ...)", "Prints the values of the given expressions on the console adding a new line at the end of the output."));

        scope.set("parse-integer", CreateFunction(ParseInteger, "(parse-integer expr)", "Convert the string expr into an integer value"));
        scope.set("parse-float", CreateFunction(ParseFloat, "(parse-float expr)", "Convert the string expr into a float value"));
        scope.set("int", CreateFunction(ToInt, "(int expr)", "Convert the expr into an integer value"));
        scope.set("float", CreateFunction(ToFloat, "(float expr)", "Convert the expr into a float value"));

        scope.set("search", CreateFunction(Search, "(search searchtxt expr [pos] [len])", "Returns the first position of the searchtxt in the string, starting from position pos."));
        scope.set("slice", CreateFunction(Slice, "(slice expr1 pos len)", "Returns a substring of the given string expr1, starting from position pos with length len."));
        scope.set("replace", CreateFunction(Replace, "(replace expr1 searchtxt replacetxt)", "Returns a string of the given string expr1 with replacing searchtxt with replacetxt."));
        scope.set("trim", CreateFunction(Trim, "(trim expr1)", "Returns a string with no starting and trailing whitespaces."));
        scope.set("lower-case", CreateFunction(LowerCase, "(lower-case expr1)", "Returns a string with only lower case characters."));
        scope.set("upper-case", CreateFunction(UpperCase, "(upper-case expr1)", "Returns a string with only upper case characters."));
        scope.set("string", CreateFunction(Addition, "(string expr1 expr2 ...)", "see: add"));
        scope.set("add", CreateFunction(Addition, "(add expr1 expr2 ...)", "Returns value of expr1 added with expr2 added with ..."));
        scope.set("+", CreateFunction(Addition, "(+ expr1 expr2 ...)", "see: add"));
        scope.set("sub", CreateFunction(Substraction, "(sub expr1 expr2 ...)", "Returns value of expr1 subtracted with expr2 subtracted with ..."));
        scope.set("-", CreateFunction(Substraction, "(- expr1 expr2 ...)", "see: sub"));
        scope.set("mul", CreateFunction(Multiplication, "(sub expr1 expr2 ...)", "(mul expr1 expr2 ...)", "Returns value of expr1 multipied by expr2 multiplied by ..."));
        scope.set("*", CreateFunction(Multiplication, "(* expr1 expr2 ...)", "see: mul"));
        scope.set("div", CreateFunction(Division, "(div expr1 expr2 ...)", "Returns value of expr1 divided by expr2 divided by ..."));
        scope.set("/", CreateFunction(Division, "(* expr1 expr2 ...)", "see: div"));
        scope.set("mod", CreateFunction(Modulo, "(mod expr1 expr2)", "Returns value of modulo operation between expr1 and expr2"));
        scope.set("%", CreateFunction(Modulo, "(% expr1 expr2)", "see: mod"));

        scope.set("<<", CreateFunction(LeftShift, "(<< expr1 expr2)", "Integer left shift. Returns integer expression1 left shifted for expression2 binary digits."));
        scope.set(">>", CreateFunction(RightShift, "(>> expr1 expr2)", "Integer right shift. Returns integer expression1 right shifted for expression2 binary digits."));
        scope.set("|", CreateFunction(BinaryOr, "(| expr1 expr2)", "Binary or for integer numbers (bitwise or)."));
        scope.set("&", CreateFunction(BinaryAnd, "(& expr1 expr2)", "Binary and for integer numbers (bitwise and)."));
        scope.set("^", CreateFunction(BinaryXOr, "(^ expr1 expr2)", "Binary xor for integer numbers (bitwise xor)."));
        scope.set("~", CreateFunction(BinaryNot, "(~ expr1)", "Binary not for integer numbers (bitwise not)."));

        scope.set("<", CreateFunction(Less, "(< expr1 expr2)", "Returns #t if value of expression1 is smaller than value of expression2 and returns #f otherwiese."));
        scope.set(">", CreateFunction(Greater, "(> expr1 expr2)", "Returns #t if value of expression1 is larger than value of expression2 and returns #f otherwiese."));
        scope.set("<=", CreateFunction(LessEqual, "(<= expr1 expr2)", "Returns #t if value of expression1 is equal or smaller than value of expression2 and returns #f otherwiese."));
        scope.set(">=", CreateFunction(GreaterEqual, "(>= expr1 expr2)", "Returns #t if value of expression1 is equal or larger than value of expression2 and returns #f otherwiese."));

        scope.set("equal", CreateFunction(EqualTest, "(equal expr1 expr2)", "Returns #t if value of expression1 is equal with value of expression2 and returns #f otherwiese."));
        scope.set("=", CreateFunction(EqualTest, "(= expr1 expr2)", "see: equal"));
        scope.set("==", CreateFunction(EqualTest, "(== expr1 expr2)", "see: equal"));
        scope.set("!=", CreateFunction(NotEqualTest, "(!= expr1 expr2)", "Returns #t if value of expression1 is not equal with value of expression2 and returns #f otherwiese."));

        scope.set("not", CreateFunction(Not, "(not expr)", "Returns the inverted bool value of the expression."));
        scope.set("!", CreateFunction(Not, "(! expr)", "see: not"));

        scope.set("list", CreateFunction(CreateList, "(list item1 item2 ...)", "Returns a new list with the given elements."));
        scope.set(MapFcn, CreateFunction(MapLoop, "(map function list)", "Returns a new list with elements, where all elements of the list where applied to the function."));
        scope.set(ReduceFcn, CreateFunction(Reduce, "(reduce function list initial)", "Reduce function."));
        scope.set("cons", CreateFunction(Cons, "(cons item list)", "Returns a new list containing the item and the elements of the list."));
        scope.set("len", CreateFunction(Length, "(len list)", "Returns the length of the list."));
        scope.set("first", CreateFunction(FirstElem, "(first list)", "see: car"));
        scope.set("last", CreateFunction(LastElem, "(last list)", "Returns the last element of the list."));
        scope.set("car", CreateFunction(FirstElem, "(car list)", "Returns the first element of the list."));
        scope.set("rest", CreateFunction(Rest, "(rest list)", "see: cdr"));
        scope.set("cdr", CreateFunction(Rest, "(cdr list)", "Returns a new list containing all elements except the first of the given list."));
        scope.set("nth", CreateFunction(Nth, "(nth number list)", "Returns the [number] element of the list."));
        scope.set("push", CreateFunction(Push, "(push elem list [index])", "Inserts the element at the given index (default value 0) into the list (implace) and returns the updated list."));
        scope.set("pop", CreateFunction(Pop, "(pop list [index])", "Removes the element at the given index (default value 0) from the list and returns the removed element."));
        scope.set("append", CreateFunction(Append, "(append list1 list2 ...)", "Returns a new list containing all given lists elements."));
        scope.set("reverse", CreateFunction(Reverse, "(reverse expr)", "Returns a list or string with a reverted order."));
        scope.set("rval", CreateFunction(RValue, "(rval expr)", "Returns a RValue of the expr, disables LValue evaluation.", true, true));
        scope.set(Sym, CreateFunction(Symbol, "(sym expr)", "Returns the evaluated expression as symbol."));
        scope.set(Str, CreateFunction(ConvertToString, "(str expr)", "Returns the evaluated expression as string."));

        scope.set(ArgsCount, CreateFunction(ArgsCountFcn, "(argscount)", "Returns the number of arguments for the current function."));
        scope.set(Args, CreateFunction(ArgsFcn, "(args)", "Returns all the values of the arguments for the current function."));
        scope.set(Arg, CreateFunction(ArgFcn, "(arg number)", "Returns the value of the [number] argument for the current function."));
        scope.set(Apply, CreateFunction(ApplyFcn, "(apply function arguments-list)", "Calls the function with the arguments."));
        scope.set(Eval, CreateFunction(EvalFcn, "(eval ast)", "Evaluates the abstract syntax tree (ast)."));
        scope.set(EvalStr, CreateFunction(EvalStrFcn, "(evalstr string)", "Evaluates the string."));

        // additional data types        
        scope.set("make-dict", CreateFunction(MakeDict, "(make-dict)", "Returns a new dictionary."));
        scope.set("dict-set", CreateFunction(DictSet, "(dict-set dict key value)", "Sets the value for the key in the dictionary."));
        scope.set("dict-get", CreateFunction(DictGet, "(dict-get dict key)", "Returns the value for the key or nil if key is not in dictionary."));
        scope.set("dict-remove", CreateFunction(DictRemove, "(dict-remove dict key)", "Removes the value / key pair from the directory and returns success flag."));
        scope.set("dict-keys", CreateFunction(DictKeys, "(dict-keys dict)", "Returns all keys in the dictionary."));
        scope.set("dict-clear", CreateFunction(DictClear, "(dict-keys dict)", "Returns all keys in the dictionary."));
        scope.set("dict-contains-key", CreateFunction(DictContainsKey, "(dict-contains-key dict key)", "Returns #t if key is contained in dictionary, otherwise #f."));
        scope.set("dict-contains-value", CreateFunction(DictContainsValue,  "(dict-contains-value dict key)", "Returns #t if value is contained in dictionary, otherwise #f."));
        // ggf. setf support

        // special forms
        scope.set(And, CreateFunction(and_form, "(and expr1 expr2 ...)", "And operator with short cut.", true, true));
        scope.set("&&", CreateFunction(and_form, "(&& expr1 expr2 ...)", "See: and", true, true));
        scope.set(Or, CreateFunction(or_form, "(or expr1 expr2 ...)", "Or operator with short cut.", true, true));
        scope.set("||", CreateFunction(or_form, "(|| expr1 expr2 ...)", "See: or", true, true));
        scope.set(Def, CreateFunction(def_form, "(def symbol expression)", "Creates a new variable with name of symbol in current scope. Evaluates expression and sets the value of the expression as the value of the symbol.", true, true));
        scope.set(Gdef, CreateFunction(gdef_form, "(gdef symbol expression)", "Creates a new variable with name of symbol in global scope. Evaluates expression and sets the value of the expression as the value of the symbol.", true, true));
        scope.set(Setf, CreateFunction(setf_form, "(setf symbol expression)", "Evaluates expression and sets the value of the expression as the value of the symbol.", true, true));

        // macros are:
        // a special form to control evaluation of function parameters inside the macro code
        // there are two options possible:
        //  - run time evaluation of macros
        //  - compile time replacement/expanding of macros
        scope.set(DefineMacro, CreateFunction(definemacroevaluate_form, "(define-macro name (arguments) statement)", "see: define-macro-eval", true, true));
        // run time evaluation for macros: 
        scope.set(DefineMacroEval, CreateFunction(definemacroevaluate_form, "(define-macro-eval name (arguments) statement)", "Special form: Defines a macro which will be evaluated at run time.", true, true));
#if ENABLE_COMPILE_TIME_MACROS
        // compile time expand for macros:
        scope.set(DefineMacroExpand, CreateFunction(definemacroexpand_form, "(define-macro-expand name (arguments) statement)", "Special form: Defines a macro which will be evaluated at compile time.", true, true));
#end       

        scope.set(Quote, CreateFunction(quote_form, "(quasiquote expr)", "Returns expression without evaluating it, but processes evaluation operators , and ,@.", true, true));
        scope.set(Quasiquote, CreateFunction(quasiquote_form, "(quasiquote expr)", "Returns expression without evaluating it, but processes evaluation operators , and ,@.", true, true));
        scope.set(UnQuote, CreateFunction(unquote_form, "(unquotesplicing expr)", "Special form for unquotingsplicing expressions in quasiquote functions.", true, true));
        scope.set(UnQuoteSplicing, CreateFunction(unquotesplicing_form, "(unquotesplicing expr)", "Special form for unquotingsplicing expressions in quasiquote functions.", true, true));
        scope.set(If, CreateFunction(if_form, "(if cond then-block [else-block])", "The if statement.", true, true));
        scope.set(While, CreateFunction(while_form, "(while cond block)", "The while loop.", true, true));
        scope.set(Do, CreateFunction(do_form, "(do statement1 statement2 ...)", "Returns a sequence of statements.", true, true));
        scope.set(Begin, CreateFunction(do_form, "(begin statement1 statement2 ...)", "see: do", true, true));
        scope.set(Lambda, CreateFunction(fn_form, "(lambda (arguments) block)", "Returns a lambda function.", true, true));
        scope.set(Fn, CreateFunction(fn_form, "(fn (arguments) block)", "Returns a function.", true, true));
        scope.set(Defn, CreateFunction(defn_form, "(defn name (args) block)", "Defines a function in the current scope.", true, true));
        scope.set(Gdefn, CreateFunction(gdefn_form, "(gdefn name (args) block)", "Defines a function in the global scope.", true, true));

        return scope;
    }
    
    private static function CheckArgs(name:String, count:Int, /*object[]*/ args:Array<Dynamic>, scope:LispScope):Void
    {
        if (count < 0 || args.length != count)
        {
            throw LispException.fromScope('Bad argument count in $name, has $args.length expected $count', scope);
        }
    }

    private static function CheckOptionalArgs(name:String, minCount:Int, maxCount:Int, /*object[]*/ args:Array<Dynamic>, scope:LispScope):Void
    {
        if ((args.length < minCount) || (args.length > maxCount))
        {
            throw new LispException('Bad argument count in $name, has ${args.length} expected between $minCount and $maxCount');
        }
    }

    private static function CreateFunction(/*Func<object[], LispScope, LispVariant>*/ func:Dynamic, signature:String = null, documentation:String = null, isBuiltin:Bool = true, isSpecialForm:Bool = false, isEvalInExpand:Bool = false, moduleName:String = "<builtin>"):Dynamic
    {
        return LispVariant.forValue(new LispFunctionWrapper(func, signature, documentation, isBuiltin, isSpecialForm, isEvalInExpand, moduleName));
    }

    private static function Fuel(/*object[]*/ args:Array<Dynamic>, scope:LispScope):LispVariant
    {
        CheckArgs("fuel", 0, args, scope);

        return LispVariant.forValue('fuel version ${LispEnvironment.FuelVersion} from ${LispEnvironment.FuelDate}');
    }
    
    private static function Copyright(/*object[]*/ args:Array<Dynamic>, scope:LispScope):LispVariant
    {
        CheckArgs("copyright", 0, args, scope);

        var text = 'Copyright: ${Lisp.License} ${Lisp.LicenseUrl}';
        return LispVariant.forValue(text);
    }

    private static function Help(/*object[]*/ args:Array<Dynamic>, scope:LispScope):LispVariant
    {
        CheckArgs("help", 0, args, scope);

        var helpText = "";  //new StringBuilder();
        helpText.Append("available functions:\n");
        for (cmd in scope.keys())
        {                
            var s = cmd+"\n";
            helpText += s;
            //helpText.Append(s);
        }
        scope.GlobalScope.Output.WriteLine(helpText/*.ToStr()*/);
        return LispVariant.forValue(helpText/*.ToStr()*/);
    }
    
    private static function Documentation(/*object[]*/ args:Array<Dynamic>, scope:LispScope):LispVariant
    {
        return DoSearchDocumentation(args, scope, null);
    }

    private static function SearchDocumentation(/*object[]*/ args:Array<Dynamic>, scope:LispScope):LispVariant
    {
        return DoSearchDocumentation(args, scope, function (k:String, n:String):Bool { return k.indexOf(n)>=0; });
    }

    private static function DoSearchDocumentation(/*object[]*/ args:Array<Dynamic>, scope:LispScope, /*Func<string, string, bool>*/ select:Dynamic):LispVariant
    {
        if (args.length > 0)
        {
            var help = "";  //string.Empty;
            for (item in args)
            {
                help += scope.GetFunctionsHelpFormated(item.ToStr(), select);
                // search for functions in all loaded modules
                // for (KeyValuePair<string, object> module in (LispScope)(scope.GlobalScope[Modules]))
                // {
                //     help += ((LispScope)module.Value).GetFunctionsHelpFormated(item.ToStr(), select);
                // }
                var modules = LispUtils.CastDynamicToLispVariant(scope.GlobalScope.get_scope(Modules));  //cast(scope.GlobalScope.get_scope(Modules), LispVariant);
                if (modules!=null) 
                {
                    for (module in modules.ListValue)
                    {
                        help += cast(module.Value, LispScope).GetFunctionsHelpFormated(item.ToStr(), select);
                    }
                }
            }
            return DumpDocumentation(scope, function () { scope.GlobalScope.Output.WriteLine(help); });
        }
        return DumpDocumentation(scope, function () { scope.GlobalScope.DumpBuiltinFunctionsHelpFormated(); });
    }

    private static function HtmlDocumentation(/*object[]*/ args:Array<Dynamic>, scope:LispScope):LispVariant
    {
        return DumpDocumentation(scope, function () { return scope.GlobalScope.DumpBuiltinFunctionsHelpHtmlFormated(); });
    }

    private static function DumpDocumentation(scope:LispScope, /*Action*/ dump:Dynamic):LispVariant
    {
        var text = new Ref<String>("");  //new StringBuilder();
        var tempOutputWriter = scope.GlobalScope.Output;
        scope.GlobalScope.Output = new LispScope.TextWriter(text);  //new StringWriter(text);
        dump();
        scope.GlobalScope.Output = tempOutputWriter;
        return LispVariant.forValue(text.value/*.ToStr()*/);
    }

    private static function Break(/*object[]*/ args:Array<Dynamic>, scope:LispScope):LispVariant
    {
        CheckArgs("break", 0, args, scope);

        scope.GlobalScope.Output.WriteLine("break -> call stack:");
        scope.DumpStack(scope.GetCallStackSize());
        var debugger = scope.GlobalScope.Debugger;
        if (debugger != null)
        {
            debugger.InteractiveLoop(/*initialTopScope:*/ scope);
        }
        else
        {
            scope.GlobalScope.Output.WriteLine("Warning: can not break, because no debugger support availabe!");
        }
        return new LispVariant(LispType.Undefined);
    }

    private static function Vars(/*object[]*/ args:Array<Dynamic>, scope:LispScope):LispVariant
    {
        CheckArgs("vars", 0, args, scope);

        scope.GlobalScope.Output.WriteLine("variables:");
        scope.DumpVars();
        return new LispVariant(LispType.Undefined);
    }

    private static function DelVar(/*object[]*/ args:Array<Dynamic>, scope:LispScope):LispVariant
    {
        return FuelFuncWrapper1/*<LispVariant, bool>*/(args, scope, "delvar", function (arg1:LispVariant):Bool { return scope.remove(arg1.ToStr()); });
    }

    private static function NeedLValue(/*object[]*/ args:Array<Dynamic>, scope:LispScope):LispVariant
    {
        return FuelFuncWrapper0/*<bool>*/(args, scope, "need-l-value", function ():Bool { return scope.NeedsLValue; });
    }

    private static function TracePrint(/*object[]*/ args:Array<Dynamic>, scope:LispScope):LispVariant
    {
        var status = cast(args[0], LispVariant);
        scope.set(Traceon, status.BoolValue);
        return LispVariant.forValue(status.BoolValue);
    }

    private static function GetTracePrint(/*object[]*/ args:Array<Dynamic>, scope:LispScope):LispVariant
    {
        var buffer = cast(scope.get_value(Tracebuffer), String);
        return LispVariant.forValue(buffer/*.ToStr()*/);
    }
    
    private static function CurrentTickCount(/*object[]*/ args:Array<Dynamic>, scope:LispScope):LispVariant
    {
        return FuelFuncWrapper0/*<int>*/(args, scope, "tickcount",
                    function ():Float
                        {
#if (node || sys)
                            return Sys.cpuTime();
#else
                            return -1;
#end
                        });
    }

    private static function Sleep(/*object[]*/ args:Array<Dynamic>, scope:LispScope):LispVariant
    {
        return FuelFuncWrapper1/*<LispVariant, LispVariant>*/(args, scope, "sleep", function (arg1:LispVariant):LispVariant
        {
#if (node || sys)
            Sys.sleep(arg1.ToDouble()*0.001);
#end
            return new LispVariant(LispType.Undefined);
        });
    }

    private static function Datetime(/*object[]*/ args:Array<Dynamic>, scope:LispScope):LispVariant
    {
        CheckArgs("date-time", 0, args, scope);

        var timestamp = Date.now();
        var year = timestamp.getFullYear();
        var month = timestamp.getMonth();
        var day = timestamp.getDay();
        var hour = timestamp.getHours();
        var minute = timestamp.getMinutes();
        var second = timestamp.getSeconds();
        var value = new Array<Dynamic>();   //List<LispVariant>
        value.push(LispVariant.forValue(year));
        value.push(LispVariant.forValue(month));
        value.push(LispVariant.forValue(day));
        value.push(LispVariant.forValue(hour));
        value.push(LispVariant.forValue(minute));
        value.push(LispVariant.forValue(second));
        return LispVariant.forValue(value);
    }

    private static function Platform(/*object[]*/ args:Array<Dynamic>, scope:LispScope):LispVariant
    {
#if (node || sys)
        var osString = Sys.systemName();
#else
        var osString = "???";
#end
        //OperatingSystem os = Environment.OSVersion;
        //PlatformID pid = os.Platform;
        /*
        switch(pid)
        {
            case PlatformID.MacOSX:
                osString = "MACOSX";
                break;
            case PlatformID.Unix:
                osString = "UNIX";
                break;
            case PlatformID.Win32NT:
            case PlatformID.Win32S:
            case PlatformID.Win32Windows:
                osString = "WIN";
                break;
            case PlatformID.Xbox:
                osString = "XBOX";
                break;
            default:
                osString = "UNKNOWN";
                break;
        }
        */
        //var is64Bit = IntPtr.Size == 8;
        var value = new Array<Dynamic>();
        value.push(LispVariant.forValue(osString));
        value.push(LispVariant.forValue('haxe-${LispUtils.GetTargetLanguage()}'));  //".NET"
        value.push(LispVariant.forValue("unknown-register-length"));  // /* is64Bit ? "64bit" : "32bit"*//*, Environment.Is64BitProcess*/ /*, Environment.OSVersion.ToStr(), Environment.Version.ToStr()*/ };
        return LispVariant.forValue(value);
    }

    private static function AddFileExtensionIfNeeded(fileName:String):String
    {
        /*const string*/var extension = ".fuel";

        if (!fileName.endsWith(extension))
        {
            fileName += extension;
        }
        return fileName;
    }

    private static function Import(/*object[]*/ args:Array<Dynamic>, scope:LispScope):LispVariant
    {
        var result = new LispVariant(LispType.Undefined);

        for (module in args)
        {
#if (node || sys)            
            var code = "";  //string.Empty;
            var orgModuleFileName = cast(module, LispVariant).StringValue;
            var fileName = orgModuleFileName;
            if (!sys.FileSystem.exists(fileName))
            {
                // try the given library path (if available)
                fileName = LispUtils.LibraryPath + LispUtils.DirectorySeparatorChar + orgModuleFileName;
                fileName = AddFileExtensionIfNeeded(fileName);
                if (!sys.FileSystem.exists(fileName)) 
                {
                    // try default path .\Library\modulename.fuel
                    fileName = "." + LispUtils.DirectorySeparatorChar + "Library" + LispUtils.DirectorySeparatorChar + orgModuleFileName;
                    fileName = AddFileExtensionIfNeeded(fileName);
                    if (!sys.FileSystem.exists(fileName))
                    {
                        // try default path for visiscript .\lib\fuel\modulename.fuel
                        fileName = "." + LispUtils.DirectorySeparatorChar + "lib" + LispUtils.DirectorySeparatorChar + "fuel" + LispUtils.DirectorySeparatorChar + orgModuleFileName;
                        fileName = AddFileExtensionIfNeeded(fileName);
                        if (!sys.FileSystem.exists(fileName))
                        {
//TODO
                            // try default path <fuel.exe-path>\Library\modulename.fuel
                            // fileName = AppDomain.CurrentDomain.BaseDirectory + LispUtils.DirectorySeparatorChar + "Library" + LispUtils.DirectorySeparatorChar + orgModuleFileName;
                            // fileName = AddFileExtensionIfNeeded(fileName);
                            // if (!sys.FileSystem.exists(fileName))
                            {
                                // try environment variable FUELPATH
                                var envPath = Sys.environment().get("FUELPATH");
                                if (envPath != null)
                                {
                                    fileName = envPath + LispUtils.DirectorySeparatorChar + orgModuleFileName;
                                    fileName = AddFileExtensionIfNeeded(fileName);
                                }
                            }
                        }
                    }
                }
            }
            if (sys.FileSystem.exists(fileName))
            {
                code = sys.io.File.getContent/*ReadAllText*/(fileName);
            }
            else
            {
                // use std lib of fuel from builtin resources
                // resources not supported in haxe environment
                // if(orgModuleFileName=="fuellib")
                // {
                //     code = Encoding.UTF8.GetString(Properties.Resources.fuellib);
                // }
                // else
                // {
                //     scope.GlobalScope.Output.WriteLine('WARNING: Library ${orgModuleFileName} not found! Tried path ${fileName}');
                // }
            }
            if (!LispUtils.IsNullOrEmpty(code))
            {
                var importScope = LispScope.forFunction("import "+fileName, scope.GlobalScope, fileName);
                scope.PushNextScope(importScope);

                result = Lisp.Eval(code, importScope, fileName);

                // add new module to modules scope
                (cast(scope, LispScope).GlobalScope.get_scope(Modules)).set(fileName, importScope);

                scope.PopNextScope();
            }
#end
        }
        return result;
    }

    public static function Nop(/*object[]*/ args:Array<Dynamic>, scope:LispScope):LispVariant
    {
        return new LispVariant(LispType.Undefined);
    }

    public static function Return(/*object[]*/ args:Array<Dynamic>, scope:LispScope):LispVariant
    {
        scope.IsInReturn = true;
        return LispVariant.forValue(args[0]);
    }

    public static function GetType(/*object[]*/ args:Array<Dynamic>, scope:LispScope):LispVariant
    {
        return FuelFuncWrapper1/*<LispVariant, int>*/(args, scope, "type", function (arg1:LispVariant):Int { return /*(int)*/LispVariant.ToTypeId(arg1.ValueType); });
    }

    public static function GetTypeString(/*object[]*/ args:Array<Dynamic>, scope:LispScope):LispVariant
    {
        return FuelFuncWrapper1/*<LispVariant, string>*/(args, scope, "typestr", function (arg1:LispVariant):String { return arg1.TypeString; });
    }

    public static function Print(/*object[]*/ args:Array<Dynamic>, scope:LispScope):LispVariant
    {
        var text = GetStringRepresentation(args, scope);
        scope.GlobalScope.Output.Write(text);
        return LispVariant.forValue(text);
    }

    public static function PrintLn(/*object[]*/ args:Array<Dynamic>, scope:LispScope):LispVariant
    {
        var text = GetStringRepresentation(args, scope);
        scope.GlobalScope.Output.WriteLine(text);
        return LispVariant.forValue(text);
    }

    public static function Format(/*object[]*/ args:Array<Dynamic>, scope:LispScope):LispVariant
    {
        CheckOptionalArgs("format", 1, 6, args, scope);

        var formatedResult = "";  //string.Empty;
        var formatStr = cast(args[0], LispVariant).ToStr();
        var /*object[]*/ valueArgs = new Array<Dynamic>();  //object[args.Count() - 1];
        valueArgs.resize(args.length - 1);
        //Array.Copy(args, 1, valueArgs, 0, args.Count()-1);
        for(i in 1...args.length) {
            valueArgs[i-1] = args[i].ToStr();
        }
        try
        {
            formatedResult = formatStr.Format(valueArgs); //string.Format(formatStr, valueArgs);
        }
        // catch(ex:ArgumentNullException)
        // {
        //     throw new LispException('Not enough items for format string: ${formatStr}');
        // }
        // catch(ex:FormatException)
        // {
        //     throw new LispException('Invalid format string: ${formatStr}');
        // }
        catch(Exception)
        {
            throw new LispException('Invalid format string: ${formatStr}');
        }
        return LispVariant.forValue(formatedResult);
    }

    public static function Flush(/*object[]*/ args:Array<Dynamic>, scope:LispScope):LispVariant
    {
        return FuelFuncWrapper0/*<LispVariant>*/(args, scope, "flush", function ():LispVariant
        {
            scope.GlobalScope.Output.Flush();
            return LispVariant.forValue(LispType.Undefined);
        });
    }

    public static function ReadLine(/*object[]*/ args:Array<Dynamic>, scope:LispScope):LispVariant
    {
        return FuelFuncWrapper0/*<string>*/(args, scope, "readline", function ():String { return scope.GlobalScope.Input.ReadLine(); });
    }

    public static function ParseInteger(/*object[]*/ args:Array<Dynamic>, scope:LispScope):LispVariant
    {
        CheckArgs("parse-integer", 1, args, scope);

        var value:Int;
        try
        {
            //value = Convert.ToInt32(((LispVariant)args[0]).ToStr(), CultureInfo.InvariantCulture);
            value = Std.parseInt(cast(args[0], LispVariant).ToStr());
        }
        catch (Exception)
        {
            return new LispVariant(LispType.Undefined);
        }
        return LispVariant.forValue(value);
    }    

    public static function ParseFloat(/*object[]*/ args:Array<Dynamic>, scope:LispScope):LispVariant
    {
        CheckArgs("parse-float", 1, args, scope);

        var value:Float;
        try
        {
            //value = Convert.ToDouble(((LispVariant)args[0]).ToStr(), CultureInfo.InvariantCulture);
            value = Std.parseFloat(cast(args[0], LispVariant).ToStr());
        }
        catch (Exception)
        {
            return new LispVariant(LispType.Undefined);
        }
        return LispVariant.forValue(value);
    }

    public static function ToInt(/*object[]*/ args:Array<Dynamic>, scope:LispScope):LispVariant
    {
        CheckArgs("int", 1, args, scope);

//TODO -> use value.ToInt() instead of if statements !        
        var value = cast(args[0], LispVariant);
        if (value.IsInt)
        {
            return LispVariant.forValue(value.IntValue);
        }
        if (value.IsDouble)
        {
            return LispVariant.forValue(Std.int(value.DoubleValue));
        }
        if (value.IsString)
        {
            return ParseInteger(args, scope);
        }
        if (value.IsBool)
        {
            return LispVariant.forValue(value.BoolValue ? 1 : 0);
        }
        return new LispVariant(LispType.Undefined);
    }

    public static function ToFloat(/*object[]*/ args:Array<Dynamic>, scope:LispScope):LispVariant
    {
        CheckArgs("float", 1, args, scope);

//TODO -> use value.ToDouble() instead of if statements !        
        var value = cast(args[0], LispVariant);
        if (value.IsInt)
        {
            return LispVariant.forValue(value.ToDouble());
        }
        if (value.IsDouble)
        {
            return LispVariant.forValue(value.DoubleValue);
        }
        if (value.IsString)
        {
            return ParseFloat(args, scope);
        }
        if (value.IsBool)
        {
            return LispVariant.forValue(value.BoolValue ? 1.0 : 0.0);
        }
        return new LispVariant(LispType.Undefined);
    }

    public static function Search(/*object[]*/ args:Array<Dynamic>, scope:LispScope):LispVariant
    {
        CheckOptionalArgs("search", 2, 4, args, scope);

        var arg0 = cast(args[0], LispVariant);
        var arg1 = cast(args[1], LispVariant);
        var pos = args.length > 2 ? cast(args[2], LispVariant).ToInt() : -1;
        var len = args.length > 3 ? cast(args[3], LispVariant).ToInt() : -1;

        var foundPos = -1;
        if (arg1.IsString)
        {
            var searchText = arg0.ToStr();
            var source = arg1.ToStr();
            if (pos >= 0)
            {
                if (len >= 0)
                {
                    foundPos = source.indexOf(searchText, pos/*TODO, len*/);
                }
                else
                {
                    foundPos = source.indexOf(searchText, pos);
                }
            }
            else
            {
                foundPos = source.indexOf(searchText);
            }
        }
        else if (arg1.IsList)
        {
            var list = arg1.ListValue;
            var i = 0;
            for (elem in list)
            {
                if (arg0.EqualOp(elem))
                {
                    foundPos = i;
                    break;
                }
                i++;
            }
        }
        else
        {
            throw new LispException('search not supported for type ${Type.typeof(arg1)}');  //.GetType()
        }
        return LispVariant.forValue(foundPos);
    }

    public static function Slice(/*object[]*/ args:Array<Dynamic>, scope:LispScope):LispVariant
    {
        CheckArgs("slice", 3, args, scope);

        var value = cast(args[0], LispVariant).ToStr();
        var startPos = cast(args[1], LispVariant).ToInt();
        var len = cast(args[2], LispVariant).ToInt();
        if (len >= 0)
        {
            value = value.substr(startPos, len);
        }
        else
        {
            value = value.substr(startPos);
        }
        return LispVariant.forValue(value);
    }

    public static function Replace(/*object[]*/ args:Array<Dynamic>, scope:LispScope):LispVariant
    {
        return FuelFuncWrapper3/*<LispVariant, LispVariant, LispVariant, string>*/(args, scope, "replace", function (arg1:LispVariant, arg2:LispVariant, arg3:LispVariant) { return StringTools.replace(arg1.ToStr(), arg2.ToStr(), arg3.ToStr());});
    }

    public static function Trim(/*object[]*/ args:Array<Dynamic>, scope:LispScope):LispVariant
    {
        return FuelFuncWrapper1/*<object, string>*/(args, scope, "trim", function (arg1):String { return StringTools.trim(arg1.ToStr()); });
    }

    public static function LowerCase(/*object[]*/ args:Array<Dynamic>, scope:LispScope):LispVariant
    {
        return FuelFuncWrapper1/*<object, string>*/(args, scope, "lower-case", function (arg1):String { return arg1.ToStr().toLowerCase(); });
    }

    public static function UpperCase(/*object[]*/ args:Array<Dynamic>, scope:LispScope):LispVariant
    {
        return FuelFuncWrapper1/*<object, string>*/(args, scope, "upper-case", function (arg1):String { return arg1.ToStr().toUpperCase(); });
    }

    public static function Addition(/*object[]*/ args:Array<Dynamic>, scope:LispScope):LispVariant
    {
        return ArithmetricOperation(args, function(l:LispVariant, r:LispVariant) return LispVariant.op_add(l, r));
    }

    public static function Substraction(/*object[]*/ args:Array<Dynamic>, scope:LispScope):LispVariant
    {
        return ArithmetricOperation(args, function(l:LispVariant, r:LispVariant) return LispVariant.op_minus(l, r));
    }

    public static function Multiplication(/*object[]*/ args:Array<Dynamic>, scope:LispScope):LispVariant
    {
        return ArithmetricOperation(args, function(l:LispVariant, r:LispVariant) return LispVariant.op_mul(l, r));
    }

    public static function Division(/*object[]*/ args:Array<Dynamic>, scope:LispScope):LispVariant
    {
        return ArithmetricOperation(args, function(l:LispVariant, r:LispVariant) return LispVariant.op_divide(l, r));
    }

    public static function Modulo(/*object[]*/ args:Array<Dynamic>, scope:LispScope):LispVariant
    {
        return ArithmetricOperation(args, function(l:LispVariant, r:LispVariant) return LispVariant.op_modulo(l, r));
    }

    public static function Not(/*object[]*/ args:Array<Dynamic>, scope:LispScope):LispVariant
    {
        return FuelFuncWrapper1/*<LispVariant, bool>*/(args, scope, "not", function (arg1:LispVariant) { return LispVariant.forValue(!arg1.ToBool()); });
    }

    public static function LeftShift(/*object[]*/ args:Array<Dynamic>, scope:LispScope):LispVariant
    {
        return FuelFuncWrapper2/*<LispVariant, LispVariant, LispVariant>*/(args, scope, "left-shift", function (arg1:LispVariant, arg2:LispVariant)
        {
            return new LispVariant(LispType.Int, arg1.ToInt() << arg2.ToInt());
        });
    }

    public static function RightShift(/*object[]*/ args:Array<Dynamic>, scope:LispScope):LispVariant
    {
        return FuelFuncWrapper2/*<LispVariant, LispVariant, LispVariant>*/(args, scope, "right-shift", function (arg1:LispVariant, arg2:LispVariant)
        {
            return new LispVariant(LispType.Int, arg1.IntValue >> arg2.IntValue);
        });
    }

    public static function BinaryOr(/*object[]*/ args:Array<Dynamic>, scope:LispScope):LispVariant
    {
        return FuelFuncWrapper2/*<LispVariant, LispVariant, LispVariant>*/(args, scope, "binary-or", function (arg1:LispVariant, arg2:LispVariant)
        {
            return new LispVariant(LispType.Int, arg1.IntValue | arg2.IntValue);
        });
    }

    public static function BinaryAnd(/*object[]*/ args:Array<Dynamic>, scope:LispScope):LispVariant
    {
        return FuelFuncWrapper2/*<LispVariant, LispVariant, LispVariant>*/(args, scope, "binary-and", function (arg1:LispVariant, arg2:LispVariant)
        {
            return new LispVariant(LispType.Int, arg1.IntValue & arg2.IntValue);
        });
    }

    public static function BinaryXOr(/*object[]*/ args:Array<Dynamic>, scope:LispScope):LispVariant
    {
        return FuelFuncWrapper2/*<LispVariant, LispVariant, LispVariant>*/(args, scope, "binary-xor", function (arg1:LispVariant, arg2:LispVariant)
        {
            return new LispVariant(LispType.Int, arg1.IntValue ^ arg2.IntValue);
        });
    }

    public static function BinaryNot(/*object[]*/ args:Array<Dynamic>, scope:LispScope):LispVariant
    {
        return FuelFuncWrapper1/*<LispVariant, LispVariant>*/(args, scope, "binary-not", function (arg1:LispVariant)
        {
            return new LispVariant(LispType.Int, ~arg1.IntValue);
        });
    }

    public static function Less(/*object[]*/ args:Array<Dynamic>, scope:LispScope):LispVariant
    {
        return CompareOperation(args, function(l:LispVariant, r:LispVariant) return LispVariant.op_less(l, r), scope, "<");
    }

    public static function Greater(/*object[]*/ args:Array<Dynamic>, scope:LispScope):LispVariant
    {
        return CompareOperation(args, function(l:LispVariant, r:LispVariant) return LispVariant.op_greater(l, r), scope, ">");
    }

    public static function LessEqual(/*object[]*/ args:Array<Dynamic>, scope:LispScope):LispVariant
    {
        return CompareOperation(args, function(l:LispVariant, r:LispVariant) return LispVariant.op_less_than(l, r), scope, "<=");
    }

    public static function GreaterEqual(/*object[]*/ args:Array<Dynamic>, scope:LispScope):LispVariant
    {
        return CompareOperation(args, function(l:LispVariant, r:LispVariant) return LispVariant.op_greater_than(l, r), scope, ">=");
    }

    public static function EqualTest(/*object[]*/ args:Array<Dynamic>, scope:LispScope):LispVariant
    {
        return CompareOperation(args, function(l:LispVariant, r:LispVariant) return LispVariant.op_equal(l, r), scope, "==");
    }

    public static function NotEqualTest(/*object[]*/ args:Array<Dynamic>, scope:LispScope):LispVariant
    {
        return CompareOperation(args, function(l:LispVariant, r:LispVariant) return LispVariant.op_not_equal(l, r), scope, "!=");
    }

    public static function CreateList(/*object[]*/ args:Array<Dynamic>, scope:LispScope):LispVariant 
    {
        var result = new LispVariant(LispType.List, new Array<Dynamic>());  //List<object>()
        for (arg in args)
        {
            result.Add(arg);
        }
        return result;
    }

    public static function MapLoop(/*object[]*/ args:Array<Dynamic>, scope:LispScope):LispVariant 
    {
        CheckArgs(MapFcn, 2, args, scope);

        var functionVal = CheckForFunction(MapFcn, args[0], scope).FunctionValue;
        var elements = CheckForList(MapFcn, args[1], scope);

        var result = new LispVariant(LispType.List, new Array<Dynamic>() /*List<object>()*/);
        for (elem in elements)
        {
            // call for every element the given function (args[0])
            var arr = new Array<Dynamic>();
            arr.push(elem);
            result.Add(functionVal.Function(/*new[] {elem}*/arr, scope));
        }
        return result;
    }
    
    public static function Reduce(/*object[]*/ args:Array<Dynamic>, scope:LispScope):LispVariant
    {
        CheckArgs(ReduceFcn, 3, args, scope);

        var functionVal = CheckForFunction(ReduceFcn, args[0], scope).FunctionValue;
        var elements = CheckForList(ReduceFcn, args[1], scope);

        var start = cast(args[2], LispVariant);
        var result = LispVariant.forValue(start);
        for (elem in elements)
        {
            // call for every element the given function (args[0])
            var arr = new Array<Dynamic>();
            arr.push(elem);
            arr.push(result);
            result = functionVal.Function(/*new[] { elem, result }*/arr, scope);
        }
        return result;
    }

    public static function Cons(/*object[]*/ args:Array<Dynamic>, scope:LispScope):LispVariant
    {
        var result = new LispVariant(LispType.List, new Array<Dynamic>());  //new List<object>()
        if (args.length > 0)
        {
            result.Add(args[0]);
        }
        if (args.length > 1)
        {
            var item2 = cast(args[1], LispVariant);
            if (item2.IsList)
            {
                for (item in item2.ListValue)
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

    public static function Length(/*object[]*/ args:Array<Dynamic>, scope:LispScope):LispVariant
    {
        CheckArgs("len", 1, args, scope);

        var val = cast(args[0], LispVariant);
        if (val.IsNativeObject)
        {
//TODO
            // if (val.Value is Dictionary<object, object>)
            // {
            //     return new LispVariant(((Dictionary<object, object>)val.Value).Count);
            // }
        }
        if (val.IsString)
        {
            return LispVariant.forValue(val.StringValue.length);
        }
        var elements = val.ListValue;
        return LispVariant.forValue(elements.length/*Count()*/);
    }

    public static function FirstElem(/*object[]*/ args:Array<Dynamic>, scope:LispScope):LispVariant
    {
        CheckArgs("first", 1, args, scope);

        var val = cast(args[0], LispVariant);
        if (val.IsString)
        {
            return LispVariant.forValue(val.StringValue.substr(0, 1));
        }
        var elements = val.ListValue;
        if (scope.NeedsLValue)
        {
            var /*List<object>*/ container:Array<Dynamic> = elements; // as List<object>;
            //Action<object> action = (v) => { container[0] = v; };
            var action = function (v) { container[0] = v; };
            return new LispVariant(LispType.LValue, action);
        }
        else
        {
            return LispVariant.forValue(elements.First());
        }
    }

    public static function LastElem(/*object[]*/ args:Array<Dynamic>, scope:LispScope):LispVariant
    {
        CheckArgs("last", 1, args, scope);

        var val = cast(args[0], LispVariant);
        if (val.IsString)
        {
            return LispVariant.forValue(val.StringValue.substr(val.StringValue.length-1));
        }
        var elements = val.ListValue;
        if (scope.NeedsLValue)
        {
            var /*List<object>*/ container:Array<Dynamic> = elements; // as List<object>;
            //Action<object> action = (v) => { container[container.Count - 1] = v; };
            var action = function (v) { container[container.length - 1] = v; };
            return new LispVariant(LispType.LValue, action);
        }
        else
        {
            return LispVariant.forValue(elements.Last());
        }
    }

    public static function Rest(/*object[]*/ args:Array<Dynamic>, scope:LispScope):LispVariant
    {
        CheckArgs("rest", 1, args, scope);

        var val = cast(args[0], LispVariant);
        if(val.IsString)
        {
            return LispVariant.forValue(val.StringValue.substr(1));
        }
        var elements = val.ListValue;
        return LispVariant.forValue(elements.Skip(1));
    }

    public static function Nth(/*object[]*/ args:Array<Dynamic>, scope:LispScope):LispVariant
    {
        CheckArgs("nth", 2, args, scope);

        var index = cast(args[0], LispVariant).IntValue;
        var val = cast(args[1], LispVariant);
        if (val.IsString)
        {
            return LispVariant.forValue(val.StringValue.substr(index, 1));
        }
        var elements = val.ListValue;
        if(scope.NeedsLValue)
        {
            var /*List<object>*/ container:Array<Dynamic> = elements; // as List<object>;
            //Action<object> action = (v) => { container[index] = v; };
            var action = function (v) { container[index] = v; };
            return new LispVariant(LispType.LValue, action);
        }
        else
        {
            return LispVariant.forValue(elements.ElementAt(index));
        }
    }

    public static function Push(/*object[]*/ args:Array<Dynamic>, scope:LispScope):LispVariant
    {
        CheckOptionalArgs("push", 2, 3, args, scope);

        var val = cast(args[0], LispVariant);
        var list = cast(args[1], LispVariant);
        var pos = args.length > 2 ? cast(args[2], LispVariant).ToInt() : 0;
        if (list.IsList)
        {
            var elements = list.ListRef;
            if (pos < elements.length)
            {
                elements.Insert(pos, val);
                return LispVariant.forValue(elements);
            }
            return LispVariant.forValue(LispType.Nil);
        }
        else
        {
            throw new LispException('push not supported for type ${GetLispType(list)}');
        }
    }

    public static function Pop(/*object[]*/ args:Array<Dynamic>, scope:LispScope):LispVariant
    {
        CheckOptionalArgs("pop", 1, 2, args, scope);

        var list = cast(args[0], LispVariant);
        var pos = args.length > 1 ? cast(args[1], LispVariant).ToInt() : 0;
        if (list.IsList)
        {
            var elements = list.ListRef;
            if (pos < elements.length)
            {
                var elem = elements.ElementAt(pos);
                elements.RemoveAt(pos);
                return LispVariant.forValue(elem);
            }
            return LispVariant.forValue(LispType.Nil);
        }
        else
        {
            throw new LispException('pop not supported for type ${GetLispType(list)}');
        }
    }

    public static function Append(/*object[]*/ args:Array<Dynamic>, scope:LispScope):LispVariant
    {
        var result = new LispVariant(LispType.List, new Array<Dynamic>());  //List<object>
        for (listElement in args)
        {
            var lst = cast(listElement, LispVariant).ListValue;
            for (item in lst)
            {
                result.Add(item);                    
            }
        }
        return result;
    }

    public static function Reverse(/*object[]*/ args:Array<Dynamic>, scope:LispScope):LispVariant
    {
        CheckArgs("reverse", 1, args, scope);

        var val = cast(args[0], LispVariant);
        if (val.IsString)
        {
            return LispVariant.forValue(val.StringValue.reverse());
        }
        var elements = val.ListValue.copy();
        elements.reverse();
        return LispVariant.forValue(elements);
    }

    public static function RValue(/*object[]*/ args:Array<Dynamic>, scope:LispScope):LispVariant
    {
        CheckArgs("rval", 1, args, scope);

        var originalLValue = scope.NeedsLValue;
        scope.NeedsLValue = false;
        var value = EvalArgIfNeeded(args[0], scope);
        scope.NeedsLValue = originalLValue;
        return value; //  new LispVariant(value);
    }

    public static function Symbol(/*object[]*/ args:Array<Dynamic>, scope:LispScope):LispVariant
    {
        return FuelFuncWrapper1/*<LispVariant, LispVariant>*/(args, scope, Sym, function (arg1) { new LispVariant(LispType.Symbol, arg1.ToStr()); } );
    }

    public static function ConvertToString(/*object[]*/ args:Array<Dynamic>, scope:LispScope):LispVariant
    {
        CheckArgs(Str, 1, args, scope);

        var value = cast(args[0], LispVariant).ToStr();
        // convert native object into a readable form
        // used for: (println (str nativeLst))
        if (args[0] is LispVariant)
        {
            var variant = cast(args[0], LispVariant);
//TODO            
            // if (variant.IsNativeObject)
            // {
            //     value = variant.NativeObjectStringRepresentation;
            // }                
        }
        return new LispVariant(LispType.String, value);
    }

    public static function ArgsCountFcn(/*object[]*/ args:Array<Dynamic>, scope:LispScope):LispVariant
    {
        //return FuelFuncWrapper0/*<int>*/(args, scope, "argscount", function () { return (cast(scope.get_value(ArgsMeta), LispVariant)).ListValue.length; });
        return FuelFuncWrapper0/*<int>*/(args, scope, "argscount", function () { return (LispUtils.CastDynamicToLispVariant(scope.get_value(ArgsMeta))).ListValue.length; });
    }

    public static function ArgsFcn(/*object[]*/ args:Array<Dynamic>, scope:LispScope):LispVariant
    {
//        return FuelFuncWrapper0(args, scope, "args", function () { return (cast(scope.get_value(ArgsMeta), LispVariant)).ListValue/*.ToArray()*/; });
        return FuelFuncWrapper0(args, scope, "args", function () { return (LispUtils.CastDynamicToLispVariant(scope.get_value(ArgsMeta)).ListValue/*.ToArray()*/); });
    }

    public static function ArgFcn(/*object[]*/ args:Array<Dynamic>, scope:LispScope):LispVariant
    {
        CheckArgs("arg", 1, args, scope);

        var index = cast(args[0], LispVariant).IntValue;
        var array = LispUtils.CastDynamicToLispVariant(scope.get_value(ArgsMeta)).ListValue;  //cast(scope.get_value(_valueArgsMeta), LispVariant).ListValue/*.ToArray()*/;
        if (index >= 0 && index < array.length)
        {
            return LispVariant.forValue(array[index]);
        }
        throw new LispException('Index out of range in args function (index=$index max=${array.length})');
    }
    
    public static function ApplyFcn(/*object[]*/ args:Array<Dynamic>, scope:LispScope):LispVariant
    {
        CheckArgs(Apply, 2, args, scope);

        var fcn = LispInterpreter.EvalAst(args[0], scope);

        var arguments = cast(args[1], LispVariant);

        if (arguments.IsList)
        {
            var argumentsArray = arguments.ListValue/*.ToArray()*/;
            var result = fcn.FunctionValue.Function(argumentsArray, scope);
            return result;
        }

        throw LispException.fromScope("Expected list as arguments in apply", scope);
    }

    public static function EvalFcn(/*object[]*/ args:Array<Dynamic>, scope:LispScope):LispVariant
    {
        CheckArgs("eval", 1, args, scope);

        var result:LispVariant;
        // convert LispVariant.List --> object[] needed for evaluation
        var variant = cast(args[0], LispVariant);
        if (variant.IsList)
        {
            var /*object[]*/ code = variant.ListValue/*.ToArray()*/;
            result = LispInterpreter.EvalAst(code, scope);
        }
        else
        {
            result = LispInterpreter.EvalAst(variant, scope);
        }
        return result;
    }

    public static function EvalStrFcn(/*object[]*/ args:Array<Dynamic>, scope:LispScope):LispVariant
    {
        CheckArgs("evalstr", 1, args, scope);

        var variant = cast(args[0], LispVariant);
        var tempModuleName = scope.ModuleName;
        scope.IsInEval = true;
        var result = Lisp.Eval(variant.ToStr(), scope, EvalStrTag + Std.string(scope.ModuleName) + ":" + variant.ToStr());
        scope.IsInEval = false;
        scope.ModuleName = tempModuleName;
        return result;
    }

    public static function MakeDict(/*object[]*/ args:Array<Dynamic>, scope:LispScope):LispVariant
    {
        return FuelFuncWrapper0/*<LispVariant>*/(args, scope, "make-dict", function ():LispVariant { return new LispVariant(LispType.NativeObject, new Map<Dynamic, Dynamic>()); });  //new LispVariant(LispType.NativeObject, new Dictionary<object, object>())
    }

    public static function DictSet(/*object[]*/ args:Array<Dynamic>, scope:LispScope):LispVariant
    {
        return FuelFuncWrapper3/*<LispVariant, LispVariant, LispVariant, LispVariant>*/(args, scope, "dict-set", function (arg1, arg2, arg3)
        {
            var nativeDict = cast(arg1.Value, Map<Dynamic, Dynamic>);  //as Dictionary<object, object>;
            nativeDict[arg2.Value] = arg3;
            return arg3;
        });
    }

    public static function DictGet(/*object[]*/ args:Array<Dynamic>, scope:LispScope):LispVariant
    {
        return FuelFuncWrapper2/*<LispVariant, LispVariant, LispVariant>*/(args, scope, "dict-get", function (arg1, arg2)
        {
            var nativeDict = cast(arg1.Value, Map<Dynamic, Dynamic>);  //as Dictionary<object, object>;
            return nativeDict.exists(arg2.Value) ? cast(nativeDict[arg2.Value], LispVariant) : new LispVariant(LispType.Undefined);
        });
    }

    public static function DictRemove(/*object[]*/ args:Array<Dynamic>, scope:LispScope):LispVariant
    {
        return FuelFuncWrapper2/*<LispVariant, LispVariant, bool>*/(args, scope, "dict-remove", function (arg1, arg2)
        {
            var nativeDict = cast(arg1.Value, Map<Dynamic, Dynamic>);  //as Dictionary<object, object>;
            var ok = nativeDict.remove(arg2.Value);
            return ok;
        });
    }

    public static function DictKeys(/*object[]*/ args:Array<Dynamic>, scope:LispScope):LispVariant
    {
        CheckArgs("dict-keys", 1, args, scope);

        var dict = cast(args[0], LispVariant);
        var nativeDict = cast(dict.Value, Map<Dynamic, Dynamic>);  //as Dictionary<object, object>;
        var /*List<LispVariant>*/ result = new Array<Dynamic>();
        for(key in nativeDict.keys())
        {
            result.Add(new LispVariant(LispVariant.GetTypeFor(key), key));
        }

        return LispVariant.forValue(result);
    }

    public static function DictClear(/*object[]*/ args:Array<Dynamic>, scope:LispScope):LispVariant
    {
        return FuelFuncWrapper1/*<LispVariant, LispVariant>*/(args, scope, "dict-clean", function (arg1:LispVariant):LispVariant
        {
            var nativeDict = cast(arg1.Value, Map<Dynamic, Dynamic>);  //as Dictionary<object, object>;
            nativeDict.clear();
            return new LispVariant(LispType.Undefined);
        });
    }

    public static function DictContainsKey(/*object[]*/ args:Array<Dynamic>, scope:LispScope):LispVariant
    {
        return FuelFuncWrapper2/*<LispVariant, LispVariant, bool>*/(args, scope, "dict-contains-key", function (arg1, arg2):Bool
        {
            var nativeDict = cast(arg1.Value, Map<Dynamic, Dynamic>);  //as Dictionary<object, object>;
            var ok = nativeDict.exists(arg2.Value);
            return ok;
        });
    }

    public static function DictContainsValue(/*object[]*/ args:Array<Dynamic>, scope:LispScope):LispVariant
    {
        return FuelFuncWrapper2/*<LispVariant, LispVariant, bool>*/(args, scope, "dict-contains-value", function (arg1, arg2):Bool
        {
            var nativeDict = cast(arg1.Value, Map<Dynamic, Dynamic>);  //as Dictionary<object, object>;
            for (key in nativeDict.keys())
            {
                var temp = nativeDict[key];
                var val = cast(temp, LispVariant);
                if (arg2.Value == val.Value)
                {
                    return true;
                }
            }
            return false;
        });
    }

    private static function CheckForFunction(functionName:String, /*object*/ arg0:Dynamic, scope:LispScope):LispVariant
    {
        var functionVal = cast(arg0, LispVariant);
        if (!functionVal.IsFunction)
        {
            throw LispException.fromScope("No function in " + functionName, scope);
        }
        return functionVal;
    }

    private static function CheckForList(functionName:String, /*object*/ listObj:Dynamic, scope:LispScope):Array<Dynamic>  //IEnumerable<object>
    {
        if (listObj is Array/*object[]*/)
        {
            return GetExpression(listObj);
        }
        var value = cast(listObj, LispVariant);
        if (value.IsNativeObject && (value.Value is /*IEnumerable<object>*/Array))
        {
            return cast(value.Value, Array<Dynamic>);  // IEnumerable<object>
        }
        if (!value.IsList)
        {
            throw new LispException("No list in " + functionName, scope.GetPreviousToken(cast(listObj, LispVariant).Token), scope.ModuleName, scope.DumpStackToString());
        }
        return value.ListValue;
    }

    private static function CompareOperation(/*object[]*/ args:Array<Dynamic>, /*Func<LispVariant, LispVariant, LispVariant>*/ op:Dynamic, scope:LispScope, name:String):LispVariant
    {
        return FuelFuncWrapper2/*<LispVariant, LispVariant, LispVariant>*/(args, scope, name, function(arg1, arg2):LispVariant return op(arg1, arg2));
    }

    private static function ArithmetricOperation(/*IEnumerable<object>*/ args:Array<Dynamic>, /*Func<LispVariant, LispVariant, LispVariant>*/ op:Dynamic):LispVariant 
    {
        var result:LispVariant = null;
        for (elem in args)
        {
            if (result == null)
            {
                result = LispVariant.forValue(elem/*.Value*/);
            }
            else
            {
                result = op(result, elem);
            }
        }
        return LispVariant.forValue(result/*.Value*/);
    }

    public static function quote_form(/*object[]*/ args:Array<Dynamic>, scope:LispScope):LispVariant
    {
        return FuelFuncWrapper1/*<object, LispVariant>*/(args, scope, Quote, function (arg1) { return LispVariant.forValue(arg1); });
    }

    private static function ProcessQuotedSExpression(/*IEnumerable<object>*/ expr:Array<Dynamic>, scope:LispScope, /*out*/ splicing:Ref<Bool>):Dynamic
    {
        //List<object> result = new List<object>();
        var result = new Array<Dynamic>();

        splicing.value = false;

        if (expr.length == 2)
        {
            var item1 = expr.First();
            var item2 = expr.ElementAt(1);
            if (item1 is LispVariant)
            {
                var variant = cast(item1, LispVariant);
                if (variant.IsSymbol && (variant.ToStr() == UnQuote || variant.ToStr() == UnQuoteSplicing))
                {
                    var evalResult = LispInterpreter.EvalAst(item2, scope);
                    splicing.value = variant.ToStr() == UnQuoteSplicing;
                    evalResult.IsUnQuoted = splicing.value ? LispUnQuoteModus.UnQuoteSplicing : LispUnQuoteModus.UnQuote;
                    return evalResult;
                }
            }
            result.Add(item1);
            result.Add(item2);
        }
        else
        {
            for (itm in expr)
            {
                if (itm is Array/*IEnumerable<object>*/)
                {
                    var tempSplicing:Ref<Bool> = new Ref<Bool>(false);
                    var res = ProcessQuotedSExpression(itm /*as IEnumerable<object>*/, scope, /*out*/ tempSplicing);
                    if (tempSplicing.value)
                    {
                        var variant = cast(res, LispVariant);
                        result.AddRange(variant.ListValue);
                    }
                    else
                    {
                        result.Add(res);
                    }
                }
                else
                {
                    result.Add(itm);
                }
            }
        }
        return result;
    }

    public static function quasiquote_form(/*object[]*/ args:Array<Dynamic>, scope:LispScope):LispVariant
    {
        CheckArgs(Quasiquote, 1, args, scope);

        // iterate through arguments and evaluate unquote/splicing expressions
        var expression = args[0];
        if (expression is LispVariant)
        {
            return cast(expression, LispVariant);
        }
        else if(expression is Array/*IEnumerable<object>*/)
        {
            var splicing = new Ref<Bool>(false);
            return new LispVariant(ProcessQuotedSExpression(expression /*Array<Dynamic>*//*as IEnumerable<object>*/, scope, /*out*/ splicing));
        }
        return LispVariant.forValue(expression);
    }
    
    public static function unquote_form(/*object[]*/ args:Array<Dynamic>, scope:LispScope):LispVariant
    {
        return FuelFuncWrapper1/*<object, LispVariant>*/(args, scope, UnQuote, function (arg1) { return LispVariant.forValue(arg1); });
    }

    public static function unquotesplicing_form(/*object[]*/ args:Array<Dynamic>, scope:LispScope):LispVariant
    {
        return FuelFuncWrapper1/*<object, LispVariant>*/(args, scope, UnQuoteSplicing, function (arg1) { return LispVariant.forValue(arg1); });
    }

    public static function if_form(/*object[]*/ args:Array<Dynamic>, scope:LispScope):LispVariant
    {
        if (!(args.length == 2 || args.length == 3))
        {
            // throw exception
            CheckArgs(If, -1, args, scope);                
        }

        var passed = LispInterpreter.EvalAst(args[0], scope).BoolValue;
        var elseCode = args.length > 2 ? args[2] : null;
        return LispInterpreter.EvalAst(passed ? args[1] : elseCode, scope);
    }

    public static function while_form(/*object[]*/ args:Array<Dynamic>, scope:LispScope):LispVariant
    {
        CheckArgs(While, 2, args, scope);

        var result = new LispVariant(null);
        var condition = LispInterpreter.EvalAst(args[0], scope);
        while (condition.ToBool())
        {
            result = LispInterpreter.EvalAst(args[1], scope);
            if (scope.IsInReturn)
            {
                break;
            }
            condition = LispInterpreter.EvalAst(args[0], scope);
        }
        return result;
    }

    public static function do_form(/*object[]*/ args:Array<Dynamic>, scope:LispScope):LispVariant
    {
        var result = LispVariant.forValue();

        for (statement in args)
        {
            var lv:LispVariant = statement;
            if (!((statement is /*Enumerable<object>*/Array) || ((statement is LispVariant) && cast(statement, LispVariant).IsList)))
            {
                throw new LispException("List expected in do", (cast(statement, LispVariant)).Token, scope.ModuleName, scope.DumpStackToString());
            }
            result = LispInterpreter.EvalAst(statement, scope);
            if (scope.IsInReturn)
            {
                break;
            }
        }

        return result;
    }

    public static function fn_form(/*object[]*/ args:Array<Dynamic>, scope:LispScope):LispVariant
    {
        var name = cast(scope.UserData, String);
        var moduleName = scope.ModuleName;
        var userDoc = scope.UserDoc;
        var signature = userDoc != null ? userDoc.value1 : null;
        var documentation = userDoc != null ? userDoc.value2 : null;
        
        var /*Func<object[], LispScope, LispVariant>*/ fcn:Dynamic =
            function (localArgs:Array<Dynamic>, localScope:LispScope):LispVariant
            {
                var childScope = LispScope.forFunction(name, localScope.GlobalScope, moduleName);
                localScope.PushNextScope(childScope);

                // add formal arguments to current scope
                var i = 0;
                var formalArgs:Array<Dynamic> = (args[0] is LispVariant ? (cast(args[0], LispVariant)).ListValue : GetExpression(args[0]))/*.ToArray()*/;

                if (formalArgs.length > localArgs.length)
                {
                    //throw new LispException("Invalid number of arguments");

                    // fill all not given arguments with nil
                    var newLocalArgs = new Array<Dynamic>();  //object[formalArgs.Length];
                    newLocalArgs.resize(formalArgs.length);
                    for (n in 0...formalArgs.length)
                    {
                        if (n < localArgs.length)
                        {
                            newLocalArgs[n] = localArgs[n];
                        }
                        else
                        {
                            newLocalArgs[n] = new LispVariant(LispType.Nil);
                        }
                    }

                    localArgs = newLocalArgs;
                }

                for (arg in formalArgs)
                {
                    childScope.set(arg.ToStr(), localArgs[i]);
                    i++;
                }

                // support args function for accessing all given parameters
                childScope.set(ArgsMeta, LispVariant.forValue(localArgs));
                var formalArgsCount:Int = formalArgs.length;
                if (localArgs.length > formalArgsCount)
                {
                    var additionalArgs = new Array<Dynamic>();  //object[localArgs.Length - formalArgsCount];
                    additionalArgs.resize(localArgs.length - formalArgsCount);
                    for (n in 0...localArgs.length - formalArgsCount)
                    {
                        additionalArgs[n] = localArgs[n + formalArgsCount];
                    }
                    childScope.set(AdditionalArgs, LispVariant.forValue(additionalArgs));
                }

                // save the current call stack to resolve variables in closures
                childScope.ClosureChain = scope;
                childScope.NeedsLValue = scope.NeedsLValue;     // support setf in recursive calls

                var ret:LispVariant;
                try
                {
                    ret = LispInterpreter.EvalAst(args[1], childScope);
                }
/* //TODO                
                catch (ex:LispStopDebuggerException)
                {
                    // forward a debugger stop exception to stop the debugger loop
                    throw ex;
                }
*/                
                catch (ex:haxe.Exception)
                {
                    // add the stack info and module name to the data of the exception
//TODO                    ex.AddModuleNameAndStackInfos(childScope.ModuleName, childScope.DumpStackToString());
//TODO                    ex.AddTokenInfos(childScope.CurrentToken);

                    var debugger = scope.GlobalScope.Debugger;
                    if (debugger != null)
                    {
                        scope.GlobalScope.Output.WriteLine(Std.string(ex));

                        debugger.InteractiveLoop(/*initialTopScope:*/ childScope, /*currentAst: (IList<object>)*/(args[1]) /*new List<object> { info.Item2 }*/ );
                    }

                    throw ex;
                }
                localScope.PopNextScope();
                return ret;
            };

        return LispVariant.forValue(CreateFunction(fcn, signature, documentation, /*isBuiltin:*/ false, /*isSpecialForm:*/ false, /*isEvalInExpand:*/ false, /*moduleName:*/ scope.ModuleName));
    }

    //
    // for tests with overloaded operators
    //
    // public static function Addition(/*object[]*/ args:Array<Dynamic>, scope:LispScope):LispVariant
    // {
    //     //var val1:OpLispVariant = cast(args[0], OpLispVariant);
    //     //var val2:OpLispVariant = cast(args[1], OpLispVariant);
    //     //var val1 = new OpLispVariant(args[0]);
    //     //var val2 = new OpLispVariant(args[1]);        
    //     //trace("ADD:", val1.Value + val2.Value);
    //     //var sum = LispVariant.add(val1, val2);
    //     //var sum:LispVariant = val1 + val2;
    //     //return LispVariant.forValue(sum);

    //     return ArithmetricOperation(args, function(l:OpLispVariant, r:OpLispVariant) return l + r);
    // }

    // private static function ArithmetricOperation(/*IEnumerable<object>*/ args:Array<Dynamic>, /*Func<LispVariant, LispVariant, LispVariant>*/ op:Dynamic):LispVariant 
    // {
    //     var result:OpLispVariant = null;
    //     for (elem in args)
    //     {
    //         if (result == null)
    //         {
    //             result = new OpLispVariant(elem);
    //         }
    //         else
    //         {
    //             result = op(result, elem);
    //         }
    //     }
    //     return LispVariant.forValue(result.Value);
    // }

    private static function GetStringRepresentation(/*object[]*/ args:Array<Dynamic>, scope:LispScope, separator:String = " "):String
    {
        var text = "";  //string.Empty;
        for (item in args)
        {
            if (text.length > 0)
            {
                text += separator;
            }
            text += item.ToStr();
        }
        /*TODO
        if (scope.ContainsKey(Traceon) && (bool)scope[Traceon])
        {
            var buffer = (StringBuilder)scope[Tracebuffer];
            buffer.Append(text);
        }
        */
        return text;
    }

    public static function IsInModules(funcName:String, scope:LispScope):Bool
    {
        var value:Ref<Dynamic> = new Ref<Dynamic>(null);  //object
        return FindFunctionInModules(funcName, scope, /*out*/ value);
    }

    public static function GetFunctionInModules(funcName:String, scope:LispScope):Dynamic  //object
    {
        var result:Ref<Dynamic> = new Ref<Dynamic>(null);  //object
        FindFunctionInModules(funcName, scope, /*out*/ result);
        return result.value;
    }

    public static function IsMacro(funcName:Dynamic, scope:LispScope):Bool
    {
        return ExistsItem(funcName, scope, Macros);
    }

    public static function GetMacro(funcName:Dynamic, scope:LispScope):Dynamic  //object
    {
        return QueryItem(funcName, scope, Macros);
    }

    public static function IsExpression(item:Dynamic):Bool
    {
        return (item is LispVariant && (cast(item, LispVariant)).IsList) ||
               (item is Array/*<Dynamic>*/);  //IEnumerable<object>
    }

    public static function GetExpression(item:Dynamic):Array<Dynamic>  //IEnumerable<object>
    {
        if (item is LispVariant && (cast(item, LispVariant)).IsList)
        {
            return (cast(item, LispVariant)).ListValue;
        }
        if (item is Array/*<Dynamic>*/)  //IEnumerable<object>
        {
            return cast(item, Array<Dynamic>);  //IEnumerable<object>
        }
        return new Array<Dynamic>() [ item ];  // List<object>
    }

    private static function QueryItem(funcName:Dynamic, scope:LispScope, key:String):Dynamic  //object
    {
        var val:Ref<Dynamic> = new Ref<Dynamic>(null);
        var val2:Ref<Dynamic> = new Ref<Dynamic>(null);
        if (scope != null &&
            scope.TryGetValue(key, /*out*/ val) &&
            (cast(val.value, LispScope)).TryGetValue(funcName.ToStr(), /*out*/ val2))
        {
            return val2.value;
        }
        return null;
    }

    private static function ExistsItem(funcName:Dynamic, scope:LispScope, key:String):Bool
    {
        var val:Ref<Dynamic> = new Ref<Dynamic>(null);  //object
        if (scope != null &&
            scope.TryGetValue(key, /*out*/ val))
        {
            return (/*(LispScope)*/cast(val.value, LispScope)).ContainsKey(funcName.ToStr());
        }
        return false;
    }

    private static function FindFunctionInModules(funcName:String, scope:LispScope, /*out object*/ foundValue:Ref<Dynamic>):Bool
    {
        foundValue.value = null;
        var importedModules = LispUtils.CastDynamicToLispScope(scope.GlobalScope.get_scope(Modules));  ///*(LispScope)*/cast(scope.GlobalScope.get_scope(Modules), LispScope);
        if (importedModules != null)
        {
            for (/*KeyValuePair<string, object>*/ kv in importedModules)
            {
                //var module = /*(LispScope)*/kv.Value;
                var module:LispScope = kv;
                var val = new Ref<Dynamic>(null);  //object
                if (module.TryGetValue(funcName, /*out*/ val))
                {
                    foundValue.value = val.value;
                    return true;
                }
            }
        }
        return false;
    }

    public static function defn_form(/*object[]*/ args:Array<Dynamic>, scope:LispScope):LispVariant
    {
        return defn_form_helper(args, scope, Def);
    }

    public static function gdefn_form(/*object[]*/ args:Array<Dynamic>, scope:LispScope):LispVariant
    {
        return defn_form_helper(args, scope, Gdef);
    }

    private static function defn_form_helper(/*object[]*/ args:Array<Dynamic>, scope:LispScope, name:String):LispVariant
    {
        CheckArgs(name, 3, args, scope);

        UpdateDocumentationInformationAtScope(args, scope);

        var fn = LispUtils.CastDynamicToLispVariant(scope.GlobalScope.get_value(Fn)).FunctionValue;  //(cast(scope.GlobalScope.get_value(Fn), LispVariant)).FunctionValue;
        scope.UserData = EvalArgIfNeeded(args[0], scope).ToStr();
        var resultingFcn = fn.Function([args[1], args[2]], scope);  //(new[] { args[1], args[2] }, scope);
        scope.UserData = null;

        var defFcn = LispUtils.CastDynamicToLispVariant(scope.GlobalScope.get_value(name)).FunctionValue;  //(cast(scope.GlobalScope.get_value(name), LispVariant)).FunctionValue;
        return defFcn.Function([args[0], resultingFcn], scope);  //(new[] { args[0], resultingFcn }, scope);
    }

    private static function EvalArgIfNeeded(/*object*/ arg:Dynamic, scope:LispScope):LispVariant
    {
        return (arg is /*IEnumerable<object>*/Array || (arg is LispVariant && (cast(arg, LispVariant).IsList))) ? LispInterpreter.EvalAst(arg, scope) : cast(arg, LispVariant);
    }

    private static function GetSignatureFromArgs(/*object*/ arg0:Dynamic, name:String):String
    {
        var signature = "(" + (name != null ? name : "?");
        var formalArgsAsString = GetFormalArgsAsString(arg0);
        if (formalArgsAsString.length > 0)
        {
            signature += " ";
        }
        signature += formalArgsAsString;
        signature += ")";
        return signature;
    }

    private static function GetFormalArgsAsString(/*object*/ args:Dynamic):String
    {
        var result = "";  //string.Empty;
        var /*IEnumerable<object>*/ theArgs:Array<Dynamic> = null;
        if (args is LispVariant)
        {
            theArgs = (cast(args, LispVariant)).ListValue;
        }
        else
        {
            theArgs = /*(IEnumerable<object>)*/cast(args, Array<Dynamic>);
        }
        for (s in theArgs)
        {
            if (result.length > 0)
            {
                result += " ";
            }
            result += s;
        }
        return result;
    }

    private static function UpdateDocumentationInformationAtScope(/*object[]*/ args:Array<Dynamic>, scope:LispScope)
    {
        var documentation = "";  //string.Empty;
        var token = GetTokenBeforeDefn(args[0], scope);
        if ((token != null) && (token.Type == LispTokenType.Comment))
        {
            documentation = token/*.Value*/.ToStr();
        }
        var signature = GetSignatureFromArgs(args[1], args[0].ToStr());
        scope.UserDoc = new LispUtils.TupleReturn<String, String>(signature, documentation);
    }

    // returns token just before the defn statement:
    // item is fcn token, go three tokens before, example:
    // ; comment before defn
    // (defn fcn (x) (+ x 1))
    // --> Comment Token
    private static function GetTokenBeforeDefn(/*object*/ item:Dynamic, scope:LispScope):LispToken
    {
        if (item is LispVariant)
        {
            var tokenName:LispVariant = cast(item, LispVariant);
            var token1 = scope.GetPreviousToken(tokenName.Token);
            var token2 = scope.GetPreviousToken(token1);
            var token3 = scope.GetPreviousToken(token2);
            return token3;
        }
        return null;
    }

    private static function bool_operation_form(/*object[]*/ args:Array<Dynamic>, scope:LispScope, /*Func<bool, bool, bool>*/ func:Dynamic, initial:Bool):LispVariant
    {
        var result = initial;
        for (arg in args)
        {
            var value:Bool = LispInterpreter.EvalAst(arg, scope).BoolValue;
            result = func(result, value);
            if(initial) {
                // process and
                if (!result)
                {
                    break;
                }
            } else {
                // process or
                if (result)
                {
                    break;
                }        
            }
        }
        return LispVariant.forValue(result);
    }

    public static function and_form(/*object[]*/ args, scope:LispScope):LispVariant
    {
        return bool_operation_form(args, scope, function (r:Bool, v:Bool) { return r && v; }, true);
    }

    public static function or_form(/*object[]*/ args:Array<Dynamic>, scope:LispScope):LispVariant
    {
        return bool_operation_form(args, scope, function (r:Bool, v:Bool) { return r || v; }, false);
    }

    public static function def_form(/*object[]*/ args:Array<Dynamic>, scope:LispScope):LispVariant
    {
        return def_form_helper(args, scope, Def, scope);
    }

    public static function gdef_form(/*object[]*/ args:Array<Dynamic>, scope:LispScope):LispVariant
    {
        return def_form_helper(args, scope, Gdef, scope.GlobalScope);
    }

    private static function def_form_helper(/*object[]*/ args:Array<Dynamic>, scope:LispScope, name:String, scopeToSet:LispScope):LispVariant
    {
        CheckArgs(name, 2, args, scope);

        var symbol = EvalArgIfNeeded(args[0], scope);
        if (!(symbol.IsSymbol || symbol.IsString))
        {
            throw LispException.fromScope("Symbol expected", scope);
        }
        var value = LispInterpreter.EvalAst(args[1], scope);
        scopeToSet.set(symbol.ToStr(), value);
        return LispVariant.forValue(value);
    }

    public static function setf_form(/*object[]*/ args:Array<Dynamic>, scope:LispScope):LispVariant
    {
        CheckArgs(Setf, 2, args, scope);

        var originalNeedsLValue = scope.NeedsLValue;
        scope.NeedsLValue = true;
        var symbol = EvalArgIfNeeded(args[0], scope);
        scope.NeedsLValue = originalNeedsLValue;  
        var symbolName = symbol != null ? symbol.ToStr() : null;
        var value = LispInterpreter.EvalAst(args[1], scope);
        if(symbol.IsLValue)
        {
            var /*Action<object>*/ action:Dynamic = /*(Action<object>)*/symbol.Value;
            action(value);
        }
        else
        {
            scope.SetInScopes(symbolName, value);
        }
        return value;
    }

    private static function definemacroevaluate_form(/*object[]*/ args:Array<Dynamic>, scope:LispScope):LispVariant
    {
        CheckArgs(DefineMacroEval, 3, args, scope);

        var macros = LispUtils.CastDynamicToLispScope(scope.GlobalScope.get_scope(Macros));  //cast(scope.GlobalScope.get_value(Macros), LispScope);
        if (macros != null)
        {
            macros.set(args[0].ToStr(), new LispMacroRuntimeEvaluate(cast(args[1], LispVariant).ListValue, cast(args[2], LispVariant).ListValue));
        }

        return null;
    }

#if ENABLE_COMPILE_TIME_MACROS 

    // (define-macro-expand name (args) (expression))
    private static function definemacroexpand_form(/*object[]*/ args:Array<Dynamic>, scope:LispScope):LispVariant
    {
        CheckArgs(DefineMacroExpand, 3, args, scope);

        var macros = cast(scope.GlobalScope.get_scope(Macros), LispScope);
        if (macros != null)
        {
            // allow macros in macros --> recursive call for ExpandMacros()
            var result = LispInterpreter.ExpandMacros(GetExpression(args[2]), scope);
            macros.set(args[0].ToStr(), new LispMacroCompileTimeExpand(GetExpression(args[1]), result /*as IEnumerable<object>*/));
        }

        return null;
    }

#end

    private static function GetLispType(/*object*/ obj:Dynamic):String
    {
        var lispVariant = cast(obj, LispVariant);
        if (lispVariant != null)
        {
            return lispVariant.TypeString;
        }
        return obj.GetType().ToStr();
    }
}
