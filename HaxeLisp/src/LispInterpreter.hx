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

using LispVariant.LispType;
using LispVariant.LispUnQuoteModus;
using LispUtils.ArrayExtender;
using LispUtils.MapExtender;
using LispUtils.Ref;
using LispParser;
using LispEnvironment;
using LispDebugger;

// TODO
//class LispMacroRuntimeEvaluate {
//}

/// <summary>
/// Exception used to stop the debugger loop.
/// </summary>
/*public*/ class LispStopDebuggerException extends LispException
{
    public function new(text:String = "")
    {
        super(text);
    }
}

 /// <summary>
/// The FUEL lisp interpreter.
/// </summary>
/*public*/ class LispInterpreter
{
    /// <summary>
    /// Resolves the items of the ast in the given scope.
    /// </summary>
    /// <param name="scope"></param>
    /// <param name="astAsList"></param>
    /// <param name="compile"></param>
    /// <returns></returns>
    public static function ResolveArgsInScopes(scope:LispScope, /*IEnumerable<object>*/ astAsList:Array<Dynamic>, compile:Bool):Array<Dynamic> //List<object>
    {
        var astWithResolvedValues = new Array<Dynamic>();  //List<object>();
        var firstElement = astAsList.FirstOrDefault();
        var isSpecialForm:Null<Bool> = null;
        for (elem in astAsList)
        {
            var resolvedElem:Dynamic = null;    //object
            if ((isSpecialForm != null && /*(bool)*/isSpecialForm) || !IsSymbol(elem))
            {
                resolvedElem = elem;
            }
            else
            {
                resolvedElem = scope.ResolveInScopes(elem, elem == firstElement);
            }
            astWithResolvedValues.push(resolvedElem);

            if (isSpecialForm == null)
            {
                var firstElem = new LispVariant.LispFunctionWrapper();
                var first:Dynamic = null;  //object
                try
                {
                    first = astWithResolvedValues.First();
                    firstElem = LispUtils.ToLispVariant(first).FunctionValue;
                }
                catch (exc:/*LispException*/haxe.Exception)
                {
                    if (!compile)
                    {
                        throw new LispException("Function \"" + firstElement.ToStr() + "\" not found"/* TODO, scope*/);
                    }
                }
                isSpecialForm = firstElem.IsSpecialForm;
            }

        }
        return astWithResolvedValues;
    }

    /// <summary>
    /// Evaluates the given ast.
    /// </summary>
    /// <param name="ast">The ast.</param>
    /// <param name="scope">The scope.</param>
    /// <returns>The result of ast evaluation.</returns>
    /// <exception cref="System.Exception">Unexpected macro modus!</exception>
    public static function EvalAst(/*object*/ ast:Dynamic, scope:LispScope):LispVariant
    {
        if (ast == null)
        {
            return null;
        }

        var astAsList:Array<Dynamic>;  //IList<object>

        if (ast is LispVariant)
        {
            var item = /*(LispVariant)*/cast(ast, LispVariant);
            // evaluate the value for the symbol
            if (item.IsSymbol)
            {
                item = LispVariant.forValue(scope.ResolveInScopes(item, false));
                return item;
            }
            else if (item.IsList && !item.IsNil)
            {
                astAsList = item.ListValue.ToList();
            }
            else
            {
                return item;
            }
        }
        else
        {
            astAsList = ast; //((IEnumerable<object>)ast).ToList();                
        }

        if (astAsList.length == 0)
        {
            return new LispVariant(LispType.Nil);
        }

        // is this function a macro ==> process the macro and return
        if (LispEnvironment.IsMacro(astAsList.First(), scope.GlobalScope))
        {
            // check the macro modus: evaluate or expand or lambda
            var _macro = LispEnvironment.GetMacro(astAsList.First(), scope.GlobalScope);

            // evaluate macro at run time:
            if (_macro is LispMacroRuntimeEvaluate)
            {
                // Example for macro at runtime handling:
                //
                // macro definition:
                // (define-macro-eval my-setf (x value) (setf x value))
                //
                // call (ast):
                // (my-setf a (+ \"blub\" \"xyz\")) 
                //          |         |
                //          v         v
                //          x        value
                //
                // Result:
                // (setf a (+ \"blub\" \"xyz\"))  <-- replace formal arguments (as symbol)

                var anyMacroReplaced = new Ref<Bool>(false);
                var runtimeMacro = /*(LispMacroRuntimeEvaluate)*/_macro;
                var expression = ReplaceFormalArgumentsInExpression(runtimeMacro.FormalArguments, astAsList, runtimeMacro.Expression, scope, /*ref*/ anyMacroReplaced);

                return EvalAst(expression, scope);
            }

            // expand macro at compile time: --> nothing to do at run time !
            // code not needed, because code for compile time macros will be removed in ExpandMacro phase
            //if (macro is LispMacroCompileTimeExpand)
            //{
            //    return new LispVariant();
            //}

            throw new haxe.Exception("Unexpected macro modus!");
        }

        // for debugging: update the current line number at the current scope
        var currentToken = (cast(astAsList.First(), LispVariant)).Token;
        scope.CurrentToken = currentToken != null ? currentToken : scope.CurrentToken;

        // resolve values via local and global scope
        var astWithResolvedValues = ResolveArgsInScopes(scope, astAsList, false);

        // get first element --> this is the function !
        var _function = astWithResolvedValues.First();

        // normal evaluation...
        var functionWrapper:LispVariant.LispFunctionWrapper = LispUtils.ToLispVariant(_function).FunctionValue;  //(LispVariant)

        // trace current function (if tracing is enabled)
        if (scope.GlobalScope.Tracing)
        {
            scope.GlobalScope.Output.WriteLine('--> ${astAsList.First().ToStr()}');
        }

        // evaluate arguments, but allow recursive lists
        var arguments = new Array<Dynamic>();  //new object[astWithResolvedValues.Count - 1];
        arguments.resize(astWithResolvedValues.length - 1);
        var i:Int = 1;
        while (i < arguments.length + 1)
        {
            //var asContainer = LispUtils.GetAsContainer(astWithResolvedValues[i]);
            //var needEvaluation = (asContainer != null) && !functionWrapper.IsSpecialForm;
            //arguments[i - 1] = needEvaluation ? EvalAst(asContainer, scope) : astWithResolvedValues[i];
            var needEvaluation = (astWithResolvedValues[i] is /*IEnumerable<object>*/Array/*<Dynamic>*/) &&
                                    !functionWrapper.IsSpecialForm;
            var result:Dynamic = needEvaluation ? EvalAst(astWithResolvedValues[i], scope) : LispVariant.forValue(astWithResolvedValues[i]);
            // process statemens like this: `,@l  with l = (1 2 3)
            var variant:LispVariant = result is LispVariant ? result : null; //result/* as LispVariant*/;
            if (variant != null) 
            {                    
                if (variant.IsUnQuoted == LispUnQuoteModus.UnQuoteSplicing && variant.IsList)
                {
                    var lst = variant.ListRef;
                    var newArguments = new Array<Dynamic>();  //object[arguments.Length + lst.Count - 1];
                    newArguments.resize(arguments.length + lst.length - 1);
                    arguments.CopyTo(newArguments, 0);
                    for (elem in lst)
                    {
                        newArguments[i - 1] = elem;
                        i++;
                    }

                    arguments = newArguments;
                    i++;
                    break;
                }
            }            
            arguments[i - 1] = result;
            i++;
        }

        // debugger processing
        var debugger = scope.GlobalScope.Debugger;
        if (debugger != null && debugger.NeedsBreak(scope, GetPosInfo(astAsList[0])))
        {
            debugger.InteractiveLoop(scope, astAsList, false, false);   // add parameters explicit for neko / hl
        }

        // call the function with the arguments
        return functionWrapper.Function(arguments, scope);
    }

//#if ENABLE_COMPILE_TIME_MACROS

    public static function ExpandMacros(/*object*/ ast:Dynamic, globalScope:LispScope):Dynamic
    {
        var result:Dynamic = ast;
        var anyMacroReplaced = new Ref<Bool>(false);
        do
        {
            anyMacroReplaced.value = false;
            result = ExpandMacrosHelper(result, globalScope, /*ref*/ anyMacroReplaced);
        } while (anyMacroReplaced.value);
        return result;
    }

    private static function ExpandMacrosHelper(/*object*/ ast:Dynamic, globalScope:LispScope, /*ref bool*/ anyMacroReplaced:Ref<Bool>):Dynamic
    {
        if (ast == null || ast is LispVariant)
        {
            return ast;
        }

        var astAsList = cast(ast, Array<Dynamic>);  //((IEnumerable<object>)ast).ToList();
        if (astAsList.length == 0)
        {
            return ast;
        }

        // compile time macro: process define-macro statements ==> call special form, this will add macro to global scope as side effect
        var functionVal = astAsList.First();
        var functionName = functionVal.ToStr();
        if (globalScope != null && globalScope.ContainsKey(functionName))
        {
            var fcn = LispUtils.CastDynamicToLispVariant(globalScope.get_value(functionName)).FunctionValue;  //cast(globalScope.get_value(functionName), LispVariant).FunctionValue;
            if (fcn.IsEvalInExpand)
            {
                var args = astAsList.copy();  //new Array<Dynamic>(astAsList);  //List<object>(astAsList);
                args.RemoveAt(0);

                // process compile time macro definition 
                //   --> side effect: add macro definition to internal macro scope
                fcn.Function(args/*.ToArray()*/, globalScope);

                // compile time macros definitions will be removed from code in expand macro phase
                // because only the side effect above is needed for further macro replacements
                return null;
            }
        }

        // compile time macros: process macro expansion in an expression which calls a macro
        if (LispEnvironment.IsMacro(functionVal, globalScope))
        {
            var macroVal = LispEnvironment.GetMacro(functionVal, globalScope);
            if (macroVal is LispMacroCompileTimeExpand)
            {
                anyMacroReplaced.value = true;
                var macroExpand = cast(macroVal, LispMacroCompileTimeExpand);
                var astWithReplacedArguments = ReplaceFormalArgumentsInExpression(macroExpand.FormalArguments, astAsList, macroExpand.Expression, globalScope, /*ref*/ anyMacroReplaced).ToList();   // PATCH
                // process recursive macro expands (do not wrap list as LispVariant at this point)
                return ConvertLispVariantListToListIfNeeded(EvalAst(astWithReplacedArguments, globalScope));
            }
        }

        var expandedAst = new Array<Dynamic>();  //List<object>();
        // Expand recursively and handle enumarations (make them flat !)
        for (elem in astAsList)
        {
            var expandResult = ExpandMacros(elem, globalScope);
            // ignore code which is removed in macro expand phase
            if (expandResult != null)
            {
                // process recursive macro expands (do not wrap list as LispVariant at this point)
                expandedAst.Add(ConvertLispVariantListToListIfNeeded(expandResult));
            }
        }

        return expandedAst;
    }

//#end

    private static function ConvertLispVariantListToListIfNeeded(something:Dynamic):Dynamic
    {
        if (something is LispVariant)
        {
            var variant:LispVariant = something/* as LispVariant*/;
            
            if (variant != null && variant.IsList)
            {
                return variant.ListRef;
            }
        }
        return something;
    }

    /// <summary>
    /// Get information about position in sourcecode for
    /// given item of the ast.
    /// </summary>
    /// <param name="item"></param>
    /// <returns></returns>
    private static function GetPosInfo(item:Dynamic):LispBreakpointPosition
    {
        var token:LispToken;
        if (item is LispToken)
        {
            token = /*(LispToken)*/item;
        }
        else
        {
            token = item.Token;  //((LispVariant)item).Token;
        }
        if (token != null)
        {
            return new LispBreakpointPosition(token.StartPos, token.StopPos, token.LineNo);
        }
        return new LispBreakpointPosition(-1, -1, -1);
    }

    private static function ReplaceSymbolWithValueInExpression(symbol:LispVariant, /*object*/ symbolValue:Dynamic, /*IEnumerable<object>*/ expression:Array<Dynamic>, macroArgsReplace:Bool, /*ref*/ replacedAnything:Ref<Bool>):Array<Dynamic>  //IEnumerable<object>
    {
        var ret = new Array<Dynamic>();  //List<object>();
        for(elem in expression)
        {
            // is the current element the symbol which should be replaced? --> Yes
            if (symbol.SymbolCompare(elem))
            {
                var l:Array<Dynamic> = symbolValue is Array? symbolValue : (symbolValue.IsList ? symbolValue : null)/*as IEnumerable<object>*/;  //IEnumerable<object>
                if (l != null && macroArgsReplace)
                {
                    ret.AddRange(l);
                }
                else
                {
                    ret.Add(symbolValue);
                }
                replacedAnything.value = true;
            }
            // is it an expression? --> recursive call
            else if (LispEnvironment.IsExpression(elem))
            {
                var temp:Array<Dynamic> = ReplaceSymbolWithValueInExpression(symbol, symbolValue, LispEnvironment.GetExpression(elem)/*.ToArray()*/, macroArgsReplace, /*ref*/ replacedAnything);  //IEnumerable<object>
                ret.Add(temp);
            }
            // current element is not the symbol which should by replaced !
            else
            {
                ret.Add(elem);
            }
        }
        return ret;
    }

    private static function ReplaceFormalArgumentsInExpression(/*IEnumerable<object>*/ formalArguments:Array<Dynamic>, /*IList<object>*/ astAsList:Array<Dynamic>, /*IEnumerable<object>*/ expression:Array<Dynamic>, scope:LispScope, /*ref*/ anyMacroReplaced:Ref<Bool>):Array<Dynamic>  //IEnumerable<object>
    {
        // replace (quoted-macro-args) --> '(<real_args>)
        var i = 1;
        var replaced = new Ref<Bool>(false);
        var realArguments:Array<Dynamic> = astAsList.Skip(1).ToList();  //IEnumerable<object>
        var quotedRealArguments:Array<Dynamic> = [new LispVariant(LispType.Symbol, LispEnvironment.Quote), realArguments];  //List<object> 
        expression = ReplaceSymbolWithValueInExpression(new LispVariant(LispType.Symbol, "quoted-macro-args"), quotedRealArguments, expression, true, /*ref*/ replaced);

        for (formalArgument in formalArguments)
        {
            var value:Dynamic = null;  //object
            if (astAsList[i] is /*IEnumerable<object>*/Array/*<Dynamic>*/)
            {
                value = ExpandMacrosHelper(astAsList[i], scope, /*ref*/ anyMacroReplaced);
                if (value is LispVariant)
                {
                    var vairantValue = value /*as LispVariant*/;
                    if (vairantValue.IsList)
                    {
                        value = vairantValue.ListValue;
                    }
                }
            }
            else
            {
                value = LispVariant.forValue(astAsList[i]);
            }
            expression = ReplaceSymbolWithValueInExpression(/*(LispVariant)*/formalArgument, value, expression, false, /*ref*/ anyMacroReplaced);
            i++;
        }
        return expression;
    }

    private static function IsSymbol(/*object*/ elem:Dynamic):Bool
    {
        var isSymbol = false;
        if (elem is LispVariant)
        {
            isSymbol = LispUtils.ToLispVariant(elem).IsSymbol;
        }
        return isSymbol;
    }
}
