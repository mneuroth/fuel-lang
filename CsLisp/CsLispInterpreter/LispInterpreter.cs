﻿/*
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

namespace CsLisp
{
    /// <summary>
    /// The FUEL lisp interpreter.
    /// </summary>
    public class LispInterpreter
    {
        #region public methods

        /// <summary>
        /// Resolves the items of the ast in the given scope.
        /// </summary>
        /// <param name="scope"></param>
        /// <param name="astAsList"></param>
        /// <param name="compile"></param>
        /// <returns></returns>
        public static List<object> ResolveArgsInScopes(LispScope scope, IEnumerable<object> astAsList, bool compile)
        {
            var astWithResolvedValues = new List<object>();
            bool? isSpecialForm = null;
            foreach (var elem in astAsList)
            {
                object resolvedElem;
                if ((isSpecialForm != null && (bool)isSpecialForm) || !IsSymbol(elem))
                {
                    resolvedElem = elem;
                }
                else
                {
                    resolvedElem = scope.ResolveInScopes(elem);
                }
                astWithResolvedValues.Add(resolvedElem);

                if (isSpecialForm == null)
                {
                    var firstElem = new LispFunctionWrapper();
                    object first = null;
                    try
                    {
                        first = astWithResolvedValues.First();
                        firstElem = ((LispVariant)first).FunctionValue;
                    }
                    catch (LispException)
                    {
                        if (!compile)
                        {
                            throw new LispException("Function \"" + first + "\" not found", scope);
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
        public static LispVariant EvalAst(object ast, LispScope scope)
        {
            if (ast == null)
            {
                return null;
            }

            IList<object> astAsList;

            if (ast is LispVariant)
            {
                var item = (LispVariant) ast;
                // evaluate the value for the symbol
                if (item.IsSymbol)
                {
                    item = new LispVariant(scope.ResolveInScopes(item));
                }
                if (item.IsList && !item.IsNil)
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
                astAsList = ((IEnumerable<object>)ast).ToList();                
            }

            if (astAsList.Count == 0)
            {
                return new LispVariant(LispType.Nil);
            }

            // is this function a macro ==> evaluate the macro... and return
            if (LispEnvironment.IsMacro(astAsList.First(), scope.GlobalScope))
            {
                // check the macro modus: evaluate or expand or lambda
                var macro = LispEnvironment.GetMacro(astAsList.First(), scope.GlobalScope);
                // evaluate macro:
                if (macro is Tuple<object, object>)
                {
// TODO --> replace formal with real parameters (nicht zur compile zeit wei bei macro-expand) sondern zur laufzeit !!!

                    //         Item1          Item2
                    // my-setf (x value) -->  (setf (sym (eval x)) (eval value))

                    // ast:
                    // (my-setf a (+ \"blub\" \"xyz\")) 
                    //          |         |
                    //          v         v
                    //          x        value
                    // ==> (setf (sym (eval a)) (eval (+ \"blub\" \"xyz\")))
                    // ==> (setf a (+ \"blub\" \"xyz\"))  <-- formale Parameter ersetzen (symbol weise)
                    //
                    // geht nicht, durch apply ersetzen ! Da Problem mit evaluieren der Parameter !!!
                    // --> (apply (lambda (x value) macro.Item2) '(a (+ \"blub\" \"xyz\")))

                    Tuple<object, object> runtimeMacro = (Tuple<object, object>) macro;

                    // 1) replace formal with current parameters in expression
                    // 2) evaluate expression

                    var expression = (IEnumerable<object>)runtimeMacro.Item2;
                    int i = 1;
// TODO --> Rekursive macros ... flag replaced auswerten
                    bool anyMacroReplaced = false;
                    foreach (var formalParameter in (IEnumerable<object>)runtimeMacro.Item1)
                    {
                        LispVariant value = new LispVariant(astAsList[i]);
                        expression = RepaceSymbolWithValueInExpression((LispVariant)formalParameter, value, expression, ref anyMacroReplaced);

                        i++;
                    }

                    return EvalAst(expression, scope);
                }
                // expand macro:
                if (macro is LispMacroExpand)
                {
                    return new LispVariant();
                }
// TODO --> should be deleted !!!
                // lambda macro:
                if (macro is IEnumerable<object>)
                {
                    var expression = EvaluateMacro(astAsList.First(), astAsList, scope.GlobalScope);
                    return expression;
                }
                throw new Exception("Unexpected macro modus!");
            }

            // for debugging: update the current line number at the current scope
            var currentToken = ((LispVariant)(astAsList.First())).Token;
            scope.CurrentToken = currentToken != null ? currentToken : scope.CurrentToken;

            // resolve values via local and global scope
            var astWithResolvedValues = ResolveArgsInScopes(scope, astAsList, false);

            // get first element --> this is the function !
            var function = astWithResolvedValues.First();

            // normal evaluation...
            LispFunctionWrapper functionWrapper = ((LispVariant)function).FunctionValue;

            // trace current function (if tracing is enabled)
            if (scope.GlobalScope.Tracing)
            {
                scope.GlobalScope.Output.WriteLine("--> {0}", astAsList.First());
            }

            // evaluate arguments, but allow recursive lists
            var arguments = new object[astWithResolvedValues.Count - 1];
            for (var i = 1; i < astWithResolvedValues.Count; i++)
            {
                var needEvaluation = (astWithResolvedValues[i] is IEnumerable<object>) &&
                                     !functionWrapper.IsSpecialForm;
                arguments[i - 1] = needEvaluation ? EvalAst(astWithResolvedValues[i], scope) : astWithResolvedValues[i];
            }

            // debugger processing
            var debugger = scope.GlobalScope.Debugger;
            if (debugger != null && debugger.NeedsBreak(scope, GetPosInfo(astAsList[0])))
            {
                debugger.InteractiveLoop(scope, astAsList);
            }

            // call the function with the arguments
            return functionWrapper.Function(arguments, scope);
        }

        public static object ExpandMacros(object ast, LispScope globalScope)
        {
            object result = ast;
            bool anyMacroReplaced;
            do
            {
                anyMacroReplaced = false;
                result = ExpandMacros(result, globalScope, ref anyMacroReplaced);
            } while (anyMacroReplaced);
            return result;
        }

        private static object ExpandMacros(object ast, LispScope globalScope, ref bool anyMacroReplaced)
        {
            if (ast == null || ast is LispVariant)
            {
                return ast;
            }

            var astAsList = ((IEnumerable<object>)ast).ToList();
            if (astAsList.Count == 0)
            {
                return ast;
            }

            // process define-macro statements ==> call special form, this will add macro to global scope as side effect
            var function = astAsList.First();
            var functionName = function.ToString();
//TODO --> static flag mit support fuer runtime / compile Time macros --> dieser block ist nur fuer compile time macros notwendig !
            if (globalScope != null && globalScope.ContainsKey(functionName))
            {
                var fcn = ((LispVariant)globalScope[functionName]).FunctionValue;
// TODO --> braucht man wirklich IsEvalInExpand, wenn macros ausschliesslich expandiert (ersetzt) werden? --> nur bei compileTime Macors notwendig --> define setzen ?
                if (fcn.IsEvalInExpand)
                {
                    var args = new List<object>(astAsList);
                    args.RemoveAt(0);

// TODO --> replace current macro code with (recursivly) replaced macro code after evaluation 
                    LispVariant macroEvalResult = fcn.Function(args.ToArray(), globalScope);
                    return macroEvalResult != null ? macroEvalResult.ListValue : ast;
                }
            }

            // process macro expansion in an expression which calls a macro
            if (LispEnvironment.IsMacro(function, globalScope))
            {
                var macro = LispEnvironment.GetMacro(function, globalScope);
                if (macro is LispMacroExpand)
                {
                    var macroExpand = (LispMacroExpand)macro;
                    var expression = macroExpand.Expression;

                    // replace formal parameters with actual parameters
                    int i = 1;
                    foreach(var formalParameter in macroExpand.FormalParameters)
                    {
// TODO working: do not replace anything with values, just replace macro expressions !
                        LispVariant value = new LispVariant(astAsList[i]); 
                        expression = RepaceSymbolWithValueInExpression((LispVariant) formalParameter, value, expression, ref anyMacroReplaced);
                        i++;
                    }

                    //TODO working gulp: 
// TODO working gulp --> rekursives Makro Expandieren unterstuetzen !!!
// TODO --> im code definierte funktionen sind bei expandierung der Macros noch nicht bekannt !!! --> runtime Macros verwenden !

                    return expression;
                }
            }

            var expandedAst = new List<object>();
            // Expand recursively and handle enumarations (make them flat !)
            foreach (var elem in astAsList)
            {
                var expandResult = ExpandMacros(elem, globalScope);
                expandedAst.Add(expandResult);                    
            }

            return expandedAst;
        }

        #endregion

        #region private methods

        /// <summary>
        /// Get information about position in sourcecode for
        /// given item of the ast.
        /// </summary>
        /// <param name="item"></param>
        /// <returns></returns>
        private static Tuple<int, int, int> GetPosInfo(object item)
        {
            LispToken token;
            if (item is LispToken)
            {
                token = (LispToken)item;
            }
            else
            {
                token = ((LispVariant)item).Token;
            }
            if (token != null)
            {
                return new Tuple<int, int, int>(token.StartPos, token.StopPos, token.LineNo);
            }
            return new Tuple<int, int, int>(-1, -1, -1);
        }

        private static IEnumerable<object> RepaceSymbolWithValueInExpression(LispVariant symbol, LispVariant symbolValue, IEnumerable<object> expression, ref bool replacedAnything)
        {
            var ret = new List<object>();
            foreach(var elem in expression)
            {
                // is the current element the symbol which should be replaced? --> Yes
                if (symbol.SymbolCompare(elem))
                {
                    ret.Add(symbolValue);
                    replacedAnything = true;
                }
                // is it an expression? --> recursive call
                else if (LispEnvironment.IsExpression(elem))
                {
                    IEnumerable<object> temp = RepaceSymbolWithValueInExpression(symbol, symbolValue, LispEnvironment.GetExpression(elem)/*.ToArray()*/, ref replacedAnything);
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

        private static bool IsSymbol(object elem)
        {
            bool isSymbol = false;
            if (elem is LispVariant)
            {
                var variant = (LispVariant)elem;
                isSymbol = variant.IsSymbol;
            }
            return isSymbol;
        }

        private static LispVariant EvaluateMacro(object function, IEnumerable<object> rawExpression, LispScope globalScope)
        {
            var macroFcn = LispEnvironment.GetMacro(function, globalScope);

            var argsTmp = new List<object>(rawExpression);
            argsTmp.RemoveAt(0);
            var args = argsTmp.ToArray();

            var evalMacro = new List<object>();
            evalMacro.Add(new LispVariant(LispType.Symbol, LispEnvironment.Apply));
            evalMacro.Add(macroFcn);

            var arguments = new LispVariant(LispType.List, new List<object>());
            foreach (var arg in args)
            {
                arguments.Add(EvalAst(arg, globalScope));
            }

            evalMacro.Add(arguments);

            // evaluate macroFcn with given args
            // (apply (lambda ...) arg1 arg2 ...)
            var result = EvalAst(evalMacro, globalScope);
            return result;
        }

        #endregion
    }
}
