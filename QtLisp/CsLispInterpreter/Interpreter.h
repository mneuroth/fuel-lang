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
                var item = (LispVariant)ast;
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

            // is this function a macro ==> process the macro and return
            if (LispEnvironment.IsMacro(astAsList.First(), scope.GlobalScope))
            {
                // check the macro modus: evaluate or expand or lambda
                var macro = LispEnvironment.GetMacro(astAsList.First(), scope.GlobalScope);

                // evaluate macro at run time:
                if (macro is LispMacroRuntimeEvaluate)
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

                    bool anyMacroReplaced = false;
                    var runtimeMacro = (LispMacroRuntimeEvaluate)macro;
                    var expression = ReplaceFormalArgumentsInExpression(runtimeMacro.FormalArguments, astAsList, runtimeMacro.Expression, ref anyMacroReplaced);

                    return EvalAst(expression, scope);
                }
                
                // expand macro at compile time: --> nothing to do at run time !
                // code not needed, because code for compile time macros will be removed in ExpandMacro phase
                //if (macro is LispMacroCompileTimeExpand)
                //{
                //    return new LispVariant();
                //}

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

#if ENABLE_COMPILE_TIME_MACROS 

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

            // compile time macro: process define-macro statements ==> call special form, this will add macro to global scope as side effect
            var function = astAsList.First();
            var functionName = function.ToString();
            if (globalScope != null && globalScope.ContainsKey(functionName))
            {
                var fcn = ((LispVariant)globalScope[functionName]).FunctionValue;
                if (fcn.IsEvalInExpand)
                {
                    var args = new List<object>(astAsList);
                    args.RemoveAt(0);

                    // process compile time macro definition 
                    //   --> side effect: add macro definition to internal macro scope
                    fcn.Function(args.ToArray(), globalScope);

                    // compile time macros definitions will be removed from code in expand macro phase
                    // because only the side effect above is needed for further macro replacements
                    return null;
                }
            }

            // compile time macros: process macro expansion in an expression which calls a macro
            if (LispEnvironment.IsMacro(function, globalScope))
            {
                var macro = LispEnvironment.GetMacro(function, globalScope);
                if (macro is LispMacroCompileTimeExpand)
                {
                    var macroExpand = (LispMacroCompileTimeExpand)macro;
                    return ReplaceFormalArgumentsInExpression(macroExpand.FormalArguments, astAsList, macroExpand.Expression, ref anyMacroReplaced);
                }
            }

            var expandedAst = new List<object>();
            // Expand recursively and handle enumarations (make them flat !)
            foreach (var elem in astAsList)
            {
                var expandResult = ExpandMacros(elem, globalScope);
                // ignore code which is removed in nacri expand phase
                if (expandResult != null)
                {
                    expandedAst.Add(expandResult);                    
                }
            }

            return expandedAst;
        }
#endif

        #endregion

        #region private methods

        /// <summary>
        /// Get information about position in sourcecode for
        /// given item of the ast.
        /// </summary>
        /// <param name="item"></param>
        /// <returns></returns>
        private static LispBreakpointPosition GetPosInfo(object item)
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
                return new LispBreakpointPosition(token.StartPos, token.StopPos, token.LineNo);
            }
            return new LispBreakpointPosition(-1, -1, -1);
        }

        private static IEnumerable<object> RepaceSymbolWithValueInExpression(LispVariant symbol, object symbolValue, IEnumerable<object> expression, ref bool replacedAnything)
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

        private static IEnumerable<object> ReplaceFormalArgumentsInExpression(IEnumerable<object> formalArguments, IList<object> astAsList, IEnumerable<object> expression, ref bool anyMacroReplaced)
        {
            int i = 1;
            foreach (var formalArgument in formalArguments)
            {
                object value;
                if (astAsList[i] is IEnumerable<object>)
                {
                    value = astAsList[i];
                }
                else
                {
                    value = new LispVariant(astAsList[i]);
                }
                expression = RepaceSymbolWithValueInExpression((LispVariant)formalArgument, value, expression, ref anyMacroReplaced);
                i++;
            }
            return expression;
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

        #endregion
    }
}
