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
        /// Get information about position in sourcecode for
        /// given item of the ast.
        /// </summary>
        /// <param name="item"></param>
        /// <returns></returns>
        public static Tuple<int, int> GetPosInfo(object item)
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
            return LispEnvironment.GetPosInfo(token);
        }

        /// <summary>
        /// Get string representation for position in sourcecode for
        /// given item of the ast.
        /// </summary>
        /// <param name="item"></param>
        /// <returns></returns>
        public static string GetPosInfoString(object item)
        {
            string info = "pos=";
            var pos = GetPosInfo(item);
            info += pos.Item1;
            info += " line=";
            info += pos.Item2;
            return info;
        }

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
                if ((isSpecialForm != null && (bool)isSpecialForm) || !IsSymbol(elem))
                {
                    astWithResolvedValues.Add(elem);
                }
                else
                {
                    var name = elem.ToString();
                    LispScope foundClosureScope;
                    // first try to resolve in local scope
                    if (scope != null && scope.ContainsKey(name))
                    {
                        astWithResolvedValues.Add(scope[name]);
                    }
                        // then try to resolve in closure chain scope(s)
                    else if (IsInClosureChain(name, scope, out foundClosureScope))
                    {
                        astWithResolvedValues.Add(foundClosureScope[name]);
                    }
                        // then try to resolve in global scope
                    else if (scope != null &&
                                scope.GlobalScope != null &&
                                scope.GlobalScope.ContainsKey(name))
                    {
                        astWithResolvedValues.Add(scope.GlobalScope[name]);
                    }
                    else
                    {
                        astWithResolvedValues.Add(elem);
                    }
                }

                if (isSpecialForm == null)
                {
                    var firstElem = new LispFunctionWrapper();
                    object first = null;
                    try
                    {
                        first = astWithResolvedValues.First();
                        firstElem = ((LispVariant)first).FunctionValue;
                    }
                    catch (InvalidCastException)
                    {
                        if (!compile)
                        {
                            throw new LispException("Method \"" + first + "\" not found (" + GetPosInfoString(first) + ")!");
                        }
                    }
                    isSpecialForm = firstElem.IsSpecialForm;
                }
            }
            return astWithResolvedValues;
        }

        public static LispVariant EvalAst(object ast, LispScope scope)
        {
            if (ast == null || ast is LispVariant)     // means item is not an IEnumerable
            {
                return (LispVariant)ast;
            }

            var astAsList = ((IEnumerable<object>)ast).ToList();

            // is this function a macro ==> evaluate the macro... and return
            if (LispEnvironment.IsMacro(astAsList.First(), scope.GlobalScope))
            {
                return EvaluateMacro(astAsList.First(), astAsList, scope.GlobalScope);
            }

            // resolve values via local and global scope
            var astWithResolvedValues = ResolveArgsInScopes(scope, astAsList, false);

            // get first element --> this is the function !
            var function = astWithResolvedValues.First();

            // normal evaluation...
            LispFunctionWrapper functionWrapper = ((LispVariant)function).FunctionValue;

            // evaluate arguments, but allow recursive lists
            var arguments = new object[astWithResolvedValues.Count - 1];
            for (var i = 1; i < astWithResolvedValues.Count; i++)
            {
                var needEvaluation = (astWithResolvedValues[i] is IEnumerable<object>) &&
                                     !functionWrapper.IsSpecialForm;
                arguments[i - 1] = needEvaluation ? EvalAst(astWithResolvedValues[i], scope) : astWithResolvedValues[i];
            }

            // debugger processing
            if (scope.GlobalScope.Debugger != null)
            {
                var debugger = scope.GlobalScope.Debugger;
                if (debugger.NeedsBreak(scope, GetPosInfo(astAsList[0])))
                {
                    debugger.InteractiveLoop(scope, astAsList);
                }
            }

            // call the function with the arguments
            return functionWrapper.Function(arguments, scope);
        }

        public static object ExpandMacros(object ast, LispScope globalScope)
        {
            if (ast == null || ast is LispVariant)
            {
                return ast;
            }

            // process define-macro statements ==> add macro to global scope
            var astAsList = ((IEnumerable<object>)ast).ToList();
            if (astAsList.Count == 0)
            {
                return ast;
            }
            var functionName = astAsList.First().ToString();
            if (globalScope != null && globalScope.ContainsKey(functionName))
            {
                var fcn = ((LispVariant)globalScope[functionName]).FunctionValue;
                if (fcn.IsEvalInExpand)
                {
                    var args = new List<object>(astAsList);
                    args.RemoveAt(0);

                    var macroEvalResult = fcn.Function(args.ToArray(), globalScope);
                    return macroEvalResult != null ? macroEvalResult : ast;
                }
            }

            // process macro expansion
            if (LispEnvironment.IsMacro(astAsList.First(), globalScope))
            {
                var macro = LispEnvironment.GetMacro(astAsList.First(), globalScope);
                if (macro is LispMacroExpand)
                {
                    var macroExpand = (LispMacroExpand)macro;
                    var expression = macroExpand.Expression.ToArray();

                    int i = 1;
                    foreach(var formalParameter in macroExpand.FormalParameters)
                    {
                        // replace formal parameters with actual parameters
                        LispVariant value = EvalAst(astAsList[i], globalScope);
                        expression = RepaceSymbolWithValueInExpression((LispVariant)formalParameter, value, expression);
                        i++;
                    }

                    expression = EvalAst(expression, globalScope).ListValue.ToArray();
                    // replace ast with expression !
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

        private static object[] RepaceSymbolWithValueInExpression(LispVariant symbol, LispVariant symbolValue, object[] expression)
        {
            for (int i = 0; i < expression.Length; i++)
            {
                if (symbol.Equals(expression[i]))
                {
                    expression[i] = symbolValue;
                }
                else if (LispEnvironment.IsExpression(expression[i]))
                {
                    expression[i] = new LispVariant(RepaceSymbolWithValueInExpression(symbol, symbolValue, LispEnvironment.GetExpression(expression[i]).ToArray()));
                }
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

        private static LispVariant EvaluateMacro(object function, IEnumerable<object> rawExpression, LispScope globalScope)
        {
            var macroFcn = LispEnvironment.GetMacro(function, globalScope);

            var argsTmp = new List<object>(rawExpression);
            argsTmp.RemoveAt(0);
            var args = argsTmp.ToArray();

            var evalMacro = new List<object>();
            evalMacro.Add(new LispVariant(LispType.Symbol, LispEnvironment.Apply));
            evalMacro.Add(macroFcn);

            foreach (var arg in args)
            {
                evalMacro.Add(EvalAst(arg, globalScope));
            }

            // evaluate macroFcn with given args
            // (apply (lambda ...) arg1 arg2 ...)
            var result = EvalAst(evalMacro, globalScope);
            return result;
        }

        private static bool IsInClosureChain(string name, LispScope scope, out LispScope closureScopeFound)
        {
            closureScopeFound = null;
            if (scope != null && scope.ClosureChain != null)
            {
                if (scope.ClosureChain.ContainsKey(name))
                {
                    closureScopeFound = scope.ClosureChain;
                    return true;
                }
                return IsInClosureChain(name, scope.ClosureChain, out closureScopeFound);
            }
            return false;
        }

        #endregion
    }
}
