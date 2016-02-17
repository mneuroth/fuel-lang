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
                object resolvedElem;
                if ((isSpecialForm != null && (bool)isSpecialForm) || !IsSymbol(elem))
                {
                    resolvedElem = elem;
                }
                else
                {
                    resolvedElem = ResolveInScopes(scope, elem);
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

        /// <summary>
        /// Resolves the items of the ast in the given scope.
        /// </summary>
        /// <param name="scope">The scope.</param>
        /// <param name="elem">The elem.</param>
        /// <returns></returns>
        public static object ResolveInScopes(LispScope scope, object elem)
        {
            object result;

            var name = elem.ToString();
            LispScope foundClosureScope;
            // first try to resolve in local scope
            if (scope != null && scope.ContainsKey(name))
            {
                result = scope[name];
            }
            // then try to resolve in closure chain scope(s)
            else if (IsInClosureChain(name, scope, out foundClosureScope))
            {
                result = foundClosureScope[name];
            }
            // then try to resolve in global scope
            else if (scope != null &&
                     scope.GlobalScope != null &&
                     scope.GlobalScope.ContainsKey(name))
            {
                result = scope.GlobalScope[name];
            }
            // then try to resolve in scope of loaded modules
            else if (scope != null &&
                     LispEnvironment.IsInModules(name, scope.GlobalScope))
            {
                result = LispEnvironment.GetFunctionInModules(name, scope.GlobalScope);
            }
            else
            {
                result = elem;
            }

            return result;
        }

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
                    item = new LispVariant(ResolveInScopes(scope, item));
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
                return EvaluateMacro(astAsList.First(), astAsList, scope.GlobalScope);
            }

            // resolve values via local and global scope
            var astWithResolvedValues = ResolveArgsInScopes(scope, astAsList, false);

            // get first element --> this is the function !
            var function = astWithResolvedValues.First();

            // normal evaluation...
            LispFunctionWrapper functionWrapper = ((LispVariant)function).FunctionValue;

            // trace current function (if tracing is enabled)
            if (scope.GlobalScope.Tracing)
            {
                Console.WriteLine("--> {0}", astAsList.First());
            }

            // evaluate arguments, but allow recursive lists
            var arguments = new object[astWithResolvedValues.Count - 1];
            for (var i = 1; i < astWithResolvedValues.Count; i++)
            {
                var needEvaluation = (astWithResolvedValues[i] is IEnumerable<object>) &&
                                     !functionWrapper.IsSpecialForm;
                arguments[i - 1] = needEvaluation ? EvalAst(astWithResolvedValues[i], scope) : astWithResolvedValues[i];
            }

            // for debugging: update the current line number at the current scope
            var currentToken = ((LispVariant)(astAsList.First())).Token;
            scope.LineNumber = currentToken != null ? currentToken.LineNo : -2;

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

        public static object ExpandMacros(object ast, LispScope globalScope, ref bool anyMacroReplaced)
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
            if (globalScope != null && globalScope.ContainsKey(functionName))
            {
                var fcn = ((LispVariant)globalScope[functionName]).FunctionValue;
// TODO --> braucht man wirklich IsEvalInExpand, wenn macros ausschliesslich expandiert (ersetzt) werden?
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

                    int i = 1;
                    foreach(var formalParameter in macroExpand.FormalParameters)
                    {
                        // replace formal parameters with actual parameters

                        // do not evaluate if a list (s-expression) is given !
                        //if (LispEnvironment.IsExpression(astAsList[i]))
                        //{
                        //    bool replacedAnything = false;
                        //    //expression = RepaceSymbolWithValueInExpression((LispVariant)formalParameter, value, expression, ref replacedAnything);
                        //}
                        //else
                        //{
// TODO working: do not replace anything with values, just replace macro expressions !
                            LispVariant value = new LispVariant(astAsList[i]); 
                            //LispVariant value = EvalAst(astAsList[i], globalScope);
                            //bool replacedAnything = false;
                            expression = RepaceSymbolWithValueInExpression((LispVariant) formalParameter, value, expression, ref anyMacroReplaced);

                            //LispVariant value2 = EvalAst(astAsList[i], globalScope);
                            //expression = RepaceSymbolWithValueInExpression((LispVariant)formalParameter, value2, expression, ref replacedAnything);
                            //}
                        // the following code is not needed anymore, because the 
                        // LispVariant.ToString() was imprived for nicer printing of expressions
                        // 
                        //if (replacedAnything)
                        //{
                        //    // needed for nice dumping of expressions, 
                        //    // ReplaceSymbolWithValueInExpression() transformates parts of the expression into List<object> elements
                        //    // but may be LisVariant.List elements or object[] !
                        //    expression = NormalizeToLists(expression);                            
                        //}
                        i++;
                    }

                    //TODO working gulp: 
// TODO working gulp --> rekursives Makro Expandieren unterstuetzen !!!
// TODO --> im code definierte funktionen sind bei expandierung der Macros noch nicht bekannt !!!

                    return expression;

                    //// create local scope for macro execution
                    //var macroScope = new LispScope("macro_scope", globalScope);
                    //globalScope.PushNextScope(macroScope);

                    //var res = EvalAst(expression, macroScope);
                    //var expressionRet = res.IsList ? res.ListValue.ToArray() : new object[] {res.Value};

                    //globalScope.PopNextScope();

                    //// replace ast with expression !
                    //return expressionRet;
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

        ///// <summary>
        ///// Normalizes to lists. Replaces all LispVariant-Lists with List{object}
        ///// </summary>
        ///// <param name="expression">The expression.</param>
        ///// <returns></returns>
        //private static IEnumerable<object> NormalizeToLists(IEnumerable<object> expression)
        //{
        //    LispVariant symbol = new LispVariant("_");
        //    LispVariant replace = new LispVariant("_");
        //    bool ignore = false;
        //    return RepaceSymbolWithValueInExpression(symbol, replace, expression, ref ignore);
        //}

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
