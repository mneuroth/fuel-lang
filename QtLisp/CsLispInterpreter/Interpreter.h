#ifndef _LISP_INTERPRETER_H
#define _LISP_INTERPRETER_H

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

//using System;
//using System.Collections.Generic;
//using System.Linq;

#include "csobject.h"
#include "cstypes.h"
#include "Scope.h"

namespace CsLisp
{
	class LispBreakpointPosition : public Tuple3<size_t, size_t, size_t>
	{
	public:
		/*public*/ LispBreakpointPosition(size_t start, size_t stop, size_t lineNumber)
			: Tuple3<size_t, size_t, size_t>(start, stop, lineNumber)
		{
		}
	};

    /// <summary>
    /// The FUEL lisp interpreter.
    /// </summary>
    /*public*/ class LispInterpreter
    {
	public:
        //#region public methods

        /// <summary>
        /// Resolves the items of the ast in the given scope.
        /// </summary>
        /// <param name="scope"></param>
        /// <param name="astAsList"></param>
        /// <param name="compile"></param>
        /// <returns></returns>
        /*public*/ static std::shared_ptr<IEnumerable<std::shared_ptr<object>>> ResolveArgsInScopes(std::shared_ptr<LispScope> scope, std::shared_ptr<IEnumerable<std::shared_ptr<object>>> astAsList, bool compile)
        {
            std::shared_ptr<IEnumerable<std::shared_ptr<object>>> astWithResolvedValues = std::make_shared<IEnumerable<std::shared_ptr<object>>>();
			std::shared_ptr<bool> isSpecialFormX = null;
            for (var elem : *astAsList)
            {
				std::shared_ptr<object> resolvedElem;
                if ((isSpecialFormX != null && (bool)*isSpecialFormX) || !IsSymbol(elem))
                {
                    resolvedElem = elem;
                }
                else
                {
                    resolvedElem = scope->ResolveInScopes(elem);
                }
                astWithResolvedValues->Add(resolvedElem);

                if (isSpecialFormX == null)
                {
					LispFunctionWrapper firstElem;
					std::shared_ptr<object> first = null;
                    try
                    {
                        first = astWithResolvedValues->First();
                        firstElem = first->ToLispVariant()->FunctionValue();
                    }
                    catch (LispException exc)
                    {
                        if (!compile)
                        {
                            throw LispException("Function \"" + first->ToString() + "\" not found"/*, scope*/);
                        }
                    }
                    isSpecialFormX = std::make_shared<bool>(firstElem.IsSpecialForm());
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
        /*public*/ static std::shared_ptr<LispVariant> EvalAst(std::shared_ptr<object> ast, std::shared_ptr<LispScope> scope)
        {
            if (ast.get() == null)
            {
                return null;
            }

            std::shared_ptr<IEnumerable<std::shared_ptr<object>>> astAsList = std::make_shared<IEnumerable<std::shared_ptr<object>>>();

            if (ast->IsLispVariant())
            {
                var item = ast->ToLispVariant();
                // evaluate the value for the symbol
                if (item->IsSymbol())
                {
                    item = std::make_shared<LispVariant>(scope->ResolveInScopes(std::make_shared<object>(*item)));
                }
                if (item->IsList() && !item->IsNil())
                {
                    *astAsList = *(item->ListValue()/*->ToList()*/);
                }
                else
                {
                    return item;
                }
            }
            else
            {
                astAsList = ast->ToList();                
            }

            if (astAsList->Count() == 0)
            {
                return std::make_shared<LispVariant>(LispVariant(LispType::_Nil));
            }

            // is this function a macro ==> process the macro and return
            if (LispEnvironment::IsMacro(astAsList->First(), scope->GlobalScope))
            {
                // check the macro modus: evaluate or expand or lambda
                var macro = LispEnvironment::GetMacro(astAsList->First(), scope->GlobalScope);

                // evaluate macro at run time:
                if (macro->IsLispMacroRuntimeEvaluate())
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
                    var runtimeMacro = macro->ToLispMacroRuntimeEvaluate();
                    var expression = ReplaceFormalArgumentsInExpression(runtimeMacro->FormalArguments, astAsList, runtimeMacro->Expression, /*ref*/ anyMacroReplaced);

                    return EvalAst(std::make_shared<object>(*expression), scope);
                }
                
                // expand macro at compile time: --> nothing to do at run time !
                // code not needed, because code for compile time macros will be removed in ExpandMacro phase
                //if (macro is LispMacroCompileTimeExpand)
                //{
                //    return new LispVariant();
                //}

                throw LispException("Unexpected macro modus!");
            }

            // for debugging: update the current line number at the current scope
            var currentToken = ((LispVariant)(astAsList->First())).Token;
            scope->CurrentToken = currentToken != null ? currentToken : scope->CurrentToken;

            // resolve values via local and global scope
            var astWithResolvedValues = ResolveArgsInScopes(scope, astAsList, false);

            // get first element --> this is the function !
            var function = astWithResolvedValues->First();

            // normal evaluation...
            LispFunctionWrapper functionWrapper = function->ToLispVariant()->FunctionValue();

            // trace current function (if tracing is enabled)
            if (scope->GlobalScope->Tracing)
            {
                scope->GlobalScope->Output.WriteLine("--> {0}", astAsList->First()->ToString());
            }
			
			// evaluate arguments, but allow recursive lists
			std::vector<std::shared_ptr<object>> arguments(astWithResolvedValues->Count()-1); // = new object[astWithResolvedValues->Count() - 1];
			for (size_t i = 1; i < astWithResolvedValues->Count(); i++)
            {
                var needEvaluation = (astWithResolvedValues->ToArray()[i]->IsList() /*is IEnumerable<object>*/) &&
                                     !functionWrapper.IsSpecialForm();
				arguments[i - 1] = needEvaluation ? std::make_shared<object>(*EvalAst(astWithResolvedValues->ToArray()[i], scope)) : astWithResolvedValues->ToArray()[i];
            }

            // debugger processing
            var debugger = scope->GlobalScope->Debugger;
            if (debugger != null && debugger->NeedsBreak(scope, GetPosInfo(astAsList->First()/*[0]*/)))
            {
                debugger->InteractiveLoop(scope, astAsList);
            }

			// call the function with the arguments
            return functionWrapper.Function(arguments, scope);
		}

#ifdef ENABLE_COMPILE_TIME_MACROS 

		/*public*/ static std::shared_ptr<object> ExpandMacros(std::shared_ptr<object> ast, std::shared_ptr<LispScope> globalScope)
        {
			std::shared_ptr<object> result = ast;
            bool anyMacroReplaced;
            do
            {
                anyMacroReplaced = false;
                result = ExpandMacros(result, globalScope, /*ref*/ anyMacroReplaced);
            } while (anyMacroReplaced);
            return result;
        }

        /*private*/ static std::shared_ptr<object> ExpandMacros(std::shared_ptr<object> ast, std::shared_ptr<LispScope> globalScope, /*ref*/ bool & anyMacroReplaced)
        {
            if (ast == null || ast->IsLispVariant())
            {
                return ast;
            }

            var astAsList = ast->ToEnumerableOfObject();
            if (astAsList.size() == 0)
            {
                return ast;
            }

            // compile time macro: process define-macro statements ==> call special form, this will add macro to global scope as side effect
            var function = astAsList.front();
            var functionName = function->ToString();
            if (globalScope != null && globalScope->ContainsKey(functionName))
            {
                var fcn = (*globalScope)[functionName]->ToLispVariant()->FunctionValue();
                if (fcn.IsEvalInExpand())
                {
                    var args = std::make_shared<IEnumerable<std::shared_ptr<object>>>(astAsList);
                    args->RemoveAt(0);

                    // process compile time macro definition 
                    //   --> side effect: add macro definition to internal macro scope
                    fcn.Function(args->ToArray(), globalScope);

                    // compile time macros definitions will be removed from code in expand macro phase
                    // because only the side effect above is needed for further macro replacements
                    return null;
                }
            }

            // compile time macros: process macro expansion in an expression which calls a macro
            if (LispEnvironment::IsMacro(function, globalScope))
            {
                var macro = LispEnvironment::GetMacro(function, globalScope);
                if (macro->IsLispMacroCompileTimeExpand())
                {
                    var macroExpand = macro->ToLispMacroCompileTimeExpand();
                    return std::make_shared<object>(*ReplaceFormalArgumentsInExpression(macroExpand->FormalArguments, std::make_shared<IEnumerable<std::shared_ptr<object>>>(astAsList), macroExpand->Expression, /*ref*/ anyMacroReplaced));
                }
            }

            var expandedAst = std::make_shared<IEnumerable<std::shared_ptr<object>>>();
            // Expand recursively and handle enumarations (make them flat !)
            for (var elem : astAsList)
            {
                var expandResult = ExpandMacros(elem, globalScope);
                // ignore code which is removed in nacri expand phase
                if (expandResult != null)
                {
                    expandedAst->Add(expandResult);                    
                }
            }

            return std::make_shared<object>(*expandedAst);
		}
#endif

        //#endregion

		private:

        //#region private methods

        /// <summary>
        /// Get information about position in sourcecode for
        /// given item of the ast.
        /// </summary>
        /// <param name="item"></param>
        /// <returns></returns>
        /*private*/ static LispBreakpointPosition GetPosInfo(std::shared_ptr<object> item)
        {
			std::shared_ptr<LispToken> token;
            if (item->IsLispToken())
            {
				token = item->ToLispToken();
            }
            else
            {
                token = ((LispVariant)item).Token;
            }
            if (token != null)
            {
                return /*new*/ LispBreakpointPosition(token->StartPos, token->StopPos, token->LineNo);
            }
            return /*new*/ LispBreakpointPosition(-1, -1, -1);
        }

        /*private*/ static std::shared_ptr<IEnumerable<std::shared_ptr<object>>> RepaceSymbolWithValueInExpression(std::shared_ptr<LispVariant> symbol, std::shared_ptr<object> symbolValue, std::shared_ptr<IEnumerable<std::shared_ptr<object>>> expression, /*ref*/ bool & replacedAnything)
        {
            var ret = std::make_shared<IEnumerable<std::shared_ptr<object>>>();
            for(var elem : *expression)
            {
                // is the current element the symbol which should be replaced? --> Yes
                if (symbol->SymbolCompare(elem))
                {
                    (*ret).Add(symbolValue);
                    replacedAnything = true;
                }
                // is it an expression? --> recursive call
                else if (LispEnvironment::IsExpression(elem))
                {
					std::shared_ptr<IEnumerable<std::shared_ptr<object>>> temp = RepaceSymbolWithValueInExpression(symbol, symbolValue, LispEnvironment::GetExpression(elem)/*.ToArray()*/, /*ref*/ replacedAnything);
                    (*ret).Add(std::make_shared<object>(*temp));
                }
                // current element is not the symbol which should by replaced !
                else
                {
                    ret->Add(elem);
                }
            }
            return ret;
        }

        /*private*/ static std::shared_ptr<IEnumerable<std::shared_ptr<object>>> ReplaceFormalArgumentsInExpression(std::shared_ptr<IEnumerable<std::shared_ptr<object>>> formalArguments, std::shared_ptr<IEnumerable<std::shared_ptr<object>>> astAsList, std::shared_ptr<IEnumerable<std::shared_ptr<object>>> expression, /*ref*/ bool & anyMacroReplaced)
        {
            int i = 1;
            for (var formalArgument : *formalArguments)
            {
				std::shared_ptr<object> value;
				auto elem = astAsList->ToArray()[i];
                if (elem->IsIEnumerableOfObject() || elem->IsList())
                {
                    value = elem;
                }
                else
                {
                    value = std::make_shared<object>(LispVariant(astAsList->ToArray()[i]));
                }
                expression = RepaceSymbolWithValueInExpression(/*(LispVariant)*/formalArgument->ToLispVariant(), value, expression, /*ref*/ anyMacroReplaced);
                i++;
            }
            return expression;
        }

        /*private*/ static bool IsSymbol(std::shared_ptr<object> elem)
        {
            bool isSymbol = false;
            if (elem->IsLispVariant())
            {
                var variant = elem->ToLispVariant();
                isSymbol = variant->IsSymbol();
            }
            return isSymbol;
        }

        //#endregion
	};
}

#endif
