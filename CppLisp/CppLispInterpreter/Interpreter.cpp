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

#include "Interpreter.h"

namespace CppLisp
{
	std::shared_ptr<IEnumerable<std::shared_ptr<object>>> LispInterpreter::ResolveArgsInScopes(std::shared_ptr<LispScope> scope, std::shared_ptr<IEnumerable<std::shared_ptr<object>>> astAsList, bool compile)
	{
		std::shared_ptr<IEnumerable<std::shared_ptr<object>>> astWithResolvedValues = std::make_shared<IEnumerable<std::shared_ptr<object>>>(astAsList->Count());
		std::shared_ptr<bool> isSpecialForm = null;
		int i = 0;
		for (var elem : *astAsList)
		{
			std::shared_ptr<object> resolvedElem;
			if ((isSpecialForm != null && (bool)*isSpecialForm) || !IsSymbol(elem))
			{
				resolvedElem = elem;
			}
			else
			{
				resolvedElem = scope->ResolveInScopes(elem);
			}
			(*astWithResolvedValues)[i++] = resolvedElem;

			if (isSpecialForm == null)
			{
				bool valIsSpecialForm = false;
				std::shared_ptr<object> first = null;
				try
				{
					first = astWithResolvedValues->First();
					const LispFunctionWrapper & firstElem = first->ToLispVariantRef().FunctionValue();
					valIsSpecialForm = firstElem.IsSpecialForm();
				}
				catch (LispException exc)
				{
					if (!compile)
					{
						throw LispException("Function \"" + first->ToString() + "\" not found", scope.get());
					}
				}
				isSpecialForm = std::make_shared<bool>(valIsSpecialForm);
			}

		}
		return astWithResolvedValues;
	}

	std::shared_ptr<LispVariant> LispInterpreter::EvalAst(std::shared_ptr<object> ast, std::shared_ptr<LispScope> scope)
	{
		if (ast.get() == null)
		{
			return null;
		}

		std::shared_ptr<IEnumerable<std::shared_ptr<object>>> astAsList = std::make_shared<IEnumerable<std::shared_ptr<object>>>();

		if (ast->IsLispVariant())
		{
			var item = ast->ToLispVariantRef();
			// evaluate the value for the symbol
			if (item.IsSymbol())
			{
				return std::make_shared<LispVariant>(scope->ResolveInScopes(std::make_shared<object>(item)));
			}
			else if (item.IsList() && !item.IsNil())
			{
				*astAsList = item.ListValueRef()/*->ToList()*/;
			}
			else
			{
				return ast->ToLispVariant();
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
				var expression = ReplaceFormalArgumentsInExpression(runtimeMacro->FormalArguments, astAsList, runtimeMacro->Expression, scope, /*ref*/ anyMacroReplaced);

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
		const LispFunctionWrapper & functionWrapper = function->ToLispVariantRef().FunctionValue();

		// trace current function (if tracing is enabled)
		if (scope->GlobalScope->Tracing)
		{
			scope->GlobalScope->Output->WriteLine("--> {0}", astAsList->First()->ToString());
		}

		// evaluate arguments, but allow recursive lists
		std::vector<std::shared_ptr<object>> arguments(astWithResolvedValues->Count() - 1); // = new object[astWithResolvedValues->Count() - 1];
		const std::vector<std::shared_ptr<object>> & arrAstWithResolvedValues = astWithResolvedValues->ToArray();
		for (size_t i = 1; i < astWithResolvedValues->Count(); i++)
		{
			var needEvaluation = (arrAstWithResolvedValues[i]->IsList() /*is IEnumerable<object>*/) && !functionWrapper.IsSpecialForm();
			arguments[i - 1] = needEvaluation ? std::make_shared<object>(*EvalAst(arrAstWithResolvedValues[i], scope)) : arrAstWithResolvedValues[i];
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

	std::shared_ptr<object> LispInterpreter::ExpandMacros(std::shared_ptr<object> ast, std::shared_ptr<LispScope> globalScope)
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

	std::shared_ptr<object> LispInterpreter::ExpandMacros(std::shared_ptr<object> ast, std::shared_ptr<LispScope> globalScope, /*ref*/ bool & anyMacroReplaced)
	{
		if (ast == null || ast->IsLispVariant())
		{
			return ast;
		}

		const IEnumerable<std::shared_ptr<object>> & astAsList = ast->ToEnumerableOfObjectRef();
		if (astAsList.size() == 0)
		{
			return ast;
		}

		// compile time macro: process define-macro statements ==> call special form, this will add macro to global scope as side effect
		var function = astAsList.front();
		var functionName = function->ToString();
		if (globalScope != null && globalScope->ContainsKey(functionName))
		{
			var fcn = (*globalScope)[functionName]->ToLispVariantRef().FunctionValue();
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
				var astWithReplacedArguments = std::make_shared<object>(*ReplaceFormalArgumentsInExpression(macroExpand->FormalArguments, std::make_shared<IEnumerable<std::shared_ptr<object>>>(astAsList), macroExpand->Expression, globalScope, /*ref*/ anyMacroReplaced));
				var processedAst = EvalAst(astWithReplacedArguments, globalScope);
				return std::make_shared<object>(*(processedAst->ListValue())); //  std::make_shared<object>(*processedAst);
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

	LispBreakpointPosition LispInterpreter::GetPosInfo(std::shared_ptr<object> item)
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

	std::shared_ptr<IEnumerable<std::shared_ptr<object>>> LispInterpreter::ReplaceSymbolWithValueInExpression(const LispVariant & symbol, std::shared_ptr<object> symbolValue, std::shared_ptr<IEnumerable<std::shared_ptr<object>>> expression, bool macroArgsReplace, /*ref*/ bool & replacedAnything)
	{
		var ret = std::make_shared<IEnumerable<std::shared_ptr<object>>>();
		for (var elem : *expression)
		{
			// is the current element the symbol which should be replaced? --> Yes
			if (symbol.SymbolCompare(elem))
			{
				if (symbolValue->IsIEnumerableOfObject() && macroArgsReplace)
				{
					(*ret).AddRange(symbolValue->ToEnumerableOfObjectRef());
				}
				if (symbolValue->IsList() && macroArgsReplace)
				{
					(*ret).AddRange(*(symbolValue->ToList()));
				}
				else
				{
					(*ret).Add(symbolValue);
				}
				replacedAnything = true;
			}
			// is it an expression? --> recursive call
			else if (LispEnvironment::IsExpression(elem))
			{
				std::shared_ptr<IEnumerable<std::shared_ptr<object>>> temp = ReplaceSymbolWithValueInExpression(symbol, symbolValue, LispEnvironment::GetExpression(elem)/*.ToArray()*/, macroArgsReplace, /*ref*/ replacedAnything);
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

	std::shared_ptr<IEnumerable<std::shared_ptr<object>>> LispInterpreter::ReplaceFormalArgumentsInExpression(std::shared_ptr<IEnumerable<std::shared_ptr<object>>> formalArguments, std::shared_ptr<IEnumerable<std::shared_ptr<object>>> astAsList, std::shared_ptr<IEnumerable<std::shared_ptr<object>>> expression, std::shared_ptr<LispScope> scope, /*ref*/ bool & anyMacroReplaced)
	{
		// replace (quoted-macro-args) --> '(<real_args>)
		int i = 1;
		bool replaced = false;
		IEnumerable<std::shared_ptr<object>> realArguments = astAsList->Skip(1)/*.ToList()*/;
		IEnumerable<std::shared_ptr<object>> lst;
		lst.Add(std::make_shared<object>(LispVariant(LispType::_Symbol, std::make_shared<object>(LispEnvironment::Quote))));
		lst.Add(std::make_shared<object>(LispVariant(std::make_shared<object>(realArguments))));
		/*List<object>*/std::shared_ptr<object> quotedRealArguments = std::make_shared<object>(lst); //  new List<object>() { new LispVariant(LispType.Symbol, LispEnvironment.Quote), realArguments };
		expression = ReplaceSymbolWithValueInExpression(LispVariant(LispType::_Symbol, std::make_shared<object>("quoted-macro-args")), quotedRealArguments, expression, true, /*ref*/ replaced);

		for (var formalArgument : *formalArguments)
		{
			std::shared_ptr<object> value;
			auto elem = (*astAsList)[i];
			if (elem->IsIEnumerableOfObject() || elem->IsList())
			{
				value = /*elem;*/ ExpandMacros((*astAsList)[i], scope, /*ref*/ anyMacroReplaced);
			}
			else
			{
				value = std::make_shared<object>(LispVariant((*astAsList)[i]));
			}
			expression = ReplaceSymbolWithValueInExpression(/*(LispVariant)*/formalArgument->ToLispVariantRef(), value, expression, false, /*ref*/ anyMacroReplaced);
			i++;
		}
		return expression;
	}

	bool LispInterpreter::IsSymbol(std::shared_ptr<object> elem)
	{
		bool isSymbol = false;
		if (elem->IsLispVariant())
		{
			isSymbol = elem->ToLispVariantRef().IsSymbol();
		}
		return isSymbol;
	}
}
