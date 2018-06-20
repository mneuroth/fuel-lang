// QtLisp.cpp : Defines the entry point for the console application.
//

// gcc -std=c++11 -c QtLisp.cpp

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

//#include "stdafx.h"

#include <iostream>
#include <memory>

#include "Token.h"
#include "Tokenizer.h"
#include "Variant.h"
#include "Scope.h"
#include "Parser.h"
#include "Interpreter.h"
#include "Lisp.h"

using namespace CsLisp;

/*
static object CreateFunction(Func<object[], LispScope, LispVariant> func, string signature = null, string documentation = null, bool isBuiltin = true, bool isSpecialForm = false, bool isEvalInExpand = false, string moduleName = Builtin)
{
	return new LispVariant(new LispFunctionWrapper(func, signature, documentation, isBuiltin, isSpecialForm, isEvalInExpand, moduleName));
}
*/
/*
static std::shared_ptr<object> CreateFunction(FuncX func)
{
	LispFunctionWrapper wrapper;
	wrapper.Function = func;
	return std::make_shared<object>(LispVariant(LispType::_Function, std::make_shared<object>(wrapper)));
}

static std::shared_ptr<LispVariant> ArithmetricOperation(std::vector<std::shared_ptr<object>> args, std::function<std::shared_ptr<LispVariant>(std::shared_ptr<LispVariant>, std::shared_ptr<LispVariant>)> op)
{
	std::shared_ptr<LispVariant> result(null);
	for(var elem : args)
	{
		if (result == null)
		{
			result = std::make_shared<LispVariant>(LispVariant(elem));
		}
		else
		{
			result = op(result, std::make_shared<LispVariant>(LispVariant(elem)));
		}
	}
	return result;
}

std::shared_ptr<LispVariant> Addition(std::vector<std::shared_ptr<object>> args, LispScope & scope)
{
	return ArithmetricOperation(args, [](std::shared_ptr<LispVariant> l, std::shared_ptr<LispVariant> r) -> std::shared_ptr<LispVariant> { return std::make_shared<LispVariant>(*l + *r); });
}
*/

int main()
{
	//string code = "(+ 1 2 3)";
	//IEnumerable<std::shared_ptr<LispToken>> tokens = LispTokenizer::Tokenize(code);

	////for each (LispToken elem in tokens)
	////{
	////	std::cout << "Elem: " << elem.ToString() << std::endl;
	////}

	//for_each(tokens.begin(), tokens.end(), [](std::shared_ptr<LispToken> elem) { std::cout << "Elem: " << elem->ToString() << std::endl; });

	//LispVariant aVariant(LispType::_Nil);
	//std::cout << "Variant: " << aVariant.ToString() << std::endl;
	//LispVariant aVariant2(std::make_shared<object>(42));
	//std::cout << "Variant: " << aVariant2.ToString() << std::endl;
	//LispVariant aVariant3(std::make_shared<object>(43));
	//std::cout << "Variant: " << aVariant3.ToString() << std::endl;

	//std::shared_ptr<object> v3 = std::make_shared<object>(aVariant3);
	//int result = aVariant2.CompareTo(v3);

	string script = "(+ 1 2 3)";
	std::shared_ptr<IEnumerable<std::shared_ptr<object>>> ast = LispParser::Parse(script);

	std::cout << "AST:" << std::endl;
	for (auto e : *ast)
	{
		std::cout << "--> " << e->ToString() << std::endl;
	}

	//std::shared_ptr<LispScope> globalScope = std::make_shared<LispScope>();
	//(*globalScope)["+"] = CreateFunction(&Addition);

	// for enable_shared_from_this the object to be shared has to be already constructed and assigned to a smart_pointer 
	//globalScope->PrivateInitForCpp();
	//std::shared_ptr<object> astAsObj = std::make_shared<object>(*ast);
	//std::shared_ptr<LispVariant> result = LispInterpreter::EvalAst(astAsObj, *globalScope);

	bool trace = false;
	bool lengthyErrorOutput = false;
	string fileName = "test.lisp";
	std::shared_ptr<LispVariant> result = Lisp::SaveEval(script, /*moduleName:*/ fileName, /*verboseErrorOutput:*/ lengthyErrorOutput, /*tracing:*/ trace);

	std::cout << "result = " << result->ToString() << std::endl;

    return 0;
}

