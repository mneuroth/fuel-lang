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

#include "Exception.h"
#include "Scope.h"

namespace CsLisp
{
	LispException::LispException(const string & text, LispScope * scope)
		//: base(text)
	{
		Message = text;
		if (scope != 0)
		{
			AddModuleNameAndStackInfos(scope->ModuleName, scope->DumpStackToString());
			AddTokenInfos(scope->CurrentToken);
		}
	}

	LispException::LispException(const string & text, std::shared_ptr<LispToken> token, const string & moduleName, const string & stackInfo)
		//: base(text)
	{
		Message = text;
		AddModuleNameAndStackInfos(moduleName, stackInfo);
		AddTokenInfos(token);
	}

	void LispException::AddModuleNameAndStackInfos(const string & moduleName, const string & stackInfo)
	{
// TODO konstanten korrekt behandeln
		Data["ModuleName"] = std::make_shared<object>(moduleName);
		Data["StackInfo"] = std::make_shared<object>(stackInfo);
	}

	void LispException::AddTokenInfos(std::shared_ptr<LispToken> token)
	{
// TODO konstanten korrekt behandeln
		Data["LineNo"] = std::make_shared<object>(token != null ? (int)token->LineNo : -1);
		Data["StartPos"] = std::make_shared<object>(token != null ? (int)token->StartPos : -1);
		Data["StopPos"] = std::make_shared<object>(token != null ? (int)token->StopPos : -1);
	}
}



