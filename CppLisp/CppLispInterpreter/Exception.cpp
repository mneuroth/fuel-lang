
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



