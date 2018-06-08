
#include "Scope.h"

using namespace CsLisp;

const string LispEnvironment::Quote = "quote";
const string LispEnvironment::Quasiquote = "quasiquote";
string LispEnvironment::Macros;

/*private static*/ std::shared_ptr<object> LispEnvironment::QueryItem(std::shared_ptr<object> funcName, std::shared_ptr<LispScope> scope, string key)
{
	if (scope != null &&
		scope->ContainsKey(key) && 
		((*scope)[key])->ToLispScope()->ContainsKey(funcName->ToString()))
	{
		return (*((*scope)[key])->ToLispScope())[funcName->ToString()];
	}
	return null;
}

