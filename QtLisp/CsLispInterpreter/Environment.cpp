
#include "Scope.h"
#include "Interpreter.h"

#include <map>

using namespace CsLisp;

const string If = "if";
const string While = "while";
const string Begin = "begin";
const string Do = "do";
const string Or = "or";
const string And = "and";
const string Fn = "fn";
const string Def = "def";
const string Gdef = "gdef";
const string Setf = "setf";
const string Defn = "defn";
const string Gdefn = "gdefn";
const string MapFcn = "map";
const string ReduceFcn = "reduce";
const string DefineMacro = "define-macro";      // == define-macro-eval
const string DefineMacroEval = "define-macro-eval";
#if ENABLE_COMPILE_TIME_MACROS 
const string DefineMacroExpand = "define-macro-expand";
#endif
const string Lambda = "lambda";
const string Tracebuffer = LispEnvironment::MetaTag + "tracebuffer" + LispEnvironment::MetaTag;
const string Traceon = LispEnvironment::MetaTag + "traceon" + LispEnvironment::MetaTag;
const string ArgsMeta = LispEnvironment::MetaTag + "args" + LispEnvironment::MetaTag;
const string AdditionalArgs = "_additionalArgs";

const string ArgsCount = "argscount";
const string Args = "args";

const string LispEnvironment::Macros = LispEnvironment::MetaTag + "macros" + LispEnvironment::MetaTag;
const string LispEnvironment::Modules = LispEnvironment::MetaTag + "modules" + LispEnvironment::MetaTag;

const string LispEnvironment::Apply = "apply";
const string LispEnvironment::Eval = "eval";
const string LispEnvironment::EvalStr = "evalstr";
const string LispEnvironment::Quote = "quote";
const string LispEnvironment::Quasiquote = "quasiquote";

const string LispEnvironment::Sym = "sym";
const string LispEnvironment::Str = "str";


/*private*/ const string LispEnvironment::MetaTag = "###";
/*private*/ const string Builtin = "<builtin>";

/*private*/ const string MainScope = "<main>";

bool LispEnvironment::FindFunctionInModules(const string & funcName, std::shared_ptr<LispScope> scope, std::shared_ptr<object> foundValue)
{
	foundValue = null;

	std::shared_ptr<object> importedModules = (*(scope->GlobalScope))[LispEnvironment::Modules];
	for (/*KeyValuePair*/std::pair<string, std::shared_ptr<object>> kv : *(importedModules->ToLispScope()))
	{
		var module = /*(LispScope)*/kv.second->ToLispScope();
		if (module->ContainsKey(funcName))
		{
			foundValue = (*module)[funcName];
			return true;
		}
	}
	return false;
}

bool LispEnvironment::IsInModules(const string & funcName, std::shared_ptr<LispScope> scope)
{
	std::shared_ptr<object> value;
	return FindFunctionInModules(funcName, scope, value);
}

std::shared_ptr<object> LispEnvironment::GetFunctionInModules(const string & funcName, std::shared_ptr<LispScope> scope)
{
	std::shared_ptr<object> result;
	FindFunctionInModules(funcName, scope, result);
	return result;
}

bool LispEnvironment::IsMacro(std::shared_ptr<object> funcName, std::shared_ptr<LispScope> scope)
{
	return false;
	//return ExistsItem(funcName, scope, LispEnvironment::Macros);
}


static std::shared_ptr<object> CreateFunction(FuncX func, const string & signature = /*null*/"", const string & documentation = /*null*/"", bool isBuiltin = true, bool isSpecialForm = false, bool isEvalInExpand = false, const string & moduleName = "Builtin")
{
	LispFunctionWrapper wrapper;
	wrapper.Function = func;
	wrapper.Signature = signature;
	wrapper.Documentation = documentation;
	wrapper.SetSpecialForm(isSpecialForm);
	return std::make_shared<object>(LispVariant(LispType::_Function, std::make_shared<object>(wrapper)));
}

static string GetStringRepresentation(std::vector<std::shared_ptr<object>> args, LispScope & scope, string separator = " ")
{
	var text = string::Empty;
	for (var item : args)
	{
		if (text.Length() > 0)
		{
			text += separator;
		}
		text += item->ToString();
	}
	if (scope.ContainsKey(Traceon) && (bool)scope[Traceon])
	{
		var buffer = /*(StringBuilder)*/scope[Tracebuffer];
		string temp = buffer->ToString();
		temp += text;
		scope[Tracebuffer] = std::make_shared<object>(temp);
		//old: buffer.Append(text);
	}
	return text;
}

static std::shared_ptr<object> QueryItem(std::shared_ptr<object> funcName, LispScope * scope, const string & key)
{
	if (scope != null &&
		scope->ContainsKey(key) &&
		((*scope)[key])->ToLispScope()->ContainsKey(funcName->ToString()))
	{
		return (*((*scope)[key]->ToLispScope()))[funcName->ToString()];
	}
	return null;
}

static bool ExistsItem(std::shared_ptr<object> funcName, LispScope * scope, const string & key)
{
	if (scope != null &&
		scope->ContainsKey(key))
	{
		return (*scope)[key]->ToLispScope()->ContainsKey(funcName->ToString());
	}
	return false;
}

static std::shared_ptr<LispVariant> Print(std::vector<std::shared_ptr<object>> args, LispScope & scope)
{
	var text = GetStringRepresentation(args, scope);
	scope.GlobalScope->Output.Write(text);
	return std::make_shared<LispVariant>(std::make_shared<object>(text));
}

static std::shared_ptr<LispVariant> PrintLn(std::vector<std::shared_ptr<object>> args, LispScope & scope)
{
	var text = GetStringRepresentation(args, scope);
	scope.GlobalScope->Output.WriteLine(text);
	return std::make_shared<LispVariant>(std::make_shared<object>(text));
}

static std::shared_ptr<LispVariant> ArithmetricOperation(std::vector<std::shared_ptr<object>> args, std::function<std::shared_ptr<LispVariant>(std::shared_ptr<LispVariant>, std::shared_ptr<LispVariant>)> op)
{
	std::shared_ptr<LispVariant> result(null);
	for (var elem : args)
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

static std::shared_ptr<LispVariant> Addition(std::vector<std::shared_ptr<object>> args, LispScope & scope)
{
	return ArithmetricOperation(args, [](std::shared_ptr<LispVariant> l, std::shared_ptr<LispVariant> r) -> std::shared_ptr<LispVariant> { return std::make_shared<LispVariant>(*l + *r); });
}
static std::shared_ptr<LispVariant> Subtraction(std::vector<std::shared_ptr<object>> args, LispScope & scope)
{
	return ArithmetricOperation(args, [](std::shared_ptr<LispVariant> l, std::shared_ptr<LispVariant> r) -> std::shared_ptr<LispVariant> { return std::make_shared<LispVariant>(*l - *r); });
}

static std::shared_ptr<LispVariant> Multiplication(std::vector<std::shared_ptr<object>> args, LispScope & scope)
{
	return ArithmetricOperation(args, [](std::shared_ptr<LispVariant> l, std::shared_ptr<LispVariant> r) -> std::shared_ptr<LispVariant> { return std::make_shared<LispVariant>(*l * *r); });
}

static std::shared_ptr<LispVariant> Division(std::vector<std::shared_ptr<object>> args, LispScope & scope)
{
	return ArithmetricOperation(args, [](std::shared_ptr<LispVariant> l, std::shared_ptr<LispVariant> r) -> std::shared_ptr<LispVariant> { return std::make_shared<LispVariant>(*l / *r); });
}

/*private*/ static void CheckArgs(const string & name, int count, std::vector<std::shared_ptr<object>> args, LispScope & scope)
{
	if (count < 0 || args.size() != count)
	{
		throw new LispException(string::Format("Bad argument count in {0}, has {1} expected {2}", name, args.size(), count), &scope);
	}
}


/*public*/ static std::shared_ptr<LispVariant> if_form(std::vector<std::shared_ptr<object>> args, LispScope & scope)
{
	if (!(args.size() == 2 || args.size() == 3))
	{
		// throw exception
		CheckArgs(If, -1, args, scope);
	}

	var passed = LispInterpreter::EvalAst(args[0], scope)->BoolValue();
	var elseCode = args.size() > 2 ? args[2] : null;
	return LispInterpreter::EvalAst(passed ? args[1] : elseCode, scope);
}

/*public*/ static std::shared_ptr<LispVariant> do_form(std::vector<std::shared_ptr<object>> args, LispScope & scope)
{
	var result = std::make_shared<LispVariant>(LispVariant(LispType::_Undefined));

	for (var statement : args)
	{
		if (!(statement->IsIEnumerableOfObject() || statement->IsList() /*is IEnumerable<object>*/))
		{
			throw new LispException("List expected in do", /*((LispVariant)statement).Token*/statement->ToLispVariant()->Token, scope.ModuleName, scope.DumpStackToString());
		}
		result = LispInterpreter::EvalAst(statement, scope);
	}

	return result;
}



std::shared_ptr<LispScope> LispEnvironment::CreateDefaultScope()
{
	std::shared_ptr<LispScope> scope = std::make_shared<LispScope>();

	(*scope)[Tracebuffer] = std::make_shared<object>(""); // new StringBuilder();
	(*scope)[Traceon] = std::make_shared<object>(false);

	(*scope)["string"] = CreateFunction(Addition, "(string expr1 expr2 ...)", "see: add");
	(*scope)["add"] = CreateFunction(Addition, "(add expr1 expr2 ...)", "Returns value of expr1 added with expr2 added with ...");
	(*scope)["+"] = CreateFunction(Addition, "(+ expr1 expr2 ...)", "see: add");
	(*scope)["sub"] = CreateFunction(Subtraction, "(sub expr1 expr2 ...)", "Returns value of expr1 subtracted with expr2 subtracted with ...");
	(*scope)["-"] = CreateFunction(Subtraction, "(- expr1 expr2 ...)", "see: sub");
	(*scope)["mul"] = CreateFunction(Multiplication, "(mul expr1 expr2 ...)", "Returns value of expr1 multipied by expr2 multiplied by ...");
	(*scope)["*"] = CreateFunction(Multiplication, "(* expr1 expr2 ...)", "see: mul");
	(*scope)["div"] = CreateFunction(Division, "(div expr1 expr2 ...)", "Returns value of expr1 divided by expr2 divided by ...");
	(*scope)["/"] = CreateFunction(Division, "(/ expr1 expr2 ...)", "see: div");

	(*scope)["print"] = CreateFunction(Print, "(println expr1 expr2 ...)", "Prints the values of the given expressions on the console.");
	(*scope)["println"] = CreateFunction(PrintLn, "(println expr1 expr2 ...)", "Prints the values of the given expressions on the console adding a new line at the end of the output.");

	(*scope)["do"] = CreateFunction(do_form, "(do statement1 statement2 ...)", "Returns a sequence of statements.", /*isBuiltin:*/true, /*isSpecialForm:*/ true);

	(*scope)[If] = CreateFunction(if_form, "(if cond then-block [else-block])", "The if statement.", /*isBuiltin:*/true, /*isSpecialForm:*/ true);

	scope->PrivateInitForCpp();

	return scope;
}


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

