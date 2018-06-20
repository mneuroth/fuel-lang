
#include "Scope.h"
#include "Interpreter.h"

using namespace CsLisp;

const string LispEnvironment::Quote = "quote";
const string LispEnvironment::Quasiquote = "quasiquote";
string LispEnvironment::Macros;

/*private*/ const string MetaTag = "###";
/*private*/ const string Builtin = "<builtin>";

/*private*/ const string MainScope = "<main>";

/*private*/ const string Lambda = "lambda";
/*private*/ const string Tracebuffer = MetaTag + "tracebuffer" + MetaTag;
/*private*/ const string Traceon = MetaTag + "traceon" + MetaTag;

static std::shared_ptr<object> CreateFunction(FuncX func , const string & signature = /*null*/"", const string & documentation = /*null*/"", bool isBuiltin = true, bool isSpecialForm = false, bool isEvalInExpand = false, const string & moduleName = "Builtin")
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
	for(var item : args)
	{
		if (text.Length() > 0)
		{
			text += separator;
		}
		text += item->ToString();
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

	(*scope)["do"] = CreateFunction(&do_form, "(do statement1 statement2 ...)", "Returns a sequence of statements.", /*isBuiltin:*/true, /*isSpecialForm:*/ true);

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

