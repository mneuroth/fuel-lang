
#include "Scope.h"
#include "Interpreter.h"
#include "Lisp.h"

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

// ************************************************************************

static string GetStringRepresentation(std::vector<std::shared_ptr<object>> args, std::shared_ptr<LispScope> scope, string separator = " ")
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
	if (scope->ContainsKey(Traceon) && (bool)(*scope)[Traceon])
	{
		var buffer = /*(StringBuilder)*/(*scope)[Tracebuffer];
		string temp = buffer->ToString();
		temp += text;
		(*scope)[Tracebuffer] = std::make_shared<object>(temp);
		//old: buffer.Append(text);
	}
	return text;
}

static void CheckArgs(const string & name, int count, std::vector<std::shared_ptr<object>> args, std::shared_ptr<LispScope> scope)
{
	if (count < 0 || args.size() != count)
	{
		throw new LispException(string::Format("Bad argument count in {0}, has {1} expected {2}", name, args.size(), count), scope.get());
	}
}

// ************************************************************************

static std::shared_ptr<LispVariant> Fuel(std::vector<std::shared_ptr<object>> args, std::shared_ptr<LispScope> scope)
{
	string text; // var text = new StringBuilder();
	text.Append(string::Format("fuel version {0} from {1}", Lisp::Version, Lisp::Date));
	return std::make_shared<LispVariant>(std::make_shared<object>(text));
}

static std::shared_ptr<LispVariant> Copyright(std::vector<std::shared_ptr<object>> args, std::shared_ptr<LispScope> scope)
{
	string text; // var text = new StringBuilder();
	text.Append(string::Format("Copyright: {0} {1}", Lisp::License, Lisp::LicenseUrl));
	return std::make_shared<LispVariant>(std::make_shared<object>(text));
}

static std::shared_ptr<LispVariant> Help(std::vector<std::shared_ptr<object>> args, std::shared_ptr<LispScope> scope)
{
	string helpText; // var helpText = new StringBuilder();
	helpText.Append("available functions:\n");
	for(var cmd : scope->GetKeys())
	{
		string s = cmd + "\n";
		helpText.Append(s);
	}
	scope->GlobalScope->Output.WriteLine(helpText);
	return std::make_shared<LispVariant>(std::make_shared<object>(helpText));
}

static std::shared_ptr<LispVariant> DumpDocumentation(std::shared_ptr<LispScope> scope, Action dump)
{
	string text; // var text = new StringBuilder();
	var tempOutputWriter = scope->GlobalScope->Output;
// TODO implement	scope->GlobalScope->Output = new StringWriter(text);
	dump();
	scope->GlobalScope->Output = tempOutputWriter;
	return std::make_shared<LispVariant>(std::make_shared<object>(text));
}

static std::shared_ptr<LispVariant> DoSearchDocumentation(std::vector<std::shared_ptr<object>> args, std::shared_ptr<LispScope> scope, std::function<bool(const string &, const string &)> select)
{
	if (args.size() > 0)
	{
		string help = string::Empty;
		for(var item : args)
		{
			help += scope->GetFunctionsHelpFormated(item->ToString(), select);
		}
		return DumpDocumentation(scope, [help, scope]() -> void { scope->GlobalScope->Output.WriteLine("{0}", help); });
	}
	return DumpDocumentation(scope, [scope]() -> void { scope->GlobalScope->DumpBuiltinFunctionsHelpFormated(); });
}

static std::shared_ptr<LispVariant> HtmlDocumentation(std::vector<std::shared_ptr<object>> args, std::shared_ptr<LispScope> scope)
{
	return DumpDocumentation(scope, [scope]() -> void { scope->GlobalScope->DumpBuiltinFunctionsHelpHtmlFormated(); });
}

static std::shared_ptr<LispVariant> Documentation(std::vector<std::shared_ptr<object>> args, std::shared_ptr<LispScope> scope)
{
	return DoSearchDocumentation(args, scope, null);
}

static std::shared_ptr<LispVariant> SearchDocumentation(std::vector<std::shared_ptr<object>> args, std::shared_ptr<LispScope> scope)
{
	return DoSearchDocumentation(args, scope, [](const string & k, const string & n) -> bool { return k.Contains(n); });
}

static std::shared_ptr<LispVariant> Break(std::vector<std::shared_ptr<object>> args, std::shared_ptr<LispScope> scope)
{
	scope->GlobalScope->Output.WriteLine("break -> call stack:");
	scope->DumpStack(scope->GetCallStackSize());
	var debugger = scope->GlobalScope->Debugger;
	if (debugger != null)
	{
		debugger->InteractiveLoop(/*initialTopScope:*/ scope);
	}
	else
	{
		scope->GlobalScope->Output.WriteLine("Warning: can not break, because no debugger support availabe!");
	}
	return std::make_shared<LispVariant>(std::make_shared<object>(LispVariant(LispType::_Undefined)));;
}

static std::shared_ptr<LispVariant> Vars(std::vector<std::shared_ptr<object>> args, std::shared_ptr<LispScope> scope)
{
	scope->GlobalScope->Output.WriteLine("variables:");
	scope->DumpVars();
	return std::make_shared<LispVariant>(std::make_shared<object>(ObjectType::__Undefined));
}

static std::shared_ptr<LispVariant> TracePrint(std::vector<std::shared_ptr<object>> args, std::shared_ptr<LispScope> scope)
{
	var status = args[0]->ToLispVariant();
	(*scope)[Traceon] = std::make_shared<object>(status->BoolValue());
	return std::make_shared<LispVariant>(std::make_shared<object>(status->BoolValue()));
}

static std::shared_ptr<LispVariant> GetTracePrint(std::vector<std::shared_ptr<object>> args, std::shared_ptr<LispScope> scope)
{
	string buffer = (*scope)[Tracebuffer]->ToString(); // var buffer = (StringBuilder)(*scope)[Tracebuffer];
	return std::make_shared<LispVariant>(std::make_shared<object>(buffer));
}

// TODO --> platform independend implementation of tickcount
#include <Windows.h>
static std::shared_ptr<LispVariant> CurrentTickCount(std::vector<std::shared_ptr<object>> args, std::shared_ptr<LispScope> scope)
{
	var value = (int)GetTickCount(); //TODO --> implement in C++ ???  Environment::TickCount;
	return std::make_shared<LispVariant>(std::make_shared<object>(value));
}

static string AddFileExtensionIfNeeded(string fileName)
{
	const string extension = ".fuel";

	if (!fileName.EndsWith(extension))
	{
		fileName += extension;
	}
	return fileName;
}

static std::shared_ptr<LispVariant> Import(std::vector<std::shared_ptr<object>> args, std::shared_ptr<LispScope> scope)
{
	std::shared_ptr<LispVariant> result = std::make_shared<LispVariant>();
/* TODO --> implement
	for(var modu : args)
	{
		string code = string::Empty;
		string orgModuleFileName = ((LispVariant)modu).StringValue;
		string fileName = orgModuleFileName;
		if (!File.Exists(fileName))
		{
			// try the given library path (if available)
			fileName = LispUtils.LibraryPath + Path.DirectorySeparatorChar + orgModuleFileName;
			fileName = AddFileExtensionIfNeeded(fileName);
			if (!File.Exists(fileName))
			{
				// try default path .\Library\modulename.fuel
				fileName = "." + Path.DirectorySeparatorChar + "Library" + Path.DirectorySeparatorChar + orgModuleFileName;
				fileName = AddFileExtensionIfNeeded(fileName);
				if (!File.Exists(fileName))
				{
					// try default path <fuel.exe-path>\Library\modulename.fuel
					fileName = AppDomain.CurrentDomain.BaseDirectory + Path.DirectorySeparatorChar + "Library" + Path.DirectorySeparatorChar + orgModuleFileName;
					fileName = AddFileExtensionIfNeeded(fileName);
					if (!File.Exists(fileName))
					{
						// try environment variable FUELPATH
						string envPath = Environment.GetEnvironmentVariable("FUELPATH");
						if (envPath != null)
						{
							fileName = envPath + Path.DirectorySeparatorChar + orgModuleFileName;
							fileName = AddFileExtensionIfNeeded(fileName);
						}
					}
				}
			}
		}
		if (File.Exists(fileName))
		{
			code = File.ReadAllText(fileName);
		}
		else
		{
			// use std lib of fuel from builtin resources
			if (orgModuleFileName == "fuellib")
			{
				code = Encoding.UTF8.GetString(Properties.Resources.fuellib);
			}
			else
			{
				scope.GlobalScope.Output.WriteLine("WARNING: Library {0} not found! Tried path {1}", orgModuleFileName, fileName);
			}
		}
		if (!string.IsNullOrEmpty(code))
		{
			var importScope = new LispScope("import " + fileName, scope.GlobalScope, fileName);
			scope.PushNextScope(importScope);

			result = Lisp.Eval(code, importScope, fileName);

			// add new module to modules scope
			((LispScope)scope.GlobalScope[Modules]).Add(fileName, importScope);

			scope.PopNextScope();
		}
	}
*/
	return result;
}

static std::shared_ptr<LispVariant> Nop(std::vector<std::shared_ptr<object>> args, std::shared_ptr<LispScope> scope)
{
	return std::make_shared<LispVariant>(LispVariant());
}

static std::shared_ptr<LispVariant> Return(std::vector<std::shared_ptr<object>> args, std::shared_ptr<LispScope> scope)
{
	return std::make_shared<LispVariant>(LispVariant(args[0]));
}

static std::shared_ptr<LispVariant> GetType(std::vector<std::shared_ptr<object>> args, std::shared_ptr<LispScope> scope)
{
	CheckArgs("type", 1, args, scope);

	var item = ((LispVariant)args[0]);
	return std::make_shared<LispVariant>(std::make_shared<object>((int)item.Type));
}

static std::shared_ptr<LispVariant> GetTypeString(std::vector<std::shared_ptr<object>> args, std::shared_ptr<LispScope> scope)
{
	CheckArgs("typestr", 1, args, scope);

	var item = ((LispVariant)args[0]);
	return std::make_shared<LispVariant>(std::make_shared<object>(item.TypeString()));
}

static std::shared_ptr<LispVariant> Print(std::vector<std::shared_ptr<object>> args, std::shared_ptr<LispScope> scope)
{
	var text = GetStringRepresentation(args, scope);
	scope->GlobalScope->Output.Write(text);
	return std::make_shared<LispVariant>(std::make_shared<object>(text));
}

static std::shared_ptr<LispVariant> PrintLn(std::vector<std::shared_ptr<object>> args, std::shared_ptr<LispScope> scope)
{
	var text = GetStringRepresentation(args, scope);
	scope->GlobalScope->Output.WriteLine(text);
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

static std::shared_ptr<LispVariant> CompareOperation(std::vector<std::shared_ptr<object>> args, std::function<std::shared_ptr<LispVariant>(std::shared_ptr<LispVariant>, std::shared_ptr<LispVariant>)> op, std::shared_ptr<LispScope> scope)
{
	CheckArgs("compare-op", 2, args, scope);

	var arg1 = args[0]->ToLispVariant();
	var arg2 = args[1]->ToLispVariant();
	std::shared_ptr<LispVariant> result = op(arg1, arg2);
	return result;
}

static std::shared_ptr<LispVariant> Addition(std::vector<std::shared_ptr<object>> args, std::shared_ptr<LispScope> scope)
{
	return ArithmetricOperation(args, [](std::shared_ptr<LispVariant> l, std::shared_ptr<LispVariant> r) -> std::shared_ptr<LispVariant> { return std::make_shared<LispVariant>(*l + *r); });
}

static std::shared_ptr<LispVariant> Subtraction(std::vector<std::shared_ptr<object>> args, std::shared_ptr<LispScope> scope)
{
	return ArithmetricOperation(args, [](std::shared_ptr<LispVariant> l, std::shared_ptr<LispVariant> r) -> std::shared_ptr<LispVariant> { return std::make_shared<LispVariant>(*l - *r); });
}

static std::shared_ptr<LispVariant> Multiplication(std::vector<std::shared_ptr<object>> args, std::shared_ptr<LispScope> scope)
{
	return ArithmetricOperation(args, [](std::shared_ptr<LispVariant> l, std::shared_ptr<LispVariant> r) -> std::shared_ptr<LispVariant> { return std::make_shared<LispVariant>(*l * *r); });
}

static std::shared_ptr<LispVariant> Division(std::vector<std::shared_ptr<object>> args, std::shared_ptr<LispScope> scope)
{
	return ArithmetricOperation(args, [](std::shared_ptr<LispVariant> l, std::shared_ptr<LispVariant> r) -> std::shared_ptr<LispVariant> { return std::make_shared<LispVariant>(*l / *r); });
}

static std::shared_ptr<LispVariant>Not(std::vector<std::shared_ptr<object>> args, std::shared_ptr<LispScope> scope)
{
	CheckArgs("not", 1, args, scope);

	var arg1 = (LispVariant)args[0];
	return std::make_shared<LispVariant>(std::make_shared<object>(!arg1.BoolValue()));
}

static std::shared_ptr<LispVariant>LessTest(std::vector<std::shared_ptr<object>> args, std::shared_ptr<LispScope> scope)
{
	return CompareOperation(args, [](std::shared_ptr<LispVariant> l, std::shared_ptr<LispVariant> r) -> std::shared_ptr<LispVariant> { return std::make_shared<LispVariant>(*l < *r); }, scope);
}

static std::shared_ptr<LispVariant>GreaterTest(std::vector<std::shared_ptr<object>> args, std::shared_ptr<LispScope> scope)
{
	return CompareOperation(args, [](std::shared_ptr<LispVariant> l, std::shared_ptr<LispVariant> r) -> std::shared_ptr<LispVariant> { return std::make_shared<LispVariant>(*l > *r); }, scope);
}

static std::shared_ptr<LispVariant>LessEqualTest(std::vector<std::shared_ptr<object>> args, std::shared_ptr<LispScope> scope)
{
	return CompareOperation(args, [](std::shared_ptr<LispVariant> l, std::shared_ptr<LispVariant> r) -> std::shared_ptr<LispVariant> { return std::make_shared<LispVariant>(*l <= *r); }, scope);
}

static std::shared_ptr<LispVariant>GreaterEqualTest(std::vector<std::shared_ptr<object>> args, std::shared_ptr<LispScope> scope)
{
	return CompareOperation(args, [](std::shared_ptr<LispVariant> l, std::shared_ptr<LispVariant> r) -> std::shared_ptr<LispVariant> { return std::make_shared<LispVariant>(*l >= *r); }, scope);
}

static std::shared_ptr<LispVariant>EqualTest(std::vector<std::shared_ptr<object>> args, std::shared_ptr<LispScope> scope)
{
	return CompareOperation(args, [](std::shared_ptr<LispVariant> l, std::shared_ptr<LispVariant> r) -> std::shared_ptr<LispVariant> { return std::make_shared<LispVariant>(std::make_shared<object>(LispVariant::EqualOp(*l, *r))); }, scope);
}

static std::shared_ptr<LispVariant>NotEqualTest(std::vector<std::shared_ptr<object>> args, std::shared_ptr<LispScope> scope)
{
	return CompareOperation(args, [](std::shared_ptr<LispVariant> l, std::shared_ptr<LispVariant> r) -> std::shared_ptr<LispVariant> { return std::make_shared<LispVariant>(std::make_shared<object>(!LispVariant::EqualOp(*l, *r))); }, scope);
}

static std::shared_ptr<LispVariant> if_form(std::vector<std::shared_ptr<object>> args, std::shared_ptr<LispScope> scope)
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

static std::shared_ptr<LispVariant> do_form(std::vector<std::shared_ptr<object>> args, std::shared_ptr<LispScope> scope)
{
	var result = std::make_shared<LispVariant>(LispVariant(LispType::_Undefined));

	for (var statement : args)
	{
		if (!(statement->IsIEnumerableOfObject() || statement->IsList() /*is IEnumerable<object>*/))
		{
			throw new LispException("List expected in do", /*((LispVariant)statement).Token*/statement->ToLispVariant()->Token, scope->ModuleName, scope->DumpStackToString());
		}
		result = LispInterpreter::EvalAst(statement, scope);
	}

	return result;
}

// ************************************************************************

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

std::shared_ptr<object> LispEnvironment::GetMacro(std::shared_ptr<object> funcName, std::shared_ptr<LispScope> scope)
{
	return QueryItem(funcName, scope, Macros);
}

bool LispEnvironment::IsExpression(std::shared_ptr<object> item)
{
	return (item->IsLispVariant() /*is LispVariant*/ && item->IsList()) ||
		(item->IsIEnumerableOfObject());
}

std::shared_ptr<IEnumerable<std::shared_ptr<object>>> LispEnvironment::GetExpression(std::shared_ptr<object> item)
{
	if (item->IsLispVariant() /*is LispVariant*/ && item->IsList()/*((LispVariant)item).IsList*/)
	{
		return item->ToLispVariant()->ListValue(); // ((LispVariant)item).ListValue;
	}
	if (item->IsIEnumerableOfObject() /*is IEnumerable<object>*/)
	{
		return item->ToList(); // (IEnumerable<object>)item;
	}
	return std::make_shared<IEnumerable<std::shared_ptr<object>>>(); // new List<object>();
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

std::shared_ptr<LispScope> LispEnvironment::CreateDefaultScope()
{
	std::shared_ptr<LispScope> scope = std::make_shared<LispScope>();

	(*scope)[Tracebuffer] = std::make_shared<object>(""); // new StringBuilder();
	(*scope)[Traceon] = std::make_shared<object>(false);

	(*scope)["fuel"] = CreateFunction(Fuel, "(fuel)", "Returns and shows information about the fuel language.");
	(*scope)["copyright"] = CreateFunction(Copyright, "(copyright)", "Returns and shows the copyright of the fuel language.");
	(*scope)["help"] = CreateFunction(Help, "(help)", "Returns and shows the available builtin functions.");
	(*scope)["doc"] = CreateFunction(Documentation, "(doc functionname ...)", "Returns and shows the documentation of all builtin functions or for the given function name(s).");
	(*scope)["searchdoc"] = CreateFunction(SearchDocumentation, "(searchdoc name ...)", "Returns and shows the documentation of functions containing name(s).");
	(*scope)["htmldoc"] = CreateFunction(HtmlDocumentation, "(htmldoc)", "Returns and shows the documentation of all builtin functions in html format.");
	(*scope)["break"] = CreateFunction(Break, "(break)", "Sets a breakpoint in the code.");
	(*scope)["vars"] = CreateFunction(Vars, "(vars)", "Returns a dump of all variables.");
	(*scope)["trace"] = CreateFunction(TracePrint, "(trace value)", "Switches the trace modus on or off.");
	(*scope)["gettrace"] = CreateFunction(GetTracePrint, "(gettrace)", "Returns the trace output.");
//	(*scope)["import"] = CreateFunction(Import, "(import module1 ...)", "Imports modules with fuel code.");
	(*scope)["tickcount"] = CreateFunction(CurrentTickCount, "(tickcount)", "Returns the current tick count in milliseconds, can be used to measure times.");

	// access to .NET
//	(*scope)["native-methods"] = CreateFunction(GetNativeMethods, "(native-methods native-obj|class-name) -> (method-name, argument-count, is-static, net-method-name)", "Returns a list of all available method names of the given native class.");
//	(*scope)["native-fields"] = CreateFunction(GetNativeFields, "(native-fields native-obj|class-name) -> (property-name)", "Returns a list of all available property names of the given native class.");
//	(*scope)["field"] = CreateFunction(CallField, "(field native-obj|class-name field-name)", "Accesses a field of a native object.");    // access native field
//	(*scope)["call"] = CreateFunction(CallNative, "(call native-obj|class-name [method-name [args...]]|[args...])", "Calls a method for a native object.");    // call native function
//	(*scope)["call-static"] = CreateFunction(CallStaticNative, "(call-static class-name method-name [args...])", "Calls a static method for a native object.");    // call native static function
	// Macro: (register-native full-class-name lisp-name) --> erzeugt konstruktoren und zugriffsmethoden fuer klasse
	// --> (lisp-name-create args)																																								// --> (lisp-name-method obj args)

	// interpreter functions
	//(*scope)["type"] = CreateFunction(GetType, "(type expr)", "Returns the type id of the value of the expression.");
	//(*scope)["typestr"] = CreateFunction(GetTypeString, "(typestr expr)", "Returns a readable string representing the type of the value of the expression.");
	//(*scope)["nop"] = CreateFunction(Nop, "(nop)", "Does nothing (no operation).");
	//(*scope)["return"] = CreateFunction(Return, "(return expr)", "Returns the value of the expression and quits the function.");
	(*scope)["print"] = CreateFunction(Print, "(println expr1 expr2 ...)", "Prints the values of the given expressions on the console.");
	(*scope)["println"] = CreateFunction(PrintLn, "(println expr1 expr2 ...)", "Prints the values of the given expressions on the console adding a new line at the end of the output.");

	(*scope)["string"] = CreateFunction(Addition, "(string expr1 expr2 ...)", "see: add");
	(*scope)["add"] = CreateFunction(Addition, "(add expr1 expr2 ...)", "Returns value of expr1 added with expr2 added with ...");
	(*scope)["+"] = CreateFunction(Addition, "(+ expr1 expr2 ...)", "see: add");
	(*scope)["sub"] = CreateFunction(Subtraction, "(sub expr1 expr2 ...)", "Returns value of expr1 subtracted with expr2 subtracted with ...");
	(*scope)["-"] = CreateFunction(Subtraction, "(- expr1 expr2 ...)", "see: sub");
	(*scope)["mul"] = CreateFunction(Multiplication, "(mul expr1 expr2 ...)", "Returns value of expr1 multipied by expr2 multiplied by ...");
	(*scope)["*"] = CreateFunction(Multiplication, "(* expr1 expr2 ...)", "see: mul");
	(*scope)["div"] = CreateFunction(Division, "(div expr1 expr2 ...)", "Returns value of expr1 divided by expr2 divided by ...");
	(*scope)["/"] = CreateFunction(Division, "(/ expr1 expr2 ...)", "see: div");

	/*
	(*scope)["<"] = CreateFunction(LessTest, "(< expr1 expr2)", "Returns #t if value of expression1 is smaller than value of expression2 and returns #f otherwiese.");
	(*scope)[">"] = CreateFunction(GreaterTest, "(> expr1 expr2)", "Returns #t if value of expression1 is larger than value of expression2 and returns #f otherwiese.");
	(*scope)["<="] = CreateFunction(LessEqualTest, "(<= expr1 expr2)", "Returns #t if value of expression1 is equal or smaller than value of expression2 and returns #f otherwiese.");
	(*scope)[">="] = CreateFunction(GreaterEqualTest, "(>= expr1 expr2)", "Returns #t if value of expression1 is equal or larger than value of expression2 and returns #f otherwiese.");

	(*scope)["equal"] = CreateFunction(EqualTest, "(equal expr1 expr2)", "Returns #t if value of expression1 is equal with value of expression2 and returns #f otherwiese.");
	(*scope)["="] = CreateFunction(EqualTest, "(= expr1 expr2)", "see: equal");
	(*scope)["=="] = CreateFunction(EqualTest, "(== expr1 expr2)", "see: equal");
	(*scope)["!="] = CreateFunction(NotEqualTest, "(!= expr1 expr2)", "Returns #t if value of expression1 is not equal with value of expression2 and returns #f otherwiese.");

	(*scope)["not"] = CreateFunction(Not, "(not expr)", "Returns the inverted bool value of the expression.");
	(*scope)["!"] = CreateFunction(Not, "(! expr)", "see: not");

	(*scope)["list"] = CreateFunction(CreateList, "(list item1 item2 ...)", "Returns a new list with the given elements.");
	(*scope)[MapFcn] = CreateFunction(Map, "(map function list)", "Returns a new list with elements, where all elements of the list where applied to the function.");
	(*scope)[ReduceFcn] = CreateFunction(Reduce, "(reduce function list initial)", "Reduce function.");
	(*scope)["cons"] = CreateFunction(Cons, "(cons item list)", "Returns a new list containing the item and the elements of the list.");
	(*scope)["len"] = CreateFunction(Length, "(len list)", "Returns the length of the list.");
	(*scope)["first"] = CreateFunction(First, "(first list)", "see: car");
	(*scope)["car"] = CreateFunction(First, "(car list)", "Returns the first element of the list.");
	(*scope)["rest"] = CreateFunction(Rest, "(rest list)", "see: cdr");
	(*scope)["cdr"] = CreateFunction(Rest, "(cdr list)", "Returns a new list containing all elements except the first of the given list.");
	(*scope)["nth"] = CreateFunction(Nth, "(nth number list)", "Returns the [number] element of the list.");
	(*scope)["append"] = CreateFunction(Append, "(append list1 list2 ...)", "Returns a new list containing all given lists elements.");
	(*scope)[Sym] = CreateFunction(Symbol, "(sym expr)", "Returns the evaluated expression as symbol.");
	(*scope)[Str] = CreateFunction(ConvertToString, "(str expr)", "Returns the evaluated expression as string.");

	(*scope)[ArgsCount] = CreateFunction(ArgsCountFcn, "(argscount)", "Returns the number of command line arguments for this script.");
	(*scope)[Args] = CreateFunction(ArgsFcn, "(args number)", "Returns the [number] command line argument for this script.");
	(*scope)[Apply] = CreateFunction(ApplyFcn, "(apply function arguments-list)", "Calls the function with the arguments.");
	(*scope)[Eval] = CreateFunction(EvalFcn, "(eval ast)", "Evaluates the abstract syntax tree (ast).");
	(*scope)[EvalStr] = CreateFunction(EvalStrFcn, "(evalstr string)", "Evaluates the string.");

	// special forms
	(*scope)[And] = CreateFunction(and_form, "(and expr1 expr2 ...)", "And operator with short cut.", isSpecialForm: true);
	(*scope)[Or] = CreateFunction(or_form, "(or expr1 expr2 ...)", "Or operator with short cut.", isSpecialForm: true);
	(*scope)[Def] = CreateFunction(def_form, "(def symbol expression)", "Creates a new variable with name of symbol in current scope. Evaluates expression and sets the value of the expression as the value of the symbol.", isSpecialForm: true);
	(*scope)[Gdef] = CreateFunction(gdef_form, "(gdef symbol expression)", "Creates a new variable with name of symbol in global scope. Evaluates expression and sets the value of the expression as the value of the symbol.", isSpecialForm: true);
	(*scope)[Setf] = CreateFunction(setf_form, "(setf symbol expression)", "Evaluates expression and sets the value of the expression as the value of the symbol.", isSpecialForm: true);

	// macros are:
	// a special form to control evaluation of function parameters inside the macro code
	// there are two options possible:
	//  - run time evaluation of macros
	//  - compile time replacement/expanding of macros
	(*scope)[DefineMacro] = CreateFunction(definemacroevaluate_form, "(define-macro name (arguments) statement)", "see: define-macro-eval", isSpecialForm: true);
	// run time evaluation for macros:
	(*scope)[DefineMacroEval] = CreateFunction(definemacroevaluate_form, "(define-macro-eval name (arguments) statement)", "Special form: Defines a macro which will be evaluated at run time.", isSpecialForm: true);
	#if ENABLE_COMPILE_TIME_MACROS
	// compile time expand for macros:
	(*scope)[DefineMacroExpand] = CreateFunction(definemacroexpand_form, "(define-macro-expand name (arguments) statement)", "Special form: Defines a macro which will be evaluated at compile time.", isSpecialForm: true, isEvalInExpand: true);
	#endif

	(*scope)[Quote] = CreateFunction(quote_form, "(quote expr)", "Returns expression without evaluating it.", isSpecialForm: true);
	(*scope)[Quasiquote] = CreateFunction(quasiquote_form, "(quasiquote expr)", "Returns expression without evaluating it, but processes evaluation operators , and ,@.", isSpecialForm: true);
	(*scope)[If] = CreateFunction(if_form, "(if cond then-block [else-block])", "The if statement.", isSpecialForm: true);
	(*scope)[While] = CreateFunction(while_form, "(while cond block)", "The while loop.", isSpecialForm: true);
	(*scope)[Do] = CreateFunction(do_form, "(do statement1 statement2 ...)", "Returns a sequence of statements.", isSpecialForm: true);
	(*scope)[Begin] = CreateFunction(do_form, "(begin statement1 statement2 ...)", "see: do", isSpecialForm: true);
	(*scope)[Lambda] = CreateFunction(fn_form, "(lambda (arguments) block)", "Returns a lambda function.", isSpecialForm: true);
	(*scope)[Fn] = CreateFunction(fn_form, "(fn (arguments) block)", "Returns a function.", isSpecialForm: true);
	(*scope)[Defn] = CreateFunction(defn_form, "(defn name (args) block)", "Defines a function in the current scope.", isSpecialForm: true);
	(*scope)[Gdefn] = CreateFunction(gdefn_form, "(gdefn name (args) block)", "Defines a function in the global scope.", isSpecialForm: true);
	*/

	(*scope)["do"] = CreateFunction(do_form, "(do statement1 statement2 ...)", "Returns a sequence of statements.", /*isBuiltin:*/true, /*isSpecialForm:*/ true);

	(*scope)[If] = CreateFunction(if_form, "(if cond then-block [else-block])", "The if statement.", /*isBuiltin:*/true, /*isSpecialForm:*/ true);

	scope->PrivateInitForCpp();

	return scope;
}
