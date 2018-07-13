
#include "Scope.h"
#include "Interpreter.h"
#include "Lisp.h"

#include <map>
#include <fstream>

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
#ifdef ENABLE_COMPILE_TIME_MACROS 
const string DefineMacroExpand = "define-macro-expand";
#endif
const string Lambda = "lambda";
const string MetaTag = "###";
const string Tracebuffer = MetaTag + "tracebuffer" + MetaTag;
const string Traceon = MetaTag + "traceon" + MetaTag;
const string ArgsMeta = MetaTag + "args" + MetaTag;
const string AdditionalArgs = "_additionalArgs";

const string ArgsCount = "argscount";
const string Args = "args";

/*private*/ const string Builtin = "<builtin>";

/*private*/ const string MainScope = "<main>";

const string LispEnvironment::MetaTag = /*MetaTag*/"###";
const string LispEnvironment::Macros = LispEnvironment::MetaTag + "macros" + LispEnvironment::MetaTag;
const string LispEnvironment::Modules = LispEnvironment::MetaTag + "modules" + LispEnvironment::MetaTag;

const string LispEnvironment::Apply = "apply";
const string LispEnvironment::Eval = "eval";
const string LispEnvironment::EvalStr = "evalstr";
const string LispEnvironment::Quote = "quote";
const string LispEnvironment::Quasiquote = "quasiquote";

const string LispEnvironment::Sym = "sym";
const string LispEnvironment::Str = "str";

// ************************************************************************

namespace CsLisp
{
	string LispUtils_LibraryPath = string::Empty;
}

static bool File_Exists(const string & fileName)
{
	std::ifstream infile(fileName);
	return infile.good();
}

std::string ReadFileOrEmptyString(const std::string & fileName)
{
	std::ifstream ifs(fileName);
	std::string content = std::string((std::istreambuf_iterator<char>(ifs)), (std::istreambuf_iterator<char>()));
	return std::string(content);
}

bool WriteTextFile(const std::string & fileName, const std::string & content)
{
	std::ofstream ofs(fileName);
	ofs << content;
	return ofs.good();
}

static string GetStringRepresentation(const std::vector<std::shared_ptr<object>> & args, std::shared_ptr<LispScope> scope, string separator = " ")
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

static std::shared_ptr<LispVariant> CheckForFunction(const string & functionName, std::shared_ptr<object> arg0, std::shared_ptr<LispScope> scope)
{
	var function = arg0->ToLispVariant();
	if (!function->IsFunction())
	{
		throw LispException("No function in " + functionName, scope.get());
	}
	return function;
}

static void CheckArgs(const string & name, size_t count, const std::vector<std::shared_ptr<object>> & args, std::shared_ptr<LispScope> scope)
{
	if (count < 0 || args.size() != count)
	{
		throw LispException(string::Format("Bad argument count in {0}, has {1} expected {2}", name, std::to_string(args.size()), std::to_string(count)), scope.get());
	}
}

static const std::vector<std::shared_ptr<object>> GetCallArgs(const std::vector<std::shared_ptr<object>> & args)
{
	std::vector<std::shared_ptr<object>> callArgs(args.size() > 1 ? args.size() - 2 : 0);
	if (args.size() > 2)
	{
		//Array.Copy(args, 2, callArgs, 0, args.Length - 2);
		for (size_t i = 2; i < args.size(); i++)
		{
			callArgs[i - 2] = args[i];
		}
	}
	return callArgs;
}

static std::shared_ptr<object> CreateFunction(FuncX func, const string & signature = /*null*/"", const string & documentation = /*null*/"", bool isBuiltin = true, bool isSpecialForm = false, bool isEvalInExpand = false, const string & moduleName = Builtin)
{
	LispFunctionWrapper wrapper;
	wrapper.Function = func;
	wrapper.Signature = signature;
	wrapper.ModuleName = moduleName;
	wrapper.Documentation = documentation;
	wrapper.SetSpecialForm(isSpecialForm);
	wrapper.SetEvalInExpand(isEvalInExpand);
	wrapper.SetBuiltin(isBuiltin);
	return std::make_shared<object>(LispVariant(LispType::_Function, std::make_shared<object>(wrapper)));
}

static std::shared_ptr<LispVariant> EvalArgIfNeeded(std::shared_ptr<object> arg, std::shared_ptr<LispScope> scope)
{
	return (arg->IsIEnumerableOfObject() /*is IEnumerable<object>*/ || arg->IsList()) ? LispInterpreter::EvalAst(arg, scope) : arg->ToLispVariant();
}

static std::shared_ptr<LispVariant> def_form_helper(const std::vector<std::shared_ptr<object>> & args, std::shared_ptr<LispScope> scope, const string & name, std::shared_ptr<LispScope> scopeToSet)
{
	CheckArgs(name, 2, args, scope);

	var symbol = EvalArgIfNeeded(args[0], scope);
	if (!(symbol->IsSymbol() || symbol->IsString()))
	{
		throw LispException("Symbol expected", scope.get());
	}
	var value = LispInterpreter::EvalAst(args[1], scope);
	var ret = std::make_shared<object>(*value);
	(*scopeToSet)[symbol->ToString()] = ret;
	return std::make_shared<LispVariant>(ret);
}

// ************************************************************************

static std::shared_ptr<LispVariant> Fuel(const std::vector<std::shared_ptr<object>> & args, std::shared_ptr<LispScope> scope)
{
	string text; // var text = new StringBuilder();
	text.Append(string::Format("fuel version {0} from {1}", Lisp::Version, Lisp::Date));
	return std::make_shared<LispVariant>(std::make_shared<object>(text));
}

static std::shared_ptr<LispVariant> Copyright(const std::vector<std::shared_ptr<object>> & args, std::shared_ptr<LispScope> scope)
{
	string text; // var text = new StringBuilder();
	text.Append(string::Format("Copyright: {0} {1}", Lisp::License, Lisp::LicenseUrl));
	return std::make_shared<LispVariant>(std::make_shared<object>(text));
}

static std::shared_ptr<LispVariant> Help(const std::vector<std::shared_ptr<object>> & args, std::shared_ptr<LispScope> scope)
{
	string helpText; // var helpText = new StringBuilder();
	helpText.Append("available functions:\n");
	for(var cmd : scope->GetKeys())
	{
		string s = cmd + "\n";
		helpText.Append(s);
	}
	scope->GlobalScope->Output->WriteLine(helpText);
	return std::make_shared<LispVariant>(std::make_shared<object>(helpText));
}

static std::shared_ptr<LispVariant> DumpDocumentation(std::shared_ptr<LispScope> scope, Action dump)
{
	//string text; // var text = new StringBuilder();
	var tempOutputWriter = scope->GlobalScope->Output;
	scope->GlobalScope->Output = std::make_shared<TextWriter>(true); // StringWriter(text);
	dump();
	string text = scope->GlobalScope->Output->GetContent();
	scope->GlobalScope->Output = tempOutputWriter;
	return std::make_shared<LispVariant>(std::make_shared<object>(text));
}

static std::shared_ptr<LispVariant> DoSearchDocumentation(const std::vector<std::shared_ptr<object>> & args, std::shared_ptr<LispScope> scope, std::function<bool(const string &, const string &)> select)
{
	if (args.size() > 0)
	{
		string help = string::Empty;
		for(var item : args)
		{
			help += scope->GetFunctionsHelpFormated(item->ToString(), select);
		}
		return DumpDocumentation(scope, [help, scope]() -> void { scope->GlobalScope->Output->WriteLine("{0}", help); });
	}
	return DumpDocumentation(scope, [scope]() -> void { scope->GlobalScope->DumpBuiltinFunctionsHelpFormated(); });
}

static std::shared_ptr<LispVariant> HtmlDocumentation(const std::vector<std::shared_ptr<object>> & args, std::shared_ptr<LispScope> scope)
{
	return DumpDocumentation(scope, [scope]() -> void { scope->GlobalScope->DumpBuiltinFunctionsHelpHtmlFormated(); });
}

static std::shared_ptr<LispVariant> Documentation(const std::vector<std::shared_ptr<object>> & args, std::shared_ptr<LispScope> scope)
{
	return DoSearchDocumentation(args, scope, null);
}

static std::shared_ptr<LispVariant> SearchDocumentation(const std::vector<std::shared_ptr<object>> & args, std::shared_ptr<LispScope> scope)
{
	return DoSearchDocumentation(args, scope, [](const string & k, const string & n) -> bool { return k.Contains(n); });
}

static std::shared_ptr<LispVariant> Break(const std::vector<std::shared_ptr<object>> & args, std::shared_ptr<LispScope> scope)
{
	scope->GlobalScope->Output->WriteLine("break -> call stack:");
	scope->DumpStack(scope->GetCallStackSize());
	var debugger = scope->GlobalScope->Debugger;
	if (debugger != null)
	{
		debugger->InteractiveLoop(/*initialTopScope:*/ scope);
	}
	else
	{
		scope->GlobalScope->Output->WriteLine("Warning: can not break, because no debugger support availabe!");
	}
	return std::make_shared<LispVariant>(std::make_shared<object>(LispVariant(LispType::_Undefined)));;
}

static std::shared_ptr<LispVariant> Vars(const std::vector<std::shared_ptr<object>> & args, std::shared_ptr<LispScope> scope)
{
	scope->GlobalScope->Output->WriteLine("variables:");
	scope->DumpVars();
	return std::make_shared<LispVariant>(LispVariant());
}

static std::shared_ptr<LispVariant> TracePrint(const std::vector<std::shared_ptr<object>> & args, std::shared_ptr<LispScope> scope)
{
	var status = args[0]->ToLispVariant();
	(*scope)[Traceon] = std::make_shared<object>(status->BoolValue());
	return std::make_shared<LispVariant>(std::make_shared<object>(status->BoolValue()));
}

static std::shared_ptr<LispVariant> GetTracePrint(const std::vector<std::shared_ptr<object>> & args, std::shared_ptr<LispScope> scope)
{
	string buffer = (*scope)[Tracebuffer]->ToString(); // var buffer = (StringBuilder)(*scope)[Tracebuffer];
	return std::make_shared<LispVariant>(std::make_shared<object>(buffer));
}

#if defined(_WIN32) || defined(_WIN64)
#include <Windows.h>
#else
//#include <sys/sysinfo.h>
#include <sys/time.h>
// see https://www.c-plusplus.net/forum/topic/153559/gettickcount-für-linux
uint64_t GetTickCount(void)
{
	struct timeval tv;

	gettimeofday(&tv, 0);
	return uint64_t(tv.tv_sec) * 1000 + tv.tv_usec / 1000;
}
/*
long getTickCount() // Zeit seit dem Booten in Sekunden
{
	struct sysinfo si;
	if (sysinfo(&si) == 0) return si.uptime;

	return -1;
}
*/
#endif

namespace CsLisp
{
	uint64_t Environment_GetTickCount(void)
	{
		return GetTickCount();
	}
}

static std::shared_ptr<LispVariant> CurrentTickCount(const std::vector<std::shared_ptr<object>> & args, std::shared_ptr<LispScope> scope)
{
	var value = (int)GetTickCount();
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

namespace CsLisp
{
	extern string LispUtils_LibraryPath;
}

static std::shared_ptr<LispVariant> Import(const std::vector<std::shared_ptr<object>> & args, std::shared_ptr<LispScope> scope)
{
#ifdef _WIN32
	const std::string DirectorySeparatorChar("\\");
#else
	const std::string DirectorySeparatorChar("/");
#endif

	std::shared_ptr<LispVariant> result = std::make_shared<LispVariant>();
	for(var modu : args)
	{
		string code = string::Empty;
		string orgModuleFileName = modu->ToLispVariant()->StringValue();
		string fileName = orgModuleFileName;
		if (!File_Exists(fileName))
		{
			// try the given library path (if available)
			fileName = LispUtils_LibraryPath + /*Path.*/DirectorySeparatorChar + orgModuleFileName;
			fileName = AddFileExtensionIfNeeded(fileName);
			if (!File_Exists(fileName))
			{
				// try default path .\Library\modulename.fuel
				fileName = "." + /*Path.*/DirectorySeparatorChar + "Library" + /*Path.*/DirectorySeparatorChar + orgModuleFileName;
				fileName = AddFileExtensionIfNeeded(fileName);		
				if (!File_Exists(fileName))
				{
		//			// try default path <fuel.exe-path>\Library\modulename.fuel
		//// TODO			fileName = AppDomain.CurrentDomain.BaseDirectory + /*Path.*/DirectorySeparatorChar + "Library" + /*Path.*/DirectorySeparatorChar + orgModuleFileName;
		//			fileName = AddFileExtensionIfNeeded(fileName);
		//			if (!File_Exists(fileName))
					{
						// try environment variable FUELPATH
						//string envPath = Environment.GetEnvironmentVariable("FUELPATH");
						char * envPath = getenv("FUELPATH");
						if (envPath != null)
						{
							fileName = envPath + /*Path.*/DirectorySeparatorChar + orgModuleFileName;
							fileName = AddFileExtensionIfNeeded(fileName);
						}
					}
				}
			}
		}
		if (File_Exists(fileName))
		{
			code = ReadFileOrEmptyString(fileName);
		}
		else
		{
			// use std lib of fuel from builtin resources
			if (orgModuleFileName == "fuellib")
			{
		// TODO		code = Encoding.UTF8.GetString(Properties.Resources.fuellib);
			}
			else
			{
				scope->GlobalScope->Output->WriteLine("WARNING: Library {0} not found! Tried path {1}", orgModuleFileName, fileName);
			}
		}
		if (!string::IsNullOrEmpty(code))
		{
			var importScope = std::make_shared<LispScope>("import " + fileName, scope->GlobalScope, std::make_shared<string>(fileName), scope->Output, scope->Input);
			scope->PushNextScope(importScope);

			result = Lisp::Eval(code, importScope, fileName);

			// add new module to modules scope
			(*(((*(scope->GlobalScope))[LispEnvironment::Modules])->GetLispScopeRef()))[fileName] = std::make_shared<object>(*importScope); // .Add(fileName, importScope);

			scope->PopNextScope();
		}
	}
	return result;
}

static std::shared_ptr<LispVariant> Nop(const std::vector<std::shared_ptr<object>> & args, std::shared_ptr<LispScope> scope)
{
	return std::make_shared<LispVariant>(LispVariant());
}

static std::shared_ptr<LispVariant> Return(const std::vector<std::shared_ptr<object>> & args, std::shared_ptr<LispScope> scope)
{
	return std::make_shared<LispVariant>(LispVariant(args[0]));
}

static std::shared_ptr<LispVariant> GetType(const std::vector<std::shared_ptr<object>> & args, std::shared_ptr<LispScope> scope)
{
	CheckArgs("type", 1, args, scope);

	var item = ((LispVariant)args[0]);
	return std::make_shared<LispVariant>(std::make_shared<object>((int)item.Type));
}

static std::shared_ptr<LispVariant> GetTypeString(const std::vector<std::shared_ptr<object>> & args, std::shared_ptr<LispScope> scope)
{
	CheckArgs("typestr", 1, args, scope);

	var item = ((LispVariant)args[0]);
	return std::make_shared<LispVariant>(std::make_shared<object>(item.TypeString()));
}

static std::shared_ptr<LispVariant> Print(const std::vector<std::shared_ptr<object>> & args, std::shared_ptr<LispScope> scope)
{
	var text = GetStringRepresentation(args, scope);
	scope->GlobalScope->Output->Write(text);
	return std::make_shared<LispVariant>(std::make_shared<object>(text));
}

static std::shared_ptr<LispVariant> PrintLn(const std::vector<std::shared_ptr<object>> & args, std::shared_ptr<LispScope> scope)
{
	var text = GetStringRepresentation(args, scope);
	scope->GlobalScope->Output->WriteLine(text);
	return std::make_shared<LispVariant>(std::make_shared<object>(text));
}

static std::shared_ptr<LispVariant> ArithmetricOperation(const std::vector<std::shared_ptr<object>> & args, std::function<std::shared_ptr<LispVariant>(std::shared_ptr<LispVariant>, std::shared_ptr<LispVariant>)> op)
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

static std::shared_ptr<LispVariant> CompareOperation(const std::vector<std::shared_ptr<object>> & args, std::function<std::shared_ptr<LispVariant>(std::shared_ptr<LispVariant>, std::shared_ptr<LispVariant>)> op, std::shared_ptr<LispScope> scope)
{
	CheckArgs("compare-op", 2, args, scope);

	var arg1 = args[0]->ToLispVariant();
	var arg2 = args[1]->ToLispVariant();
	std::shared_ptr<LispVariant> result = op(arg1, arg2);
	return result;
}

static std::shared_ptr<LispVariant> Addition(const std::vector<std::shared_ptr<object>> & args, std::shared_ptr<LispScope> scope)
{
	return ArithmetricOperation(args, [](std::shared_ptr<LispVariant> l, std::shared_ptr<LispVariant> r) -> std::shared_ptr<LispVariant> { return std::make_shared<LispVariant>(*l + *r); });
}

static std::shared_ptr<LispVariant> Subtraction(const std::vector<std::shared_ptr<object>> & args, std::shared_ptr<LispScope> scope)
{
	return ArithmetricOperation(args, [](std::shared_ptr<LispVariant> l, std::shared_ptr<LispVariant> r) -> std::shared_ptr<LispVariant> { return std::make_shared<LispVariant>(*l - *r); });
}

static std::shared_ptr<LispVariant> Multiplication(const std::vector<std::shared_ptr<object>> & args, std::shared_ptr<LispScope> scope)
{
	return ArithmetricOperation(args, [](std::shared_ptr<LispVariant> l, std::shared_ptr<LispVariant> r) -> std::shared_ptr<LispVariant> { return std::make_shared<LispVariant>(*l * *r); });
}

static std::shared_ptr<LispVariant> Division(const std::vector<std::shared_ptr<object>> & args, std::shared_ptr<LispScope> scope)
{
	return ArithmetricOperation(args, [](std::shared_ptr<LispVariant> l, std::shared_ptr<LispVariant> r) -> std::shared_ptr<LispVariant> { return std::make_shared<LispVariant>(*l / *r); });
}

static std::shared_ptr<LispVariant> Not(const std::vector<std::shared_ptr<object>> & args, std::shared_ptr<LispScope> scope)
{
	CheckArgs("not", 1, args, scope);

	var arg1 = (LispVariant)args[0];
	return std::make_shared<LispVariant>(std::make_shared<object>(!arg1.BoolValue()));
}

static std::shared_ptr<LispVariant> LessTest(const std::vector<std::shared_ptr<object>> & args, std::shared_ptr<LispScope> scope)
{
	return CompareOperation(args, [](std::shared_ptr<LispVariant> l, std::shared_ptr<LispVariant> r) -> std::shared_ptr<LispVariant> { return std::make_shared<LispVariant>(*l < *r); }, scope);
}

static std::shared_ptr<LispVariant> GreaterTest(const std::vector<std::shared_ptr<object>> & args, std::shared_ptr<LispScope> scope)
{
	return CompareOperation(args, [](std::shared_ptr<LispVariant> l, std::shared_ptr<LispVariant> r) -> std::shared_ptr<LispVariant> { return std::make_shared<LispVariant>(*l > *r); }, scope);
}

static std::shared_ptr<LispVariant> LessEqualTest(const std::vector<std::shared_ptr<object>> & args, std::shared_ptr<LispScope> scope)
{
	return CompareOperation(args, [](std::shared_ptr<LispVariant> l, std::shared_ptr<LispVariant> r) -> std::shared_ptr<LispVariant> { return std::make_shared<LispVariant>(*l <= *r); }, scope);
}

static std::shared_ptr<LispVariant> GreaterEqualTest(const std::vector<std::shared_ptr<object>> & args, std::shared_ptr<LispScope> scope)
{
	return CompareOperation(args, [](std::shared_ptr<LispVariant> l, std::shared_ptr<LispVariant> r) -> std::shared_ptr<LispVariant> { return std::make_shared<LispVariant>(*l >= *r); }, scope);
}

static std::shared_ptr<LispVariant> EqualTest(const std::vector<std::shared_ptr<object>> & args, std::shared_ptr<LispScope> scope)
{
	return CompareOperation(args, [](std::shared_ptr<LispVariant> l, std::shared_ptr<LispVariant> r) -> std::shared_ptr<LispVariant> { return std::make_shared<LispVariant>(std::make_shared<object>(LispVariant::EqualOp(*l, *r))); }, scope);
}

static std::shared_ptr<LispVariant> NotEqualTest(const std::vector<std::shared_ptr<object>> & args, std::shared_ptr<LispScope> scope)
{
	return CompareOperation(args, [](std::shared_ptr<LispVariant> l, std::shared_ptr<LispVariant> r) -> std::shared_ptr<LispVariant> { return std::make_shared<LispVariant>(std::make_shared<object>(!LispVariant::EqualOp(*l, *r))); }, scope);
}

static std::shared_ptr<LispVariant> CreateList(const std::vector<std::shared_ptr<object>> & args, std::shared_ptr<LispScope> scope)
{
	var list = IEnumerable<std::shared_ptr<object>>();
	for(var arg : args)
	{
		list.Add(arg);
	}
	var result = std::make_shared<LispVariant>(LispType::_List, std::make_shared<object>(list));
	return result;
}

static std::shared_ptr<LispVariant> Map(const std::vector<std::shared_ptr<object>> & args, std::shared_ptr<LispScope> scope)
{
	CheckArgs(MapFcn, 2, args, scope);

	var function = CheckForFunction(MapFcn, args[0], scope)->FunctionValue();
	var elements = LispEnvironment::CheckForList(MapFcn, args[1], scope);

	var list = IEnumerable<std::shared_ptr<object>>();
	for(var elem : *elements)
	{
		// call for every element the given function (args[0])
		std::vector<std::shared_ptr<object>> args;
		args.push_back(std::make_shared<object>(*elem));
		list.Add(std::make_shared<object>(*(function.Function(args, scope))));
	}
	var result = std::make_shared<LispVariant>(LispType::_List, std::make_shared<object>(list));
	return result;
}

static std::shared_ptr<LispVariant> Reduce(const std::vector<std::shared_ptr<object>> & args, std::shared_ptr<LispScope> scope)
{
	CheckArgs(ReduceFcn, 3, args, scope);

	var function = CheckForFunction(ReduceFcn, args[0], scope)->FunctionValue();
	var elements = LispEnvironment::CheckForList(ReduceFcn, args[1], scope);

	var start = args[2]->ToLispVariant();
	var result = std::make_shared<LispVariant>(*start);
	for(var elem : *elements)
	{
		// call for every element the given function (args[0])
		std::vector<std::shared_ptr<object>> args;
		args.push_back(std::make_shared<object>(*elem));
		args.push_back(std::make_shared<object>(*result));		
		var x = *(function.Function(args, scope));
		result = std::make_shared<LispVariant>(x);
	}
	return result;
}

static std::shared_ptr<LispVariant> Cons(const std::vector<std::shared_ptr<object>> & args, std::shared_ptr<LispScope> scope)
{
	var list = IEnumerable<std::shared_ptr<object>>();
	if (args.size() > 0)
	{
		list.Add(args[0]);
	}
	if (args.size() > 1)
	{
		var item2 = args[1]->ToLispVariant();
		if (item2->IsList())
		{
			std::shared_ptr<IEnumerable<std::shared_ptr<object>>> l = item2->ListValue();
			for(var item : *l)
			{
				list.Add(item);
			}
		}
		else
		{
			list.Add(args[1]);
		}
	}
	var result = std::make_shared<LispVariant>(LispType::_List, std::make_shared<object>(list));
	return result;
}

static std::shared_ptr<LispVariant> Length(const std::vector<std::shared_ptr<object>> & args, std::shared_ptr<LispScope> scope)
{
	CheckArgs("len", 1, args, scope);

	var elements = *(args[0]->ToLispVariant()->ListValue());
	return std::make_shared<LispVariant>(std::make_shared<object>((int)elements.Count()));
}

static std::shared_ptr<LispVariant> First(const std::vector<std::shared_ptr<object>> & args, std::shared_ptr<LispScope> scope)
{
	CheckArgs("first", 1, args, scope);

	var elements = *(args[0]->ToLispVariant()->ListValue());
	return std::make_shared<LispVariant>(std::make_shared<object>(*(elements.First())));
}

static std::shared_ptr<LispVariant> Rest(const std::vector<std::shared_ptr<object>> & args, std::shared_ptr<LispScope> scope)
{
	CheckArgs("rest", 1, args, scope);

	var elements = *(args[0]->ToLispVariant()->ListValue());
	return std::make_shared<LispVariant>(std::make_shared<object>(elements.Skip(1)));
}

static std::shared_ptr<LispVariant> Nth(const std::vector<std::shared_ptr<object>> & args, std::shared_ptr<LispScope> scope)
{
	CheckArgs("nth", 2, args, scope);

	var index = args[0]->ToLispVariant()->IntValue();
	var elements = args[1]->ToLispVariant()->ListValue();
	var val = elements->ElementAt(index);
	return std::make_shared<LispVariant>(std::make_shared<object>(*val));
}

static std::shared_ptr<LispVariant> Append(const std::vector<std::shared_ptr<object>> & args, std::shared_ptr<LispScope> scope)
{
	var list = IEnumerable<std::shared_ptr<object>>();
	for(var listElement : args)
	{
		var lst = listElement->ToLispVariant()->ListValue();
		for(var item : *lst)
		{
			list.Add(item);
		}
	}
	var result = std::make_shared<LispVariant>(LispType::_List, std::make_shared<object>(list));
	return result;
}

static std::shared_ptr<LispVariant> SymbolFcn(const std::vector<std::shared_ptr<object>> & args, std::shared_ptr<LispScope> scope)
{
	CheckArgs(LispEnvironment::Sym, 1, args, scope);

	var symbol = args[0]->ToString();
	return std::make_shared<LispVariant>(LispType::_Symbol, std::make_shared<object>(symbol));
}

static std::shared_ptr<LispVariant> ConvertToString(const std::vector<std::shared_ptr<object>> & args, std::shared_ptr<LispScope> scope)
{
	CheckArgs(LispEnvironment::Str, 1, args, scope);

	var value = args[0]->ToString();
	// convert native object into a readable form
	// used for: (println (str nativeLst))
	if (args[0]->IsLispVariant())
	{
		var variant = args[0]->ToLispVariant();
		if (variant->IsNativeObject())
		{
			value = variant->NativeObjectStringRepresentation();
		}
	}
	return std::make_shared<LispVariant>(LispType::_String, std::make_shared<object>(value));
}

static std::shared_ptr<LispVariant> ArgsCountFcn(const std::vector<std::shared_ptr<object>> & args, std::shared_ptr<LispScope> scope)
{
	CheckArgs(LispEnvironment::Apply, 0, args, scope);

	size_t cnt = (*scope)[ArgsMeta]->ToList()->Count();
	return std::make_shared<LispVariant>(std::make_shared<object>((int)cnt));
}

static std::shared_ptr<LispVariant> ArgsFcn(const std::vector<std::shared_ptr<object>> & args, std::shared_ptr<LispScope> scope)
{
	CheckArgs(LispEnvironment::Apply, 1, args, scope);

	var index = args[0]->ToLispVariant()->IntValue();
	var array = (*scope)[ArgsMeta]->ToList()->ToArray();
	if (index >= 0 && index < (int)array.size())
	{
		return std::make_shared<LispVariant>(array[index]);
	}
	throw LispException(string::Format("Index out of range in args function (index={0} max={1})", std::to_string(index), std::to_string((int)array.size())));
}

static std::shared_ptr<LispVariant> ApplyFcn(const std::vector<std::shared_ptr<object>> & args, std::shared_ptr<LispScope> scope)
{
	CheckArgs(LispEnvironment::Apply, 2, args, scope);

	var fcn = LispInterpreter::EvalAst(args[0], scope);

	var arguments = args[1]->ToLispVariant();

	if (arguments->IsList())
	{
		var argumentsArray = arguments->ListValue()->ToArray();
		var result = fcn->FunctionValue().Function(argumentsArray, scope);
		return result;
	}

	throw LispException("Expected list as arguments in apply", scope.get());
}

static std::shared_ptr<LispVariant> EvalFcn(const std::vector<std::shared_ptr<object>> & args, std::shared_ptr<LispScope> scope)
{
	CheckArgs("eval", 1, args, scope);

	std::shared_ptr<LispVariant> result;
	// convert LispVariant.List --> object[] needed for evaluation
	var variant = args[0]->ToLispVariant();
	if (variant->IsList())
	{
		std::shared_ptr<object> code = std::make_shared<object>(*(variant->ListValue()));
		result = LispInterpreter::EvalAst(code, scope);
	}
	else
	{
		// if a single value is given for evaluation --> just return value !
		result = variant;
	}
	return result;
}

static std::shared_ptr<LispVariant> EvalStrFcn(const std::vector<std::shared_ptr<object>> & args, std::shared_ptr<LispScope> scope)
{
	CheckArgs("evalstr", 1, args, scope);

	var variant = args[0]->ToLispVariant();
	var result = Lisp::Eval(variant->Value->ToString(), scope, scope->ModuleName);
	return result;
}

static std::shared_ptr<LispVariant> bool_operation_form(const std::vector<std::shared_ptr<object>> & args, std::shared_ptr<LispScope> scope, std::function<bool(bool, bool)> func, bool initial)
{
	var result = initial;
	for(var arg : args)
	{
		bool value = LispInterpreter::EvalAst(arg, scope)->BoolValue();
		result = func(result, value);
		if (!result)
		{
			break;
		}
	}
	return std::make_shared<LispVariant>(std::make_shared<object>(result));
}

static std::shared_ptr<LispVariant> and_form(const std::vector<std::shared_ptr<object>> & args, std::shared_ptr<LispScope> scope)
{
	return bool_operation_form(args, scope, [](bool r, bool v) -> bool { return r && v; }, true);
}

static std::shared_ptr<LispVariant> or_form(const std::vector<std::shared_ptr<object>> & args, std::shared_ptr<LispScope> scope)
{
	return bool_operation_form(args, scope, [](bool r, bool v) -> bool { return r || v; }, false);
}

static std::shared_ptr<LispVariant> def_form(const std::vector<std::shared_ptr<object>> & args, std::shared_ptr<LispScope> scope)
{
	return def_form_helper(args, scope, Def, scope);
}

static std::shared_ptr<LispVariant> gdef_form(const std::vector<std::shared_ptr<object>> & args, std::shared_ptr<LispScope> scope)
{
	return def_form_helper(args, scope, Gdef, scope->GlobalScope);
}

static std::shared_ptr<LispVariant> setf_form(const std::vector<std::shared_ptr<object>> & args, std::shared_ptr<LispScope> scope)
{
	CheckArgs(Setf, 2, args, scope);

	var symbol = EvalArgIfNeeded(args[0], scope);
	var symbolName = symbol != null ? symbol->ToString() : /*null*/string::Empty;
	var value = LispInterpreter::EvalAst(args[1], scope);
	scope->SetInScopes(symbolName, std::make_shared<object>(*value));
	return value;
}

static std::shared_ptr<LispVariant> definemacroevaluate_form(const std::vector<std::shared_ptr<object>> & args, std::shared_ptr<LispScope> scope)
{
	CheckArgs(DefineMacroEval, 3, args, scope);

	auto dict = (*(scope->GlobalScope));
	if (dict.find(LispEnvironment::Macros) != dict.end())
	{
		var macros = dict[LispEnvironment::Macros]->GetLispScopeRef() /*as LispScope*/;
		if (macros != null)
		{
			(*macros)[args[0]->ToString()] = std::make_shared<object>(LispMacroRuntimeEvaluate(std::make_shared<IEnumerable<std::shared_ptr<object>>>(args[1]->ToEnumerableOfObject()), std::make_shared<IEnumerable<std::shared_ptr<object>>>(args[2]->ToEnumerableOfObject())));
		}
	}

	return null;
}

#ifdef ENABLE_COMPILE_TIME_MACROS 

// (define-macro-expand name (args) (expression))
static std::shared_ptr<LispVariant> definemacroexpand_form(const std::vector<std::shared_ptr<object>> & args, std::shared_ptr<LispScope> scope)
{
	CheckArgs(DefineMacroExpand, 3, args, scope);

	var macros = ((*(scope->GlobalScope))[LispEnvironment::Macros])->GetLispScopeRef();
	if (macros != null)
	{
		// allow macros in macros --> recursive call for ExpandMacros()
		std::shared_ptr<object> result = LispInterpreter::ExpandMacros(std::make_shared<object>(*LispEnvironment::GetExpression(args[2])), scope);
		(*macros)[args[0]->ToString()] = std::make_shared<object>(LispMacroCompileTimeExpand(LispEnvironment::GetExpression(args[1]), std::make_shared<IEnumerable<std::shared_ptr<object>>>(result->ToEnumerableOfObject())));
	}

	return null;
}

#endif

static std::shared_ptr<LispVariant> quote_form(const std::vector<std::shared_ptr<object>> & args, std::shared_ptr<LispScope> scope)
{
	CheckArgs(Quote, 1, args, scope);

	return std::make_shared<LispVariant>(std::make_shared<object>(*(args[0])));
}

std::shared_ptr<object> UnQuoteIfNeeded(std::shared_ptr<object> item, bool & isSplicing, std::shared_ptr<LispScope> scope)
{
	var value = item->ToLispVariant(); // as LispVariant
	isSplicing = false;
	if (value != null)
	{
		if (value->IsUnQuoted == LispUnQuoteModus::_UnQuote)
		{
			return (*scope)[value->StringValue()];
		}
		if (value->IsUnQuoted == LispUnQuoteModus::_UnQuoteSplicing)
		{
			isSplicing = true;
			return (*scope)[value->StringValue()];
		}
	}
	return item;
}

static std::shared_ptr<IEnumerable<std::shared_ptr<object>>> ToEnumerable(std::shared_ptr<object> obj)
{
	if (obj->IsIEnumerableOfObject() /*enumerable != null*/ || obj->IsList())
	{
		var enumerable = obj->ToEnumerableOfObject() /*as IEnumerable<object>*/;
		return std::make_shared<IEnumerable<std::shared_ptr<object>>>(enumerable);
	}
	if (obj->IsLispVariant())
	{
		var variant = obj->ToLispVariant() /*as LispVariant*/;
		if (variant->IsList())
		{
			return std::make_shared<IEnumerable<std::shared_ptr<object>>>(*(variant->ListValue()));
		}
		else
		{
			return std::make_shared<IEnumerable<std::shared_ptr<object>>>(IEnumerable<std::shared_ptr<object>>());
		}
	}
	else
	{
		return std::make_shared<IEnumerable<std::shared_ptr<object>>>(IEnumerable<std::shared_ptr<object>>());
	}
}

std::shared_ptr<LispVariant> LispEnvironment::quasiquote_form(const std::vector<std::shared_ptr<object>> & args, std::shared_ptr<LispScope> scope)
{
	CheckArgs(LispEnvironment::Quasiquote, 1, args, scope);

	// unquote elements of list if needed
	var lst = GetExpression(args[0]);
	IEnumerable<std::shared_ptr<object>> ret;
	for(var elem : *lst)
	{
		bool isSplicing;
		std::shared_ptr<object> item = UnQuoteIfNeeded(elem, /*out*/ isSplicing, scope);
		// process unquotesplicing
		std::shared_ptr<IEnumerable<std::shared_ptr<object>>> sublst = ToEnumerable(item);
		if (isSplicing && sublst != null)
		{
			for(var subitem : *sublst)
			{
				ret.Add(subitem);
			}
		}
		else
		{
			ret.Add(item);
		}
	}
	return std::make_shared<LispVariant>(std::make_shared<object>(ret));
}

static std::shared_ptr<LispVariant> if_form(const std::vector<std::shared_ptr<object>> & args, std::shared_ptr<LispScope> scope)
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

static std::shared_ptr<LispVariant> while_form(const std::vector<std::shared_ptr<object>> & args, std::shared_ptr<LispScope> scope)
{
	CheckArgs(While, 2, args, scope);

	var result = std::make_shared<LispVariant>();
	var condition = LispInterpreter::EvalAst(args[0], scope);
	while (condition->ToBool())
	{
		result = LispInterpreter::EvalAst(args[1], scope);
		condition = LispInterpreter::EvalAst(args[0], scope);
	}
	return result;
}

static std::shared_ptr<LispVariant> do_form(const std::vector<std::shared_ptr<object>> & args, std::shared_ptr<LispScope> scope)
{
	var result = std::make_shared<LispVariant>(LispType::_Undefined);

	for (var statement : args)
	{
		if (!(statement->IsIEnumerableOfObject() || statement->IsList() /*is IEnumerable<object>*/))
		{
			throw LispException("List expected in do", /*((LispVariant)statement).Token*/statement->ToLispVariant()->Token, scope->ModuleName, scope->DumpStackToString());
		}
		result = LispInterpreter::EvalAst(statement, scope);
	}

	return result;
}

static IEnumerable<std::shared_ptr<object>> VectorToList(const std::vector<std::shared_ptr<object>> & args)
{
	IEnumerable<std::shared_ptr<object>> ret;
	for (auto elem : args)
	{
		ret.push_back(elem);
	}
	return ret;
}

std::shared_ptr<LispVariant> LispEnvironment::fn_form(const std::vector<std::shared_ptr<object>> & args, std::shared_ptr<LispScope> scope)
{
	var name = /*(string)*/scope->UserData.get()!=null ? scope->UserData->ToString() : "";
	var moduleName = scope->ModuleName;
	var userDoc = scope->UserDoc;
	var signature = userDoc.get() != null ? userDoc->Item1() : string::Empty/*null*/;
	var documentation = userDoc.get() != null ? userDoc->Item2() : string::Empty/*null*/;

	std::function<std::shared_ptr<LispVariant>(const std::vector<std::shared_ptr<object>> &, std::shared_ptr<LispScope>)> fcn = 
		[name, moduleName, args, scope](const std::vector<std::shared_ptr<object>> & localArgs, std::shared_ptr<LispScope> localScope) -> std::shared_ptr<LispVariant>
	{
		var childScope = std::make_shared<LispScope>(name, localScope->GlobalScope, std::make_shared<string>(moduleName), scope->Output, scope->Input);
		localScope->PushNextScope(childScope);

		// add formal arguments to current scope
		var i = 0;
		var formalArgs = (args[0]->IsLispVariant() /*is LispVariant*/ ? args[0]->ToLispVariant()->ListValue() : GetExpression(args[0]))->ToArray();
		
		if (formalArgs.size() > localArgs.size())
		{
			throw LispException("Invalid number of arguments");
		}

		for(var arg : formalArgs)
		{
			(*childScope)[arg->ToString()] = localArgs[i];
			i++;
		}

		// support args function for accessing all given parameters
		(*childScope)[ArgsMeta] = std::make_shared<object>(IEnumerable<std::shared_ptr<object>>(VectorToList(localArgs)));
		size_t formalArgsCount = formalArgs.size();
		if ((int)localArgs.size() > formalArgsCount)
		{
			//var additionalArgs = new object[localArgs.size() - formalArgsCount];
			std::vector<std::shared_ptr<object>> additionalArgs(localArgs.size() - formalArgsCount);
			for (size_t n = 0; n < localArgs.size() - formalArgsCount; n++)
			{
				additionalArgs[n] = localArgs[n + formalArgsCount];
			}
			(*childScope)[AdditionalArgs] = std::make_shared<object>(LispVariant(std::make_shared<object>(VectorToList(additionalArgs))));
		}

		// save the current call stack to resolve variables in closures
		childScope->ClosureChain = scope;

		std::shared_ptr<LispVariant> ret;
		try
		{
			ret = LispInterpreter:: EvalAst(args[1], childScope);
		}
		catch (LispStopDebuggerException & exc)
		{
			// forward a debugger stop exception to stop the debugger loop
			throw exc;
		}
		catch (LispException & ex)
		{
			// add the stack info and module name to the data of the exception
// TODO --> implement for debugger
//			ex.AddModuleNameAndStackInfos(childScope.ModuleName, childScope.DumpStackToString());
//			ex.AddTokenInfos(childScope.CurrentToken);

			var debugger = scope->GlobalScope->Debugger;
			if (debugger != null)
			{
				scope->GlobalScope->Output->WriteLine(ex.ToString());

				debugger->InteractiveLoop(/*initialTopScope: */childScope, /*currentAst :*/ /*(IEnumerable<object>)*/std::make_shared<IEnumerable<std::shared_ptr<object>>>(args[1]->ToEnumerableOfObject()) /*new List<object> { info.Item2 }*/);
			}

			throw ex;
		}
		localScope->PopNextScope();
		return ret;
	};

	return std::make_shared<LispVariant>(CreateFunction(fcn, signature, documentation, /*isBuiltin:*/ false, /*isSpecialForm:*/ false,/*isEvalInExpand: */ false, /*moduleName :*/ scope->ModuleName));
}

// returns token just before the defn statement:
// item is fcn token, go three tokens before, example:
// ; comment before defn
// (defn fcn (x) (+ x 1))
// --> Comment Token
static std::shared_ptr<LispToken> GetTokenBeforeDefn(std::shared_ptr<object> item, std::shared_ptr<LispScope> scope)
{
	if (item->IsLispVariant() /*is LispVariant*/)
	{
		std::shared_ptr<LispVariant> tokenName = item->ToLispVariant();
		std::shared_ptr<LispToken> token1 = scope->GetPreviousToken(tokenName->Token);
		std::shared_ptr<LispToken> token2 = scope->GetPreviousToken(token1);
		std::shared_ptr<LispToken> token3 = scope->GetPreviousToken(token2);
		return token3;
	}
	return null;
}

static string GetFormalArgsAsString(std::shared_ptr<object> args)
{
	string result = string::Empty;
	IEnumerable<std::shared_ptr<object>> theArgs = args->ToEnumerableOfObject();
	for(var s : theArgs)
	{
		if (result.size() > 0)
		{
			result += " ";
		}
		result += s->ToString();
	}
	return result;
}

static string GetSignatureFromArgs(std::shared_ptr<object> arg0, const string & name)
{
	string signature = "(" + name;
	string formalArgsAsString = GetFormalArgsAsString(arg0);
	if (formalArgsAsString.size() > 0)
	{
		signature += " ";
	}
	signature += formalArgsAsString;
	signature += ")";
	return signature;
}

static void UpdateDocumentationInformationAtScope(const std::vector<std::shared_ptr<object>> & args, std::shared_ptr<LispScope> scope)
{
	var documentation = string::Empty;
	var token = GetTokenBeforeDefn(args[0], scope);
	if ((token.get() != null) && (token->Type == LispTokenType::Comment))
	{
		documentation = token->Value->ToString();
	}
	var signature = GetSignatureFromArgs(args[1], args[0]->ToString());
	scope->UserDoc = std::make_shared<Tuple<string, string>>(signature, documentation);
}

static std::shared_ptr<LispVariant> defn_form_helper(const std::vector<std::shared_ptr<object>> & args, std::shared_ptr<LispScope> scope, const string & name)
{
	CheckArgs(name, 3, args, scope);

	UpdateDocumentationInformationAtScope(args, scope);

	var fn = ((*(scope->GlobalScope))[Fn])->ToLispVariant()->FunctionValue();
	scope->UserData = std::make_shared<object>(EvalArgIfNeeded(args[0], scope)->ToString());

	std::vector<std::shared_ptr<object>> tempArgs;
	tempArgs.push_back(args[1]);
	tempArgs.push_back(args[2]);
	var resultingFcn = fn.Function(tempArgs, scope);
	scope->UserData = null;

	var defFcn = ((*(scope->GlobalScope))[name])->ToLispVariant()->FunctionValue();
	std::vector<std::shared_ptr<object>> tempArgs2;
	tempArgs2.push_back(args[0]);
	tempArgs2.push_back(std::make_shared<object>(*resultingFcn));
	return defFcn.Function(tempArgs2, scope);
}

static std::shared_ptr<LispVariant> defn_form(const std::vector<std::shared_ptr<object>> & args, std::shared_ptr<LispScope> scope)
{
	return defn_form_helper(args, scope, Def);
}

static std::shared_ptr<LispVariant> gdefn_form(const std::vector<std::shared_ptr<object>> & args, std::shared_ptr<LispScope> scope)
{
	return defn_form_helper(args, scope, Gdef);
}

static std::shared_ptr<LispVariant> CallStaticNative(const std::vector<std::shared_ptr<object>> & args, std::shared_ptr<LispScope> scope)
{
	var className = args[0]->ToLispVariant();
	var methodName = args.size() > 1 ? args[1]->ToString() : string::Empty;

	if (className->IsString() || className->IsSymbol())
	{
		var callArgs = GetCallArgs(args);

// TODO --> implement
		//Type nativeClass = Type.GetType(className.ToString());
		//if (nativeClass != null)
		//{
		//	MethodInfo method;
		//	try
		//	{
		//		method = nativeClass.GetMethod(methodName);
		//	}
		//	catch (AmbiguousMatchException)
		//	{
		//		// process overloaded methods, try to resolve method via types of given arguments
		//		// example: Math-Abs, File-Exits, ...
		//		var callArgsTypes = GetTypes(callArgs);
		//		method = nativeClass.GetMethod(methodName, callArgsTypes);
		//	}
		//	if (method != null)
		//	{
		//		ParameterInfo[] parameterInfos = method.GetParameters();
		//		object result = method.Invoke(null, ConvertAllToNative(callArgs, parameterInfos));
		//		return new LispVariant(result);
		//	}
		//}
	}
	//throw LispException("Bad static method " + methodName + " for class " + className, scope);
}

// ************************************************************************

bool LispEnvironment::FindFunctionInModules(const string & funcName, std::shared_ptr<LispScope> scope, std::shared_ptr<object> & foundValue)
{
	foundValue = null;

	std::shared_ptr<object> importedModules = (*(scope->GlobalScope))[LispEnvironment::Modules];
	if (importedModules.get() != null)
	{
		auto tempScope = importedModules->GetLispScopeRef();
		for (/*KeyValuePair*/std::pair<string, std::shared_ptr<object>> kv : *tempScope)
		{
			var module = /*(LispScope)*/kv.second->GetLispScopeRef();
			if (module->ContainsKey(funcName))
			{
				foundValue = (*module)[funcName];
				return true;
			}
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

static bool ExistsItem(std::shared_ptr<object> funcName, std::shared_ptr<LispScope> scope, const string & key)
{
    if (scope.get() != null &&
        scope->ContainsKey(key))
    {
        return (*scope)[key]->GetLispScopeRef()->ContainsKey(funcName->ToString());
    }
    return false;
}

bool LispEnvironment::IsMacro(std::shared_ptr<object> funcName, std::shared_ptr<LispScope> scope)
{
    return ExistsItem(funcName, scope, LispEnvironment::Macros);
}

std::shared_ptr<object> LispEnvironment::GetMacro(std::shared_ptr<object> funcName, std::shared_ptr<LispScope> scope)
{
	return QueryItem(funcName, scope, Macros);
}

bool LispEnvironment::IsExpression(std::shared_ptr<object> item)
{
	return (item->IsLispVariant() /*is LispVariant*/ && item->IsList()) ||
           (item->IsIEnumerableOfObject()) || (item->IsList());
}

std::shared_ptr<IEnumerable<std::shared_ptr<object>>> LispEnvironment::GetExpression(std::shared_ptr<object> item)
{
	if (item->IsLispVariant() /*is LispVariant*/ && item->IsList()/*((LispVariant)item).IsList*/)
	{
		return item->ToLispVariant()->ListValue(); // ((LispVariant)item).ListValue;
	}
	if (item->IsIEnumerableOfObject() /*is IEnumerable<object>*/ || item->IsList())
	{
		return item->ToList(); // (IEnumerable<object>)item;
	}
	return std::make_shared<IEnumerable<std::shared_ptr<object>>>(); // new List<object>();
}

std::shared_ptr<IEnumerable<std::shared_ptr<object>>> LispEnvironment::CheckForList(const string & functionName, std::shared_ptr<object> listObj, std::shared_ptr<LispScope> scope)
{
	// TODO --> pruefen...
	if (listObj->IsList() /*is object[]*/)
	{
		return GetExpression(listObj);
	}
	var value = listObj->ToLispVariant();
	if (value->IsNativeObject() && (value->Value->IsList() /*is IEnumerable<object>*/))
	{
		return /*(IEnumerable<object>)*/value->Value->ToList();
	}
	if (!value->IsList())
	{
		throw LispException("No list in " + functionName, scope->GetPreviousToken(((LispVariant)listObj).Token), scope->ModuleName, scope->DumpStackToString());
	}
	return value->ListValue();
}

static std::shared_ptr<object> QueryItem(std::shared_ptr<object> funcName, LispScope * scope, const string & key)
{
	if (scope != null &&
		scope->ContainsKey(key) &&
		((*scope)[key])->GetLispScopeRef()->ContainsKey(funcName->ToString()))
	{
		return (*((*scope)[key]->GetLispScopeRef()))[funcName->ToString()];
	}
	return null;
}

/*private static*/ std::shared_ptr<object> LispEnvironment::QueryItem(std::shared_ptr<object> funcName, std::shared_ptr<LispScope> scope, string key)
{
	if (scope != null &&
		scope->ContainsKey(key) &&
		((*scope)[key])->GetLispScopeRef()->ContainsKey(funcName->ToString()))
	{
		return (*((*scope)[key])->GetLispScopeRef())[funcName->ToString()];
	}
	return null;
}

std::shared_ptr<LispVariant> FileExits(const std::vector<std::shared_ptr<object>> & args, std::shared_ptr<LispScope> scope)
{
	CheckArgs("File-Exits", 1, args, scope);

	var fileName = args[0]->ToLispVariant()->ToString();
	return std::make_shared<LispVariant>(std::make_shared<object>(File_Exists(fileName)));
}

std::shared_ptr<LispVariant> FileReadAllText(const std::vector<std::shared_ptr<object>> & args, std::shared_ptr<LispScope> scope)
{
	CheckArgs("File-ReadAllText", 1, args, scope);

	var fileName = args[0]->ToLispVariant()->ToString();
	return std::make_shared<LispVariant>(std::make_shared<object>(ReadFileOrEmptyString(fileName)));
}

std::shared_ptr<LispVariant> FileWriteAllText(const std::vector<std::shared_ptr<object>> & args, std::shared_ptr<LispScope> scope)
{
	CheckArgs("File-WriteAllText", 2, args, scope);

	var fileName = args[0]->ToLispVariant()->ToString();
	var content = args[1]->ToLispVariant()->ToString();
	return std::make_shared<LispVariant>(std::make_shared<object>(WriteTextFile(fileName, content)));
}

std::shared_ptr<LispScope> LispEnvironment::CreateDefaultScope()
{
	std::shared_ptr<LispScope> scope = std::make_shared<LispScope>(MainScope);

	(*scope)[Modules] = std::make_shared<object>(LispScope(Modules, scope));
	(*scope)[Macros] = std::make_shared<object>(LispScope(Macros, scope));
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
    (*scope)["import"] = CreateFunction(Import, "(import module1 ...)", "Imports modules with fuel code.");
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
	(*scope)["type"] = CreateFunction(GetType, "(type expr)", "Returns the type id of the value of the expression.");
	(*scope)["typestr"] = CreateFunction(GetTypeString, "(typestr expr)", "Returns a readable string representing the type of the value of the expression.");
	(*scope)["nop"] = CreateFunction(Nop, "(nop)", "Does nothing (no operation).");
	(*scope)["return"] = CreateFunction(Return, "(return expr)", "Returns the value of the expression and quits the function.");
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
    (*scope)[Sym] = CreateFunction(SymbolFcn, "(sym expr)", "Returns the evaluated expression as symbol.");
	(*scope)[Str] = CreateFunction(ConvertToString, "(str expr)", "Returns the evaluated expression as string.");

	(*scope)[ArgsCount] = CreateFunction(ArgsCountFcn, "(argscount)", "Returns the number of command line arguments for this script.");
	(*scope)[Args] = CreateFunction(ArgsFcn, "(args number)", "Returns the [number] command line argument for this script.");
	(*scope)[Apply] = CreateFunction(ApplyFcn, "(apply function arguments-list)", "Calls the function with the arguments.");
	(*scope)[Eval] = CreateFunction(EvalFcn, "(eval ast)", "Evaluates the abstract syntax tree (ast).");
	(*scope)[EvalStr] = CreateFunction(EvalStrFcn, "(evalstr string)", "Evaluates the string.");

	// special forms
	(*scope)[And] = CreateFunction(and_form, "(and expr1 expr2 ...)", "And operator with short cut.", /*isBuiltin:*/true, /*isSpecialForm:*/ true);
	(*scope)[Or] = CreateFunction(or_form, "(or expr1 expr2 ...)", "Or operator with short cut.", /*isBuiltin:*/true, /*isSpecialForm:*/ true);
	(*scope)[Def] = CreateFunction(def_form, "(def symbol expression)", "Creates a new variable with name of symbol in current scope. Evaluates expression and sets the value of the expression as the value of the symbol.", /*isBuiltin:*/true, /*isSpecialForm:*/ true);
	(*scope)[Gdef] = CreateFunction(gdef_form, "(gdef symbol expression)", "Creates a new variable with name of symbol in global scope. Evaluates expression and sets the value of the expression as the value of the symbol.", /*isBuiltin:*/true, /*isSpecialForm:*/ true);
	(*scope)[Setf] = CreateFunction(setf_form, "(setf symbol expression)", "Evaluates expression and sets the value of the expression as the value of the symbol.", /*isBuiltin:*/true, /*isSpecialForm:*/ true);

	// macros are:
	// a special form to control evaluation of function parameters inside the macro code
	// there are two options possible:
	//  - run time evaluation of macros
	//  - compile time replacement/expanding of macros
	(*scope)[DefineMacro] = CreateFunction(definemacroevaluate_form, "(define-macro name (arguments) statement)", "see: define-macro-eval", /*isBuiltin:*/true, /*isSpecialForm:*/ true);
	// run time evaluation for macros:
	(*scope)[DefineMacroEval] = CreateFunction(definemacroevaluate_form, "(define-macro-eval name (arguments) statement)", "Special form: Defines a macro which will be evaluated at run time.", /*isBuiltin:*/true, /*isSpecialForm:*/ true);
#ifdef ENABLE_COMPILE_TIME_MACROS
	// compile time expand for macros:
	(*scope)[DefineMacroExpand] = CreateFunction(definemacroexpand_form, "(define-macro-expand name (arguments) statement)", "Special form: Defines a macro which will be evaluated at compile time.", /*isSpecialForm:*/ true, /*isSpecialForm:*/ false, /*isEvalInExpand:*/ true);
#endif

	(*scope)[Quote] = CreateFunction(quote_form, "(quote expr)", "Returns expression without evaluating it.", /*isBuiltin:*/true, /*isSpecialForm:*/ true);
	(*scope)[Quasiquote] = CreateFunction(quasiquote_form, "(quasiquote expr)", "Returns expression without evaluating it, but processes evaluation operators , and ,@.", /*isBuiltin:*/true, /*isSpecialForm:*/ true);
	(*scope)[If] = CreateFunction(if_form, "(if cond then-block [else-block])", "The if statement.", /*isBuiltin:*/true, /*isSpecialForm:*/ true);
	(*scope)[While] = CreateFunction(while_form, "(while cond block)", "The while loop.", /*isBuiltin:*/true, /*isSpecialForm:*/ true);
	(*scope)[Do] = CreateFunction(do_form, "(do statement1 statement2 ...)", "Returns a sequence of statements.", /*isBuiltin:*/true, /*isSpecialForm:*/ true);
	(*scope)[Begin] = CreateFunction(do_form, "(begin statement1 statement2 ...)", "see: do", /*isBuiltin:*/true, /*isSpecialForm:*/ true);
	
	(*scope)[Lambda] = CreateFunction(fn_form, "(lambda (arguments) block)", "Returns a lambda function.", /*isBuiltin:*/true, /*isSpecialForm:*/ true);
	
	(*scope)[Fn] = CreateFunction(fn_form, "(fn (arguments) block)", "Returns a function.", /*isBuiltin:*/true, /*isSpecialForm:*/ true);
	(*scope)[Defn] = CreateFunction(defn_form, "(defn name (args) block)", "Defines a function in the current scope.", /*isBuiltin:*/true, /*isSpecialForm:*/ true);
	(*scope)[Gdefn] = CreateFunction(gdefn_form, "(gdefn name (args) block)", "Defines a function in the global scope.", /*isBuiltin:*/true, /*isSpecialForm:*/ true);	

	(*scope)["do"] = CreateFunction(do_form, "(do statement1 statement2 ...)", "Returns a sequence of statements.", /*isBuiltin:*/true, /*isSpecialForm:*/ true);

	(*scope)[If] = CreateFunction(if_form, "(if cond then-block [else-block])", "The if statement.", /*isBuiltin:*/true, /*isSpecialForm:*/ true);

	// runtime for C++ implementation
	(*scope)["File-Exists"] = CreateFunction(FileExits, "(File-Exists name)", "Returns #t if the file exists, otherwise returns #f.");
	(*scope)["File-ReadAllText"] = CreateFunction(FileReadAllText, "(File-ReadAllText name)", "Returns the content of the file with name.");
	(*scope)["File-WriteAllText"] = CreateFunction(FileWriteAllText, "(File-WriteAllText name content)", "Writes the content to the file with name.");

	scope->PrivateInitForCpp();

	return scope;
}
