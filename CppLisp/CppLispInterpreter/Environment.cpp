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

#include "Scope.h"
#include "Interpreter.h"
#include "Lisp.h"

#include <map>
#include <fstream>

#include <chrono>
#include <thread>

#include <ctime>

using namespace CppLisp;

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
const string Arg = "arg";

/*private*/ const string Builtin = "<builtin>";

/*private*/ const string MainScope = "<main>";

const string LispEnvironment::EvalStrTag = "<evalstr>:";
const string LispEnvironment::MetaTag = /*MetaTag*/"###";
const string LispEnvironment::Macros = LispEnvironment::MetaTag + "macros" + LispEnvironment::MetaTag;
const string LispEnvironment::Modules = LispEnvironment::MetaTag + "modules" + LispEnvironment::MetaTag;

const string LispEnvironment::Apply = "apply";
const string LispEnvironment::Eval = "eval";
const string LispEnvironment::EvalStr = "evalstr";
const string LispEnvironment::Quote = "quote";
const string LispEnvironment::Quasiquote = "quasiquote";   
const string LispEnvironment::UnQuote = "_unquote";
const string LispEnvironment::UnQuoteSplicing = "_unquotesplicing";

const string LispEnvironment::Sym = "sym";
const string LispEnvironment::Str = "str";

// ************************************************************************

namespace CppLisp
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

static string GetStringRepresentation(const std::vector<std::shared_ptr<object>> & args, std::shared_ptr<LispScope> scope, const string & separator = " ")
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
	if (count == (size_t)-1 || args.size() != count)
	{
		throw LispException(string::Format("Bad argument count in {0}, has {1} expected {2}", name, std::to_string(args.size()), std::to_string(count)), scope.get());
	}
}

static void CheckOptionalArgs(const string & name, size_t minCount, size_t maxCount, const std::vector<std::shared_ptr<object>> & args, std::shared_ptr<LispScope> scope)
{
	if ((args.size() < minCount) || (args.size() > maxCount))
	{
		throw LispException(string::Format("Bad argument count in {0}, has {1} expected between {2} and {3}", name, std::to_string(args.size()), std::to_string(minCount), std::to_string(maxCount)), scope.get());
	}
}

/* not needed yet...
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
*/

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

static std::shared_ptr<LispVariant> Fuel(const std::vector<std::shared_ptr<object>> & /*args*/, std::shared_ptr<LispScope> /*scope*/)
{
	string text; // var text = new StringBuilder();
	text.Append(string::Format("fuel version {0} from {1}", Lisp::Version, Lisp::Date));
	return std::make_shared<LispVariant>(std::make_shared<object>(text));
}

static std::shared_ptr<LispVariant> Copyright(const std::vector<std::shared_ptr<object>> & /*args*/, std::shared_ptr<LispScope> /*scope*/)
{
	string text; // var text = new StringBuilder();
	text.Append(string::Format("Copyright: {0} {1}", Lisp::License, Lisp::LicenseUrl));
	return std::make_shared<LispVariant>(std::make_shared<object>(text));
}

static std::shared_ptr<LispVariant> Help(const std::vector<std::shared_ptr<object>> & /*args*/, std::shared_ptr<LispScope> scope)
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
			// search for functions in all loaded modules
			auto tempScope = ((*(scope->GlobalScope))[LispEnvironment::Macros])->GetLispScopeRef();
			for(/*KeyValuePair*/std::pair<string, std::shared_ptr<object>> module : *tempScope)
			{
				help += module.second->GetLispScopeRef()->GetFunctionsHelpFormated(item->ToString(), select);
			}
		}
		return DumpDocumentation(scope, [help, scope]() -> void { scope->GlobalScope->Output->WriteLine("{0}", help); });
	}
	return DumpDocumentation(scope, [scope]() -> void { scope->GlobalScope->DumpBuiltinFunctionsHelpFormated(); });
}

static std::shared_ptr<LispVariant> HtmlDocumentation(const std::vector<std::shared_ptr<object>> & /*args*/, std::shared_ptr<LispScope> scope)
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

static std::shared_ptr<LispVariant> Break(const std::vector<std::shared_ptr<object>> & /*args*/, std::shared_ptr<LispScope> scope)
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

static std::shared_ptr<LispVariant> Vars(const std::vector<std::shared_ptr<object>> & /*args*/, std::shared_ptr<LispScope> scope)
{
	scope->GlobalScope->Output->WriteLine("variables:");
	scope->DumpVars();
	return std::make_shared<LispVariant>(LispVariant());
}

static std::shared_ptr<LispVariant> DelVar(const std::vector<std::shared_ptr<object>> & args, std::shared_ptr<LispScope> scope)
{
	CheckArgs("delvar", 1, args, scope);

	var name = ((LispVariant)args[0]);
	var ok = scope->Remove(name.ToString());
	return std::make_shared<LispVariant>(std::make_shared<object>(ok));
}

static std::shared_ptr<LispVariant> NeedLValue(const std::vector<std::shared_ptr<object>> & args, std::shared_ptr<LispScope> scope)
{
	CheckArgs("need-l-value", 0, args, scope);

	return std::make_shared<LispVariant>(std::make_shared<object>(scope->NeedsLValue));
}

static std::shared_ptr<LispVariant> TracePrint(const std::vector<std::shared_ptr<object>> & args, std::shared_ptr<LispScope> scope)
{
	var status = args[0]->ToLispVariantRef();
	(*scope)[Traceon] = std::make_shared<object>(status.BoolValue());
	return std::make_shared<LispVariant>(std::make_shared<object>(status.BoolValue()));
}

static std::shared_ptr<LispVariant> GetTracePrint(const std::vector<std::shared_ptr<object>> & /*args*/, std::shared_ptr<LispScope> scope)
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

namespace CppLisp
{
	uint64_t Environment_GetTickCount(void)
	{
		return GetTickCount();
	}
}

static std::shared_ptr<LispVariant> CurrentTickCount(const std::vector<std::shared_ptr<object>> & /*args*/, std::shared_ptr<LispScope> /*scope*/)
{
	var value = (int)GetTickCount();
	return std::make_shared<LispVariant>(std::make_shared<object>(value));
}

static std::shared_ptr<LispVariant> _Sleep(const std::vector<std::shared_ptr<object>> & args, std::shared_ptr<LispScope> /*scope*/)
{
	var timeInMs = ((LispVariant)args[0]).ToInt();
	std::this_thread::sleep_for(std::chrono::milliseconds(timeInMs));
	return std::make_shared<LispVariant>(LispVariant());
}

static std::shared_ptr<LispVariant> Datetime(const std::vector<std::shared_ptr<object>> & /*args*/, std::shared_ptr<LispScope> /*scope*/)
{
	std::time_t t = std::time(0);   // get time now
	std::tm* now = std::localtime(&t);

	var year = now->tm_year;
	var month = now->tm_mon;
	var day = now->tm_mday;
	var hour = now->tm_hour;
	var minute = now->tm_min;
	var second = now->tm_sec;
	var value = IEnumerable<std::shared_ptr<object>>();
	value.Add(std::make_shared<object>(year));
	value.Add(std::make_shared<object>(month));
	value.Add(std::make_shared<object>(day));
	value.Add(std::make_shared<object>(hour));
	value.Add(std::make_shared<object>(minute));
	value.Add(std::make_shared<object>(second));
	var result = std::make_shared<LispVariant>(LispType::_List, std::make_shared<object>(value));
	return result;
}

#ifdef _WIN32
#define _OS_STRING	"WIN"
#endif

#ifdef __OS2__
#define _OS_STRING	"OS2"
#endif

#if defined( __linux__ )
#define _OS_STRING	"UNIX"
#endif

#if defined( __APPLE__ )
#define _OS_STRING	"MACOSX"
#endif

static std::shared_ptr<LispVariant> Platform(const std::vector<std::shared_ptr<object>> & /*args*/, std::shared_ptr<LispScope> /*scope*/)
{
	var list = IEnumerable<std::shared_ptr<object>>();
	list.Add(std::make_shared<object>(_OS_STRING));
	list.Add(std::make_shared<object>("C++"));
	bool is64Bit = sizeof(void *) == 8;
	list.Add(std::make_shared<object>(is64Bit ? "64bit" : "32bit"));
	var result = std::make_shared<LispVariant>(LispType::_List, std::make_shared<object>(list));
	return result;
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

namespace CppLisp
{
	extern string LispUtils_LibraryPath;
}

#if defined(_WIN32) || defined(_WIN64)
const std::string DirectorySeparatorChar("\\");
const std::string OtherDirectorySeparatorChar("/");
#else
const std::string DirectorySeparatorChar("/");
const std::string OtherDirectorySeparatorChar("\\");
#endif

string ConvertToLocalDirectorySeperators(const string & path)
{
	return path.Replace(OtherDirectorySeparatorChar, DirectorySeparatorChar);
}

static std::shared_ptr<LispVariant> Import(const std::vector<std::shared_ptr<object>> & args, std::shared_ptr<LispScope> scope)
{
	std::shared_ptr<LispVariant> result = std::make_shared<LispVariant>();
	for(var modu : args)
	{
		string code = string::Empty;
		string orgModuleFileName = ConvertToLocalDirectorySeperators(modu->ToLispVariantRef().StringValue());
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
					// try default path for visiscript .\lib\fuel\modulename.fuel
					fileName = "." + /*Path.*/DirectorySeparatorChar + "lib" + /*Path.*/DirectorySeparatorChar + "fuel" + DirectorySeparatorChar + orgModuleFileName;
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
		}
		if (File_Exists(fileName))
		{
			code = ReadFileOrEmptyString(fileName);
		}
		else
		{
			// use std lib of fuel from builtin resources
		// resources not supported in C++ for all paltforms...
		//	if (orgModuleFileName == "fuellib")
		//	{
		// 		code = Encoding.UTF8.GetString(Properties.Resources.fuellib);
		//	}
		//	else
		//	{
				scope->GlobalScope->Output->WriteLine("WARNING: Library {0} not found! Tried path {1}", orgModuleFileName, fileName);
		//	}
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

static std::shared_ptr<LispVariant> Nop(const std::vector<std::shared_ptr<object>> & /*args*/, std::shared_ptr<LispScope> /*scope*/)
{
	return std::make_shared<LispVariant>(LispVariant());
}

static std::shared_ptr<LispVariant> Return(const std::vector<std::shared_ptr<object>> & args, std::shared_ptr<LispScope> scope)
{
	scope->IsInReturn = true;
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

static std::shared_ptr<LispVariant> Flush(const std::vector<std::shared_ptr<object>> & args, std::shared_ptr<LispScope> scope)
{
	CheckArgs("flush", 0, args, scope);

	scope->GlobalScope->Output->Flush();
	return std::make_shared<LispVariant>(LispVariant(LispType::_Undefined));
}

static std::shared_ptr<LispVariant> ReadLine(const std::vector<std::shared_ptr<object>> & args, std::shared_ptr<LispScope> scope)
{
	CheckArgs("readline", 0, args, scope);

	var text = scope->GlobalScope->Input->ReadLine();
	return std::make_shared<LispVariant>(std::make_shared<object>(text));
}

static std::shared_ptr<LispVariant> ParseInteger(const std::vector<std::shared_ptr<object>> & args, std::shared_ptr<LispScope> scope)
{
	CheckArgs("parse-integer", 1, args, scope);

	int value;
	try
	{
		size_t idx = 0;
		string s = ((LispVariant)args[0]).ToString();
		value = std::stoi(s, &idx);
		if (idx < s.size())
		{
			throw LispException("format error");
		}
	}
	catch (...)
	{
		return std::make_shared<LispVariant>(LispVariant(LispType::_Undefined));
	}
	return std::make_shared<LispVariant>(std::make_shared<object>(value));
}

static std::shared_ptr<LispVariant> ParseFloat(const std::vector<std::shared_ptr<object>> & args, std::shared_ptr<LispScope> scope)
{
	CheckArgs("parse-float", 1, args, scope);

	double value;
	try
	{
		value = std::stod(((LispVariant)args[0]).ToString());
	}
	catch (...)
	{
		return std::make_shared<LispVariant>(LispVariant(LispType::_Undefined));
	}
	return std::make_shared<LispVariant>(std::make_shared<object>(value));
}

static std::shared_ptr<LispVariant> ToInt(const std::vector<std::shared_ptr<object>> & args, std::shared_ptr<LispScope> scope)
{
	CheckArgs("int", 1, args, scope);

	var value = (LispVariant)args[0];
	if (value.IsInt())
	{
		return std::make_shared<LispVariant>(std::make_shared<object>(value.IntValue()));
	}
	if (value.IsDouble())
	{
		return std::make_shared<LispVariant>(std::make_shared<object>((int)value.DoubleValue()));
	}
	if (value.IsString())
	{
		return ParseInteger(args, scope);
	}
	if (value.IsBool())
	{
		return std::make_shared<LispVariant>(std::make_shared<object>(value.BoolValue() ? 1 : 0));
	}
	return std::make_shared<LispVariant>(std::make_shared<object>(LispType::_Undefined));
}

static std::shared_ptr<LispVariant> ToFloat(const std::vector<std::shared_ptr<object>> & args, std::shared_ptr<LispScope> scope)
{
	CheckArgs("float", 1, args, scope);

	var value = (LispVariant)args[0];
	if (value.IsInt())
	{
		return std::make_shared<LispVariant>(std::make_shared<object>((double)value.IntValue()));
	}
	if (value.IsDouble())
	{
		return std::make_shared<LispVariant>(std::make_shared<object>(value.DoubleValue()));
	}
	if (value.IsString())
	{
		return ParseFloat(args, scope);
	}
	if (value.IsBool())
	{
		return std::make_shared<LispVariant>(std::make_shared<object>(value.BoolValue() ? 1.0 : 0.0));
	}
	return std::make_shared<LispVariant>(std::make_shared<object>(LispType::_Undefined));
}

static std::shared_ptr<LispVariant> Search(const std::vector<std::shared_ptr<object>> & args, std::shared_ptr<LispScope> scope)
{
	CheckOptionalArgs("search", 2, 4, args, scope);

	var searchText = ((LispVariant)args[0]).ToString();
	const LispVariant & arg1 = args[1]->ToLispVariantRef();
	size_t pos = args.size() > 2 ? (size_t)((LispVariant)args[2]).ToInt() : std::string::npos;
	size_t len = args.size() > 3 ? (size_t)((LispVariant)args[3]).ToInt() : std::string::npos;
	size_t foundPos = std::string::npos;
	if (arg1.IsString())
	{
		var source = ((LispVariant)args[1]).ToString();
		if (pos != std::string::npos)
		{
			if (len != std::string::npos)
			{
				foundPos = source.IndexOf(searchText, pos, len);
			}
			else
			{
				foundPos = source.IndexOf(searchText, pos);
			}
		}
		else
		{
			foundPos = source.IndexOf(searchText);
		}
	}
	else if (arg1.IsList())
	{
		var list = arg1.ListValueRef();
		size_t i = 0;
		//foreach(var elem in list)
		for (var elem : list)
		{
			if (searchText.Equals(elem->ToString()))
			{
				foundPos = i;
				break;
			}
			i++;
		}
	}
	else
	{
		throw LispException("search not supported for type " + string((int)args[1]->GetType()));
	}
	return std::make_shared<LispVariant>(std::make_shared<object>((int)foundPos));
}

static std::shared_ptr<LispVariant> Slice(const std::vector<std::shared_ptr<object>> & args, std::shared_ptr<LispScope> scope)
{
	CheckArgs("slice", 3, args, scope);

	var value = ((LispVariant)args[0]).ToString();
	var startPos = (size_t)((LispVariant)args[1]).ToInt();
	var len = ((LispVariant)args[2]).ToInt();
	if (len >= 0)
	{
		value = value.Substring(startPos, (size_t)len);
	}
	else
	{
		value = value.Substring(startPos);
	}
	return std::make_shared<LispVariant>(std::make_shared<object>(value));
}

static std::shared_ptr<LispVariant> Replace(const std::vector<std::shared_ptr<object>> & args, std::shared_ptr<LispScope> scope)
{
	CheckArgs("replace", 3, args, scope);

	string value = args[0]->ToString();
	const string & search = args[1]->ToString();
	const string & replace = args[2]->ToString();
	value = value.Replace(search, replace);
	return std::make_shared<LispVariant>(std::make_shared<object>(value));
}

static std::shared_ptr<LispVariant> Trim(const std::vector<std::shared_ptr<object>> & args, std::shared_ptr<LispScope> scope)
{
	CheckArgs("trim", 1, args, scope);

	var value = ((LispVariant)args[0]).ToString();
	return std::make_shared<LispVariant>(std::make_shared<object>(value.Trim()));
}

static std::shared_ptr<LispVariant> LowerCase(const std::vector<std::shared_ptr<object>> & args, std::shared_ptr<LispScope> scope)
{
	CheckArgs("lower-case", 1, args, scope);

	var value = ((LispVariant)args[0]).ToString();
	return std::make_shared<LispVariant>(std::make_shared<object>(value.ToLower()));
}

static std::shared_ptr<LispVariant> UpperCase(const std::vector<std::shared_ptr<object>> & args, std::shared_ptr<LispScope> scope)
{
	CheckArgs("upper-case", 1, args, scope);

	var value = ((LispVariant)args[0]).ToString();
	return std::make_shared<LispVariant>(std::make_shared<object>(value.ToUpper()));
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

static std::shared_ptr<LispVariant> CompareOperation(const std::vector<std::shared_ptr<object>> & args, std::function<std::shared_ptr<LispVariant>(const LispVariant &, const LispVariant &)> op, std::shared_ptr<LispScope> scope)
{
	CheckArgs("compare-op", 2, args, scope);

	const LispVariant & arg1 = args[0]->ToLispVariantRef();
	const LispVariant & arg2 = args[1]->ToLispVariantRef();
	std::shared_ptr<LispVariant> result = op(arg1, arg2);
	return result;
}

static std::shared_ptr<LispVariant> Addition(const std::vector<std::shared_ptr<object>> & args, std::shared_ptr<LispScope> /*scope*/)
{
	return ArithmetricOperation(args, [](std::shared_ptr<LispVariant> l, std::shared_ptr<LispVariant> r) -> std::shared_ptr<LispVariant> { return std::make_shared<LispVariant>(*l + *r); });
}

static std::shared_ptr<LispVariant> Subtraction(const std::vector<std::shared_ptr<object>> & args, std::shared_ptr<LispScope> /*scope*/)
{
	// process unary - operator
	if (args.size() == 1)
	{
		var value = (LispVariant)args[0];
		if (value.IsInt())
		{
			return std::make_shared<LispVariant>(std::make_shared<object>(-value.IntValue()));
		}
		if (value.IsDouble())
		{
			return std::make_shared<LispVariant>(std::make_shared<object>(-value.DoubleValue()));
		}
		throw LispException(string::Format("Unary operator - not available for {0}", value.TypeString()));
	}

	return ArithmetricOperation(args, [](std::shared_ptr<LispVariant> l, std::shared_ptr<LispVariant> r) -> std::shared_ptr<LispVariant> { return std::make_shared<LispVariant>(*l - *r); });
}

static std::shared_ptr<LispVariant> Multiplication(const std::vector<std::shared_ptr<object>> & args, std::shared_ptr<LispScope> /*scope*/)
{
	return ArithmetricOperation(args, [](std::shared_ptr<LispVariant> l, std::shared_ptr<LispVariant> r) -> std::shared_ptr<LispVariant> { return std::make_shared<LispVariant>(*l * *r); });
}

static std::shared_ptr<LispVariant> Division(const std::vector<std::shared_ptr<object>> & args, std::shared_ptr<LispScope> /*scope*/)
{
	return ArithmetricOperation(args, [](std::shared_ptr<LispVariant> l, std::shared_ptr<LispVariant> r) -> std::shared_ptr<LispVariant> { return std::make_shared<LispVariant>(*l / *r); });
}

static std::shared_ptr<LispVariant> Modulo(const std::vector<std::shared_ptr<object>> & args, std::shared_ptr<LispScope> /*scope*/)
{
	return ArithmetricOperation(args, [](std::shared_ptr<LispVariant> l, std::shared_ptr<LispVariant> r) -> std::shared_ptr<LispVariant> { return std::make_shared<LispVariant>(*l % *r); });
}

static std::shared_ptr<LispVariant> Not(const std::vector<std::shared_ptr<object>> & args, std::shared_ptr<LispScope> scope)
{
	CheckArgs("not", 1, args, scope);

	var arg1 = (LispVariant)args[0];
	return std::make_shared<LispVariant>(std::make_shared<object>(!arg1.BoolValue()));
}

static std::shared_ptr<LispVariant> LessTest(const std::vector<std::shared_ptr<object>> & args, std::shared_ptr<LispScope> scope)
{
	return CompareOperation(args, [](const LispVariant & l, const LispVariant & r) -> std::shared_ptr<LispVariant> { return std::make_shared<LispVariant>(l < r); }, scope);
}

static std::shared_ptr<LispVariant> GreaterTest(const std::vector<std::shared_ptr<object>> & args, std::shared_ptr<LispScope> scope)
{
	return CompareOperation(args, [](const LispVariant & l, const LispVariant & r) -> std::shared_ptr<LispVariant> { return std::make_shared<LispVariant>(l > r); }, scope);
}

static std::shared_ptr<LispVariant> LessEqualTest(const std::vector<std::shared_ptr<object>> & args, std::shared_ptr<LispScope> scope)
{
	return CompareOperation(args, [](const LispVariant & l, const LispVariant & r) -> std::shared_ptr<LispVariant> { return std::make_shared<LispVariant>(l <= r); }, scope);
}

static std::shared_ptr<LispVariant> GreaterEqualTest(const std::vector<std::shared_ptr<object>> & args, std::shared_ptr<LispScope> scope)
{
	return CompareOperation(args, [](const LispVariant & l, const LispVariant & r) -> std::shared_ptr<LispVariant> { return std::make_shared<LispVariant>(l >= r); }, scope);
}

static std::shared_ptr<LispVariant> EqualTest(const std::vector<std::shared_ptr<object>> & args, std::shared_ptr<LispScope> scope)
{
	return CompareOperation(args, [](const LispVariant & l, const LispVariant & r) -> std::shared_ptr<LispVariant> { return std::make_shared<LispVariant>(std::make_shared<object>(LispVariant::EqualOp(l, r))); }, scope);
}

static std::shared_ptr<LispVariant> NotEqualTest(const std::vector<std::shared_ptr<object>> & args, std::shared_ptr<LispScope> scope)
{
	return CompareOperation(args, [](const LispVariant & l, const LispVariant & r) -> std::shared_ptr<LispVariant> { return std::make_shared<LispVariant>(std::make_shared<object>(!LispVariant::EqualOp(l, r))); }, scope);
}

static std::shared_ptr<LispVariant> CreateList(const std::vector<std::shared_ptr<object>> & args, std::shared_ptr<LispScope> /*scope*/)
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

	const LispVariant & start = args[2]->ToLispVariantRef();
	var result = std::make_shared<LispVariant>(start);
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

static std::shared_ptr<LispVariant> Cons(const std::vector<std::shared_ptr<object>> & args, std::shared_ptr<LispScope> /*scope*/)
{
	var list = IEnumerable<std::shared_ptr<object>>();
	if (args.size() > 0)
	{
		list.Add(args[0]);
	}
	if (args.size() > 1)
	{
		var item2 = args[1]->ToLispVariantRef();
		if (item2.IsList())
		{
			const IEnumerable<std::shared_ptr<object>> & l = item2.ListValueRef();
			for(var item : l)
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

	const LispVariant & val = args[0]->ToLispVariantRef();
	if (val.IsNativeObject())
	{
		if (val.Value->IsDictionary())
		{
			return std::make_shared<LispVariant>(std::make_shared<object>((int)val.Value->ToDictionary().size()));
		}
	}
	if (val.IsString())
	{
		return std::make_shared<LispVariant>(std::make_shared<object>((int)val.StringValue().size()));
	}
	const IEnumerable<std::shared_ptr<object>> & elements = val.ListValueRef();
	return std::make_shared<LispVariant>(std::make_shared<object>((int)elements.Count()));
}

static std::shared_ptr<LispVariant> First(const std::vector<std::shared_ptr<object>> & args, std::shared_ptr<LispScope> scope)
{
	CheckArgs("first", 1, args, scope);

	const LispVariant & val = args[0]->ToLispVariantRef();
	if (val.IsString())
	{
		return std::make_shared<LispVariant>(std::make_shared<object>(val.StringValue().Substring(0, 1)));
	}
	if (scope->NeedsLValue)
	{
		std::shared_ptr<object> listValue = args[0];

		std::function<void(std::shared_ptr<object>)> action = 
			[listValue](std::shared_ptr<object> newValue) -> void
			{ 
				var vl = listValue->ToLispVariantNotConstRef();
				IEnumerable<std::shared_ptr<object>> & container = vl.ListValueNotConstRef();
				container[0] = newValue;
			};

		return std::make_shared<LispVariant>(LispVariant(/*LispType::_LValue,*/ action));
	}
	else
	{
		const IEnumerable<std::shared_ptr<object>> & elements = val.ListValueRef();
		return std::make_shared<LispVariant>(elements.First());
	}
}

static std::shared_ptr<LispVariant> Last(const std::vector<std::shared_ptr<object>> & args, std::shared_ptr<LispScope> scope)
{
	CheckArgs("last", 1, args, scope);

	const LispVariant & val = args[0]->ToLispVariantRef();
	if (val.IsString())
	{
		return std::make_shared<LispVariant>(std::make_shared<object>(val.StringValue().Substring(val.StringValue().size() - 1)));
	}
	if (scope->NeedsLValue)
	{
		std::shared_ptr<object> listValue = args[0];

		std::function<void(std::shared_ptr<object>)> action =
			[listValue](std::shared_ptr<object> newValue) -> void
		{
			var vl = listValue->ToLispVariantNotConstRef();
			IEnumerable<std::shared_ptr<object>> & container = vl.ListValueNotConstRef();
			container[container.Count()-1] = newValue;
		};

		return std::make_shared<LispVariant>(LispVariant(/*LispType::_LValue,*/ action));
	}
	else
	{
		const IEnumerable<std::shared_ptr<object>> & elements = val.ListValueRef();
		return std::make_shared<LispVariant>(elements.Last());
	}
}

static std::shared_ptr<LispVariant> Rest(const std::vector<std::shared_ptr<object>> & args, std::shared_ptr<LispScope> scope)
{
	CheckArgs("rest", 1, args, scope);

	const LispVariant & val = args[0]->ToLispVariantRef();
	if (val.IsString())
	{
		return std::make_shared<LispVariant>(std::make_shared<object>(val.StringValue().Substring(1)));
	}
	IEnumerable<std::shared_ptr<object>> elements = val.ListValueRef();
	return std::make_shared<LispVariant>(std::make_shared<object>(elements.Skip(1)));
}

static std::shared_ptr<LispVariant> Nth(const std::vector<std::shared_ptr<object>> & args, std::shared_ptr<LispScope> scope)
{
	CheckArgs("nth", 2, args, scope);

	var index = (size_t)args[0]->ToLispVariantRef().IntValue();
	const LispVariant & val = args[1]->ToLispVariantRef();
	if (val.IsString())
	{
		return std::make_shared<LispVariant>(std::make_shared<object>(val.StringValue().Substring(index, 1)));
	}
	if (scope->NeedsLValue)
	{
		std::shared_ptr<object> listValue = args[1];

		std::function<void(std::shared_ptr<object>)> action =
			[listValue,index](std::shared_ptr<object> newValue) -> void
		{
			var vl = listValue->ToLispVariantNotConstRef();
			IEnumerable<std::shared_ptr<object>> & container = vl.ListValueNotConstRef();
			container[index] = newValue;
		};

		return std::make_shared<LispVariant>(LispVariant(/*LispType::_LValue,*/ action));
	}
	else
	{
		const IEnumerable<std::shared_ptr<object>> & elements = val.ListValueRef();
		return std::make_shared<LispVariant>(elements.ElementAt(index));
	}
}

static std::shared_ptr<LispVariant> Push(const std::vector<std::shared_ptr<object>> & args, std::shared_ptr<LispScope> scope)
{
	CheckOptionalArgs("push", 2, 3, args, scope);

	const LispVariant & val = args[0]->ToLispVariantRef();
	LispVariant & list = args[1]->ToLispVariantNotConstRef();
	var pos = args.size() > 2 ? args[2]->ToLispVariantRef().ToInt() : 0;
	if (list.IsList())
	{
		IEnumerable<std::shared_ptr<object>> & elements = list.ListValueNotConstRef();
		if (pos < (int)elements.Count())
		{
			elements.Insert(pos, std::make_shared<object>(val));
			return std::make_shared<LispVariant>(LispVariant(std::make_shared<object>(elements)));
		}
		return std::make_shared<LispVariant>(LispVariant(LispType::_Nil));
	}
	else
	{
		throw LispException(string("push not supported for type ") + LispEnvironment::GetLispType(args[1]));
	}
}

static std::shared_ptr<LispVariant> Pop(const std::vector<std::shared_ptr<object>> & args, std::shared_ptr<LispScope> scope)
{
	CheckOptionalArgs("pop", 1, 2, args, scope);

	LispVariant & list = args[0]->ToLispVariantNotConstRef();
	var pos = args.size() > 1 ? args[1]->ToLispVariantRef().ToInt() : 0;
	if (list.IsList())
	{
		IEnumerable<std::shared_ptr<object>> & elements = list.ListValueNotConstRef();
		if (pos < (int)elements.Count())
		{
			var elem = elements.ElementAt(pos);
			elements.RemoveAt(pos);
			return std::make_shared<LispVariant>(LispVariant(elem));
		}
		return std::make_shared<LispVariant>(LispVariant(LispType::_Nil));
	}
	else
	{
		throw LispException(string("pop not supported for type ") + LispEnvironment::GetLispType(args[0]));
	}
}

static std::shared_ptr<LispVariant> Append(const std::vector<std::shared_ptr<object>> & args, std::shared_ptr<LispScope> /*scope*/)
{
	var list = IEnumerable<std::shared_ptr<object>>();
	for(var listElement : args)
	{
		const IEnumerable<std::shared_ptr<object>> & lst = listElement->ToLispVariantRef().ListValueRef();
		for(var item : lst)
		{
			list.Add(item);
		}
	}
	var result = std::make_shared<LispVariant>(LispType::_List, std::make_shared<object>(list));
	return result;
}

static std::shared_ptr<LispVariant> Reverse(const std::vector<std::shared_ptr<object>> & args, std::shared_ptr<LispScope> scope)
{
	CheckArgs("reverse", 1, args, scope);

	const LispVariant & val = args[0]->ToLispVariantRef();
	if (val.IsString())
	{
		var temp = val.StringValue();
		std::reverse(temp.begin(), temp.end());
		return std::make_shared<LispVariant>(LispType::_String, std::make_shared<object>(temp));
	}
	IEnumerable<std::shared_ptr<object>> elements = val.ListValueRef();
	std::reverse(elements.begin(), elements.end());
	return std::make_shared<LispVariant>(LispType::_List, std::make_shared<object>(elements));
}

static std::shared_ptr<LispVariant> RValue(const std::vector<std::shared_ptr<object>> & args, std::shared_ptr<LispScope> scope)
{
	CheckArgs("rval", 1, args, scope);

	var originalLValue = scope->NeedsLValue;
	scope->NeedsLValue = false;
	var value = EvalArgIfNeeded(args[0], scope);
	scope->NeedsLValue = originalLValue;
	return value;
}

static std::shared_ptr<LispVariant> SymbolFcn(const std::vector<std::shared_ptr<object>> & args, std::shared_ptr<LispScope> scope)
{
	CheckArgs(LispEnvironment::Sym, 1, args, scope);

	const string & symbol = args[0]->ToString();
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
		const LispVariant & variant = args[0]->ToLispVariantRef();
		if (variant.IsNativeObject())
		{
			value = variant.NativeObjectStringRepresentation();
		}
	}
	return std::make_shared<LispVariant>(LispType::_String, std::make_shared<object>(value));
}

static std::shared_ptr<LispVariant> ArgsCountFcn(const std::vector<std::shared_ptr<object>> & args, std::shared_ptr<LispScope> scope)
{
	CheckArgs("argscount", 0, args, scope);

	size_t cnt = (*scope)[ArgsMeta]->ToListRef().Count();
	return std::make_shared<LispVariant>(std::make_shared<object>((int)cnt));
}

static std::shared_ptr<LispVariant> ArgsFcn(const std::vector<std::shared_ptr<object>> & args, std::shared_ptr<LispScope> scope)
{
	CheckArgs("args", 0, args, scope);

	var array = (*scope)[ArgsMeta]->ToListRef()/*.ToArray()*/;
	return std::make_shared<LispVariant>(std::make_shared<object>(array));
}

static std::shared_ptr<LispVariant> ArgFcn(const std::vector<std::shared_ptr<object>> & args, std::shared_ptr<LispScope> scope)
{
	CheckArgs("arg", 1, args, scope);

	var index = args[0]->ToLispVariantRef().IntValue();
	const IEnumerable<std::shared_ptr<object>> & array = (*scope)[ArgsMeta]->ToListRef();
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

	const LispVariant & arguments = args[1]->ToLispVariantRef();

	if (arguments.IsList())
	{
		const IEnumerable<std::shared_ptr<object>> & argumentsArray = arguments.ListValueRef();
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
	const LispVariant & variant = args[0]->ToLispVariantRef();
	if (variant.IsList())
	{
		std::shared_ptr<object> code = std::make_shared<object>(variant.ListValueRef());
		result = LispInterpreter::EvalAst(code, scope);
	}
	else
	{
		result = LispInterpreter::EvalAst(std::make_shared<object>(variant), scope);
	}
	return result;
}

static std::shared_ptr<LispVariant> EvalStrFcn(const std::vector<std::shared_ptr<object>> & args, std::shared_ptr<LispScope> scope)
{
	CheckArgs("evalstr", 1, args, scope);

	const LispVariant & variant = args[0]->ToLispVariantRef();
	var tempModuleName = scope->ModuleName;
	scope->IsInEval = true;
	var result = Lisp::Eval(variant.Value->ToString(), scope, LispEnvironment::EvalStrTag + scope->ModuleName + ":" + variant.Value->ToString());
	scope->IsInEval = false;
	scope->ModuleName = tempModuleName;
	return result;
}

static std::shared_ptr<LispVariant> MakeDict(const std::vector<std::shared_ptr<object>> & args, std::shared_ptr<LispScope> scope)
{
	CheckArgs("make-dict", 0, args, scope);

	return std::make_shared<LispVariant>(LispVariant(LispType::_NativeObject, std::make_shared<object>(Dictionary<size_t, std::shared_ptr<object>>())));
}

static std::shared_ptr<LispVariant> DictSet(const std::vector<std::shared_ptr<object>> & args, std::shared_ptr<LispScope> scope)
{
	CheckArgs("dict-set", 3, args, scope);

	Dictionary<size_t, std::shared_ptr<object>> &  dict = args[0]->ToLispVariantRef().Value->ToDictionary();
	size_t key = args[1]->ToLispVariant()->Value->GetHash();
	var value = args[2]->ToLispVariant();
	dict[key] = value->Value;

	return value;
}

static std::shared_ptr<LispVariant> DictGet(const std::vector<std::shared_ptr<object>> & args, std::shared_ptr<LispScope> scope)
{
	CheckArgs("dict-get", 2, args, scope);

	Dictionary<size_t, std::shared_ptr<object>> &  dict = args[0]->ToLispVariantRef().Value->ToDictionary();
	size_t key = args[1]->ToLispVariant()->Value->GetHash();
	var result = dict.ContainsKey(key) ? dict[key] : std::make_shared<object>(object());

	return std::make_shared<LispVariant>(result);
}

static std::shared_ptr<LispVariant> DictRemove(const std::vector<std::shared_ptr<object>> & args, std::shared_ptr<LispScope> scope)
{
	CheckArgs("dict-remove", 2, args, scope);

	Dictionary<size_t, std::shared_ptr<object>> &  dict = args[0]->ToLispVariantRef().Value->ToDictionary();
	size_t key = args[1]->ToLispVariant()->Value->GetHash();
	var ok = dict.Remove(key);

	return std::make_shared<LispVariant>(std::make_shared<object>(ok));
}

//static std::shared_ptr<LispVariant> DictKeys(const std::vector<std::shared_ptr<object>> & args, std::shared_ptr<LispScope> scope)
//{
//	CheckArgs("dict-keys", 1, args, scope);
//
//	Dictionary<size_t, std::shared_ptr<object>> &  dict = args[0]->ToLispVariantRef().Value->ToDictionary();
//	size_t key = args[1]->ToLispVariant()->Value->GetHash();
//	List<LispVariant> result = new List<LispVariant>();
//	foreach(var key in nativeDict.Keys)
//	{
//		result.Add(new LispVariant(LispVariant.GetTypeFor(key), key));
//	}
//
//	return new LispVariant(result);
//}

static std::shared_ptr<LispVariant> DictClear(const std::vector<std::shared_ptr<object>> & args, std::shared_ptr<LispScope> scope)
{
	CheckArgs("dict-clean", 1, args, scope);

	Dictionary<size_t, std::shared_ptr<object>> &  dict = args[0]->ToLispVariantRef().Value->ToDictionary();
	dict.Clear();

	return std::make_shared<LispVariant>();
}

static std::shared_ptr<LispVariant> DictContainsKey(const std::vector<std::shared_ptr<object>> & args, std::shared_ptr<LispScope> scope)
{
	CheckArgs("dict-contains-key", 2, args, scope);

	Dictionary<size_t, std::shared_ptr<object>> &  dict = args[0]->ToLispVariantRef().Value->ToDictionary();
	size_t key = args[1]->ToLispVariant()->Value->GetHash();
	var result = dict.ContainsKey(key);

	return std::make_shared<LispVariant>(std::make_shared<object>(result));
}

static std::shared_ptr<LispVariant> DictContainsValue(const std::vector<std::shared_ptr<object>> & args, std::shared_ptr<LispScope> scope)
{
	CheckArgs("dict-contains-value", 2, args, scope);

	Dictionary<size_t, std::shared_ptr<object>> &  dict = args[0]->ToLispVariantRef().Value->ToDictionary();
	var value = args[2]->ToLispVariant();
	var result = dict.ContainsValue(value->Value);

	return std::make_shared<LispVariant>(std::make_shared<object>(result));
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

	var originalNeedsLValue = scope->NeedsLValue;
	scope->NeedsLValue = true;
	var symbol = EvalArgIfNeeded(args[0], scope);
	scope->NeedsLValue = originalNeedsLValue;
	var symbolName = symbol != null ? symbol->ToString() : /*null*/string::Empty;
	var value = LispInterpreter::EvalAst(args[1], scope);
	if (symbol->IsLValue())
	{
		std::function<void(std::shared_ptr<object>)> action = symbol->Value->ToSetterAction();
		action(std::make_shared<object>(*value));
	}
	else
	{
		scope->SetInScopes(symbolName, std::make_shared<object>(*value));
	}
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
			(*macros)[args[0]->ToString()] = std::make_shared<object>(LispMacroRuntimeEvaluate(std::make_shared<IEnumerable<std::shared_ptr<object>>>(args[1]->ToEnumerableOfObjectRef()), std::make_shared<IEnumerable<std::shared_ptr<object>>>(args[2]->ToEnumerableOfObjectRef())));
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
		(*macros)[args[0]->ToString()] = std::make_shared<object>(LispMacroCompileTimeExpand(LispEnvironment::GetExpression(args[1]), std::make_shared<IEnumerable<std::shared_ptr<object>>>(result->ToEnumerableOfObjectRef())));
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
	isSplicing = false;
	if (item->IsLispVariant())
	{
		const LispVariant & value = item->ToLispVariantRef(); 
		if (value.IsUnQuoted == LispUnQuoteModus::_UnQuote || value.IsUnQuoted == LispUnQuoteModus::_UnQuoteSplicing)
		{
			isSplicing = value.IsUnQuoted == LispUnQuoteModus::_UnQuoteSplicing;
			if (value.IsList())
			{
				std::shared_ptr<object> pObj = std::make_shared<object>(value.ListValueRef());
				return LispInterpreter::EvalAst(pObj, scope)->Value;
			}
			else
			{
				return (*scope)[value.StringValue()];
			}
		}
	}
	return item;
}

//static std::shared_ptr<IEnumerable<std::shared_ptr<object>>> ToEnumerable(std::shared_ptr<object> obj)
//{
//	if (obj->IsIEnumerableOfObject() /*enumerable != null*/ || obj->IsList())
//	{
//		var enumerable = obj->ToEnumerableOfObjectRef() /*as IEnumerable<object>*/;
//		return std::make_shared<IEnumerable<std::shared_ptr<object>>>(enumerable);
//	}
//	if (obj->IsLispVariant())
//	{
//		const LispVariant & variant = obj->ToLispVariantRef() /*as LispVariant*/;
//		if (variant.IsList())
//		{
//			return std::make_shared<IEnumerable<std::shared_ptr<object>>>(variant.ListValueRef());
//		}
//		else
//		{
//			return std::make_shared<IEnumerable<std::shared_ptr<object>>>(IEnumerable<std::shared_ptr<object>>());
//		}
//	}
//	else
//	{
//		return std::make_shared<IEnumerable<std::shared_ptr<object>>>(IEnumerable<std::shared_ptr<object>>());
//	}
//}

std::shared_ptr<LispVariant> unquote_form(const std::vector<std::shared_ptr<object>> & args, std::shared_ptr<LispScope> scope)
{
	CheckArgs(UnQuote, 1, args, scope);

	return std::make_shared<LispVariant>(args[0]);
}

std::shared_ptr<LispVariant> unquotesplicing_form(const std::vector<std::shared_ptr<object>> & args, std::shared_ptr<LispScope> scope)
{
	CheckArgs(UnQuoteSplicing, 1, args, scope);

	return std::make_shared<LispVariant>(args[0]);
}

static std::shared_ptr<object> ProcessQuotedSExpression(const IEnumerable<std::shared_ptr<object>> & expr, std::shared_ptr<LispScope> scope, /*out*/ bool & splicing)
{
	IEnumerable<std::shared_ptr<object>> result;

	splicing = false;

	if (expr.Count() == 2)
	{
		var item1 = expr.First();
		var item2 = expr.ElementAt(1);
		if (item1->IsLispVariant())
		{
			const LispVariant & variant = item1->ToLispVariantRef();
			if (variant.IsSymbol() && (variant.ToString() == LispEnvironment::UnQuote || variant.ToString() == LispEnvironment::UnQuoteSplicing))
			{
				var evalResult = LispInterpreter::EvalAst(item2, scope);
				splicing = variant.ToString() == LispEnvironment::UnQuoteSplicing;
				return std::make_shared<object>(*evalResult);
			}
		}
		result.Add(item1);
		result.Add(item2);
	}
	else
	{
		for (var itm : expr)
		{
			if (itm->IsIEnumerableOfObject() || itm->IsList())
			{
				bool tempSplicing = false;
				var res = ProcessQuotedSExpression(itm->IsIEnumerableOfObject() ? itm->ToEnumerableOfObjectRef() : itm->ToListRef(), scope, /*out*/ tempSplicing);
				if (tempSplicing)
				{
					const LispVariant & variant = res->ToLispVariantRef();
					result.AddRange(variant.ListValueRef());
				}
				else
				{
					result.Add(res);
				}
			}
			else
			{
				result.Add(itm);
			}
		}
	}
	return std::make_shared<object>(result);
}

std::shared_ptr<LispVariant> quasiquote_form(const std::vector<std::shared_ptr<object>> & args, std::shared_ptr<LispScope> scope)
{
	CheckArgs(LispEnvironment::Quasiquote, 1, args, scope);

	// iterate through arguments and evaluate unquote/splicing expressions
	var expression = args[0];
	if (expression->IsLispVariant())
	{
		return expression->ToLispVariant();
	}
	else if (expression->IsIEnumerableOfObject() || expression->IsList())
	{
		bool splicing = false;
		return std::make_shared<LispVariant>(ProcessQuotedSExpression(expression->IsIEnumerableOfObject() ? expression->ToEnumerableOfObjectRef() : expression->ToListRef(), scope, /*out*/ splicing));
	}
	return std::make_shared<LispVariant>(expression);
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
		if (scope->IsInReturn)
		{
			break;
		}
		condition = LispInterpreter::EvalAst(args[0], scope);
	}
	return result;
}

static std::shared_ptr<LispVariant> do_form(const std::vector<std::shared_ptr<object>> & args, std::shared_ptr<LispScope> scope)
{
	var result = std::make_shared<LispVariant>(LispType::_Undefined);

	for (var statement : args)
	{
		if (!(statement->IsIEnumerableOfObject() || statement->IsList() /*is IEnumerable<object>*/) || ((statement->IsLispVariant()) && (statement->ToLispVariantRef().IsList())))
		{
			throw LispException("List expected in do", /*((LispVariant)statement).Token*/statement->ToLispVariantRef().Token, scope->ModuleName, scope->DumpStackToString());
		}
		result = LispInterpreter::EvalAst(statement, scope);
		if (scope->IsInReturn)
		{
			break;
		}
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

std::shared_ptr<LispVariant> fn_form(const std::vector<std::shared_ptr<object>> & args, std::shared_ptr<LispScope> scope)
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
		std::vector<std::shared_ptr<object>> formalArgs = args[0]->IsLispVariant() /*is LispVariant*/ ? args[0]->ToLispVariantRef().ListValueRef().ToArray() : LispEnvironment::GetExpression(args[0])->ToArray();
		
		std::vector<std::shared_ptr<object>> tempLocalArgs = localArgs;

		if (formalArgs.size() > localArgs.size())
		{
			//throw LispException("Invalid number of arguments");

			// fill all not given arguments with nil
			//var newLocalArgs = new object[formalArgs.size()];
			std::vector<std::shared_ptr<object>> newLocalArgs(formalArgs.size());
			for (size_t n = 0; n < formalArgs.size(); n++)
			{
				if (n < localArgs.size())
				{
					newLocalArgs[n] = localArgs[n];
				}
				else
				{
					newLocalArgs[n] = std::make_shared<object>(LispVariant(LispType::_Nil));
				}
			}

			tempLocalArgs = newLocalArgs;
		}

		for(var arg : formalArgs)
		{
			(*childScope)[arg->ToString()] = tempLocalArgs[i];
			i++;
		}

		// support args function for accessing all given parameters
		(*childScope)[ArgsMeta] = std::make_shared<object>(VectorToList(tempLocalArgs));
		size_t formalArgsCount = formalArgs.size();
		if (tempLocalArgs.size() > formalArgsCount)
		{
			//var additionalArgs = new object[tempLocalArgs.size() - formalArgsCount];
			std::vector<std::shared_ptr<object>> additionalArgs(tempLocalArgs.size() - formalArgsCount);
			for (size_t n = 0; n < tempLocalArgs.size() - formalArgsCount; n++)
			{
				additionalArgs[n] = tempLocalArgs[n + formalArgsCount];
			}
			(*childScope)[AdditionalArgs] = std::make_shared<object>(LispVariant(std::make_shared<object>(VectorToList(additionalArgs))));
		}

		// save the current call stack to resolve variables in closures
		childScope->ClosureChain = scope;
		childScope->NeedsLValue = scope->NeedsLValue;     // support setf in recursive calls

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

				debugger->InteractiveLoop(/*initialTopScope: */childScope, /*currentAst :*/ /*(IEnumerable<object>)*/std::make_shared<IEnumerable<std::shared_ptr<object>>>(args[1]->ToEnumerableOfObjectRef()) /*new List<object> { info.Item2 }*/);
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
		const LispVariant & tokenName = item->ToLispVariantRef();
		std::shared_ptr<LispToken> token1 = scope->GetPreviousToken(tokenName.Token);
		std::shared_ptr<LispToken> token2 = scope->GetPreviousToken(token1);
		std::shared_ptr<LispToken> token3 = scope->GetPreviousToken(token2);
		return token3;
	}
	return null;
}

static const IEnumerable<std::shared_ptr<object>> & GetEnumerableFromArgs(std::shared_ptr<object> args)
{
	if (args->IsLispVariant())
	{
		const LispVariant & tempVal = args->ToLispVariantRef();
		if (tempVal.IsList())
		{
			return tempVal.ListValueRef();
		}
	}
	return args->ToEnumerableOfObjectRef();
}

static string GetFormalArgsAsString(std::shared_ptr<object> args)
{
	string result = string::Empty;
	
	const IEnumerable<std::shared_ptr<object>> & theArgs = GetEnumerableFromArgs(args);
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

	var fn = ((*(scope->GlobalScope))[Fn])->ToLispVariantRef().FunctionValue();
	scope->UserData = std::make_shared<object>(EvalArgIfNeeded(args[0], scope)->ToString());

	std::vector<std::shared_ptr<object>> tempArgs;
	tempArgs.push_back(args[1]);
	tempArgs.push_back(args[2]);
	var resultingFcn = fn.Function(tempArgs, scope);
	scope->UserData = null;

	var defFcn = ((*(scope->GlobalScope))[name])->ToLispVariantRef().FunctionValue();
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

// TODO --> implement call static native
//static std::shared_ptr<LispVariant> CallStaticNative(const std::vector<std::shared_ptr<object>> & args, std::shared_ptr<LispScope> scope)
//{
//	var className = args[0]->ToLispVariant();
//	var methodName = args.size() > 1 ? args[1]->ToString() : string::Empty;
//
//	if (className->IsString() || className->IsSymbol())
//	{
//		//var callArgs = GetCallArgs(args);
//
//		//Type nativeClass = Type.GetType(className.ToString());
//		//if (nativeClass != null)
//		//{
//		//	MethodInfo method;
//		//	try
//		//	{
//		//		method = nativeClass.GetMethod(methodName);
//		//	}
//		//	catch (AmbiguousMatchException)
//		//	{
//		//		// process overloaded methods, try to resolve method via types of given arguments
//		//		// example: Math-Abs, File-Exits, ...
//		//		var callArgsTypes = GetTypes(callArgs);
//		//		method = nativeClass.GetMethod(methodName, callArgsTypes);
//		//	}
//		//	if (method != null)
//		//	{
//		//		ParameterInfo[] parameterInfos = method.GetParameters();
//		//		object result = method.Invoke(null, ConvertAllToNative(callArgs, parameterInfos));
//		//		return new LispVariant(result);
//		//	}
//		//}
//	}
//	//throw LispException("Bad static method " + methodName + " for class " + className, scope);
//	return null;
//}

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
			auto item = module->find(funcName);
			if (item != module->end())
			{
				foundValue = item->second;
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
    if (scope.get() != null)
    {
		auto item = scope->find(key);
		if (item != scope->end())
		{
			return (*item).second->GetLispScopeRef()->ContainsKey(funcName->ToString());
		}
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
		return item->ToLispVariantRef().ListValue(); // ((LispVariant)item).ListValue;
	}
	if (item->IsIEnumerableOfObject() /*is IEnumerable<object>*/ || item->IsList())
	{
		return item->ToList(); // (IEnumerable<object>)item;
	}
	var result = std::make_shared<IEnumerable<std::shared_ptr<object>>>();
	result->Add(item);
	return result;
}

std::shared_ptr<IEnumerable<std::shared_ptr<object>>> LispEnvironment::CheckForList(const string & functionName, std::shared_ptr<object> listObj, std::shared_ptr<LispScope> scope)
{
	if (listObj->IsList() /*is object[]*/)
	{
		return GetExpression(listObj);
	}
	const LispVariant & value = listObj->ToLispVariantRef();
	if (value.IsNativeObject() && (value.Value->IsList() /*is IEnumerable<object>*/))
	{
		return /*(IEnumerable<object>)*/value.Value->ToList();
	}
	if (!value.IsList())
	{
		throw LispException("No list in " + functionName, scope->GetPreviousToken(((LispVariant)listObj).Token), scope->ModuleName, scope->DumpStackToString());
	}
	return value.ListValue();
}

/* Not needed yet
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
*/

/*private static*/ std::shared_ptr<object> LispEnvironment::QueryItem(std::shared_ptr<object> funcName, std::shared_ptr<LispScope> scope, const string & key)
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

	var fileName = args[0]->ToLispVariantRef().ToString();
	return std::make_shared<LispVariant>(std::make_shared<object>(File_Exists(fileName)));
}

std::shared_ptr<LispVariant> FileReadAllText(const std::vector<std::shared_ptr<object>> & args, std::shared_ptr<LispScope> scope)
{
	CheckArgs("File-ReadAllText", 1, args, scope);

	var fileName = args[0]->ToLispVariantRef().ToString();
	return std::make_shared<LispVariant>(std::make_shared<object>(ReadFileOrEmptyString(fileName)));
}

std::shared_ptr<LispVariant> FileWriteAllText(const std::vector<std::shared_ptr<object>> & args, std::shared_ptr<LispScope> scope)
{
	CheckArgs("File-WriteAllText", 2, args, scope);

	var fileName = args[0]->ToLispVariantRef().ToString();
	var content = args[1]->ToLispVariantRef().ToString();
	return std::make_shared<LispVariant>(std::make_shared<object>(WriteTextFile(fileName, content)));
}

string LispEnvironment::GetLispType(std::shared_ptr<object> obj)
{
	if (obj->IsLispVariant())
	{
		const LispVariant & lispVariant = obj->ToLispVariantRef();
		return lispVariant.TypeString();
	}
	return obj->GetTypeName();
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
	(*scope)["delvar"] = CreateFunction(DelVar, "(delvar name)", "Deletes a local variable with the given name and returns a success flag.");
	(*scope)["need-l-value"] = CreateFunction(NeedLValue, "(need-l-value)", "Returns #t if a l-value is needed as return value of the current function.");
	(*scope)["trace"] = CreateFunction(TracePrint, "(trace value)", "Switches the trace modus on or off.");
	(*scope)["gettrace"] = CreateFunction(GetTracePrint, "(gettrace)", "Returns the trace output.");
    (*scope)["import"] = CreateFunction(Import, "(import module1 ...)", "Imports modules with fuel code.");
	(*scope)["tickcount"] = CreateFunction(CurrentTickCount, "(tickcount)", "Returns the current tick count in milliseconds, can be used to measure times.");
	(*scope)["sleep"] = CreateFunction(_Sleep, "(sleep time-in-ms)", "Sleeps the given number of milliseconds.");
	(*scope)["date-time"] = CreateFunction(Datetime, "(date-time)", "Returns a list with informations about the current date and time: (year month day hours minutes seconds).");
	(*scope)["platform"] = CreateFunction(Platform, "(platform)", "Returns a list with informations about the current platform: (operating_system runtime_environment).");

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
	(*scope)["flush"] = CreateFunction(Flush, "(flush)", "Flushes the output to the console.");
	(*scope)["readline"] = CreateFunction(ReadLine, "(readline)", "Reads a line from the console input.");

	(*scope)["parse-integer"] = CreateFunction(ParseInteger, "(parse-integer expr)", "Convert the string expr into an integer value");
	(*scope)["parse-float"] = CreateFunction(ParseFloat, "(parse-float expr)", "Convert the string expr into a float value");
	(*scope)["int"] = CreateFunction(ToInt, "(int expr)", "Convert the expr into an integer value");
	(*scope)["float"] = CreateFunction(ToFloat, "(float expr)", "Convert the expr into a float value");

	(*scope)["search"] = CreateFunction(Search, "(search searchtxt expr [pos] [len])", "Returns the first position of the searchtxt in the string, starting from position pos.");
	(*scope)["slice"] = CreateFunction(Slice, "(slice expr1 pos len)", "Returns a substring of the given string expr1, starting from position pos with length len.");
	(*scope)["replace"] = CreateFunction(Replace, "(replace expr1 searchtxt replacetxt)", "Returns a string of the given string expr1 with replacing searchtxt with replacetxt.");
	(*scope)["trim"] = CreateFunction(Trim, "(trim expr1)", "Returns a string with no starting and trailing whitespaces.");
	(*scope)["lower-case"] = CreateFunction(LowerCase, "(lower-case expr1)", "Returns a string with only lower case characters.");
	(*scope)["upper-case"] = CreateFunction(UpperCase, "(upper-case expr1)", "Returns a string with only upper case characters.");
	(*scope)["string"] = CreateFunction(Addition, "(string expr1 expr2 ...)", "see: add");
	(*scope)["add"] = CreateFunction(Addition, "(add expr1 expr2 ...)", "Returns value of expr1 added with expr2 added with ...");
	(*scope)["+"] = CreateFunction(Addition, "(+ expr1 expr2 ...)", "see: add");
	(*scope)["sub"] = CreateFunction(Subtraction, "(sub expr1 expr2 ...)", "Returns value of expr1 subtracted with expr2 subtracted with ...");
	(*scope)["-"] = CreateFunction(Subtraction, "(- expr1 expr2 ...)", "see: sub");
	(*scope)["mul"] = CreateFunction(Multiplication, "(mul expr1 expr2 ...)", "Returns value of expr1 multipied by expr2 multiplied by ...");
	(*scope)["*"] = CreateFunction(Multiplication, "(* expr1 expr2 ...)", "see: mul");
	(*scope)["div"] = CreateFunction(Division, "(div expr1 expr2 ...)", "Returns value of expr1 divided by expr2 divided by ...");
	(*scope)["/"] = CreateFunction(Division, "(/ expr1 expr2 ...)", "see: div");
	(*scope)["mod"] = CreateFunction(Modulo, "(mod expr1 expr2)", "Returns value of modulo operation between expr1 and expr2");
	(*scope)["%"] = CreateFunction(Modulo, "(% expr1 expr2)", "see: div");

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
	(*scope)["last"] = CreateFunction(Last, "(last list)", "Returns the last element of the list.");
	(*scope)["car"] = CreateFunction(First, "(car list)", "Returns the first element of the list.");
	(*scope)["rest"] = CreateFunction(Rest, "(rest list)", "see: cdr");
	(*scope)["cdr"] = CreateFunction(Rest, "(cdr list)", "Returns a new list containing all elements except the first of the given list.");
	(*scope)["nth"] = CreateFunction(Nth, "(nth number list)", "Returns the [number] element of the list.");
	(*scope)["push"] = CreateFunction(Push, "(push elem list [index])", "Inserts the element at the given index (default value 0) into the list (implace) and returns the updated list.");
	(*scope)["pop"] = CreateFunction(Pop, "(pop list [index])", "Removes the element at the given index (default value 0) from the list and returns the removed element.");
	(*scope)["append"] = CreateFunction(Append, "(append list1 list2 ...)", "Returns a new list containing all given lists elements.");
	(*scope)["reverse"] = CreateFunction(Reverse, "(reverse expr)", "Returns a list or string with a reverted order.");
	(*scope)["rval"] = CreateFunction(RValue, "(rval expr)", "Returns a RValue of the expr, disables LValue evaluation.", /*isBuiltin:*/true, /*isSpecialForm:*/ true);
	(*scope)[Sym] = CreateFunction(SymbolFcn, "(sym expr)", "Returns the evaluated expression as symbol.");
	(*scope)[Str] = CreateFunction(ConvertToString, "(str expr)", "Returns the evaluated expression as string.");

	(*scope)[ArgsCount] = CreateFunction(ArgsCountFcn, "(argscount)", "Returns the number of arguments for the current function.");
	(*scope)[Args] = CreateFunction(ArgsFcn, "(args)", "Returns all the values of the arguments for the current function.");
    (*scope)[Arg] = CreateFunction(ArgFcn, "(arg number)", "Returns the value of the [number] argument for the current function.");
	(*scope)[Apply] = CreateFunction(ApplyFcn, "(apply function arguments-list)", "Calls the function with the arguments.");
	(*scope)[Eval] = CreateFunction(EvalFcn, "(eval ast)", "Evaluates the abstract syntax tree (ast).");
	(*scope)[EvalStr] = CreateFunction(EvalStrFcn, "(evalstr string)", "Evaluates the string.");

	// additional data types
	(*scope)["make-dict"] = CreateFunction(MakeDict, "(make-dict)", "Returns a new dictionary.");
	(*scope)["dict-set"] = CreateFunction(DictSet, "(dict-set dict key value)", "Sets the value for the key in the dictionary.");
	(*scope)["dict-get"] = CreateFunction(DictGet, "(dict-get dict key)", "Returns the value for the key or nil if key is not in dictionary.");
	(*scope)["dict-remove"] = CreateFunction(DictRemove, "(dict-remove dict key)", "Removes the value / key pair from the directory and returns success flag.");
	//(*scope)["dict-keys"] = CreateFunction(DictKeys, "(dict-keys dict)", "Returns all keys in the dictionary.");
	(*scope)["dict-clear"] = CreateFunction(DictClear, "(dict-clear dict)", "Clears the dictionary.");
	(*scope)["dict-contains-key"] = CreateFunction(DictContainsKey, "(dict-contains-key dict key)", "Returns #t if key is contained in dictionary, otherwise #f.");
	(*scope)["dict-contains-value"] = CreateFunction(DictContainsValue, "(dict-contains-value dict key)", "Returns #t if value is contained in dictionary, otherwise #f.");

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
	(*scope)[UnQuote] = CreateFunction(unquote_form, "(unquote expr)", "Special form for unquoting expressions in quasiquote functions.", /*isBuiltin:*/true, /*isSpecialForm:*/ true);
	(*scope)[UnQuoteSplicing] = CreateFunction(unquotesplicing_form, "(unquotesplicing expr)", "Special form for unquotingsplicing expressions in quasiquote functions.", /*isBuiltin:*/true, /*isSpecialForm:*/ true);
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
