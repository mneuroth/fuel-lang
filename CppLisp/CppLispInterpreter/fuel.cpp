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

//using System;
//using System.IO;
//using System.Linq;
//using System.Reflection;

#include "fuel.h"

#if defined( _WIN32 )
#include <windows.h>
#define FUEL_DEBUGGER_NAME "FuelDebugger.dll"

#elif defined( __linux__ )
#define FUEL_DEBUGGER_NAME "libFuelDebugger.so"

#elif defined( __APPLE__ )
#define FUEL_DEBUGGER_NAME "libFuelDebugger.dylib"

#elif defined( __PIC32MX__ )
#define FUEL_DEBUGGER_NAME "libFuelDebugger.so"

#elif defined( ARDUINO_ARCH_ESP32 )
#define FUEL_DEBUGGER_NAME "libFuelDebugger.so"

#else 
#error no valid platform defined

#endif

#if defined( __linux__ ) || defined( __APPLE__ )
#include <dlfcn.h>			// fuer: 
#endif

void * SimpleLoadLibrary(const char * sDllName)
{
#ifdef _WIN32
#ifdef UNICODE
	std::string s(sDllName);
	std::wstring stemp = std::wstring(s.begin(), s.end());
    return (void *)LoadLibrary( (LPCWSTR)stemp.c_str() );
#else
    return (void *)LoadLibrary( (LPCSTR)sDllName );
#endif
#endif
#if defined( __linux__ ) || defined( __APPLE__ )
    return (void *)dlopen( sDllName, RTLD_LAZY );
#endif
}

bool SimpleFreeLibrary(void * hDllModule)
{
#ifdef _WIN32
	return FreeLibrary((HMODULE)hDllModule) == TRUE;
#endif
#if defined( __linux__ ) || defined( __APPLE__ )
    return dlclose( (void *)hDllModule );
#endif
}

void * SimpleGetProcAddress(void * hDllModule, const char * sProcName)
{
#ifdef _WIN32
	return (void *)GetProcAddress((HMODULE)hDllModule, sProcName);
#endif
#if defined( __linux__ ) || defined( __APPLE__ )
	return (void *)dlsym( (void *)hDllModule, sProcName );
#endif
}

static void * g_hDebuggerDLL = 0;

#if defined(UNIT_TEST) || defined(WITH_STATIC_DEBUGGER)
extern "C" CppLisp::ILispDebugger * create_debugger();
#endif

static void DisconnectDebugger()
{
	if (g_hDebuggerDLL != 0)
	{
		SimpleFreeLibrary(g_hDebuggerDLL);
	}
}

namespace CppLisp
{
	extern string LispUtils_LibraryPath;

	uint64_t Environment_GetTickCount(void);

    static bool ContainsOptionAndRemove(std::vector<string> & args, const string & option)
	{
		var iter = std::find(args.begin(), args.end(), option);
		if (iter != args.end())
		{
			args.erase(iter);
			return true;
		}

		return false;
	}

	bool Contains(const std::vector<string> & container, const string & search)
	{
		return std::find(container.begin(), container.end(), search) != container.end();
	}

	std::vector<string> GetScriptFilesFromProgramArgs(const std::vector<string> & args)
	{
		std::vector<string> result;
		std::copy_if(args.begin(), args.end(), back_inserter(result), [](const string & s) -> bool { return !s.StartsWith("-");  });
		return result;
	}

	void Fuel::Main(std::vector<string> args)
	{
		std::shared_ptr<TextWriter> writer = std::make_shared<TextWriter>();
		std::shared_ptr<TextReader> reader = std::make_shared<TextReader>();
		MainExtended(args, /*Console.Out, Console.In*/writer, reader);
	}

	void Fuel::MainExtended(std::vector<string> args, std::shared_ptr<TextWriter> output, std::shared_ptr<TextReader> input)
	{
#ifndef _DISABLE_DEBUGGER
		if (args.size() == 0)
		{
			Usage(output);
			return;
		}
#endif

		std::vector<string> allArgs = args;

		string script = /*null*/string::Empty;
		var loadFiles = true;
		var trace = false;
		var macroExpand = false;
		//var compile = false;
		var wasDebugging = false;
		//var showCompileOutput = false;
		var measureTime = false;
		var lengthyErrorOutput = false;
		var interactiveLoop = false;
		var startDebugger = false;
		var result = std::make_shared<LispVariant>();
		var startTickCount = /*Environment.TickCount*/Environment_GetTickCount();
#ifndef _DISABLE_DEBUGGER
		var debugger = TryGetDebugger();

		if (ContainsOptionAndRemove(allArgs, "-m"))
		{
			measureTime = true;
		}
		if (ContainsOptionAndRemove(allArgs, "-v"))
		{
			output->WriteLine(Lisp::ProgramName + " " + Lisp::Version + " from " + Lisp::Date + " compiled with " + Lisp::GetCompilerInfo());
			return;
		}
		if (ContainsOptionAndRemove(allArgs, "-h"))
		{
			Usage(output);
			return;
		}
		if (ContainsOptionAndRemove(allArgs, "--doc"))
		{
			script = "(doc)";
			trace = true;
			loadFiles = false;
		}
		if (ContainsOptionAndRemove(allArgs, "--html"))
		{
			script = "(htmldoc)";
			trace = true;
			loadFiles = false;
		}
#endif
		if (ContainsOptionAndRemove(allArgs, "--macro-expand"))
		{
			macroExpand = true;
		}
		if (ContainsOptionAndRemove(allArgs, "-x"))
		{
			lengthyErrorOutput = true;
		}
		if (ContainsOptionAndRemove(allArgs, "-t"))
		{
			trace = true;
		}
		if (ContainsOptionAndRemove(allArgs, "-e"))
		{
			script = /*LispUtils.*/GetScriptFilesFromProgramArgs(args)[0]/*.FirstOrDefault()*/;
			loadFiles = false;
		}
		//org: var libPath = args.Where(v = > v.StartsWith("-l=")).Select(v = > v).ToArray();
		var libPath = std::vector<string>(std::find_if(args.begin(), args.end(), [](const string & v) -> bool { return v.StartsWith("-l="); }), args.end());
		if (libPath.size() > 0)
		{
			if (libPath.size() == 1)
			{
				string libraryPath = libPath.front().Substring(3);
				LispUtils_LibraryPath = libraryPath;
				ContainsOptionAndRemove(allArgs, *(libPath.begin()));
			}
			else
			{
				output->WriteLine("Error: only one library path is supported");
				return;
			}
		}

		// handle options for compiler
		//if (ContainsOptionAndRemove(allArgs, "-c"))
		//{
		//	compile = true;
		//}
		//if (ContainsOptionAndRemove(allArgs, "-s"))
		//{
		//	showCompileOutput = true;
		//}

#ifndef _DISABLE_DEBUGGER
			// handle options for debugger
		if (debugger != null)
		{
			if (ContainsOptionAndRemove(allArgs, "-i"))
			{
				interactiveLoop = true;
			}
			if (ContainsOptionAndRemove(allArgs, "-d"))
			{
				startDebugger = true;
			}
		}
#endif

		var scriptFiles = /*LispUtils.*/GetScriptFilesFromProgramArgs(args);

		// check if all command line options could be consumed
		//allArgs = allArgs.Where(x = > !scriptFiles.Contains(x)).ToList();    // remove script files from option list
		for (var scriptName : scriptFiles)
		{
			ContainsOptionAndRemove(allArgs, scriptName);
		}
		if (allArgs.size() > 0)
		{
			string unknownOptions;
			var iter = allArgs.begin();
			while (iter != allArgs.end())
			{
				unknownOptions += *iter + " ";
				++iter;
			}
			output->WriteLine("Error: unknown option(s) " + unknownOptions);	// {LispUtils.DumpEnumerable(allArgs, " ")}
			return;
		}

#ifndef _DISABLE_DEBUGGER
		if (debugger != null)
		{
			debugger->SetInputOutputStreams(output, input);
			if (interactiveLoop)
			{
				InteractiveLoopHeader(output);
				debugger->InteractiveLoop(null, null, /*startedFromMain:*/ true, /*tracing :*/ trace, output, input);
				loadFiles = false;
				wasDebugging = true;
			}
			if (startDebugger)
			{
				//var fileName = /*LispUtils.*/GetScriptFilesFromProgramArgs(args)./*FirstOrDefault()*/front();
				var scriptFiles = GetScriptFilesFromProgramArgs(args);
				var fileName = scriptFiles.size() > 0 ? scriptFiles.front() : string::Empty;
				// process -e option if script is given via command line
				if (string::IsNullOrEmpty(script))
				{
					script = /*LispUtils.*/ReadFileOrEmptyString(fileName);
				}
				else
				{
					fileName = "command-line";
				}

				InteractiveLoopHeader(output);
				result = debugger->DebuggerLoop(script, fileName, /*tracing:*/ trace);
				loadFiles = false;
				wasDebugging = true;
			}
		}
#endif

		if (loadFiles)
		{
			for (var fileName : scriptFiles)
			{
				script = /*LispUtils.*/ReadFileOrEmptyString(fileName);
				//ILispCompiler compiler = TryGetCompiler();
				//if (compile && compiler != null)
				//{
				//	result = compiler.CompileToExe(script, fileName + ".exe");
				//}
				//else if (showCompileOutput && compiler != null)
				//{
				//	result = compiler.CompileToCsCode(script);
				//	output->WriteLine(result.StringValue);
				//}
				//else
				{
					result = Lisp::SaveEval(script, /*moduleName:*/ fileName, /*verboseErrorOutput:*/ lengthyErrorOutput, /*tracing:*/ trace, output, input, macroExpand);
				}
			}
		}
		else if (!string::IsNullOrEmpty(script) && !wasDebugging)
		{
			// process -e option
			result = Lisp::SaveEval(script, /*moduleName:*/ "cmdline", /*verboseErrorOutput:*/ false, /*tracing:*/ false, output, input, macroExpand);
		}

		if (macroExpand)
		{
			output->WriteLine(string("Macro expand: ") + result->ToString());
		}
		if (trace)
		{
			output->WriteLine(string("Result=") + result->ToString());
		} 
		if (measureTime)
		{
			output->WriteLine("Execution time = {0} s", std::to_string((/*Environment.TickCount*/Environment_GetTickCount() - startTickCount) * 0.001));
		}

		DisconnectDebugger();
	}

#ifndef _DISABLE_DEBUGGER
	void Fuel::Usage(std::shared_ptr<TextWriter> output)
	{
		/*LispUtils.*/ShowAbout(output);
		output->WriteLine("usage:");
		output->WriteLine(">" + Lisp::ProgramName + " [options] [script_file_name]");
		output->WriteLine();
		output->WriteLine("options:");
		output->WriteLine("  -v          : show version");
		output->WriteLine("  -h          : show help");
		output->WriteLine("  -e \"script\" : execute given script");
		output->WriteLine("  -l=\"path\"   : path to library");
		output->WriteLine("  --doc       : show language documentation");
		output->WriteLine("  --html      : show language documentation in html");
		output->WriteLine("  --macro-expand : expand all macros and show resulting code");
		output->WriteLine("  -m          : measure execution time");
		output->WriteLine("  -t          : enable tracing");
		output->WriteLine("  -x          : exhaustive error output");
#ifndef _DISABLE_DEBUGGER
		if (TryGetDebugger() != null)
		{
			output->WriteLine("  -i          : interactive shell");
			output->WriteLine("  -d          : start debugger");
		}
		else
		{
			output->WriteLine();
			output->WriteLine("Info: no debugger support installed !");
		}
#endif
		//if (TryGetCompiler() != null)
		//{
		//	output->WriteLine("  -c          : compile program");
		//	output->WriteLine("  -s          : show C# compiler output");
		//}
		//else
		//{
		//	output->WriteLine();
		//	output->WriteLine("Info: no compiler support installed !");
		//}
		output->WriteLine();
	}

	void Fuel::InteractiveLoopHeader(std::shared_ptr<TextWriter> output)
	{
		/*LispUtils.*/ShowVersion(output);
		output->WriteLine("Type \"help\" for informations.");
		output->WriteLine();
	}

	std::shared_ptr<ILispDebugger> Fuel::TryGetDebugger()
	{
		std::shared_ptr<ILispDebugger> dbg = 0;

#if !defined(UNIT_TEST) && !defined(WITH_STATIC_DEBUGGER)
		typedef ILispDebugger * (*fcnGetDebugger)();

		if (g_hDebuggerDLL == 0)
		{
			g_hDebuggerDLL = SimpleLoadLibrary(FUEL_DEBUGGER_NAME);
		}
		if (g_hDebuggerDLL)
		{
			fcnGetDebugger pGetDebugger = (fcnGetDebugger)SimpleGetProcAddress(g_hDebuggerDLL, "create_debugger");
			if (pGetDebugger)
			{
				dbg = std::shared_ptr<ILispDebugger>((*pGetDebugger)());
			}
		}
#else
		dbg = std::shared_ptr<ILispDebugger>(create_debugger());		
#endif
		
		return dbg;
	}

#endif

};

extern "C" int fuel_main(int argc, char *argv[])
{
	std::vector<CppLisp::string> args;
	for (int i = 1; i < argc; i++)
	{
		args.push_back(argv[i]);
	}
	CppLisp::Fuel::Main(args);

	return 0;
}
