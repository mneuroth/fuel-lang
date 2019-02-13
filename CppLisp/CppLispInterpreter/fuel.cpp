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

#ifdef _WIN32
#include <windows.h>
#define FUELDEBUGGER_NAME "FuelDebugger.dll"
#endif

#if defined( __linux__ ) || defined( __APPLE__ )
#include <unistd.h>			// fuer: usleep(), execvp(), vfork()
#include <stdlib.h>			// fuer: system()
#include <time.h>			// fuer: clock()
#include <dlfcn.h>			// fuer: 
#endif

#if defined( __linux__ )
#define FUELDEBUGGER_NAME "FuelDebugger.so"
#endif

#if defined( __APPLE__ )
#define FUELDEBUGGER_NAME "FuelDebugger.dylib"
#endif

//******************************************************************
/** Funktion zum Laden von DLLs */
void * SimpleLoadLibrary(const char * sDllName)
{
#ifdef _WIN32
	return (void *)LoadLibrary( /*(LPCWSTR)*/sDllName);
#endif
#if defined( __linux__ ) || defined( __APPLE__ )
	return (void *)0; //dlopen( sDllName, RTLD_LAZY );
#endif
}

//******************************************************************
/** Funktion zum Entladen von DLLs */
bool SimpleFreeLibrary(void * hDllModule)
{
#ifdef _WIN32
	return FreeLibrary((HMODULE)hDllModule) == TRUE;
#endif
#if defined( __linux__ ) || defined( __APPLE__ )
	return 0; //dlclose( (void *)hDllModule );
#endif
}

//******************************************************************
/** Funktion zum Laden von Funktionen auf DLLs */
void * SimpleGetProcAddress(void * hDllModule, const char * sProcName)
{
#ifdef _WIN32
	return (void *)GetProcAddress((HMODULE)hDllModule, sProcName);
#endif
#if defined( __linux__ ) || defined( __APPLE__ )
	return (void *)dlsym( (void *)hDllModule, sProcName );
#endif
}

namespace CppLisp
{
	extern string LispUtils_LibraryPath;

	uint64_t Environment_GetTickCount(void);

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
		if (args.size() == 0)
		{
			Usage(output);
			return;
		}

		string script = /*null*/string::Empty;
		var loadFiles = true;
		var trace = false;
		var macroExpand = false;
		//var compile = false;
		var wasDebugging = false;
		//var showCompileOutput = false;
		var measureTime = false;
		var lengthyErrorOutput = false;
		var result = std::make_shared<LispVariant>();
		var startTickCount = /*Environment.TickCount*/Environment_GetTickCount();
		var debugger = TryGetDebugger();

		if (Contains(args, "-m"))
		{
			measureTime = true;
		}
		if (Contains(args, "-v"))
		{
			output->WriteLine(Lisp::ProgramName + " " + Lisp::Version + " from " + Lisp::Date);
			return;
		}
		if (Contains(args, "-h"))
		{
			Usage(output);
			return;
		}
		if (Contains(args, "--doc"))
		{
			script = "(println (doc))";
			loadFiles = false;
		}
		if (Contains(args, "--html"))
		{
			script = "(println (htmldoc))";
			loadFiles = false;
		}
		if (Contains(args, "--macro-expand"))
		{
			macroExpand = true;
		}
		if (Contains(args, "-x"))
		{
			lengthyErrorOutput = true;
		}
		if (Contains(args, "-t"))
		{
			trace = true;
		}
		if (Contains(args, "-e"))
		{
			script = /*LispUtils.*/GetScriptFilesFromProgramArgs(args)[0]/*.FirstOrDefault()*/;
			loadFiles = false;
		}
		//org: var libPath = args.Where(v = > v.StartsWith("-l=")).Select(v = > v).ToArray();
		var libPath = std::vector<string>(std::find_if(args.begin(), args.end(), [](const string & v) -> bool { return v.StartsWith("-l="); }), args.end());
		if (libPath.size() > 0)
		{
			string libraryPath = libPath.front().Substring(3);
			LispUtils_LibraryPath = libraryPath;
		}

		// handle options for compiler
		//if (Contains(args, "-c"))
		//{
		//	compile = true;
		//}
		//if (Contains(args, "-s"))
		//{
		//	showCompileOutput = true;
		//}

		// handle options for debugger
		if (debugger != null)
		{
			debugger->SetInputOutputStreams(output, input);
			if (Contains(args, "-i"))
			{
				InteractiveLoopHeader(output);
				debugger->InteractiveLoop(null, null, /*startedFromMain:*/ true, /*tracing :*/ trace, output, input);
				loadFiles = false;
				wasDebugging = true;
			}
			if (Contains(args, "-d"))
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

		if (loadFiles)
		{
			var scriptFiles = /*LispUtils.*/GetScriptFilesFromProgramArgs(args);

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
	}

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

		typedef ILispDebugger * (*fcnGetDebugger)();

		void * hDLL = SimpleLoadLibrary(FUELDEBUGGER_NAME);
		if (hDLL)
		{
			fcnGetDebugger pGetDebugger = (fcnGetDebugger)SimpleGetProcAddress(hDLL, "create_debugger");
			if (pGetDebugger)
			{
				dbg = std::shared_ptr<ILispDebugger>((*pGetDebugger)());
			}
		}
		
		return dbg;
	}
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
