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

#ifndef _FUEL_H
#define _FUEL_H

#include "csstring.h"
#include "Utils.h"
#include "Lisp.h"
#include "Debugger.h"

namespace CsLisp
{
	extern string LispUtils_LibraryPath;

	bool Contains(const std::vector<string> & container, const string & search);
	
	std::vector<string> GetScriptFilesFromProgramArgs(const std::vector<string> & args);

	/// <summary>
	/// Fast Usable Embeddable Lisp Interpreter and Compiler (FUEL).
	/// </summary>
	/*public*/ class Fuel
	{
	public:
		/*public*/ static void Main(std::vector<string> args)
		{
			TextWriter writer;
			TextReader reader;
			MainExtended(args, /*Console.Out, Console.In*/writer, reader);
		}

		/*public*/ static void MainExtended(std::vector<string> args, TextWriter & output, TextReader & input, string * pRedirectToString = 0, string * pRedirectFromString = 0)
		{
			if (args.size() == 0)
			{
				Usage(output);
				return;
			}

			string script = /*null*/string::Empty;
			var loadFiles = true;
			var trace = false;
			var compile = false;
			var wasDebugging = false;
			var showCompileOutput = false;
			var measureTime = false;
			var lengthyErrorOutput = false;
			var result = std::make_shared<LispVariant>();
			//			var startTickCount = Environment.TickCount;
			var debugger = TryGetDebugger();

			if (Contains(args, "-m"))
			{
				measureTime = true;
			}
			if (Contains(args, "-v"))
			{
				output.WriteLine(Lisp::ProgramName + " " + Lisp::Version + " from " + Lisp::Date);
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
			if (Contains(args, "-c"))
			{
				compile = true;
			}
			if (Contains(args, "-s"))
			{
				showCompileOutput = true;
			}

			// handle options for debugger
			if (debugger != null)
			{
				debugger->SetInputOutputStreams(output, input);
				if (Contains(args, "-i"))
				{
// TODO --> Input/Output als shared_ptr, damit besser zwischen Objekten geteilt werden kann
// TODO --> Debugger Streams != global Streams !!! --> dies ist ein Problem; Debugger sollte Streams an global Scope weiter reichen !
					InteractiveLoopHeader(output);
					debugger->InteractiveLoop(null, null, /*startedFromMain:*/ true, /*tracing :*/ trace, pRedirectToString, pRedirectFromString);
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
					//	output.WriteLine(result.StringValue);
					//}
					//else
					{
						result = Lisp::SaveEval(script, /*moduleName:*/ fileName, /*verboseErrorOutput:*/ lengthyErrorOutput, /*tracing:*/ trace, /*pRedirectToString:*/ pRedirectToString, /*pRedirectFromString:*/ pRedirectFromString);
					}
				}
			}
			else if (!string::IsNullOrEmpty(script) && !wasDebugging)
			{
				// process -e option
				result = Lisp::SaveEval(script, /*moduleName:*/ "cmdline", /*verboseErrorOutput:*/ false, /*tracing:*/ false, /*pRedirectToString:*/ pRedirectToString, /*pRedirectFromString:*/ pRedirectFromString);
			}

			if (trace)
			{
				output.WriteLine(string("Result=") + result->ToString());
			}
			//if (measureTime)
			//{
			//	output.WriteLine("Execution time = {0} s", (Environment.TickCount - startTickCount) * 0.001);
			//}
		}

	private:
		//#region private methods

		/*private*/ static void Usage(TextWriter & output)
		{
			/*LispUtils.*/ShowAbout(output);
			output.WriteLine("usage:");
			output.WriteLine(">" + Lisp::ProgramName + " [options] [script_file_name]");
			output.WriteLine();
			output.WriteLine("options:");
			output.WriteLine("  -v          : show version");
			output.WriteLine("  -h          : show help");
			output.WriteLine("  -e \"script\" : execute given script");
			output.WriteLine("  -l=\"path\"   : path to library");
			output.WriteLine("  --doc       : show language documentation");
			output.WriteLine("  --html      : show language documentation in html");
			output.WriteLine("  -m          : measure execution time");
			output.WriteLine("  -t          : enable tracing");
			output.WriteLine("  -x          : exhaustive error output");
			if (TryGetDebugger() != null)
			{
				output.WriteLine("  -i          : interactive shell");
				output.WriteLine("  -d          : start debugger");
			}
			else
			{
				output.WriteLine();
				output.WriteLine("Info: no debugger support installed !");
			}
			//if (TryGetCompiler() != null)
			//{
			//	output.WriteLine("  -c          : compile program");
			//	output.WriteLine("  -s          : show C# compiler output");
			//}
			//else
			//{
			//	output.WriteLine();
			//	output.WriteLine("Info: no compiler support installed !");
			//}
			output.WriteLine();
		}

		/*private*/ static void InteractiveLoopHeader(TextWriter & output)
		{
			/*LispUtils.*/ShowVersion(output);
			output.WriteLine("Type \"help\" for informations.");
			output.WriteLine();
		}

		///*private*/ static ILispCompiler TryGetCompiler()
		//{
		//	return TryGetClassFromDll<ILispCompiler>("FuelCompiler.dll", "CsLisp.LispCompiler");
		//}

		///*private*/ static ILispDebugger TryGetDebugger()
		//{
		//	return TryGetClassFromDll<ILispDebugger>("FuelDebugger.dll", "CsLisp.LispDebugger");
		//}

		static std::shared_ptr<ILispDebugger> TryGetDebugger()
		{
			std::shared_ptr<LispDebugger> dbg = std::make_shared<LispDebugger>();
			return dbg;
		}

		//template<T>
		///*private*/ static T TryGetClassFromDll(const string & dllName, const string & className)
		//{
		//	var applicationPath = GetApplicationPath();
		//	var dllPath = Path.Combine(applicationPath, dllName);

		//	if (File.Exists(dllPath))
		//	{
		//		var dll = Assembly.LoadFile(dllPath);
		//		if (dll != null)
		//		{
		//			var classType = dll.GetType(className);
		//			var instance = Activator.CreateInstance(classType);
		//			return (T)instance;
		//		}
		//	}

		//	return default(T);
		//}

		///*private*/ static string GetApplicationPath()
		//{
		//	var application = Assembly.GetExecutingAssembly().Location;
		//	var applicationPath = Path.GetDirectoryName(application);
		//	return applicationPath;
		//}

		//#endregion
	};
}

#endif
