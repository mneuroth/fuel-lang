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

using System;
using System.IO;
using System.Linq;
using System.Reflection;

namespace CsLisp
{
    /// <summary>
    /// Fast Usable Embeddable Lisp Interpreter and Compiler (FUEL).
    /// </summary>
    public class Fuel
    {
        public static void Main(string[] args)
        {
            MainExtended(args, Console.Out, Console.In);
        }

        public static void MainExtended(string[] args, TextWriter output, TextReader input)
        {
            if (args.Length == 0)
            {
                Usage(output);
                return;
            }

            string script = null;
            var loadFiles = true;
            var trace = false;
            var compile = false;
            var wasDebugging = false;
            var showCompileOutput = false;
            var measureTime = false;
            var lengthyErrorOutput = false;
            var result = new LispVariant();
            var startTickCount = Environment.TickCount;
            var debugger = TryGetDebugger();

            if (args.Contains("-m"))
            {
                measureTime = true;
            }
            if (args.Contains("-v"))
            {
                output.WriteLine(Lisp.ProgramName + " " + Lisp.Version + " from " + Lisp.Date);
                return;
            }
            if (args.Contains("-h"))
            {
                Usage(output);
                return;
            }
            if (args.Contains("--doc"))
            {
                script = "(println (doc))";
                loadFiles = false;
            }
            if (args.Contains("--html"))
            {
                script = "(println (htmldoc))";
                loadFiles = false;
            }
            if (args.Contains("-x"))
            {
                lengthyErrorOutput = true;
            }
            if (args.Contains("-t"))
            {
                trace = true;
            }
            if (args.Contains("-e"))
            {
                script = LispUtils.GetScriptFilesFromProgramArgs(args).FirstOrDefault();
                loadFiles = false;
            }
            var libPath = args.Where(v => v.StartsWith("-l=")).Select(v => v).ToArray();
            if (libPath.Length > 0)
            {
                string libraryPath = libPath.First().Substring(3);
                LispUtils.LibraryPath = libraryPath;
            }

            // handle options for compiler
            if (args.Contains("-c"))
            {
                compile = true;
            }
            if (args.Contains("-s"))
            {
                showCompileOutput = true;
            }

            // handle options for debugger
            if (debugger != null)
            {
                debugger.SetInputOutputStreams(output, input);
                if (args.Contains("-i"))
                {
                    InteractiveLoopHeader(output);
                    debugger.InteractiveLoop(startedFromMain: true, tracing: trace);
                    loadFiles = false;
                    wasDebugging = true;
                }
                if (args.Contains("-d"))
                {
                    var fileName = LispUtils.GetScriptFilesFromProgramArgs(args).FirstOrDefault();
                    // process -e option if script is given via command line
                    if (script == null)
                    {
                        script = LispUtils.ReadFileOrEmptyString(fileName);
                    }
                    else
                    {
                        fileName = "command-line";
                    }

                    InteractiveLoopHeader(output);
                    result = debugger.DebuggerLoop(script, fileName, tracing: trace);
                    loadFiles = false;
                    wasDebugging = true;
                }
            }

            if (loadFiles)
            {
                var scriptFiles = LispUtils.GetScriptFilesFromProgramArgs(args);

                foreach (var fileName in scriptFiles)
                {
                    script = LispUtils.ReadFileOrEmptyString(fileName);
                    ILispCompiler compiler = TryGetCompiler();
                    if (compile && compiler != null)
                    {
                        result = compiler.CompileToExe(script, fileName + ".exe");
                    }
                    else if (showCompileOutput && compiler != null)
                    {
                        result = compiler.CompileToCsCode(script);
                        output.WriteLine(result.StringValue);
                    }
                    else
                    {
                        result = Lisp.SaveEval(script, moduleName: fileName, verboseErrorOutput: lengthyErrorOutput, tracing: trace);
                    }
                }
            }
            else if (script != null && !wasDebugging)
            {
                // process -e option
                result = Lisp.SaveEval(script);
            }

            if (trace)
            {
                output.WriteLine("Result=" + result);
            }
            if (measureTime)
            {
                output.WriteLine("Execution time = {0} s", (Environment.TickCount - startTickCount) * 0.001);
            }
        }

        #region private methods

        private static void Usage(TextWriter output)
        {
            LispUtils.ShowAbout(output);
            output.WriteLine("usage:");
            output.WriteLine(">" + Lisp.ProgramName + " [options] [script_file_name]");
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
            if (TryGetCompiler() != null)
            {
                output.WriteLine("  -c          : compile program");
                output.WriteLine("  -s          : show C# compiler output");
            }
            else
            {
                output.WriteLine();
                output.WriteLine("Info: no compiler support installed !");
            }
            output.WriteLine();
        }

        private static void InteractiveLoopHeader(TextWriter output)
        {
            LispUtils.ShowVersion(output);
            output.WriteLine("Type \"help\" for informations.");
            output.WriteLine();
        }

        private static ILispCompiler TryGetCompiler()
        {
            return TryGetClassFromDll<ILispCompiler>("FuelCompiler.dll", "CsLisp.LispCompiler");
        }

        private static ILispDebugger TryGetDebugger()
        {
            return TryGetClassFromDll<ILispDebugger>("FuelDebugger.dll", "CsLisp.LispDebugger");
        }

        private static T TryGetClassFromDll<T>(string dllName, string className)
        {
            var applicationPath = GetApplicationPath();
            var dllPath = Path.Combine(applicationPath, dllName);

            if (File.Exists(dllPath))
            {
                var dll = Assembly.LoadFile(dllPath);
                if (dll != null)
                {
                    var classType = dll.GetType(className);
                    var instance = Activator.CreateInstance(classType);
                    return (T)instance;
                }
            }

            return default(T);
        }

        private static string GetApplicationPath()
        {
            var application = Assembly.GetExecutingAssembly().Location;
            var applicationPath = Path.GetDirectoryName(application);
            return applicationPath;
        }

        #endregion
    }
}
