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
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Reflection;

namespace CsLisp
{
    public class LispMainHelper
    {
        private static bool ContainsOptionAndRemove(List<string> args, string option)
        {
            if (args.Contains(option))
            {
                args.Remove(option);
                return true;
            }

            return false;
        }

        public static void MainExtended(string[] args, TextWriter output, TextReader input)
        {
            if (args.Length == 0)
            {
                Usage(output);
                return;
            }

            List<string> allArgs = args.ToList();

            string script = null;
            var loadFiles = true;
            var trace = false;
            var macroExpand = false;
            var compile = false;
            var wasDebugging = false;
            var showCompileOutput = false;
            var measureTime = false;
            var lengthyErrorOutput = false;
            var interactiveLoop = false;
            var startDebugger = false;
            var result = new LispVariant();
            var startTickCount = Environment.TickCount;
            var debugger = TryGetDebugger();

            if (ContainsOptionAndRemove(allArgs, "-m"))
            {
                measureTime = true;
            }
            if (ContainsOptionAndRemove(allArgs, "-v"))
            {
                output.WriteLine(Lisp.ProgramName + " " + Lisp.Version + " from " + Lisp.Date);
                return;
            }
            if (ContainsOptionAndRemove(allArgs, "-h"))
            {
                Usage(output);
                return;
            }
            if (ContainsOptionAndRemove(allArgs, "--doc"))
            {
                script = "(println (doc))";
                loadFiles = false;
            }
            if (ContainsOptionAndRemove(allArgs, "--html"))
            {
                script = "(println (htmldoc))";
                loadFiles = false;
            }
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
                script = LispUtils.GetScriptFilesFromProgramArgs(args).FirstOrDefault();
                loadFiles = false;
            }
            var libPath = args.Where(v => v.StartsWith("-l=")).Select(v => v).ToArray();
            if (libPath.Length > 0)
            {
                if (libPath.Length == 1)
                {
                    string libraryPath = libPath.First().Substring(3);
                    LispUtils.LibraryPath = libraryPath;
                    allArgs.Remove(libPath.First());
                }
                else
                {
                    output.WriteLine("Error: only one library path is supported");
                    return;
                }
            }

            // handle options for compiler
            if (ContainsOptionAndRemove(allArgs, "-c"))
            {
                compile = true;
            }
            if (ContainsOptionAndRemove(allArgs, "-s"))
            {
                showCompileOutput = true;
            }

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

            var scriptFiles = LispUtils.GetScriptFilesFromProgramArgs(args);

            // check if all command line options could be consumed
            allArgs = allArgs.Where(x => !scriptFiles.Contains(x)).ToList();    // remove script files from option list
            if (allArgs.Count > 0)
            {
                output.WriteLine($"Error: unknown option(s) {LispUtils.DumpEnumerable(allArgs, " ")}");
                return;
            }

            if (debugger != null)
            {
                debugger.SetInputOutputStreams(output, input);
                if (interactiveLoop)
                {
                    InteractiveLoopHeader(output);
                    debugger.InteractiveLoop(startedFromMain: true, tracing: trace);
                    loadFiles = false;
                    wasDebugging = true;
                }
                if (startDebugger)
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
                        result = Lisp.SaveEval(script, moduleName: fileName, verboseErrorOutput: lengthyErrorOutput, tracing: trace, onlyMacroExpand: macroExpand);
                    }
                }
            }
            else if (script != null && !wasDebugging)
            {
                // process -e option
                result = Lisp.SaveEval(script, onlyMacroExpand: macroExpand);
            }

            if (macroExpand)
            {
                output.WriteLine("Macro expand: " + result);
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
            output.WriteLine("  -v             : show version");
            output.WriteLine("  -h             : show help");
            output.WriteLine("  -e \"script\"    : execute given script");
            output.WriteLine("  -l=\"path\"      : path to library");
            output.WriteLine("  --doc          : show language documentation");
            output.WriteLine("  --html         : show language documentation in html");
            output.WriteLine("  --macro-expand : expand all macros and show resulting code");
            output.WriteLine("  -m             : measure execution time");
            output.WriteLine("  -t             : enable tracing");
            output.WriteLine("  -x             : exhaustive error output");
            if (TryGetDebugger() != null)
            {
                output.WriteLine("  -i             : interactive shell");
                output.WriteLine("  -d             : start debugger");
            }
            else
            {
                output.WriteLine();
                output.WriteLine("Info: no debugger support installed !");
            }
            if (TryGetCompiler() != null)
            {
                output.WriteLine("  -c             : compile program");
                output.WriteLine("  -s             : show C# compiler output");
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
