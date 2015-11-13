using System;
using System.Linq;

namespace CsLisp
{
    /// <summary>
    /// Fast Usable Embeddable Lisp Interpreter and Compiler (FUEL).
    /// </summary>
    public class Fuel
    {
        public static void Main(string[] args)
        {
            if (args.Length == 0)
            {
                Usage();
                return;
            }

            var doNotLoadFiles = false;
            var compile = false;
            var showCompileOutput = false;
            var measureTime = false;
            var result = new LispVariant();
            var startTickCount = Environment.TickCount;

            if (args.Contains("-m"))
            {
                measureTime = true;
            }
            if (args.Contains("-v"))
            {
                Console.WriteLine(Lisp.Version);
                return;
            }
            if (args.Contains("-h"))
            {
                Usage();
                return;
            }
            if (args.Contains("-i"))
            {
                InteractiveLoopHeader();
                LispDebugger.InteractiveLoop();
                doNotLoadFiles = true;
            }
            if (args.Contains("-e"))
            {
                var script = LispUtils.GetScriptFilesFromProgramArgs(args).FirstOrDefault();
                result = Lisp.Eval(script);
                doNotLoadFiles = true;
            }
            if (args.Contains("-d"))
            {
                InteractiveLoopHeader();
                result = LispDebugger.DebuggerLoop(args);
                doNotLoadFiles = true;
            }
            if (args.Contains("-c"))
            {
                compile = true;
            }
            if (args.Contains("-s"))
            {
                showCompileOutput = true;
            }

            if (!doNotLoadFiles)
            {
                var scriptFiles = LispUtils.GetScriptFilesFromProgramArgs(args);

                foreach (var fileName in scriptFiles)
                {
                    var script = LispUtils.ReadFile(fileName);
                    if (compile)
                    {
                        result = LispCompiler.CompileToExe(script, fileName + ".exe");
                    }
                    else if (showCompileOutput)
                    {
                        result = LispCompiler.CompileToCsCode(script);
                        Console.WriteLine(result.StringValue);
                    }
                    else
                    {
                        result = Lisp.SaveEval(script);
                    }
                }
            }

            if (args.Contains("-t"))
            {
                Console.WriteLine("Result=" + result);
            }
            if (measureTime)
            {
                Console.WriteLine("Execution time = {0} s", (Environment.TickCount - startTickCount) * 0.001);
            }
        }

        #region private methods

        private static void Usage()
        {
            LispUtils.ShowAbout();
            Console.WriteLine("usage:");
            Console.WriteLine(">" + Lisp.ProgramName + " [options] [script_file_name]");
            Console.WriteLine();
            Console.WriteLine("options:");
            Console.WriteLine("  -v          : show version");
            Console.WriteLine("  -h          : show help");
            Console.WriteLine("  -i          : interactive shell");
            Console.WriteLine("  -t          : enable tracing");
            Console.WriteLine("  -d          : start debugger");
            Console.WriteLine("  -c          : compile program");
            Console.WriteLine("  -s          : show C# compiler output");
            Console.WriteLine("  -m          : measure execution time");
            Console.WriteLine("  -e \"script\" : execute given script");
            Console.WriteLine();
        }

        private static void InteractiveLoopHeader()
        {
            LispUtils.ShowVersion();
            Console.WriteLine("Type \"help\" for informations.");
            Console.WriteLine();
        }

        #endregion
    }
}
