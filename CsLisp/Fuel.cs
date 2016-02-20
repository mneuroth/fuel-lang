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
            Main(args, Console.Out, Console.In);
        }

        public static void Main(string[] args, TextWriter output, TextReader input)
        {
            if (args.Length == 0)
            {
                Usage(output);
                return;
            }

            string script = null;
            var doNotLoadFiles = false;
            var trace = false;
            var compile = false;
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
                output.WriteLine(Lisp.Version);
                return;
            }
            if (args.Contains("-h"))
            {
                Usage(output);
                return;
            }
            if (args.Contains("-l"))
            {
                lengthyErrorOutput = true;
            }
            if (args.Contains("-e"))
            {
                script = LispUtils.GetScriptFilesFromProgramArgs(args).FirstOrDefault();
                doNotLoadFiles = true;
            }
            if (args.Contains("-t"))
            {
                trace = true;
            }

            // handle options for debugger
            if (debugger != null)
            {                
                if (args.Contains("-i"))
                {
                    InteractiveLoopHeader(output);
                    debugger.InteractiveLoop(startedFromMain:true, tracing: trace);
                    doNotLoadFiles = true;
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
                    result = debugger.DebuggerLoop(script, fileName, output, input, tracing: trace);
                    doNotLoadFiles = true;
                }                
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

            if (!doNotLoadFiles)
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
            else if (script != null)
            {
                result = Lisp.Eval(script);
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
            output.WriteLine("  -m          : measure execution time");
            output.WriteLine("  -t          : enable tracing");
            output.WriteLine("  -l          : lengthy error output");
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
            return TryGetClassFromDll<ILispCompiler>("CsLispCompiler.dll", "CsLisp.LispCompiler");
        }

        private static ILispDebugger TryGetDebugger()
        {
            return TryGetClassFromDll<ILispDebugger>("CsLispDebugger.dll", "CsLisp.LispDebugger");
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


    // TODO:
    // (- debuggen: run funktioniert nicht in errorinmodule.fuel
    // ((- debuggen: anzeige module und line no in stack
    // (- debuggen: anzeige des korrekten codes, falls module geladen ist
    // ((- debuggen: set breakpoints in andren modulen realisieren
    // (- debuggen: up/down sollte auch den --> Zeiger anpassen
    // - ist LispScope.Finished und LispScope.SourceCode noch notwendig? 
    // - Makro Behandlung aufraeumen
    // - Quellcode aufraeumen
    // - debuggen: set next statement realisieren?
    // - bug: step out funktioniert anscheinend bei modulen nicht ganz korrekt
    // (- ggf. module als eigenen Scope implementieren --> ###modules###
    // - debuggen: show loaded module names 
    // - debuggen: funcs befehl um module erweitern und anzeige von funktionen in modulen
    // - stdlib um list<object> erweitern, damit man immer mit listen arbeiten kann
    // (- debugger: v (step over) funktioniert nicht so wie erwartet --> haengt bei quote
    // - Behandlung von Variablen im Modulen korrekt realisieren --> sind global nicht sichtbar, nur im Modul selbst --> im debugger anzeigen
    // - unit tests erweitern um neue Features: set breakpoints in modulen, debuggen von modulen, line no anzeige in stack, source code anzeige aktualisierung in up/down
    // - setf macro implementieren...
    // - out funktioniert aus import module nicht korrekt...-?
    // - option -e funktioniert nicht mit -d korrekt
}
