/**
    Multi-line comments for documentation.
**/

import LispUtils.CurrentTickCount;
using LispScope;
using LispUtils;
using LispDebugger;

using StringTools;

class Fuel {

    public static function MainExtended(/*string[]*/ args:Array<String>, output:TextWriter, input:TextReader)
    {
        if (args.length == 0)
        {
            Usage(output);
            return;
        }

        var /*List<string>*/ allArgs = args; //.ToList();

        var script:String = null;
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
        var result = new LispVariant(null);
        var startTickCount = CurrentTickCount();
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
        var libPath = args.filter(function (v) { return v.startsWith("-l="); }); 
        //var libPath = args.Where(function (v) { return v.StartsWith("-l="); }).Select(function (v) { return v; }).ToArray(); 
        if (libPath.length > 0)
        {
            if (libPath.length == 1)
            {
                var libraryPath = libPath.First().substr(3);
                LispUtils.LibraryPath = libraryPath;
                ContainsOptionAndRemove(allArgs, libPath.First());
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
        allArgs = allArgs.filter(function (x) { return !scriptFiles.contains(x); });
        //allArgs = allArgs.Where(x => !scriptFiles.Contains(x)).ToList();    // remove script files from option list
        if (allArgs.length > 0)
        {
            output.WriteLine('Error: unknown option(s) ${/*LispUtils.DumpEnumerable(allArgs, " ")*/allArgs}');
            return;
        }

        if (debugger != null)
        {
            debugger.SetInputOutputStreams(output, input);
            if (interactiveLoop)
            {
                InteractiveLoopHeader(output);
                debugger.InteractiveLoop(null, null, /*startedFromMain:*/ true, /*tracing:*/ trace);
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
                result = debugger.DebuggerLoop(script, fileName, /*tracing:*/ trace);
                loadFiles = false;
                wasDebugging = true;
            }
        }

        if (loadFiles)
        {
            for (fileName in scriptFiles)
            {
                script = LispUtils.ReadFileOrEmptyString(fileName);
                /* compiler is not supported yet for haxe environment !
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
                */
                {
                    result = Lisp.SaveEval(script, /*moduleName:*/ fileName, /*verboseErrorOutput:*/ lengthyErrorOutput, /*tracing:*/ trace, /*onlyMacroExpand:*/ macroExpand);
                }
            }
        }
        else if (script != null && !wasDebugging)
        {
            // process -e option
            result = Lisp.SaveEval(script, /*onlyMacroExpand:*/ macroExpand);
//            trace("RESULT:", /*result,*/ result.TypeString, result.ToStr());     //TODO -> remove later
        }

        if (macroExpand)
        {
            output.WriteLine("Macro expand: " + result);
        }
        if (trace)
        {
            output.WriteLine("Result=" + result.ToStr());
        }
        if (measureTime)
        {
            output.WriteLine('Execution time = ${(LispUtils.CurrentTickCount() - startTickCount) /** 0.001*/} s');
        }
    }

    private static function ContainsOptionAndRemove(/*List<string>*/ args:Array<String>, option:String):Bool
    {
        if (args.contains(option))
        {
            args.remove(option);
            return true;
        }

        return false;
    }

    private static function Usage(output:TextWriter)
    {
        ShowAbout(output);
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

        
    /// <summary>
    /// Show the version of this FUEL interpreter.
    /// </summary>
    /// <param name="output">The output stream.</param>
    public static function ShowVersion(output:TextWriter)
    {
      output.WriteLine();
      output.WriteLine(Lisp.Name + " " + Lisp.Version + " (for " + Lisp.Platform + ") from " + Lisp.Date + ", " + Lisp.Copyright);
      output.WriteLine();
    }

    /// <summary>
    /// Show informations about this FUEL interperter.
    /// </summary>
    /// <param name="output">The output stream.</param>
    public static function ShowAbout(output:TextWriter)
    {
      ShowVersion(output);
      output.WriteLine(Lisp.Info);
      output.WriteLine();
    }

    public static function TryGetDebugger():Dynamic
    {
        return new LispDebugger();
    }

    public static function TryGetCompiler():Dynamic
    {
        return null;
    }
    
    private static function InteractiveLoopHeader(output:TextWriter):Void
    {
        LispUtils.ShowVersion(output);
        output.WriteLine("Type \"help\" for informations.");
        output.WriteLine();
    }
    
  static public function main():Void {
      // see: https://haxe.org/manual/lf-target-defines.html
      var args:Array<String>;
#if eval
      // haxe --class-path src --main Fuel --interp
      var test_args = new Array<String>();
      //test_args.push("-h");
      //test_args.push("-d");
      //test_args.push("-l=asdf");
      //test_args.push("test.fuel");
      test_args.push("-e");
      //test_args.push("(import fuellib) (defn isMyHaxe () (return -2)) (isMyHaxe) (isHaxe)");  //(+ 1 2 3)
      //(def x 1.0) (setf x (+ x 1)) (typestr x)
      test_args.push("(def x 1) (typestr x)");
      test_args.push("-t");
      args = test_args;
#else      
#if (node || sys)
      var org_args = Sys.args();
      args = org_args;
#else
      var org_args = new Array<String>();
      args = org_args;
#end
#end
      MainExtended(args, new TextWriter(), new TextReader());
  }
}
