/*
 * FUEL(isp) is a fast usable embeddable lisp interpreter.
 *
 * Copyright (c) 2023 Michael Neuroth
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

package; 

using StringTools;

using LispUtils;

import LispUtils;
import LispScope;
import LispInterpreter;

/// <summary>
/// The debugger module for FUEL. 
/// Enabled command line debugging for FUEL.
/// </summary>
/*public*/ class LispDebugger //: ILispDebugger
{
    private static /*const string*/ var Prompt = Lisp.Name + "> ";

    private static /*const string*/ var DbgPrompt = Lisp.Name + "-DBG> ";

    private var IsProgramStop:Bool;  // { get; set; }

    private /*Func<LispScope, bool>*/ var IsStopStepFcn:Dynamic;  // { get; set; }

    private /*List<LispBreakpointInfo>*/ var Breakpoints:Array<LispBreakpointInfo>;  // { get; set; }

    private var CommandLineScript:String;  // { get; set; }

    private var Output:TextWriter;  // { get; set; }

    private var Input:TextReader;  // { get; set; }

    /// <summary>
    /// Initializes a new instance of the <see cref="LispDebugger"/> class.
    /// </summary>
    /// <remarks>
    /// Public constructor needed for dynamic loading in fuel.exe.
    /// </remarks>
    public function new()
    {
        Breakpoints = new Array<LispBreakpointInfo>();
        Output = new TextWriter();  //Console.Out;
        Input = new TextReader();  //Console.In;
        CommandLineScript = "";  //string.Empty;
        Reset();
    }

    /// <summary>
    /// Processing of the interactive loop of the debugger.
    /// </summary>
    /// <param name="debugger">The debugger.</param>
    /// <param name="initialTopScope">The initial top scope.</param>
    /// <param name="startedFromMain">if set to <c>true</c> [started from main].</param>
    /// <param name="tracing">if set to <c>true</c> tracing is enabled.</param>
    /// <returns>True if program should be restarted.</returns>
    /// <exception cref="LispStopDebuggerException"></exception>
    /// <exception cref="CsLisp.LispStopDebuggerException"></exception>
    public static function InteractiveLoopStatic(debugger:LispDebugger = null, initialTopScope:LispScope = null, startedFromMain:Bool = false, tracing:Bool = false):Bool
    {
        var interactiveScript = "";  //string.Empty;

        startedFromMain = startedFromMain || debugger == null;
        if (debugger == null)
        {
            debugger = new LispDebugger();
        }
        var globalScope = initialTopScope != null ? initialTopScope.GlobalScope : LispEnvironment.CreateDefaultScope();
        // do not switch off tracing if already enabled
        if (!globalScope.Tracing)
        {
            globalScope.Tracing = tracing;                
        }
        var topScope = initialTopScope != null ? initialTopScope : globalScope;
        var currentScope = topScope;
        var bContinueWithNextStatement = false;
        var bRestart = false;
        do
        {
             debugger.Output.Write(debugger != null ? DbgPrompt : Prompt);

            // Warning:
            // QProcess and .NET >3.5 does not work correclty reading from input !!!
            // see: http://www.qtcentre.org/threads/62415-QProcess-not-communicating-with-net-framework-gt-3-5
            // ==> CsLisp is now using .NET 3.5 !
            var cmd = debugger.Input.ReadLine();
            cmd = cmd != null ? cmd.trim() : null;

            if (cmd == null || cmd.Equals("exit") || cmd.Equals("quit") || cmd.Equals("q"))
            {
                bContinueWithNextStatement = true;
                bRestart = false;
                if (!startedFromMain)
                {
                    throw new LispStopDebuggerException();
                }
            }
            else if (cmd.Equals("help") || cmd.Equals("h"))
            {
                ShowInteractiveCmds(debugger.Output);
            }
            else if (cmd.Equals("about"))
            {
                ShowAbout(debugger.Output);
            }
            else if (cmd.Equals("funcs"))
            {
                globalScope.DumpFunctions();
            }
            else if (cmd.Equals("macros"))
            {
                globalScope.DumpMacros();
            }
            else if (cmd.Equals("builtins"))
            {
                globalScope.DumpBuiltinFunctions();
            }
            else if (cmd.startsWith("doc"))
            {
                var items = cmd.Split(' ');
                if (items.length > 1)
                {
                    var docCmd = "(doc '" + items[1] + ")";
                    var result = Lisp.Eval(docCmd, currentScope, currentScope.ModuleName);
                    debugger.Output.WriteLine('${result}');
                }
                else
                {
                    globalScope.DumpBuiltinFunctionsHelp();                        
                }
            }
            else if (cmd.startsWith("searchdoc"))
            {
                var items = cmd.Split(' ');
                if (items.length > 1)
                {
                    var docCmd = "(searchdoc '" + items[1] + ")";
                    var result = Lisp.Eval(docCmd, currentScope, currentScope.ModuleName);
                    debugger.Output.WriteLine('${result}');
                }
                else
                {
                    globalScope.DumpBuiltinFunctionsHelp();
                }
            }
            else if (cmd.Equals("modules"))
            {
                globalScope.DumpModules();
            }
            else if (cmd.startsWith("clear"))
            {
                ClearBreakpoints(debugger, cmd);
            }
            else if (cmd.Equals("stack") || cmd.startsWith("k"))
            {
                topScope.DumpStack(currentScope.GetCallStackSize());
            }
            else if (cmd.Equals("code") || cmd.startsWith("c"))
            {
                var script = "";  //string.Empty;
                var moduleName = currentScope.ModuleName;
                if (moduleName == null)
                {
                    script = interactiveScript;
                }
                else
                {
                    script = moduleName.startsWith(LispEnvironment.EvalStrTag) ? moduleName.substr(LispEnvironment.EvalStrTag.length + moduleName.indexOf(":", LispEnvironment.EvalStrTag.length)) : LispUtils.ReadFileOrEmptyString(moduleName);
                }
                // use the script given on command line if no valid module name was set
                if (LispUtils.IsNullOrEmpty(script))
                {
                    script = debugger.CommandLineScript;
                }
                ShowSourceCode(debugger, script, currentScope.ModuleName, currentScope.CurrentLineNo);
            }
            else if (cmd.startsWith("list") || cmd.startsWith("t"))
            {
                debugger.ShowBreakpoints();
            }
            else if (cmd.startsWith("break ") || cmd.startsWith("b "))
            {
                AddBreakpointStatic(debugger, cmd, currentScope.ModuleName);
            }
            else if (cmd.Equals("up") || cmd.startsWith("u"))
            {
                if (currentScope.Next != null)
                {
                    currentScope = currentScope.Next;
                }
            }
            else if (cmd.Equals("down") || cmd.startsWith("d"))
            {
                if (currentScope.Previous != null)
                {
                    currentScope = currentScope.Previous;
                }
            }
            else if (cmd.Equals("step") || cmd.Equals("s"))
            {
                debugger.DoStep(currentScope);
                bContinueWithNextStatement = true;
            }
            else if (cmd.Equals("over") || cmd.Equals("v"))
            {
                debugger.DoStepOver(currentScope);
                bContinueWithNextStatement = true;
            }
            else if (cmd.Equals("out") || cmd.Equals("o"))
            {
                debugger.DoStepOut(currentScope);
                bContinueWithNextStatement = true;
            }
            else if (cmd.Equals("run") || cmd.Equals("r"))
            {
                debugger.DoRun();
                bContinueWithNextStatement = true;
            }
            else if (cmd.Equals("locals") || cmd.startsWith("l"))
            {
                currentScope.DumpVars();
            }
            else if (cmd.Equals("globals") || cmd.startsWith("g"))
            {
                globalScope.DumpVars();
            }
            else if (cmd.Equals("restart"))
            {
                bContinueWithNextStatement = true;
                bRestart = true;
            }
            else if (cmd.Equals("version") || cmd.Equals("ver"))
            {
                LispUtils.ShowVersion(debugger.Output);
            }
            else
            {
                try
                {
                    var result = Lisp.Eval(cmd, currentScope, currentScope.ModuleName);
                    debugger.Output.WriteLine('result=${result.ToStr()}');
                    interactiveScript += cmd + '\n';
                }
                catch (ex:haxe.Exception)
                {
                    debugger.Output.WriteLine("Exception: " + Std.string(ex)/*.Message*/);
                }
            }
        } while (!bContinueWithNextStatement);

        return bRestart;
    }

    /// <summary>
    /// See interface.
    /// </summary>
    public function InteractiveLoop(initialTopScope:LispScope = null, /*IList<object>*/ currentAst:Array<Dynamic> = null, startedFromMain:Bool = false, tracing:Bool = false):Void
    {
        if (currentAst != null)
        {
            var lineNumber = initialTopScope != null ? initialTopScope.CurrentLineNo : -1;
            var startPos = initialTopScope != null ? initialTopScope.CurrentToken.StartPos : -1;
            var stopPos = initialTopScope != null ? initialTopScope.CurrentToken.StopPos : -1;
            var moduleName = initialTopScope != null ? initialTopScope.ModuleName : "?";
            var tokenTxt = initialTopScope != null ? initialTopScope.CurrentToken.ToStr() : "?";
            Output.WriteLine("--> " + currentAst[0].ToStr() + " line=" + lineNumber + " start=" + startPos + " stop=" + stopPos + " module=" + moduleName + " token=" + tokenTxt);
        }
        InteractiveLoopStatic(this, initialTopScope, startedFromMain, tracing);
    }

    /// <summary>
    /// See interface.
    /// </summary>
    public function NeedsBreak(scope:LispScope, posInfosOfCurrentAstItem:LispBreakpointPosition):Bool
    {
        if ((IsProgramStop && IsStopStepFcn(scope)) || HitsBreakpoint(posInfosOfCurrentAstItem.value3, scope.ModuleName, scope))
        {
            IsProgramStop = false;
            return true;
        }
        return false;
    }

    /// <summary>
    /// See interface.
    /// </summary>
    public function DebuggerLoop(script:String, moduleName:String, tracing:Bool = false):LispVariant
    {
        var result:LispVariant = null;
        var bRestart = true;
        while (bRestart)
        {
            // save the source code if the script is transfered via command line
            if (moduleName == LispUtils.CommandLineModule)
            {
                CommandLineScript = script;
            }

            var globalScope = LispEnvironment.CreateDefaultScope();
            globalScope.Input = Input;
            globalScope.Output = Output;
            globalScope.Debugger = this;                

            try
            {
                result = Lisp.Eval(script, globalScope, moduleName, /*tracing:*/ tracing);
                Reset();
            }
            // catch (exc:haxe.Exception)
            // {
            //     trace(":::::::::::::::::::",exc);
            // }
            catch (exc:LispStopDebuggerException)
            {
                bRestart = false;
            }
            catch (exc:/*haxe.*/LispException)
            {
                //trace(exc);
                Output.WriteLine('\nException: ${exc}');
                var stackInfo = exc.ExcData.exists(LispUtils.StackInfo) ? cast(exc.ExcData.get(LispUtils.StackInfo), String) : ""/*string.Empty*/;
                Output.WriteLine('\nStack:\n${stackInfo}');
                bRestart = InteractiveLoopStatic(this, globalScope, /*tartedFromMain:*/ true);
            }

            if (bRestart)
            {
                Output.WriteLine("restart program");

                // process empty script --> just start interactive loop
                if (result == null)
                {
                    bRestart = InteractiveLoopStatic(this, globalScope, /*startedFromMain:*/ true);
                }
            }

            globalScope.Debugger = null;
        }

        return result;
    }

    /// <summary>
    /// See interface.
    /// </summary>
    public function SetInputOutputStreams(output:TextWriter, input:TextReader):Void
    {
        Output = output;
        Input = input;            
    }

    private function Reset():Void
    {
        IsProgramStop = true;
        IsStopStepFcn = function (scope:LispScope):Bool { return true; };
    }

    private function HitsBreakpoint(lineNo:Int, moduleName:String, scope:LispScope):Bool
    {
        for (breakpoint in Breakpoints)
        {
            var isSameModule = IsSameModule(breakpoint.ModuleName, scope != null ? scope.ModuleName : moduleName);
            if (isSameModule && (lineNo == breakpoint.LineNo))
            {
                if (breakpoint.Condition.length > 0 && scope != null)
                {
                    try
                    {
                        var result = Lisp.Eval(breakpoint.Condition, scope, scope.ModuleName);
                        return result.BoolValue;
                    }
                    catch(exc:haxe.Exception)
                    {
                        Output.WriteLine('Error: bad condition for line ${breakpoint.LineNo}: ${breakpoint.Condition}');
                        return false;
                    } 
                }
                return true;
            }
        }
        return false;
    }

    private function HasBreakpointAt(lineNo:Int, moduleName:String):Bool
    {
        return HitsBreakpoint(lineNo, moduleName, null);
    }

    private function AddBreakpoint(lineNo:Int, moduleName:String, condition:String)
    {
        var newItem = new LispBreakpointInfo(lineNo, moduleName, condition);
        var index = Breakpoints.FindIndex(function (elem:LispBreakpointInfo) { return (elem.LineNo == lineNo) && (elem.ModuleName == moduleName); });
        if (index >= 0)
        {
            // replace existing item for this line
            Breakpoints[index] = newItem; 
        }
        else
        {
            Breakpoints.Add(newItem);
        }
    }

    private function ClearBreakpoint(no:Int):Bool
    {
        var index = no - 1;
        if (index >= 0 && index < Breakpoints.length)
        {
            Breakpoints.RemoveAt(index);
            return true;
        }
        return false;
    }

    private function ClearAllBreakpoints():Void
    {
        Breakpoints.Clear();
    }

    // ReSharper disable once UnusedParameter.Local
    private function DoStep(currentScope:LispScope):Void
    {
        IsStopStepFcn = function (scope:LispScope) { return true; }
        IsProgramStop = true;
    }

    private function DoStepOver(currentScope:LispScope):Void
    {
        var currentCallStackSize = currentScope.GetCallStackSize();
        IsStopStepFcn = function (scope:LispScope) { return (currentCallStackSize >= scope.GetCallStackSize()) && !scope.IsInEval; }
        IsProgramStop = true;
    }

    private function DoStepOut(currentScope:LispScope):Void
    {
        var currentCallStackSize = currentScope.GetCallStackSize();
        IsStopStepFcn = function (scope:LispScope) { return currentCallStackSize - 1 >= scope.GetCallStackSize(); }
        IsProgramStop = true;
    }

    private function DoRun():Void
    {
        IsProgramStop = false;
    }

    private function ShowBreakpoints():Void
    {
        Output.WriteLine("Breakpoints:");
        var no = 1;
        for (breakpoint in Breakpoints)
        {
            Output.WriteLine('#${no} line=${breakpoint.LineNo} module=${breakpoint.ModuleName} condition=${breakpoint.Condition}');  //TODO ,-3 ,-5 ,-25
            no++;
        }
    }

    private static function IsSameModule(moduleName1:String, moduleName2:String):Bool
    {
        // if one module name is not set --> handle as same module
        if (LispUtils.IsNullOrEmpty(moduleName1) || LispUtils.IsNullOrEmpty(moduleName2))
        {
            return true;
        }

//TODO -> implement        
        // compare only with file name, ignore the path
        // var module1 = new FileInfo(moduleName1);
        // var module2 = new FileInfo(moduleName2);
        // return module1.Name.Equals(module2.Name);
        return false;
    }

    private static function ShowSourceCode(debugger:LispDebugger, sourceCode:String, moduleName:String, /*int?*/ currentLineNo:Null<Int>):Void
    {
        if (debugger != null)
        {
            var sourceCodeLines:Array<String> = sourceCode.Split('\n');
            for (i in 0...sourceCodeLines.length)
            {
                var breakMark = debugger.HasBreakpointAt(i + 1, moduleName) ? "B " : "  ";
                var mark = currentLineNo != null && currentLineNo/*.Value*/ == i + 1 ? "-->" : ""/*string.Empty*/;
                debugger.Output.WriteLine('${i+1} ${breakMark} ${mark} ${sourceCodeLines[i]}');     //TODO ,3 ,2 ,3
            }
        }
    }

    private static function ClearBreakpoints(debugger:LispDebugger, cmd:String):Void
    {
        if (debugger != null)
        {
            var rest = cmd.substr(5).trim();
            if (rest.length > 0)
            {
                var /*Tuple<bool, int>*/ val = ConvertToInt(rest);
                if (!val.value1 || !debugger.ClearBreakpoint(val.value2))
                {
                    debugger.Output.WriteLine("Warning: no breakpoint cleared");
                }
            }
            else
            {
                debugger.Output.WriteLine("Really delete all breakpoints? (y/n)");
                var answer:String;
                do
                {
                    answer = debugger.Input.ReadLine();
                    if (answer != null)
                    {
                        answer = answer.ToUpper();
                    }
                } while (!(answer == "Y" || answer == "N" || answer == "YES" || answer == "NO"));
                if (answer == "Y" || answer == "YES")
                {
                    debugger.ClearAllBreakpoints();
                }
            }
        }
    }

    /// <summary>
    /// Adds a breakpoint.
    /// Possible command strings after break:
    /// break 7             --&gt; breakpoint with just one line number in the current module
    /// break modulename:7  --&gt; breakpoint with line number and module name
    /// break 7 condition   --&gt; breakpoint with line number and condition
    /// break modulename:7 condition --&gt; breakpoint with line number, module name and condition
    /// break "modulename with spaces":7 condition --&gt; breakpoint with line number, module name and condition
    /// </summary>
    /// <param name="debugger">The debugger.</param>
    /// <param name="cmd">The command.</param>
    /// <param name="currentModuleName">Name of the current module.</param>
    private static function AddBreakpointStatic(debugger:LispDebugger, cmd:String, currentModuleName:String):Void
    {
        var added = false;
        if (debugger != null)
        {
            var cmdArgs = new Array<String>();  //string[0];
            var moduleName = currentModuleName;
            var rest = cmd.substr(cmd.indexOf(" "/*, StringComparison.Ordinal*/)).trim();
            if (rest.startsWith("\""))
            {
                // process: filename:linenumber
                var iStopPos = new LispUtils.Ref<Int>(0);
                moduleName = GetStringLiteral(rest.substr(1), /*out*/ iStopPos);
                rest = rest.substr(iStopPos.value + 2); // adjust for the two "
                if (rest.startsWith(":"))
                {
                    cmdArgs = rest.substr(1).Split(' ');
                }
            }
            else
            {
                // process: linennumber
                cmdArgs = rest.Split(' ');
            }
            var indexRest = rest.indexOf(" "/*, StringComparison.Ordinal*/);
            rest = indexRest >= 0 ? rest.substr(indexRest).trim() : ""/*string.Empty*/;
            if (cmdArgs.length > 0)
            {
                var lineNumberString = cmdArgs[0];
                var posModuleSeparator = cmdArgs[0].lastIndexOf(":"/*, StringComparison.Ordinal*/);
                if (posModuleSeparator >= 0)
                {
                    lineNumberString = cmdArgs[0].substr(posModuleSeparator + 1);
                    moduleName = cmdArgs[0].substr(0, posModuleSeparator);
                }
                /*Tuple<bool, int>*/var val = ConvertToInt(lineNumberString);
                if (val.value1)
                {
                    debugger.AddBreakpoint(val.value2, moduleName, rest.length > 0 ? rest : ""/*string.Empty*/);
                    added = true;
                }
            }
        }
        if (!added && debugger != null)
        {
            debugger.Output.WriteLine("Warning: no breakpoint set or modified");
        }
    }

    private static function GetStringLiteral(text:String, /*out*/ stopPos:LispUtils.Ref<Int>):String
    {
        var result = "";  //string.Empty;
        stopPos.value = text.length;

        var i = 0;
        while (i < text.length)
        {
            var ch = text.charAt(i);
            if (ch == '"')
            {
                // string literal is finished, stop loop
                stopPos.value = i;
                break;
            }
            result += ch;
            i++;
        }

        return result;
    }

    private static function ShowInteractiveCmds(output:TextWriter):Void
    {
        output.WriteLine();
        output.WriteLine("help for interactive loop:");
        output.WriteLine();
        output.WriteLine("  (h)elp                       : show this help");
        output.WriteLine("  version                      : show of this interpreter");
        output.WriteLine("  about                        : show informations about this interpreter");
        output.WriteLine("  (c)ode                       : show the program code");
        output.WriteLine("  stac(k)                      : show the current call stack");
        output.WriteLine("  (u)p                         : go one step up in call stack");
        output.WriteLine("  (d)own                       : go one step down in call stack");
        output.WriteLine("  (r)un                        : execute the program");
        output.WriteLine("  (s)tep                       : step into function");
        output.WriteLine("  o(v)er                       : step over function");
        output.WriteLine("  (o)ut                        : step out of function");
        output.WriteLine("  (b)reak [module:]line [cond] : set a breakpoint in line no with condition cond");
        output.WriteLine("  clear [no]                   : clears a breakpoint with number no or clears all");
        output.WriteLine("  lis(t)                       : shows all breakpoints");
        output.WriteLine("  restart                      : restart program");
        output.WriteLine("  (l)ocals                     : show all local variables of current scope");
        output.WriteLine("  (g)lobals                    : show all global variables");
        output.WriteLine("  modules                      : show all available modules");
        output.WriteLine("  builtins                     : show all builtin functions");
        output.WriteLine("  funcs                        : show all available functions");
        output.WriteLine("  macros                       : show all available macros");
        output.WriteLine("  doc [function]               : show documentation for all or only given function(s)");
        output.WriteLine("  searchdoc name               : show documentation for function(s) containing name");
        output.WriteLine("  exit                         : exit the interactive loop");
        output.WriteLine();
    }

    private static function /*Tuple<bool, int>*/ ConvertToInt(value:String):TupleReturn<Bool, Int>
    {
        var result = 0;
        var ok = true;
        try
        {
            result = /*Convert.ToInt32*/Std.parseInt(value/*, CultureInfo.InvariantCulture*/);
        }
        catch (FormatException)
        {
            ok = false;
        }
        return new TupleReturn<Bool, Int>(ok, result);
    }
}

// ********************************************************************
/*internal*/ class LispBreakpointInfo
{
    /*internal*/ public function new(lineNo:Int, moduleName:String, condition:String)
    {
        LineNo = lineNo;
        ModuleName = moduleName;
        Condition = condition;
    }

    /*internal*/public var LineNo:Int;  // { get; set; }

    /*internal*/public var ModuleName:String;  // { get; set; }

    /*internal*/public var Condition:String;  // { get; set; }
}

/*public*/ class LispBreakpointPosition extends TripleReturn<Int, Int, Int>
{
    public function new(start:Int, stop:Int, lineNumber:Int)
    {            
        super(start, stop, lineNumber);
    }

    public var Start:Int;
    // {
    //     get
    //     {
    //         return Item1;
    //     }
    // }

    public var Stop:Int;
    // {
    //     get
    //     {
    //         return Item2;
    //     }
    // }

    public var LineNo:Int;
    // {
    //     get
    //     {
    //         return Item3;
    //     }
    // }
}
