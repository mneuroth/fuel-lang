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

namespace CsLisp
{
    /// <summary>
    /// The debugger module for FUEL. 
    /// Enabled command line debugging for FUEL.
    /// </summary>
    public class LispDebugger : ILispDebugger
    {
        #region constants

        private const string Prompt = Lisp.Name + "> ";

        private const string DbgPrompt = Lisp.Name + "-DBG> ";

        #endregion

        #region properties

        private bool IsProgramStop { get; set; }

        private Func<LispScope, bool> IsStopStepFcn { get; set; }

        private List<LispBreakpointInfo> Breakpoints { get; set; }

        private string CommandLineScript { get; set; }

        private TextWriter Output { get; set; }

        private TextReader Input { get; set; }

        #endregion

        #region constructor

        /// <summary>
        /// Initializes a new instance of the <see cref="LispDebugger"/> class.
        /// </summary>
        /// <remarks>
        /// Public constructor needed for dynamic loading in fuel.exe.
        /// </remarks>
        public LispDebugger()
        {
            Breakpoints = new List<LispBreakpointInfo>();
            Output = Console.Out;
            Input = Console.In;
            CommandLineScript = string.Empty;
            Reset();
        }

        #endregion

        #region public methods

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
        public static bool InteractiveLoop(LispDebugger debugger = null, LispScope initialTopScope = null, bool startedFromMain = false, bool tracing = false)
        {
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
                cmd = cmd != null ? cmd.Trim() : null;

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
                    LispUtils.ShowAbout(debugger.Output);
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
                else if (cmd.Equals("doc"))
                {
                    globalScope.DumpBuiltinFunctionsHelp();
                }
                else if (cmd.Equals("modules"))
                {
                    globalScope.DumpModules();
                }
                else if (cmd.StartsWith("clear"))
                {
                    ClearBreakpoints(debugger, cmd);
                }
                else if (cmd.Equals("stack") || cmd.StartsWith("k"))
                {
                    topScope.DumpStack(currentScope.GetCallStackSize());
                }
                else if (cmd.Equals("code") || cmd.StartsWith("c"))
                {
                    var script = LispUtils.ReadFileOrEmptyString(currentScope.ModuleName);
                    // use the script given on command line if no valid module name was set
                    if (string.IsNullOrEmpty(script))
                    {
                        script = debugger.CommandLineScript;
                    }
                    ShowSourceCode(debugger, script, currentScope.ModuleName, currentScope.CurrentLineNo);
                }
                else if (cmd.StartsWith("list") || cmd.StartsWith("t"))
                {
                    debugger.ShowBreakpoints();
                }
                else if (cmd.StartsWith("break ") || cmd.StartsWith("b "))
                {
                    AddBreakpoint(debugger, cmd, currentScope.ModuleName);
                }
                else if (cmd.Equals("up") || cmd.StartsWith("u"))
                {
                    if (currentScope.Next != null)
                    {
                        currentScope = currentScope.Next;
                    }
                }
                else if (cmd.Equals("down") || cmd.StartsWith("d"))
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
                else if (cmd.Equals("locals") || cmd.StartsWith("l"))
                {
                    currentScope.DumpVars();
                }
                else if (cmd.Equals("globals") || cmd.StartsWith("g"))
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
                        LispVariant result = Lisp.Eval(cmd, currentScope, currentScope.ModuleName);
                        debugger.Output.WriteLine("result=" + result);
                    }
                    catch (Exception ex)
                    {
                        debugger.Output.WriteLine("Exception: " + ex.Message);
                    }
                }
            } while (!bContinueWithNextStatement);

            return bRestart;
        }

        #endregion

        #region LispDebuggerInterface

        /// <summary>
        /// See interface.
        /// </summary>
        public void InteractiveLoop(LispScope initialTopScope, IList<object> currentAst = null, bool startedFromMain = false, bool tracing = false)
        {
            if (currentAst != null)
            {
                var lineNumber = initialTopScope != null ? initialTopScope.CurrentLineNo : -1;
                var startPos = initialTopScope != null ? initialTopScope.CurrentToken.StartPos : -1;
                var stopPos = initialTopScope != null ? initialTopScope.CurrentToken.StopPos : -1;
                var moduleName = initialTopScope != null ? initialTopScope.ModuleName : "?";
                Output.WriteLine("--> " + currentAst[0] + " line=" + lineNumber + " start=" + startPos + " stop=" + stopPos + " module=" + moduleName);
            }
            InteractiveLoop(this, initialTopScope, startedFromMain, tracing);
        }

        /// <summary>
        /// See interface.
        /// </summary>
        public bool NeedsBreak(LispScope scope, LispBreakpointPosition posInfosOfCurrentAstItem)
        {
            if ((IsProgramStop && IsStopStepFcn(scope)) || HitsBreakpoint(posInfosOfCurrentAstItem.Item3, scope.ModuleName, scope))
            {
                IsProgramStop = false;
                return true;
            }
            return false;
        }

        /// <summary>
        /// See interface.
        /// </summary>
        public LispVariant DebuggerLoop(string script, string moduleName, bool tracing = false)
        {
            LispVariant result = null;
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
                    result = Lisp.Eval(script, globalScope, moduleName, tracing: tracing);
                    Reset();
                }
                catch (LispStopDebuggerException)
                {
                    bRestart = false;
                }
                catch (Exception exception)
                {
                    Output.WriteLine("\nException: {0}", exception);
                    string stackInfo = exception.Data.Contains(LispUtils.StackInfo) ? (string)exception.Data[LispUtils.StackInfo] : string.Empty;
                    Output.WriteLine("\nStack:\n{0}", stackInfo);
                    bRestart = InteractiveLoop(this, globalScope, startedFromMain: true);
                }

                if (bRestart)
                {
                    Output.WriteLine("restart program");

                    // process empty script --> just start interactive loop
                    if (result == null)
                    {
                        bRestart = InteractiveLoop(this, globalScope, startedFromMain: true);
                    }
                }

                globalScope.Debugger = null;
            }

            return result;
        }

        /// <summary>
        /// See interface.
        /// </summary>
        public void SetInputOutputStreams(TextWriter output, TextReader input)
        {
            Output = output;
            Input = input;            
        }

        #endregion

        #region private methods

        private void Reset()
        {
            IsProgramStop = true;
            IsStopStepFcn = (scope) => true;
        }

        private bool HitsBreakpoint(int lineNo, string moduleName, LispScope scope)
        {
            foreach (var breakpoint in Breakpoints)
            {
                bool isSameModule = IsSameModule(breakpoint.ModuleName, scope != null ? scope.ModuleName : moduleName);
                if (isSameModule && (lineNo == breakpoint.LineNo))
                {
                    if (breakpoint.Condition.Length > 0 && scope != null)
                    {
                        try
                        {
                            LispVariant result = Lisp.Eval(breakpoint.Condition, scope, scope.ModuleName);
                            return result.BoolValue;
                        }
                        catch
                        {
                            Output.WriteLine("Error: bad condition for line {0}: {1}", breakpoint.LineNo, breakpoint.Condition);
                            return false;
                        } 
                    }
                    return true;
                }
            }
            return false;
        }

        private bool HasBreakpointAt(int lineNo, string moduleName)
        {
            return HitsBreakpoint(lineNo, moduleName, null);
        }

        private void AddBreakpoint(int lineNo, string moduleName, string condition)
        {
            var newItem = new LispBreakpointInfo(lineNo, moduleName, condition);
            var index = Breakpoints.FindIndex(elem => (elem.LineNo == lineNo) && (elem.ModuleName == moduleName));
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

        private bool ClearBreakpoint(int no)
        {
            var index = no - 1;
            if (index >= 0 && index < Breakpoints.Count())
            {
                Breakpoints.RemoveAt(index);
                return true;
            }
            return false;
        }

        private void ClearAllBreakpoints()
        {
            Breakpoints.Clear();
        }

        // ReSharper disable once UnusedParameter.Local
        private void DoStep(LispScope currentScope)
        {
            IsStopStepFcn = (scope) => true;
            IsProgramStop = true;
        }

        private void DoStepOver(LispScope currentScope)
        {
            var currentCallStackSize = currentScope.GetCallStackSize();
            IsStopStepFcn = (scope) => currentCallStackSize >= scope.GetCallStackSize();
            IsProgramStop = true;
        }

        private void DoStepOut(LispScope currentScope)
        {
            var currentCallStackSize = currentScope.GetCallStackSize();
            IsStopStepFcn = (scope) => currentCallStackSize - 1 >= scope.GetCallStackSize();
            IsProgramStop = true;
        }

        private void DoRun()
        {
            IsProgramStop = false;
        }

        private void ShowBreakpoints()
        {
            Output.WriteLine("Breakpoints:");
            var no = 1;
            foreach (var breakpoint in Breakpoints)
            {
                Output.WriteLine("#{0,-3} line={1,-5} module={2,-25} condition={3}", no, breakpoint.LineNo, breakpoint.ModuleName, breakpoint.Condition);
                no++;
            }
        }

        private static bool IsSameModule(string moduleName1, string moduleName2)
        {
            // if one module name is not set --> handle as same module
            if (string.IsNullOrEmpty(moduleName1) || string.IsNullOrEmpty(moduleName2))
            {
                return true;
            }

            // compare only with file name, ignore the path
            var module1 = new FileInfo(moduleName1);
            var module2 = new FileInfo(moduleName2);
            return module1.Name.Equals(module2.Name);
        }

        private static void ShowSourceCode(LispDebugger debugger, string sourceCode, string moduleName, int? currentLineNo)
        {
            if (debugger != null)
            {
                string[] sourceCodeLines = sourceCode.Split('\n');
                for (var i = 0; i < sourceCodeLines.Length; i++)
                {
                    string breakMark = debugger.HasBreakpointAt(i + 1, moduleName) ? "B " : "  ";
                    string mark = currentLineNo != null && currentLineNo.Value == i + 1 ? "-->" : String.Empty;
                    debugger.Output.WriteLine("{0,3} {1,2} {2,3} {3}", i + 1, breakMark, mark, sourceCodeLines[i]);
                }
            }
        }

        private static void ClearBreakpoints(LispDebugger debugger, string cmd)
        {
            if (debugger != null)
            {
                string rest = cmd.Substring(5).Trim();
                if (rest.Length > 0)
                {
                    Tuple<bool, int> val = ConvertToInt(rest);
                    if (!val.Item1 || !debugger.ClearBreakpoint(val.Item2))
                    {
                        debugger.Output.WriteLine("Warning: no breakpoint cleared");
                    }
                }
                else
                {
                    debugger.Output.WriteLine("Really delete all breakpoints? (y/n)");
					string answer;
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
        private static void AddBreakpoint(LispDebugger debugger, string cmd, string currentModuleName)
        {
            bool added = false;
            if (debugger != null)
            {
                var cmdArgs = new string[0];
                string moduleName = currentModuleName;
                string rest = cmd.Substring(cmd.IndexOf(" ", StringComparison.Ordinal)).Trim();
                if (rest.StartsWith("\""))
                {
                    // process: filename:linenumber
                    int iStopPos;
                    moduleName = GetStringLiteral(rest.Substring(1), out iStopPos);
                    rest = rest.Substring(iStopPos + 2); // adjust for the two "
                    if (rest.StartsWith(":"))
                    {
                        cmdArgs = rest.Substring(1).Split(' ');
                    }
                }
                else
                {
                    // process: linennumber
                    cmdArgs = rest.Split(' ');
                }
                int indexRest = rest.IndexOf(" ", StringComparison.Ordinal);
                rest = indexRest >= 0 ? rest.Substring(indexRest).Trim() : String.Empty;
                if (cmdArgs.Length > 0)
                {
                    string lineNumberString = cmdArgs[0];
                    int posModuleSeparator = cmdArgs[0].IndexOf(":", StringComparison.Ordinal);
                    if (posModuleSeparator >= 0)
                    {
                        lineNumberString = cmdArgs[0].Substring(posModuleSeparator + 1);
                        moduleName = cmdArgs[0].Substring(0, posModuleSeparator);
                    }
                    Tuple<bool, int> val = ConvertToInt(lineNumberString);
                    if (val.Item1)
                    {
                        debugger.AddBreakpoint(val.Item2, moduleName, rest.Length > 0 ? rest : String.Empty);
                        added = true;
                    }
                }
            }
            if (!added && debugger != null)
            {
                debugger.Output.WriteLine("Warning: no breakpoint set or modified");
            }
        }

        private static string GetStringLiteral(string text, out int stopPos)
        {
            string result = string.Empty;
            stopPos = text.Length;

            int i = 0;
            while (i < text.Length)
            {
                char ch = text[i];
                if (ch == '"')
                {
                    // string literal is finished, stop loop
                    stopPos = i;
                    break;
                }
                else
                {
                    result += ch;
                }
                i++;
            }

            return result;
        }

        private static void ShowInteractiveCmds(TextWriter output)
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
            output.WriteLine("  doc                          : show documentation about all builtin functions");
            output.WriteLine("  exit                         : exit the interactive loop");
            output.WriteLine();
        }

        private static Tuple<bool, int> ConvertToInt(string value)
        {
            var result = 0;
            var ok = true;
            try
            {
                result = Convert.ToInt32(value);
            }
            catch (FormatException)
            {
                ok = false;
            }
            return new Tuple<bool, int>(ok, result);
        }

        #endregion
    }

    // ********************************************************************
    internal struct LispBreakpointInfo
    {
        internal LispBreakpointInfo(int lineNo, string moduleName, string condition)
            : this()
        {
            LineNo = lineNo;
            ModuleName = moduleName;
            Condition = condition;
        }

        internal int LineNo { get; set; }

        internal string ModuleName { get; set; }

        internal string Condition { get; set; }
    }
}
