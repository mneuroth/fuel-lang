using System;
using System.Collections.Generic;
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

        private string SourceCode { get; set; }

        private bool IsProgramStop { get; set; }

        private Func<LispScope, bool> IsStopStepFcn { get; set; }

        private List<LispBreakpointInfo> Breakpoints { get; set; }

        #endregion

        #region constructor

        private LispDebugger(List<LispBreakpointInfo> breakpoints = null)
        {
            IsProgramStop = true;
            IsStopStepFcn = (scope) => true;
            Breakpoints = breakpoints != null ? breakpoints : new List<LispBreakpointInfo>();
        }

        #endregion

        #region public methods

        /// <summary>
        /// Loop of the debugger.
        /// </summary>
        /// <param name="args">The arguments.</param>
        /// <returns>Value of the last expression</returns>
        public static LispVariant DebuggerLoop(string[] args)
        {
            LispVariant result = null;
            var bRestart = true;
            var breakpoints = new List<LispBreakpointInfo>();
            while (bRestart)
            {
                var script = LispUtils.ReadFile(LispUtils.GetScriptFilesFromProgramArgs(args).FirstOrDefault());
                // if no valid script file name is given, try to use string as script
                if (script == String.Empty && args.Length > 1)
                {
                    script = args[1].Trim(new[] { '"' });
                }

                var globalScope = LispEnvironment.CreateDefaultScope();
                var debugger = new LispDebugger(breakpoints);
                globalScope.Debugger = debugger;
                debugger.SourceCode = script;

                try
                {
                    result = Lisp.Eval(script, globalScope);
                }
                catch (LispStopDebuggerException)
                {
                    bRestart = false;
                }
                catch (Exception exception)
                {
                    Console.WriteLine("Exception: {0}", exception);
                    int? currentLineNo = exception.Data.Contains(LispUtils.LineNo) ? (int?)exception.Data[LispUtils.LineNo] : null;
                    bRestart = InteractiveLoop(debugger, globalScope, currentLineNo, startedFromMain: true);                    
                }

                breakpoints = debugger.Breakpoints;

                if (bRestart)
                {
                    Console.WriteLine("restart program");

                    // process empty script --> just start interactive loop
                    if (result == null)
                    {
                        bRestart = InteractiveLoop(debugger, globalScope, startedFromMain: true);
                    }
                }

                globalScope.Debugger = null;
            }
            return result;
        }

        /// <summary>
        /// Processing of the interactive loop of the debugger.
        /// </summary>
        /// <param name="debugger">The debugger.</param>
        /// <param name="initialTopScope">The initial top scope.</param>
        /// <param name="currentAst">The current ast.</param>
        /// <param name="startedFromMain">if set to <c>true</c> [started from main].</param>
        /// <returns>Value of the last expression</returns>
        /// <exception cref="CsLisp.LispStopDebuggerException"></exception>
        public static bool InteractiveLoop(LispDebugger debugger = null, LispScope initialTopScope = null, int? currentLineNo = null, bool startedFromMain = false)
        {
            startedFromMain = !startedFromMain ? debugger == null : startedFromMain;
            if (debugger == null)
            {
                debugger = new LispDebugger();
            }
            var globalScope = initialTopScope != null ? initialTopScope.GlobalScope : LispEnvironment.CreateDefaultScope();
            var topScope = initialTopScope != null ? initialTopScope : globalScope;
            var currentScope = topScope;
            var bStop = false;
            var bRestart = false;
            do
            {
                Console.Write(debugger != null ? DbgPrompt : Prompt);

                string cmd = Console.ReadLine();
                cmd = cmd != null ? cmd.Trim() : null;

                if (cmd == null || cmd.Equals("exit") || cmd.Equals("quit") || cmd.Equals("q"))
                {
                    bStop = true;
                    bRestart = false;
                    if (!startedFromMain)
                    {
                        throw new LispStopDebuggerException();
                    }
                }
                else if (cmd.Equals("help") || cmd.Equals("h"))
                {
                    ShowInteractiveCmds();
                }
                else if (cmd.Equals("about"))
                {
                    LispUtils.ShowAbout();
                }
                else if (cmd.Equals("funcs"))
                {
                    globalScope.DumpFunctions();
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
                    ShowSourceCode(debugger, currentLineNo);
                }
                else if (cmd.StartsWith("list") || cmd.StartsWith("t"))
                {
                    debugger.ShowBreakpoints();
                }
                else if (cmd.StartsWith("break ") || cmd.StartsWith("b "))
                {
                    AddBreakpoint(debugger, cmd);
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
                    bStop = CheckForStop(globalScope, bStop);
                }
                else if (cmd.Equals("over") || cmd.Equals("v"))
                {
                    debugger.DoStepOver(currentScope);
                    bStop = CheckForStop(globalScope, bStop);
                }
                else if (cmd.Equals("out") || cmd.Equals("o"))
                {
                    debugger.DoStepOut(currentScope);
                    bStop = CheckForStop(globalScope, bStop);
                }
                else if (cmd.Equals("run") || cmd.Equals("r"))
                {
                    debugger.DoRun();
                    bStop = CheckForStop(globalScope, bStop);
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
                    bStop = true;
                    bRestart = true;
                }
                else if (cmd.Equals("version") || cmd.Equals("ver"))
                {
                    LispUtils.ShowVersion();
                }
                else
                {
                    try
                    {
                        LispVariant result = Lisp.Eval(cmd, currentScope);
                        Console.WriteLine("result=" + result);
                    }
                    catch (Exception ex)
                    {
                        Console.WriteLine("Exception: " + ex.Message);
                    }
                }
            } while (!bStop);

            return bRestart;
        }

        #endregion

        #region LispDebuggerInterface

        /// <summary>
        /// See interface.
        /// </summary>
        public void InteractiveLoop(LispScope initialTopScope, IList<object> currentAst = null)
        {
            int? currentLineNo = null;
            if (currentAst != null)
            {
                currentLineNo = LispInterpreter.GetPosInfo(currentAst[0]).Item2;
                Console.WriteLine("--> " + currentAst[0] + " " + LispInterpreter.GetPosInfoString(currentAst[0]));
            }
            InteractiveLoop(this, initialTopScope, currentLineNo);
        }

        /// <summary>
        /// See interface.
        /// </summary>
        public bool NeedsBreak(LispScope scope, Tuple<int, int> posInfosOfCurrentAstItem)
        {
            if ((IsProgramStop && IsStopStepFcn(scope)) || HitsBreakpoint(posInfosOfCurrentAstItem.Item2, scope))
            {
                IsProgramStop = false;
                return true;
            }
            return false;
        }

        #endregion

        #region private methods

        private bool HitsBreakpoint(int lineNo, LispScope scope)
        {
            foreach (var breakpoint in Breakpoints)
            {
                if (lineNo == breakpoint.LineNo)
                {
                    if (breakpoint.Condition.Length > 0 && scope != null)
                    {
                        try
                        {
                            LispVariant result = EvalCondition(breakpoint.Condition, scope);
                            return result.BoolValue;
                        }
                        catch
                        {
                            Console.WriteLine("Error: bad condition for line {0}: {1}", breakpoint.LineNo, breakpoint.Condition);
                            return false;
                        } 
                    }
                    return true;
                }
            }
            return false;
        }

        private bool HasBreakpointAt(int lineNo)
        {
            return HitsBreakpoint(lineNo, null);
        }

        private void AddBreakpoint(int lineNo, string condition)
        {
            var newItem = new LispBreakpointInfo(lineNo, condition);
            int index = Breakpoints.FindIndex(elem => elem.LineNo == lineNo);
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

        private bool ClearBreakpoint(int lineNo)
        {
            var index = Breakpoints.FindIndex(e => e.LineNo == lineNo);
            if (index >= 0)
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

        private void DoStep(LispScope currentScope)
        {
            IsStopStepFcn = (scope) => true;
            IsProgramStop = true;
        }

        private void DoStepOver(LispScope currentScope)
        {
            IsStopStepFcn = (scope) => currentScope.GetCallStackSize() >= scope.GetCallStackSize();
            IsProgramStop = true;
        }

        private void DoStepOut(LispScope currentScope)
        {
            IsStopStepFcn = (scope) => currentScope.GetCallStackSize() - 1 >= scope.GetCallStackSize();
            IsProgramStop = true;
        }

        private void DoRun()
        {
            IsProgramStop = false;
        }

        private void ShowBreakpoints()
        {
            foreach (var breakpoint in Breakpoints)
            {
                Console.WriteLine("line {0,-5} condition: {1}", breakpoint.LineNo, breakpoint.Condition);
            }
        }

        private static void ShowSourceCode(LispDebugger debugger, int? currentLineNo)
        {
            if (debugger != null)
            {
                string[] sourceCodeLines = debugger.SourceCode.Split('\n');
                for (int i = 0; i < sourceCodeLines.Length; i++)
                {
                    string breakMark = debugger.HasBreakpointAt(i + 1) ? "B " : "  ";
                    string mark = currentLineNo != null && currentLineNo.Value == i + 1 ? "-->" : String.Empty;
                    Console.WriteLine("{0,3} {1,2} {2,3} {3}", i + 1, breakMark, mark, sourceCodeLines[i]);
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
                        Console.WriteLine("Warning: no breakpoint cleared");
                    }
                }
                else
                {
                    Console.WriteLine("Really delete all breakpoints? (y/n)");
                    ConsoleKeyInfo key;
                    do
                    {
                        key = Console.ReadKey();
                    } while (char.ToUpper(key.KeyChar) != 'Y' && char.ToUpper(key.KeyChar) != 'N');
                    if (char.ToUpper(key.KeyChar) == 'Y')
                    {
                        debugger.ClearAllBreakpoints();
                    }
                }
            }
        }

        private static void AddBreakpoint(LispDebugger debugger, string cmd)
        {
            if (debugger != null)
            {
                string rest = cmd.Substring(cmd.IndexOf(" ", StringComparison.Ordinal)).Trim();
                string[] cmdArgs = rest.Split(' ');
                int indexRest = rest.IndexOf(" ", StringComparison.Ordinal);
                if (indexRest >= 0)
                {
                    rest = rest.Substring(indexRest).Trim();
                }
                else
                {
                    rest = String.Empty;
                }
                Tuple<bool, int> val = ConvertToInt(cmdArgs[0]);
                if (val.Item1)
                {
                    debugger.AddBreakpoint(val.Item2, rest.Length > 0 ? rest : String.Empty);
                }
                else
                {
                    Console.WriteLine("Warning: no breakpoint set or modified");
                }
            }
        }

        private static bool CheckForStop(LispScope globalScope, bool bStop)
        {
            if (!globalScope.Finished)
            {
                bStop = true;
            }
            return bStop;
        }

        private static LispVariant EvalCondition(string lispCode, LispScope scope)
        {
            var finished = scope.Finished;
            var result = Lisp.Eval(lispCode, scope);
            scope.Finished = finished;
            return result;
        }

        private static void ShowInteractiveCmds()
        {
            Console.WriteLine();
            Console.WriteLine("help for interactive loop:");
            Console.WriteLine();
            Console.WriteLine("  (h)elp            : show this help");
            Console.WriteLine("  version           : show of this interpreter");
            Console.WriteLine("  about             : show informations about this interpreter");
            Console.WriteLine("  (c)ode            : show the program code");
            Console.WriteLine("  stac(k)           : show the current call stack");
            Console.WriteLine("  (u)shift          : go one step up in call stack");
            Console.WriteLine("  (d)own            : go one step down in call stack");
            Console.WriteLine("  (r)un             : execute program");
            Console.WriteLine("  (s)tep            : step into function");
            Console.WriteLine("  o(v)er            : step over function");
            Console.WriteLine("  (o)ut             : step out of function");
            Console.WriteLine("  (b)reak no [cond] : set a breakpoint in line no with condition cond");
            Console.WriteLine("  clear [no]        : clears a breakpoint in line no or clears all");
            Console.WriteLine("  lis(t)            : shows all breakpoints");
            Console.WriteLine("  restart           : restart program");
            Console.WriteLine("  (l)ocals          : show all local variables of current scope");
            Console.WriteLine("  (g)lobals         : show all global variables");
            Console.WriteLine("  funcs             : show all available (buit-in) functions");
            Console.WriteLine("  exit              : exit the interactive loop");
            Console.WriteLine();
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
    internal class LispStopDebuggerException : LispException
    {
        internal LispStopDebuggerException(string text = "")
            : base(text)
        {
        }
    }

    // ********************************************************************
    internal struct LispBreakpointInfo
    {
        internal LispBreakpointInfo(int lineNo, string condition)
            : this()
        {
            LineNo = lineNo;
            Condition = condition;
        }

        internal int LineNo { get; set; }

        internal string Condition { get; set; }
    }
}
