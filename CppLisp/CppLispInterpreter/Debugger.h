#ifndef _DEBUGGER_H
#define _DEBUGGER_H

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
//using System.Collections.Generic;
//using System.IO;
//using System.Linq;

#include "cstypes.h"
#include "DebuggerInterface.h"
#include "Lisp.h"
#include "Environment.h"

//#include <list>
#include <vector>

namespace CsLisp
{
	void ShowAbout(std::shared_ptr<TextWriter> output);
	void ShowVersion(std::shared_ptr<TextWriter> output);
	extern string ReadFileOrEmptyString(const string & fileName);

	// ********************************************************************
	/*internal*/ struct LispBreakpointInfo
	{
	public:
		/*internal*/ LispBreakpointInfo(size_t lineNo, const string & moduleName, const string & condition)
			//    : this()
		{
			LineNo = lineNo;
			ModuleName = moduleName;
			Condition = condition;
		}

		/*internal*/ size_t LineNo; // { get; set; }

		/*internal*/ string ModuleName; // { get; set; }

		/*internal*/ string Condition; // { get; set; }
	};
	
	/// <summary>
    /// The debugger module for FUEL. 
    /// Enabled command line debugging for FUEL.
    /// </summary>
    /*public*/ class LispDebugger : public ILispDebugger
    {
	private:
        //#region constants

        /*private*/ const static string Prompt;

        /*private*/ const static string DbgPrompt;

        //#endregion

        //#region properties

		/*private*/ bool IsProgramStop; // { get; set; }

		/*private*/ std::function<bool(std::shared_ptr<LispScope>)> IsStopStepFcn; // { get; set; }

		/*private*/ std::vector<LispBreakpointInfo> Breakpoints; // { get; set; }

		/*private*/ string CommandLineScript; // { get; set; }

		/*private*/ std::shared_ptr<TextWriter> Output; // { get; set; }

		/*private*/ std::shared_ptr<TextReader> Input; // { get; set; }

        //#endregion

	public:
        //#region constructor

        /// <summary>
        /// Initializes a new instance of the <see cref="LispDebugger"/> class.
        /// </summary>
        /// <remarks>
        /// Public constructor needed for dynamic loading in fuel.exe.
        /// </remarks>
        /*public*/ LispDebugger()
        {
            Breakpoints = std::vector<LispBreakpointInfo>();
            Output = /*Console.Out*/std::make_shared<TextWriter>();
            Input = /*Console.In*/std::make_shared<TextReader>();
            CommandLineScript = string::Empty;
            Reset();
        }

		virtual ~LispDebugger()
		{
		}

        //#endregion

        //#region public methods

        /// <summary>
        /// Processing of the interactive loop of the debugger->
        /// </summary>
        /// <param name="debugger">The debugger-></param>
        /// <param name="initialTopScope">The initial top scope.</param>
        /// <param name="startedFromMain">if set to <c>true</c> [started from main].</param>
        /// <param name="tracing">if set to <c>true</c> tracing is enabled.</param>
        /// <returns>True if program should be restarted.</returns>
        /// <exception cref="LispStopDebuggerException"></exception>
        /// <exception cref="CsLisp.LispStopDebuggerException"></exception>
		/*public*/ static bool InteractiveLoop(LispDebugger * debugger = null, std::shared_ptr<LispScope> initialTopScope = null, bool startedFromMain = false, bool tracing = false, string * pRedirectToString = 0, string * pRedirectFromString = 0)
        {
            startedFromMain = startedFromMain || debugger == null;
            if (debugger == null)
            {
                debugger = new LispDebugger();
            }
            var globalScope = initialTopScope != null ? initialTopScope->GlobalScope : LispEnvironment::CreateDefaultScope();
            // do not switch off tracing if already enabled
            if (!globalScope->Tracing)
            {
                globalScope->Tracing = tracing;                
            }
			if (pRedirectToString != 0)
			{
				globalScope->Output->EnableToString(true);
			}
			if (pRedirectFromString != 0)
			{
				globalScope->Input->SetContent(*pRedirectFromString);
				globalScope->Input->EnableFromString(true);
			}
            var topScope = initialTopScope != null ? initialTopScope : globalScope;
            var currentScope = topScope;
            var bContinueWithNextStatement = false;
            var bRestart = false;
            do
            {
                debugger->Output->Write(debugger != null ? DbgPrompt : Prompt);

                // Warning:
                // QProcess and .NET >3.5 does not work correclty reading from input !!!
                // see: http://www.qtcentre.org/threads/62415-QProcess-not-communicating-with-net-framework-gt-3-5
                // ==> CsLisp is now using .NET 3.5 !
                var cmd = debugger->Input->ReadLine();
                cmd = /*cmd != null ?*/ cmd.Trim() /*: null*/;

                if (/*cmd == null ||*/ cmd.Equals("exit") || cmd.Equals("quit") || cmd.Equals("q"))
                {
                    bContinueWithNextStatement = true;
                    bRestart = false;
                    if (!startedFromMain)
                    {
                        throw LispStopDebuggerException();
                    }
                }
                else if (cmd.Equals("help") || cmd.Equals("h"))
                {
                    ShowInteractiveCmds(debugger->Output);
                }
                else if (cmd.Equals("about"))
                {
                    /*LispUtils.*/ShowAbout(debugger->Output);
                }
                else if (cmd.Equals("funcs"))
                {
                    globalScope->DumpFunctions();
                }
                else if (cmd.Equals("macros"))
                {
                    globalScope->DumpMacros();
                }
                else if (cmd.Equals("builtins"))
                {
                    globalScope->DumpBuiltinFunctions();
                }
                else if (cmd.StartsWith("doc"))
                {
                    var items = cmd.Split(' ');
                    if (items.size() > 1)
                    {
                        string docCmd = "(doc '" + items[1] + ")";
						std::shared_ptr<LispVariant> result = Lisp::Eval(docCmd, currentScope, currentScope->ModuleName);
                        debugger->Output->WriteLine(string("{0}"), result->ToString());
                    }
                    else
                    {
                        globalScope->DumpBuiltinFunctionsHelp();                        
                    }
                }
                else if (cmd.StartsWith("searchdoc"))
                {
                    var items = cmd.Split(' ');
                    if (items.size() > 1)
                    {
                        string docCmd = "(searchdoc '" + items[1] + ")";
                        std::shared_ptr<LispVariant> result = Lisp::Eval(docCmd, currentScope, currentScope->ModuleName);
                        debugger->Output->WriteLine(string("{0}"), result->ToString());
                    }
                    else
                    {
                        globalScope->DumpBuiltinFunctionsHelp();
                    }
                }
                else if (cmd.Equals("modules"))
                {
                    globalScope->DumpModules();
                }
                else if (cmd.StartsWith("clear"))
                {
                    ClearBreakpoints(debugger, cmd);
                }
                else if (cmd.Equals("stack") || cmd.StartsWith("k"))
                {
                    topScope->DumpStack(currentScope->GetCallStackSize());
                }
                else if (cmd.Equals("code") || cmd.StartsWith("c"))
                {
                    var script = /*LispUtils::*/ReadFileOrEmptyString(currentScope->ModuleName);
                    // use the script given on command line if no valid module name was set
                    if (string::IsNullOrEmpty(script))
                    {
                        script = debugger->CommandLineScript;
                    }
                    ShowSourceCode(debugger, script, currentScope->ModuleName, currentScope->CurrentLineNo());
                }
                else if (cmd.StartsWith("list") || cmd.StartsWith("t"))
                {
                    debugger->ShowBreakpoints();
                }
                else if (cmd.StartsWith("break ") || cmd.StartsWith("b "))
                {
                    AddBreakpoint(debugger, cmd, currentScope->ModuleName);
                }
                else if (cmd.Equals("up") || cmd.StartsWith("u"))
                {
                    if (currentScope->Next != null)
                    {
                        currentScope = currentScope->Next;
                    }
                }
                else if (cmd.Equals("down") || cmd.StartsWith("d"))
                {
                    if (currentScope->Previous != null)
                    {
                        currentScope = currentScope->Previous;
                    }
                }
                else if (cmd.Equals("step") || cmd.Equals("s"))
                {
                    debugger->DoStep(currentScope);
                    bContinueWithNextStatement = true;
                }
                else if (cmd.Equals("over") || cmd.Equals("v"))
                {
                    debugger->DoStepOver(currentScope);
                    bContinueWithNextStatement = true;
                }
                else if (cmd.Equals("out") || cmd.Equals("o"))
                {
                    debugger->DoStepOut(currentScope);
                    bContinueWithNextStatement = true;
                }
                else if (cmd.Equals("run") || cmd.Equals("r"))
                {
                    debugger->DoRun();
                    bContinueWithNextStatement = true;
                }
                else if (cmd.Equals("locals") || cmd.StartsWith("l"))
                {
                    currentScope->DumpVars();
                }
                else if (cmd.Equals("globals") || cmd.StartsWith("g"))
                {
                    globalScope->DumpVars();
                }
                else if (cmd.Equals("restart"))
                {
                    bContinueWithNextStatement = true;
                    bRestart = true;
                }
                else if (cmd.Equals("version") || cmd.Equals("ver"))
                {
                    /*LispUtils::*/ShowVersion(debugger->Output);
                }
                else
                {
                    try
                    {
                        std::shared_ptr<LispVariant> result = Lisp::Eval(cmd, currentScope, currentScope->ModuleName);
                        debugger->Output->WriteLine("result={0}", result->ToString());
                    }
                    catch (LispException & ex)
                    {
                        debugger->Output->WriteLine("Exception: " + ex.Message);
                    }
					catch (...)
					{
						debugger->Output->WriteLine("Native Exception");
					}
				}
            } while (!bContinueWithNextStatement);

			if (pRedirectToString != 0)
			{
				*pRedirectToString = globalScope->Output->GetContent();
			}

            return bRestart;
        }

        //#endregion

        //#region LispDebuggerInterface

        /// <summary>
        /// See interface.
        /// </summary>
		/*public*/ virtual void InteractiveLoop(std::shared_ptr<LispScope> initialTopScope, std::shared_ptr<IEnumerable<std::shared_ptr<object>>> currentAst = null, bool startedFromMain = false, bool tracing = false, string * pRedirectToString = 0, string * pRedirectFromString = 0)
        {
            if (currentAst != null)
            {
                var lineNumber = initialTopScope != null ? initialTopScope->CurrentLineNo() : 0;
                var startPos = initialTopScope != null ? initialTopScope->CurrentToken->StartPos : 0;
                var stopPos = initialTopScope != null ? initialTopScope->CurrentToken->StopPos : 0;
                var moduleName = initialTopScope != null ? initialTopScope->ModuleName : "?";
                Output->WriteLine("--> " + (*((*currentAst).begin()))->ToString()/*[0]*/ + " line=" + std::to_string((int)lineNumber) + " start=" + std::to_string((int)startPos) + " stop=" + std::to_string((int)stopPos) + " module=" + moduleName);
            }
            InteractiveLoop(this, initialTopScope, startedFromMain, tracing, pRedirectToString, pRedirectFromString);
        }

        /// <summary>
        /// See interface.
        /// </summary>
		/*public*/ virtual bool NeedsBreak(std::shared_ptr<LispScope> scope, LispBreakpointPosition posInfosOfCurrentAstItem)
        {
            if ((IsProgramStop && IsStopStepFcn(scope)) || HitsBreakpoint(posInfosOfCurrentAstItem.Item3(), scope->ModuleName, scope))
            {
                IsProgramStop = false;
                return true;
            }
            return false;
        }

        /// <summary>
        /// See interface.
        /// </summary>
		/*public*/ virtual std::shared_ptr<LispVariant> DebuggerLoop(const string & script, const string & moduleName, bool tracing = false)
        {
			std::shared_ptr<LispVariant> result = null;
            var bRestart = true;
            while (bRestart)
            {
                // save the source code if the script is transfered via command line
                if (moduleName == /*LispUtils::CommandLineModule*/"command-line")
                {
                    CommandLineScript = script;
                }

                var globalScope = LispEnvironment::CreateDefaultScope();
                globalScope->Input = Input;
                globalScope->Output = Output;
                globalScope->Debugger = this;                

                try
                {
                    result = Lisp::Eval(script, globalScope, moduleName, /*tracing:*/ tracing);
                    Reset();
                }
                catch (LispStopDebuggerException & /*exc*/)
                {
                    bRestart = false;
                }
                catch (LispException & exception)
                {
                    Output->WriteLine("\nException: {0}", exception.ToString());
// TODO --> implement...
                    //string stackInfo = exception.Data.Contains(/*LispUtils.StackInfo*/"StackInfo") ? (string)exception.Data[/*LispUtils.StackInfo*/"StackInfo"] : string::Empty;
					string stackInfo = "<TODO>";
                    Output->WriteLine("\nStack:\n{0}", stackInfo);
                    bRestart = InteractiveLoop(this, globalScope, /*startedFromMain:*/ true);
                }
				catch (...)
				{
					Output->WriteLine("\nNative Exception...");
				}

                if (bRestart)
                {
                    Output->WriteLine("restart program");

                    // process empty script --> just start interactive loop
                    if (result == null)
                    {
                        bRestart = InteractiveLoop(this, globalScope, /*startedFromMain:*/ true);
                    }
                }

                globalScope->Debugger = null;
            }

            return result;
        }

        /// <summary>
        /// See interface.
        /// </summary>
        /*public*/ void SetInputOutputStreams(std::shared_ptr<TextWriter> output, std::shared_ptr<TextReader> input)
        {
            Output = output;
            Input = input;            
        }

        //#endregion

	private:
        //#region private methods

		/*private*/ void Reset()
        {
            IsProgramStop = true;
			IsStopStepFcn = [](std::shared_ptr<LispScope> scope) -> bool { return true; };
        }

		/*private*/ bool HitsBreakpoint(size_t lineNo, const string & moduleName, std::shared_ptr<LispScope> scope)
        {
            for (var breakpoint : Breakpoints)
            {
                bool isSameModule = IsSameModule(breakpoint.ModuleName, scope != null ? scope->ModuleName : moduleName);
                if (isSameModule && (lineNo == breakpoint.LineNo))
                {
                    if (breakpoint.Condition.size() > 0 && scope != null)
                    {
                        try
                        {
                            std::shared_ptr<LispVariant> result = Lisp::Eval(breakpoint.Condition, scope, scope->ModuleName);
                            return result->BoolValue();
                        }
                        catch(...)
                        {
                            Output->WriteLine("Error: bad condition for line {0}: {1}", std::to_string(breakpoint.LineNo), breakpoint.Condition);
                            return false;
                        } 
                    }
                    return true;
                }
            }
            return false;
        }

		/*private*/ bool HasBreakpointAt(size_t lineNo, string moduleName)
        {
            return HitsBreakpoint(lineNo, moduleName, null);
        }

		/*private*/ void AddBreakpoint(size_t lineNo, string moduleName, string condition)
        {
            var newItem = /*new*/ LispBreakpointInfo(lineNo, moduleName, condition);
            //var index = Breakpoints.FindIndex(elem => (elem.LineNo == lineNo) && (elem.ModuleName == moduleName));
            var indexIter = std::find_if(Breakpoints.begin(), Breakpoints.end(), [lineNo, moduleName](const LispBreakpointInfo & elem) -> bool { return (elem.LineNo == lineNo) && (elem.ModuleName == moduleName); });
			size_t index = indexIter != Breakpoints.end() ? indexIter - Breakpoints.begin() : -1;
            if (index >= 0)
            {
                // replace existing item for this line
                Breakpoints[index] = newItem; 
            }
            else
            {
                Breakpoints./*Add*/push_back(newItem);
            }
        }

		/*private*/ bool ClearBreakpoint(int no)
        {
            size_t index = no - 1;
            if (index >= 0 && index < Breakpoints.size())
            {
                Breakpoints./*RemoveAt*/erase(Breakpoints.begin()+index);
                return true;
            }
            return false;
        }

		/*private*/ void ClearAllBreakpoints()
        {
            Breakpoints.clear();
        }

        // ReSharper disable once UnusedParameter.Local
		/*private*/ void DoStep(std::shared_ptr<LispScope> currentScope)
        {
            IsStopStepFcn = [](std::shared_ptr<LispScope> scope) -> bool { return true; };
            IsProgramStop = true;
        }

		/*private*/ void DoStepOver(std::shared_ptr<LispScope> currentScope)
        {
            var currentCallStackSize = currentScope->GetCallStackSize();
            IsStopStepFcn = [currentCallStackSize](std::shared_ptr<LispScope> scope) -> bool { return currentCallStackSize >= scope->GetCallStackSize(); };
            IsProgramStop = true;
        }

		/*private*/ void DoStepOut(std::shared_ptr<LispScope> currentScope)
        {
            var currentCallStackSize = currentScope->GetCallStackSize();
            IsStopStepFcn = [currentCallStackSize](std::shared_ptr<LispScope> scope) -> bool { return currentCallStackSize - 1 >= scope->GetCallStackSize(); };
            IsProgramStop = true;
        }

		/*private*/ void DoRun()
        {
            IsProgramStop = false;
        }

		/*private*/ void ShowBreakpoints()
        {
            Output->WriteLine("Breakpoints:");
            var no = 1;
            for (var breakpoint : Breakpoints)
            {
                Output->WriteLine("#{0,-3} line={1,-5} module={2,-25} condition={3}", std::to_string(no), std::to_string(breakpoint.LineNo), breakpoint.ModuleName, breakpoint.Condition);
                no++;
            }
        }

		/*private*/ static bool IsSameModule(const string & moduleName1, const string & moduleName2)
        {
            // if one module name is not set --> handle as same module
            if (string::IsNullOrEmpty(moduleName1) || string::IsNullOrEmpty(moduleName2))
            {
                return true;
            }

            // compare only with file name, ignore the path
//            var module1 = new FileInfo(moduleName1);
//            var module2 = new FileInfo(moduleName2);
//            return module1.Name.Equals(module2.Name);
			return moduleName1 == moduleName2;
        }

		/*private*/ static void ShowSourceCode(LispDebugger * debugger, const string & sourceCode, const string & moduleName, size_t/*?*/ currentLineNo)
        {
            if (debugger != null)
            {
                var sourceCodeLines = sourceCode.Split('\n');
                for (size_t i = 0; i < sourceCodeLines.size(); i++)
                {
                    string breakMark = debugger->HasBreakpointAt(i + 1, moduleName) ? "B " : "  ";
                    string mark = std::to_string(/*currentLineNo != null &&*/ currentLineNo/*.Value*/ == i + 1 /*? "-->" : string::Empty*/);
                    debugger->Output->WriteLine("{0,3} {1,2} {2,3} {3}", std::to_string(i + 1), breakMark, mark, sourceCodeLines[i]);
                }
            }
        }

		/*private*/ static void ClearBreakpoints(LispDebugger * debugger, const string & cmd)
        {
            if (debugger != null)
            {
                string rest = cmd.Substring(5).Trim();
                if (rest.size() > 0)
                {
                    Tuple<bool, int> val = ConvertToInt(rest);
                    if (!val.Item1() || !debugger->ClearBreakpoint(val.Item2()))
                    {
                        debugger->Output->WriteLine("Warning: no breakpoint cleared");
                    }
                }
                else
                {
                    debugger->Output->WriteLine("Really delete all breakpoints? (y/n)");
					string answer;
                    do
                    {
                        answer = debugger->Input->ReadLine();
                        //if (answer != null)
                        {
                            answer = answer.ToUpper();
                        }
					} while (!(answer == "Y" || answer == "N" || answer == "YES" || answer == "NO"));
					if (answer == "Y" || answer == "YES")
                    {
                        debugger->ClearAllBreakpoints();
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
        /// <param name="debugger">The debugger-></param>
        /// <param name="cmd">The command.</param>
        /// <param name="currentModuleName">Name of the current module.</param>
		/*private*/ static void AddBreakpoint(LispDebugger * debugger, const string & cmd, const string & currentModuleName)
        {
            bool added = false;
            if (debugger != null)
            {
                std::vector<string> cmdArgs/* = new string[0]*/;
                string moduleName = currentModuleName;
                string rest = cmd.Substring(cmd.IndexOf(" ", "StringComparison.Ordinal")).Trim();
                if (rest.StartsWith("\""))
                {
                    // process: filename:linenumber
                    size_t iStopPos;
                    moduleName = GetStringLiteral(rest.Substring(1), /*out*/ iStopPos);
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
                size_t indexRest = rest.IndexOf(" ", "StringComparison.Ordinal");
                rest = indexRest >= 0 ? rest.Substring(indexRest).Trim() : string::Empty;
                if (cmdArgs.size() > 0)
                {
                    string lineNumberString = cmdArgs[0];
                    size_t posModuleSeparator = cmdArgs[0].LastIndexOf(":", "StringComparison.Ordinal");
                    if (posModuleSeparator >= 0)
                    {
                        lineNumberString = cmdArgs[0].Substring(posModuleSeparator + 1);
                        moduleName = cmdArgs[0].Substring(0, posModuleSeparator);
                    }
                    Tuple<bool, int> val = ConvertToInt(lineNumberString);
                    if (val.Item1())
                    {
                        debugger->AddBreakpoint(val.Item2(), moduleName, rest.size() > 0 ? rest : string::Empty);
                        added = true;
                    }
                }
            }
            if (!added && debugger != null)
            {
                debugger->Output->WriteLine("Warning: no breakpoint set or modified");
            }
        }
		
		/*private*/ static string GetStringLiteral(const string & text, /*out*/ size_t & stopPos)
        {
            string result = string::Empty;
            stopPos = text.size();

            size_t i = 0;
            while (i < text.size())
            {
                char ch = text[i];
                if (ch == '"')
                {
                    // string literal is finished, stop loop
                    stopPos = i;
                    break;
                }
                result += ch;
                i++;
            }

            return result;
        }

        /*private*/ static void ShowInteractiveCmds(std::shared_ptr<TextWriter> output)
        {
            output->WriteLine();
            output->WriteLine("help for interactive loop:");
            output->WriteLine();
            output->WriteLine("  (h)elp                       : show this help");
            output->WriteLine("  version                      : show of this interpreter");
            output->WriteLine("  about                        : show informations about this interpreter");
            output->WriteLine("  (c)ode                       : show the program code");
            output->WriteLine("  stac(k)                      : show the current call stack");
            output->WriteLine("  (u)p                         : go one step up in call stack");
            output->WriteLine("  (d)own                       : go one step down in call stack");
            output->WriteLine("  (r)un                        : execute the program");
            output->WriteLine("  (s)tep                       : step into function");
            output->WriteLine("  o(v)er                       : step over function");
            output->WriteLine("  (o)ut                        : step out of function");
            output->WriteLine("  (b)reak [module:]line [cond] : set a breakpoint in line no with condition cond");
            output->WriteLine("  clear [no]                   : clears a breakpoint with number no or clears all");
            output->WriteLine("  lis(t)                       : shows all breakpoints");
            output->WriteLine("  restart                      : restart program");
            output->WriteLine("  (l)ocals                     : show all local variables of current scope");
            output->WriteLine("  (g)lobals                    : show all global variables");
            output->WriteLine("  modules                      : show all available modules");
            output->WriteLine("  builtins                     : show all builtin functions");
            output->WriteLine("  funcs                        : show all available functions");
            output->WriteLine("  macros                       : show all available macros");
            output->WriteLine("  doc [function]               : show documentation for all or only given function(s)");
            output->WriteLine("  searchdoc name               : show documentation for function(s) containing name");
            output->WriteLine("  exit                         : exit the interactive loop");
            output->WriteLine();
        }

        /*private*/ static Tuple<bool, int> ConvertToInt(const string & value)
        {
            var result = 0;
            var ok = true;
            try
            {
                result = /*Convert.ToInt32*/atoi(value.c_str());
            }
            catch (/*FormatException*/...)
            {
                ok = false;
            }
            return /*new*/ Tuple<bool, int>(ok, result);
        }

        //#endregion
	};
}

#endif
