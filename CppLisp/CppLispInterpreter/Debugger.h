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

#ifndef _DEBUGGER_H
#define _DEBUGGER_H

 //using System;
//using System.Collections.Generic;
//using System.IO;
//using System.Linq;

#include "DebuggerInterface.h"
#include "Lisp.h"
#include "Environment.h"
#include "Utils.h"

namespace CppLisp
{
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

        /*private*/ static const string Prompt;

        /*private*/ static const string DbgPrompt;

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
		/*public*/ LispDebugger();

		virtual ~LispDebugger();

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
		/*public*/ static bool InteractiveLoop(LispDebugger * debugger = null, std::shared_ptr<LispScope> initialTopScope = null, bool startedFromMain = false, bool tracing = false, std::shared_ptr<TextWriter> outp = null, std::shared_ptr<TextReader> inp = null);
		
		//#endregion

        //#region LispDebuggerInterface

        /// <summary>
        /// See interface.
        /// </summary>
		/*public*/ virtual void InteractiveLoop(std::shared_ptr<LispScope> initialTopScope, std::shared_ptr<IEnumerable<std::shared_ptr<object>>> currentAst = null, bool startedFromMain = false, bool tracing = false, std::shared_ptr<TextWriter> outp = null, std::shared_ptr<TextReader> inp = null);
 
		/// <summary>
        /// See interface.
        /// </summary>
		/*public*/ virtual bool NeedsBreak(std::shared_ptr<LispScope> scope, LispBreakpointPosition posInfosOfCurrentAstItem);

        /// <summary>
        /// See interface.
        /// </summary>
		/*public*/ virtual std::shared_ptr<LispVariant> DebuggerLoop(const string & script, const string & moduleName, bool tracing = false);

        /// <summary>
        /// See interface.
        /// </summary>
        /*public*/ inline void SetInputOutputStreams(std::shared_ptr<TextWriter> output, std::shared_ptr<TextReader> input)
        {
            Output = output;
            Input = input;            
        }

        //#endregion

	private:
        //#region private methods

		/*private*/ void Reset();

		/*private*/ bool HitsBreakpoint(size_t lineNo, const string & moduleName, std::shared_ptr<LispScope> scope);
		/*private*/ bool HasBreakpointAt(size_t lineNo, const string & moduleName);
		/*private*/ void AddBreakpoint(size_t lineNo, const string & moduleName, const string & condition);
		/*private*/ bool ClearBreakpoint(int no);
		/*private*/ void ClearAllBreakpoints();

        // ReSharper disable once UnusedParameter.Local
		/*private*/ void DoStep(std::shared_ptr<LispScope> /*currentScope*/);
		/*private*/ void DoStepOver(std::shared_ptr<LispScope> currentScope);
		/*private*/ void DoStepOut(std::shared_ptr<LispScope> currentScope);
		/*private*/ void DoRun();

		/*private*/ void ShowBreakpoints();

		/*private*/ static bool IsSameModule(const string & moduleName1, const string & moduleName2);

		/*private*/ static void ShowSourceCode(LispDebugger * debugger, const string & sourceCode, const string & moduleName, size_t/*?*/ currentLineNo);

		/*private*/ static void ClearBreakpoints(LispDebugger * debugger, const string & cmd);

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
		/*private*/ static void AddBreakpoint(LispDebugger * debugger, const string & cmd, const string & currentModuleName);
		
		/*private*/ static string GetStringLiteral(const string & text, /*out*/ size_t & stopPos);

		/*private*/ static void ShowInteractiveCmds(std::shared_ptr<TextWriter> output);

		/*private*/ static Tuple<bool, int> ConvertToInt(const string & value);
        
        //#endregion
	};
}

#endif
