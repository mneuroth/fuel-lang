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

#ifndef _DEBUGGER_INTERFACE_H
#define _DEBUGGER_INTERFACE_H

//using System.Collections.Generic;
//using System.IO;

#include "cstypes.h"

namespace CppLisp
{
	class LispScope;
	class LispBreakpointPosition;

    /// <summary>
    /// The generic debugger interface for the runtime environment.
    /// </summary>
    /*public interface*/class DLLEXPORT ILispDebugger
    {
	public:
		/// <summary>
		/// The destructor.
		/// </summary>
		//virtual ~ILispDebugger();

        /// <summary>
        /// Enters the interactive loop.
        /// </summary>
        /// <param name="initialTopScope">The initial top scope.</param>
        /// <param name="currentAst">The current ast.</param>
        /// <param name="startedFromMain">if set to <c>true</c> [started from main].</param>
        /// <param name="tracing">if set to <c>true</c> tracing is enabled.</param>
        virtual void InteractiveLoop(std::shared_ptr<LispScope> initialTopScope /*= null*/, std::shared_ptr<IEnumerable<std::shared_ptr<object>>> currentAst = null, bool startedFromMain = false, bool tracing = false, std::shared_ptr<TextWriter> outp = null, std::shared_ptr<TextReader> inp = null) = 0;

        /// <summary>
        /// Verifies if the current execution position needes a break,
        /// this means a breakpoint was hit.
        /// </summary>
        /// <param name="scope">The scope.</param>
        /// <param name="posInfosOfCurrentAstItem">
        /// The position infos of current ast item.
        /// </param>
        /// <returns>True if a break is needed</returns>
        virtual bool NeedsBreak(std::shared_ptr<LispScope> scope, LispBreakpointPosition posInfosOfCurrentAstItem) = 0;

        /// <summary>
        /// Enters the loop of the debugger.
        /// </summary>
        /// <param name="script">The script.</param>
        /// <param name="moduleName">The module name.</param>
        /// <param name="tracing">if set to <c>true</c> tracing is enabled.</param>
        /// <returns>Value of the last expression</returns>
        virtual std::shared_ptr<LispVariant> DebuggerLoop(const string & script, const string & moduleName, bool tracing = false) = 0;

        /// <summary>
        /// Sets the input and output streams for the debugger.
        /// </summary>
        /// <param name="output">The output stream.</param>
        /// <param name="input">The input stream.</param>
        virtual void SetInputOutputStreams(std::shared_ptr<TextWriter> output, std::shared_ptr<TextReader> input) = 0;
	};

/*
    /// <summary>
    /// Class representing breakpoint position information.
    /// Item1 is start position in full text,
    /// Item2 is stop position in full text,
    /// Item3 is line number
    /// </summary>
    public class LispBreakpointPosition : Tuple<int, int, int>
    {
        public LispBreakpointPosition(int start, int stop, int lineNumber)
            : base(start, stop, lineNumber)
        {            
        }

        public int Start
        {
            get
            {
                return Item1;
            }
        }

        public int Stop
        {
            get
            {
                return Item2;
            }
        }

        public int LineNo
        {
            get
            {
                return Item3;
            }
        }
    }
	*/
}

#endif
