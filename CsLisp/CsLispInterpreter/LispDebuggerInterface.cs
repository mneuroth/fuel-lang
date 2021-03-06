﻿/*
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

using System.Collections.Generic;
using System.IO;

namespace CsLisp
{
    /// <summary>
    /// The generic debugger interface for the runtime environment.
    /// </summary>
    public interface ILispDebugger
    {
        /// <summary>
        /// Enters the interactive loop.
        /// </summary>
        /// <param name="initialTopScope">The initial top scope.</param>
        /// <param name="currentAst">The current ast.</param>
        /// <param name="startedFromMain">if set to <c>true</c> [started from main].</param>
        /// <param name="tracing">if set to <c>true</c> tracing is enabled.</param>
        void InteractiveLoop(LispScope initialTopScope = null, IList<object> currentAst = null, bool startedFromMain = false, bool tracing = false);

        /// <summary>
        /// Verifies if the current execution position needes a break,
        /// this means a breakpoint was hit.
        /// </summary>
        /// <param name="scope">The scope.</param>
        /// <param name="posInfosOfCurrentAstItem">
        /// The position infos of current ast item.
        /// </param>
        /// <returns>True if a break is needed</returns>
        bool NeedsBreak(LispScope scope, LispBreakpointPosition posInfosOfCurrentAstItem);

        /// <summary>
        /// Enters the loop of the debugger.
        /// </summary>
        /// <param name="script">The script.</param>
        /// <param name="moduleName">The module name.</param>
        /// <param name="tracing">if set to <c>true</c> tracing is enabled.</param>
        /// <returns>Value of the last expression</returns>
        LispVariant DebuggerLoop(string script, string moduleName, bool tracing = false);

        /// <summary>
        /// Sets the input and output streams for the debugger.
        /// </summary>
        /// <param name="output">The output stream.</param>
        /// <param name="input">The input stream.</param>
        void SetInputOutputStreams(TextWriter output, TextReader input);
    }

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
}
