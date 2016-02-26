using System;
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
        /// Item1 is start position in full text,
        /// Item2 is stop position in full text,
        /// Item3 is line number
        /// </param>
        /// <returns>True if a break is needed</returns>
        bool NeedsBreak(LispScope scope, Tuple<int, int, int> posInfosOfCurrentAstItem);

        /// <summary>
        /// Enters the loop of the debugger.
        /// </summary>
        /// <param name="script">The script.</param>
        /// <param name="moduleName">The module name.</param>
        /// <param name="output">The output stream.</param>
        /// <param name="input">The input stream.</param>
        /// <param name="tracing">if set to <c>true</c> tracing is enabled.</param>
        /// <returns>Value of the last expression</returns>
        LispVariant DebuggerLoop(string script, string moduleName, TextWriter output, TextReader input, bool tracing = false);
    }
}
