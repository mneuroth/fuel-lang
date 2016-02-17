using System;
using System.Collections.Generic;

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
        /// <param name="posInfosOfCurrentAstItem">The position infos of current ast item.</param>
        /// <returns>True if a break is needed</returns>
        bool NeedsBreak(LispScope scope, Tuple<int, int> posInfosOfCurrentAstItem);

        /// <summary>
        /// Loop of the debugger.
        /// </summary>
        /// <param name="args">The arguments.</param>
        /// <param name="tracing">if set to <c>true</c> tracing is enabled.</param>
        /// <returns>Value of the last expression</returns>
        LispVariant DebuggerLoop(string[] args, bool tracing = false);
    }
}
