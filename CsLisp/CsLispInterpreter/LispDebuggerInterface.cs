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
        void InteractiveLoop(LispScope initialTopScope, IList<object> currentAst = null);

        /// <summary>
        /// Verifies if the current execution position needes a break,
        /// this means a breakpoint was hit.
        /// </summary>
        /// <param name="scope">The scope.</param>
        /// <param name="posInfosOfCurrentAstItem">The position infos of current ast item.</param>
        /// <returns>True if a break is needed</returns>
        bool NeedsBreak(LispScope scope, Tuple<int, int> posInfosOfCurrentAstItem);
    }
}
