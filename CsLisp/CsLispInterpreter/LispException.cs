using System;

namespace CsLisp
{
    /// <summary>
    /// Exception for the FUEL lisp interpreter
    /// </summary>
    public class LispException : Exception
    {
        /// <summary>
        /// Initializes a new instance of the <see cref="LispException" /> class.
        /// </summary>
        /// <param name="text">The text.</param>
        /// <param name="lineNo">The line no.</param>
        public LispException(string text, int? lineNo = null)
            : base(text)
        {
            Data[LispUtils.LineNo] = lineNo;
        }
    }
}
