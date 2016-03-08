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
using System.Globalization;

namespace CsLisp
{
    /// <summary>
    /// Type for lisp tokens.
    /// </summary>
    public enum LispTokenType
    {
        ListStart = 0,
        ListEnd = 1,
        Symbol = 2,
        String = 3,
        Int = 4,
        Double = 5,
        Quote = 6,
        QuasiQuote = 7,
        UnQuote = 8,
        UnQuoteSplicing = 9,
        True = 10,
        False = 11,
        Comment = 12,
        Nil = 13,
    }

    /// <summary>
    /// Interface for a lisp token.
    /// </summary>
    /// <remark>
    /// This interface is intended to remove the dependency to the LispToken class in the future.
    /// </remark>
    public interface ILispTokenInterface
    {
        /// <summary>
        /// Gets the type of the token.
        /// </summary>
        /// <value>
        /// The type.
        /// </value>
        LispTokenType Type { get; }

        /// <summary>
        /// Gets the value.
        /// </summary>
        /// <value>
        /// The value.
        /// </value>
        object Value { get; }

        /// <summary>
        /// Gets the start position of the token.
        /// </summary>
        /// <value>
        /// The start position.
        /// </value>
        int StartPos { get; }
    }

    /// <summary>
    /// Lisp token.
    /// </summary>
    public class LispToken : ILispTokenInterface
    {
        #region constants

        public const string StringStart = "\"";
        public const string Quote = "'";
        public const string Quasiquote = "`";
        public const string Unquote = ",";
        public const string Unquotesplicing = ",@";
        public const string Nil = "NIL";

        #endregion

        #region properties

        /// <summary>
        /// Gets the type of the token.
        /// </summary>
        /// <value>
        /// The type.
        /// </value>
        public LispTokenType Type { get; private set; }

        /// <summary>
        /// Gets the value.
        /// </summary>
        /// <value>
        /// The value.
        /// </value>
        public object Value { get; private set; }

        /// <summary>
        /// Gets or sets the start position of the token.
        /// </summary>
        /// <value>
        /// The start position.
        /// </value>
        public int StartPos { get; set; }

        /// <summary>
        /// Gets or sets the stop position of the token.
        /// </summary>
        /// <value>
        /// The stop position.
        /// </value>
        public int StopPos { get; set; }

        /// <summary>
        /// Gets or sets the line no of the token.
        /// </summary>
        /// <value>
        /// The line no.
        /// </value>
        public int LineNo { get; set; }

        #endregion

        #region constructor

        /// <summary>
        /// Initializes a new instance of the <see cref="LispToken"/> class.
        /// </summary>
        /// <param name="text">The text of the token.</param>
        /// <param name="start">The start position.</param>
        /// <param name="stop">The stop position.</param>
        /// <param name="lineNo">The line no.</param>
        public LispToken(string text, int start, int stop, int lineNo)
        {
            int intValue;
            double doubleValue;

            StartPos = start;
            StopPos = stop;
            LineNo = lineNo;
            Value = text;

            if (text.StartsWith(StringStart))
            {
                Type = LispTokenType.String;
                Value = text.Substring(1, text.Length - 2);
            }
            else if (text == Quote)
            {
                Type = LispTokenType.Quote;
            }
            else if (text == Quasiquote)
            {
                Type = LispTokenType.QuasiQuote;
            }
            else if (text == Unquote)
            {
                Type = LispTokenType.UnQuote;
            }
            else if (text == Unquotesplicing)
            {
                Type = LispTokenType.UnQuoteSplicing;
            }
            else if (text == "(")
            {
                Type = LispTokenType.ListStart;
            }
            else if (text == ")")
            {
                Type = LispTokenType.ListEnd;
            }
            else if (Int32.TryParse(text, out intValue))
            {
                Type = LispTokenType.Int;
                Value = intValue;
            }
            else if (Double.TryParse(text, NumberStyles.Any, CultureInfo.InvariantCulture, out doubleValue))
            {
                Type = LispTokenType.Double;
                Value = doubleValue;
            }
            else if (text.Equals("true") || text.Equals("#t"))
            {
                Type = LispTokenType.True;
                Value = true;
            }
            else if (text.Equals("false") || text.Equals("#f"))
            {
                Type = LispTokenType.False;
                Value = false;
            }
            else if (text.ToUpper().Equals(Nil))
            {
                Type = LispTokenType.Nil;
                Value = null;
            }
            else if (text.StartsWith(";"))
            {
                Type = LispTokenType.Comment;
                Value = text;
            }
            else
            {
                Type = LispTokenType.Symbol;
                Value = text;
            }
        }

        #endregion

        #region public methods

        /// <summary>
        /// Returns a <see cref="System.String" /> that represents this instance.
        /// </summary>
        /// <returns>
        /// A <see cref="System.String" /> that represents this instance.
        /// </returns>
        public override string ToString()
        {
            if (Type == LispTokenType.Nil)
            {
                return Nil;
            }
            return Value.ToString();
        }

        #endregion
    }
}
