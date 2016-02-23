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
            else if (CheckInt(text, out intValue))
            {
                Type = LispTokenType.Int;
                Value = intValue;
            }
            else if (CheckDouble(text, out doubleValue))
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
            else if (text.Equals("nil"))
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
        /// Gets the position information representation.
        /// </summary>
        /// <returns></returns>
        public string GetPosInfo()
        {
            var info = "pos=";
            info += StartPos;
            return info;
        }

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

        #region private methods

        static private bool CheckInt(string text, out int value)
        {
            var checkInt = true;
            value = 0;
            try
            {
                var result = Int32.Parse(text);
                value = result;
            }
            catch (FormatException)
            {
                checkInt = false;
            }
            return checkInt;
        }

        static private bool CheckDouble(string text, out double value)
        {
            var checkDouble = true;
            value = 0;
            try
            {
                var result = Double.Parse(text, CultureInfo.InvariantCulture);
                value = result;
            }
            catch (FormatException)
            {
                checkDouble = false;
            }
            return checkDouble;
        }

        #endregion
    }
}
