using System.Collections.Generic;
using System.Linq;

namespace CsLisp
{
    /// <summary>
    /// The FUEL lisp parser.
    /// </summary>
    public class LispParser
    {
        #region constants

        private const string BracketsOutOfBalance = "Brackets out of balance";
        private const string BracketsOutOfBalanceOrUnexpectedScriptCode = BracketsOutOfBalance + " or unexpected script code";
        private const string UnexpectedToken = "unexpected token";

        #endregion

        #region public static methods

        /// <summary>
        /// Parses the specified code.
        /// string ==> List(Tokens) ==> List(object) mit object == List(object), LispVariant(string, int, double, and ==> for unquoting Symbols)
        /// </summary>
        /// <param name="code">The code.</param>
        /// <param name="scope">The scope.</param>
        /// <returns></returns>
        public static IEnumerable<object> Parse(string code, LispScope scope = null)
        {
            List<object> parseResult = null;

            // set tokens at LispScope to improve debugging and 
            // support displaying of error position 
            var tokens = LispTokenizer.Tokenize(code).ToList();
            if (scope != null)
            {
                scope.Tokens = tokens;
            }

            ParseTokens(tokens, 0, ref parseResult, isToplevel: true);

            return parseResult;
        }

        #endregion

        #region private methods

        private static int ParseTokens(IList<LispToken> tokens, int startIndex, ref List<object> parseResult, bool isToplevel)
        {
            int i;
            List<object> current = null;
            var listStack = new Stack<List<object>>();

            for (i = startIndex; i < tokens.Count; i++)
            {
                LispToken token = tokens[i];
                if (token.Type == LispTokenType.ListStart)
                {
                    current = new List<object>();
                    if (parseResult == null)
                    {
                        parseResult = current;
                    }
                    listStack.Push(current);
                }
                else if (token.Type == LispTokenType.ListEnd)
                {
                    var temp = current;
                    listStack.Pop();
                    if (listStack.Count > 0)
                    {
                        listStack.Peek().Add(temp);
                        current = listStack.Peek();
                    }
                    else
                    {
                        if (isToplevel && i+1<tokens.Count && !OnlyCommentTokensFrom(tokens, i+1))
                        {
                            throw new LispException(BracketsOutOfBalanceOrUnexpectedScriptCode + GetPosInfo(token));
                        }
                        return i;
                    }
                }
                else if (token.Type == LispTokenType.Quote || token.Type == LispTokenType.QuasiQuote)
                {
                    var quote = new List<object>();
                    quote.Add(new LispVariant(LispType.Symbol, token.Type == LispTokenType.Quote ? LispEnvironment.Quote : LispEnvironment.Quasiquote));

                    var nextToken = tokens[i + 1];
                    if (nextToken.Type == LispTokenType.ListStart)
                    {
                        List<object> quotedList = null;
                        i = ParseTokens(tokens, i + 1, ref quotedList, isToplevel: false);
                        quote.Add(quotedList);
                    }
                    else
                    {
                        quote.Add(new LispVariant(nextToken));
                        i++;
                    }
                    current.Add(quote);
                }
                else if (token.Type == LispTokenType.UnQuote || token.Type == LispTokenType.UnQuoteSplicing)
                {
                    i++;
                    var nextToken = tokens[i];
                    current.Add(new LispVariant(nextToken, unQuoted: token.Type == LispTokenType.UnQuote ? LispUnQuoteModus.UnQuote : LispUnQuoteModus.UnQuoteSplicing));
                }
                else if (token.Type == LispTokenType.Comment)
                {
                    // just ignore comment 
                }
                else
                {
                    if (current == null)
                    {
                        throw new LispException(UnexpectedToken + GetPosInfo(token));
                    }
                    current.Add(new LispVariant(token));
                }
            }

            if (isToplevel && tokens.Count>0)
            {
                throw new LispException(BracketsOutOfBalance + GetPosInfo(tokens.Last()));
            }

            return i;
        }

        private static bool OnlyCommentTokensFrom(IList<LispToken> tokens, int i)
        {
            for (var n = i; n < tokens.Count; n++)
            {
                if (tokens[n].Type != LispTokenType.Comment)
                {
                    return false;
                }
            }
            return true;
        }

        private static string GetPosInfo(LispToken token)
        {
            return "(" + LispInterpreter.GetPosInfoString(token) + ")";
        }

        #endregion
    }
}
