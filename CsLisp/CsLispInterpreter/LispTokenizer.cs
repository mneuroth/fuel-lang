using System;
using System.Collections.Generic;

namespace CsLisp
{
    /// <summary>
    /// The FUEL lisp tokenizer
    /// </summary>
    public class LispTokenizer
    {
        #region public static methods

        /// <summary>
        /// Tokenizes the specified code.
        /// </summary>
        /// <param name="code">The code.</param>
        /// <returns>Container with tokens</returns>
        public static IEnumerable<LispToken> Tokenize(string code)
        {
            var tokens = new List<LispToken>();
            var currentToken = String.Empty;
            var currentTokenStartPos = 0;
            var lineCount = 1;
            var isInString = false;
            var isInSymbol = false;
            var wasLastBackslash = false;

            Action<string, int, int> addToken = (currentTok, pos, line) =>
            {
                tokens.Add(new LispToken(currentTok, currentTokenStartPos, pos, line));
                isInSymbol = false;
                isInString = false;
                currentToken = String.Empty;
                currentTokenStartPos = pos+1;
            };

            //foreach (char ch in code)
            for (int i = 0; i < code.Length; i++)
            {
                char ch = code[i];
                if (Char.IsWhiteSpace(ch))
                {
                    if (isInString)
                    {
                        currentToken += ch;
                    }
                    else if (isInSymbol)
                    {
                        addToken(currentToken, i, lineCount);
                    }
                    wasLastBackslash = false;
                    if (ch == '\n')
                    {
                        lineCount++;
                    }
                }
                else if (ch == '\\')
                {
                    if (wasLastBackslash)
                    {
                        currentToken += ch;
                        wasLastBackslash = false;
                    }
                    else
                    {
                        wasLastBackslash = true;
                    }
                }
                else if (ch == '(' || ch == ')')
                {
                    if (isInString)
                    {
                        currentToken += ch;
                    }
                    else if (isInSymbol)
                    {
                        addToken(currentToken, i, lineCount);
                        addToken(String.Empty + ch, i, lineCount);
                    }
                    else
                    {
                        addToken(String.Empty + ch, i, lineCount);
                    }
                    wasLastBackslash = false;
                }
                else if (ch == ';')
                {
                    if (isInString)
                    {
                        currentToken += ch;
                    }
                    else if (isInSymbol)
                    {
                        addToken(currentToken, i, lineCount);
                        i = ProcessComment(code, i, lineCount, ch, addToken);
                    }
                    else
                    {
                        i = ProcessComment(code, i, lineCount, ch, addToken);
                    }
                    wasLastBackslash = false;
                    // comment ends always with new line
                    lineCount++;
                }
                else if (ch == '\'' || ch == '`' || ch == ',')
                {
                    if (isInString)
                    {
                        currentToken += ch;
                    }
                    else
                    {
                        if (code[i + 1] == '@')
                        {
                            // process unquotesplicing
                            string s = String.Empty;
                            s += ch;
                            i++;
                            s += code[i];
                            addToken(s, i, lineCount);
                        }
                        else
                        {
                            addToken(String.Empty + ch, i, lineCount);
                        }
                    }
                    wasLastBackslash = false;
                }
                else if (ch == '"')
                {
                    if (wasLastBackslash)
                    {
                        currentToken += ch;
                    }
                    else if (isInString)
                    {
                        // finish string
                        addToken("\"" + currentToken + "\"", i, lineCount);
                    }
                    else
                    {
                        // start string
                        isInString = true;
                        currentToken = String.Empty;
                    }
                    wasLastBackslash = false;
                }
                else
                {
                    if (!isInSymbol && !isInString)
                    {
                        isInSymbol = true;
                    }
                    currentToken += ch;
                    wasLastBackslash = false;
                }
            }
            if (currentToken != String.Empty)
            {
                addToken(currentToken, -1, lineCount);
            }
            return tokens;
        }

        #endregion

        #region private static methods

        private static int ProcessComment(string code, int i, int lineCount, char ch, Action<string, int, int> addToken)
        {
            int newIndex;
            var comment = String.Empty + ch + GetRestOfLine(code, i + 1, out newIndex);
            addToken(comment, i, lineCount);
            i = newIndex;
            return i;
        }

        private static string GetRestOfLine(string code, int i, out int newIndex)
        {
            var rest = code.Substring(i);
            var pos = rest.IndexOf("\n", StringComparison.InvariantCulture);
            if (pos > 0)
            {
                newIndex = i + pos;
                return rest.Substring(0, pos+1);
            }
            newIndex = i + rest.Length -1;
            return rest;
        }

        #endregion
    }
}
