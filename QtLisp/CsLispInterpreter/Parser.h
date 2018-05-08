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

#include <string>

#include "Token.h"
#include "Tokenizer.h"

namespace CsLisp
{
	/// <summary>
	/// The FUEL lisp parser.
	/// </summary>
    /*public*/ class LispParser
	{
		//#region constants
			
		const string BracketsOutOfBalance = "Brackets out of balance";
		const string BracketsOutOfBalanceOrUnexpectedScriptCode = BracketsOutOfBalance + " or unexpected script code";
		const string UnexpectedToken = "Unexpected token";

		//#endregion

	public:
		//#region public static methods
	
		/// <summary>
		/// Parses the specified code.
		/// string ==&gt; List(Tokens) ==&gt; List(object) mit object == List(object), LispVariant(string, int, double, and ==&gt; for unquoting Symbols)
		/// </summary>
		/// <param name="code">The code.</param>
		/// <param name="offset">The position offset.</param>
		/// <param name="scope">The scope.</param>
		/// <returns>Abstract syntax tree as container</returns>
		/*public*/ static IEnumerable<object> Parse(string code, int offset = 0, LispScope * scope = null)
		{
			List<object> parseResult = null;
			string moduleName = ""; // string.Empty;

			// set tokens at LispScope to improve debugging and 
			// support displaying of error position 
			var tokens = LispTokenizer::Tokenize(code, offset).ToList();
			if (scope != null)
			{
				scope.Tokens = tokens;
				moduleName = scope.ModuleName;
			}

			ParseTokens(moduleName, tokens, 0, ref parseResult, isToplevel: true);

			return parseResult;
		}

		//#endregion

	private:
		//#region private methods

		/*private*/ static int ParseTokens(string moduleName, IList<LispToken> tokens, int startIndex, /*ref*/ List<object> & parseResult, bool isToplevel)
		{
			int i;
			List<object> current = null;
			var listStack = new Stack<List<object>>();

			for (i = startIndex; i < tokens.Count; i++)
			{
				LispToken token = tokens[i];
				if (token.Type == LispTokenType::ListStart)
				{
					current = new List<object>();
					if (parseResult == null)
					{
						parseResult = current;
					}
					listStack.Push(current);
				}
				else if (token.Type == LispTokenType::ListEnd)
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
						if (isToplevel && i + 1<tokens.Count && !OnlyCommentTokensFrom(tokens, i + 1))
						{
							throw new LispException(BracketsOutOfBalanceOrUnexpectedScriptCode, token, moduleName);
						}
						return i;
					}
				}
				else if (token.Type == LispTokenType::Quote || token.Type == LispTokenType::QuasiQuote)
				{
					var quote = new List<object>();
					quote.Add(new LispVariant(LispType.Symbol, token.Type == LispTokenType::Quote ? LispEnvironment.Quote : LispEnvironment.Quasiquote));

					var nextToken = tokens[i + 1];
					if (nextToken.Type == LispTokenType::ListStart)
					{
						List<object> quotedList = null;
						i = ParseTokens(moduleName, tokens, i + 1, ref quotedList, isToplevel: false);
						quote.Add(quotedList);
					}
					else
					{
						quote.Add(new LispVariant(nextToken));
						i++;
					}
					if (current != null)
					{
						current.Add(quote);
					}
				}
				else if (token.Type == LispTokenType::UnQuote || token.Type == LispTokenType::UnQuoteSplicing)
				{
					i++;
					var nextToken = tokens[i];
					if (current != null)
					{
						current.Add(new LispVariant(nextToken, unQuoted: token.Type == LispTokenType.UnQuote ? LispUnQuoteModus.UnQuote : LispUnQuoteModus.UnQuoteSplicing));
					}
				}
				else if (token.Type == LispTokenType::Comment)
				{
					// just ignore comment 
				}
				else
				{
					if (current == null)
					{
						throw new LispException(UnexpectedToken, token, moduleName);
					}
					current.Add(new LispVariant(token));
				}
			}

			if (isToplevel && tokens.Count>0)
			{
				LispToken token = tokens.Last();
				throw new LispException(BracketsOutOfBalance, token, moduleName);
			}

			return i;
		}

		/*private*/ static bool OnlyCommentTokensFrom(IList<LispToken> tokens, int i)
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

		//#endregion
	};
}