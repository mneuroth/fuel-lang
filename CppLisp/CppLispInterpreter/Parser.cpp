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

#include "Parser.h"
#include "Exception.h"

#include <stack>

using namespace CppLisp;

const string LispParser::BracketsOutOfBalance = "Brackets out of balance";
const string LispParser::BracketsOutOfBalanceOrUnexpectedScriptCode = BracketsOutOfBalance + " or unexpected script code";
const string LispParser::UnexpectedToken = "Unexpected token";

namespace CppLisp
{
	std::shared_ptr<object> LispParser::Parse(const string & code, size_t offset, std::shared_ptr<LispScope> scope)
	{
		std::shared_ptr<object> parseResult/* = null*/;
		string moduleName = ""; // string.Empty;

		// set tokens at LispScope to improve debugging and 
		// support displaying of error position 
		var tokens = LispTokenizer::Tokenize(code, offset);
		if (scope.get() != null)
		{
			scope->Tokens = tokens;
			moduleName = scope->ModuleName;
		}

		ParseTokens(moduleName, tokens.ToArray(), 0, /*ref*/ parseResult, /*isToplevel:*/ true);

		return parseResult;
	}

	size_t LispParser::ParseTokens(const string & moduleName, std::vector<std::shared_ptr<LispToken>> tokens, size_t startIndex, /*ref*/ std::shared_ptr<object> & parseResult, bool isToplevel)
	{
		size_t i;
		std::shared_ptr<IEnumerable<std::shared_ptr<object>>> current = null;
		std::stack<std::shared_ptr<IEnumerable<std::shared_ptr<object>>>> listStack;

		for (i = startIndex; i < tokens.size(); i++)
		{
			std::shared_ptr<LispToken> token = tokens[i];
			if (token->Type == LispTokenType::ListStart)
			{
				current = std::make_shared<IEnumerable<std::shared_ptr<object>>>();
				listStack.push(current);
			}
			else if (token->Type == LispTokenType::ListEnd)
			{
				var temp = current;
				listStack.pop();
				if (listStack.size() > 0)
				{
					listStack.top()->push_back/*Add*/(std::make_shared<object>(object(*temp)));
					current = listStack.top();
				}
				else
				{
					if (isToplevel && i + 1<tokens.size() && !OnlyCommentTokensFrom(tokens, i + 1))
					{
						throw LispException(BracketsOutOfBalanceOrUnexpectedScriptCode, token, moduleName);
					}
					parseResult = std::make_shared<object>(*current);
					return i;
				}
			}
			else if (token->Type == LispTokenType::Quote || token->Type == LispTokenType::QuasiQuote)
			{
				std::shared_ptr<IEnumerable<std::shared_ptr<object>>> quote = std::make_shared<IEnumerable<std::shared_ptr<object>>>();
				quote->push_back/*Add*/(std::make_shared<object>(object(LispVariant(LispType::_Symbol, std::make_shared<object>(token->Type == LispTokenType::Quote ? LispEnvironment::Quote : LispEnvironment::Quasiquote)))));

				std::shared_ptr<object> quotedList = null;
				i = ParseTokens(moduleName, tokens, i + 1, /*ref*/ quotedList, /*isToplevel:*/ false);
				quote->push_back/*Add*/(std::make_shared<object>(object(*quotedList)));

				if (current != null)
				{
					current->push_back/*Add*/(std::make_shared<object>(object(*quote)));
				}
			}
			else if (token->Type == LispTokenType::UnQuote || token->Type == LispTokenType::UnQuoteSplicing)
			{
				var unquote = std::make_shared<IEnumerable<std::shared_ptr<object>>>();
                //LispUnQuoteModus unquotedModus = token->Type == LispTokenType::UnQuote ? LispUnQuoteModus::_UnQuote : LispUnQuoteModus::_UnQuoteSplicing;
				unquote->push_back/*Add*/(std::make_shared<object>(LispVariant(LispType::_Symbol, std::make_shared<object>(object(token->Type == LispTokenType::UnQuote ? LispEnvironment::UnQuote : LispEnvironment::UnQuoteSplicing)))));

				std::shared_ptr<object> quotedList = null;
				i = ParseTokens(moduleName, tokens, i + 1, /*ref*/ quotedList, /*isToplevel:*/ false);
				unquote->push_back/*Add*/(quotedList);

				if (current != null)
				{
					current->push_back/*Add*/(std::make_shared<object>(*unquote));
				}
				else
				{
					parseResult = std::make_shared<object>(*unquote);
					return i;
				}
			}
			else if (token->Type == LispTokenType::Comment)
			{
				// just ignore comment 
			}
			else
			{
				if (!isToplevel && current == null)
				{
					parseResult = std::make_shared<object>(LispVariant(token));
					return i;
				}
				if (current == null)
				{
					throw LispException(UnexpectedToken, token, moduleName);
				}
				current->push_back/*Add*/(std::make_shared<object>(LispVariant(std::make_shared<LispToken>(*token))));
			}
		}

		if (isToplevel && tokens.size()>0)
		{
			std::shared_ptr<LispToken> token = tokens.back/*Last*/();
			throw LispException(BracketsOutOfBalance, token, moduleName);
		}

		parseResult = std::make_shared<object>(*current);
		return i;
	}

	bool LispParser::OnlyCommentTokensFrom(const std::vector<std::shared_ptr<LispToken>> & tokens, size_t i)
	{
		for (size_t n = i; n < tokens.size(); n++)
		{
			if (tokens[n]->Type != LispTokenType::Comment)
			{
				return false;
			}
		}
		return true;
	}
}
