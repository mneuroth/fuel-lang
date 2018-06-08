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

#include "stdafx.h"

#include "CppUnitTest.h"

#include "../CsLispInterpreter/csstring.h"
#include "../CsLispInterpreter/csobject.h"
#include "../CsLispInterpreter/Tokenizer.h"
#include "../CsLispInterpreter/Token.h"
#include "../CsLispInterpreter/Tokenizer.h"

using namespace Microsoft::VisualStudio::CppUnitTestFramework;

using namespace CsLisp;

namespace QtLispInterpreterTests
{		
	TEST_CLASS(UnitTestTokenizer)
	{
	public:
		
		TEST_METHOD(Test_TokenizerEmptyCode)
		{
			IEnumerable<std::shared_ptr<LispToken>> result = LispTokenizer::Tokenize("");
			//Assert::IsNotNull(result);
			Assert::AreEqual<size_t>(0, result.Count());

			result = LispTokenizer::Tokenize("     \t \n   ");
			//Assert::IsNotNull(result);
			Assert::AreEqual<size_t>(0, result.Count());
		}

		TEST_METHOD(Test_Tokenizer1)
		{
			IEnumerable<std::shared_ptr<LispToken>> result = LispTokenizer::Tokenize("()");
			//Assert::IsNotNull(result);
			Assert::AreEqual<size_t>(2, result.Count());
			Assert::AreEqual("(", result.First()->ToString().c_str());
			Assert::AreEqual(")", result.Last()->ToString().c_str());

			result = LispTokenizer::Tokenize("  (  \n    )  ");
			//Assert::IsNotNull(result);
			Assert::AreEqual<size_t>(2, result.Count());
			Assert::AreEqual("(", result.First()->ToString().c_str());
			Assert::AreEqual(")", result.Last()->ToString().c_str());
		}

		TEST_METHOD(Test_Tokenizer2)
		{
			IEnumerable<std::shared_ptr<LispToken>> result = LispTokenizer::Tokenize("(+ 1 #t 3.1415 \"asdf blub\" #f )");
			//Assert::IsNotNull(result);
			Assert::AreEqual<size_t>(8, result.Count());
			var resultAsArray = result.ToArray();
			Assert::AreEqual("(", resultAsArray[0]->ToString().c_str());
			Assert::IsTrue(LispTokenType::ListStart == resultAsArray[0]->Type);
			Assert::AreEqual("+", resultAsArray[1]->ToString().c_str());
			Assert::IsTrue(LispTokenType::Symbol == resultAsArray[1]->Type);
			Assert::AreEqual(1, (int)*(resultAsArray[2]->Value));
			Assert::IsTrue(LispTokenType::Int == resultAsArray[2]->Type);
			Assert::AreEqual(true, (bool)*(resultAsArray[3]->Value));
			Assert::IsTrue(LispTokenType::True == resultAsArray[3]->Type);
			Assert::AreEqual(3.1415, (double)*(resultAsArray[4]->Value));
			Assert::IsTrue(LispTokenType::Double == resultAsArray[4]->Type);
			Assert::AreEqual("asdf blub", resultAsArray[5]->ToString().c_str());
			Assert::IsTrue(LispTokenType::String == resultAsArray[5]->Type);
			bool o = (bool)*(resultAsArray[6]->Value);
			Assert::AreEqual(false, (bool)*(resultAsArray[6]->Value));
			Assert::IsTrue(LispTokenType::False == resultAsArray[6]->Type);
			Assert::AreEqual(")", resultAsArray[7]->ToString().c_str());
			Assert::IsTrue(LispTokenType::ListEnd == resultAsArray[7]->Type);
		}

		TEST_METHOD(Test_Tokenizer3)
		{
			IEnumerable<std::shared_ptr<LispToken>> result = LispTokenizer::Tokenize("(do (print (* 9 9)))");
			//Assert::IsNotNull(result);
			Assert::AreEqual<size_t>(11, result.Count());
			var resultAsArray = result.ToArray();
			Assert::AreEqual("(", resultAsArray[0]->ToString().c_str());
			Assert::AreEqual("do", resultAsArray[1]->ToString().c_str());
			Assert::IsTrue(LispTokenType::Symbol == resultAsArray[1]->Type);
			Assert::AreEqual("(", resultAsArray[2]->ToString().c_str());
			Assert::AreEqual("print", resultAsArray[3]->ToString().c_str());
			Assert::AreEqual("(", resultAsArray[4]->ToString().c_str());
			Assert::AreEqual("*", resultAsArray[5]->ToString().c_str());
			Assert::AreEqual(9, (int)*(resultAsArray[6]->Value));
			Assert::AreEqual(9, (int)*(resultAsArray[7]->Value));
			Assert::AreEqual(")", resultAsArray[8]->ToString().c_str());
			Assert::AreEqual(")", resultAsArray[9]->ToString().c_str());
			Assert::AreEqual(")", resultAsArray[10]->ToString().c_str());
		}

		TEST_METHOD(Test_Tokenizer4)
		{
			IEnumerable<std::shared_ptr<LispToken>> result = LispTokenizer::Tokenize("(do\n (print (* 9 9)) ; this is a comment\n)\n");
			//Assert::IsNotNull(result);
			Assert::AreEqual<size_t>(12, result.Count());
			var resultAsArray = result.ToArray();
			Assert::AreEqual("(", resultAsArray[0]->ToString().c_str());
			Assert::AreEqual("do", resultAsArray[1]->ToString().c_str());
			Assert::AreEqual("(", resultAsArray[2]->ToString().c_str());
			Assert::AreEqual("print", resultAsArray[3]->ToString().c_str());
			Assert::AreEqual("(", resultAsArray[4]->ToString().c_str());
			Assert::AreEqual("*", resultAsArray[5]->ToString().c_str());
			Assert::AreEqual(9, (int)*(resultAsArray[6]->Value));
			Assert::AreEqual(9, (int)*(resultAsArray[7]->Value));
			Assert::AreEqual(")", resultAsArray[8]->ToString().c_str());
			Assert::AreEqual(")", resultAsArray[9]->ToString().c_str());
			Assert::AreEqual("; this is a comment\n", resultAsArray[10]->ToString().c_str());
			Assert::IsTrue(LispTokenType::Comment == resultAsArray[10]->Type);
			Assert::AreEqual(")", resultAsArray[11]->ToString().c_str());
		}

		TEST_METHOD(Test_Tokenizer5)
		{
			IEnumerable<std::shared_ptr<LispToken>> result = LispTokenizer::Tokenize("(test '(1 2 3))");
			//Assert::IsNotNull(result);
			Assert::AreEqual<size_t>(9, result.Count());
			var resultAsArray = result.ToArray();
			Assert::AreEqual("'", resultAsArray[2]->ToString().c_str());
			Assert::IsTrue(LispTokenType::Quote == resultAsArray[2]->Type);

			result = LispTokenizer::Tokenize("(test `(1 2 3))");
			//Assert::IsNotNull(result);
			Assert::AreEqual<size_t>(9, result.Count());
			resultAsArray = result.ToArray();
			Assert::AreEqual("`", resultAsArray[2]->ToString().c_str());
			Assert::IsTrue(LispTokenType::QuasiQuote == resultAsArray[2]->Type);

			result = LispTokenizer::Tokenize("(test ,a)");
			//Assert::IsNotNull(result);
			Assert::AreEqual<size_t>(5, result.Count());
			resultAsArray = result.ToArray();
			Assert::AreEqual(",", resultAsArray[2]->ToString().c_str());
			Assert::IsTrue(LispTokenType::UnQuote == resultAsArray[2]->Type);

			result = LispTokenizer::Tokenize("(test ,@a)");
			//Assert::IsNotNull(result);
			Assert::AreEqual<size_t>(5, result.Count());
			resultAsArray = result.ToArray();
			Assert::AreEqual(",@", resultAsArray[2]->ToString().c_str());
			Assert::IsTrue(LispTokenType::UnQuoteSplicing == resultAsArray[2]->Type);
		}

		TEST_METHOD(Test_Tokenizer6)
		{
			IEnumerable<std::shared_ptr<LispToken>> result = LispTokenizer::Tokenize("(test nil)");
			//Assert::IsNotNull(result);
			Assert::AreEqual<size_t>(4, result.Count());
			var resultAsArray = result.ToArray();
			Assert::AreEqual(LispToken::NilConst.c_str(), resultAsArray[2]->ToString().c_str());
			Assert::IsTrue(LispTokenType::Nil == resultAsArray[2]->Type);
		}

		TEST_METHOD(Test_Tokenizer7)
		{
			IEnumerable<std::shared_ptr<LispToken>> result = LispTokenizer::Tokenize("(test \"blub\nhello\")");
			//Assert::IsNotNull(result);
			Assert::AreEqual<size_t>(4, result.Count());
			var resultAsArray = result.ToArray();
			Assert::AreEqual("blub\nhello", resultAsArray[2]->ToString().c_str());
			Assert::IsTrue(LispTokenType::String == resultAsArray[2]->Type);
		}
	};
}