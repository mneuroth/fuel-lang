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

#include "../CppLispInterpreter/Parser.h"

using namespace Microsoft::VisualStudio::CppUnitTestFramework;

using namespace CppLisp;

namespace QtLispUnitTests
{
	TEST_CLASS(UnitTestParser)
	{
	public:

		TEST_METHOD(Test_SingleParserEmptyCode)
        {
			std::shared_ptr<IEnumerable<std::shared_ptr<object>>> result = LispParser::Parse("()");
            Assert::IsNotNull(result.get());
            Assert::AreEqual<size_t>(0, result->size());
        }

		TEST_METHOD(Test_SingleParser1)
        {
			std::shared_ptr<IEnumerable<std::shared_ptr<object>>> result = LispParser::Parse("(print 1 2.54 \"string\")");
            Assert::IsNotNull(result.get());
            var resultAsArray = result->ToArray();
            Assert::AreEqual<size_t>(4, resultAsArray.size());

            var value = resultAsArray[0]->ToLispVariant();
            Assert::IsTrue(value->IsSymbol());
            Assert::AreEqual("print", value->Value->ToString().c_str());
            value = resultAsArray[1]->ToLispVariant();
            Assert::IsTrue(value->IsInt());
            Assert::AreEqual<int>(1, (int)*(value->Value));
            value = resultAsArray[2]->ToLispVariant();
            Assert::IsTrue(value->IsDouble());
            Assert::AreEqual<double>(2.54, (double)*(value->Value));
            value = resultAsArray[3]->ToLispVariant();
            Assert::IsTrue(value->IsString());
            Assert::AreEqual("string", value->Value->ToString().c_str());
        }

		TEST_METHOD(Test_SingleParser2)
		{
			std::shared_ptr<IEnumerable<std::shared_ptr<object>>> result = LispParser::Parse("(do (print #t 2.54 \"string\"))");
            Assert::IsNotNull(result.get());
            var resultAsArrayDo = result->ToArray();
            Assert::AreEqual<size_t>(2, resultAsArrayDo.size());

            var value = resultAsArrayDo[0]->ToLispVariant();
            Assert::IsTrue(value->IsSymbol());
            Assert::AreEqual("do", value->Value->ToString().c_str());

            var listValue = resultAsArrayDo[1]->ToList();
            var resultAsArray = listValue->ToArray();
            value = resultAsArray[1]->ToLispVariant();
            Assert::IsTrue(value->IsBool());
            Assert::AreEqual(true, (bool)*(value->Value));
            value = resultAsArray[2]->ToLispVariant();
            Assert::IsTrue(value->IsDouble());
            Assert::AreEqual(2.54, (double)*(value->Value));
            value = resultAsArray[3]->ToLispVariant();
            Assert::IsTrue(value->IsString());
            Assert::AreEqual("string", value->Value->ToString().c_str());
        }
	};
}
