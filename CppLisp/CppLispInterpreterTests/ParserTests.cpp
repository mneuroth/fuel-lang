﻿/*
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

#include "FuelUnitTestHelper.h"

using namespace Microsoft::VisualStudio::CppUnitTestFramework;

using namespace CppLisp;

namespace QtLispUnitTests
{
	TEST_CLASS(UnitTestParser)
	{
	public:

		TEST_METHOD(Test_SingleParserEmptyCode)
        {
			std::shared_ptr<object> result = LispParser::Parse("()");
			QVERIFY(result.get() != 0);
			QVERIFY(result->IsList());
			QCOMPARE((size_t)0, result->ToList()->size());
        }

		TEST_METHOD(Test_SingleParser1)
        {
			std::shared_ptr<object> result = LispParser::Parse("(print 1 2.54 \"string\")");
			QVERIFY(result.get() != 0);
			QVERIFY(result->IsList());
			var resultAsArray = result->ToEnumerableOfObjectRef();
            QCOMPARE((size_t)4, resultAsArray.size());

            var value = resultAsArray[0]->ToLispVariant();
            QVERIFY(value->IsSymbol());
            QCOMPARE("print", value->Value->ToString().c_str());
            value = resultAsArray[1]->ToLispVariant();
            QVERIFY(value->IsInt());
            QCOMPARE(1, (int)*(value->Value));
            value = resultAsArray[2]->ToLispVariant();
            QVERIFY(value->IsDouble());
            QCOMPARE(2.54, (double)*(value->Value));
            value = resultAsArray[3]->ToLispVariant();
            QVERIFY(value->IsString());
            QCOMPARE("string", value->Value->ToString().c_str());
        }

		TEST_METHOD(Test_SingleParser2)
		{
			std::shared_ptr<object> result = LispParser::Parse("(do (print #t 2.54 \"string\"))");
			QVERIFY(result.get() != 0);
			QVERIFY(result->IsList());
			var resultAsArrayDo = result->ToEnumerableOfObjectRef();
			QCOMPARE((size_t)2, resultAsArrayDo.size());

            var value = resultAsArrayDo[0]->ToLispVariant();
            QVERIFY(value->IsSymbol());
            QCOMPARE("do", value->Value->ToString().c_str());

            var listValue = resultAsArrayDo[1]->ToList();
            var resultAsArray = listValue->ToArray();
            value = resultAsArray[1]->ToLispVariant();
            QVERIFY(value->IsBool());
            QCOMPARE(true, (bool)*(value->Value));
            value = resultAsArray[2]->ToLispVariant();
            QVERIFY(value->IsDouble());
            QCOMPARE(2.54, (double)*(value->Value));
            value = resultAsArray[3]->ToLispVariant();
            QVERIFY(value->IsString());
            QCOMPARE("string", value->Value->ToString().c_str());
        }
	};
}
