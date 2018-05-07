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

#include <memory>

#include "../CsLispInterpreter/csstring.h"
#include "../CsLispInterpreter/csobject.h"
#include "../CsLispInterpreter/Variant.h"

using namespace Microsoft::VisualStudio::CppUnitTestFramework;

using namespace CsLisp;

namespace QtLispInterpreterTests
{
	TEST_CLASS(UnitTestVariant)
	{
	public:

		TEST_METHOD(Test_CreateVariant)
		{
			LispVariant * variant = new LispVariant(LispType::_Nil);
			Assert::IsNotNull(variant);

			delete variant;
			variant = new LispVariant(std::make_shared<object>(3));
			Assert::IsTrue(variant->IsInt());
			Assert::AreEqual(3, variant->IntValue());

			delete variant;
			variant = new LispVariant(std::make_shared<object>(3.1415));
			Assert::IsTrue(variant->IsDouble());
			Assert::AreEqual(3.1415, variant->DoubleValue());

			delete variant;
			variant = new LispVariant(std::make_shared<object>(string("text")));
			Assert::IsTrue(variant->IsString());

			delete variant;
			variant = new LispVariant(std::make_shared<object>("blub"));
			Assert::IsTrue(variant->IsString());
			Assert::IsTrue(string("blub") == variant->ToString());
		}

		TEST_METHOD(Test_VariantCompare)
		{
			std::shared_ptr<LispVariant> variant1 = std::make_shared<LispVariant>(LispVariant(std::make_shared<object>(4.3)));
			std::shared_ptr<LispVariant> variant2 = std::make_shared<LispVariant>(LispVariant(std::make_shared<object>(56.1)));
			std::shared_ptr<LispVariant> variant3 = std::make_shared<LispVariant>(LispVariant(std::make_shared<object>(42)));
			std::shared_ptr<LispVariant> variant4 = std::make_shared<LispVariant>(LispVariant(std::make_shared<object>(string("abc"))));
			Assert::IsTrue(variant1->CompareTo(variant2) < 0);
			Assert::IsTrue(variant2->CompareTo(variant1) > 0);
			Assert::IsTrue(variant1->CompareTo(std::make_shared<object>(1.23)) > 0);
			Assert::IsTrue(variant1->CompareTo(std::make_shared<object>(-5)) > 0);
			Assert::IsTrue(variant3->CompareTo(std::make_shared<object>(42)) == 0);
			Assert::IsTrue(variant4->CompareTo(std::make_shared<object>("abc")) == 0);
			Assert::IsTrue(variant4->CompareTo(std::make_shared<object>("xyz")) < 0);
		}
	};
}