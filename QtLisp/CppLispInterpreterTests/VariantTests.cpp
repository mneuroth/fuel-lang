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

#include "../CppLispInterpreter/csstring.h"
#include "../CppLispInterpreter/csobject.h"
#include "../CppLispInterpreter/Variant.h"

using namespace Microsoft::VisualStudio::CppUnitTestFramework;

using namespace CsLisp;

namespace QtLispUnitTests
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

		TEST_METHOD(Test_VariantConvert)
		{
			std::shared_ptr<LispVariant> variant1 = std::make_shared<LispVariant>(LispVariant(std::make_shared<object>(4.3)));
			std::shared_ptr<LispVariant> variant2 = std::make_shared<LispVariant>(LispVariant(std::make_shared<object>(56.1)));
			std::shared_ptr<LispVariant> variant3 = std::make_shared<LispVariant>(LispVariant(std::make_shared<object>(42)));
			std::shared_ptr<LispVariant> variant4 = std::make_shared<LispVariant>(LispVariant(std::make_shared<object>("4.5")));
			std::shared_ptr<LispVariant> variant5 = std::make_shared<LispVariant>(LispVariant(std::make_shared<object>(true)));
			std::shared_ptr<LispVariant> variant6 = std::make_shared<LispVariant>(LispVariant(LispType::_Int, std::make_shared<object>(0)));
			Assert::AreEqual(true, variant1->ToBool());
			Assert::AreEqual(true, variant3->ToBool());
			Assert::AreEqual(false, variant6->ToBool());
			Assert::AreEqual(4.5, variant4->ToDouble());
			Assert::AreEqual(1.0, variant5->ToDouble());
			Assert::AreEqual(56, variant2->ToInt());
			Assert::AreEqual(true, variant2->ToBool());
		}

		TEST_METHOD(Test_VariantOperations)
		{
			std::shared_ptr<LispVariant> variant1 = std::make_shared<LispVariant>(LispVariant(std::make_shared<object>(4.3)));
			std::shared_ptr<LispVariant> variant2 = std::make_shared<LispVariant>(LispVariant(std::make_shared<object>(56.1)));
			std::shared_ptr<LispVariant> variant3 = std::make_shared<LispVariant>(LispVariant(std::make_shared<object>(42)));
			std::shared_ptr<LispVariant> variant4 = std::make_shared<LispVariant>(LispVariant(std::make_shared<object>(45)));
			Assert::AreEqual(1890, (*variant3 * *variant4).ToInt());
			Assert::AreEqual(60.4, (*variant1 + *variant2).ToDouble());
		}

		TEST_METHOD(Test_VariantEqualOp)
		{
			std::shared_ptr<LispVariant> variant1 = std::make_shared<LispVariant>(LispVariant(std::make_shared<object>(4.3)));
			std::shared_ptr<LispVariant> variant2 = std::make_shared<LispVariant>(LispVariant(std::make_shared<object>(56.1)));
			Assert::IsFalse(LispVariant::EqualOp(*variant1, *variant2));
			Assert::IsTrue(LispVariant::EqualOp(*variant1, *variant1));
		}
	
		TEST_METHOD(Test_VariantCastError)
		{
			try
			{
				std::shared_ptr<LispVariant> variant = std::make_shared<LispVariant>(LispVariant(std::make_shared<object>(4.3)));
				//Assert::IsNotNull(variant);
				int value = variant->IntValue();
				Assert::AreEqual(4, value);      // will not be evaluated because of expected exception !
				Assert::IsTrue(false);
			}
			catch (LispException)
			{
				Assert::IsTrue(true);
			}
		}

	};
}