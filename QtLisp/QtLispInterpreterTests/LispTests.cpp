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

#include "../CsLispInterpreter/Lisp.h"

using namespace Microsoft::VisualStudio::CppUnitTestFramework;

using namespace CsLisp;

namespace QtLispUnitTests
{
	TEST_CLASS(UnitTestLisp)
	{
	public:

		TEST_METHOD(Test_Comments)
		{
			std::shared_ptr<LispVariant> result = Lisp::Eval("(do (println \"hello\") ; a comment\n(println; separate lists with comments\n\"world\"));comment in last line");
			Assert::AreEqual("world", result->ToString().c_str());
		}

		TEST_METHOD(Test_DoAndPrint)
		{
			std::shared_ptr<LispVariant> result = Lisp::Eval("(do (println \"hello\")\n (println \"world\"))");
			Assert::AreEqual("world", result->ToString().c_str());
		}

		TEST_METHOD(Test_PrintLnMultilines)
		{
			std::shared_ptr<LispVariant> result = Lisp::Eval("(do (println \"hello\nworld\"))");
			Assert::AreEqual("hello\nworld", result->ToString().c_str());
		}

		TEST_METHOD(Test_PrintTrace)
		{
			std::shared_ptr<LispVariant> result = Lisp::Eval("(do (trace #t) (println \"hello world\") (println (+ 9 8)) (gettrace))");
			Assert::AreEqual("hello world17", result->ToString().c_str());
		}

		TEST_METHOD(Test_If1)
		{
			std::shared_ptr<LispVariant> result = Lisp::Eval("(if #t (+ 1 2) (- 3 5))");
			Assert::AreEqual(3, result->ToInt());
		}

		TEST_METHOD(Test_If2)
		{
			std::shared_ptr<LispVariant> result = Lisp::Eval("(if #f (* 1 0) (/ 6 3))");
			Assert::AreEqual(2, result->ToInt());
		}

		TEST_METHOD(Test_If3)
		{
			std::shared_ptr<LispVariant> result = Lisp::Eval("(if true 1 0)");
			Assert::AreEqual(1, result->ToInt());
		}

		TEST_METHOD(Test_If4)
		{
			std::shared_ptr<LispVariant> result = Lisp::Eval("(if false 1 0)");
			Assert::AreEqual(0, result->ToInt());
		}

		TEST_METHOD(Test_If5)
		{
			std::shared_ptr<LispVariant> result = Lisp::Eval("(if true 1)");
			Assert::AreEqual(1, result->ToInt());
			result = Lisp::Eval("(if false 1)");
			Assert::IsNull(result.get());
		}

		TEST_METHOD(Test_While1)
		{
			std::shared_ptr<LispVariant> result = Lisp::Eval("(do (def a 1) (def b 1) (while (< a 10) (do (setf a (+ a 1)) (setf b (+ b 1)))))");
			Assert::AreEqual(10, result->ToInt());
		}

		TEST_METHOD(Test_Setf1)
		{
			std::shared_ptr<LispVariant> result = Lisp::Eval("(do (def a 1) (def b 1) (setf (first (list 'a 'b 'c)) 9))");
			Assert::AreEqual(9, result->ToInt());
		}

		TEST_METHOD(Test_Defn1)
		{
			std::shared_ptr<LispVariant> result = Lisp::Eval("(do (def g_prn \"START:\") (defn prn (x) (setf g_prn (add g_prn x))) (prn \"34\") (println g_prn))");
			Assert::AreEqual("START:34", result->ToString().c_str());
		}

		TEST_METHOD(Test_AddString)
		{
			std::shared_ptr<LispVariant> result = Lisp::Eval("(+ \"abc\" \"def() ; blub\" \"xxx\")");
			Assert::AreEqual("abcdef() ; blubxxx", result->ToString().c_str());
		}

		TEST_METHOD(Test_AddLists)
		{
			std::shared_ptr<LispVariant> result = Lisp::Eval("(+ '(1 2 3) '(\"hello world\" 2.3 42))");
			string s = result->ToString();
			Assert::AreEqual("(1 2 3 \"hello world\" 2.300000 42)", result->ToString().c_str());
		}

		TEST_METHOD(Test_ListFirst)
		{
			std::shared_ptr<LispVariant> result = Lisp::Eval("(first '(1 2 3))");
			Assert::AreEqual(1, result->ToInt());
		}

		TEST_METHOD(Test_ListCar)
		{
			std::shared_ptr<LispVariant> result = Lisp::Eval("(car '(\"abc\" 2 3))");
			Assert::AreEqual("abc", result->ToString().c_str());
		}

		TEST_METHOD(Test_ListRest)
		{
			std::shared_ptr<LispVariant> result = Lisp::Eval("(rest '(1 2 3))");
			Assert::AreEqual("(2 3)", result->ToString().c_str());
		}

		TEST_METHOD(Test_ListCdr)
		{
			std::shared_ptr<LispVariant> result = Lisp::Eval("(cdr '(\"nix\" 1 2 3))");
			Assert::AreEqual("(1 2 3)", result->ToString().c_str());
		}

		TEST_METHOD(Test_ListLength)
		{
			std::shared_ptr<LispVariant> result = Lisp::Eval("(len '(1 2 3))");
			Assert::AreEqual(3, result->ToInt());
		}

		TEST_METHOD(Test_ListAppend)
		{
			std::shared_ptr<LispVariant> result = Lisp::Eval("(append (list 4 54 3) (list 7 9))");
			Assert::AreEqual("(4 54 3 7 9)", result->ToString().c_str());
		}

		TEST_METHOD(Test_LogicalOperators)
		{
			std::shared_ptr<LispVariant> result = Lisp::Eval("(list (and #t #f) (and #t #t) (or #t #f) (or #f #f) (or #t #f #t))");
			Assert::AreEqual("(#f #t #t #f #t)", result->ToString().c_str());
		}

		TEST_METHOD(Test_CompareOperators1)
		{
			std::shared_ptr<LispVariant> result = Lisp::Eval("(list (= 1 2) (= 4 4) (== \"blub\" \"blub\") (== #t #f) (equal 3 4))");
			Assert::AreEqual("(#f #t #t #f #f)", result->ToString().c_str());
			result = Lisp::Eval("(do (def a ()) (== a ()))");
			Assert::AreEqual(true, result->BoolValue());
			result = Lisp::Eval("(do (def a 42) (== a ()))");
			Assert::AreEqual(false, result->BoolValue());
			result = Lisp::Eval("(do (def a blub) (def b nix) (== a b))");
			Assert::AreEqual(false, result->BoolValue());
			result = Lisp::Eval("(do (def a blub) (def b blub) (== a b))");
			Assert::AreEqual(true, result->BoolValue());
			result = Lisp::Eval("(do (def a blub) (def b blub) (== a b))");
			Assert::AreEqual(true, result->BoolValue());
			result = Lisp::Eval("(do (def a (list 1 2 3)) (def b (list 2 3 4)) (== a b))");
			Assert::AreEqual(false, result->BoolValue());
			result = Lisp::Eval("(do (def a (list 1 2 3)) (def b (list 1 2 3)) (== a b))");
			Assert::AreEqual(true, result->BoolValue());
			result = Lisp::Eval("(do (def a (list 1 2 3)) (def b (list 1 (sym 2) 3)) (== a b))");
			Assert::AreEqual(false, result->BoolValue());
			result = Lisp::Eval("(do (def a blub) (def b nix) (!= a b))");
			Assert::AreEqual(true, result->BoolValue());
			result = Lisp::Eval("(do (def a 7) (def b 7) (!= a b))");
			Assert::AreEqual(false, result->BoolValue());
		}

		TEST_METHOD(Test_CompareOperators2)
		{
			std::shared_ptr<LispVariant> result = Lisp::Eval("(list (< 1 2) (< 4 1) (> 5 2) (> 1 3) (> 4.0 4.0))");
			Assert::AreEqual("(#t #f #t #f #f)", result->ToString().c_str());
			result = Lisp::Eval("(do (def a \"abc\") (def b \"def\") (< a b))");
			Assert::AreEqual(true, result->BoolValue());
			result = Lisp::Eval("(do (def a \"abc\") (def b \"abc\") (< a b))");
			Assert::AreEqual(false, result->BoolValue());
			result = Lisp::Eval("(do (def a \"abc\") (def b \"def\") (<= a b))");
			Assert::AreEqual(true, result->BoolValue());
			result = Lisp::Eval("(do (def a \"abc\") (def b \"abc\") (<= a b))");
			Assert::AreEqual(true, result->BoolValue());
		}

		TEST_METHOD(Test_CompareOperators3)
		{
			std::shared_ptr<LispVariant> result = Lisp::Eval("(list (<= 1 2) (<= 4 1) (>= 5 2) (>= 1 3) (>= 4.0 4.0) (<= 42 42))");
			Assert::AreEqual("(#t #f #t #f #t #t)", result->ToString().c_str());
		}

		TEST_METHOD(Test_Not)
		{
			std::shared_ptr<LispVariant> result = Lisp::Eval("(list (! #t) (not #t) (not #f) (! #f))");
			Assert::AreEqual("(#f #f #t #t)", result->ToString().c_str());
		}

		TEST_METHOD(Test_Arithmetric1)
		{
			std::shared_ptr<LispVariant> result = Lisp::Eval("(+ 1 2 3 4)");
			Assert::AreEqual(10, result->ToInt());
		}

		TEST_METHOD(Test_DefWithNil)
		{
			std::shared_ptr<LispVariant> result = Lisp::Eval("(do (def a nil) (println a))");
			Assert::AreEqual(LispToken::NilConst.c_str(), result->ToString().c_str());
		}

		TEST_METHOD(Test_Fn)
		{
			std::shared_ptr<LispVariant> result = Lisp::Eval("(do (def f (fn (x) (+ x x 1))) (println (f 8)))");
			Assert::AreEqual(17, result->ToInt());
		}

		TEST_METHOD(Test_Map)
		{
			std::shared_ptr<LispVariant> result = Lisp::Eval("(map (lambda (x) (+ x 1)) '(1 2 3))");
			Assert::AreEqual("(2 3 4)", result->ToString().c_str());
		}

		/*
		TEST_METHOD(Test_MapError1)
		//[ExpectedException(typeof(LispException))]
		public void Test_MapError1()
		{
			Lisp::Eval("(map 4 '(1 2 3))");
		}
		*/
	};
}
