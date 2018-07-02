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

#include <math.h>

double Math_Round(double val)
{
	return round(val);
}

namespace QtLispUnitTests
{
	TEST_CLASS(UnitTestLispInterpreter)
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

		TEST_METHOD(Test_Setf1)
		{
			std::shared_ptr<LispVariant> result = Lisp::Eval("(do (def a 1) (def b 1) (setf (first (list 'a 'b 'c)) 9))");
			Assert::AreEqual(9, result->ToInt());
		}

		TEST_METHOD(Test_DefWithNil)
		{
			std::shared_ptr<LispVariant> result = Lisp::Eval("(do (def a nil) (println a))");
			Assert::AreEqual(LispToken::NilConst.c_str()	, result->ToString().c_str());
		}

		TEST_METHOD(Test_Fn)
		{
			std::shared_ptr<LispVariant> result = Lisp::Eval("(do (def f (fn (x) (+ x x 1))) (println (f 8)))");
			Assert::AreEqual(17, result->ToInt());
		}

		TEST_METHOD(Test_Gdef)
		{
			std::shared_ptr<LispVariant> result = Lisp::Eval("(do (defn f (x) (gdef z (+ x x))) (f 8) (println z))");
			Assert::AreEqual(16, result->ToInt());
		}

		TEST_METHOD(Test_Gdefn)
		{
			std::shared_ptr<LispVariant> result = Lisp::Eval("(do (defn f (x) (gdefn g (x) (+ x x))) (f 2) (g 8))");
			Assert::AreEqual(16, result->ToInt());
		}

		TEST_METHOD(Test_Eval1)
		{
			std::shared_ptr<LispVariant> result = Lisp::Eval("(eval (list 'def 'x 43))");
			Assert::AreEqual(43, result->ToInt());
		}

		TEST_METHOD(Test_Eval2)
		{
			std::shared_ptr<LispVariant> result = Lisp::Eval("(eval '(def x 456))");
			Assert::AreEqual(456, result->ToInt());
		}

		TEST_METHOD(Test_Eval3)
		{
			std::shared_ptr<LispVariant> result = Lisp::Eval("(eval #t)");
			Assert::AreEqual(true, result->ToBool());
			result = Lisp::Eval("(eval 42)");
			Assert::AreEqual(42, result->ToInt());
		}

		TEST_METHOD(Test_EvalStr)
		{
			std::shared_ptr<LispVariant> result = Lisp::Eval("(evalstr \"(def x 456)\")");
			Assert::AreEqual(456, result->ToInt());
		}

		TEST_METHOD(Test_Doc)
		{
			std::shared_ptr<LispVariant> result = Lisp::Eval("(doc 'if)");
			var s = result->ToString();
			Assert::IsTrue(s.Contains("-------------------------------------------------"));
			Assert::IsTrue(s.Contains("if  [special form]"));
			Assert::IsTrue(s.Contains("Syntax: (if cond then-block [else-block])"));
			Assert::IsTrue(s.Contains("The if statement."));
		}

		TEST_METHOD(Test_DocForDefn)
		{
			std::shared_ptr<LispVariant> result = Lisp::Eval("; this is a fcn documenataion\n(defn blub (x) (+ x 1))\n(doc 'blub)");
			var s = result->ToString();
			Assert::IsTrue(s.Contains("-------------------------------------------------"));
			Assert::IsTrue(s.Contains("blub"));
			Assert::IsTrue(s.Contains("Syntax: (blub x)"));
			Assert::IsTrue(s.Contains("; this is a fcn documenataion"));
		}

		TEST_METHOD(Test_NoAutoDocForDefn)
		{
			std::shared_ptr<LispVariant> result = Lisp::Eval("(defn blub (x y ) (+ x y 1))\n(doc 'blub)");
			var s = result->ToString();
			Assert::IsTrue(s.Contains("-------------------------------------------------"));
			Assert::IsTrue(s.Contains("blub"));
			Assert::IsTrue(s.Contains("Syntax: (blub x y)"));
		}

		TEST_METHOD(Test_TickCount)
		{
			std::shared_ptr<LispVariant> result = Lisp::Eval("(tickcount)");
			Assert::IsTrue(result->IsInt());
		}

		TEST_METHOD(Test_While1)
		{
			std::shared_ptr<LispVariant> result = Lisp::Eval("(do (def a 1) (def b 1) (while (< a 10) (do (setf a (+ a 1)) (setf b (+ b 1)))))");
			Assert::AreEqual(10, result->ToInt());
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

		TEST_METHOD(Test_Arithmetric2)
		{
			std::shared_ptr<LispVariant> result = Lisp::Eval("(+ 1.1 2.2 3.3 4.3)");
			int res = (int)Math_Round(result->ToDouble() * 10.0);
			Assert::AreEqual(109, res);
		}

		TEST_METHOD(Test_Arithmetric3)
		{
			std::shared_ptr<LispVariant> result = Lisp::Eval("(* 3 8 2)");
			Assert::AreEqual(48, result->ToInt());
		}

		TEST_METHOD(Test_Arithmetric4)
		{
			std::shared_ptr<LispVariant> result = Lisp::Eval("(/ 1.0 2.0)");
			int res = (int)Math_Round(result->ToDouble() * 10.0);
			Assert::AreEqual(5, res);
		}

		TEST_METHOD(Test_Arithmetric5)
		{
			std::shared_ptr<LispVariant> result = Lisp::Eval("(/ 10 2)");
			Assert::AreEqual(5, result->ToInt());
		}

		TEST_METHOD(Test_Arithmetric6)
		{
			std::shared_ptr<LispVariant> result = Lisp::Eval("(- 42 12 6)");
			Assert::AreEqual(24, result->ToInt());
		}

		TEST_METHOD(Test_Arithmetric7)
		{
			std::shared_ptr<LispVariant> result = Lisp::Eval("(- 42.5 0.5)");
			int res = (int)Math_Round(result->ToDouble() * 10.0);
			Assert::AreEqual(420, res);
		}

		TEST_METHOD(Test_Arithmetric8)
		{
			std::shared_ptr<LispVariant> result = Lisp::Eval("(sub 42.5 1.5 2.0)");
			int res = (int)Math_Round(result->ToDouble() * 10.0);
			Assert::AreEqual(390, res);
		}

		TEST_METHOD(Test_Arithmetric9)
		{
			std::shared_ptr<LispVariant> result = Lisp::Eval("(div 12 3)");
			Assert::AreEqual(4, result->ToInt());
		}

		TEST_METHOD(Test_Arithmetric10)
		{
			std::shared_ptr<LispVariant> result = Lisp::Eval("(mul 2 3 4)");
			Assert::AreEqual(24, result->ToInt());
		}

		TEST_METHOD(Test_Arithmetric11)
		{
			std::shared_ptr<LispVariant> result = Lisp::Eval("(add 2 3 4)");
			Assert::AreEqual(9, result->ToInt());
		}

		TEST_METHOD(Test_MacrosEvaluateNested)
		{
			const string macroExpandScript = "(do\
			(define-macro-eval first-macro\
				(a b)\
				(do\
					(println first-macro)\
					(def i 1)\
					(+ a b i)\
				)\
			)\
\
			(define-macro-eval second-macro\
				(x y)\
			    (do\
					(println second-macro)\
					(* x y (first-macro (+ x 1) (+ y 2)))\
				)\
			)\
\
			(def m (second-macro 4 3))\
		)";

			//using (ConsoleRedirector cr = new ConsoleRedirector())
			{
				var scope = LispEnvironment::CreateDefaultScope();
				scope->Output.EnableToString();
				std::shared_ptr<LispVariant> result = Lisp::Eval(macroExpandScript, scope);
				Assert::AreEqual("132", result->ToString().c_str());

				//string s = cr.ToString().Trim();
				string s = scope->Output.GetContent().Trim();
				Assert::IsTrue(s.Contains("first-macro"));
				Assert::IsTrue(s.Contains("second-macro"));
			}
		}

		TEST_METHOD(Test_MacrosEvaluateRecursive)
		{
			const string macroExpandScript = "(do\
			(define-macro-eval first-macro\
				(a b)\
				(do\
					(println first-macro)\
					(def i 1)\
					(+ a b i)\
				)\
			)\
\
			(define-macro-eval second-macro\
				(x y)\
				(do\
					(println second-macro)\
					(* x y (first-macro x (+ y 4)))\
				)\
			)\
\
			(def m (second-macro 4 3))\
		)";

			//using (ConsoleRedirector cr = new ConsoleRedirector())
			{
				std::shared_ptr<LispVariant> result = Lisp::Eval(macroExpandScript);
				Assert::AreEqual("144", result->ToString().c_str());

				//string s = cr.ToString().Trim();
				//Assert.IsTrue(s.Contains("first-macro"));
				//Assert.IsTrue(s.Contains("second-macro"));
			}
		}


		TEST_METHOD(Test_MacrosEvaluateDoubleMacroCall)
		{
			const string macroExpandScript = "(do\
			(define-macro-eval first-macro\
				(a b)\
				(do\
					(println first-macro)\
					(def i 1)\
					(+ a b i)\
				)\
			)\
\
			(define-macro-eval second-macro\
				(x y)\
				(do\
					(println second-macro)\
					(* x y)\
				)\
			)\
\
			(def m (second-macro 4 (first-macro 6 3)))\
		)";

			//using (ConsoleRedirector cr = new ConsoleRedirector())
			{
				std::shared_ptr<LispVariant> result = Lisp::Eval(macroExpandScript);
				Assert::AreEqual("40", result->ToString().c_str());

				//string s = cr.ToString().Trim();
				//Assert.IsTrue(s.Contains("first-macro"));
				//Assert.IsTrue(s.Contains("second-macro"));
			}
		}

		TEST_METHOD(Test_MacrosSetf2)
		{
			std::shared_ptr<LispVariant> result = Lisp::Eval("(do (def a 42) (defn my-setf (x value) (setf x value)) (my-setf a (+ 8 9)) (println a))");
			Assert::AreEqual(42, result->ToInt());
		}

		TEST_METHOD(Test_MacrosSetf3)
		{
			std::shared_ptr<LispVariant> result = Lisp::Eval("(do (def a 42) (define-macro-eval my-setf (x value) (setf x value)) (my-setf a (+ \"blub\" \"xyz\")) (println a))");
			Assert::AreEqual("blubxyz", result->ToString().c_str());
		}

		TEST_METHOD(Test_Quasiquote1)
		{
			std::shared_ptr<LispVariant> result = Lisp::Eval("(do (def a '(42 99 102 \"hello\")) (def b 55) (println (type a)) (println (nth 3 `(1 2 3 ,@a))))");
			Assert::AreEqual("42", result->ToString().c_str());
		}

		TEST_METHOD(Test_Quasiquote2)
		{
			std::shared_ptr<LispVariant> result = Lisp::Eval("(do (def a 42) (println `(1 2 3 ,a)))");
			Assert::AreEqual("(1 2 3 42)", result->ToString().c_str());
		}

		TEST_METHOD(Test_Quasiquote3)
		{
			std::shared_ptr<LispVariant> result = Lisp::Eval("(do (def a 42) (def lst (list 6 8 12)) (println (quasiquote (1 2 3 ,a ,@lst))))");
			Assert::AreEqual("(1 2 3 42 6 8 12)", result->ToString().c_str());
		}

		TEST_METHOD(Test_Quasiquote4)
		{
			std::shared_ptr<LispVariant> result = Lisp::Eval("(do (def a 42) (def lst (list 6 8 12)) (println (quasiquote (1 2 3 ,a ,lst))))");
			Assert::AreEqual("(1 2 3 42 (6 8 12))", result->ToString().c_str());
		}

		TEST_METHOD(Test_Quote1)
		{
			std::shared_ptr<LispVariant> result = Lisp::Eval("(do (def x 42) (println 'x))");
			Assert::AreEqual("x", result->ToString().c_str());
		}

		TEST_METHOD(Test_String1)
		{
			std::shared_ptr<LispVariant> result = Lisp::Eval("(println \"hello \\\\ \\' צהü \n \\\"blub\\\"\")");
			Assert::AreEqual("hello \\ ' צהü \n \"blub\"", result->ToString().c_str());
		}

		TEST_METHOD(Test_String2)
		{
			std::shared_ptr<LispVariant>  result = Lisp::Eval("(string \"hello\" \"-\" \"world\")");
			Assert::AreEqual("hello-world", result->ToString().c_str());
		}

		TEST_METHOD(Test_Map)
		{
			std::shared_ptr<LispVariant> result = Lisp::Eval("(map (lambda (x) (+ x 1)) '(1 2 3))");
			Assert::AreEqual("(2 3 4)", result->ToString().c_str());
		}

		TEST_METHOD(Test_Reduce1)
		{
			std::shared_ptr<LispVariant> result = Lisp::Eval("(reduce (lambda (x y) (+ x y)) '(1 2 3) 0)");
			Assert::AreEqual(6, result->ToInt());
		}

		TEST_METHOD(Test_Reduce2)
		{
			std::shared_ptr<LispVariant> result = Lisp::Eval("(reduce (lambda (x y) (* x y)) '(2 3 4 5) 2)");
			Assert::AreEqual(240, result->ToInt());
		}

		TEST_METHOD(Test_Closure1)
		{
			std::shared_ptr<LispVariant> result = Lisp::Eval("(do (defn addx (delta) (lambda (x) (+ x delta))) (def addclosure (addx 41)) (println (addclosure 1)))");
			Assert::AreEqual(42, result->ToInt());
		}

		TEST_METHOD(Test_Closure2)
		{
			std::shared_ptr<LispVariant> result = Lisp::Eval("(do (defn g (x) (do (+ x 2))) (defn f (x) (do (def i 7) (+ x 1 i (g 2)))) (println (f 1)))");
			Assert::AreEqual(13, result->	ToInt());
		}

		TEST_METHOD(Test_Closure3)
		{
			try
			{
				Lisp::Eval("(do (defn g (x) (do (+ x 2 i))) (defn f (x) (do (def i 7) (+ x 1 i (g 2)))) (println (f 1)))");
				Assert::IsTrue(false);
			}
			catch (const CsLisp::LispException &)
			{
				Assert::IsTrue(true);
			}
			catch (...)
			{
				Assert::IsTrue(false);
			}
		}

		TEST_METHOD(Test_RecursiveCall1)
		{
			std::shared_ptr<LispVariant> result = Lisp::Eval("(do (defn addConst (x a) (+ x a)) (def add2 (lambda (x) (addConst x 2))) (println (addConst 8 2)) (println (add2 4)))");
			Assert::AreEqual(6, result->ToInt());
		}

		TEST_METHOD(Test_Return1)
		{
			std::shared_ptr<LispVariant> result = Lisp::Eval("(do (defn f (x) (return (+ x x))) (println (f 7)))");
			Assert::AreEqual(14, result->ToInt());
		}

		TEST_METHOD(Test_Apply1)
		{
			std::shared_ptr<LispVariant> result = Lisp::Eval("(apply (lambda (x) (println \"hello\" x)) '(55))");
			Assert::AreEqual("hello 55", result->ToString().c_str());
		}

		TEST_METHOD(Test_Apply2)
		{
			std::shared_ptr<LispVariant> result = Lisp::Eval("(do (def f (lambda (x) (+ x x))) (apply f '(5)))");
			Assert::AreEqual(10, result->ToInt());
		}

		TEST_METHOD(Test_Apply3)
		{
			std::shared_ptr<LispVariant> result = Lisp::Eval("(do (def f '+) (apply f '(5 6 7)))");
			Assert::AreEqual(18, result->ToInt());
		}

		TEST_METHOD(Test_Args1)
		{
			std::shared_ptr<LispVariant> result = Lisp::Eval("(do (defn f (x) (do (println \"count=\" (argscount)) (+ x x))) (f 5 6 7))");
			Assert::AreEqual(10, result->ToInt());
		}

		TEST_METHOD(Test_Args2)
		{
			//using (ConsoleRedirector cr = new ConsoleRedirector())
			{
				std::shared_ptr<LispVariant> result = Lisp::Eval("(do (defn f (x) (do (println \"count=\" (argscount)) (println (args 0)) (println (args 1)) (println (args 2)) (println \"additional=\" (nth 1 _additionalArgs)) (+ x x))) (f 5 6 7))");
				Assert::AreEqual(10, result->ToInt());

				//string s = cr.ToString().Trim();
				//Assert.AreEqual(true, s.Contains("count= 3"));
				//Assert.AreEqual(true, s.Contains("5"));
				//Assert.AreEqual(true, s.Contains("6"));
				//Assert.AreEqual(true, s.Contains("7"));
				//Assert.AreEqual(true, s.Contains("additional= 7"));
			}
		}

		TEST_METHOD(Test_Args3)
		{
			try
			{
				std::shared_ptr<LispVariant> result = Lisp::Eval("(do (defn f (x) (do (args 7) (+ x x))) (f 5 6 7))");
				Assert::AreEqual(10, result->ToInt());
				Assert::IsTrue(false);
			}
			catch (const CsLisp::LispException &)
			{
				Assert::IsTrue(true);
			}
			catch (...)
			{
				Assert::IsTrue(false);
			}
		}

		TEST_METHOD(Test_Cons1)
		{
			std::shared_ptr<LispVariant> result = Lisp::Eval("(cons 1 2)");
			Assert::AreEqual("(1 2)", result->ToString().c_str());
		}

		TEST_METHOD(Test_Cons2)
		{
			std::shared_ptr<LispVariant> result = Lisp::Eval("(cons 1 '(2 3 4))");
			Assert::AreEqual("(1 2 3 4)", result->ToString().c_str());
		}

		TEST_METHOD(Test_Cons3)
		{
			std::shared_ptr<LispVariant> result = Lisp::Eval("(cons 12)");
			Assert::AreEqual("(12)", result->ToString().c_str());
		}

		TEST_METHOD(Test_Cons4)
		{
			std::shared_ptr<LispVariant> result = Lisp::Eval("(cons)");
			Assert::AreEqual("()", result->ToString().c_str());
		}

		TEST_METHOD(Test_Nop)
		{
			std::shared_ptr<LispVariant> result = Lisp::Eval("(nop)");
			Assert::IsTrue(result->IsUndefined());
		}

		TEST_METHOD(Test_Symbol)
		{
			std::shared_ptr<LispVariant> result = Lisp::Eval("(sym a)");
			Assert::IsTrue(result->IsSymbol());
			Assert::AreEqual("a", result->StringValue().c_str());
		}

		TEST_METHOD(Test_Str)
		{
			std::shared_ptr<LispVariant> result = Lisp::Eval("(str abc)");
			Assert::IsTrue(result->IsString());
			Assert::AreEqual("abc", result->StringValue().c_str());
		}

		TEST_METHOD(Test_MapError1)
		{
			try
			{
				Lisp::Eval("(map 4 '(1 2 3))");
				Assert::IsTrue(false);
			}
			catch (const CsLisp::LispException &)
			{
				Assert::IsTrue(true);
			}
			catch (...)
			{
				Assert::IsTrue(false);
			}
		}

		TEST_METHOD(Test_MapError2)
		{
			try
			{
				Lisp::Eval("(map (lambda (x) (+ x 1)) 4)");
				Assert::IsTrue(false);
			}
			catch (const CsLisp::LispException &)
			{
				Assert::IsTrue(true);
			}
			catch (...)
			{
				Assert::IsTrue(false);
			}
		}

		TEST_METHOD(Test_ReduceError1)
		{
			try
			{
				Lisp::Eval("(reduce \"blub\" '(1 2 3) 0)");
				Assert::IsTrue(false);
			}
			catch (const CsLisp::LispException &)
			{
				Assert::IsTrue(true);
			}
			catch (...)
			{
				Assert::IsTrue(false);
			}
		}

		TEST_METHOD(Test_ReduceError2)
		{
			try
			{
				Lisp::Eval("(reduce (lambda (x y) (+ x y))  \"test\" 0)");
				Assert::IsTrue(false);
			}
			catch (const CsLisp::LispException &)
			{
				Assert::IsTrue(true);
			}
			catch (...)
			{
				Assert::IsTrue(false);
			}
		}

		TEST_METHOD(Test_SetfError)
		{
			try
			{
				Lisp::Eval("(setf a 2.0)");
				Assert::IsTrue(false);
			}
			catch (const CsLisp::LispException &)
			{
				Assert::IsTrue(true);
			}
			catch (...)
			{
				Assert::IsTrue(false);
			}
		}

		TEST_METHOD(Test_Parser1)
		{
			try
			{
				Lisp::Eval("(println \"hello\"))");
				Assert::IsTrue(false);
			}
			catch (const CsLisp::LispException &)
			{
				Assert::IsTrue(true);
			}
			catch (...)
			{
				Assert::IsTrue(false);
			}
		}

		TEST_METHOD(Test_Parser2)
		{
			try
			{
				Lisp::Eval("((println \"hello\")");
				Assert::IsTrue(false);
			}
			catch (const CsLisp::LispException &)
			{
				Assert::IsTrue(true);
			}
			catch (...)
			{
				Assert::IsTrue(false);
			}
		}

		TEST_METHOD(Test_Parser3)
		{
			try
			{
				Lisp::Eval("(blub 1 2 3)");
				Assert::IsTrue(false);
			}
			catch (const CsLisp::LispException &)
			{
				Assert::IsTrue(true);
			}
			catch (...)
			{
				Assert::IsTrue(false);
			}
		}

		TEST_METHOD(Test_NotError)
		{
			try
			{
				Lisp::Eval("(not a 2.0)");
				Assert::IsTrue(false);
			}
			catch (const CsLisp::LispException &)
			{
				Assert::IsTrue(true);
			}
			catch (...)
			{
				Assert::IsTrue(false);
			}
		}

		TEST_METHOD(Test_CompareError1)
		{
			try
			{
				Lisp::Eval("(> 2.0)");
				Assert::IsTrue(false);
			}
			catch (const CsLisp::LispException &)
			{
				Assert::IsTrue(true);
			}
			catch (...)
			{
				Assert::IsTrue(false);
			}
		}

		TEST_METHOD(Test_CompareError2)
		{
			try
			{
				Lisp::Eval("(> 2.0 5 234)");
				Assert::IsTrue(false);
			}
			catch (const CsLisp::LispException &)
			{
				Assert::IsTrue(true);
			}
			catch (...)
			{
				Assert::IsTrue(false);
			}
		}

		TEST_METHOD(Test_ScriptToLong)
		{
			try
			{
				Lisp::Eval("(setf a 2.0) asdf");
				Assert::IsTrue(false);
			}
			catch (const CsLisp::LispException &)
			{
				Assert::IsTrue(true);
			}
			catch (...)
			{
				Assert::IsTrue(false);
			}
		}

		TEST_METHOD(Test_DefError)
		{
			try
			{
				Lisp::Eval("(def 1 2)");
				Assert::IsTrue(false);
			}
			catch (const CsLisp::LispException &)
			{
				Assert::IsTrue(true);
			}
			catch (...)
			{
				Assert::IsTrue(false);
			}
		}

		TEST_METHOD(Test_DoError)
		{
			try
			{
				Lisp::Eval("(do (def a 2) blub (setf a 5))");
				Assert::IsTrue(false);
			}
			catch (const CsLisp::LispException &)
			{
				Assert::IsTrue(true);
			}
			catch (...)
			{
				Assert::IsTrue(false);
			}
		}

		TEST_METHOD(Test_IfError)
		{
			try
			{
				Lisp::Eval("(if #t 1 2 3)");
				Assert::IsTrue(false);
			}
			catch (const CsLisp::LispException &)
			{
				Assert::IsTrue(true);
			}
			catch (...)
			{
				Assert::IsTrue(false);
			}
		}

		TEST_METHOD(Test_FunctionNotFound)
		{
			try
			{
				Lisp::Eval("(unknown-fcn 1 2 3)");
				Assert::IsTrue(false);
			}
			catch (const CsLisp::LispException &)
			{
				Assert::IsTrue(true);
			}
			catch (...)
			{
				Assert::IsTrue(false);
			}
		}

		TEST_METHOD(Test_BracketsOutOfBalance1)
		{
			try
			{
				Lisp::Eval("(do (println 2)))");
				Assert::IsTrue(false);
			}
			catch (const CsLisp::LispException &)
			{
				Assert::IsTrue(true);
			}
			catch (...)
			{
				Assert::IsTrue(false);
			}
		}

		TEST_METHOD(Test_BracketsOutOfBalance2)
		{
			try
			{
				Lisp::Eval("(do ( (println 2))");
				Assert::IsTrue(false);
			}
			catch (const CsLisp::LispException &)
			{
				Assert::IsTrue(true);
			}
			catch (...)
			{
				Assert::IsTrue(false);
			}
		}

		TEST_METHOD(Test_UnexpectedToken1)
		{
			try
			{
				Lisp::Eval("blub (do (println 2))");
				Assert::IsTrue(false);
			}
			catch (const CsLisp::LispException &)
			{
				Assert::IsTrue(true);
			}
			catch (...)
			{
				Assert::IsTrue(false);
			}
		}

		TEST_METHOD(Test_UnexpectedTokenButIsBracketsOutOfBalance)
		{
			try
			{
				Lisp::Eval("(do (println 2)) asfd");
				Assert::IsTrue(false);
			}
			catch (const CsLisp::LispException &)
			{
				Assert::IsTrue(true);
			}
			catch (...)
			{
				Assert::IsTrue(false);
			}
		}

		TEST_METHOD(TestLispException)
		{
			try
			{
				Lisp::Eval("(println \"hello\"))");
				Assert::IsTrue(false);
			}
			catch (const CsLisp::LispException &)
			{
				Assert::IsTrue(true);
			}
			catch (...)
			{
				Assert::IsTrue(false);
			}
		}

		TEST_METHOD(Test_LispVariantToString)
		{
			LispVariant result(std::make_shared<object>("hello"));
			string s = result.ToString();
			Assert::AreEqual("hello", s.c_str());
		}

		TEST_METHOD(Test_LispScope1)
		{
			LispScope scope;
			var result = scope.GetPreviousToken(std::make_shared<LispToken>("a", 0, 0, 1));
			Assert::IsNull(result.get());
		}

		TEST_METHOD(Test_LispTokenToString)
		{
			LispToken token("(", 0, 1, 1);
			string s = token.ToString();
			Assert::AreEqual("(", s.c_str());
		}

		TEST_METHOD(Test_Type1)
		{
			std::shared_ptr<LispVariant> result = Lisp::Eval("(type 7)");
			Assert::IsTrue(result->IsInt());
			Assert::AreEqual(3, result->IntValue());
		}

		TEST_METHOD(Test_Type2)
		{
			std::shared_ptr<LispVariant> result = Lisp::Eval("(type #f)");
			Assert::IsTrue(result->IsInt());
			Assert::AreEqual(2, result->IntValue());
		}

		TEST_METHOD(Test_Type3)
		{
			std::shared_ptr<LispVariant> result = Lisp::Eval("(type '(2 3 1))");
			Assert::IsTrue(result->IsInt());
			Assert::AreEqual(6, result->IntValue());
		}

		TEST_METHOD(Test_Type4)
		{
			std::shared_ptr<LispVariant> result = Lisp::Eval("(type aSymbol)");
			Assert::IsTrue(result->IsInt());
			Assert::AreEqual(8, result->IntValue());
		}

		TEST_METHOD(Test_Type5)
		{
			std::shared_ptr<LispVariant> result = Lisp::Eval("(type \"a string\")");
			Assert::IsTrue(result->IsInt());
			Assert::AreEqual(5, result->IntValue());
		}

		TEST_METHOD(Test_TypeStr)
		{
			std::shared_ptr<LispVariant> result = Lisp::Eval("(typestr 1.23)");
			Assert::IsTrue(result->IsString());
			Assert::AreEqual("Double", result->Value->ToString().c_str());
		}

		TEST_METHOD(Test_Fuel)
		{
			std::shared_ptr<LispVariant> result = Lisp::Eval("(fuel)");
			Assert::IsTrue(result->IsString());
			Assert::IsTrue(result->StringValue().Contains("fuel version"));
		}

		TEST_METHOD(Test_Copyright)
		{
			std::shared_ptr<LispVariant> result = Lisp::Eval("(copyright)");
			Assert::IsTrue(result->IsString());
			Assert::IsTrue(result->StringValue().Contains("Copyright: MIT-License"));
		}

		TEST_METHOD(Test_Help)
		{
			std::shared_ptr<LispVariant> result = Lisp::Eval("(help)");
			Assert::IsTrue(result->IsString());
			Assert::IsTrue(result->StringValue().Contains("available functions:"));
		}
	};
}