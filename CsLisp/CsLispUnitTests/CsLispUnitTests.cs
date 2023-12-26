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

using System;
using System.Globalization;
using System.Collections.Generic;
using System.Linq;
using CsLisp;
using Microsoft.VisualStudio.TestTools.UnitTesting;

namespace LispUnitTests
{
    [TestClass]
    public class CsLispInterpreterTests
    {
        #region interperter tests

        [TestMethod]
        public void Test_Comments()
        {
            LispVariant result = Lisp.Eval("(do (println \"hello\") ; a comment\n(println; separate lists with comments\n\"world\"));comment in last line");
            Assert.AreEqual("world", result.ToString());
        }

        [TestMethod]
        public void Test_DoAndPrint()
        {
            LispVariant result = Lisp.Eval("(do (println \"hello\")\n (println \"world\"))");
            Assert.AreEqual("world", result.ToString());
        }

        [TestMethod]
        public void Test_PrintLnMultilines()
        {
            LispVariant result = Lisp.Eval("(do (println \"hello\nworld\"))");
            Assert.AreEqual("hello\nworld", result.ToString());
        }

        [TestMethod]
        //[ExpectedException(typeof(LispException))]
        public void Test_PrintLnUnknownSymbol()
        {
            LispVariant result = Lisp.Eval("(do (println blub))");
            Assert.AreEqual("blub", result.ToString());
        }

        [TestMethod]
        public void Test_PrintTrace()
        {
            LispVariant result = Lisp.Eval("(do (trace #t) (println \"hello world\") (println (+ 9 8)) (gettrace))");
            Assert.AreEqual("hello world17", result.ToString());
        }

        [TestMethod]
        public void Test_If1()
        {
            LispVariant result = Lisp.Eval("(if #t (+ 1 2) (- 3 5))");
            Assert.AreEqual(3, result.ToInt());
        }

        [TestMethod]
        public void Test_If2()
        {
            LispVariant result = Lisp.Eval("(if #f (* 1 0) (/ 6 3))");
            Assert.AreEqual(2, result.ToInt());
        }

        [TestMethod]
        public void Test_If3()
        {
            LispVariant result = Lisp.Eval("(if true 1 0)");
            Assert.AreEqual(1, result.ToInt());
        }

        [TestMethod]
        public void Test_If4()
        {
            LispVariant result = Lisp.Eval("(if false 1 0)");
            Assert.AreEqual(0, result.ToInt());
        }

        [TestMethod]
        public void Test_If5()
        {
            LispVariant result = Lisp.Eval("(if true 1)");
            Assert.AreEqual(1, result.ToInt());
            result = Lisp.Eval("(if false 1)");
            Assert.IsNull(result);

        }

        [TestMethod]
        public void Test_Setf1()
        {
            LispVariant result = Lisp.Eval("(do (def a 1) (def b 1) (setf (first (list 'a 'b 'c)) 9))");
            Assert.AreEqual(9, result.ToInt());
        }

        [TestMethod]
        public void Test_SetfWithNth()
        {
            LispVariant result = Lisp.Eval("(do (def l '(a b c d)) (setf (nth 2 l) 9) (print l))");
            Assert.AreEqual("(a b 9 d)", result.ToString());
        }

        [TestMethod]
        public void Test_SetfWithFirst()
        {
            LispVariant result = Lisp.Eval("(do (def l '(a b c d)) (setf (first l) 21) (print l))");
            Assert.AreEqual("(21 b c d)", result.ToString());
        }

        [TestMethod]
        public void Test_SetfWithLast()
        {
            LispVariant result = Lisp.Eval("(do (def l '(a b c d)) (setf (last l) \"xyz\") (print l))");
            Assert.AreEqual("(a b c \"xyz\")", result.ToString());
        }

        [TestMethod]
        [DeploymentItem(@"../../../../../Library/fuellib.fuel", "Library")]
        public void Test_SetfWithMacros()
        {
            LispVariant result = Lisp.Eval("(do (import fuellib) (defstruct point x y) (def p (make-point 12 17)) (setf (get-point-x p) 9) (println p))");
            Assert.IsTrue(result.IsString);
            Assert.AreEqual("(#point 9 17)", result.ToString());
        }

        [TestMethod]
        [DeploymentItem(@"../../../../../Library/fuellib.fuel", "Library")]
        public void Test_DefstructMacro()
        {
            LispVariant result = Lisp.Eval("(do (import fuellib) (defstruct point x y) (def p (make-point 12 17)))");
            Assert.IsTrue(result.IsList);
            Assert.AreEqual("(#point 12 17)", result.ToString());
        }

        [TestMethod]
        public void Test_QuoteList()
        {
            LispVariant result = Lisp.Eval("(do (def a 1) (def l '(a b c)))");
            Assert.AreEqual("(a b c)", result.ToString());
        }

        [TestMethod]
        public void Test_Def1()
        {
            LispVariant result = Lisp.Eval("(do (def a 1) (def b 1) (def (nth 2 (list 'a 'b 'c)) 9) (+ c 2))");
            Assert.AreEqual(11, result.ToInt());
        }

        [TestMethod]
        public void Test_DefWithNil()
        {
            LispVariant result = Lisp.Eval("(do (def a nil) (println a))");
            Assert.AreEqual(LispToken.Nil, result.ToString());
        }

        [TestMethod]
        public void Test_Fn()
        {
            LispVariant result = Lisp.Eval("(do (def f (fn (x) (+ x x 1))) (println (f 8)))");
            Assert.AreEqual(17, result.ToInt());
        }

        [TestMethod]
        public void Test_Gdef()
        {
            LispVariant result = Lisp.Eval("(do (defn f (x) (gdef z (+ x x))) (f 8) (println z))");
            Assert.AreEqual(16, result.ToInt());
        }

        [TestMethod]
        public void Test_Gdefn()
        {
            LispVariant result = Lisp.Eval("(do (defn f (x) (gdefn g (x) (+ x x))) (f 2) (g 8))");
            Assert.AreEqual(16, result.ToInt());
        }

        [TestMethod]
        public void Test_Eval1()
        {
            LispVariant result = Lisp.Eval("(eval (list 'def 'x 43))");
            Assert.AreEqual(43, result.ToInt());
        }

        [TestMethod]
        public void Test_Eval2()
        {
            LispVariant result = Lisp.Eval("(eval '(def x 456))");
            Assert.AreEqual(456, result.ToInt());
        }

        [TestMethod]
        public void Test_Eval3()
        {
            LispVariant result = Lisp.Eval("(eval #t)");
            Assert.AreEqual(true, result.ToBool());
            result = Lisp.Eval("(eval 42)");
            Assert.AreEqual(42, result.ToInt());
        }

        [TestMethod]
        public void Test_Eval4()
        {
            const string script = @"(do 
                                      (defn defstructfunc (name) 
                                        (do
                                          (def structsym (sym (+ ""#"" name)))
                                          (list 'defn (sym (+ ""make-"" name)) (cdr (args)) 
                                              `(list ,structsym ,@(cdr(args)))
                                          )
                                        )
                                      )

                                      (def f (defstructfunc point x y z))
                                      (eval f)
                                      (def p (make-point 1 2 3))                                      
                                    )";
            LispVariant result = Lisp.Eval(script);
            Assert.AreEqual(true, result.IsList);
            Assert.AreEqual("(#point 1 2 3)", result.ToString());
        }

        [TestMethod]
        public void Test_Eval5()
        {
            const string script = @"(do 
                                      (defn defsimplefunc () 
                                        (do
                                          (list 'defn 'simplefunc '(a b) 
                                              '(+ a b)
                                          )
                                        )
                                      )

                                      (def f (defsimplefunc))
                                      (eval f)
                                      (def p (simplefunc 1 2))                                      
                                    )";
            LispVariant result = Lisp.Eval(script);
            Assert.AreEqual(true, result.IsInt);
            Assert.AreEqual("3", result.ToString());
        }

        [TestMethod]
        public void Test_EvalStr()
        {
            LispVariant result = Lisp.Eval("(evalstr \"(def x 456)\")");
            Assert.AreEqual(456, result.ToInt());
        }

        [TestMethod]
        public void Test_Doc()
        {
            LispVariant result = Lisp.Eval("(doc 'if)");
            var s = result.ToString();
            Assert.IsTrue(s.Contains("-------------------------------------------------"));
            Assert.IsTrue(s.Contains("if  [special form]"));
            Assert.IsTrue(s.Contains("Syntax: (if cond then-block [else-block])"));
            Assert.IsTrue(s.Contains("The if statement."));
        }

        [TestMethod]
        public void Test_DocForDefn()
        {
            LispVariant result = Lisp.Eval("; this is a fcn documenataion\n(defn blub (x) (+ x 1))\n(doc 'blub)");
            var s = result.ToString();
            Assert.IsTrue(s.Contains("-------------------------------------------------"));
            Assert.IsTrue(s.Contains("blub"));
            Assert.IsTrue(s.Contains("Syntax: (blub x)"));
            Assert.IsTrue(s.Contains("; this is a fcn documenataion"));
        }

        [TestMethod]
        public void Test_NoAutoDocForDefn()
        {
            LispVariant result = Lisp.Eval("(defn blub (x y ) (+ x y 1))\n(doc 'blub)");
            var s = result.ToString();
            Assert.IsTrue(s.Contains("-------------------------------------------------"));
            Assert.IsTrue(s.Contains("blub"));
            Assert.IsTrue(s.Contains("Syntax: (blub x y)"));
        }

        [TestMethod]
        public void Test_TickCount()
        {
            LispVariant result = Lisp.Eval("(tickcount)");
            Assert.IsTrue(result.IsInt);
        }

        [TestMethod]
        public void Test_While1()
        {
            LispVariant result = Lisp.Eval("(do (def a 1) (def b 1) (while (< a 10) (do (setf a (+ a 1)) (setf b (+ b 1)))))");
            Assert.AreEqual(10, result.ToInt());
        }

        [TestMethod]
        public void Test_Defn1()
        {
            LispVariant result = Lisp.Eval("(do (def g_prn \"START:\") (defn prn (x) (setf g_prn (add g_prn x))) (prn \"34\") (println g_prn))");
            Assert.AreEqual("START:34", result.ToString());
        }

        [TestMethod]
        public void Test_AddString()
        {
            LispVariant result = Lisp.Eval("(+ \"abc\" \"def() ; blub\" \"xxx\")");
            Assert.AreEqual("abcdef() ; blubxxx", result.ToString());
        }

        [TestMethod]
        public void Test_AddLists()
        {
            LispVariant result = Lisp.Eval("(+ '(1 2 3) '(\"hello world\" 2.3 42))");
            Assert.AreEqual("(1 2 3 \"hello world\" 2.3 42)", result.ToString());
        }

        [TestMethod]
        public void Test_ListFirst()
        {
            LispVariant result = Lisp.Eval("(first '(1 2 3))");
            Assert.AreEqual(1, result.ToInt());
        }

        [TestMethod]
        public void Test_ListFirstSymbol()
        {
            LispVariant result = Lisp.Eval("(first '(a b c))");
            Assert.AreEqual("a", result.ToString());
        }

        [TestMethod]
        public void Test_ListLast()
        {
            LispVariant result = Lisp.Eval("(last '(1 2 3))");
            Assert.AreEqual(3, result.ToInt());
        }

        [TestMethod]
        public void Test_ListLastSymbol()
        {
            LispVariant result = Lisp.Eval("(last '(abc def xyz))");
            Assert.AreEqual("xyz", result.ToString());
        }

        [TestMethod]
        public void Test_ListCar()
        {
            LispVariant result = Lisp.Eval("(car '(\"abc\" 2 3))");
            Assert.AreEqual("abc", result.ToString());
        }

        [TestMethod]
        public void Test_ListRest()
        {
            LispVariant result = Lisp.Eval("(rest '(1 2 3))");
            Assert.AreEqual("(2 3)", result.ToString());
        }

        [TestMethod]
        public void Test_ListCdr()
        {
            LispVariant result = Lisp.Eval("(cdr '(\"nix\" 1 2 3))");
            Assert.AreEqual("(1 2 3)", result.ToString());
        }

        [TestMethod]
        public void Test_ListLength()
        {
            LispVariant result = Lisp.Eval("(len '(1 2 3))");
            Assert.AreEqual(3, result.ToInt());
        }

        [TestMethod]
        public void Test_ListAppend()
        {
            LispVariant result = Lisp.Eval("(append (list 4 54 3) (list 7 9))");
            Assert.AreEqual("(4 54 3 7 9)", result.ToString());
        }

        [TestMethod]
        public void Test_ListPush1()
        {
            LispVariant result = Lisp.Eval("(do (def l '(a b c)) (push 'z l))");
            Assert.AreEqual("(z a b c)", result.ToString());
        }

        [TestMethod]
        public void Test_ListPush2()
        {
            LispVariant result = Lisp.Eval("(do (def l '(a b c)) (push 'z l 2))");
            Assert.AreEqual("(a b z c)", result.ToString());
        }

        [TestMethod]
        public void Test_ListPush3()
        {
            LispVariant result = Lisp.Eval("(do (def l '(a b c)) (push 'z l 2) (print l))");
            Assert.AreEqual("(a b z c)", result.ToString());
        }

        [TestMethod]
        [ExpectedException(typeof(LispException))]
        public void Test_ListPushError()
        {
            LispVariant result = Lisp.Eval("(do (def l 42) (push z l 2))");
            Assert.Fail();
        }

        [TestMethod]
        public void Test_ListPop1()
        {
            LispVariant result = Lisp.Eval("(do (def l '(a b c)) (def a (pop l)) (print a l))");
            Assert.AreEqual("a (b c)", result.ToString());
        }

        [TestMethod]
        public void Test_ListPop2()
        {
            LispVariant result = Lisp.Eval("(do (def l '()) (def a (pop l)) (print a l))");
            Assert.AreEqual("NIL ()", result.ToString());
        }

        [TestMethod]
        public void Test_ListPop3()
        {
            LispVariant result = Lisp.Eval("(do (def l '()) (def a (pop l)) (print a l))");
            Assert.AreEqual("NIL ()", result.ToString());
        }

        [TestMethod]
        public void Test_ListPop4()
        {
            LispVariant result = Lisp.Eval("(do (def l '(a b c)) (def a (pop l 5)) (print a l))");
            Assert.AreEqual("NIL (a b c)", result.ToString());
        }

        [TestMethod]
        public void Test_ListPop5()
        {
            LispVariant result = Lisp.Eval("(do (def l '(a b c)) (def a (pop l 1)) (print a l))");
            Assert.AreEqual("b (a c)", result.ToString());
        }

        [TestMethod]
        [ExpectedException(typeof(LispException))]
        public void Test_ListPopError()
        {
            LispVariant result = Lisp.Eval("(do (def l 5) (def a (pop l)) (print a l))");
            Assert.Fail();
        }

        [TestMethod]
        public void Test_LogicalOperators()
        {
            LispVariant result = Lisp.Eval("(list (and #t #f) (and #t #t) (or #t #f) (or #f #f) (or #t #f #t))");
            Assert.AreEqual("(#f #t #t #f #t)", result.ToString());
        }

        [TestMethod]
        public void Test_LogicalOperators2()
        {
            LispVariant result = Lisp.Eval("(list (&& #t #f) (&& #t #t) (|| #t #f) (|| #f #f) (|| #t #f #t))");
            Assert.AreEqual("(#f #t #t #f #t)", result.ToString());
        }

        [TestMethod]
        public void Test_CompareOperators1()
        {
            LispVariant result = Lisp.Eval("(list (= 1 2) (= 4 4) (== \"blub\" \"blub\") (== #t #f) (equal 3 4))");
            Assert.AreEqual("(#f #t #t #f #f)", result.ToString());
            result = Lisp.Eval("(do (def a ()) (== a ()))");
            Assert.AreEqual(true, result.BoolValue);
            result = Lisp.Eval("(do (def a 42) (== a ()))");
            Assert.AreEqual(false, result.BoolValue);
            result = Lisp.Eval("(do (def a 'blub) (def b 'nix) (== a b))");
            Assert.AreEqual(false, result.BoolValue);
            result = Lisp.Eval("(do (def a 'blub) (def b 'blub) (== a b))");
            Assert.AreEqual(true, result.BoolValue);
            result = Lisp.Eval("(do (def a 'blub) (def b 'blub) (== a b))");
            Assert.AreEqual(true, result.BoolValue);
            result = Lisp.Eval("(do (def a (list 1 2 3)) (def b (list 2 3 4)) (== a b))");
            Assert.AreEqual(false, result.BoolValue);
            result = Lisp.Eval("(do (def a (list 1 2 3)) (def b (list 1 2 3)) (== a b))");
            Assert.AreEqual(true, result.BoolValue);
            result = Lisp.Eval("(do (def a (list 1 2 3)) (def b (list 1 (sym 2) 3)) (== a b))");
            Assert.AreEqual(false, result.BoolValue);
            result = Lisp.Eval("(do (def a 'blub) (def b 'nix) (!= a b))");
            Assert.AreEqual(true, result.BoolValue);
            result = Lisp.Eval("(do (def a 7) (def b 7) (!= a b))");
            Assert.AreEqual(false, result.BoolValue);
        }

        [TestMethod]
        public void Test_CompareOperators2()
        {
            LispVariant result = Lisp.Eval("(list (< 1 2) (< 4 1) (> 5 2) (> 1 3) (> 4.0 4.0))");
            Assert.AreEqual("(#t #f #t #f #f)", result.ToString());
            result = Lisp.Eval("(do (def a \"abc\") (def b \"def\") (< a b))");
            Assert.AreEqual(true, result.BoolValue);
            result = Lisp.Eval("(do (def a \"abc\") (def b \"abc\") (< a b))");
            Assert.AreEqual(false, result.BoolValue);
            result = Lisp.Eval("(do (def a \"abc\") (def b \"def\") (<= a b))");
            Assert.AreEqual(true, result.BoolValue);
            result = Lisp.Eval("(do (def a \"abc\") (def b \"abc\") (<= a b))");
            Assert.AreEqual(true, result.BoolValue);
        }

        [TestMethod]
        public void Test_CompareOperators3()
        {
            LispVariant result = Lisp.Eval("(list (<= 1 2) (<= 4 1) (>= 5 2) (>= 1 3) (>= 4.0 4.0) (<= 42 42))");
            Assert.AreEqual("(#t #f #t #f #t #t)", result.ToString());
        }

        [TestMethod]
        public void Test_Not()
        {
            LispVariant result = Lisp.Eval("(list (! #t) (not #t) (not #f) (! #f))");
            Assert.AreEqual("(#f #f #t #t)", result.ToString());
        }

        [TestMethod]
        public void Test_Arithmetric1()
        {
            LispVariant result = Lisp.Eval("(+ 1 2 3 4)");
            Assert.AreEqual(10, result.ToInt());
        }

        [TestMethod]
        public void Test_Arithmetric2()
        {
            LispVariant result = Lisp.Eval("(+ 1.1 2.2 3.3 4.3)");
            int res = (int)Math.Round(result.ToDouble() * 10.0);
            Assert.AreEqual(109, res);
        }

        [TestMethod]
        public void Test_Arithmetric3()
        {
            LispVariant result = Lisp.Eval("(* 3 8 2)");
            Assert.AreEqual(48, result.ToInt());
        }

        [TestMethod]
        public void Test_Arithmetric4()
        {
            LispVariant result = Lisp.Eval("(/ 1.0 2.0)");
            int res = (int)Math.Round(result.ToDouble() * 10.0);
            Assert.AreEqual(5, res);
        }

        [TestMethod]
        public void Test_Arithmetric5()
        {
            LispVariant result = Lisp.Eval("(/ 10 2)");
            Assert.AreEqual(5, result.ToInt());
        }

        [TestMethod]
        public void Test_Arithmetric6()
        {
            LispVariant result = Lisp.Eval("(- 42 12 6)");
            Assert.AreEqual(24, result.ToInt());
        }

        [TestMethod]
        public void Test_Arithmetric7()
        {
            LispVariant result = Lisp.Eval("(- 42.5 0.5)");
            int res = (int)Math.Round(result.ToDouble() * 10.0);
            Assert.AreEqual(420, res);
        }

        [TestMethod]
        public void Test_Arithmetric8()
        {
            LispVariant result = Lisp.Eval("(sub 42.5 1.5 2.0)");
            int res = (int)Math.Round(result.ToDouble() * 10.0);
            Assert.AreEqual(390, res);
        }

        [TestMethod]
        public void Test_Arithmetric9()
        {
            LispVariant result = Lisp.Eval("(div 12 3)");
            Assert.AreEqual(4, result.ToInt());
        }

        [TestMethod]
        public void Test_Arithmetric10()
        {
            LispVariant result = Lisp.Eval("(mul 2 3 4)");
            Assert.AreEqual(24, result.ToInt());
        }

        [TestMethod]
        public void Test_Arithmetric11()
        {
            LispVariant result = Lisp.Eval("(add 2 3 4)");
            Assert.AreEqual(9, result.ToInt());
        }

        [TestMethod]
        public void Test_MacrosEvaluateNested()
        {
            const string macroExpandScript = @"(do
  (define-macro-eval first-macro
        (a b) 
        (do 
           (println first-macro)
    	   (def i 1)
           (+ a b i)
        )
  )
  
  (define-macro-eval second-macro
        (x y) 
        (do 
           (println second-macro)
           (* x y (first-macro (+ x 1) (+ y 2)))
        )
  )
  
  (def m (second-macro 4 3))
)";

            using (ConsoleRedirector cr = new ConsoleRedirector())
            {
                LispVariant result = Lisp.Eval(macroExpandScript);
                Assert.AreEqual("132", result.ToString());

                string s = cr.ToString().Trim();
                Assert.IsTrue(s.Contains("first-macro"));
                Assert.IsTrue(s.Contains("second-macro"));
            }
        }

        [TestMethod]
        public void Test_MacrosEvaluateRecursive()
        {
            const string macroExpandScript = @"(do
  (define-macro-eval first-macro
        (a b) 
        (do 
           (println first-macro)
    	   (def i 1)
           (+ a b i)
        )
  )
  
  (define-macro-eval second-macro
        (x y) 
        (do 
           (println second-macro)
           (* x y (first-macro x (+ y 4)))
        )
  )
  
  (def m (second-macro 4 3))
)";

            using (ConsoleRedirector cr = new ConsoleRedirector())
            {
                LispVariant result = Lisp.Eval(macroExpandScript);
                Assert.AreEqual("144", result.ToString());

                string s = cr.ToString().Trim();
                Assert.IsTrue(s.Contains("first-macro"));
                Assert.IsTrue(s.Contains("second-macro"));
            }
        }

        [TestMethod]
        public void Test_MacrosEvaluateDoubleMacroCall()
        {
            const string macroExpandScript = @"(do
  (define-macro-eval first-macro
        (a b) 
        (do 
           (println first-macro)
    	   (def i 1)
           (+ a b i)
        )
  )
  
  (define-macro-eval second-macro
        (x y) 
        (do 
           (println second-macro)
           (* x y)
        )
  )
  
  (def m (second-macro 4 (first-macro 6 3)))
)";

            using (ConsoleRedirector cr = new ConsoleRedirector())
            {
                LispVariant result = Lisp.Eval(macroExpandScript);
                Assert.AreEqual("40", result.ToString());

                string s = cr.ToString().Trim();
                Assert.IsTrue(s.Contains("first-macro"));
                Assert.IsTrue(s.Contains("second-macro"));
            }
        }

        [TestMethod]
        public void Test_MacrosExpand1()
        {
            if (LispUtils.IsCompileTimeMacroEnabled)
            {
                LispVariant result = Lisp.Eval("(do (define-macro-expand blub (x y) '(println x y)) (println (quote (1 2 3))) (blub 3 4))");
                Assert.AreEqual("3 4", result.ToString());
            }
            else
            {
                Assert.IsTrue(true);
            }
        }

        [TestMethod]
        public void Test_MacrosExpand2()
        {
            const string macroExpandScript = @"(do
  (define-macro-expand first-macro
        (a b) 
        '(do 
    	   (def i 1)
           (+ a b i)
         )
  )
  
  (define-macro-expand second-macro
        (x y) 
        '(do 
           (* x y (first-macro x y))
         )
  )
  
  (def m (second-macro 4 3))
)";
            if (LispUtils.IsCompileTimeMacroEnabled)
            {
                LispVariant result = Lisp.Eval(macroExpandScript);
                Assert.AreEqual("96", result.ToString());
            }
            else
            {
                Assert.IsTrue(true);
            }
        }

        [TestMethod]
        public void Test_MacrosExpandDefineStruct()
        {
            const string macroExpandScript = @"(do
  (define-macro-eval dotimes (counterinfo statements)
      (do
        (def (first 'counterinfo) 0)
        (while (eval (list < (first 'counterinfo) (eval (nth 1 'counterinfo))))
          (do
             (eval 'statements)
             (setf (rval (first 'counterinfo)) (eval (list + (first 'counterinfo) 1)))
          )
        )
      )
  )

    (define-macro-eval defstruct (name)
    (do
        
      (eval
         (list 'defn (sym (+ ""make-"" name)) (cdr (quoted-macro-args)) 
                  `(list ,(sym (+ ""#"" name)) ,@(cdr(quoted-macro-args)))
         )
      )

	  (eval
         (list 'defn (sym (+ ""is-"" name ""-p"")) '(data)		 
            `(and(== (type data) 6) (== (first data) ,(sym (+ ""#"" name))))
         )
	  )

	  (dotimes (i (- (len (quoted-macro-args)) 1))
        (eval
              (list 'defn (sym (+ ""get-"" name ""-"" (str (nth (+ i 1) (quoted-macro-args))))) '(data)
			     `(nth (+ ,i 1) data)
            )
        )
      )
	)
  )
  
  (defstruct point x y)
  (def p (make-point 2 3))
)";
            if (LispUtils.IsCompileTimeMacroEnabled)
            {
                LispVariant result = Lisp.Eval(macroExpandScript);
                Assert.AreEqual("(#point 2 3)", result.ToString());
            }
            else
            {
                Assert.IsTrue(true);
            }
        }

        [TestMethod]
        public void Test_MacrosExpandNested()
        {
            const string macroExpandScript = @"(do
  (define-macro-expand first-macro
        (a b) 
        '(do 
           (println first-macro)
    	   (def i 1)
           (+ a b i)
         )
  )
  
  (define-macro-expand second-macro
        (x y) 
        '(do 
           (println second-macro)
           (* x y (first-macro (+ x 1) (+ y 2)))
         )
  )
  
  (def m (second-macro 4 3))
)";

            if (LispUtils.IsCompileTimeMacroEnabled)
            {
                using (ConsoleRedirector cr = new ConsoleRedirector())
                {
                    LispVariant result = Lisp.Eval(macroExpandScript);
                    Assert.AreEqual("132", result.ToString());

                    string s = cr.ToString().Trim();
                    Assert.IsTrue(s.Contains("first-macro"));
                    Assert.IsTrue(s.Contains("second-macro"));
                }
            }
            else
            {
                Assert.IsTrue(true);
            }
        }

        [TestMethod]
        public void Test_MacrosExpandRecursive()
        {
            const string macroExpandScript = @"(do
  (define-macro-expand first-macro
        (a b) 
        '(do 
           (println first-macro)
    	   (def i 1)
           (+ a b i)
         )
  )
  
  (define-macro-expand second-macro
        (x y) 
        '(do 
           (println second-macro)
           (* x y (first-macro x (+ y 4)))
         )
  )
  
  (def m (second-macro 4 3))
)";

            if (LispUtils.IsCompileTimeMacroEnabled)
            {
                using (ConsoleRedirector cr = new ConsoleRedirector())
                {
                    LispVariant result = Lisp.Eval(macroExpandScript);
                    Assert.AreEqual("144", result.ToString());

                    string s = cr.ToString().Trim();
                    Assert.IsTrue(s.Contains("first-macro"));
                    Assert.IsTrue(s.Contains("second-macro"));
                }
            }
            else
            {
                Assert.IsTrue(true);
            }
        }

        [TestMethod]
        public void Test_MacrosExpandDoubleMacroCall()
        {
            const string macroExpandScript = @"(do
  (define-macro-expand first-macro
        (a b) 
        '(do 
           (println first-macro)
    	   (def i 1)
           (+ a b i)
         )
  )
  
  (define-macro-expand second-macro
        (x y) 
        '(do 
           (println second-macro)
           (* x y)
         )
  )
  
  (def m (second-macro 4 (first-macro 6 3)))
)";

            if (LispUtils.IsCompileTimeMacroEnabled)
            {
                using (ConsoleRedirector cr = new ConsoleRedirector())
                {
                    LispVariant result = Lisp.Eval(macroExpandScript);
                    Assert.AreEqual("40", result.ToString());

                    string s = cr.ToString().Trim();
                    Assert.IsTrue(s.Contains("first-macro"));
                    Assert.IsTrue(s.Contains("second-macro"));
                }
            }
            else
            {
                Assert.IsTrue(true);
            }
        }

        [TestMethod]
        public void Test_MacrosSetf1()
        {
            if (LispUtils.IsCompileTimeMacroEnabled)
            {
                LispVariant result =
                    Lisp.Eval(
                        "(do (def a 42) (define-macro-expand my-setf (x value) '(setf x value)) (my-setf a (+ \"blub\" \"xyz\")) (println a))");
                Assert.AreEqual("blubxyz", result.ToString());
            }
            else
            {
                Assert.IsTrue(true);
            }
        }

        [TestMethod]
        public void Test_MacrosSetf2()
        {
            LispVariant result = Lisp.Eval("(do (def a 42) (defn my-setf (x value) (setf x value)) (my-setf a (+ 8 9)) (println a))");
            Assert.AreEqual(42, result.ToInt());
        }

        [TestMethod]
        public void Test_MacrosSetf3()
        {
            LispVariant result = Lisp.Eval("(do (def a 42) (define-macro-eval my-setf (x value) (setf x value)) (my-setf a (+ \"blub\" \"xyz\")) (println a))");
            Assert.AreEqual("blubxyz", result.ToString());
        }

        [TestMethod]
        public void Test_Quasiquote1()
        {
            LispVariant result = Lisp.Eval("(do (def a '(42 99 102 \"hello\")) (def b 55) (println (type a)) (println (nth 3 `(1 2 3 ,@a))))");
            Assert.AreEqual("42", result.ToString());
        }

        [TestMethod]
        public void Test_Quasiquote2()
        {
            LispVariant result = Lisp.Eval("(do (def a 42) (println `(1 2 3 ,a)))");
            Assert.AreEqual("(1 2 3 42)", result.ToString());
        }

        [TestMethod]
        public void Test_Quasiquote3()
        {
            LispVariant result = Lisp.Eval("(do (def a 42) (def lst (list 6 8 12)) (println (quasiquote (1 2 3 ,a ,@lst))))");
            Assert.AreEqual("(1 2 3 42 6 8 12)", result.ToString());
        }

        [TestMethod]
        public void Test_Quasiquote4()
        {
            LispVariant result = Lisp.Eval("(do (def a 42) (def lst (list 6 8 12)) (println (quasiquote (1 2 3 ,a ,lst))))");
            Assert.AreEqual("(1 2 3 42 (6 8 12))", result.ToString());
        }

        [TestMethod]
        public void Test_Quasiquote5()
        {
            LispVariant result = Lisp.Eval("(do (def a 42) (println (quasiquote (1 2 3 ,(+ 3 a)))))");
            Assert.AreEqual("(1 2 3 45)", result.ToString());
        }

        [TestMethod]
        public void Test_Quasiquote6()
        {
            LispVariant result = Lisp.Eval("(do (def a 42) (println (quasiquote (1 2 3 ,@(list 9 8 7 a)))))");
            Assert.AreEqual("(1 2 3 9 8 7 42)", result.ToString());
        }

        [TestMethod]
        public void Test_Quasiquote7()
        {
            LispVariant result = Lisp.Eval("(do (def args '(1 2 3)) `,(first args))");
            Assert.AreEqual("1", result.ToString());
        }

        [TestMethod]
        public void Test_Quasiquote8()
        {
            LispVariant result = Lisp.Eval("(do (def args '(1 2 3)) `(,(first args)))");
            Assert.AreEqual("(1)", result.ToString());
        }

        [TestMethod]
        public void Test_Quasiquote9()
        {
            LispVariant result = Lisp.Eval("(do `(b))");
            Assert.AreEqual("(b)", result.ToString());
        }

        [TestMethod]
        public void Test_Quasiquote10()
        {
            LispVariant result = Lisp.Eval("(do `a)");
            Assert.AreEqual("a", result.ToString());
        }

        [TestMethod]
        public void Test_Quote1()
        {
            LispVariant result = Lisp.Eval("(do (def x 42) (println 'x))");
            Assert.AreEqual("x", result.ToString());
        }

        [TestMethod]
        public void Test_Quote2()
        {
            LispVariant result = Lisp.Eval("(do 'x)");
            Assert.AreEqual("x", result.ToString());
        }

        [TestMethod]
        public void Test_Quote3()
        {
            LispVariant result = Lisp.Eval("(do '(a b 6 x))");
            Assert.AreEqual("(a b 6 x)", result.ToString());
        }

        [TestMethod]
        public void Test_EvalQuasiQuote1()
        {
            LispVariant result = Lisp.Eval("(do (def a 4) (def lst '(a 2 3)) (eval `,(first lst)))");
            Assert.AreEqual("4", result.ToString());
        }

        [TestMethod]
        public void Test_String1()
        {
            LispVariant result = Lisp.Eval("(println \"hello \\\\ \\' öäü \n \\\"blub\\\"\")");
            Assert.AreEqual("hello \\ ' öäü \n \"blub\"", result.ToString());
        }

        [TestMethod]
        public void Test_String2()
        {
            LispVariant result = Lisp.Eval("(string \"hello\" \"-\" \"world\")");
            Assert.AreEqual("hello-world", result.ToString());
        }

        [TestMethod]
        public void Test_Map()
        {
            LispVariant result = Lisp.Eval("(map (lambda (x) (+ x 1)) '(1 2 3))");
            Assert.AreEqual("(2 3 4)", result.ToString());
        }

        [TestMethod]
        [ExpectedException(typeof(LispException))]
        public void Test_MapError1()
        {
            Lisp.Eval("(map 4 '(1 2 3))");
        }

        [TestMethod]
        [ExpectedException(typeof(LispException))]
        public void Test_MapError2()
        {
            Lisp.Eval("(map (lambda (x) (+ x 1)) 4)");
        }

        [TestMethod]
        [ExpectedException(typeof(LispException))]
        public void Test_ReduceError1()
        {
            Lisp.Eval("(reduce \"blub\" '(1 2 3) 0)");
        }

        [TestMethod]
        [ExpectedException(typeof(LispException))]
        public void Test_ReduceError2()
        {
            Lisp.Eval("(reduce (lambda (x y) (+ x y))  \"test\" 0)");
        }

        [TestMethod]
        public void Test_Reduce1()
        {
            LispVariant result = Lisp.Eval("(reduce (lambda (x y) (+ x y)) '(1 2 3) 0)");
            Assert.AreEqual(6, result.ToInt());
        }

        [TestMethod]
        public void Test_Reduce2()
        {
            LispVariant result = Lisp.Eval("(reduce (lambda (x y) (* x y)) '(2 3 4 5) 2)");
            Assert.AreEqual(240, result.ToInt());
        }

        [TestMethod]
        public void Test_Closure1()
        {
            LispVariant result = Lisp.Eval("(do (defn addx (delta) (lambda (x) (+ x delta))) (def addclosure (addx 41)) (println (addclosure 1)))");
            Assert.AreEqual(42, result.ToInt());
        }

        [TestMethod]
        public void Test_Closure2()
        {
            LispVariant result = Lisp.Eval("(do (defn g (x) (do (+ x 2))) (defn f (x) (do (def i 7) (+ x 1 i (g 2)))) (println (f 1)))");
            Assert.AreEqual(13, result.ToInt());
        }

        [TestMethod]
        [ExpectedException(typeof(LispException))]
        public void Test_Closure3()
        {
            Lisp.Eval("(do (defn g (x) (do (+ x 2 i))) (defn f (x) (do (def i 7) (+ x 1 i (g 2)))) (println (f 1)))");
        }

        [TestMethod]
        public void Test_Closure4()
        {
// TODO working...
            LispVariant result = Lisp.Eval("(do (defn g (x) (do (+ x 2))) (defn f (x) (do (def i 7) (+ x 1 i (g 2)))) (println (f 1)))");
            Assert.AreEqual(13, result.ToInt());
        }

        [TestMethod]
        public void Test_RecursiveCall1()
        {
            LispVariant result = Lisp.Eval("(do (defn addConst (x a) (+ x a)) (def add2 (lambda (x) (addConst x 2))) (println (addConst 8 2)) (println (add2 4)))");
            Assert.AreEqual(6, result.ToInt());
        }

        [TestMethod]
        public void Test_Return1()
        {
            LispVariant result = Lisp.Eval("(do (defn f (x) (return (+ x x))) (println (f 7)))");
            Assert.AreEqual(14, result.ToInt());
        }

        [TestMethod]
        public void Test_Return2()
        {
            // return statement was not implmented correctly until 25.7.2018...
            LispVariant result = Lisp.Eval("(do (defn f (x) (do (return (+ x x)) (return 9))) (println (f 7)))");
            Assert.AreEqual(14, result.ToInt());
        }

        [TestMethod]
        public void Test_Call1()
        {
            LispVariant result = Lisp.Eval("(do (def obj 0) (setf obj (call \"CsLisp.DummyNative\")) (println obj (type obj)) (call obj \"Test\"))");
            Assert.AreEqual(42, result.ToInt());
        }

        [TestMethod]
        public void Test_CallStatic()
        {
            LispVariant result = Lisp.Eval("(do (call-static \"System.IO.File\" Exists \"dummy\"))");
            Assert.AreEqual(false, result.ToBool());
        }

        [TestMethod]
        [ExpectedException(typeof(LispException))]
        public void Test_CallStaticError()
        {
            Lisp.Eval("(do (call-static \"System.IO.File\" NotExistingFunction \"dummy\"))");
        }

        [TestMethod]
        public void Test_Apply1()
        {
            LispVariant result = Lisp.Eval("(apply (lambda (x) (println \"hello\" x)) '(55))");
            Assert.AreEqual("hello 55", result.ToString());
        }

        [TestMethod]
        public void Test_Apply2()
        {
            LispVariant result = Lisp.Eval("(do (def f (lambda (x) (+ x x))) (apply f '(5)))");
            Assert.AreEqual(10, result.ToInt());
        }

        [TestMethod]
        public void Test_Apply3()
        {
            LispVariant result = Lisp.Eval("(do (def f '+) (apply f '(5 6 7)))");
            Assert.AreEqual(18, result.ToInt());
        }

        [TestMethod]
        public void Test_Argscount1()
        {
            LispVariant result = Lisp.Eval("(do (defn f (x) (do (println \"count=\" (argscount)) (+ x x))) (f 5 6 7))");
            Assert.AreEqual(10, result.ToInt());
        }

        [TestMethod]
        public void Test_Arg2()
        {
            using (ConsoleRedirector cr = new ConsoleRedirector())
            {
                LispVariant result = Lisp.Eval("(do (defn f (x) (do (println \"count=\" (argscount)) (println (arg 0)) (println (arg 1)) (println (arg 2)) (println \"additional=\" (nth 1 _additionalArgs)) (+ x x))) (f 5 6 7))");
                Assert.AreEqual(10, result.ToInt());

                string s = cr.ToString().Trim();
                Assert.AreEqual(true, s.Contains("count= 3"));
                Assert.AreEqual(true, s.Contains("5"));
                Assert.AreEqual(true, s.Contains("6"));
                Assert.AreEqual(true, s.Contains("7"));
                Assert.AreEqual(true, s.Contains("additional= 7"));
            }
        }

        [TestMethod]
        [ExpectedException(typeof(LispException))]
        public void Test_Arg3()
        {
            LispVariant result = Lisp.Eval("(do (defn f (x) (do (arg 7) (+ x x))) (f 5 6 7))");
            Assert.AreEqual(10, result.ToInt());
        }

        [TestMethod]
        public void Test_Arg4()
        {
            LispVariant result = Lisp.Eval("(do (defn f () (do (def z (arg 2)) (+ z z))) (f 5 6 7))");
            Assert.AreEqual(14, result.ToInt());
        }

        [TestMethod]
        public void Test_Args1()
        {
            LispVariant result = Lisp.Eval("(do (defn f () (do (def z (args)))) (f 5 6 7))");
            Assert.AreEqual("(5 6 7)", result.ToString());
        }

        [TestMethod]
        public void Test_Cons1()
        {
            LispVariant result = Lisp.Eval("(cons 1 2)");
            Assert.AreEqual("(1 2)", result.ToString());
        }

        [TestMethod]
        public void Test_Cons2()
        {
            LispVariant result = Lisp.Eval("(cons 1 '(2 3 4))");
            Assert.AreEqual("(1 2 3 4)", result.ToString());
        }

        [TestMethod]
        public void Test_Cons3()
        {
            LispVariant result = Lisp.Eval("(cons 12)");
            Assert.AreEqual("(12)", result.ToString());
        }

        [TestMethod]
        public void Test_Cons4()
        {
            LispVariant result = Lisp.Eval("(cons)");
            Assert.AreEqual("()", result.ToString());
        }

        [TestMethod]
        public void Test_Nop()
        {
            LispVariant result = Lisp.Eval("(nop)");
            Assert.IsTrue(result.IsUndefined);
        }

        [TestMethod]
        public void Test_Symbol()
        {
            LispVariant result = Lisp.Eval("(sym 'a)");
            Assert.IsTrue(result.IsSymbol);
            Assert.AreEqual("a", result.StringValue);
        }

        [TestMethod]
        public void Test_Str()
        {
            LispVariant result = Lisp.Eval("(str 'abc)");
            Assert.IsTrue(result.IsString);
            Assert.AreEqual("abc", result.StringValue);
        }

        #endregion

        #region expected errors

        [TestMethod]
        [ExpectedException(typeof(LispException))]
        public void Test_Parser1()
        {
            Lisp.Eval("(println \"hello\"))");
        }

        [TestMethod]
        [ExpectedException(typeof(LispException))]
        public void Test_Parser2()
        {
            Lisp.Eval("((println \"hello\")");
        }

        [TestMethod]
        [ExpectedException(typeof(LispException))]
        public void Test_Parser3()
        {
            Lisp.Eval("(blub 1 2 3)");
        }

        [TestMethod]
        [ExpectedException(typeof(LispException))]
        public void Test_SetfError()
        {
            Lisp.Eval("(setf a 2.0)");
        }

        [TestMethod]
        [ExpectedException(typeof(LispException))]
        public void Test_NotError()
        {
            Lisp.Eval("(not a 2.0)");
        }

        [TestMethod]
        [ExpectedException(typeof(LispException))]
        public void Test_CompareError1()
        {
            Lisp.Eval("(> 2.0)");
        }

        [TestMethod]
        [ExpectedException(typeof(LispException))]
        public void Test_CompareError2()
        {
            Lisp.Eval("(> 2.0 5 234)");
        }

        [TestMethod]
        [ExpectedException(typeof(LispException))]
        public void Test_ScriptToLong()
        {
            Lisp.Eval("(setf a 2.0) asdf");
        }

        [TestMethod]
        [ExpectedException(typeof(LispException))]
        public void Test_DefError()
        {
            Lisp.Eval("(def 1 2)");
        }

        [TestMethod]
        [ExpectedException(typeof(LispException))]
        public void Test_DoError()
        {
            Lisp.Eval("(do (def a 2) blub (setf a 5))");
        }

        [TestMethod]
        [ExpectedException(typeof(LispException))]
        public void Test_IfError()
        {
            Lisp.Eval("(if #t 1 2 3)");
        }

        [TestMethod]
        [ExpectedException(typeof(LispException))]
        public void Test_FunctionNotFound()
        {
            Lisp.Eval("(unknown-fcn 1 2 3)");
        }

        [TestMethod]
        [ExpectedException(typeof(LispException))]
        public void Test_BracketsOutOfBalance1()
        {
            Lisp.Eval("(do (println 2)))");
        }

        [TestMethod]
        [ExpectedException(typeof(LispException))]
        public void Test_BracketsOutOfBalance2()
        {
            Lisp.Eval("(do ( (println 2))");
        }

        [TestMethod]
        [ExpectedException(typeof(LispException))]
        public void Test_UnexpectedToken1()
        {
            Lisp.Eval("blub (do (println 2))");
        }

        [TestMethod]
        [ExpectedException(typeof(LispException))]
        public void Test_UnexpectedTokenButIsBracketsOutOfBalance()
        {
            Lisp.Eval("(do (println 2)) asfd");
        }

        #endregion

        #region interpreter internals

        [TestMethod]
        public void Test_LispVariantToString()
        {
            var result = new LispVariant("hello");
            string s = result.ToString();
            Assert.AreEqual("hello", s);
        }

        [TestMethod]
        public void Test_LispScope1()
        {
            var scope = new LispScope();
            var result = scope.GetPreviousToken(new LispToken("a", 0, 0, 1));
            Assert.AreEqual(null, result);
        }

        [TestMethod]
        public void Test_LispTokenToString()
        {
            var token = new LispToken("(", 0, 1, 1);
            string s = token.ToString();
            Assert.AreEqual("(", s);
        }

        #endregion

        #region infrastructure and debugging

        [TestMethod]
        public void Test_Type1()
        {
            LispVariant result = Lisp.Eval("(type 7)");
            Assert.IsTrue(result.IsInt);
            Assert.AreEqual(3, result.IntValue);
        }

        [TestMethod]
        public void Test_Type2()
        {
            LispVariant result = Lisp.Eval("(type #f)");
            Assert.IsTrue(result.IsInt);
            Assert.AreEqual(2, result.IntValue);
        }

        [TestMethod]
        public void Test_Type3()
        {
            LispVariant result = Lisp.Eval("(type '(2 3 1))");
            Assert.IsTrue(result.IsInt);
            Assert.AreEqual(6, result.IntValue);
        }

        [TestMethod]
        public void Test_Type4()
        {
            LispVariant result = Lisp.Eval("(type 'aSymbol)");
            Assert.IsTrue(result.IsInt);
            Assert.AreEqual(8, result.IntValue);
        }

        [TestMethod]
        public void Test_Type5()
        {
            LispVariant result = Lisp.Eval("(type \"a string\")");
            Assert.IsTrue(result.IsInt);
            Assert.AreEqual(5, result.IntValue);
        }

        [TestMethod]
        public void Test_TypeStr()
        {
            LispVariant result = Lisp.Eval("(typestr 1.23)");
            Assert.IsTrue(result.IsString);
            Assert.AreEqual("Double", result.Value.ToString());
        }

        [TestMethod]
        public void Test_Vars()
        {
            using (ConsoleRedirector cr = new ConsoleRedirector())
            {
                LispVariant result = Lisp.Eval("(do (def a 4) (def b \"asdf\") (vars))");
                Assert.IsTrue(result.IsUndefined);

                string s = cr.ToString().Trim();
                Assert.AreEqual(true, s.Contains("a --> 4"));
                Assert.AreEqual(true, s.Contains("b --> \"asdf\""));
            }
        }

        [TestMethod]
        public void Test_Fuel()
        {
            LispVariant result = Lisp.Eval("(fuel)");
            Assert.IsTrue(result.IsString);
            Assert.IsTrue(result.StringValue.Contains("fuel version"));
        }

        [TestMethod]
        public void Test_Copyright()
        {
            LispVariant result = Lisp.Eval("(copyright)");
            Assert.IsTrue(result.IsString);
            Assert.IsTrue(result.StringValue.Contains("Copyright: MIT-License"));
        }

        [TestMethod]
        public void Test_Help()
        {
            LispVariant result = Lisp.Eval("(help)");
            Assert.IsTrue(result.IsString);
            Assert.IsTrue(result.StringValue.Contains("available functions:"));
        }

        [TestMethod]
        [DeploymentItem(@"../../../../../Library/fuellib.fuel", "Library")]
        public void Test_Import()
        {
            using (ConsoleRedirector cr = new ConsoleRedirector())
            {
                LispVariant result = Lisp.Eval("(do (import \"Library/fuellib.fuel\") (foreach '(1 5 7) (lambda (x) (println x))))");
                Assert.IsTrue(result.IsInt);
                Assert.AreEqual(3, result.IntValue);

                string s = cr.ToString().Trim();
                Assert.IsTrue(s.Contains("1"));
                Assert.IsTrue(s.Contains("5"));
                Assert.IsTrue(s.Contains("7"));
            }
        }

        [TestMethod]
        [DeploymentItem(@"../../../../../Library/fuellib.fuel", "Library")]
        public void Test_Import2()
        {
            using (ConsoleRedirector cr = new ConsoleRedirector())
            {
                LispVariant result = Lisp.Eval("(do (import fuellib) (foreach '(1 4 6) (lambda (x) (println x))))");
                Assert.IsTrue(result.IsInt);
                Assert.AreEqual(3, result.IntValue);    // is last value of internal loop variable in foreach

                // test results
                string s = cr.ToString().Trim();
                Assert.IsTrue(s.Contains("1"));
                Assert.IsTrue(s.Contains("4"));
                Assert.IsTrue(s.Contains("6"));
            }
        }

        [TestMethod]
        public void Test_BadForeach()
        {
            LispVariant result = Lisp.Eval("(do (import fuellib) (foreach))");
            Assert.IsTrue(result.IsUndefined);
        }

        [TestMethod]
        [DeploymentItem(@"../../../../../Library/fuellib.fuel", "Library")]
        public void Test_CreateNative()
        {
            using (ConsoleRedirector cr = new ConsoleRedirector())
            {
                LispVariant result = Lisp.Eval("(do (import fuellib) (create-native \"List\" \"System.Collections.Generic.List`1[[System.Int32]]\") (def obj (create-List)) (List-Add obj 7) (call obj \"Add\" 5) (println (List-get_Count obj)))");
                Assert.IsTrue(result.IsString);
                Assert.AreEqual("2", result.Value.ToString());

                string s = cr.ToString().Trim();
                Assert.IsTrue(s.Contains("2"));
            }
        }
        
        [TestMethod]
        public void Test_RegisterNativeObjects()
        {
            using (ConsoleRedirector cr = new ConsoleRedirector())
            {
                var model = new DummyNative("fuel");

                var nativeItems = new Dictionary<string, object>();
                nativeItems["model"] = model;

                const string script =
                    "(println model)" +
                    "(println (call model GetMessage \"hello\")) " +      // call a method
                    "(println (call model get_MyValue))" +                // access property --> get
                    "(println (call model set_MyValue \"test\"))" +       // access property --> set
                    "(println (call model GetMessage \"hello\")) ";       // call a method

                LispVariant result = Lisp.Eval(script, nativeItems: nativeItems);
                
                Assert.IsTrue(result.IsString);
                Assert.AreEqual("hello test", result.Value.ToString());

                string s = cr.ToString().Trim();
                Assert.IsTrue(s.Contains("hello fuel"));
                Assert.IsTrue(s.Contains("<undefined>"));
                Assert.IsTrue(s.Contains("hello test"));
            }
        }

        [TestMethod]
        public void Test_Break()
        {
            using (ConsoleRedirector cr = new ConsoleRedirector())
            {
                LispVariant result = Lisp.Eval("(break)");
                Assert.IsTrue(result.IsUndefined);

                string s = cr.ToString().Trim();
                Assert.IsTrue(s.Contains("no debugger support"));
            }            
        }

        #endregion

        #region test fuel standard lib

        [TestMethod]
        [DeploymentItem(@"../../../../../Library/fuellib.fuel", "Library")]
        public void Test_StdLibList()
        {
            using (ConsoleRedirector cr = new ConsoleRedirector())
            {
                LispVariant result = Lisp.Eval("(do (import fuellib) (def lst (create-List)) (List-Add lst \"blub\") (List-Add lst 1.2354) (println \"count=\" (List-get_Count lst)) (map println lst) )");
                Assert.IsTrue(result.IsList);
                Assert.AreEqual(2, ((IEnumerable<object>)result.Value).Count());

                string s = cr.ToString().Trim();
                Assert.IsTrue(s.Contains("count= 2"));
                Assert.IsTrue(s.Contains("blub"));
                Assert.IsTrue(s.Contains("1.2354"));               
            }
        }

        [TestMethod]
        [DeploymentItem(@"../../../../../Library/fuellib.fuel", "Library")]
        public void Test_StdLibListSort()
        {
            using (ConsoleRedirector cr = new ConsoleRedirector())
            {
                LispVariant result = Lisp.Eval("(do (import fuellib) (def lst (create-List)) (List-Add lst 3) (List-Add lst 7) (List-Add lst 4) (println \"count=\" (List-get_Count lst)) (List-Sort lst) (println (str lst)) (return lst))");
                Assert.IsTrue(result.IsNativeObject);
                Assert.AreEqual(3, ((IEnumerable<object>)result.Value).Count());

                string s = cr.ToString().Trim();
                Assert.IsTrue(s.Contains("count= 3"));
                Assert.IsTrue(s.Contains("(3 4 7)"));
            }
        }

        [TestMethod]
        [DeploymentItem(@"../../../../../Library/fuellib.fuel", "Library")]
        public void Test_StdLibFile()
        {
            using (ConsoleRedirector cr = new ConsoleRedirector())
            {
                LispVariant result = Lisp.Eval("(do (import fuellib) (println (File-Exists \"asdf.test\")) (File-Exists \"asdf.test\"))");
                Assert.IsTrue(result.IsBool);
                
                string s = cr.ToString().Trim();
                Assert.IsTrue(s.Contains("#f"));
            }
        }

        [TestMethod]
        [DeploymentItem(@"../../../../../Library/fuellib.fuel", "Library")]
        public void Test_StdLibMath()
        {
            using (ConsoleRedirector cr = new ConsoleRedirector())
            {
                LispVariant result = Lisp.Eval("(do (import fuellib) (println Math-PI) (println Math-E) (println (Math-Sin 1.234)) (Math-Cos 0.0) )");
                Assert.IsTrue(result.IsDouble);
                Assert.AreEqual(1.0, result.DoubleValue);

                string s = cr.ToString().Trim();
                Assert.IsTrue(s.Contains("0.94381820937463"));
                Assert.IsTrue(s.Contains("3.14159265358979"));
                Assert.IsTrue(s.Contains("2.71828182845904"));
            }
        }

        [TestMethod]
        [DeploymentItem(@"../../../../../Library/fuellib.fuel", "Library")]
        public void Test_StdLibMath2()
        {
            using (ConsoleRedirector cr = new ConsoleRedirector())
            {
                LispVariant result = Lisp.Eval("(import fuellib) (def val (Math-Abs -3000.5)) (println val) (return val)");
                Assert.IsTrue(result.IsDouble);
                Assert.AreEqual(3000.5, result.DoubleValue);

                string s = cr.ToString().Trim();
                Assert.IsTrue(s.Contains("3000.5"));
            }
        }

        [TestMethod]
        [DeploymentItem(@"../../../../../Library/fuellib.fuel", "Library")]
        public void Test_StdLibArray()
        {
            using (ConsoleRedirector cr = new ConsoleRedirector())
            {
                LispVariant result = Lisp.Eval("(do (import fuellib) (def arr (create-Array 10)) (Array-Set arr 0 7) (println (Array-Get arr 0)) (println \"len=\" (Array-get_Length arr)) (return arr))");
                Assert.IsTrue(result.IsNativeObject);

                string s = cr.ToString().Trim();
                Assert.IsTrue(s.Contains("7"));
                Assert.IsTrue(s.Contains("len= 10"));
            }
        }

        [TestMethod]
        [DeploymentItem(@"../../../../../Library/fuellib.fuel", "Library")]
        public void Test_StdLibDictionary()
        {
            using (ConsoleRedirector cr = new ConsoleRedirector())
            {
                LispVariant result = Lisp.Eval("(do (import fuellib) (def dict (create-Dict)) (println (Dict-get_Count dict)) (Dict-get_Count dict))");
                Assert.IsTrue(result.IsInt);
                Assert.AreEqual(0, result.IntValue);

                string s = cr.ToString().Trim();
                Assert.IsTrue(s.Contains("0"));
            }
        }

        #endregion

        [TestMethod]
        public void Test_ReadLine()
        {
            using (ConsoleRedirector cr = new ConsoleRedirector("some input\nmore input\n"))
            {
                LispVariant result = Lisp.Eval("(do (def a (readline)) (println a) (def b (readline)) (println b))");
                Assert.IsTrue(result.IsString);
                Assert.AreEqual("more input", result.ToString());

                string s = cr.ToString().Trim();
                Assert.IsTrue(s.Contains("some input"));
                Assert.IsTrue(s.Contains("more input"));
            }
        }

        [TestMethod]
        [ExpectedException(typeof(LispException))]
        public void Test_ReadLineWithArgs()
        {
            using (ConsoleRedirector cr = new ConsoleRedirector("some input\nmore input\n"))
            {
                LispVariant result = Lisp.Eval("(do (def a (readline 1)) (println a)");
                Assert.IsTrue(false);
            }
        }

        [TestMethod]
        public void Test_ParseInteger()
        {
            LispVariant result = Lisp.Eval("(do (def s \"42\") (def i (parse-integer s)))");
            Assert.IsTrue(result.IsInt);
            Assert.AreEqual(42, result.IntValue);
        }

        [TestMethod]
        public void Test_ParseIntegerError()
        {
            LispVariant result = Lisp.Eval("(do (def s \"nonumber\") (def i (parse-integer s)))");
            Assert.IsTrue(result.IsUndefined);
        }

        [TestMethod]
        public void Test_ParseFloat()
        {
            LispVariant result = Lisp.Eval("(do (def s \"42.789\") (def f (parse-float s)))");
            Assert.IsTrue(result.IsDouble);
            Assert.AreEqual(42.789, result.DoubleValue);
        }

        [TestMethod]
        public void Test_ParseFloatError()
        {
            LispVariant result = Lisp.Eval("(do (def s \"nonumber\") (def f (parse-float s)))");
            Assert.IsTrue(result.IsUndefined);
        }

        [TestMethod]
        public void Test_Slice1()
        {
            LispVariant result = Lisp.Eval("(do (def s \"this is a string\") (slice s 0 4))");
            Assert.IsTrue(result.IsString);
            Assert.AreEqual("this", result.StringValue);
        }

        [TestMethod]
        public void Test_Slice2()
        {
            LispVariant result = Lisp.Eval("(do (def s \"this is a string\") (slice s 8 -1))");
            Assert.IsTrue(result.IsString);
            Assert.AreEqual("a string", result.StringValue);
        }

        [TestMethod]
        public void Test_Slice3()
        {
            LispVariant result = Lisp.Eval("(do (def s \"this is a string\") (slice s 5 4))");
            Assert.IsTrue(result.IsString);
            Assert.AreEqual("is a", result.StringValue);
        }

        [TestMethod]
        [ExpectedException(typeof(LispException))]
        public void Test_SliceError()
        {
            LispVariant result = Lisp.Eval("(do (def s \"this is a string\") (slice s))");
            Assert.IsTrue(false);
        }

        [TestMethod]
        public void Test_FirstForString()
        {
            LispVariant result = Lisp.Eval("(do (def s \"this is a string\") (first s))");
            Assert.IsTrue(result.IsString);
            Assert.AreEqual("t", result.StringValue);
        }

        [TestMethod]
        public void Test_LastForString()
        {
            LispVariant result = Lisp.Eval("(do (def s \"this is a string\") (last s))");
            Assert.IsTrue(result.IsString);
            Assert.AreEqual("g", result.StringValue);
        }

        [TestMethod]
        public void Test_RestForString()
        {
            LispVariant result = Lisp.Eval("(do (def s \"this is a string\") (rest s))");
            Assert.IsTrue(result.IsString);
            Assert.AreEqual("his is a string", result.StringValue);
        }

        [TestMethod]
        public void Test_NthForString()
        {
            LispVariant result = Lisp.Eval("(do (def s \"this is a string\") (nth 5 s))");
            Assert.IsTrue(result.IsString);
            Assert.AreEqual("i", result.StringValue);
        }

        [TestMethod]
        public void Test_LenForString()
        {
            LispVariant result = Lisp.Eval("(do (def s \"this is a string\") (len s))");
            Assert.IsTrue(result.IsInt);
            Assert.AreEqual(16, result.IntValue);
        }

        [TestMethod]
        public void Test_Trim()
        {
            LispVariant result = Lisp.Eval("(do (def s \" \t   this is a string  \") (trim s))");
            Assert.IsTrue(result.IsString);
            Assert.AreEqual("this is a string", result.StringValue);
        }

        [TestMethod]
        public void Test_LowerCase()
        {
            LispVariant result = Lisp.Eval("(do (def s \"THIS is A stRing\") (lower-case s))");
            Assert.IsTrue(result.IsString);
            Assert.AreEqual("this is a string", result.StringValue);
        }

        [TestMethod]
        public void Test_UpperCase()
        {
            LispVariant result = Lisp.Eval("(do (def s \"THIS is A stRing 8 ,.!?\") (upper-case s))");
            Assert.IsTrue(result.IsString);
            Assert.AreEqual("THIS IS A STRING 8 ,.!?", result.StringValue);
        }

        [TestMethod]
        [DeploymentItem(@"../../../../../Library/fuellib.fuel", "Library")]
        public void Test_Find1()
        {
            LispVariant result = Lisp.Eval("(do (import \"Library/fuellib.fuel\") (def l '(1 5 7)) (find 5 l))");
            Assert.IsTrue(result.IsInt);
            Assert.AreEqual(1, result.IntValue);
        }

        [TestMethod]
        [DeploymentItem(@"../../../../../Library/fuellib.fuel", "Library")]
        public void Test_Find2()
        {
            LispVariant result = Lisp.Eval("(do (import \"Library/fuellib.fuel\") (def l '(1 5 7)) (find 9 l))");
            Assert.IsTrue(result.IsInt);
            Assert.AreEqual(-1, result.IntValue);
        }

        [TestMethod]
        [DeploymentItem(@"../../../../../Library/fuellib.fuel", "Library")]
        public void Test_DoTimes1()
        {
            LispVariant result = Lisp.Eval("(do (import fuellib) (def l '()) (dotimes (ix 7) (setf l (cons ix l))) (println l))");
            Assert.IsTrue(result.IsString);
            Assert.AreEqual("(6 5 4 3 2 1 0)", result.StringValue);
        }

        [TestMethod]
        [DeploymentItem(@"../../../../../Library/fuellib.fuel", "Library")]
        public void Test_DoTimes2()
        {
            LispVariant result = Lisp.Eval("(do (import fuellib) (def l '()) (dotimes (i 7) (setf l (cons i l))) (dotimes (i 9) (setf l (cons i l))) (println l))");
            Assert.IsTrue(result.IsString);
            Assert.AreEqual("(8 7 6 5 4 3 2 1 0 6 5 4 3 2 1 0)", result.StringValue);
        }

        [TestMethod]
        public void Test_Reverse()
        {
            LispVariant result = Lisp.Eval("(do (def l (list 1 2 'b \"nix\" 4.5)) (print (reverse l)))");
            Assert.IsTrue(result.IsString);
            Assert.AreEqual("(4.5 \"nix\" b 2 1)", result.StringValue);
        }

        [TestMethod]
        public void Test_ReverseString()
        {
            LispVariant result = Lisp.Eval("(do (def s \"this is text\") (print (reverse s)))");
            Assert.IsTrue(result.IsString);
            Assert.AreEqual("txet si siht", result.StringValue);
        }

        [TestMethod]
        public void Test_Search1()
        {
            LispVariant result = Lisp.Eval("(do (def s \"this is text\") (print (search \"tex\" s)))");
            Assert.IsTrue(result.IsString);
            Assert.AreEqual("8", result.StringValue);
        }

        [TestMethod]
        public void Test_Search2()
        {
            LispVariant result = Lisp.Eval("(do (def s \"this is text\") (search \"tes\" s))");
            Assert.IsTrue(result.IsInt);
            Assert.AreEqual(-1, result.IntValue);
        }

        [TestMethod]
        public void Test_Search3()
        {
            LispVariant result = Lisp.Eval("(do (def l '(a b 4 d 5.234 \"blub\")) (search 'b l))");
            Assert.IsTrue(result.IsInt);
            Assert.AreEqual(1, result.IntValue);
        }

        [TestMethod]
        public void Test_Search4()
        {
            LispVariant result = Lisp.Eval("(do (def l '(a b 4 d 5.234 \"blub\")) (search 4 l))");
            Assert.IsTrue(result.IsInt);
            Assert.AreEqual(2, result.IntValue);
        }

        [TestMethod]
        public void Test_Search5()
        {
            LispVariant result = Lisp.Eval("(do (def l '(a b 4 d 5.234 \"blub\")) (search \"blub\" l))");
            Assert.IsTrue(result.IsInt);
            Assert.AreEqual(5, result.IntValue);
        }

        [TestMethod]
        public void Test_Search6()
        {
            LispVariant result = Lisp.Eval("(do (def l '(a b 4 d 5.234 \"blub\")) (search 'nix l))");
            Assert.IsTrue(result.IsInt);
            Assert.AreEqual(-1, result.IntValue);
        }

        [TestMethod]
        [ExpectedException(typeof(LispException))]
        public void Test_Search7()
        {
            LispVariant result = Lisp.Eval("(do (def l 5.234) (search nix l))");
            Assert.IsTrue(result.IsInt);
            Assert.AreEqual(-1, result.IntValue);
        }

        [TestMethod]
        public void Test_Search8()
        {
            LispVariant result = Lisp.Eval("(do (def s \"this is text, with more text items for search.\") (search \"text\" s))");
            Assert.IsTrue(result.IsInt);
            Assert.AreEqual(8, result.IntValue);
            result = Lisp.Eval("(do (def s \"this is text, with more text items for search.\") (search \"text\" s 9))");
            Assert.IsTrue(result.IsInt);
            Assert.AreEqual(24, result.IntValue);
        }

        [TestMethod]
        public void Test_Replace1()
        {
            LispVariant result = Lisp.Eval("(do (def s \"this is a long text\") (replace s \"long\" \"short\"))");
            Assert.IsTrue(result.IsString);
            Assert.AreEqual("this is a short text", result.StringValue);
        }

        [TestMethod]
        public void Test_Replace2()
        {
            LispVariant result = Lisp.Eval("(do (def s \"this is a long long text\") (replace s \"long\" \"short\"))");
            Assert.IsTrue(result.IsString);
            Assert.AreEqual("this is a short short text", result.StringValue);
        }

        [TestMethod]
        public void Test_Replace3()
        {
            LispVariant result = Lisp.Eval("(do (def s \"this is a long text\") (replace s \"verylong\" \"short\"))");
            Assert.IsTrue(result.IsString);
            Assert.AreEqual("this is a long text", result.StringValue);
        }

        [TestMethod]
        public void Test_Replace4()
        {
            LispVariant result = Lisp.Eval("(do (def s \"this is a long text with a lot of words a\") (replace s \"a \" \"an \"))");
            Assert.IsTrue(result.IsString);
            Assert.AreEqual("this is an long text with an lot of words a", result.StringValue);
        }

        [TestMethod]
        public void Test_UnaryMinusInt()
        {
            LispVariant result = Lisp.Eval("(do (def a 8) (- a))");
            Assert.IsTrue(result.IsInt);
            Assert.AreEqual(-8, result.IntValue);
        }

        [TestMethod]
        public void Test_UnaryMinusDouble()
        {
            LispVariant result = Lisp.Eval("(do (def a 1.234) (- a))");
            Assert.IsTrue(result.IsDouble);
            Assert.AreEqual(-1.234, result.DoubleValue);
        }

        [TestMethod]
        [ExpectedException(typeof(LispException))]
        public void Test_UnaryMinusString()
        {
            LispVariant result = Lisp.Eval("(do (def a \"nix\") (- a))");
        }

        [TestMethod]
        public void Test_ModuloInt()
        {
            LispVariant result = Lisp.Eval("(do (% 7 3))");
            Assert.IsTrue(result.IsInt);
            Assert.AreEqual(1, result.IntValue);
        }

        [TestMethod]
        public void Test_ModuloDouble()
        {
            LispVariant result = Lisp.Eval("(do (% 7.4 2.8))");
            Assert.IsTrue(result.IsDouble);
            Assert.AreEqual("1.8", result.DoubleValue.ToString(CultureInfo.InvariantCulture).Substring(0, 3));  // because of rounding error...
        }

        [TestMethod]
        public void Test_LeftShift()
        {
            LispVariant result = Lisp.Eval("(do (<< 4 2))");
            Assert.IsTrue(result.IsInt);
            Assert.AreEqual("16", result.IntValue.ToString(CultureInfo.InvariantCulture));
        }

        [TestMethod]
        public void Test_RightShift()
        {
            LispVariant result = Lisp.Eval("(do (>> 33 1))");
            Assert.IsTrue(result.IsInt);
            Assert.AreEqual("16", result.IntValue.ToString(CultureInfo.InvariantCulture));
        }

        [TestMethod]
        public void Test_BinaryOr()
        {
            LispVariant result = Lisp.Eval("(do (| 18 7))");
            Assert.IsTrue(result.IsInt);
            Assert.AreEqual("23", result.IntValue.ToString(CultureInfo.InvariantCulture));
        }

        [TestMethod]
        public void Test_BinaryAnd()
        {
            LispVariant result = Lisp.Eval("(do (& 18 7))");
            Assert.IsTrue(result.IsInt);
            Assert.AreEqual("2", result.IntValue.ToString(CultureInfo.InvariantCulture));
        }

        [TestMethod]
        public void Test_BinaryXOr()
        {
            LispVariant result = Lisp.Eval("(do (^ 13 6))");
            Assert.IsTrue(result.IsInt);
            Assert.AreEqual("11", result.IntValue.ToString(CultureInfo.InvariantCulture));
        }

        [TestMethod]
        public void Test_BinaryNot()
        {
            LispVariant result = Lisp.Eval("(do (~ 256))");
            Assert.IsTrue(result.IsInt);
            Assert.AreEqual("-257", result.IntValue.ToString(CultureInfo.InvariantCulture));
        }

        [TestMethod]
        public void Test_NotEnoughFunctionArguments()
        {
            LispVariant result = Lisp.Eval("(do (defn f (x y z) (add (str x) (str y) (str z))) (f 3))");
            Assert.IsTrue(result.IsString);
            Assert.AreEqual("3NILNIL", result.StringValue);
        }

        [TestMethod]
        public void Test_IntConversion1()
        {
            LispVariant result = Lisp.Eval("(do (int 7.4))");
            Assert.IsTrue(result.IsInt);
            Assert.AreEqual(7, result.IntValue);
        }

        [TestMethod]
        public void Test_IntConversion2()
        {
            LispVariant result = Lisp.Eval("(do (int \"61.234\"))");
            Assert.IsTrue(result.IsUndefined);
        }

        [TestMethod]
        public void Test_IntConversion3()
        {
            LispVariant result = Lisp.Eval("(do (int #t))");
            Assert.IsTrue(result.IsInt);
            Assert.AreEqual(1, result.IntValue);
        }

        [TestMethod]
        public void Test_IntConversion4()
        {
            LispVariant result = Lisp.Eval("(do (int #f))");
            Assert.IsTrue(result.IsInt);
            Assert.AreEqual(0, result.IntValue);
        }

        [TestMethod]
        public void Test_IntConversion5()
        {
            LispVariant result = Lisp.Eval("(do (int \"a text\"))");
            Assert.IsTrue(result.IsUndefined);
        }

        [TestMethod]
        public void Test_FloatConversion1()
        {
            LispVariant result = Lisp.Eval("(do (float 7))");
            Assert.IsTrue(result.IsDouble);
            Assert.AreEqual(7, result.DoubleValue);
        }

        [TestMethod]
        public void Test_FloatConversion2()
        {
            LispVariant result = Lisp.Eval("(do (float \"61.234\"))");
            Assert.IsTrue(result.IsDouble);
            Assert.AreEqual(61.234, result.DoubleValue);
        }

        [TestMethod]
        public void Test_FloatConversion3()
        {
            LispVariant result = Lisp.Eval("(do (float #t))");
            Assert.IsTrue(result.IsDouble);
            Assert.AreEqual(1, result.DoubleValue);
        }

        [TestMethod]
        public void Test_FloatConversion4()
        {
            LispVariant result = Lisp.Eval("(do (float #f))");
            Assert.IsTrue(result.IsDouble);
            Assert.AreEqual(0, result.DoubleValue);
        }

        [TestMethod]
        public void Test_FloatConversion5()
        {
            LispVariant result = Lisp.Eval("(do (float \"a text\"))");
            Assert.IsTrue(result.IsUndefined);
        }

        [TestMethod]
        public void Test_DelVar1()
        {
            LispVariant result = Lisp.Eval("(do (def a 8) (delvar 'a))");
            Assert.IsTrue(result.IsBool);
            Assert.AreEqual(true, result.ToBool());
        }

        [TestMethod]
        public void Test_DelVar2()
        {
            LispVariant result = Lisp.Eval("(do (def a 8) (delvar 'b))");
            Assert.IsTrue(result.IsBool);
            Assert.AreEqual(false, result.ToBool());
        }

        [TestMethod]
        public void Test_NeedLValue1()
        {
            LispVariant result = Lisp.Eval("(do (def a 7) (defn test () (do (println (need-l-value)) (return 'a))) (setf (test) 8) (println a))");
            Assert.IsTrue(result.IsString);
            Assert.AreEqual("8", result.ToString());
        }

        [TestMethod]
        public void Test_NeedLValue2()
        {
            LispVariant result = Lisp.Eval("(do (def a 7) (defn test () (need-l-value)) (println (test)))");
            Assert.IsTrue(result.IsString);
            Assert.AreEqual("#f", result.ToString());
        }

        [TestMethod]
        public void Test_ImportPathTest()
        {
            LispVariant result = Lisp.Eval("(do (import \"../../../../../Library/fuellib.fuel\") (def s \"some text\") (strlen s))");
            Assert.IsTrue(result.IsInt);
            Assert.AreEqual(9, result.ToInt());
        }

        [TestMethod]
        public void Test_DictMake()
        {
            LispVariant result = Lisp.Eval("(do (def d (make-dict)))");
            Assert.IsTrue(result.IsNativeObject);
            Assert.AreEqual("{  }", result.ToString());
        }

        [TestMethod]
        public void Test_DictSet()
        {
            LispVariant result = Lisp.Eval("(do (def d (make-dict)) (dict-set d \"key1\" 42) (dict-set d 7 \"some text\") (println d))");
            Assert.IsTrue(result.IsString);
            Assert.AreEqual("{ [\"key1\" : 42], [7 : \"some text\"] }", result.ToString());
        }

        [TestMethod]
        public void Test_DictGet()
        {
            LispVariant result = Lisp.Eval("(do (def d (make-dict)) (dict-set d \"key1\" 42) (dict-set d 7 \"some text\") (dict-get d 7))");
            Assert.IsTrue(result.IsString);
            Assert.AreEqual("some text", result.ToString());
        }

        [TestMethod]
        public void Test_DictLen()
        {
            LispVariant result = Lisp.Eval("(do (def d (make-dict)) (dict-set d \"key1\" 42) (dict-set d 7 \"some text\") (len d))");
            Assert.IsTrue(result.IsInt);
            Assert.AreEqual(2, result.ToInt());
        }

        [TestMethod]
        public void Test_DictClear()
        {
            LispVariant result = Lisp.Eval("(do (def d (make-dict)) (dict-set d \"key1\" 42) (dict-set d 7 \"some text\") (dict-clear d) (len d))");
            Assert.IsTrue(result.IsInt);
            Assert.AreEqual(0, result.ToInt());
        }

        [TestMethod]
        public void Test_DictKeys()
        {
            LispVariant result = Lisp.Eval("(do (def d (make-dict)) (dict-set d \"key1\" 42) (dict-set d 7 \"some text\") (dict-keys d))");
            Assert.IsTrue(result.IsList);
            Assert.AreEqual("(\"key1\" 7)", result.ToString());
        }

        [TestMethod]
        public void Test_DictRemove()
        {
            LispVariant result = Lisp.Eval("(do (def d (make-dict)) (dict-set d \"key1\" 42) (dict-set d 7 \"some text\") (dict-remove d 7) (println d))");
            Assert.IsTrue(result.IsString);
            Assert.AreEqual("{ [\"key1\" : 42] }", result.ToString());
        }

        [TestMethod]
        public void Test_DictContainsKey1()
        {
            LispVariant result = Lisp.Eval("(do (def d (make-dict)) (dict-set d \"key1\" 42) (dict-set d 7 \"some text\") (dict-contains-key d 8))");
            Assert.IsTrue(result.IsBool);
            Assert.AreEqual(false, result.ToBool());
        }

        [TestMethod]
        public void Test_DictContainsKey2()
        {
            LispVariant result = Lisp.Eval("(do (def d (make-dict)) (dict-set d \"key1\" 42) (dict-set d 7 \"some text\") (dict-contains-key d \"key1\"))");
            Assert.IsTrue(result.IsBool);
            Assert.AreEqual(true, result.ToBool());
        }

        [TestMethod]
        public void Test_DictContainsValue1()
        {
            LispVariant result = Lisp.Eval("(do (def d (make-dict)) (dict-set d \"key1\" 42) (dict-set d 7 \"some text\") (dict-contains-value d 99))");
            Assert.IsTrue(result.IsBool);
            Assert.AreEqual(false, result.ToBool());
        }

        [TestMethod]
        public void Test_DictContainsValue2()
        {
            LispVariant result = Lisp.Eval("(do (def d (make-dict)) (dict-set d \"key1\" 42) (dict-set d 7 \"some text\") (dict-contains-value d 42))");
            Assert.IsTrue(result.IsBool);
            Assert.AreEqual(true, result.ToBool());
        }

        [TestMethod]
        public void Test_Quasiquote11()
        {
            LispVariant result = Lisp.Eval("(do (def l (list 1 2 3)) (println `,@l))");
            Assert.IsTrue(result.IsString);
            Assert.AreEqual("1 2 3", result.ToString());
        }

        [TestMethod]
        public void Test_Quasiquote12()
        {
            LispVariant result = Lisp.Eval("(do (def l 78) (println `,@l))");
            Assert.IsTrue(result.IsString);
            Assert.AreEqual("78", result.ToString());
        }

        [TestMethod]
        public void Test_Quasiquote13()
        {
            LispVariant result = Lisp.Eval("(do (def l 78) (println `,l))");
            Assert.IsTrue(result.IsString);
            Assert.AreEqual("78", result.ToString());
        }

        [TestMethod]
        public void Test_Quasiquote14()
        {
            LispVariant result = Lisp.Eval("(do (def l (list 1 2 3)) (println `,l))");
            Assert.IsTrue(result.IsString);
            Assert.AreEqual("(1 2 3)", result.ToString());
        }

        [TestMethod]
        public void Test_NestedMacroExpand()
        {
            LispVariant result = Lisp.Eval(@"(do
(define-macro-expand setqq (symbol value)
   '(setf symbol value)
)

(define-macro-expand begin (sequence)
   (list 'do `,@(quoted-macro-args))
)

(begin
  (def x 42)
  (setqq x 4)    
  (println x)
))");
            Assert.IsTrue(result.IsString);
            Assert.AreEqual("4", result.ToString());
        }

        [TestMethod]
        [ExpectedException(typeof(LispException))]
        public void Test_FormatStr0()
        {
            LispVariant result = Lisp.Eval("(do (println (format \"Hello int={0} double={1} str={2}\")))");
            Assert.IsTrue(result.IsString);
            Assert.AreEqual("Hello int= double= str=", result.ToString());
        }

        [TestMethod]
        public void Test_FormatStr1()
        {
            LispVariant result = Lisp.Eval("(do (println (format \"Hello int={0} double={1} str={2}\" 42 2.3456 \"world\")))");
            Assert.IsTrue(result.IsString);
            Assert.AreEqual("Hello int=42 double=2.3456 str=world", result.ToString());
        }
    }
}
