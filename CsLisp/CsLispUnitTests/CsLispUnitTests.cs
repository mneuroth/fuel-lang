using System;
using CsLisp;
using Microsoft.VisualStudio.TestTools.UnitTesting;

namespace LispUnitTests
{
    [TestClass]
    public class LispInterpreterTest
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
        public void Test_Setf1()
        {
            LispVariant result = Lisp.Eval("(do (def a 1) (def b 1) (setf (first (list 'a 'b 'c)) 9))");
            Assert.AreEqual(9, result.ToInt());
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
            Assert.AreEqual("NIL", result.ToString());
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
        public void Test_EvalStr()
        {
            LispVariant result = Lisp.Eval("(evalstr \"(def x 456)\")");
            Assert.AreEqual(456, result.ToInt());
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
            LispVariant result = Lisp.Eval("(+ '(1 2 3) '(\"hello\" 2.3 42))");
            Assert.AreEqual("(1 2 3 hello 2.3 42)", result.ToString());
        }

        [TestMethod]
        public void Test_ListFirst()
        {
            LispVariant result = Lisp.Eval("(first '(1 2 3))");
            Assert.AreEqual(1, result.ToInt());
        }

        [TestMethod]
        public void Test_ListRest()
        {
            LispVariant result = Lisp.Eval("(rest '(1 2 3))");
            Assert.AreEqual("(2 3)", result.ToString());
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
        public void Test_LogicalOperators()
        {
            LispVariant result = Lisp.Eval("(list (and #t #f) (and #t #t) (or #t #f) (or #f #f) (or #t #f #t))");
            Assert.AreEqual("(#f #t #t #f #t)", result.ToString());
        }

        [TestMethod]
        public void Test_CompareOperators1()
        {
            LispVariant result = Lisp.Eval("(list (= 1 2) (= 4 4) (== \"blub\" \"blub\") (== #t #f) (equal 3 4))");
            Assert.AreEqual("(#f #t #t #f #f)", result.ToString());
        }

        [TestMethod]
        public void Test_CompareOperators2()
        {
            LispVariant result = Lisp.Eval("(list (< 1 2) (< 4 1) (> 5 2) (> 1 3) (> 4.0 4.0))");
            Assert.AreEqual("(#t #f #t #f #f)", result.ToString());
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
        public void Test_Macros1()
        {
            LispVariant result = Lisp.Eval("(do (define-macro blub (lambda (x y) (println x y))) (println (quote (1 2 3))) (blub 3 4))");
            Assert.AreEqual("3 4", result.ToString());
        }

        [TestMethod]
        public void Test_MacrosExpand1()
        {
            LispVariant result = Lisp.Eval("(do (define-macro-expand blub (x y) (println x y)) (println (quote (1 2 3))) (blub 3 4))");
            Assert.AreEqual("3 4", result.ToString());
        }

        [TestMethod]
        public void Test_MacrosExpand2()
        {
            const string MacroExpandScript = @"(do
  (define-macro-expand first-macro
        (a b) 
        (do 
    	   (def i 1)
           (+ a b i)
        )
  )
  
  (define-macro-expand second-macro
        (x y) 
        (do 
           (* x y (first-macro x y))
        )
  )
  
  (def m (second-macro 4 3))
)";
            LispVariant result = Lisp.Eval(MacroExpandScript);
            Assert.AreEqual("96", result.ToString());
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
        public void Test_Quote1()
        {
            LispVariant result = Lisp.Eval("(do (def x 42) (println 'x))");
            Assert.AreEqual("x", result.ToString());
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
            LispVariant result = Lisp.Eval("(map 4 '(1 2 3))");
        }

        [TestMethod]
        [ExpectedException(typeof(LispException))]
        public void Test_MapError2()
        {
            LispVariant result = Lisp.Eval("(map (lambda (x) (+ x 1)) 4)");
        }

        [TestMethod]
        [ExpectedException(typeof(LispException))]
        public void Test_ReduceError1()
        {
            LispVariant result = Lisp.Eval("(reduce \"blub\" '(1 2 3) 0)");
        }

        [TestMethod]
        [ExpectedException(typeof(LispException))]
        public void Test_ReduceError2()
        {
            LispVariant result = Lisp.Eval("(reduce (lambda (x y) (+ x y))  \"test\" 0)");
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
        public void Test_Call1()
        {
            LispVariant result = Lisp.Eval("(do (def obj 0) (setf obj (call \"CsLisp.DummyNative\")) (println obj (type obj)) (call obj \"Test\"))");
            Assert.AreEqual(42, result.ToInt());
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
            LispVariant result = Lisp.Eval("(sym a)");
            Assert.IsTrue(result.IsSymbol);
            Assert.AreEqual("a", result.StringValue);
        }

        [TestMethod]
        public void Test_Str()
        {
            LispVariant result = Lisp.Eval("(str abc)");
            Assert.IsTrue(result.IsString);
            Assert.AreEqual("abc", result.StringValue);
        }

        #endregion

        #region expected errors

        [TestMethod]
        [ExpectedException(typeof(LispException))]
        public void Test_Parser1()
        {
            LispVariant result = Lisp.Eval("(println \"hello\"))");
        }

        [TestMethod]
        [ExpectedException(typeof(LispException))]
        public void Test_Parser2()
        {
            LispVariant result = Lisp.Eval("((println \"hello\")");
        }

        [TestMethod]
        [ExpectedException(typeof(LispException))]
        public void Test_Parser3()
        {
            LispVariant result = Lisp.Eval("(blub 1 2 3)");
        }

        [TestMethod]
        [ExpectedException(typeof(LispException))]
        public void Test_SetfError()
        {
            LispVariant result = Lisp.Eval("(setf a 2.0)");
        }

        [TestMethod]
        [ExpectedException(typeof(LispException))]
        public void Test_NotError()
        {
            LispVariant result = Lisp.Eval("(not a 2.0)");
        }

        [TestMethod]
        [ExpectedException(typeof(LispException))]
        public void Test_CompareError1()
        {
            LispVariant result = Lisp.Eval("(> 2.0)");
        }

        [TestMethod]
        [ExpectedException(typeof(LispException))]
        public void Test_CompareError2()
        {
            LispVariant result = Lisp.Eval("(> 2.0 5 234)");
        }

        [TestMethod]
        [ExpectedException(typeof(LispException))]
        public void Test_ScriptToLong()
        {
            LispVariant result = Lisp.Eval("(setf a 2.0) asdf");
        }

        [TestMethod]
        [ExpectedException(typeof(LispException))]
        public void Test_DefError()
        {
            LispVariant result = Lisp.Eval("(def 1 2)");
        }

        [TestMethod]
        [ExpectedException(typeof(LispException))]
        public void Test_DoError()
        {
            LispVariant result = Lisp.Eval("(do (def a 2) blub (setf a 5))");
        }

        [TestMethod]
        [ExpectedException(typeof(LispException))]
        public void Test_IfError()
        {
            LispVariant result = Lisp.Eval("(if #t 1 2 3)");
        }

        [TestMethod]
        [ExpectedException(typeof(LispException))]
        public void Test_FunctionNotFound()
        {
            LispVariant result = Lisp.Eval("(unknown-fcn 1 2 3)");
        }

        [TestMethod]
        [ExpectedException(typeof(LispException))]
        public void Test_BracketsOutOfBalance1()
        {
            LispVariant result = Lisp.Eval("(do (println 2)))");
        }

        [TestMethod]
        [ExpectedException(typeof(LispException))]
        public void Test_BracketsOutOfBalance2()
        {
            LispVariant result = Lisp.Eval("(do ( (println 2))");
        }

        [TestMethod]
        [ExpectedException(typeof(LispException))]
        public void Test_UnexpectedToken1()
        {
            LispVariant result = Lisp.Eval("blub (do (println 2))");
        }

        [TestMethod]
        [ExpectedException(typeof(LispException))]
        public void Test_UnexpectedTokenButIsBracketsOutOfBalance()
        {
            LispVariant result = Lisp.Eval("(do (println 2)) asfd");
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
            LispVariant result = Lisp.Eval("(type aSymbol)");
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
                Assert.AreEqual(true, s.Contains("b --> asdf"));
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
        [DeploymentItem(@"..\..\..\Library\fuellib.fuel", "Library")]
        public void Test_Import()
        {
            using (ConsoleRedirector cr = new ConsoleRedirector())
            {
                LispVariant result = Lisp.Eval("(do (import \"Library\\\\fuellib.fuel\") (foreach '(1 5 7) (lambda (x) (println x))))");
                Assert.IsTrue(result.IsInt);
                Assert.AreEqual(3, result.IntValue);

                string s = cr.ToString().Trim();
                Assert.IsTrue(s.Contains("1"));
                Assert.IsTrue(s.Contains("7"));
                Assert.IsTrue(s.Contains("7"));
            }
        }

        [TestMethod]
        [DeploymentItem(@"..\..\..\Library\fuellib.fuel", "Library")]
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
        [DeploymentItem(@"..\..\..\Library\fuellib.fuel", "Library")]
        public void Test_CreateNative()
        {
            using (ConsoleRedirector cr = new ConsoleRedirector())
            {
                LispVariant result = Lisp.Eval("(do (import fuellib) (create-native \"List\" \"System.Collections.Generic.List`1[[System.Int32, mscorlib, Version=4.0.0.0, Culture=neutral, PublicKeyToken=b77a5c561934e089]]\") (def obj (create-List)) (List-Add obj 7) (call obj \"Add\" 5) (println (List-get_Count obj)))");
                Assert.IsTrue(result.IsString);
                Assert.AreEqual("2", result.Value.ToString());

                string s = cr.ToString().Trim();
                Assert.IsTrue(s.Contains("2"));
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
    }
}
