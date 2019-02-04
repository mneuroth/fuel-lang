#include <QString>
#include <QtTest>
#include <QDir>
#include <qtestcase.h>

#include <QDebug>

#include "../CppLispInterpreter/Variant.h"
#include "../CppLispInterpreter/Lisp.h"

using namespace CppLisp;

double Math_Round(double val)
{
    return round(val);
}

class QtLispInterpreterUnitTestsTest : public QObject
{
    Q_OBJECT

public:
    QtLispInterpreterUnitTestsTest();

private Q_SLOTS:
    void initTestCase()
    {
        qDebug("INIT fuel unit tests.");
        QDir aCurrentDir(".");
        aCurrentDir.mkpath("./library");
        QFile::copy(":/fuellib.fuel", "./library/fuellib.fuel");
    }

    void testCreateVariant();
    void testVariantCompare();
    void testVariantConvert();
    void testVariantOperations();
    void testVariantEqualOp();
    void testVariantCastError();
    void testStringIndexOf();

    void Test_Comments()
    {
        std::shared_ptr<LispVariant> result = Lisp::Eval("(do (println \"hello\") ; a comment\n(println; separate lists with comments\n\"world\"));comment in last line");
        QCOMPARE("world", result->ToString().c_str());
    }

    void Test_DoAndPrint()
    {
        std::shared_ptr<LispVariant> result = Lisp::Eval("(do (println \"hello\")\n (println \"world\"))");
        QCOMPARE("world", result->ToString().c_str());
    }

    void Test_PrintLnMultilines()
    {
        std::shared_ptr<LispVariant> result = Lisp::Eval("(do (println \"hello\nworld\"))");
        QCOMPARE("hello\nworld", result->ToString().c_str());
    }

    void Test_PrintTrace()
    {
        std::shared_ptr<LispVariant> result = Lisp::Eval("(do (trace #t) (println \"hello world\") (println (+ 9 8)) (gettrace))");
        QCOMPARE("hello world17", result->ToString().c_str());
    }

    void Test_If1()
    {
        std::shared_ptr<LispVariant> result = Lisp::Eval("(if #t (+ 1 2) (- 3 5))");
        QCOMPARE(3, result->ToInt());
    }

    void Test_If2()
    {
        std::shared_ptr<LispVariant> result = Lisp::Eval("(if #f (* 1 0) (/ 6 3))");
        QCOMPARE(2, result->ToInt());
    }

    void Test_If3()
    {
        std::shared_ptr<LispVariant> result = Lisp::Eval("(if true 1 0)");
        QCOMPARE(1, result->ToInt());
    }

    void Test_If4()
    {
        std::shared_ptr<LispVariant> result = Lisp::Eval("(if false 1 0)");
        QCOMPARE(0, result->ToInt());
    }

    void Test_If5()
    {
        std::shared_ptr<LispVariant> result = Lisp::Eval("(if true 1)");
        QCOMPARE(1, result->ToInt());
        result = Lisp::Eval("(if false 1)");
        QVERIFY(result.get() == 0);
    }

    void Test_Setf1()
    {
        std::shared_ptr<LispVariant> result = Lisp::Eval("(do (def a 1) (def b 1) (setf (first (list 'a 'b 'c)) 9))");
        QCOMPARE(9, result->ToInt());
    }

    void Test_SetfWithNth()
    {
        std::shared_ptr<LispVariant> result = Lisp::Eval("(do (def l '(a b c d)) (setf (nth 2 l) 9) (print l))");
        QCOMPARE("(a b 9 d)", result->ToString().c_str());
    }

    void Test_SetfWithFirst()
    {
        std::shared_ptr<LispVariant> result = Lisp::Eval("(do (def l '(a b c d)) (setf (first l) 21) (print l))");
        QCOMPARE("(21 b c d)", result->ToString().c_str());
    }

    void Test_SetfWithLast()
    {
        std::shared_ptr<LispVariant> result = Lisp::Eval("(do (def l '(a b c d)) (setf (last l) \"xyz\") (print l))");
        QCOMPARE("(a b c \"xyz\")", result->ToString().c_str());
    }

    void Test_SetfWithMacros()
    //[DeploymentItem(@"Library\fuellib.fuel", "Library")]
    {
        std::shared_ptr<LispVariant> result = Lisp::Eval("(do (import fuellib) (defstruct point x y) (def p (make-point 12 17)) (setf (get-point-x p) 9) (println p))");
        QVERIFY(result->IsString());
        QCOMPARE("(#point 9 17)", result->ToString().c_str());
    }

    void Test_DefstructMacro()
    //[DeploymentItem(@"Library\fuellib.fuel", "Library")]
    {
        std::shared_ptr<LispVariant> result = Lisp::Eval("(do (import fuellib) (defstruct point x y) (def p (make-point 12 17)))");
        QVERIFY(result->IsList());
        QCOMPARE("(#point 12 17)", result->ToString().c_str());
    }
    void Test_QuoteList()
    {
        std::shared_ptr<LispVariant> result = Lisp::Eval("(do (def a 1) (def l '(a b c)))");
        QCOMPARE("(a b c)", result->ToString().c_str());
    }

    void Test_Def1()
    {
        std::shared_ptr<LispVariant> result = Lisp::Eval("(do (def a 1) (def b 1) (def (nth 2 (list 'a 'b 'c)) 9) (+ c 2))");
        QCOMPARE(11, result->ToInt());
    }

    void Test_DefWithNil()
    {
        std::shared_ptr<LispVariant> result = Lisp::Eval("(do (def a nil) (println a))");
        QCOMPARE(LispToken::NilConst.c_str(), result->ToString().c_str());
    }

    void Test_Fn()
    {
        std::shared_ptr<LispVariant> result = Lisp::Eval("(do (def f (fn (x) (+ x x 1))) (println (f 8)))");
        QCOMPARE(17, result->ToInt());
    }

    void Test_Gdef()
    {
        std::shared_ptr<LispVariant> result = Lisp::Eval("(do (defn f (x) (gdef z (+ x x))) (f 8) (println z))");
        QCOMPARE(16, result->ToInt());
    }

    void Test_Gdefn()
    {
        std::shared_ptr<LispVariant> result = Lisp::Eval("(do (defn f (x) (gdefn g (x) (+ x x))) (f 2) (g 8))");
        QCOMPARE(16, result->ToInt());
    }

    void Test_Eval1()
    {
        std::shared_ptr<LispVariant> result = Lisp::Eval("(eval (list 'def 'x 43))");
        QCOMPARE(43, result->ToInt());
    }

    void Test_Eval2()
    {
        std::shared_ptr<LispVariant> result = Lisp::Eval("(eval '(def x 456))");
        QCOMPARE(456, result->ToInt());
    }

    void Test_Eval3()
    {
        std::shared_ptr<LispVariant> result = Lisp::Eval("(eval #t)");
        QCOMPARE(true, result->ToBool());
        result = Lisp::Eval("(eval 42)");
        QCOMPARE(42, result->ToInt());
    }

    void Test_Eval4()
    {
        const string script = "(do  \
                                  (defn defstructfunc (name)  \
                                    (do \
                                      (def structsym (sym (+ \"#\" name))) \
                                      (list 'defn (sym (+ \"make-\" name)) (cdr (args))  \
                                          `(list ,structsym ,@(cdr(args))) \
                                      ) \
                                    ) \
                                  ) \
\
                                  (def f (defstructfunc point x y z)) \
                                  (eval f) \
                                  (def p (make-point 1 2 3)) \
                                )";
        std::shared_ptr<LispVariant> result = Lisp::Eval(script);
        QCOMPARE(true, result->IsList());
        QCOMPARE("(#point 1 2 3)", result->ToString().c_str());
    }

    void Test_Eval5()
    {
        const string script = "(do \
                                  (defn defsimplefunc ()  \
                                    (do \
                                      (list 'defn 'simplefunc '(a b)  \
                                          '(+ a b) \
                                      ) \
                                    ) \
                                  ) \
\
                                  (def f (defsimplefunc)) \
                                  (eval f) \
                                  (def p (simplefunc 1 2))  \
                                )";
        std::shared_ptr<LispVariant> result = Lisp::Eval(script);
        QCOMPARE(true, result->IsInt());
        QCOMPARE("3", result->ToString().c_str());
    }

    void Test_EvalStr()
    {
        std::shared_ptr<LispVariant> result = Lisp::Eval("(evalstr \"(def x 456)\")");
        QCOMPARE(456, result->ToInt());
    }

    void Test_Doc()
    {
        std::shared_ptr<LispVariant> result = Lisp::Eval("(doc 'if)");
        var s = result->ToString();
        QVERIFY(s.Contains("-------------------------------------------------"));
        QVERIFY(s.Contains("if  [special form]"));
        QVERIFY(s.Contains("Syntax: (if cond then-block [else-block])"));
        QVERIFY(s.Contains("The if statement."));
    }

    void Test_DocForDefn()
    {
        std::shared_ptr<LispVariant> result = Lisp::Eval("; this is a fcn documenataion\n(defn blub (x) (+ x 1))\n(doc 'blub)");
        var s = result->ToString();
        QVERIFY(s.Contains("-------------------------------------------------"));
        QVERIFY(s.Contains("blub"));
        QVERIFY(s.Contains("Syntax: (blub x)"));
        QVERIFY(s.Contains("; this is a fcn documenataion"));
    }

    void Test_NoAutoDocForDefn()
    {
        std::shared_ptr<LispVariant> result = Lisp::Eval("(defn blub (x y ) (+ x y 1))\n(doc 'blub)");
        var s = result->ToString();
        QVERIFY(s.Contains("-------------------------------------------------"));
        QVERIFY(s.Contains("blub"));
        QVERIFY(s.Contains("Syntax: (blub x y)"));
    }

    void Test_TickCount()
    {
        std::shared_ptr<LispVariant> result = Lisp::Eval("(tickcount)");
        QVERIFY(result->IsInt());
    }

    void Test_While1()
    {
        std::shared_ptr<LispVariant> result = Lisp::Eval("(do (def a 1) (def b 1) (while (< a 10) (do (setf a (+ a 1)) (setf b (+ b 1)))))");
        QCOMPARE(10, result->ToInt());
    }

    void Test_Defn1()
    {
        std::shared_ptr<LispVariant> result = Lisp::Eval("(do (def g_prn \"START:\") (defn prn (x) (setf g_prn (add g_prn x))) (prn \"34\") (println g_prn))");
        QCOMPARE("START:34", result->ToString().c_str());
    }

    void Test_AddString()
    {
        std::shared_ptr<LispVariant> result = Lisp::Eval("(+ \"abc\" \"def() ; blub\" \"xxx\")");
        QCOMPARE("abcdef() ; blubxxx", result->ToString().c_str());
    }

    void Test_AddLists()
    {
        std::shared_ptr<LispVariant> result = Lisp::Eval("(+ '(1 2 3) '(\"hello world\" 2.3 42))");
        string s = result->ToString();
        QCOMPARE("(1 2 3 \"hello world\" 2.300000 42)", result->ToString().c_str());
    }

    void Test_ListFirst()
    {
        std::shared_ptr<LispVariant> result = Lisp::Eval("(first '(1 2 3))");
        QCOMPARE(1, result->ToInt());
    }

    void Test_ListFirstSymbol()
    {
        std::shared_ptr<LispVariant> result = Lisp::Eval("(first '(a b c))");
        QCOMPARE("a", result->ToString().c_str());
    }

    void Test_ListLast()
    {
        std::shared_ptr<LispVariant> result = Lisp::Eval("(last '(1 2 3))");
        QCOMPARE(3, result->ToInt());
    }

    void Test_ListLastSymbol()
    {
        std::shared_ptr<LispVariant> result = Lisp::Eval("(last '(abc def xyz))");
        QCOMPARE("xyz", result->ToString().c_str());
    }

    void Test_ListCar()
    {
        std::shared_ptr<LispVariant> result = Lisp::Eval("(car '(\"abc\" 2 3))");
        QCOMPARE("abc", result->ToString().c_str());
    }

    void Test_ListRest()
    {
        std::shared_ptr<LispVariant> result = Lisp::Eval("(rest '(1 2 3))");
        QCOMPARE("(2 3)", result->ToString().c_str());
    }

    void Test_ListCdr()
    {
        std::shared_ptr<LispVariant> result = Lisp::Eval("(cdr '(\"nix\" 1 2 3))");
        QCOMPARE("(1 2 3)", result->ToString().c_str());
    }

    void Test_ListLength()
    {
        std::shared_ptr<LispVariant> result = Lisp::Eval("(len '(1 2 3))");
        QCOMPARE(3, result->ToInt());
    }

    void Test_ListAppend()
    {
        std::shared_ptr<LispVariant> result = Lisp::Eval("(append (list 4 54 3) (list 7 9))");
        QCOMPARE("(4 54 3 7 9)", result->ToString().c_str());
    }

    void Test_ListPush1()
    {
        std::shared_ptr<LispVariant> result = Lisp::Eval("(do (def l '(a b c)) (push z l))");
        QCOMPARE("(z a b c)", result->ToString().c_str());
    }

    void Test_ListPush2()
    {
        std::shared_ptr<LispVariant> result = Lisp::Eval("(do (def l '(a b c)) (push z l 2))");
        QCOMPARE("(a b z c)", result->ToString().c_str());
    }

    void Test_ListPush3()
    {
        std::shared_ptr<LispVariant> result = Lisp::Eval("(do (def l '(a b c)) (push z l 2) (print l))");
        QCOMPARE("(a b z c)", result->ToString().c_str());
    }

    void Test_ListPushError()
    {
        try
        {
            std::shared_ptr<LispVariant> result = Lisp::Eval("(do (def l 42) (push z l 2))");
            QVERIFY(false);
        }
        catch (const CppLisp::LispException &)
        {
            QVERIFY(true);
        }
        catch (...)
        {
            QVERIFY(false);
        }
    }

    void Test_ListPop1()
    {
        std::shared_ptr<LispVariant> result = Lisp::Eval("(do (def l '(a b c)) (def a (pop l)) (print a l))");
        QCOMPARE("a (b c)", result->ToString().c_str());
    }

    void Test_ListPop2()
    {
        std::shared_ptr<LispVariant> result = Lisp::Eval("(do (def l '()) (def a (pop l)) (print a l))");
        QCOMPARE("NIL ()", result->ToString().c_str());
    }

    void Test_ListPop3()
    {
        std::shared_ptr<LispVariant> result = Lisp::Eval("(do (def l '()) (def a (pop l)) (print a l))");
        QCOMPARE("NIL ()", result->ToString().c_str());
    }

    void Test_ListPop4()
    {
        std::shared_ptr<LispVariant> result = Lisp::Eval("(do (def l '(a b c)) (def a (pop l 5)) (print a l))");
        QCOMPARE("NIL (a b c)", result->ToString().c_str());
    }

    void Test_ListPop5()
    {
        std::shared_ptr<LispVariant> result = Lisp::Eval("(do (def l '(a b c)) (def a (pop l 1)) (print a l))");
        QCOMPARE("b (a c)", result->ToString().c_str());
    }

    void Test_ListPopError()
    {
        try
        {
            std::shared_ptr<LispVariant> result = Lisp::Eval("(do (def l 5) (def a (pop l)) (print a l))");
            QVERIFY(false);
        }
        catch (const CppLisp::LispException &)
        {
            QVERIFY(true);
        }
        catch (...)
        {
            QVERIFY(false);
        }
    }

    void Test_LogicalOperators()
    {
        std::shared_ptr<LispVariant> result = Lisp::Eval("(list (and #t #f) (and #t #t) (or #t #f) (or #f #f) (or #t #f #t))");
        QCOMPARE("(#f #t #t #f #t)", result->ToString().c_str());
    }

    void Test_CompareOperators1()
    {
        std::shared_ptr<LispVariant> result = Lisp::Eval("(list (= 1 2) (= 4 4) (== \"blub\" \"blub\") (== #t #f) (equal 3 4))");
        QCOMPARE("(#f #t #t #f #f)", result->ToString().c_str());
        result = Lisp::Eval("(do (def a ()) (== a ()))");
        QCOMPARE(true, result->BoolValue());
        result = Lisp::Eval("(do (def a 42) (== a ()))");
        QCOMPARE(false, result->BoolValue());
        result = Lisp::Eval("(do (def a blub) (def b nix) (== a b))");
        QCOMPARE(false, result->BoolValue());
        result = Lisp::Eval("(do (def a blub) (def b blub) (== a b))");
        QCOMPARE(true, result->BoolValue());
        result = Lisp::Eval("(do (def a blub) (def b blub) (== a b))");
        QCOMPARE(true, result->BoolValue());
        result = Lisp::Eval("(do (def a (list 1 2 3)) (def b (list 2 3 4)) (== a b))");
        QCOMPARE(false, result->BoolValue());
        result = Lisp::Eval("(do (def a (list 1 2 3)) (def b (list 1 2 3)) (== a b))");
        QCOMPARE(true, result->BoolValue());
        result = Lisp::Eval("(do (def a (list 1 2 3)) (def b (list 1 (sym 2) 3)) (== a b))");
        QCOMPARE(false, result->BoolValue());
        result = Lisp::Eval("(do (def a blub) (def b nix) (!= a b))");
        QCOMPARE(true, result->BoolValue());
        result = Lisp::Eval("(do (def a 7) (def b 7) (!= a b))");
        QCOMPARE(false, result->BoolValue());
    }

    void Test_CompareOperators2()
    {
        std::shared_ptr<LispVariant> result = Lisp::Eval("(list (< 1 2) (< 4 1) (> 5 2) (> 1 3) (> 4.0 4.0))");
        QCOMPARE("(#t #f #t #f #f)", result->ToString().c_str());
        result = Lisp::Eval("(do (def a \"abc\") (def b \"def\") (< a b))");
        QCOMPARE(true, result->BoolValue());
        result = Lisp::Eval("(do (def a \"abc\") (def b \"abc\") (< a b))");
        QCOMPARE(false, result->BoolValue());
        result = Lisp::Eval("(do (def a \"abc\") (def b \"def\") (<= a b))");
        QCOMPARE(true, result->BoolValue());
        result = Lisp::Eval("(do (def a \"abc\") (def b \"abc\") (<= a b))");
        QCOMPARE(true, result->BoolValue());
    }

    void Test_CompareOperators3()
    {
        std::shared_ptr<LispVariant> result = Lisp::Eval("(list (<= 1 2) (<= 4 1) (>= 5 2) (>= 1 3) (>= 4.0 4.0) (<= 42 42))");
        QCOMPARE("(#t #f #t #f #t #t)", result->ToString().c_str());
    }

    void Test_Not()
    {
        std::shared_ptr<LispVariant> result = Lisp::Eval("(list (! #t) (not #t) (not #f) (! #f))");
        QCOMPARE("(#f #f #t #t)", result->ToString().c_str());
    }

    void Test_Arithmetric1()
    {
        std::shared_ptr<LispVariant> result = Lisp::Eval("(+ 1 2 3 4)");
        QCOMPARE(10, result->ToInt());
    }

    void Test_Arithmetric2()
    {
        std::shared_ptr<LispVariant> result = Lisp::Eval("(+ 1.1 2.2 3.3 4.3)");
        int res = (int)Math_Round(result->ToDouble() * 10.0);
        QCOMPARE(109, res);
    }

    void Test_Arithmetric3()
    {
        std::shared_ptr<LispVariant> result = Lisp::Eval("(* 3 8 2)");
        QCOMPARE(48, result->ToInt());
    }

    void Test_Arithmetric4()
    {
        std::shared_ptr<LispVariant> result = Lisp::Eval("(/ 1.0 2.0)");
        int res = (int)Math_Round(result->ToDouble() * 10.0);
        QCOMPARE(5, res);
    }

    void Test_Arithmetric5()
    {
        std::shared_ptr<LispVariant> result = Lisp::Eval("(/ 10 2)");
        QCOMPARE(5, result->ToInt());
    }

    void Test_Arithmetric6()
    {
        std::shared_ptr<LispVariant> result = Lisp::Eval("(- 42 12 6)");
        QCOMPARE(24, result->ToInt());
    }

    void Test_Arithmetric7()
    {
        std::shared_ptr<LispVariant> result = Lisp::Eval("(- 42.5 0.5)");
        int res = (int)Math_Round(result->ToDouble() * 10.0);
        QCOMPARE(420, res);
    }

    void Test_Arithmetric8()
    {
        std::shared_ptr<LispVariant> result = Lisp::Eval("(sub 42.5 1.5 2.0)");
        int res = (int)Math_Round(result->ToDouble() * 10.0);
        QCOMPARE(390, res);
    }

    void Test_Arithmetric9()
    {
        std::shared_ptr<LispVariant> result = Lisp::Eval("(div 12 3)");
        QCOMPARE(4, result->ToInt());
    }

    void Test_Arithmetric10()
    {
        std::shared_ptr<LispVariant> result = Lisp::Eval("(mul 2 3 4)");
        QCOMPARE(24, result->ToInt());
    }

    void Test_Arithmetric11()
    {
        std::shared_ptr<LispVariant> result = Lisp::Eval("(add 2 3 4)");
        QCOMPARE(9, result->ToInt());
    }

    void Test_MacrosEvaluateNested()
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
            scope->Output->EnableToString(true);
            std::shared_ptr<LispVariant> result = Lisp::Eval(macroExpandScript, scope);
            QCOMPARE("132", result->ToString().c_str());

            string s = scope->Output->GetContent().Trim();
            QVERIFY(s.Contains("first-macro"));
            QVERIFY(s.Contains("second-macro"));
        }
    }

    void Test_MacrosEvaluateRecursive()
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
            var scope = LispEnvironment::CreateDefaultScope();
            scope->Output->EnableToString(true);
            std::shared_ptr<LispVariant> result = Lisp::Eval(macroExpandScript, scope);
            QCOMPARE("144", result->ToString().c_str());

            string s = scope->Output->GetContent().Trim();
            QVERIFY(s.Contains("first-macro"));
            QVERIFY(s.Contains("second-macro"));
        }
    }

    void Test_MacrosEvaluateDoubleMacroCall()
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
            var scope = LispEnvironment::CreateDefaultScope();
            scope->Output->EnableToString(true);
            std::shared_ptr<LispVariant> result = Lisp::Eval(macroExpandScript, scope);
            QCOMPARE("40", result->ToString().c_str());

            string s = scope->Output->GetContent().Trim();
            QVERIFY(s.Contains("first-macro"));
            QVERIFY(s.Contains("second-macro"));
        }
    }

    void Test_MacrosExpand1()
    {
#ifdef ENABLE_COMPILE_TIME_MACROS
        std::shared_ptr<LispVariant> result = Lisp::Eval("(do (define-macro-expand blub (x y) (println x y)) (println (quote (1 2 3))) (blub 3 4))");
        QCOMPARE("3 4", result->ToString().c_str());
#else
        QVERIFY(true);
#endif
    }

    void Test_MacrosExpand2()
    {
        const string macroExpandScript = "(do\
            (define-macro-expand first-macro\
                (a b)\
                (do\
                    (def i 1)\
                    (+ a b i)\
                )\
            )\
\
            (define-macro-expand second-macro\
                (x y)\
                (do\
                    (* x y (first-macro x y))\
                )\
            )\
\
            (def m (second-macro 4 3))\
        )";
#ifdef ENABLE_COMPILE_TIME_MACROS
        std::shared_ptr<LispVariant> result = Lisp::Eval(macroExpandScript);
        QCOMPARE("96", result->ToString().c_str());
#else
        QVERIFY(true);
#endif
    }

    void Test_MacrosExpandDefineStruct()
    {
        const string macroExpandScript = "(do\
(define-macro-eval dotimes (counterinfo statements)\
  (do\
    (def (first 'counterinfo) 0)\
    (while (eval (list < (first 'counterinfo) (eval (nth 1 'counterinfo))))\
      (do\
         (eval 'statements)\
         (setf (rval (first 'counterinfo)) (eval (list + (first 'counterinfo) 1)))\
      )\
    )\
  )\
)\
\
(define-macro-eval defstruct (name) \
(do \
\
  (eval\
     (list 'defn (sym (+ \"make-\" name)) (cdr (quoted-macro-args)) \
              `(list ,(sym (+ \"#\" name)) ,@(cdr(quoted-macro-args))) \
     )\
  )\
\
  (eval\
     (list 'defn (sym (+ \"is-\" name \"-p\")) '(data)	\
        `(and(== (type data) 6) (== (first data) ,(sym (+ \"#\" name)))) \
     ) \
  )\
\
  (dotimes (i (- (len (quoted-macro-args)) 1)) \
    (eval\
          (list 'defn (sym (+ \"get-\" name \"-\" (str (nth (+ i 1) (quoted-macro-args))))) '(data) \
             `(nth (+ ,i 1) data) \
        )\
    )\
  )\
)\
)\
\
(defstruct point x y)\
(def p (make-point 2 3))\
)";
#ifdef ENABLE_COMPILE_TIME_MACROS
                {
                    std::shared_ptr<LispVariant> result = Lisp::Eval(macroExpandScript);
                    QCOMPARE("(#point 2 3)", result->ToString().c_str());
                }
#else
                {
                    QVERIFY(true);
                }
#endif
    }

    void Test_MacrosExpandNested()
    {
        const string macroExpandScript = "(do\
            (define-macro-expand first-macro\
                (a b)\
                (do\
                    (println first-macro)\
                    (def i 1)\
                    (+ a b i)\
                )\
            )\
\
            (define-macro-expand second-macro\
                (x y)\
                (do\
                    (println second-macro)\
                        (* x y (first-macro (+ x 1) (+ y 2)))\
                    )\
                )\
\
            (def m (second-macro 4 3))\
        )";

#ifdef ENABLE_COMPILE_TIME_MACROS
        //using (ConsoleRedirector cr = new ConsoleRedirector())
        {
            var scope = LispEnvironment::CreateDefaultScope();
            scope->Output->EnableToString(true);
            std::shared_ptr<LispVariant> result = Lisp::Eval(macroExpandScript, scope);
            QCOMPARE("132", result->ToString().c_str());

            string s = scope->Output->GetContent().Trim();
            QVERIFY(s.Contains("first-macro"));
            QVERIFY(s.Contains("second-macro"));
        }
#else
        QVERIFY(true);
#endif
    }

    void Test_MacrosExpandRecursive()
    {
        const string macroExpandScript = "(do\
            (define-macro-expand first-macro\
                (a b)\
                (do\
                    (println first-macro)\
                    (def i 1)\
                    (+ a b i)\
                )\
            )\
\
            (define-macro-expand second-macro\
                (x y)\
                (do\
                    (println second-macro)\
                    (* x y (first-macro x (+ y 4)))\
                )\
            )\
\
            (def m (second-macro 4 3))\
        )";

#ifdef ENABLE_COMPILE_TIME_MACROS
            //using (ConsoleRedirector cr = new ConsoleRedirector())
            {
                var scope = LispEnvironment::CreateDefaultScope();
                scope->Output->EnableToString(true);
                std::shared_ptr<LispVariant> result = Lisp::Eval(macroExpandScript, scope);
                QCOMPARE("144", result->ToString().c_str());

                string s = scope->Output->GetContent().Trim();
                QVERIFY(s.Contains("first-macro"));
                QVERIFY(s.Contains("second-macro"));
            }
#else
            QVERIFY(true);
#endif
    }

    void Test_MacrosExpandDoubleMacroCall()
    {
        const string macroExpandScript = "(do\
            (define-macro-expand first-macro\
                (a b)\
                (do\
                    (println first-macro)\
                    (def i 1)\
                    (+ a b i)\
                )\
            )\
\
            (define-macro-expand second-macro\
                (x y)\
                (do\
                    (println second-macro)\
                    (* x y)\
                )\
            )\
\
            (def m (second-macro 4 (first-macro 6 3)))\
        )";

#ifdef ENABLE_COMPILE_TIME_MACROS
        //using (ConsoleRedirector cr = new ConsoleRedirector())
        {
            var scope = LispEnvironment::CreateDefaultScope();
            scope->Output->EnableToString(true);
            std::shared_ptr<LispVariant> result = Lisp::Eval(macroExpandScript, scope);
            QCOMPARE("40", result->ToString().c_str());

            string s = scope->Output->GetContent().Trim();
            QVERIFY(s.Contains("first-macro"));
            QVERIFY(s.Contains("second-macro"));
        }
#else
        QVERIFY(true);
#endif
    }

    void Test_MacrosSetf1()
    {
#ifdef ENABLE_COMPILE_TIME_MACROS
        {
            var scope = LispEnvironment::CreateDefaultScope();
            scope->Output->EnableToString(true);
            std::shared_ptr<LispVariant> result =
                Lisp::Eval(
                    "(do (def a 42) (define-macro-expand my-setf (x value) (setf x value)) (my-setf a (+ \"blub\" \"xyz\")) (println a))", scope);
            QCOMPARE("blubxyz", result->ToString().c_str());
        }
#else
        QVERIFY(true);
#endif
    }

    void Test_MacrosSetf2()
    {
        std::shared_ptr<LispVariant> result = Lisp::Eval("(do (def a 42) (defn my-setf (x value) (setf x value)) (my-setf a (+ 8 9)) (println a))");
        QCOMPARE(42, result->ToInt());
    }

    void Test_MacrosSetf3()
    {
        std::shared_ptr<LispVariant> result = Lisp::Eval("(do (def a 42) (define-macro-eval my-setf (x value) (setf x value)) (my-setf a (+ \"blub\" \"xyz\")) (println a))");
        QCOMPARE("blubxyz", result->ToString().c_str());
    }

    void Test_Quasiquote1()
    {
        std::shared_ptr<LispVariant> result = Lisp::Eval("(do (def a '(42 99 102 \"hello\")) (def b 55) (println (type a)) (println (nth 3 `(1 2 3 ,@a))))");
        QCOMPARE("42", result->ToString().c_str());
    }

    void Test_Quasiquote2()
    {
        std::shared_ptr<LispVariant> result = Lisp::Eval("(do (def a 42) (println `(1 2 3 ,a)))");
        QCOMPARE("(1 2 3 42)", result->ToString().c_str());
    }

    void Test_Quasiquote3()
    {
        std::shared_ptr<LispVariant> result = Lisp::Eval("(do (def a 42) (def lst (list 6 8 12)) (println (quasiquote (1 2 3 ,a ,@lst))))");
        QCOMPARE("(1 2 3 42 6 8 12)", result->ToString().c_str());
    }

    void Test_Quasiquote4()
    {
        std::shared_ptr<LispVariant> result = Lisp::Eval("(do (def a 42) (def lst (list 6 8 12)) (println (quasiquote (1 2 3 ,a ,lst))))");
        QCOMPARE("(1 2 3 42 (6 8 12))", result->ToString().c_str());
    }

    void Test_Quasiquote5()
    {
        std::shared_ptr<LispVariant> result = Lisp::Eval("(do (def a 42) (println (quasiquote (1 2 3 ,(+ 3 a)))))");
        QCOMPARE("(1 2 3 45)", result->ToString().c_str());
    }

    void Test_Quasiquote6()
    {
        std::shared_ptr<LispVariant> result = Lisp::Eval("(do (def a 42) (println (quasiquote (1 2 3 ,@(list 9 8 7 a)))))");
        QCOMPARE("(1 2 3 9 8 7 42)", result->ToString().c_str());
    }

    void Test_Quasiquote7()
    {
        std::shared_ptr<LispVariant> result = Lisp::Eval("(do (def args '(1 2 3)) `,(first args))");
        QCOMPARE("1", result->ToString().c_str());
    }

    void Test_Quasiquote8()
    {
        std::shared_ptr<LispVariant> result = Lisp::Eval("(do (def args '(1 2 3)) `(,(first args)))");
        QCOMPARE("(1)", result->ToString().c_str());
    }

    void Test_Quasiquote9()
    {
        std::shared_ptr<LispVariant> result = Lisp::Eval("(do `(b))");
        QCOMPARE("(b)", result->ToString().c_str());
    }

    void Test_Quasiquote10()
    {
        std::shared_ptr<LispVariant> result = Lisp::Eval("(do `a)");
        QCOMPARE("a", result->ToString().c_str());
    }

    void cleanupTestCase()
    {
        qDebug("CLEANUP fuel unit tests.");
    }
};

QtLispInterpreterUnitTestsTest::QtLispInterpreterUnitTestsTest()
{
}

void QtLispInterpreterUnitTestsTest::testCreateVariant()
{
    LispVariant * variant = new LispVariant(LispType::_Nil);
    QVERIFY(variant != 0);

    delete variant;
    variant = new LispVariant(std::make_shared<object>(3));
    QVERIFY(variant->IsInt());
    QVERIFY(3 == variant->IntValue());

    delete variant;
    variant = new LispVariant(std::make_shared<object>(3.1415));
    QVERIFY(variant->IsDouble());
    QVERIFY(3.1415 == variant->DoubleValue());

    delete variant;
    variant = new LispVariant(std::make_shared<object>(string("text")));
    QVERIFY(variant->IsString());

    delete variant;
    variant = new LispVariant(std::make_shared<object>("blub"));
    QVERIFY(variant->IsString());
    QVERIFY(string("blub") == variant->ToString());
}

void QtLispInterpreterUnitTestsTest::testVariantCompare()
{
    std::shared_ptr<LispVariant> variant1 = std::make_shared<LispVariant>(LispVariant(std::make_shared<object>(4.3)));
    std::shared_ptr<LispVariant> variant2 = std::make_shared<LispVariant>(LispVariant(std::make_shared<object>(56.1)));
    std::shared_ptr<LispVariant> variant3 = std::make_shared<LispVariant>(LispVariant(std::make_shared<object>(42)));
    std::shared_ptr<LispVariant> variant4 = std::make_shared<LispVariant>(LispVariant(std::make_shared<object>(string("abc"))));
    QVERIFY(variant1->CompareTo(variant2) < 0);
    QVERIFY(variant2->CompareTo(variant1) > 0);
    QVERIFY(variant1->CompareTo(std::make_shared<object>(1.23)) > 0);
    QVERIFY(variant1->CompareTo(std::make_shared<object>(-5)) > 0);
    QVERIFY(variant3->CompareTo(std::make_shared<object>(42)) == 0);
    QVERIFY(variant4->CompareTo(std::make_shared<object>("abc")) == 0);
    QVERIFY(variant4->CompareTo(std::make_shared<object>("xyz")) < 0);
}

void QtLispInterpreterUnitTestsTest::testVariantConvert()
{
    std::shared_ptr<LispVariant> variant1 = std::make_shared<LispVariant>(LispVariant(std::make_shared<object>(4.3)));
    std::shared_ptr<LispVariant> variant2 = std::make_shared<LispVariant>(LispVariant(std::make_shared<object>(56.1)));
    std::shared_ptr<LispVariant> variant3 = std::make_shared<LispVariant>(LispVariant(std::make_shared<object>(42)));
    std::shared_ptr<LispVariant> variant4 = std::make_shared<LispVariant>(LispVariant(std::make_shared<object>("4.5")));
    std::shared_ptr<LispVariant> variant5 = std::make_shared<LispVariant>(LispVariant(std::make_shared<object>(true)));
    std::shared_ptr<LispVariant> variant6 = std::make_shared<LispVariant>(LispVariant(LispType::_Int, std::make_shared<object>(0)));
    QVERIFY(true == variant1->ToBool());
    QVERIFY(true == variant3->ToBool());
    QVERIFY(false == variant6->ToBool());
    QVERIFY(4.5 == variant4->ToDouble());
    QVERIFY(1.0 == variant5->ToDouble());
    QVERIFY(56 == variant2->ToInt());
    QVERIFY(true == variant2->ToBool());
}

void QtLispInterpreterUnitTestsTest::testVariantOperations()
{
    std::shared_ptr<LispVariant> variant1 = std::make_shared<LispVariant>(LispVariant(std::make_shared<object>(4.3)));
    std::shared_ptr<LispVariant> variant2 = std::make_shared<LispVariant>(LispVariant(std::make_shared<object>(56.1)));
    std::shared_ptr<LispVariant> variant3 = std::make_shared<LispVariant>(LispVariant(std::make_shared<object>(42)));
    std::shared_ptr<LispVariant> variant4 = std::make_shared<LispVariant>(LispVariant(std::make_shared<object>(45)));
    QVERIFY(1890 == (*variant3 * *variant4).ToInt());
    QVERIFY(60.4 == (*variant1 + *variant2).ToDouble());
}

void QtLispInterpreterUnitTestsTest::testVariantEqualOp()
{
    std::shared_ptr<LispVariant> variant1 = std::make_shared<LispVariant>(LispVariant(std::make_shared<object>(4.3)));
    std::shared_ptr<LispVariant> variant2 = std::make_shared<LispVariant>(LispVariant(std::make_shared<object>(56.1)));
    QVERIFY(!LispVariant::EqualOp(*variant1, *variant2));
    QVERIFY(LispVariant::EqualOp(*variant1, *variant1));
}

void QtLispInterpreterUnitTestsTest::testVariantCastError()
{
    try
    {
        std::shared_ptr<LispVariant> variant = std::make_shared<LispVariant>(LispVariant(std::make_shared<object>(4.3)));
        //Assert::IsNotNull(variant);
        int value = variant->IntValue();
        QVERIFY(4 == value);      // will not be evaluated because of expected exception !
        QVERIFY(false);
    }
    catch (LispException)
    {
        QVERIFY(true);
    }
}

void QtLispInterpreterUnitTestsTest::testStringIndexOf()
{
    CppLisp::string target("abc def blub 123");

    QVERIFY((size_t)4 == target.IndexOf("def", ""));
    ///*size_t st =*/ target.IndexOf("test", "");
    QVERIFY(std::string::npos == target.IndexOf("test", "nix"));
}

QTEST_APPLESS_MAIN(QtLispInterpreterUnitTestsTest)

#include "tst_qtlispinterpreterunitteststest.moc"
