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
        aCurrentDir.mkpath("./Library");
        QFile::copy(":/fuellib.fuel", "./Library/fuellib.fuel");
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
            std::shared_ptr<LispVariant> result = Lisp::Eval("(do (def a 42) (define-macro-expand my-setf (x value) (setf x value)) (my-setf a (+ \"blub\" \"xyz\")) (println a))", scope);
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

    void Test_Quote1()
    {
        std::shared_ptr<LispVariant> result = Lisp::Eval("(do (def x 42) (println 'x))");
        QCOMPARE("x", result->ToString().c_str());
    }

    void Test_Quote2()
    {
        std::shared_ptr<LispVariant> result = Lisp::Eval("(do 'x)");
        QCOMPARE("x", result->ToString().c_str());
    }

    void Test_Quote3()
    {
        std::shared_ptr<LispVariant> result = Lisp::Eval("(do '(a b 6 x))");
        QCOMPARE("(a b 6 x)", result->ToString().c_str());
    }

    void Test_EvalQuasiQuote1()
    {
        std::shared_ptr<LispVariant> result = Lisp::Eval("(do (def a 4) (def lst '(a 2 3)) (eval `,(first lst)))");
        QCOMPARE("4", result->ToString().c_str());
    }

    void Test_String1()
    {
        std::shared_ptr<LispVariant> result = Lisp::Eval("(println \"hello \\\\ \\' öäü \n \\\"blub\\\"\")");
        QCOMPARE("hello \\ ' öäü \n \"blub\"", result->ToString().c_str());
    }

    void Test_String2()
    {
        std::shared_ptr<LispVariant>  result = Lisp::Eval("(string \"hello\" \"-\" \"world\")");
        QCOMPARE("hello-world", result->ToString().c_str());
    }

    void Test_Map()
    {
        std::shared_ptr<LispVariant> result = Lisp::Eval("(map (lambda (x) (+ x 1)) '(1 2 3))");
        QCOMPARE("(2 3 4)", result->ToString().c_str());
    }

    void Test_Reduce1()
    {
        std::shared_ptr<LispVariant> result = Lisp::Eval("(reduce (lambda (x y) (+ x y)) '(1 2 3) 0)");
        QCOMPARE(6, result->ToInt());
    }

    void Test_Reduce2()
    {
        std::shared_ptr<LispVariant> result = Lisp::Eval("(reduce (lambda (x y) (* x y)) '(2 3 4 5) 2)");
        QCOMPARE(240, result->ToInt());
    }

    void Test_Closure1()
    {
        std::shared_ptr<LispVariant> result = Lisp::Eval("(do (defn addx (delta) (lambda (x) (+ x delta))) (def addclosure (addx 41)) (println (addclosure 1)))");
        QCOMPARE(42, result->ToInt());
    }

    void Test_Closure2()
    {
        std::shared_ptr<LispVariant> result = Lisp::Eval("(do (defn g (x) (do (+ x 2))) (defn f (x) (do (def i 7) (+ x 1 i (g 2)))) (println (f 1)))");
        QCOMPARE(13, result->ToInt());
    }

    void Test_Closure3()
    {
        try
        {
            Lisp::Eval("(do (defn g (x) (do (+ x 2 i))) (defn f (x) (do (def i 7) (+ x 1 i (g 2)))) (println (f 1)))");
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

    void Test_Closure4()
    {
// TODO working...
        std::shared_ptr<LispVariant> result = Lisp::Eval("(do (defn g (x) (do (+ x 2))) (defn f (x) (do (def i 7) (+ x 1 i (g 2)))) (println (f 1)))");
        QCOMPARE(13, result->ToInt());
    }

    void Test_RecursiveCall1()
    {
        std::shared_ptr<LispVariant> result = Lisp::Eval("(do (defn addConst (x a) (+ x a)) (def add2 (lambda (x) (addConst x 2))) (println (addConst 8 2)) (println (add2 4)))");
        QCOMPARE(6, result->ToInt());
    }

    void Test_Return1()
    {
        std::shared_ptr<LispVariant> result = Lisp::Eval("(do (defn f (x) (return (+ x x))) (println (f 7)))");
        QCOMPARE(14, result->ToInt());
    }

    void Test_Return2()
    {
        // return statement was not implmented correctly until 25.7.2018...
        std::shared_ptr<LispVariant> result = Lisp::Eval("(do (defn f (x) (do (return (+ x x)) (return 9))) (println (f 7)))");
        QCOMPARE(14, result->ToInt());
    }

    /* TODO --> not implemented yet for C++
    void Test_Call1()
    {
        std::shared_ptr<LispVariant> result = Lisp::Eval("(do (def obj 0) (setf obj (call \"CsLisp.DummyNative\")) (println obj (type obj)) (call obj \"Test\"))");
        QCOMPARE(42, result->ToInt());
    }

    void Test_CallStatic()
    {
        std::shared_ptr<LispVariant> result = Lisp::Eval("(do (call-static \"System.IO.File\" Exists \"dummy\"))");
        QCOMPARE(false, result->ToBool());
    }
*/

    void Test_CallStaticError()
    {
        try
        {
            Lisp::Eval("(do (call-static \"System.IO.File\" NotExistingFunction \"dummy\"))");
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

    void Test_Apply1()
    {
        std::shared_ptr<LispVariant> result = Lisp::Eval("(apply (lambda (x) (println \"hello\" x)) '(55))");
        QCOMPARE("hello 55", result->ToString().c_str());
    }

    void Test_Apply2()
    {
        std::shared_ptr<LispVariant> result = Lisp::Eval("(do (def f (lambda (x) (+ x x))) (apply f '(5)))");
        QCOMPARE(10, result->ToInt());
    }

    void Test_Apply3()
    {
        std::shared_ptr<LispVariant> result = Lisp::Eval("(do (def f '+) (apply f '(5 6 7)))");
        QCOMPARE(18, result->ToInt());
    }

    void Test_Argscount1()
    {
        std::shared_ptr<LispVariant> result = Lisp::Eval("(do (defn f (x) (do (println \"count=\" (argscount)) (+ x x))) (f 5 6 7))");
        QCOMPARE(10, result->ToInt());
    }

    void Test_Arg2()
    {
        //using (ConsoleRedirector cr = new ConsoleRedirector())
        {
            var scope = LispEnvironment::CreateDefaultScope();
            scope->Output->EnableToString(true);
            std::shared_ptr<LispVariant> result = Lisp::Eval("(do (defn f (x) (do (println \"count=\" (argscount)) (println (arg 0)) (println (arg 1)) (println (arg 2)) (println \"additional=\" (nth 1 _additionalArgs)) (+ x x))) (f 5 6 7))", scope);
            QCOMPARE(10, result->ToInt());

            string s = scope->Output->GetContent().Trim();
            QCOMPARE(true, s.Contains("count= 3"));
            QCOMPARE(true, s.Contains("5"));
            QCOMPARE(true, s.Contains("6"));
            QCOMPARE(true, s.Contains("7"));
            QCOMPARE(true, s.Contains("additional= 7"));
        }
    }

    void Test_Arg3()
    {
        try
        {
            std::shared_ptr<LispVariant> result = Lisp::Eval("(do (defn f (x) (do (arg 7) (+ x x))) (f 5 6 7))");
            QCOMPARE(10, result->ToInt());
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

    void Test_Arg4()
    {
        std::shared_ptr<LispVariant> result = Lisp::Eval("(do (defn f () (do (def z (arg 2)) (+ z z))) (f 5 6 7))");
        QCOMPARE(14, result->ToInt());
    }

    void Test_Args1()
    {
        std::shared_ptr<LispVariant> result = Lisp::Eval("(do (defn f () (do (def z (args)))) (f 5 6 7))");
        QCOMPARE("(5 6 7)", result->ToString().c_str());
    }

    void Test_Cons1()
    {
        std::shared_ptr<LispVariant> result = Lisp::Eval("(cons 1 2)");
        QCOMPARE("(1 2)", result->ToString().c_str());
    }

    void Test_Cons2()
    {
        std::shared_ptr<LispVariant> result = Lisp::Eval("(cons 1 '(2 3 4))");
        QCOMPARE("(1 2 3 4)", result->ToString().c_str());
    }

    void Test_Cons3()
    {
        std::shared_ptr<LispVariant> result = Lisp::Eval("(cons 12)");
        QCOMPARE("(12)", result->ToString().c_str());
    }

    void Test_Cons4()
    {
        std::shared_ptr<LispVariant> result = Lisp::Eval("(cons)");
        QCOMPARE("()", result->ToString().c_str());
    }

    void Test_Nop()
    {
        std::shared_ptr<LispVariant> result = Lisp::Eval("(nop)");
        QVERIFY(result->IsUndefined());
    }

    void Test_Symbol()
    {
        std::shared_ptr<LispVariant> result = Lisp::Eval("(sym a)");
        QVERIFY(result->IsSymbol());
        QCOMPARE("a", result->StringValue().c_str());
    }

    void Test_Str()
    {
        std::shared_ptr<LispVariant> result = Lisp::Eval("(str abc)");
        QVERIFY(result->IsString());
        QCOMPARE("abc", result->StringValue().c_str());
    }

    void Test_MapError1()
    {
        try
        {
            Lisp::Eval("(map 4 '(1 2 3))");
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

    void Test_MapError2()
    {
        try
        {
            Lisp::Eval("(map (lambda (x) (+ x 1)) 4)");
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

    void Test_ReduceError1()
    {
        try
        {
            Lisp::Eval("(reduce \"blub\" '(1 2 3) 0)");
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

    void Test_ReduceError2()
    {
        try
        {
            Lisp::Eval("(reduce (lambda (x y) (+ x y))  \"test\" 0)");
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

    void Test_SetfError()
    {
        try
        {
            Lisp::Eval("(setf a 2.0)");
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

    void Test_Parser1()
    {
        try
        {
            Lisp::Eval("(println \"hello\"))");
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

    void Test_Parser2()
    {
        try
        {
            Lisp::Eval("((println \"hello\")");
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

    void Test_Parser3()
    {
        try
        {
            Lisp::Eval("(blub 1 2 3)");
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

    void Test_NotError()
    {
        try
        {
            Lisp::Eval("(not a 2.0)");
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

    void Test_CompareError1()
    {
        try
        {
            Lisp::Eval("(> 2.0)");
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

    void Test_CompareError2()
    {
        try
        {
            Lisp::Eval("(> 2.0 5 234)");
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

    void Test_ScriptToLong()
    {
        try
        {
            Lisp::Eval("(setf a 2.0) asdf");
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

    void Test_DefError()
    {
        try
        {
            Lisp::Eval("(def 1 2)");
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

    void Test_DoError()
    {
        try
        {
            Lisp::Eval("(do (def a 2) blub (setf a 5))");
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

    void Test_IfError()
    {
        try
        {
            Lisp::Eval("(if #t 1 2 3)");
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

    void Test_FunctionNotFound()
    {
        try
        {
            Lisp::Eval("(unknown-fcn 1 2 3)");
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

    void Test_BracketsOutOfBalance1()
    {
        try
        {
            Lisp::Eval("(do (println 2)))");
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

    void Test_BracketsOutOfBalance2()
    {
        try
        {
            Lisp::Eval("(do ( (println 2))");
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

    void Test_UnexpectedToken1()
    {
        try
        {
            Lisp::Eval("blub (do (println 2))");
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

    void Test_UnexpectedTokenButIsBracketsOutOfBalance()
    {
        try
        {
            Lisp::Eval("(do (println 2)) asfd");
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

    void Test_LispVariantToString()
    {
        LispVariant result(std::make_shared<object>("hello"));
        string s = result.ToString();
        QCOMPARE("hello", s.c_str());
    }

    void Test_LispScope1()
    {
        LispScope scope;
        var result = scope.GetPreviousToken(std::make_shared<LispToken>("a", 0, 0, 1));
        QVERIFY(result.get() == 0);
    }

    void Test_LispTokenToString()
    {
        LispToken token("(", 0, 1, 1);
        string s = token.ToString();
        QCOMPARE("(", s.c_str());
    }

    void Test_Type1()
    {
        std::shared_ptr<LispVariant> result = Lisp::Eval("(type 7)");
        QVERIFY(result->IsInt());
        QCOMPARE(3, result->IntValue());
    }

    void Test_Type2()
    {
        std::shared_ptr<LispVariant> result = Lisp::Eval("(type #f)");
        QVERIFY(result->IsInt());
        QCOMPARE(2, result->IntValue());
    }

    void Test_Type3()
    {
        std::shared_ptr<LispVariant> result = Lisp::Eval("(type '(2 3 1))");
        QVERIFY(result->IsInt());
        QCOMPARE(6, result->IntValue());
    }

    void Test_Type4()
    {
        std::shared_ptr<LispVariant> result = Lisp::Eval("(type aSymbol)");
        QVERIFY(result->IsInt());
        QCOMPARE(8, result->IntValue());
    }

    void Test_Type5()
    {
        std::shared_ptr<LispVariant> result = Lisp::Eval("(type \"a string\")");
        QVERIFY(result->IsInt());
        QCOMPARE(5, result->IntValue());
    }

    void Test_TypeStr()
    {
        std::shared_ptr<LispVariant> result = Lisp::Eval("(typestr 1.23)");
        QVERIFY(result->IsString());
        QCOMPARE("Double", result->Value->ToString().c_str());
    }

    void Test_Vars()
    {
        //using (ConsoleRedirector cr = new ConsoleRedirector())
        {
            var scope = LispEnvironment::CreateDefaultScope();
            scope->Output->EnableToString(true);
            std::shared_ptr<LispVariant> result = Lisp::Eval("(do (def a 4) (def b \"asdf\") (vars))", scope);
            QVERIFY(result->IsUndefined());

            string s = scope->Output->GetContent().Trim();
            QCOMPARE(true, s.Contains("a --> 4"));
            QCOMPARE(true, s.Contains("b --> \"asdf\""));
        }
    }

    void Test_Fuel()
    {
        std::shared_ptr<LispVariant> result = Lisp::Eval("(fuel)");
        QVERIFY(result->IsString());
        QVERIFY(result->StringValue().Contains("fuel version"));
    }

    void Test_Copyright()
    {
        std::shared_ptr<LispVariant> result = Lisp::Eval("(copyright)");
        QVERIFY(result->IsString());
        QVERIFY(result->StringValue().Contains("Copyright: MIT-License"));
    }

    void Test_Help()
    {
        std::shared_ptr<LispVariant> result = Lisp::Eval("(help)");
        QVERIFY(result->IsString());
        QVERIFY(result->StringValue().Contains("available functions:"));
    }

    void Test_Import()
    //[DeploymentItem(@"..\..\..\Library\fuellib.fuel", "Library")]
    {
        //using (ConsoleRedirector cr = new ConsoleRedirector())
        {
            var scope = LispEnvironment::CreateDefaultScope();
            scope->Output->EnableToString(true);
            std::shared_ptr<LispVariant> result = Lisp::Eval("(do (import \"Library\\\\fuellib.fuel\") (foreach '(1 5 7) (lambda (x) (println x))))", scope);
            QVERIFY(result->IsInt());
            QCOMPARE(3, result->IntValue());

            string s = scope->Output->GetContent().Trim();
            QVERIFY(s.Contains("1"));
            QVERIFY(s.Contains("7"));
            QVERIFY(s.Contains("7"));
        }
    }

    void Test_Import2()
    //[DeploymentItem(@"..\..\..\Library\fuellib.fuel", "Library")]
    {
        //using (ConsoleRedirector cr = new ConsoleRedirector())
        {
            var scope = LispEnvironment::CreateDefaultScope();
            scope->Output->EnableToString(true);
            std::shared_ptr<LispVariant> result = Lisp::Eval("(do (import fuellib) (foreach '(1 4 6) (lambda (x) (println x))))", scope);
            QVERIFY(result->IsInt());
            QCOMPARE(3, result->IntValue());    // is last value of internal loop variable in foreach

            // test results
            string s = scope->Output->GetContent().Trim();
            QVERIFY(s.Contains("1"));
            QVERIFY(s.Contains("4"));
            QVERIFY(s.Contains("6"));
        }
    }

    void Test_BadForeach()
    {
        std::shared_ptr<LispVariant> result = Lisp::Eval("(do (import fuellib) (foreach))");
        QVERIFY(result->IsUndefined());
    }

    void Test_Break()
    {
        //using (ConsoleRedirector cr = new ConsoleRedirector())
        {
            var scope = LispEnvironment::CreateDefaultScope();
            scope->Output->EnableToString(true);
            std::shared_ptr<LispVariant> result = Lisp::Eval("(break)", scope);
            QVERIFY(result->IsUndefined());

            string s = scope->Output->GetContent().Trim();
            QVERIFY(s.Contains("no debugger support"));
        }
    }

    void Test_ReadLine()
    {
        //using (ConsoleRedirector cr = new ConsoleRedirector("some input\nmore input\n"))
        {
            var scope = LispEnvironment::CreateDefaultScope();
            scope->Output->EnableToString(true);
            scope->Input->EnableFromString(true);
            scope->Input->SetContent("some input\nmore input\n");
            std::shared_ptr<LispVariant> result = Lisp::Eval("(do (def a (readline)) (println a) (def b (readline)) (println b))", scope);
            QVERIFY(result->IsString());
            QCOMPARE("more input", result->ToString().c_str());

            string s = scope->Output->GetContent().Trim();
            QVERIFY(s.Contains("some input"));
            QVERIFY(s.Contains("more input"));
        }
    }

    void Test_ReadLineWithArgs()
    {
        //using (ConsoleRedirector cr = new ConsoleRedirector("some input\nmore input\n"))
        {
            try
            {
                var scope = LispEnvironment::CreateDefaultScope();
                scope->Output->EnableToString(true);
                std::shared_ptr<LispVariant> result = Lisp::Eval("(do (def a (readline 1)) (println a)", scope);
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
    }

    void Test_ParseInteger()
    {
        std::shared_ptr<LispVariant> result = Lisp::Eval("(do (def s \"42\") (def i (parse-integer s)))");
        QVERIFY(result->IsInt());
        QCOMPARE(42, result->IntValue());
    }

    void Test_ParseIntegerError()
    {
        std::shared_ptr<LispVariant> result = Lisp::Eval("(do (def s \"nonumber\") (def i (parse-integer s)))");
        QVERIFY(result->IsUndefined());
    }

    void Test_ParseFloat()
    {
        std::shared_ptr<LispVariant> result = Lisp::Eval("(do (def s \"42.789\") (def f (parse-float s)))");
        QVERIFY(result->IsDouble());
        QCOMPARE(42.789, result->DoubleValue());
    }

    void Test_ParseFloatError()
    {
        std::shared_ptr<LispVariant> result = Lisp::Eval("(do (def s \"nonumber\") (def f (parse-float s)))");
        QVERIFY(result->IsUndefined());
    }

    void Test_Slice1()
    {
        std::shared_ptr<LispVariant> result = Lisp::Eval("(do (def s \"this is a string\") (slice s 0 4))");
        QVERIFY(result->IsString());
        QCOMPARE("this", result->StringValue().c_str());
    }

    void Test_Slice2()
    {
        std::shared_ptr<LispVariant> result = Lisp::Eval("(do (def s \"this is a string\") (slice s 8 -1))");
        QVERIFY(result->IsString());
        QCOMPARE("a string", result->StringValue().c_str());
    }

    void Test_Slice3()
    {
        std::shared_ptr<LispVariant> result = Lisp::Eval("(do (def s \"this is a string\") (slice s 5 4))");
        QVERIFY(result->IsString());
        QCOMPARE("is a", result->StringValue().c_str());
    }

    void Test_SliceError()
    {
        try
        {
            std::shared_ptr<LispVariant> result = Lisp::Eval("(do (def s \"this is a string\") (slice s))");
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

    void Test_FirstForString()
    {
        std::shared_ptr<LispVariant> result = Lisp::Eval("(do (def s \"this is a string\") (first s))");
        QVERIFY(result->IsString());
        QCOMPARE("t", result->StringValue().c_str());
    }

    void Test_LastForString()
    {
        std::shared_ptr<LispVariant> result = Lisp::Eval("(do (def s \"this is a string\") (last s))");
        QVERIFY(result->IsString());
        QCOMPARE("g", result->StringValue().c_str());
    }

    void Test_RestForString()
    {
        std::shared_ptr<LispVariant> result = Lisp::Eval("(do (def s \"this is a string\") (rest s))");
        QVERIFY(result->IsString());
        QCOMPARE("his is a string", result->StringValue().c_str());
    }

    void Test_NthForString()
    {
        std::shared_ptr<LispVariant> result = Lisp::Eval("(do (def s \"this is a string\") (nth 5 s))");
        QVERIFY(result->IsString());
        QCOMPARE("i", result->StringValue().c_str());
    }

    void Test_LenForString()
    {
        std::shared_ptr<LispVariant> result = Lisp::Eval("(do (def s \"this is a string\") (len s))");
        QVERIFY(result->IsInt());
        QCOMPARE(16, result->IntValue());
    }

    void Test_Trim()
    {
        std::shared_ptr<LispVariant> result = Lisp::Eval("(do (def s \" \t   this is a string  \") (trim s))");
        QVERIFY(result->IsString());
        QCOMPARE("this is a string", result->StringValue().c_str());
    }

    void Test_LowerCase()
    {
        std::shared_ptr<LispVariant> result = Lisp::Eval("(do (def s \"THIS is A stRing\") (lower-case s))");
        QVERIFY(result->IsString());
        QCOMPARE("this is a string", result->StringValue().c_str());
    }

    void Test_UpperCase()
    {
        std::shared_ptr<LispVariant> result = Lisp::Eval("(do (def s \"THIS is A stRing 8 ,.!?\") (upper-case s))");
        QVERIFY(result->IsString());
        QCOMPARE("THIS IS A STRING 8 ,.!?", result->StringValue().c_str());
    }

    void Test_Find1()
    {
        std::shared_ptr<LispVariant> result = Lisp::Eval("(do (import \"Library\\\\fuellib.fuel\") (def l '(1 5 7)) (find 5 l))");
        QVERIFY(result->IsInt());
        QCOMPARE(1, result->IntValue());
    }

    void Test_Find2()
    {
        std::shared_ptr<LispVariant> result = Lisp::Eval("(do (import \"Library\\\\fuellib.fuel\") (def l '(1 5 7)) (find 9 l))");
        QVERIFY(result->IsInt());
        QCOMPARE(-1, result->IntValue());
    }

    void Test_DoTimes1()
    {
        std::shared_ptr<LispVariant> result = Lisp::Eval("(do (import fuellib) (def l '()) (dotimes (ix 7) (setf l (cons ix l))) (println l))");
        QVERIFY(result->IsString());
        QCOMPARE("(6 5 4 3 2 1 0)", result->StringValue().c_str());
    }

    void Test_DoTimes2()
    {
        std::shared_ptr<LispVariant> result = Lisp::Eval("(do (import fuellib) (def l '()) (dotimes (i 7) (setf l (cons i l))) (dotimes (i 9) (setf l (cons i l))) (println l))");
        QVERIFY(result->IsString());
        QCOMPARE("(8 7 6 5 4 3 2 1 0 6 5 4 3 2 1 0)", result->StringValue().c_str());
    }

    void Test_Reverse()
    {
        std::shared_ptr<LispVariant> result = Lisp::Eval("(do (def l (list 1 2 b \"nix\" 4.5)) (print (reverse l)))");
        QVERIFY(result->IsString());
        QCOMPARE("(4.500000 \"nix\" b 2 1)", result->StringValue().c_str());
    }

    void Test_ReverseString()
    {
        std::shared_ptr<LispVariant> result = Lisp::Eval("(do (def s \"this is text\") (print (reverse s)))");
        QVERIFY(result->IsString());
        QCOMPARE("txet si siht", result->StringValue().c_str());
    }

    void Test_Search1()
    {
        std::shared_ptr<LispVariant> result = Lisp::Eval("(do (def s \"this is text\") (print (search \"tex\" s)))");
        QVERIFY(result->IsString());
        QCOMPARE("8", result->StringValue().c_str());
    }

    void Test_Search2()
    {
        std::shared_ptr<LispVariant> result = Lisp::Eval("(do (def s \"this is text\") (search \"tes\" s))");
        QVERIFY(result->IsInt());
        QCOMPARE(-1, result->IntValue());
    }

    void Test_Search3()
    {
        std::shared_ptr<LispVariant> result = Lisp::Eval("(do (def l '(a b 4 d 5.234 \"blub\")) (search b l))");
        QVERIFY(result->IsInt());
        QCOMPARE(1, result->IntValue());
    }

    void Test_Search4()
    {
        std::shared_ptr<LispVariant> result = Lisp::Eval("(do (def l '(a b 4 d 5.234 \"blub\")) (search 4 l))");
        QVERIFY(result->IsInt());
        QCOMPARE(2, result->IntValue());
    }

    void Test_Search5()
    {
        std::shared_ptr<LispVariant> result = Lisp::Eval("(do (def l '(a b 4 d 5.234 \"blub\")) (search \"blub\" l))");
        QVERIFY(result->IsInt());
        QCOMPARE(5, result->IntValue());
    }

    void Test_Search6()
    {
        std::shared_ptr<LispVariant> result = Lisp::Eval("(do (def l '(a b 4 d 5.234 \"blub\")) (search nix l))");
        QVERIFY(result->IsInt());
        QCOMPARE(-1, result->IntValue());
    }

    void Test_Search7()
    {
        try
        {
            std::shared_ptr<LispVariant> result = Lisp::Eval("(do (def l 5.234) (search nix l))");
            QVERIFY(result->IsInt());
            QCOMPARE(-1, result->IntValue());
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

    void Test_Replace1()
    {
        std::shared_ptr<LispVariant> result = Lisp::Eval("(do (def s \"this is a long text\") (replace s \"long\" \"short\"))");
        QVERIFY(result->IsString());
        QCOMPARE("this is a short text", result->StringValue().c_str());
    }

    void Test_Replace2()
    {
        std::shared_ptr<LispVariant> result = Lisp::Eval("(do (def s \"this is a long long text\") (replace s \"long\" \"short\"))");
        QVERIFY(result->IsString());
        QCOMPARE("this is a short short text", result->StringValue().c_str());
    }

    void Test_Replace3()
    {
        std::shared_ptr<LispVariant> result = Lisp::Eval("(do (def s \"this is a long text\") (replace s \"verylong\" \"short\"))");
        QVERIFY(result->IsString());
        QCOMPARE("this is a long text", result->StringValue().c_str());
    }

    void Test_Replace4()
    {
        std::shared_ptr<LispVariant> result = Lisp::Eval("(do (def s \"this is a long text with a lot of words a\") (replace s \"a \" \"an \"))");
        QVERIFY(result->IsString());
        QCOMPARE("this is an long text with an lot of words a", result->StringValue().c_str());
    }

    void Test_UnaryMinusInt()
    {
        std::shared_ptr<LispVariant> result = Lisp::Eval("(do (def a 8) (- a))");
        QVERIFY(result->IsInt());
        QCOMPARE(-8, result->IntValue());
    }

    void Test_UnaryMinusDouble()
    {
        std::shared_ptr<LispVariant> result = Lisp::Eval("(do (def a 1.234) (- a))");
        QVERIFY(result->IsDouble());
        QCOMPARE(-1.234, result->DoubleValue());
    }

    void Test_UnaryMinusString()
    {
        try
        {
            std::shared_ptr<LispVariant> result = Lisp::Eval("(do (def a \"nix\") (- a))");
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

    void Test_ModuloInt()
    {
        std::shared_ptr<LispVariant> result = Lisp::Eval("(do (% 7 3))");
        QVERIFY(result->IsInt());
        QCOMPARE(1, result->IntValue());
    }

    void Test_ModuloDouble()
    {
        std::shared_ptr<LispVariant> result = Lisp::Eval("(do (% 7.4 2.8))");
        QVERIFY(result->IsDouble());
        QCOMPARE("1.800000", std::to_string(result->DoubleValue()).c_str());
    }

    void Test_NotEnoughFunctionArguments()
    {
        std::shared_ptr<LispVariant> result = Lisp::Eval("(do (defn f (x y z) (add (str x) (str y) (str z))) (f 3))");
        QVERIFY(result->IsString());
        QCOMPARE("3NILNIL", result->StringValue().c_str());
    }

    void Test_IntConversion1()
    {
        std::shared_ptr<LispVariant> result = Lisp::Eval("(do (int 7.4))");
        QVERIFY(result->IsInt());
        QCOMPARE(7, result->IntValue());
    }

    void Test_IntConversion2()
    {
        std::shared_ptr<LispVariant> result = Lisp::Eval("(do (int \"61.234\"))");
        QVERIFY(result->IsUndefined());
    }

    void Test_IntConversion3()
    {
        std::shared_ptr<LispVariant> result = Lisp::Eval("(do (int #t))");
        QVERIFY(result->IsInt());
        QCOMPARE(1, result->IntValue());
    }

    void Test_IntConversion4()
    {
        std::shared_ptr<LispVariant> result = Lisp::Eval("(do (int #f))");
        QVERIFY(result->IsInt());
        QCOMPARE(0, result->IntValue());
    }

    void Test_IntConversion5()
    {
        std::shared_ptr<LispVariant> result = Lisp::Eval("(do (int \"a text\"))");
        QVERIFY(result->IsUndefined());
    }

    void Test_FloatConversion1()
    {
        std::shared_ptr<LispVariant> result = Lisp::Eval("(do (float 7))");
        QVERIFY(result->IsDouble());
        QCOMPARE(7.0, result->DoubleValue());
    }

    void Test_FloatConversion2()
    {
        std::shared_ptr<LispVariant> result = Lisp::Eval("(do (float \"61.234\"))");
        QVERIFY(result->IsDouble());
        QCOMPARE(61.234, result->DoubleValue());
    }

    void Test_FloatConversion3()
    {
        std::shared_ptr<LispVariant> result = Lisp::Eval("(do (float #t))");
        QVERIFY(result->IsDouble());
        QCOMPARE(1.0, result->DoubleValue());
    }

    void Test_FloatConversion4()
    {
        std::shared_ptr<LispVariant> result = Lisp::Eval("(do (float #f))");
        QVERIFY(result->IsDouble());
        QCOMPARE(0.0, result->DoubleValue());
    }

    void Test_FloatConversion5()
    {
        std::shared_ptr<LispVariant> result = Lisp::Eval("(do (float \"a text\"))");
        QVERIFY(result->IsUndefined());
    }

    void Test_DelVar1()
    {
        std::shared_ptr<LispVariant> result = Lisp::Eval("(do (def a 8) (delvar 'a))");
        QVERIFY(result->IsBool());
        QCOMPARE(true, result->ToBool());
    }

    void Test_DelVar2()
    {
        std::shared_ptr<LispVariant> result = Lisp::Eval("(do (def a 8) (delvar 'b))");
        QVERIFY(result->IsBool());
        QCOMPARE(false, result->ToBool());
    }

    void Test_NeedLValue1()
    {
        std::shared_ptr<LispVariant> result = Lisp::Eval("(do (def a 7) (defn test () (do (println (need-l-value)) (return 'a))) (setf (test) 8) (println a))");
        QVERIFY(result->IsString());
        QCOMPARE("8", result->ToString().c_str());
    }

    void Test_NeedLValue2()
    {
        std::shared_ptr<LispVariant> result = Lisp::Eval("(do (def a 7) (defn test () (need-l-value)) (println (test)))");
        QVERIFY(result->IsString());
        QCOMPARE("#f", result->ToString().c_str());
    }

    // TODO / NOT IMPLEMENTED:
    // Test_CreateNative
    // Test_RegisterNativeObjects
    // Test_StdLibArray
    // Test_StdLibDictionary
    // Test_StdLibFile
    // Test_StdLibList
    // Test_StdLibListSort
    // Test_StdLibMath
    // Test_StdLibMath2

    // TODO --> open points:
    // compare functions in lisp available ? --> (== f g) where f and g are functions: (defn f (x) (+ x x))

    // ***************************************************

    void Test_SingleParserEmptyCode()
    {
        std::shared_ptr<object> result = LispParser::Parse("()");
        QVERIFY(result.get() != 0);
        QVERIFY(result->IsList());
        QCOMPARE((size_t)0, result->ToList()->size());
    }

    void Test_SingleParser1()
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

    void Test_SingleParser2()
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

    // ***************************************************

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
