#include <QString>
#include <QtTest>
#include <QDir>
#include <qtestcase.h>

#include <QDebug>

#include "../CppLispInterpreter/Variant.h"
#include "../CppLispInterpreter/Lisp.h"

using namespace CppLisp;

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
