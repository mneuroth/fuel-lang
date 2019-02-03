#include <QString>
#include <QtTest>

#include "../CppLispInterpreter/Variant.h"
#include "../CppLispInterpreter/Lisp.h"

using namespace CppLisp;

class QtLispInterpreterUnitTestsTest : public QObject
{
    Q_OBJECT

public:
    QtLispInterpreterUnitTestsTest();

private Q_SLOTS:
    void testCreateVariant();
    void testVariantCompare();
    void testVariantConvert();
    void testVariantOperations();
    void testVariantEqualOp();
    void testVariantCastError();
    void testStringIndexOf();
    void Test_Comments();
    void Test_DoAndPrint();
    void Test_PrintLnMultilines();
    void Test_PrintTrace();
    void Test_If1();
    void Test_If2();
    void Test_If3();
    void Test_If4();
    void Test_If5();
    void Test_Setf1();
    void Test_SetfWithNth();
    void Test_SetfWithFirst();
    void Test_SetfWithLast();
    void Test_SetfWithMacros();
    void Test_DefstructMacro();
    void Test_QuoteList();
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

void QtLispInterpreterUnitTestsTest::Test_Comments()
{
    std::shared_ptr<LispVariant> result = Lisp::Eval("(do (println \"hello\") ; a comment\n(println; separate lists with comments\n\"world\"));comment in last line");
    QVERIFY(QString("world") == result->ToString().c_str());
}

void QtLispInterpreterUnitTestsTest::Test_DoAndPrint()
{
    std::shared_ptr<LispVariant> result = Lisp::Eval("(do (println \"hello\")\n (println \"world\"))");
    QVERIFY(QString("world") == result->ToString().c_str());
}

void QtLispInterpreterUnitTestsTest::Test_PrintLnMultilines()
{
    std::shared_ptr<LispVariant> result = Lisp::Eval("(do (println \"hello\nworld\"))");
    QVERIFY(QString("hello\nworld") == result->ToString().c_str());
}

void QtLispInterpreterUnitTestsTest::Test_PrintTrace()
{
    std::shared_ptr<LispVariant> result = Lisp::Eval("(do (trace #t) (println \"hello world\") (println (+ 9 8)) (gettrace))");
    QVERIFY(QString("hello world17") == result->ToString().c_str());
}

void QtLispInterpreterUnitTestsTest::Test_If1()
{
    std::shared_ptr<LispVariant> result = Lisp::Eval("(if #t (+ 1 2) (- 3 5))");
    QVERIFY(3 == result->ToInt());
}

void QtLispInterpreterUnitTestsTest::Test_If2()
{
    std::shared_ptr<LispVariant> result = Lisp::Eval("(if #f (* 1 0) (/ 6 3))");
    QVERIFY(2 == result->ToInt());
}

void QtLispInterpreterUnitTestsTest::Test_If3()
{
    std::shared_ptr<LispVariant> result = Lisp::Eval("(if true 1 0)");
    QVERIFY(1 == result->ToInt());
}

void QtLispInterpreterUnitTestsTest::Test_If4()
{
    std::shared_ptr<LispVariant> result = Lisp::Eval("(if false 1 0)");
    QVERIFY(0 == result->ToInt());
}

void QtLispInterpreterUnitTestsTest::Test_If5()
{
    std::shared_ptr<LispVariant> result = Lisp::Eval("(if true 1)");
    QVERIFY(1 == result->ToInt());
    result = Lisp::Eval("(if false 1)");
    QVERIFY(result.get() == 0);
}

void QtLispInterpreterUnitTestsTest::Test_Setf1()
{
    std::shared_ptr<LispVariant> result = Lisp::Eval("(do (def a 1) (def b 1) (setf (first (list 'a 'b 'c)) 9))");
    QVERIFY(9 == result->ToInt());
}

void QtLispInterpreterUnitTestsTest::Test_SetfWithNth()
{
    std::shared_ptr<LispVariant> result = Lisp::Eval("(do (def l '(a b c d)) (setf (nth 2 l) 9) (print l))");
    QVERIFY(QString("(a b 9 d)") == result->ToString().c_str());
}

void QtLispInterpreterUnitTestsTest::Test_SetfWithFirst()
{
    std::shared_ptr<LispVariant> result = Lisp::Eval("(do (def l '(a b c d)) (setf (first l) 21) (print l))");
    QVERIFY(QString("(21 b c d)") == result->ToString().c_str());
}

void QtLispInterpreterUnitTestsTest::Test_SetfWithLast()
{
    std::shared_ptr<LispVariant> result = Lisp::Eval("(do (def l '(a b c d)) (setf (last l) \"xyz\") (print l))");
    QVERIFY(QString("(a b c \"xyz\")") == result->ToString().c_str());
}

void QtLispInterpreterUnitTestsTest::Test_SetfWithMacros()
//[DeploymentItem(@"Library\fuellib.fuel", "Library")]
{
    std::shared_ptr<LispVariant> result = Lisp::Eval("(do (import fuellib) (defstruct point x y) (def p (make-point 12 17)) (setf (get-point-x p) 9) (println p))");
    QVERIFY(result->IsString());
    QVERIFY(QString("(#point 9 17)") == result->ToString().c_str());
}

void QtLispInterpreterUnitTestsTest::Test_DefstructMacro()
//[DeploymentItem(@"Library\fuellib.fuel", "Library")]
{
    std::shared_ptr<LispVariant> result = Lisp::Eval("(do (import fuellib) (defstruct point x y) (def p (make-point 12 17)))");
    QVERIFY(result->IsList());
    QVERIFY(QString("(#point 12 17)") == result->ToString().c_str());
}

void QtLispInterpreterUnitTestsTest::Test_QuoteList()
{
    std::shared_ptr<LispVariant> result = Lisp::Eval("(do (def a 1) (def l '(a b c)))");
    QVERIFY(QString("(a b c)") == result->ToString().c_str());
}

QTEST_APPLESS_MAIN(QtLispInterpreterUnitTestsTest)

#include "tst_qtlispinterpreterunitteststest.moc"
