#include <QString>
#include <QtTest>

#include "../CppLispInterpreter/Variant.h"

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
    size_t st = target.IndexOf("test", "");
    QVERIFY(std::string::npos == target.IndexOf("test", "nix"));
}

QTEST_APPLESS_MAIN(QtLispInterpreterUnitTestsTest)

#include "tst_qtlispinterpreterunitteststest.moc"
