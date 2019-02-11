#include <QString>
#include <QtTest>
#include <QDir>
#include <QtTest/QTest>

#include <QDebug>

#include "tst_qtlisputils.h"

#include "../CppLispInterpreter/Variant.h"
#include "../CppLispInterpreter/Debugger.h"
#include "../CppLispInterpreter/Lisp.h"
#include "../CppLispInterpreter/fuel.h"

using namespace CppLisp;

double Math_Round(double val)
{
    return round(val);
}

string ConvertToLocalDirectorySeperators(const string & path);

class QtLispInterpreterUnitTestsTest : public QObject
{
    Q_OBJECT

public:
    QtLispInterpreterUnitTestsTest()
    {
    }

private Q_SLOTS:

    void initTestCase()
    {
        qDebug("INIT fuel unit tests.");
        QDir aCurrentDir(".");
        aCurrentDir.mkpath("./Library");
        QFile::copy(":/fuellib.fuel", "./Library/fuellib.fuel");
    }

    // *****************************

    TEST_METHOD(Test_CreateVariant)
    {
        LispVariant * variant = new LispVariant(LispType::_Nil);
        QVERIFY(variant != 0);

        delete variant;
        variant = new LispVariant(std::make_shared<object>(3));
        QVERIFY(variant->IsInt());
        QCOMPARE(3, variant->IntValue());

        delete variant;
        variant = new LispVariant(std::make_shared<object>(3.1415));
        QVERIFY(variant->IsDouble());
        QCOMPARE(3.1415, variant->DoubleValue());

        delete variant;
        variant = new LispVariant(std::make_shared<object>(string("text")));
        QVERIFY(variant->IsString());

        delete variant;
        variant = new LispVariant(std::make_shared<object>("blub"));
        QVERIFY(variant->IsString());
        QVERIFY(string("blub") == variant->ToString());
    }

    TEST_METHOD(Test_VariantCompare)
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

    TEST_METHOD(Test_VariantConvert)
    {
        std::shared_ptr<LispVariant> variant1 = std::make_shared<LispVariant>(LispVariant(std::make_shared<object>(4.3)));
        std::shared_ptr<LispVariant> variant2 = std::make_shared<LispVariant>(LispVariant(std::make_shared<object>(56.1)));
        std::shared_ptr<LispVariant> variant3 = std::make_shared<LispVariant>(LispVariant(std::make_shared<object>(42)));
        std::shared_ptr<LispVariant> variant4 = std::make_shared<LispVariant>(LispVariant(std::make_shared<object>("4.5")));
        std::shared_ptr<LispVariant> variant5 = std::make_shared<LispVariant>(LispVariant(std::make_shared<object>(true)));
        std::shared_ptr<LispVariant> variant6 = std::make_shared<LispVariant>(LispVariant(LispType::_Int, std::make_shared<object>(0)));
        QCOMPARE(true, variant1->ToBool());
        QCOMPARE(true, variant3->ToBool());
        QCOMPARE(false, variant6->ToBool());
        QCOMPARE(4.5, variant4->ToDouble());
        QCOMPARE(1.0, variant5->ToDouble());
        QCOMPARE(56, variant2->ToInt());
        QCOMPARE(true, variant2->ToBool());
    }

    TEST_METHOD(Test_VariantOperations)
    {
        std::shared_ptr<LispVariant> variant1 = std::make_shared<LispVariant>(LispVariant(std::make_shared<object>(4.3)));
        std::shared_ptr<LispVariant> variant2 = std::make_shared<LispVariant>(LispVariant(std::make_shared<object>(56.1)));
        std::shared_ptr<LispVariant> variant3 = std::make_shared<LispVariant>(LispVariant(std::make_shared<object>(42)));
        std::shared_ptr<LispVariant> variant4 = std::make_shared<LispVariant>(LispVariant(std::make_shared<object>(45)));
        QCOMPARE(1890, (*variant3 * *variant4).ToInt());
        QCOMPARE(60.4, (*variant1 + *variant2).ToDouble());
    }

    TEST_METHOD(Test_VariantEqualOp)
    {
        std::shared_ptr<LispVariant> variant1 = std::make_shared<LispVariant>(LispVariant(std::make_shared<object>(4.3)));
        std::shared_ptr<LispVariant> variant2 = std::make_shared<LispVariant>(LispVariant(std::make_shared<object>(56.1)));
        QVERIFY(!LispVariant::EqualOp(*variant1, *variant2));
        QVERIFY(LispVariant::EqualOp(*variant1, *variant1));
    }

    TEST_METHOD(Test_VariantCastError)
    {
        try
        {
            std::shared_ptr<LispVariant> variant = std::make_shared<LispVariant>(LispVariant(std::make_shared<object>(4.3)));
            //Assert::IsNotNull(variant);
            int value = variant->IntValue();
            QCOMPARE(4, value);      // will not be evaluated because of expected exception !
            QVERIFY(false);
        }
        catch (LispException)
        {
            QVERIFY(true);
        }
    }

    // *****************************

    TEST_METHOD(Test_TokenizerEmptyCode)
    {
        IEnumerable<std::shared_ptr<LispToken>> result = LispTokenizer::Tokenize("");
        //Assert::IsNotNull(result);
        QCOMPARE((size_t)0, result.Count());

        result = LispTokenizer::Tokenize("     \t \n   ");
        //Assert::IsNotNull(result);
        QCOMPARE((size_t)0, result.Count());
    }

    TEST_METHOD(Test_Tokenizer1)
    {
        IEnumerable<std::shared_ptr<LispToken>> result = LispTokenizer::Tokenize("()");
        //Assert::IsNotNull(result);
        QCOMPARE((size_t)2, result.Count());
        QCOMPARE("(", result.First()->ToString().c_str());
        QCOMPARE(")", result.Last()->ToString().c_str());

        result = LispTokenizer::Tokenize("  (  \n    )  ");
        //Assert::IsNotNull(result);
        QCOMPARE((size_t)2, result.Count());
        QCOMPARE("(", result.First()->ToString().c_str());
        QCOMPARE(")", result.Last()->ToString().c_str());
    }

    TEST_METHOD(Test_Tokenizer2)
    {
        IEnumerable<std::shared_ptr<LispToken>> result = LispTokenizer::Tokenize("(+ 1 #t 3.1415 \"asdf blub\" #f )");
        //Assert::IsNotNull(result);
        QCOMPARE((size_t)8, result.Count());
        var resultAsArray = result.ToArray();
        QCOMPARE("(", resultAsArray[0]->ToString().c_str());
        QVERIFY(LispTokenType::ListStart == resultAsArray[0]->Type);
        QCOMPARE("+", resultAsArray[1]->ToString().c_str());
        QVERIFY(LispTokenType::Symbol == resultAsArray[1]->Type);
        QCOMPARE(1, (int)*(resultAsArray[2]->Value));
        QVERIFY(LispTokenType::Int == resultAsArray[2]->Type);
        QCOMPARE(true, (bool)*(resultAsArray[3]->Value));
        QVERIFY(LispTokenType::True == resultAsArray[3]->Type);
        QCOMPARE(3.1415, (double)*(resultAsArray[4]->Value));
        QVERIFY(LispTokenType::Double == resultAsArray[4]->Type);
        QCOMPARE("asdf blub", resultAsArray[5]->ToString().c_str());
        QVERIFY(LispTokenType::String == resultAsArray[5]->Type);
        /*bool o =*/ (bool)*(resultAsArray[6]->Value);
        QCOMPARE(false, (bool)*(resultAsArray[6]->Value));
        QVERIFY(LispTokenType::False == resultAsArray[6]->Type);
        QCOMPARE(")", resultAsArray[7]->ToString().c_str());
        QVERIFY(LispTokenType::ListEnd == resultAsArray[7]->Type);
    }

    TEST_METHOD(Test_Tokenizer3)
    {
        IEnumerable<std::shared_ptr<LispToken>> result = LispTokenizer::Tokenize("(do (print (* 9 9)))");
        //Assert::IsNotNull(result);
        QCOMPARE((size_t)11, result.Count());
        var resultAsArray = result.ToArray();
        QCOMPARE("(", resultAsArray[0]->ToString().c_str());
        QCOMPARE("do", resultAsArray[1]->ToString().c_str());
        QVERIFY(LispTokenType::Symbol == resultAsArray[1]->Type);
        QCOMPARE("(", resultAsArray[2]->ToString().c_str());
        QCOMPARE("print", resultAsArray[3]->ToString().c_str());
        QCOMPARE("(", resultAsArray[4]->ToString().c_str());
        QCOMPARE("*", resultAsArray[5]->ToString().c_str());
        QCOMPARE(9, (int)*(resultAsArray[6]->Value));
        QCOMPARE(9, (int)*(resultAsArray[7]->Value));
        QCOMPARE(")", resultAsArray[8]->ToString().c_str());
        QCOMPARE(")", resultAsArray[9]->ToString().c_str());
        QCOMPARE(")", resultAsArray[10]->ToString().c_str());
    }

    TEST_METHOD(Test_Tokenizer4)
    {
        IEnumerable<std::shared_ptr<LispToken>> result = LispTokenizer::Tokenize("(do\n (print (* 9 9)) ; this is a comment\n)\n");
        //Assert::IsNotNull(result);
        QCOMPARE((size_t)12, result.Count());
        var resultAsArray = result.ToArray();
        QCOMPARE("(", resultAsArray[0]->ToString().c_str());
        QCOMPARE("do", resultAsArray[1]->ToString().c_str());
        QCOMPARE("(", resultAsArray[2]->ToString().c_str());
        QCOMPARE("print", resultAsArray[3]->ToString().c_str());
        QCOMPARE("(", resultAsArray[4]->ToString().c_str());
        QCOMPARE("*", resultAsArray[5]->ToString().c_str());
        QCOMPARE(9, (int)*(resultAsArray[6]->Value));
        QCOMPARE(9, (int)*(resultAsArray[7]->Value));
        QCOMPARE(")", resultAsArray[8]->ToString().c_str());
        QCOMPARE(")", resultAsArray[9]->ToString().c_str());
        QCOMPARE("; this is a comment\n", resultAsArray[10]->ToString().c_str());
        QVERIFY(LispTokenType::Comment == resultAsArray[10]->Type);
        QCOMPARE(")", resultAsArray[11]->ToString().c_str());
    }

    TEST_METHOD(Test_Tokenizer5)
    {
        IEnumerable<std::shared_ptr<LispToken>> result = LispTokenizer::Tokenize("(test '(1 2 3))");
        //Assert::IsNotNull(result);
        QCOMPARE((size_t)9, result.Count());
        var resultAsArray = result.ToArray();
        QCOMPARE("'", resultAsArray[2]->ToString().c_str());
        QVERIFY(LispTokenType::Quote == resultAsArray[2]->Type);

        result = LispTokenizer::Tokenize("(test `(1 2 3))");
        //Assert::IsNotNull(result);
        QCOMPARE((size_t)9, result.Count());
        resultAsArray = result.ToArray();
        QCOMPARE("`", resultAsArray[2]->ToString().c_str());
        QVERIFY(LispTokenType::QuasiQuote == resultAsArray[2]->Type);

        result = LispTokenizer::Tokenize("(test ,a)");
        //Assert::IsNotNull(result);
        QCOMPARE((size_t)5, result.Count());
        resultAsArray = result.ToArray();
        QCOMPARE(",", resultAsArray[2]->ToString().c_str());
        QVERIFY(LispTokenType::UnQuote == resultAsArray[2]->Type);

        result = LispTokenizer::Tokenize("(test ,@a)");
        //Assert::IsNotNull(result);
        QCOMPARE((size_t)5, result.Count());
        resultAsArray = result.ToArray();
        QCOMPARE(",@", resultAsArray[2]->ToString().c_str());
        QVERIFY(LispTokenType::UnQuoteSplicing == resultAsArray[2]->Type);
    }

    TEST_METHOD(Test_Tokenizer6)
    {
        IEnumerable<std::shared_ptr<LispToken>> result = LispTokenizer::Tokenize("(test nil)");
        //Assert::IsNotNull(result);
        QCOMPARE((size_t)4, result.Count());
        var resultAsArray = result.ToArray();
        QCOMPARE(LispToken::NilConst.c_str(), resultAsArray[2]->ToString().c_str());
        QVERIFY(LispTokenType::Nil == resultAsArray[2]->Type);
    }

    TEST_METHOD(Test_Tokenizer7)
    {
        IEnumerable<std::shared_ptr<LispToken>> result = LispTokenizer::Tokenize("(test \"blub\nhello\")");
        //Assert::IsNotNull(result);
        QCOMPARE((size_t)4, result.Count());
        var resultAsArray = result.ToArray();
        QCOMPARE("blub\nhello", resultAsArray[2]->ToString().c_str());
        QVERIFY(LispTokenType::String == resultAsArray[2]->Type);
    }

    // *****************************


    TEST_METHOD(Test_Comments)
    {
        std::shared_ptr<LispVariant> result = Lisp::Eval("(do (println \"hello\") ; a comment\n(println; separate lists with comments\n\"world\"));comment in last line");
        QCOMPARE("world", result->ToString().c_str());
    }

    TEST_METHOD(Test_DoAndPrint)
    {
        std::shared_ptr<LispVariant> result = Lisp::Eval("(do (println \"hello\")\n (println \"world\"))");
        QCOMPARE("world", result->ToString().c_str());
    }

    TEST_METHOD(Test_PrintLnMultilines)
    {
        std::shared_ptr<LispVariant> result = Lisp::Eval("(do (println \"hello\nworld\"))");
        QCOMPARE("hello\nworld", result->ToString().c_str());
    }

    TEST_METHOD(Test_PrintTrace)
    {
        std::shared_ptr<LispVariant> result = Lisp::Eval("(do (trace #t) (println \"hello world\") (println (+ 9 8)) (gettrace))");
        QCOMPARE("hello world17", result->ToString().c_str());
    }

    TEST_METHOD(Test_If1)
    {
        std::shared_ptr<LispVariant> result = Lisp::Eval("(if #t (+ 1 2) (- 3 5))");
        QCOMPARE(3, result->ToInt());
    }

    TEST_METHOD(Test_If2)
    {
        std::shared_ptr<LispVariant> result = Lisp::Eval("(if #f (* 1 0) (/ 6 3))");
        QCOMPARE(2, result->ToInt());
    }

    TEST_METHOD(Test_If3)
    {
        std::shared_ptr<LispVariant> result = Lisp::Eval("(if true 1 0)");
        QCOMPARE(1, result->ToInt());
    }

    TEST_METHOD(Test_If4)
    {
        std::shared_ptr<LispVariant> result = Lisp::Eval("(if false 1 0)");
        QCOMPARE(0, result->ToInt());
    }

    TEST_METHOD(Test_If5)
    {
        std::shared_ptr<LispVariant> result = Lisp::Eval("(if true 1)");
        QCOMPARE(1, result->ToInt());
        result = Lisp::Eval("(if false 1)");
        QVERIFY(result.get() == 0);
    }

    TEST_METHOD(Test_Setf1)
    {
        std::shared_ptr<LispVariant> result = Lisp::Eval("(do (def a 1) (def b 1) (setf (first (list 'a 'b 'c)) 9))");
        QCOMPARE(9, result->ToInt());
    }

    TEST_METHOD(Test_SetfWithNth)
    {
        std::shared_ptr<LispVariant> result = Lisp::Eval("(do (def l '(a b c d)) (setf (nth 2 l) 9) (print l))");
        QCOMPARE("(a b 9 d)", result->ToString().c_str());
    }

    TEST_METHOD(Test_SetfWithFirst)
    {
        std::shared_ptr<LispVariant> result = Lisp::Eval("(do (def l '(a b c d)) (setf (first l) 21) (print l))");
        QCOMPARE("(21 b c d)", result->ToString().c_str());
    }

    TEST_METHOD(Test_SetfWithLast)
    {
        std::shared_ptr<LispVariant> result = Lisp::Eval("(do (def l '(a b c d)) (setf (last l) \"xyz\") (print l))");
        QCOMPARE("(a b c \"xyz\")", result->ToString().c_str());
    }

    TEST_METHOD(Test_SetfWithMacros)
    //[DeploymentItem(@"Library\fuellib.fuel", "Library")]
    {
        std::shared_ptr<LispVariant> result = Lisp::Eval("(do (import fuellib) (defstruct point x y) (def p (make-point 12 17)) (setf (get-point-x p) 9) (println p))");
        QVERIFY(result->IsString());
        QCOMPARE("(#point 9 17)", result->ToString().c_str());
    }

    TEST_METHOD(Test_DefstructMacro)
    //[DeploymentItem(@"Library\fuellib.fuel", "Library")]
    {
        std::shared_ptr<LispVariant> result = Lisp::Eval("(do (import fuellib) (defstruct point x y) (def p (make-point 12 17)))");
        QVERIFY(result->IsList());
        QCOMPARE("(#point 12 17)", result->ToString().c_str());
    }

    TEST_METHOD(Test_QuoteList)
    {
        std::shared_ptr<LispVariant> result = Lisp::Eval("(do (def a 1) (def l '(a b c)))");
        QCOMPARE("(a b c)", result->ToString().c_str());
    }

    TEST_METHOD(Test_Def1)
    {
        std::shared_ptr<LispVariant> result = Lisp::Eval("(do (def a 1) (def b 1) (def (nth 2 (list 'a 'b 'c)) 9) (+ c 2))");
        QCOMPARE(11, result->ToInt());
    }

    TEST_METHOD(Test_DefWithNil)
    {
        std::shared_ptr<LispVariant> result = Lisp::Eval("(do (def a nil) (println a))");
        QCOMPARE(LispToken::NilConst.c_str(), result->ToString().c_str());
    }

    TEST_METHOD(Test_Fn)
    {
        std::shared_ptr<LispVariant> result = Lisp::Eval("(do (def f (fn (x) (+ x x 1))) (println (f 8)))");
        QCOMPARE(17, result->ToInt());
    }

    TEST_METHOD(Test_Gdef)
    {
        std::shared_ptr<LispVariant> result = Lisp::Eval("(do (defn f (x) (gdef z (+ x x))) (f 8) (println z))");
        QCOMPARE(16, result->ToInt());
    }

    TEST_METHOD(Test_Gdefn)
    {
        std::shared_ptr<LispVariant> result = Lisp::Eval("(do (defn f (x) (gdefn g (x) (+ x x))) (f 2) (g 8))");
        QCOMPARE(16, result->ToInt());
    }

    TEST_METHOD(Test_Eval1)
    {
        std::shared_ptr<LispVariant> result = Lisp::Eval("(eval (list 'def 'x 43))");
        QCOMPARE(43, result->ToInt());
    }

    TEST_METHOD(Test_Eval2)
    {
        std::shared_ptr<LispVariant> result = Lisp::Eval("(eval '(def x 456))");
        QCOMPARE(456, result->ToInt());
    }

    TEST_METHOD(Test_Eval3)
    {
        std::shared_ptr<LispVariant> result = Lisp::Eval("(eval #t)");
        QCOMPARE(true, result->ToBool());
        result = Lisp::Eval("(eval 42)");
        QCOMPARE(42, result->ToInt());
    }

    TEST_METHOD(Test_Eval4)
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

    TEST_METHOD(Test_Eval5)
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

    TEST_METHOD(Test_EvalStr)
    {
        std::shared_ptr<LispVariant> result = Lisp::Eval("(evalstr \"(def x 456)\")");
        QCOMPARE(456, result->ToInt());
    }

    TEST_METHOD(Test_Doc)
    {
        std::shared_ptr<LispVariant> result = Lisp::Eval("(doc 'if)");
        var s = result->ToString();
        QVERIFY(s.Contains("-------------------------------------------------"));
        QVERIFY(s.Contains("if  [special form]"));
        QVERIFY(s.Contains("Syntax: (if cond then-block [else-block])"));
        QVERIFY(s.Contains("The if statement."));
    }

    TEST_METHOD(Test_DocForDefn)
    {
        std::shared_ptr<LispVariant> result = Lisp::Eval("; this is a fcn documenataion\n(defn blub (x) (+ x 1))\n(doc 'blub)");
        var s = result->ToString();
        QVERIFY(s.Contains("-------------------------------------------------"));
        QVERIFY(s.Contains("blub"));
        QVERIFY(s.Contains("Syntax: (blub x)"));
        QVERIFY(s.Contains("; this is a fcn documenataion"));
    }

    TEST_METHOD(Test_NoAutoDocForDefn)
    {
        std::shared_ptr<LispVariant> result = Lisp::Eval("(defn blub (x y ) (+ x y 1))\n(doc 'blub)");
        var s = result->ToString();
        QVERIFY(s.Contains("-------------------------------------------------"));
        QVERIFY(s.Contains("blub"));
        QVERIFY(s.Contains("Syntax: (blub x y)"));
    }

    TEST_METHOD(Test_TickCount)
    {
        std::shared_ptr<LispVariant> result = Lisp::Eval("(tickcount)");
        QVERIFY(result->IsInt());
    }

    TEST_METHOD(Test_While1)
    {
        std::shared_ptr<LispVariant> result = Lisp::Eval("(do (def a 1) (def b 1) (while (< a 10) (do (setf a (+ a 1)) (setf b (+ b 1)))))");
        QCOMPARE(10, result->ToInt());
    }

    TEST_METHOD(Test_Defn1)
    {
        std::shared_ptr<LispVariant> result = Lisp::Eval("(do (def g_prn \"START:\") (defn prn (x) (setf g_prn (add g_prn x))) (prn \"34\") (println g_prn))");
        QCOMPARE("START:34", result->ToString().c_str());
    }

    TEST_METHOD(Test_AddString)
    {
        std::shared_ptr<LispVariant> result = Lisp::Eval("(+ \"abc\" \"def() ; blub\" \"xxx\")");
        QCOMPARE("abcdef() ; blubxxx", result->ToString().c_str());
    }

    TEST_METHOD(Test_AddLists)
    {
        std::shared_ptr<LispVariant> result = Lisp::Eval("(+ '(1 2 3) '(\"hello world\" 2.3 42))");
        string s = result->ToString();
        QCOMPARE("(1 2 3 \"hello world\" 2.300000 42)", result->ToString().c_str());
    }

    TEST_METHOD(Test_ListFirst)
    {
        std::shared_ptr<LispVariant> result = Lisp::Eval("(first '(1 2 3))");
        QCOMPARE(1, result->ToInt());
    }

    TEST_METHOD(Test_ListFirstSymbol)
    {
        std::shared_ptr<LispVariant> result = Lisp::Eval("(first '(a b c))");
        QCOMPARE("a", result->ToString().c_str());
    }

    TEST_METHOD(Test_ListLast)
    {
        std::shared_ptr<LispVariant> result = Lisp::Eval("(last '(1 2 3))");
        QCOMPARE(3, result->ToInt());
    }

    TEST_METHOD(Test_ListLastSymbol)
    {
        std::shared_ptr<LispVariant> result = Lisp::Eval("(last '(abc def xyz))");
        QCOMPARE("xyz", result->ToString().c_str());
    }

    TEST_METHOD(Test_ListCar)
    {
        std::shared_ptr<LispVariant> result = Lisp::Eval("(car '(\"abc\" 2 3))");
        QCOMPARE("abc", result->ToString().c_str());
    }

    TEST_METHOD(Test_ListRest)
    {
        std::shared_ptr<LispVariant> result = Lisp::Eval("(rest '(1 2 3))");
        QCOMPARE("(2 3)", result->ToString().c_str());
    }

    TEST_METHOD(Test_ListCdr)
    {
        std::shared_ptr<LispVariant> result = Lisp::Eval("(cdr '(\"nix\" 1 2 3))");
        QCOMPARE("(1 2 3)", result->ToString().c_str());
    }

    TEST_METHOD(Test_ListLength)
    {
        std::shared_ptr<LispVariant> result = Lisp::Eval("(len '(1 2 3))");
        QCOMPARE(3, result->ToInt());
    }

    TEST_METHOD(Test_ListAppend)
    {
        std::shared_ptr<LispVariant> result = Lisp::Eval("(append (list 4 54 3) (list 7 9))");
        QCOMPARE("(4 54 3 7 9)", result->ToString().c_str());
    }

    TEST_METHOD(Test_ListPush1)
    {
        std::shared_ptr<LispVariant> result = Lisp::Eval("(do (def l '(a b c)) (push z l))");
        QCOMPARE("(z a b c)", result->ToString().c_str());
    }

    TEST_METHOD(Test_ListPush2)
    {
        std::shared_ptr<LispVariant> result = Lisp::Eval("(do (def l '(a b c)) (push z l 2))");
        QCOMPARE("(a b z c)", result->ToString().c_str());
    }

    TEST_METHOD(Test_ListPush3)
    {
        std::shared_ptr<LispVariant> result = Lisp::Eval("(do (def l '(a b c)) (push z l 2) (print l))");
        QCOMPARE("(a b z c)", result->ToString().c_str());
    }

    TEST_METHOD(Test_ListPushError)
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

    TEST_METHOD(Test_ListPop1)
    {
        std::shared_ptr<LispVariant> result = Lisp::Eval("(do (def l '(a b c)) (def a (pop l)) (print a l))");
        QCOMPARE("a (b c)", result->ToString().c_str());
    }

    TEST_METHOD(Test_ListPop2)
    {
        std::shared_ptr<LispVariant> result = Lisp::Eval("(do (def l '()) (def a (pop l)) (print a l))");
        QCOMPARE("NIL ()", result->ToString().c_str());
    }

    TEST_METHOD(Test_ListPop3)
    {
        std::shared_ptr<LispVariant> result = Lisp::Eval("(do (def l '()) (def a (pop l)) (print a l))");
        QCOMPARE("NIL ()", result->ToString().c_str());
    }

    TEST_METHOD(Test_ListPop4)
    {
        std::shared_ptr<LispVariant> result = Lisp::Eval("(do (def l '(a b c)) (def a (pop l 5)) (print a l))");
        QCOMPARE("NIL (a b c)", result->ToString().c_str());
    }

    TEST_METHOD(Test_ListPop5)
    {
        std::shared_ptr<LispVariant> result = Lisp::Eval("(do (def l '(a b c)) (def a (pop l 1)) (print a l))");
        QCOMPARE("b (a c)", result->ToString().c_str());
    }

    TEST_METHOD(Test_ListPopError)
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

    TEST_METHOD(Test_LogicalOperators)
    {
        std::shared_ptr<LispVariant> result = Lisp::Eval("(list (and #t #f) (and #t #t) (or #t #f) (or #f #f) (or #t #f #t))");
        QCOMPARE("(#f #t #t #f #t)", result->ToString().c_str());
    }

    TEST_METHOD(Test_CompareOperators1)
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

    TEST_METHOD(Test_CompareOperators2)
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

    TEST_METHOD(Test_CompareOperators3)
    {
        std::shared_ptr<LispVariant> result = Lisp::Eval("(list (<= 1 2) (<= 4 1) (>= 5 2) (>= 1 3) (>= 4.0 4.0) (<= 42 42))");
        QCOMPARE("(#t #f #t #f #t #t)", result->ToString().c_str());
    }

    TEST_METHOD(Test_Not)
    {
        std::shared_ptr<LispVariant> result = Lisp::Eval("(list (! #t) (not #t) (not #f) (! #f))");
        QCOMPARE("(#f #f #t #t)", result->ToString().c_str());
    }

    TEST_METHOD(Test_Arithmetric1)
    {
        std::shared_ptr<LispVariant> result = Lisp::Eval("(+ 1 2 3 4)");
        QCOMPARE(10, result->ToInt());
    }

    TEST_METHOD(Test_Arithmetric2)
    {
        std::shared_ptr<LispVariant> result = Lisp::Eval("(+ 1.1 2.2 3.3 4.3)");
        int res = (int)Math_Round(result->ToDouble() * 10.0);
        QCOMPARE(109, res);
    }

    TEST_METHOD(Test_Arithmetric3)
    {
        std::shared_ptr<LispVariant> result = Lisp::Eval("(* 3 8 2)");
        QCOMPARE(48, result->ToInt());
    }

    TEST_METHOD(Test_Arithmetric4)
    {
        std::shared_ptr<LispVariant> result = Lisp::Eval("(/ 1.0 2.0)");
        int res = (int)Math_Round(result->ToDouble() * 10.0);
        QCOMPARE(5, res);
    }

    TEST_METHOD(Test_Arithmetric5)
    {
        std::shared_ptr<LispVariant> result = Lisp::Eval("(/ 10 2)");
        QCOMPARE(5, result->ToInt());
    }

    TEST_METHOD(Test_Arithmetric6)
    {
        std::shared_ptr<LispVariant> result = Lisp::Eval("(- 42 12 6)");
        QCOMPARE(24, result->ToInt());
    }

    TEST_METHOD(Test_Arithmetric7)
    {
        std::shared_ptr<LispVariant> result = Lisp::Eval("(- 42.5 0.5)");
        int res = (int)Math_Round(result->ToDouble() * 10.0);
        QCOMPARE(420, res);
    }

    TEST_METHOD(Test_Arithmetric8)
    {
        std::shared_ptr<LispVariant> result = Lisp::Eval("(sub 42.5 1.5 2.0)");
        int res = (int)Math_Round(result->ToDouble() * 10.0);
        QCOMPARE(390, res);
    }

    TEST_METHOD(Test_Arithmetric9)
    {
        std::shared_ptr<LispVariant> result = Lisp::Eval("(div 12 3)");
        QCOMPARE(4, result->ToInt());
    }

    TEST_METHOD(Test_Arithmetric10)
    {
        std::shared_ptr<LispVariant> result = Lisp::Eval("(mul 2 3 4)");
        QCOMPARE(24, result->ToInt());
    }

    TEST_METHOD(Test_Arithmetric11)
    {
        std::shared_ptr<LispVariant> result = Lisp::Eval("(add 2 3 4)");
        QCOMPARE(9, result->ToInt());
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
            scope->Output->EnableToString(true);
            std::shared_ptr<LispVariant> result = Lisp::Eval(macroExpandScript, scope);
            QCOMPARE("132", result->ToString().c_str());

            string s = scope->Output->GetContent().Trim();
            QVERIFY(s.Contains("first-macro"));
            QVERIFY(s.Contains("second-macro"));
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
            var scope = LispEnvironment::CreateDefaultScope();
            scope->Output->EnableToString(true);
            std::shared_ptr<LispVariant> result = Lisp::Eval(macroExpandScript, scope);
            QCOMPARE("144", result->ToString().c_str());

            string s = scope->Output->GetContent().Trim();
            QVERIFY(s.Contains("first-macro"));
            QVERIFY(s.Contains("second-macro"));
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
            var scope = LispEnvironment::CreateDefaultScope();
            scope->Output->EnableToString(true);
            std::shared_ptr<LispVariant> result = Lisp::Eval(macroExpandScript, scope);
            QCOMPARE("40", result->ToString().c_str());

            string s = scope->Output->GetContent().Trim();
            QVERIFY(s.Contains("first-macro"));
            QVERIFY(s.Contains("second-macro"));
        }
    }

    TEST_METHOD(Test_MacrosExpand1)
    {
#ifdef ENABLE_COMPILE_TIME_MACROS
        std::shared_ptr<LispVariant> result = Lisp::Eval("(do (define-macro-expand blub (x y) '(println x y)) (println (quote (1 2 3))) (blub 3 4))");
        QCOMPARE("3 4", result->ToString().c_str());
#else
        QVERIFY(true);
#endif
    }

    TEST_METHOD(Test_MacrosExpand2)
    {
        const string macroExpandScript = "(do\
            (define-macro-expand first-macro\
                (a b)\
                '(do\
                    (def i 1)\
                    (+ a b i)\
                 )\
            )\
\
            (define-macro-expand second-macro\
                (x y)\
                '(do\
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

    TEST_METHOD(Test_MacrosExpandDefineStruct)
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

    TEST_METHOD(Test_MacrosExpandNested)
    {
        const string macroExpandScript = "(do\
            (define-macro-expand first-macro\
                (a b)\
                '(do\
                    (println first-macro)\
                    (def i 1)\
                    (+ a b i)\
                 )\
            )\
\
            (define-macro-expand second-macro\
                (x y)\
                '(do\
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

    TEST_METHOD(Test_MacrosExpandRecursive)
    {
        const string macroExpandScript = "(do\
            (define-macro-expand first-macro\
                (a b)\
                '(do\
                    (println first-macro)\
                    (def i 1)\
                    (+ a b i)\
                 )\
            )\
\
            (define-macro-expand second-macro\
                (x y)\
                '(do\
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

    TEST_METHOD(Test_MacrosExpandDoubleMacroCall)
    {
        const string macroExpandScript = "(do\
            (define-macro-expand first-macro\
                (a b)\
                '(do\
                    (println first-macro)\
                    (def i 1)\
                    (+ a b i)\
                 )\
            )\
\
            (define-macro-expand second-macro\
                (x y)\
                '(do\
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

    TEST_METHOD(Test_MacrosSetf1)
    {
#ifdef ENABLE_COMPILE_TIME_MACROS
        {
            var scope = LispEnvironment::CreateDefaultScope();
            scope->Output->EnableToString(true);
            std::shared_ptr<LispVariant> result =
                Lisp::Eval(
                    "(do (def a 42) (define-macro-expand my-setf (x value) '(setf x value)) (my-setf a (+ \"blub\" \"xyz\")) (println a))", scope);
            QCOMPARE("blubxyz", result->ToString().c_str());
        }
#else
        QVERIFY(true);
#endif
    }

    TEST_METHOD(Test_MacrosSetf2)
    {
        std::shared_ptr<LispVariant> result = Lisp::Eval("(do (def a 42) (defn my-setf (x value) (setf x value)) (my-setf a (+ 8 9)) (println a))");
        QCOMPARE(42, result->ToInt());
    }

    TEST_METHOD(Test_MacrosSetf3)
    {
        std::shared_ptr<LispVariant> result = Lisp::Eval("(do (def a 42) (define-macro-eval my-setf (x value) (setf x value)) (my-setf a (+ \"blub\" \"xyz\")) (println a))");
        QCOMPARE("blubxyz", result->ToString().c_str());
    }

    TEST_METHOD(Test_Quasiquote1)
    {
        std::shared_ptr<LispVariant> result = Lisp::Eval("(do (def a '(42 99 102 \"hello\")) (def b 55) (println (type a)) (println (nth 3 `(1 2 3 ,@a))))");
        QCOMPARE("42", result->ToString().c_str());
    }

    TEST_METHOD(Test_Quasiquote2)
    {
        std::shared_ptr<LispVariant> result = Lisp::Eval("(do (def a 42) (println `(1 2 3 ,a)))");
        QCOMPARE("(1 2 3 42)", result->ToString().c_str());
    }

    TEST_METHOD(Test_Quasiquote3)
    {
        std::shared_ptr<LispVariant> result = Lisp::Eval("(do (def a 42) (def lst (list 6 8 12)) (println (quasiquote (1 2 3 ,a ,@lst))))");
        QCOMPARE("(1 2 3 42 6 8 12)", result->ToString().c_str());
    }

    TEST_METHOD(Test_Quasiquote4)
    {
        std::shared_ptr<LispVariant> result = Lisp::Eval("(do (def a 42) (def lst (list 6 8 12)) (println (quasiquote (1 2 3 ,a ,lst))))");
        QCOMPARE("(1 2 3 42 (6 8 12))", result->ToString().c_str());
    }

    TEST_METHOD(Test_Quasiquote5)
    {
        std::shared_ptr<LispVariant> result = Lisp::Eval("(do (def a 42) (println (quasiquote (1 2 3 ,(+ 3 a)))))");
        QCOMPARE("(1 2 3 45)", result->ToString().c_str());
    }

    TEST_METHOD(Test_Quasiquote6)
    {
        std::shared_ptr<LispVariant> result = Lisp::Eval("(do (def a 42) (println (quasiquote (1 2 3 ,@(list 9 8 7 a)))))");
        QCOMPARE("(1 2 3 9 8 7 42)", result->ToString().c_str());
    }

    TEST_METHOD(Test_Quasiquote7)
    {
        std::shared_ptr<LispVariant> result = Lisp::Eval("(do (def args '(1 2 3)) `,(first args))");
        QCOMPARE("1", result->ToString().c_str());
    }

    TEST_METHOD(Test_Quasiquote8)
    {
        std::shared_ptr<LispVariant> result = Lisp::Eval("(do (def args '(1 2 3)) `(,(first args)))");
        QCOMPARE("(1)", result->ToString().c_str());
    }

    TEST_METHOD(Test_Quasiquote9)
    {
        std::shared_ptr<LispVariant> result = Lisp::Eval("(do `(b))");
        QCOMPARE("(b)", result->ToString().c_str());
    }

    TEST_METHOD(Test_Quasiquote10)
    {
        std::shared_ptr<LispVariant> result = Lisp::Eval("(do `a)");
        QCOMPARE("a", result->ToString().c_str());
    }

    TEST_METHOD(Test_Quote1)
    {
        std::shared_ptr<LispVariant> result = Lisp::Eval("(do (def x 42) (println 'x))");
        QCOMPARE("x", result->ToString().c_str());
    }

    TEST_METHOD(Test_Quote2)
    {
        std::shared_ptr<LispVariant> result = Lisp::Eval("(do 'x)");
        QCOMPARE("x", result->ToString().c_str());
    }

    TEST_METHOD(Test_Quote3)
    {
        std::shared_ptr<LispVariant> result = Lisp::Eval("(do '(a b 6 x))");
        QCOMPARE("(a b 6 x)", result->ToString().c_str());
    }

    TEST_METHOD(Test_EvalQuasiQuote1)
    {
        std::shared_ptr<LispVariant> result = Lisp::Eval("(do (def a 4) (def lst '(a 2 3)) (eval `,(first lst)))");
        QCOMPARE("4", result->ToString().c_str());
    }

    TEST_METHOD(Test_String1)
    {
        std::shared_ptr<LispVariant> result = Lisp::Eval("(println \"hello \\\\ \\' öäü \n \\\"blub\\\"\")");
        QCOMPARE("hello \\ ' öäü \n \"blub\"", result->ToString().c_str());
    }

    TEST_METHOD(Test_String2)
    {
        std::shared_ptr<LispVariant>  result = Lisp::Eval("(string \"hello\" \"-\" \"world\")");
        QCOMPARE("hello-world", result->ToString().c_str());
    }

    TEST_METHOD(Test_Map)
    {
        std::shared_ptr<LispVariant> result = Lisp::Eval("(map (lambda (x) (+ x 1)) '(1 2 3))");
        QCOMPARE("(2 3 4)", result->ToString().c_str());
    }

    TEST_METHOD(Test_Reduce1)
    {
        std::shared_ptr<LispVariant> result = Lisp::Eval("(reduce (lambda (x y) (+ x y)) '(1 2 3) 0)");
        QCOMPARE(6, result->ToInt());
    }

    TEST_METHOD(Test_Reduce2)
    {
        std::shared_ptr<LispVariant> result = Lisp::Eval("(reduce (lambda (x y) (* x y)) '(2 3 4 5) 2)");
        QCOMPARE(240, result->ToInt());
    }

    TEST_METHOD(Test_Closure1)
    {
        std::shared_ptr<LispVariant> result = Lisp::Eval("(do (defn addx (delta) (lambda (x) (+ x delta))) (def addclosure (addx 41)) (println (addclosure 1)))");
        QCOMPARE(42, result->ToInt());
    }

    TEST_METHOD(Test_Closure2)
    {
        std::shared_ptr<LispVariant> result = Lisp::Eval("(do (defn g (x) (do (+ x 2))) (defn f (x) (do (def i 7) (+ x 1 i (g 2)))) (println (f 1)))");
        QCOMPARE(13, result->ToInt());
    }

    TEST_METHOD(Test_Closure3)
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

    TEST_METHOD(Test_Closure4)
    {
// TODO working...
        std::shared_ptr<LispVariant> result = Lisp::Eval("(do (defn g (x) (do (+ x 2))) (defn f (x) (do (def i 7) (+ x 1 i (g 2)))) (println (f 1)))");
        QCOMPARE(13, result->ToInt());
    }

    TEST_METHOD(Test_RecursiveCall1)
    {
        std::shared_ptr<LispVariant> result = Lisp::Eval("(do (defn addConst (x a) (+ x a)) (def add2 (lambda (x) (addConst x 2))) (println (addConst 8 2)) (println (add2 4)))");
        QCOMPARE(6, result->ToInt());
    }

    TEST_METHOD(Test_Return1)
    {
        std::shared_ptr<LispVariant> result = Lisp::Eval("(do (defn f (x) (return (+ x x))) (println (f 7)))");
        QCOMPARE(14, result->ToInt());
    }

    TEST_METHOD(Test_Return2)
    {
        // return statement was not implmented correctly until 25.7.2018...
        std::shared_ptr<LispVariant> result = Lisp::Eval("(do (defn f (x) (do (return (+ x x)) (return 9))) (println (f 7)))");
        QCOMPARE(14, result->ToInt());
    }

    /* TODO --> not implemented yet for C++
    TEST_METHOD(Test_Call1)
    {
        std::shared_ptr<LispVariant> result = Lisp::Eval("(do (def obj 0) (setf obj (call \"CsLisp.DummyNative\")) (println obj (type obj)) (call obj \"Test\"))");
        QCOMPARE(42, result->ToInt());
    }

    TEST_METHOD(Test_CallStatic)
    {
        std::shared_ptr<LispVariant> result = Lisp::Eval("(do (call-static \"System.IO.File\" Exists \"dummy\"))");
        QCOMPARE(false, result->ToBool());
    }
*/

    TEST_METHOD(Test_CallStaticError)
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

    TEST_METHOD(Test_Apply1)
    {
        std::shared_ptr<LispVariant> result = Lisp::Eval("(apply (lambda (x) (println \"hello\" x)) '(55))");
        QCOMPARE("hello 55", result->ToString().c_str());
    }

    TEST_METHOD(Test_Apply2)
    {
        std::shared_ptr<LispVariant> result = Lisp::Eval("(do (def f (lambda (x) (+ x x))) (apply f '(5)))");
        QCOMPARE(10, result->ToInt());
    }

    TEST_METHOD(Test_Apply3)
    {
        std::shared_ptr<LispVariant> result = Lisp::Eval("(do (def f '+) (apply f '(5 6 7)))");
        QCOMPARE(18, result->ToInt());
    }

    TEST_METHOD(Test_Argscount1)
    {
        std::shared_ptr<LispVariant> result = Lisp::Eval("(do (defn f (x) (do (println \"count=\" (argscount)) (+ x x))) (f 5 6 7))");
        QCOMPARE(10, result->ToInt());
    }

    TEST_METHOD(Test_Arg2)
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

    TEST_METHOD(Test_Arg3)
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

    TEST_METHOD(Test_Arg4)
    {
        std::shared_ptr<LispVariant> result = Lisp::Eval("(do (defn f () (do (def z (arg 2)) (+ z z))) (f 5 6 7))");
        QCOMPARE(14, result->ToInt());
    }

    TEST_METHOD(Test_Args1)
    {
        std::shared_ptr<LispVariant> result = Lisp::Eval("(do (defn f () (do (def z (args)))) (f 5 6 7))");
        QCOMPARE("(5 6 7)", result->ToString().c_str());
    }

    TEST_METHOD(Test_Cons1)
    {
        std::shared_ptr<LispVariant> result = Lisp::Eval("(cons 1 2)");
        QCOMPARE("(1 2)", result->ToString().c_str());
    }

    TEST_METHOD(Test_Cons2)
    {
        std::shared_ptr<LispVariant> result = Lisp::Eval("(cons 1 '(2 3 4))");
        QCOMPARE("(1 2 3 4)", result->ToString().c_str());
    }

    TEST_METHOD(Test_Cons3)
    {
        std::shared_ptr<LispVariant> result = Lisp::Eval("(cons 12)");
        QCOMPARE("(12)", result->ToString().c_str());
    }

    TEST_METHOD(Test_Cons4)
    {
        std::shared_ptr<LispVariant> result = Lisp::Eval("(cons)");
        QCOMPARE("()", result->ToString().c_str());
    }

    TEST_METHOD(Test_Nop)
    {
        std::shared_ptr<LispVariant> result = Lisp::Eval("(nop)");
        QVERIFY(result->IsUndefined());
    }

    TEST_METHOD(Test_Symbol)
    {
        std::shared_ptr<LispVariant> result = Lisp::Eval("(sym a)");
        QVERIFY(result->IsSymbol());
        QCOMPARE("a", result->StringValue().c_str());
    }

    TEST_METHOD(Test_Str)
    {
        std::shared_ptr<LispVariant> result = Lisp::Eval("(str abc)");
        QVERIFY(result->IsString());
        QCOMPARE("abc", result->StringValue().c_str());
    }

    TEST_METHOD(Test_MapError1)
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

    TEST_METHOD(Test_MapError2)
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

    TEST_METHOD(Test_ReduceError1)
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

    TEST_METHOD(Test_ReduceError2)
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

    TEST_METHOD(Test_SetfError)
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

    TEST_METHOD(Test_Parser1)
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

    TEST_METHOD(Test_Parser2)
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

    TEST_METHOD(Test_Parser3)
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

    TEST_METHOD(Test_NotError)
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

    TEST_METHOD(Test_CompareError1)
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

    TEST_METHOD(Test_CompareError2)
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

    TEST_METHOD(Test_ScriptToLong)
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

    TEST_METHOD(Test_DefError)
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

    TEST_METHOD(Test_DoError)
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

    TEST_METHOD(Test_IfError)
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

    TEST_METHOD(Test_FunctionNotFound)
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

    TEST_METHOD(Test_BracketsOutOfBalance1)
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

    TEST_METHOD(Test_BracketsOutOfBalance2)
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

    TEST_METHOD(Test_UnexpectedToken1)
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

    TEST_METHOD(Test_UnexpectedTokenButIsBracketsOutOfBalance)
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

    TEST_METHOD(Test_LispVariantToString)
    {
        LispVariant result(std::make_shared<object>("hello"));
        string s = result.ToString();
        QCOMPARE("hello", s.c_str());
    }

    TEST_METHOD(Test_LispScope1)
    {
        LispScope scope;
        var result = scope.GetPreviousToken(std::make_shared<LispToken>("a", 0, 0, 1));
        QVERIFY(result.get() == 0);
    }

    TEST_METHOD(Test_LispTokenToString)
    {
        LispToken token("(", 0, 1, 1);
        string s = token.ToString();
        QCOMPARE("(", s.c_str());
    }

    TEST_METHOD(Test_Type1)
    {
        std::shared_ptr<LispVariant> result = Lisp::Eval("(type 7)");
        QVERIFY(result->IsInt());
        QCOMPARE(3, result->IntValue());
    }

    TEST_METHOD(Test_Type2)
    {
        std::shared_ptr<LispVariant> result = Lisp::Eval("(type #f)");
        QVERIFY(result->IsInt());
        QCOMPARE(2, result->IntValue());
    }

    TEST_METHOD(Test_Type3)
    {
        std::shared_ptr<LispVariant> result = Lisp::Eval("(type '(2 3 1))");
        QVERIFY(result->IsInt());
        QCOMPARE(6, result->IntValue());
    }

    TEST_METHOD(Test_Type4)
    {
        std::shared_ptr<LispVariant> result = Lisp::Eval("(type aSymbol)");
        QVERIFY(result->IsInt());
        QCOMPARE(8, result->IntValue());
    }

    TEST_METHOD(Test_Type5)
    {
        std::shared_ptr<LispVariant> result = Lisp::Eval("(type \"a string\")");
        QVERIFY(result->IsInt());
        QCOMPARE(5, result->IntValue());
    }

    TEST_METHOD(Test_TypeStr)
    {
        std::shared_ptr<LispVariant> result = Lisp::Eval("(typestr 1.23)");
        QVERIFY(result->IsString());
        QCOMPARE("Double", result->Value->ToString().c_str());
    }

    TEST_METHOD(Test_Vars)
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

    TEST_METHOD(Test_Fuel)
    {
        std::shared_ptr<LispVariant> result = Lisp::Eval("(fuel)");
        QVERIFY(result->IsString());
        QVERIFY(result->StringValue().Contains("fuel version"));
    }

    TEST_METHOD(Test_Copyright)
    {
        std::shared_ptr<LispVariant> result = Lisp::Eval("(copyright)");
        QVERIFY(result->IsString());
        QVERIFY(result->StringValue().Contains("Copyright: MIT-License"));
    }

    TEST_METHOD(Test_Help)
    {
        std::shared_ptr<LispVariant> result = Lisp::Eval("(help)");
        QVERIFY(result->IsString());
        QVERIFY(result->StringValue().Contains("available functions:"));
    }

    TEST_METHOD(Test_Import)
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
            QVERIFY(s.Contains("5"));
            QVERIFY(s.Contains("7"));
        }
    }

    TEST_METHOD(Test_Import2)
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

    TEST_METHOD(Test_BadForeach)
    {
        std::shared_ptr<LispVariant> result = Lisp::Eval("(do (import fuellib) (foreach))");
        QVERIFY(result->IsUndefined());
    }

    TEST_METHOD(Test_Break)
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

    TEST_METHOD(Test_ReadLine)
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

    TEST_METHOD(Test_ReadLineWithArgs)
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

    TEST_METHOD(Test_ParseInteger)
    {
        std::shared_ptr<LispVariant> result = Lisp::Eval("(do (def s \"42\") (def i (parse-integer s)))");
        QVERIFY(result->IsInt());
        QCOMPARE(42, result->IntValue());
    }

    TEST_METHOD(Test_ParseIntegerError)
    {
        std::shared_ptr<LispVariant> result = Lisp::Eval("(do (def s \"nonumber\") (def i (parse-integer s)))");
        QVERIFY(result->IsUndefined());
    }

    TEST_METHOD(Test_ParseFloat)
    {
        std::shared_ptr<LispVariant> result = Lisp::Eval("(do (def s \"42.789\") (def f (parse-float s)))");
        QVERIFY(result->IsDouble());
        QCOMPARE(42.789, result->DoubleValue());
    }

    TEST_METHOD(Test_ParseFloatError)
    {
        std::shared_ptr<LispVariant> result = Lisp::Eval("(do (def s \"nonumber\") (def f (parse-float s)))");
        QVERIFY(result->IsUndefined());
    }

    TEST_METHOD(Test_Slice1)
    {
        std::shared_ptr<LispVariant> result = Lisp::Eval("(do (def s \"this is a string\") (slice s 0 4))");
        QVERIFY(result->IsString());
        QCOMPARE("this", result->StringValue().c_str());
    }

    TEST_METHOD(Test_Slice2)
    {
        std::shared_ptr<LispVariant> result = Lisp::Eval("(do (def s \"this is a string\") (slice s 8 -1))");
        QVERIFY(result->IsString());
        QCOMPARE("a string", result->StringValue().c_str());
    }

    TEST_METHOD(Test_Slice3)
    {
        std::shared_ptr<LispVariant> result = Lisp::Eval("(do (def s \"this is a string\") (slice s 5 4))");
        QVERIFY(result->IsString());
        QCOMPARE("is a", result->StringValue().c_str());
    }

    TEST_METHOD(Test_SliceError)
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

    TEST_METHOD(Test_FirstForString)
    {
        std::shared_ptr<LispVariant> result = Lisp::Eval("(do (def s \"this is a string\") (first s))");
        QVERIFY(result->IsString());
        QCOMPARE("t", result->StringValue().c_str());
    }

    TEST_METHOD(Test_LastForString)
    {
        std::shared_ptr<LispVariant> result = Lisp::Eval("(do (def s \"this is a string\") (last s))");
        QVERIFY(result->IsString());
        QCOMPARE("g", result->StringValue().c_str());
    }

    TEST_METHOD(Test_RestForString)
    {
        std::shared_ptr<LispVariant> result = Lisp::Eval("(do (def s \"this is a string\") (rest s))");
        QVERIFY(result->IsString());
        QCOMPARE("his is a string", result->StringValue().c_str());
    }

    TEST_METHOD(Test_NthForString)
    {
        std::shared_ptr<LispVariant> result = Lisp::Eval("(do (def s \"this is a string\") (nth 5 s))");
        QVERIFY(result->IsString());
        QCOMPARE("i", result->StringValue().c_str());
    }

    TEST_METHOD(Test_LenForString)
    {
        std::shared_ptr<LispVariant> result = Lisp::Eval("(do (def s \"this is a string\") (len s))");
        QVERIFY(result->IsInt());
        QCOMPARE(16, result->IntValue());
    }

    TEST_METHOD(Test_Trim)
    {
        std::shared_ptr<LispVariant> result = Lisp::Eval("(do (def s \" \t   this is a string  \") (trim s))");
        QVERIFY(result->IsString());
        QCOMPARE("this is a string", result->StringValue().c_str());
    }

    TEST_METHOD(Test_LowerCase)
    {
        std::shared_ptr<LispVariant> result = Lisp::Eval("(do (def s \"THIS is A stRing\") (lower-case s))");
        QVERIFY(result->IsString());
        QCOMPARE("this is a string", result->StringValue().c_str());
    }

    TEST_METHOD(Test_UpperCase)
    {
        std::shared_ptr<LispVariant> result = Lisp::Eval("(do (def s \"THIS is A stRing 8 ,.!?\") (upper-case s))");
        QVERIFY(result->IsString());
        QCOMPARE("THIS IS A STRING 8 ,.!?", result->StringValue().c_str());
    }

    TEST_METHOD(Test_Find1)
    {
        std::shared_ptr<LispVariant> result = Lisp::Eval("(do (import \"Library\\\\fuellib.fuel\") (def l '(1 5 7)) (find 5 l))");
        QVERIFY(result->IsInt());
        QCOMPARE(1, result->IntValue());
    }

    TEST_METHOD(Test_Find2)
    {
        std::shared_ptr<LispVariant> result = Lisp::Eval("(do (import \"Library\\\\fuellib.fuel\") (def l '(1 5 7)) (find 9 l))");
        QVERIFY(result->IsInt());
        QCOMPARE(-1, result->IntValue());
    }

    TEST_METHOD(Test_DoTimes1)
    {
        std::shared_ptr<LispVariant> result = Lisp::Eval("(do (import fuellib) (def l '()) (dotimes (ix 7) (setf l (cons ix l))) (println l))");
        QVERIFY(result->IsString());
        QCOMPARE("(6 5 4 3 2 1 0)", result->StringValue().c_str());
    }

    TEST_METHOD(Test_DoTimes2)
    {
        std::shared_ptr<LispVariant> result = Lisp::Eval("(do (import fuellib) (def l '()) (dotimes (i 7) (setf l (cons i l))) (dotimes (i 9) (setf l (cons i l))) (println l))");
        QVERIFY(result->IsString());
        QCOMPARE("(8 7 6 5 4 3 2 1 0 6 5 4 3 2 1 0)", result->StringValue().c_str());
    }

    TEST_METHOD(Test_Reverse)
    {
        std::shared_ptr<LispVariant> result = Lisp::Eval("(do (def l (list 1 2 b \"nix\" 4.5)) (print (reverse l)))");
        QVERIFY(result->IsString());
        QCOMPARE("(4.500000 \"nix\" b 2 1)", result->StringValue().c_str());
    }

    TEST_METHOD(Test_ReverseString)
    {
        std::shared_ptr<LispVariant> result = Lisp::Eval("(do (def s \"this is text\") (print (reverse s)))");
        QVERIFY(result->IsString());
        QCOMPARE("txet si siht", result->StringValue().c_str());
    }

    TEST_METHOD(Test_Search1)
    {
        std::shared_ptr<LispVariant> result = Lisp::Eval("(do (def s \"this is text\") (print (search \"tex\" s)))");
        QVERIFY(result->IsString());
        QCOMPARE("8", result->StringValue().c_str());
    }

    TEST_METHOD(Test_Search2)
    {
        std::shared_ptr<LispVariant> result = Lisp::Eval("(do (def s \"this is text\") (search \"tes\" s))");
        QVERIFY(result->IsInt());
        QCOMPARE(-1, result->IntValue());
    }

    TEST_METHOD(Test_Search3)
    {
        std::shared_ptr<LispVariant> result = Lisp::Eval("(do (def l '(a b 4 d 5.234 \"blub\")) (search b l))");
        QVERIFY(result->IsInt());
        QCOMPARE(1, result->IntValue());
    }

    TEST_METHOD(Test_Search4)
    {
        std::shared_ptr<LispVariant> result = Lisp::Eval("(do (def l '(a b 4 d 5.234 \"blub\")) (search 4 l))");
        QVERIFY(result->IsInt());
        QCOMPARE(2, result->IntValue());
    }

    TEST_METHOD(Test_Search5)
    {
        std::shared_ptr<LispVariant> result = Lisp::Eval("(do (def l '(a b 4 d 5.234 \"blub\")) (search \"blub\" l))");
        QVERIFY(result->IsInt());
        QCOMPARE(5, result->IntValue());
    }

    TEST_METHOD(Test_Search6)
    {
        std::shared_ptr<LispVariant> result = Lisp::Eval("(do (def l '(a b 4 d 5.234 \"blub\")) (search nix l))");
        QVERIFY(result->IsInt());
        QCOMPARE(-1, result->IntValue());
    }

    TEST_METHOD(Test_Search7)
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

    TEST_METHOD(Test_Search8)
    {
        std::shared_ptr<LispVariant> result = Lisp::Eval("(do (def s \"this is text, with more text items for search.\") (search \"text\" s))");
        QVERIFY(result->IsInt());
        QCOMPARE(8, result->IntValue());
        result = Lisp::Eval("(do (def s \"this is text, with more text items for search.\") (search \"text\" s 9))");
        QVERIFY(result->IsInt());
        QCOMPARE(24, result->IntValue());
    }

    TEST_METHOD(Test_Replace1)
    {
        std::shared_ptr<LispVariant> result = Lisp::Eval("(do (def s \"this is a long text\") (replace s \"long\" \"short\"))");
        QVERIFY(result->IsString());
        QCOMPARE("this is a short text", result->StringValue().c_str());
    }

    TEST_METHOD(Test_Replace2)
    {
        std::shared_ptr<LispVariant> result = Lisp::Eval("(do (def s \"this is a long long text\") (replace s \"long\" \"short\"))");
        QVERIFY(result->IsString());
        QCOMPARE("this is a short short text", result->StringValue().c_str());
    }

    TEST_METHOD(Test_Replace3)
    {
        std::shared_ptr<LispVariant> result = Lisp::Eval("(do (def s \"this is a long text\") (replace s \"verylong\" \"short\"))");
        QVERIFY(result->IsString());
        QCOMPARE("this is a long text", result->StringValue().c_str());
    }

    TEST_METHOD(Test_Replace4)
    {
        std::shared_ptr<LispVariant> result = Lisp::Eval("(do (def s \"this is a long text with a lot of words a\") (replace s \"a \" \"an \"))");
        QVERIFY(result->IsString());
        QCOMPARE("this is an long text with an lot of words a", result->StringValue().c_str());
    }

    TEST_METHOD(Test_UnaryMinusInt)
    {
        std::shared_ptr<LispVariant> result = Lisp::Eval("(do (def a 8) (- a))");
        QVERIFY(result->IsInt());
        QCOMPARE(-8, result->IntValue());
    }

    TEST_METHOD(Test_UnaryMinusDouble)
    {
        std::shared_ptr<LispVariant> result = Lisp::Eval("(do (def a 1.234) (- a))");
        QVERIFY(result->IsDouble());
        QCOMPARE(-1.234, result->DoubleValue());
    }

    TEST_METHOD(Test_UnaryMinusString)
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

    TEST_METHOD(Test_ModuloInt)
    {
        std::shared_ptr<LispVariant> result = Lisp::Eval("(do (% 7 3))");
        QVERIFY(result->IsInt());
        QCOMPARE(1, result->IntValue());
    }

    TEST_METHOD(Test_ModuloDouble)
    {
        std::shared_ptr<LispVariant> result = Lisp::Eval("(do (% 7.4 2.8))");
        QVERIFY(result->IsDouble());
        QCOMPARE("1.800000", std::to_string(result->DoubleValue()).c_str());
    }

    TEST_METHOD(Test_NotEnoughFunctionArguments)
    {
        std::shared_ptr<LispVariant> result = Lisp::Eval("(do (defn f (x y z) (add (str x) (str y) (str z))) (f 3))");
        QVERIFY(result->IsString());
        QCOMPARE("3NILNIL", result->StringValue().c_str());
    }

    TEST_METHOD(Test_IntConversion1)
    {
        std::shared_ptr<LispVariant> result = Lisp::Eval("(do (int 7.4))");
        QVERIFY(result->IsInt());
        QCOMPARE(7, result->IntValue());
    }

    TEST_METHOD(Test_IntConversion2)
    {
        std::shared_ptr<LispVariant> result = Lisp::Eval("(do (int \"61.234\"))");
        QVERIFY(result->IsUndefined());
    }

    TEST_METHOD(Test_IntConversion3)
    {
        std::shared_ptr<LispVariant> result = Lisp::Eval("(do (int #t))");
        QVERIFY(result->IsInt());
        QCOMPARE(1, result->IntValue());
    }

    TEST_METHOD(Test_IntConversion4)
    {
        std::shared_ptr<LispVariant> result = Lisp::Eval("(do (int #f))");
        QVERIFY(result->IsInt());
        QCOMPARE(0, result->IntValue());
    }

    TEST_METHOD(Test_IntConversion5)
    {
        std::shared_ptr<LispVariant> result = Lisp::Eval("(do (int \"a text\"))");
        QVERIFY(result->IsUndefined());
    }

    TEST_METHOD(Test_FloatConversion1)
    {
        std::shared_ptr<LispVariant> result = Lisp::Eval("(do (float 7))");
        QVERIFY(result->IsDouble());
        QCOMPARE(7.0, result->DoubleValue());
    }

    TEST_METHOD(Test_FloatConversion2)
    {
        std::shared_ptr<LispVariant> result = Lisp::Eval("(do (float \"61.234\"))");
        QVERIFY(result->IsDouble());
        QCOMPARE(61.234, result->DoubleValue());
    }

    TEST_METHOD(Test_FloatConversion3)
    {
        std::shared_ptr<LispVariant> result = Lisp::Eval("(do (float #t))");
        QVERIFY(result->IsDouble());
        QCOMPARE(1.0, result->DoubleValue());
    }

    TEST_METHOD(Test_FloatConversion4)
    {
        std::shared_ptr<LispVariant> result = Lisp::Eval("(do (float #f))");
        QVERIFY(result->IsDouble());
        QCOMPARE(0.0, result->DoubleValue());
    }

    TEST_METHOD(Test_FloatConversion5)
    {
        std::shared_ptr<LispVariant> result = Lisp::Eval("(do (float \"a text\"))");
        QVERIFY(result->IsUndefined());
    }

    TEST_METHOD(Test_DelVar1)
    {
        std::shared_ptr<LispVariant> result = Lisp::Eval("(do (def a 8) (delvar 'a))");
        QVERIFY(result->IsBool());
        QCOMPARE(true, result->ToBool());
    }

    TEST_METHOD(Test_DelVar2)
    {
        std::shared_ptr<LispVariant> result = Lisp::Eval("(do (def a 8) (delvar 'b))");
        QVERIFY(result->IsBool());
        QCOMPARE(false, result->ToBool());
    }

    TEST_METHOD(Test_NeedLValue1)
    {
        std::shared_ptr<LispVariant> result = Lisp::Eval("(do (def a 7) (defn test () (do (println (need-l-value)) (return 'a))) (setf (test) 8) (println a))");
        QVERIFY(result->IsString());
        QCOMPARE("8", result->ToString().c_str());
    }

    TEST_METHOD(Test_NeedLValue2)
    {
        std::shared_ptr<LispVariant> result = Lisp::Eval("(do (def a 7) (defn test () (need-l-value)) (println (test)))");
        QVERIFY(result->IsString());
        QCOMPARE("#f", result->ToString().c_str());
    }

    TEST_METHOD(Test_ImportPathTest)
    {
        std::shared_ptr<LispVariant> result = Lisp::Eval("(do (import \"Library/fuellib.fuel\") (def s \"some text\") (strlen s))");
        QVERIFY(result->IsInt());
        QCOMPARE(9, result->IntValue());
    }

    TEST_METHOD(Test_DictMake)
    {
        std::shared_ptr<LispVariant> result = Lisp::Eval("(do (def d (make-dict)))");
        QVERIFY(result->IsNativeObject());
        QCOMPARE("{  }", result->ToString().c_str());
    }

    TEST_METHOD(Test_DictSet)
    {
        std::shared_ptr<LispVariant> result = Lisp::Eval("(do (def d (make-dict)) (dict-set d \"key1\" 42) (dict-set d 7 \"some text\") (println d))");
        QVERIFY(result->IsString());
        QCOMPARE("{ [\"key1\" : 42], [7 : \"some text\"] }", result->ToString().c_str());
    }

    TEST_METHOD(Test_DictGet)
    {
        std::shared_ptr<LispVariant> result = Lisp::Eval("(do (def d (make-dict)) (dict-set d \"key1\" 42) (dict-set d 7 \"some text\") (dict-get d 7))");
        QVERIFY(result->IsString());
        QCOMPARE("some text", result->ToString().c_str());
    }

    TEST_METHOD(Test_DictLen)
    {
        std::shared_ptr<LispVariant> result = Lisp::Eval("(do (def d (make-dict)) (dict-set d \"key1\" 42) (dict-set d 7 \"some text\") (len d))");
        QVERIFY(result->IsInt());
        QCOMPARE(2, result->ToInt());
    }

    TEST_METHOD(Test_DictClear)
    {
        std::shared_ptr<LispVariant> result = Lisp::Eval("(do (def d (make-dict)) (dict-set d \"key1\" 42) (dict-set d 7 \"some text\") (dict-clear d) (len d))");
        QVERIFY(result->IsInt());
        QCOMPARE(0, result->ToInt());
    }

    TEST_METHOD(Test_DictKeys)
    {
        std::shared_ptr<LispVariant> result = Lisp::Eval("(do (def d (make-dict)) (dict-set d \"key1\" 42) (dict-set d 7 \"some text\") (dict-keys d))");
        QVERIFY(result->IsList());
        QCOMPARE("(\"key1\" 7)", result->ToString().c_str());
    }

    TEST_METHOD(Test_DictRemove)
    {
        std::shared_ptr<LispVariant> result = Lisp::Eval("(do (def d (make-dict)) (dict-set d \"key1\" 42) (dict-set d 7 \"some text\") (dict-remove d 7) (println d))");
        QVERIFY(result->IsString());
        QCOMPARE("{ [\"key1\" : 42] }", result->ToString().c_str());
    }

    TEST_METHOD(Test_DictContainsKey1)
    {
        std::shared_ptr<LispVariant> result = Lisp::Eval("(do (def d (make-dict)) (dict-set d \"key1\" 42) (dict-set d 7 \"some text\") (dict-contains-key d 8))");
        QVERIFY(result->IsBool());
        QCOMPARE(false, result->ToBool());
    }

    TEST_METHOD(Test_DictContainsKey2)
    {
        std::shared_ptr<LispVariant> result = Lisp::Eval("(do (def d (make-dict)) (dict-set d \"key1\" 42) (dict-set d 7 \"some text\") (dict-contains-key d \"key1\"))");
        QVERIFY(result->IsBool());
        QCOMPARE(true, result->ToBool());
    }

    TEST_METHOD(Test_DictContainsValue1)
    {
        std::shared_ptr<LispVariant> result = Lisp::Eval("(do (def d (make-dict)) (dict-set d \"key1\" 42) (dict-set d 7 \"some text\") (dict-contains-value d 99))");
        QVERIFY(result->IsBool());
        QCOMPARE(false, result->ToBool());
    }

    TEST_METHOD(Test_DictContainsValue2)
    {
        std::shared_ptr<LispVariant> result = Lisp::Eval("(do (def d (make-dict)) (dict-set d \"key1\" 42) (dict-set d 7 \"some text\") (dict-contains-value d 42))");
        QVERIFY(result->IsBool());
        QCOMPARE(true, result->ToBool());
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

    TEST_METHOD(Test_Debugger)
    {
        std::shared_ptr<LispDebugger> debugger = std::make_shared<LispDebugger>();
        QVERIFY(debugger.get() != 0);
    }

    TEST_METHOD(Test_Main)
    {
        //using (ConsoleRedirector cr = new ConsoleRedirector())
        {
            std::shared_ptr<TextWriter> output = std::make_shared<TextWriter>();
            std::shared_ptr<TextReader> input = std::make_shared<TextReader>();
            output->EnableToString(true);
            Fuel::MainExtended(std::vector<string>(), output, input);

            string s = output->GetContent().Trim();
            QVERIFY(s.StartsWith(Lisp::Name));
        }
    }
/*
    TEST_METHOD(Test_MainFile)
    {
        //using (ConsoleRedirector cr = new ConsoleRedirector())
        {
            std::shared_ptr<TextWriter> output = std::make_shared<TextWriter>();
            std::shared_ptr<TextReader> input = std::make_shared<TextReader>();
            output->EnableToString(true);
            std::vector<string> args;
            args.push_back("scripts\\simple.fuel");
            Fuel::MainExtended(args, output, input);

            string s = output->GetContent();
            QVERIFY(s.StartsWith("hello world !"));
        }
    }

    TEST_METHOD(Test_MainFileError)
    {
        //using (ConsoleRedirector cr = new ConsoleRedirector())
        {
            std::shared_ptr<TextWriter> output = std::make_shared<TextWriter>();
            std::shared_ptr<TextReader> input = std::make_shared<TextReader>();
            output->EnableToString(true);
            std::vector<string> args;
            args.push_back("scripts\\error.fuel");
            Fuel::MainExtended(args, output, input);

            string s = output->GetContent();
            QVERIFY(s.Contains("Error executing script"));
            QVERIFY(s.Contains("printx"));
            QVERIFY(s.Contains("not found"));
        }
    }

    TEST_METHOD(Test_MainFileErrorDetailed)
    {
        //using (ConsoleRedirector cr = new ConsoleRedirector())
        {
            std::shared_ptr<TextWriter> output = std::make_shared<TextWriter>();
            std::shared_ptr<TextReader> input = std::make_shared<TextReader>();
            output->EnableToString(true);
            std::vector<string> args;
            args.push_back("-x");
            args.push_back("scripts\\error.fuel");
            Fuel::MainExtended(args, output, input);

            string s = output->GetContent();
            QVERIFY(s.Contains("Error executing script"));
            QVERIFY(s.Contains("printx"));
            QVERIFY(s.Contains("not found"));
            QVERIFY(s.Contains("Callstack"));
            QVERIFY(s.Contains("Exception in"));
        }
    }
*/
    TEST_METHOD(Test_MainHelp)
    {
        //using (ConsoleRedirector cr = new ConsoleRedirector())
        {
            std::shared_ptr<TextWriter> output = std::make_shared<TextWriter>();
            std::shared_ptr<TextReader> input = std::make_shared<TextReader>();
            output->EnableToString(true);
            std::vector<string> args;
            args.push_back("-h");
            Fuel::MainExtended(args, output, input);

            string s = output->GetContent().Trim();
            QVERIFY(s.StartsWith(Lisp::Name));
        }
    }

    TEST_METHOD(Test_MainVersion)
    {
        //using (ConsoleRedirector cr = new ConsoleRedirector())
        {
            std::shared_ptr<TextWriter> output = std::make_shared<TextWriter>();
            std::shared_ptr<TextReader> input = std::make_shared<TextReader>();
            output->EnableToString(true);
            std::vector<string> args;
            args.push_back("-v");
            Fuel::MainExtended(args, output, input);

            string s = output->GetContent().Trim();
            QVERIFY(s.StartsWith(Lisp::ProgramName + " " + Lisp::Version + " from " + Lisp::Date));
        }
    }

    TEST_METHOD(Test_MainExecute)
    {
        //using (ConsoleRedirector cr = new ConsoleRedirector())
        {
            std::shared_ptr<TextWriter> output = std::make_shared<TextWriter>();
            std::shared_ptr<TextReader> input = std::make_shared<TextReader>();
            output->EnableToString(true);
            std::vector<string> args;
            args.push_back("-e");
            args.push_back("(print (+ 1 2))");
            Fuel::MainExtended(args, output, input);

            string s = output->GetContent();
            QVERIFY(s == "3");
        }
    }

    TEST_METHOD(Test_MainExecuteAutoBlockDecorate)
    {
        //using (ConsoleRedirector cr = new ConsoleRedirector())
        {
            std::shared_ptr<TextWriter> output = std::make_shared<TextWriter>();
            std::shared_ptr<TextReader> input = std::make_shared<TextReader>();
            output->EnableToString(true);
            std::vector<string> args;
            args.push_back("-e");
            args.push_back("(println \"hello world\") (println \"done.\")");
            Fuel::MainExtended(args, output, input);

            string s = output->GetContent();
            QVERIFY(s.Contains("hello world"));
            QVERIFY(s.Contains("done"));
        }
    }

    TEST_METHOD(Test_MainInteractiveImport)
    {
        //using (ConsoleRedirector cr = new ConsoleRedirector("(import fuellib)\nmacros\nmodules\nfuncs\n"))
        {
            string useForInput = "(import fuellib)\nmacros\nmodules\nfuncs\n";
            std::shared_ptr<TextWriter> output = std::make_shared<TextWriter>();
            std::shared_ptr<TextReader> input = std::make_shared<TextReader>();
            output->EnableToString(true);
            input->SetContent(useForInput);
            input->EnableFromString(true);
            std::vector<string> args;
            args.push_back("-i");
            Fuel::MainExtended(args, output, input);

            string s = output->GetContent();
            QVERIFY(s.Contains(ConvertToLocalDirectorySeperators(".\\Library\\fuellib.fuel")));
            //QVERIFY(s.Contains("Dict-Remove--> function(Dict - Remove obj p0) : Function: module = .\\Library\\fuellib.fuel"));
            QVERIFY(s.Contains(ConvertToLocalDirectorySeperators("foreach --> function (foreach container fcn)         : Function  : module=.\\Library\\fuellib.fuel")));
        }
    }

    TEST_METHOD(Test_MainInteractiveDoc)
    {
        //using (ConsoleRedirector cr = new ConsoleRedirector("doc\ndoc if"))
        {
            std::shared_ptr<TextWriter> output = std::make_shared<TextWriter>();
            std::shared_ptr<TextReader> input = std::make_shared<TextReader>();
            output->EnableToString(true);
            string useForInput = "doc\ndoc if";
            input->SetContent(useForInput);
            input->EnableFromString(true);
            std::vector<string> args;
            args.push_back("-i");
            Fuel::MainExtended(args, output, input);

            string s = output->GetContent();
            QVERIFY(s.Contains("doc --> (doc functionname ...)"));
            QVERIFY(s.Contains("Returns and shows the documentation of all builtin functions or for the given function name(s)."));
            QVERIFY(s.Contains("-------------------------------"));
        }
    }

    TEST_METHOD(Test_MainInteractiveSearchDoc)
    {
        //using (ConsoleRedirector cr = new ConsoleRedirector("searchdoc arg"))
        {
            std::shared_ptr<TextWriter> output = std::make_shared<TextWriter>();
            std::shared_ptr<TextReader> input = std::make_shared<TextReader>();
            output->EnableToString(true);
            string useForInput = "searchdoc arg";
            input->SetContent(useForInput);
            input->EnableFromString(true);
            std::vector<string> args;
            args.push_back("-i");
            Fuel::MainExtended(args, output, input);

            string s = output->GetContent();
            QVERIFY(s.Contains("Syntax: (argscount)"));
            QVERIFY(s.Contains("Syntax: (args)"));
            QVERIFY(s.Contains("Syntax: (arg number)"));
        }
    }
/*
    TEST_METHOD(Test_MultiPrintLn)
    {
        //using (ConsoleRedirector cr = new ConsoleRedirector())
        {
            std::shared_ptr<TextWriter> output = std::make_shared<TextWriter>();
            std::shared_ptr<TextReader> input = std::make_shared<TextReader>();
            output->EnableToString(true);
            std::vector<string> args;
            args.push_back("TestData\\multiprintln.fuel");
            Fuel::MainExtended(args, output, input);

            string s = output->GetContent();
            QVERIFY(s.Contains("hello\nworld\ndone."));
        }
    }

    TEST_METHOD(Test_WriteAndReadFile)
    {
        //using (ConsoleRedirector cr = new ConsoleRedirector())
        {
            std::shared_ptr<TextWriter> output = std::make_shared<TextWriter>();
            std::shared_ptr<TextReader> input = std::make_shared<TextReader>();
            output->EnableToString(true);
            std::vector<string> args;
            args.push_back("TestData\\writereadfile.fuel");
            Fuel::MainExtended(args, output, input);

            string s = output->GetContent();
            QVERIFY(s.Contains("exists file =  #t"));
            QVERIFY(s.Contains("test non existing file =  #f"));
            QVERIFY(s.Contains("is equal =  #t"));
        }
    }

    TEST_METHOD(Test_Profile)
    {
        //using (ConsoleRedirector cr = new ConsoleRedirector())
        {
            std::shared_ptr<TextWriter> output = std::make_shared<TextWriter>();
            std::shared_ptr<TextReader> input = std::make_shared<TextReader>();
            output->EnableToString(true);
            std::vector<string> args;
            args.push_back("-m");
            args.push_back("TestData\\simple.fuel");
            Fuel::MainExtended(args, output, input);

            string s = output->GetContent();
            QVERIFY(s.Contains("Execution time ="));
        }
    }

    TEST_METHOD(Test_Trace)
    {
        //using (ConsoleRedirector cr = new ConsoleRedirector())
        {
            std::shared_ptr<TextWriter> output = std::make_shared<TextWriter>();
            std::shared_ptr<TextReader> input = std::make_shared<TextReader>();
            output->EnableToString(true);
            std::vector<string> args;
            args.push_back("-t");
            args.push_back("TestData\\simple.fuel");
            Fuel::MainExtended(args, output, input);

            string s = output->GetContent();
            QVERIFY(s.Contains("--> do"));
            QVERIFY(s.Contains("--> print"));
        }
    }
*/
    TEST_METHOD(Test_MainInteractive)
    {
        //using (var cr = new ConsoleRedirector("help\nfuncs\nbuiltins\nq\n"))
        {
            std::shared_ptr<TextWriter> output = std::make_shared<TextWriter>();
            std::shared_ptr<TextReader> input = std::make_shared<TextReader>();
            output->EnableToString(true);
            string useForInput = "help\nfuncs\nbuiltins\nq\n";
            input->SetContent(useForInput);
            input->EnableFromString(true);
            std::vector<string> args;
            args.push_back("-i");
            Fuel::MainExtended(args, output, input);

            string s = output->GetContent();
            //TestContext.WriteLine("Result=" + result);
            QVERIFY(s.Contains("DBG>"));
            QVERIFY(s.Contains("Type \"help\" for informations."));
            QVERIFY(s.Contains("help for interactive loop:")); // help
            QVERIFY(s.Contains("equal --> function (equal expr1 expr2)             : Function"));
            QVERIFY(s.Contains("define-macro --> function (define-macro name (arguments) statement) : Function  : module=<builtin>"));
        }
    }

    TEST_METHOD(Test_MainDebuggerExecute)
    {
        //using (ConsoleRedirector cr = new ConsoleRedirector("r\nhelp\nb 3\nb 4 (= a 42)\nr\nr\no\ns\nrestart\nv\nr\nb 3\nclear 3\nlist\nstack\nglobals\nlocals\ncode\nfuncs\nq\n"))
        {
            const string script = "(do\n\
(def a 42)\n\
(print (+ 1 2))\n\
(print (* 3 4 5)))\n";

            std::shared_ptr<TextWriter> output = std::make_shared<TextWriter>();
            std::shared_ptr<TextReader> input = std::make_shared<TextReader>();
            output->EnableToString(true);
            string useForInput = "r\nhelp\nb 3\nb 4 (= a 42)\nr\nr\no\ns\nrestart\nv\nr\nb 3\nclear 3\nlist\nstack\nglobals\nlocals\ncode\nfuncs\nq\n";
            input->SetContent(useForInput);
            input->EnableFromString(true);
            std::vector<string> args;
            args.push_back("-d");
            args.push_back("-e");
            args.push_back(script);
            Fuel::MainExtended(args, output, input);

            string s = output->GetContent();
            QVERIFY(s.Contains("DBG>"));
            QVERIFY(s.Contains("Type \"help\" for informations."));
            QVERIFY(s.Contains("--> do line=1 start=1 stop=3"));
            QVERIFY(s.Contains("help for interactive loop:")); // help
            QVERIFY(s.Contains("#2   line=4     module=command-line              condition=(= a 42)")); // list
            QVERIFY(s.Contains("-->    1 name=<main>                              lineno=3    module=command-line")); // stack
            QVERIFY(s.Contains("a --> 42                                       : Int")); // locals / globals
            QVERIFY(s.Contains("(def a 42)")); // code
            QVERIFY(s.Contains("print --> function (println expr1 expr2 ...)       : Function  : module=<builtin>")); // funcs
        }
    }
/*
    TEST_METHOD(Test_DebugFile)
    {
        //using (ConsoleRedirector cr = new ConsoleRedirector("b 4\nr\nl\nk\nlist\ndown\nk\nup\ncode\ndown\ncode\nclear\ny\nlist\nver\nabout"))
        {
            std::shared_ptr<TextWriter> output = std::make_shared<TextWriter>();
            std::shared_ptr<TextReader> input = std::make_shared<TextReader>();
            output->EnableToString(true);
            string useForInput = "b 4\nr\nl\nk\nlist\ndown\nk\nup\ncode\ndown\ncode\nclear\ny\nlist\nver\nabout";
            input->SetContent(useForInput);
            input->EnableFromString(true);
            std::vector<string> args;
            args.push_back("-d");
            args.push_back("TestData\\testdebugger.fuel");
            Fuel::MainExtended(args, output, input);

            string s = output->GetContent();
            QVERIFY(s.Contains("FUEL(isp)-DBG>                    x --> 4                                        : Int "));
            QVERIFY(s.Contains("       1 name=<main>                              lineno=11   module=TestData\\testdebugger.fuel"));
            QVERIFY(s.Contains("       2 name=g                                   lineno=8    module=TestData\\testdebugger.fuel"));
            QVERIFY(s.Contains("-->    3 name=f                                   lineno=4    module=TestData\\testdebugger.fuel"));
            QVERIFY(s.Contains("FUEL(isp)-DBG> Breakpoints:"));
            QVERIFY(s.Contains("#1   line=4     module=TestData\\testdebugger.fuel condition="));
            QVERIFY(s.Contains("-->    2 name=g                                   lineno=8    module=TestData\\testdebugger.fuel"));
            QVERIFY(s.Contains("  4 B  --> 	   (+ x 1)"));
            QVERIFY(s.Contains("  8    --> 	   (* x x (f x))"));
            QVERIFY(s.Contains("  4 B      	   (+ x 1)"));
            QVERIFY(s.Contains("FUEL(isp)-DBG> Really delete all breakpoints? (y/n)"));
            QVERIFY(s.Contains("FUEL(isp) v0.99.3 (for C++) from 12.1.2019, (C) by Michael Neuroth"));
            QVERIFY(s.Contains("FUEL(isp) is a fast usable embeddable lisp interpreter"));
        }
    }
*/
    TEST_METHOD(Test_DebugSetBreakpoints)
    {
        //using (ConsoleRedirector cr = new ConsoleRedirector("b \"module name\":4 (== a 4)\nlist"))
        {
            std::shared_ptr<TextWriter> output = std::make_shared<TextWriter>();
            std::shared_ptr<TextReader> input = std::make_shared<TextReader>();
            output->EnableToString(true);
            string useForInput = "b \"module name\":4 (== a 4)\nlist";
            input->SetContent(useForInput);
            input->EnableFromString(true);
            std::vector<string> args;
            args.push_back("-d");
            args.push_back("TestData\\simple.fuel");
            Fuel::MainExtended(args, output, input);

            string s = output->GetContent();
            QVERIFY(s.Contains("#1   line=4     module=module name               condition=(== a 4)"));
        }
    }
/*
    TEST_METHOD(Test_DebugModule)
    {
        //using (ConsoleRedirector cr = new ConsoleRedirector("b .\\testmodule.fuel:4\nlist\nr\nk\nl"))
        {
            std::shared_ptr<TextWriter> output = std::make_shared<TextWriter>();
            std::shared_ptr<TextReader> input = std::make_shared<TextReader>();
            output->EnableToString(true);
            string useForInput = "b .\\Library\\testmodule.fuel:4\nlist\nr\nk\nl";
            input->SetContent(useForInput);
            input->EnableFromString(true);
            std::vector<string> args;
            args.push_back("-d");
            args.push_back("TestData\\test.fuel");
            args.push_back("-l=.");
            Fuel::MainExtended(args, output, input);

            string s = output->GetContent();
            QVERIFY(s.Contains("FUEL(isp)-DBG> Breakpoints:"));
            QVERIFY(s.Contains("#1   line=4     module=.\\Library\\testmodule.fuel condition="));
            QVERIFY(s.Contains("       1 name=<main>                              lineno=4    module=TestData\\test.fuel"));
            QVERIFY(s.Contains("-->    2 name=blub                                lineno=4    module=.\\Library\\testmodule.fuel"));
            QVERIFY(s.Contains("x --> 8                                        : Int"));
        }
    }
*/
    TEST_METHOD(Test_Documentation)
    {
        //using (ConsoleRedirector cr = new ConsoleRedirector())
        {
            std::shared_ptr<TextWriter> output = std::make_shared<TextWriter>();
            std::shared_ptr<TextReader> input = std::make_shared<TextReader>();
            output->EnableToString(true);
            std::vector<string> args;
            args.push_back("--doc");
            Fuel::MainExtended(args, output, input);

            string s = output->GetContent();
            QVERIFY(s.Contains("lambda"));
            QVERIFY(s.Contains("Syntax: (lambda (arguments) block)"));
        }
    }

    TEST_METHOD(Test_MainTestParserBracketsOutOfBalance)
    {
        //using (ConsoleRedirector cr = new ConsoleRedirector())
        {
            std::shared_ptr<TextWriter> output = std::make_shared<TextWriter>();
            std::shared_ptr<TextReader> input = std::make_shared<TextReader>();
            output->EnableToString(true);
            std::vector<string> args;
            args.push_back("-e");
            args.push_back("(print ( (+ 1 2))");
            Fuel::MainExtended(args, output, input);

            string s = output->GetContent();
            QVERIFY(s.Contains("Brackets out of balance --> line=2 start=17 stop=18 module="));
        }
    }

    TEST_METHOD(Test_MainTestParserUnexpectedToken)
    {
        //using (ConsoleRedirector cr = new ConsoleRedirector())
        {
            std::shared_ptr<TextWriter> output = std::make_shared<TextWriter>();
            std::shared_ptr<TextReader> input = std::make_shared<TextReader>();
            output->EnableToString(true);
            std::vector<string> args;
            args.push_back("-e");
            args.push_back("dummy (print (+ 1 2))");
            Fuel::MainExtended(args, output, input);

            string s = output->GetContent();
            QVERIFY(s.Contains("List expected in do --> line=1 start=0 stop=5 module="));
        }
    }

    TEST_METHOD(Test_MainTestFunctionNotFound)
    {
        //using (ConsoleRedirector cr = new ConsoleRedirector())
        {
            std::shared_ptr<TextWriter> output = std::make_shared<TextWriter>();
            std::shared_ptr<TextReader> input = std::make_shared<TextReader>();
            output->EnableToString(true);
            std::vector<string> args;
            args.push_back("-e");
            args.push_back("(unknown-fcn (+ 1 2))");
            Fuel::MainExtended(args, output, input);

            string s = output->GetContent();
            QVERIFY(s.Contains("Function \"unknown-fcn\" not found --> line=1 start=1 stop=12"));
        }
    }

    TEST_METHOD(Test_MainTestSymbolNotFound)
    {
        //using (ConsoleRedirector cr = new ConsoleRedirector())
        {
            std::shared_ptr<TextWriter> output = std::make_shared<TextWriter>();
            std::shared_ptr<TextReader> input = std::make_shared<TextReader>();
            output->EnableToString(true);
            std::vector<string> args;
            args.push_back("-e");
            args.push_back("(setf a 5)");
            Fuel::MainExtended(args, output, input);

            string s = output->GetContent();
            QVERIFY(s.Contains("Symbol a not found --> line=1 start=1 stop=5"));
        }
    }

    TEST_METHOD(Test_MainTestListExpectedInDo)
    {
        //using (ConsoleRedirector cr = new ConsoleRedirector())
        {
            std::shared_ptr<TextWriter> output = std::make_shared<TextWriter>();
            std::shared_ptr<TextReader> input = std::make_shared<TextReader>();
            output->EnableToString(true);
            std::vector<string> args;
            args.push_back("-e");
            args.push_back("(do (print 3) 5)");
            Fuel::MainExtended(args, output, input);

            string s = output->GetContent();
            QVERIFY(s.Contains("List expected in do --> line=1 start=13 stop=15"));
        }
    }

    TEST_METHOD(Test_MainTestBadArgumentCount)
    {
        //using (ConsoleRedirector cr = new ConsoleRedirector())
        {
            std::shared_ptr<TextWriter> output = std::make_shared<TextWriter>();
            std::shared_ptr<TextReader> input = std::make_shared<TextReader>();
            output->EnableToString(true);
            std::vector<string> args;
            args.push_back("-e");
            args.push_back("(do (print 3) (defn x))");
            Fuel::MainExtended(args, output, input);

            string s = output->GetContent();
            QVERIFY(s.Contains("Bad argument count in def, has 1 expected 3 --> line=1 start=15 stop=19"));
        }
    }

    TEST_METHOD(Test_MainTestNoFunction)
    {
        //using (ConsoleRedirector cr = new ConsoleRedirector())
        {
            std::shared_ptr<TextWriter> output = std::make_shared<TextWriter>();
            std::shared_ptr<TextReader> input = std::make_shared<TextReader>();
            output->EnableToString(true);
            std::vector<string> args;
            args.push_back("-e");
            args.push_back("(do (print 3) (map 3 '(1 2 3)))");
            Fuel::MainExtended(args, output, input);

            string s = output->GetContent();
            QVERIFY(s.Contains("No function in map --> line=1 start=15 stop=18"));
        }
    }

    TEST_METHOD(Test_MainTestNoList)
    {
        //using (ConsoleRedirector cr = new ConsoleRedirector())
        {
            std::shared_ptr<TextWriter> output = std::make_shared<TextWriter>();
            std::shared_ptr<TextReader> input = std::make_shared<TextReader>();
            output->EnableToString(true);
            std::vector<string> args;
            args.push_back("-e");
            args.push_back("(do (print 3) (map (lambda (x) (print x)) 3))");
            Fuel::MainExtended(args, output, input);

            string s = output->GetContent();
            QVERIFY(s.Contains("No list in map --> line=1 start=40 stop=40"));
        }
    }

    TEST_METHOD(Test_MainTestSymbolExpected)
    {
        //using (ConsoleRedirector cr = new ConsoleRedirector())
        {
            std::shared_ptr<TextWriter> output = std::make_shared<TextWriter>();
            std::shared_ptr<TextReader> input = std::make_shared<TextReader>();
            output->EnableToString(true);
            std::vector<string> args;
            args.push_back("-e");
            args.push_back("(do (print 3) (def 4 \"test\"))");
            Fuel::MainExtended(args, output, input);

            string s = output->GetContent();
            QVERIFY(s.Contains("Symbol expected --> line=1 start=15 stop=18"));
        }
    }

    // *****************************

    TEST_METHOD(Test_TestIndexOfInString)
    {
        CppLisp::string target("abc def blub 123");

        QCOMPARE((size_t)4, target.IndexOf("def", "nix"));
        QCOMPARE(CppLisp::string::npos, target.IndexOf("test", "nix"));
    }

    // *****************************
    // *****************************

    void cleanupTestCase()
    {
        qDebug("CLEANUP fuel unit tests.");
    }
};

QTEST_APPLESS_MAIN(QtLispInterpreterUnitTestsTest)

#include "tst_qtlispinterpreterunitteststest.moc"
