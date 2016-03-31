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

using System.Collections.Generic;
using System.Linq;
using CsLisp;
using Microsoft.VisualStudio.TestTools.UnitTesting;

namespace LispUnitTests
{
    [TestClass]
    public class CsLispTokenizerTests
    {
        [TestMethod]
        public void Test_TokenizerEmptyCode()
        {
            IEnumerable<LispToken> result = LispTokenizer.Tokenize("");
            Assert.IsNotNull(result);
            Assert.AreEqual(0, result.Count());

            result = LispTokenizer.Tokenize("     \t \n   ");
            Assert.IsNotNull(result);
            Assert.AreEqual(0, result.Count());
        }

        [TestMethod]
        public void Test_Tokenizer1()
        {
            IEnumerable<LispToken> result = LispTokenizer.Tokenize("()");
            Assert.IsNotNull(result);
            Assert.AreEqual(2, result.Count());
            Assert.AreEqual("(", result.First().ToString());
            Assert.AreEqual(")", result.Last().ToString());

            result = LispTokenizer.Tokenize("  (  \n    )  ");
            Assert.IsNotNull(result);
            Assert.AreEqual(2, result.Count());
            Assert.AreEqual("(", result.First().ToString());
            Assert.AreEqual(")", result.Last().ToString());
        }

        [TestMethod]
        public void Test_Tokenizer2()
        {
            IEnumerable<LispToken> result = LispTokenizer.Tokenize("(+ 1 #t 3.1415 \"asdf blub\" #f )");
            Assert.IsNotNull(result);
            Assert.AreEqual(8, result.Count());
            var resultAsArray = result.ToArray();
            Assert.AreEqual("(", resultAsArray[0].ToString());
            Assert.AreEqual(LispTokenType.ListStart, resultAsArray[0].Type);
            Assert.AreEqual("+", resultAsArray[1].ToString());
            Assert.AreEqual(LispTokenType.Symbol, resultAsArray[1].Type);
            Assert.AreEqual(1, resultAsArray[2].Value);
            Assert.AreEqual(LispTokenType.Int, resultAsArray[2].Type);
            Assert.AreEqual(true, resultAsArray[3].Value);
            Assert.AreEqual(LispTokenType.True, resultAsArray[3].Type);
            Assert.AreEqual(3.1415, resultAsArray[4].Value);
            Assert.AreEqual(LispTokenType.Double, resultAsArray[4].Type);
            Assert.AreEqual("asdf blub", resultAsArray[5].ToString());
            Assert.AreEqual(LispTokenType.String, resultAsArray[5].Type);
            Assert.AreEqual(false, resultAsArray[6].Value);
            Assert.AreEqual(LispTokenType.False, resultAsArray[6].Type);
            Assert.AreEqual(")", resultAsArray[7].ToString());
            Assert.AreEqual(LispTokenType.ListEnd, resultAsArray[7].Type);
        }

        [TestMethod]
        public void Test_Tokenizer3()
        {
            IEnumerable<LispToken> result = LispTokenizer.Tokenize("(do (print (* 9 9)))");
            Assert.IsNotNull(result);
            Assert.AreEqual(11, result.Count());
            var resultAsArray = result.ToArray();
            Assert.AreEqual("(", resultAsArray[0].ToString());
            Assert.AreEqual("do", resultAsArray[1].ToString());
            Assert.AreEqual(LispTokenType.Symbol, resultAsArray[1].Type);
            Assert.AreEqual("(", resultAsArray[2].ToString());
            Assert.AreEqual("print", resultAsArray[3].ToString());
            Assert.AreEqual("(", resultAsArray[4].ToString());
            Assert.AreEqual("*", resultAsArray[5].ToString());
            Assert.AreEqual(9, resultAsArray[6].Value);
            Assert.AreEqual(9, resultAsArray[7].Value);
            Assert.AreEqual(")", resultAsArray[8].ToString());
            Assert.AreEqual(")", resultAsArray[9].ToString());
            Assert.AreEqual(")", resultAsArray[10].ToString());
        }

        [TestMethod]
        public void Test_Tokenizer4()
        {
            IEnumerable<LispToken> result = LispTokenizer.Tokenize("(do\n (print (* 9 9)) ; this is a comment\n)\n");
            Assert.IsNotNull(result);
            Assert.AreEqual(12, result.Count());
            var resultAsArray = result.ToArray();
            Assert.AreEqual("(", resultAsArray[0].ToString());
            Assert.AreEqual("do", resultAsArray[1].ToString());
            Assert.AreEqual("(", resultAsArray[2].ToString());
            Assert.AreEqual("print", resultAsArray[3].ToString());
            Assert.AreEqual("(", resultAsArray[4].ToString());
            Assert.AreEqual("*", resultAsArray[5].ToString());
            Assert.AreEqual(9, resultAsArray[6].Value);
            Assert.AreEqual(9, resultAsArray[7].Value);
            Assert.AreEqual(")", resultAsArray[8].ToString());
            Assert.AreEqual(")", resultAsArray[9].ToString());
            Assert.AreEqual("; this is a comment\n", resultAsArray[10].ToString());
            Assert.AreEqual(LispTokenType.Comment, resultAsArray[10].Type);
            Assert.AreEqual(")", resultAsArray[11].ToString());
        }

        [TestMethod]
        public void Test_Tokenizer5()
        {
            IEnumerable<LispToken> result = LispTokenizer.Tokenize("(test '(1 2 3))");
            Assert.IsNotNull(result);
            Assert.AreEqual(9, result.Count());
            var resultAsArray = result.ToArray();
            Assert.AreEqual("'", resultAsArray[2].ToString());
            Assert.AreEqual(LispTokenType.Quote, resultAsArray[2].Type);

            result = LispTokenizer.Tokenize("(test `(1 2 3))");
            Assert.IsNotNull(result);
            Assert.AreEqual(9, result.Count());
            resultAsArray = result.ToArray();
            Assert.AreEqual("`", resultAsArray[2].ToString());
            Assert.AreEqual(LispTokenType.QuasiQuote, resultAsArray[2].Type);

            result = LispTokenizer.Tokenize("(test ,a)");
            Assert.IsNotNull(result);
            Assert.AreEqual(5, result.Count());
            resultAsArray = result.ToArray();
            Assert.AreEqual(",", resultAsArray[2].ToString());
            Assert.AreEqual(LispTokenType.UnQuote, resultAsArray[2].Type);

            result = LispTokenizer.Tokenize("(test ,@a)");
            Assert.IsNotNull(result);
            Assert.AreEqual(5, result.Count());
            resultAsArray = result.ToArray();
            Assert.AreEqual(",@", resultAsArray[2].ToString());
            Assert.AreEqual(LispTokenType.UnQuoteSplicing, resultAsArray[2].Type);
        }

        [TestMethod]
        public void Test_Tokenizer6()
        {
            IEnumerable<LispToken> result = LispTokenizer.Tokenize("(test nil)");
            Assert.IsNotNull(result);
            Assert.AreEqual(4, result.Count());
            var resultAsArray = result.ToArray();
            Assert.AreEqual(LispToken.Nil, resultAsArray[2].ToString());
            Assert.AreEqual(LispTokenType.Nil, resultAsArray[2].Type);
        }

        [TestMethod]
        public void Test_Tokenizer7()
        {
            IEnumerable<LispToken> result = LispTokenizer.Tokenize("(test \"blub\nhello\")");
            Assert.IsNotNull(result);
            Assert.AreEqual(4, result.Count());
            var resultAsArray = result.ToArray();
            Assert.AreEqual("blub\nhello", resultAsArray[2].ToString());
            Assert.AreEqual(LispTokenType.String, resultAsArray[2].Type);
        }
    }
}
