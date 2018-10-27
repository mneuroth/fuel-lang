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
    public class CsLispParserTests
    {
        [TestMethod]
        public void Test_SingleParserEmptyCode()
        {
            object result = LispParser.Parse("()");
            Assert.IsNotNull(result);
            Assert.IsTrue(result as IEnumerable<object> != null);
            Assert.AreEqual(0, ((IEnumerable<object>)result).Count());
        }

        [TestMethod]
        public void Test_SingleParser1()
        {
            object result = LispParser.Parse("(print 1 2.54 \"string\")");
            Assert.IsNotNull(result);
            Assert.IsTrue(result as IEnumerable<object> != null);
            var resultAsArray = ((IEnumerable<object>)result).ToArray();
            Assert.AreEqual(4, resultAsArray.Length);

            var value = (LispVariant) resultAsArray[0];
            Assert.IsTrue(value.IsSymbol);
            Assert.AreEqual("print", value.Value);
            value = (LispVariant)resultAsArray[1];
            Assert.IsTrue(value.IsInt);
            Assert.AreEqual(1, value.Value);
            value = (LispVariant)resultAsArray[2];
            Assert.IsTrue(value.IsDouble);
            Assert.AreEqual(2.54, value.Value);
            value = (LispVariant)resultAsArray[3];
            Assert.IsTrue(value.IsString);
            Assert.AreEqual("string", value.Value);
        }

        [TestMethod]
        public void Test_SingleParser2()
        {
            object result = LispParser.Parse("(do (print #t 2.54 \"string\"))");
            Assert.IsNotNull(result);
            Assert.IsTrue(result as IEnumerable<object> != null);
            var resultAsArrayDo = ((IEnumerable<object>)result).ToArray();
            Assert.AreEqual(2, resultAsArrayDo.Length);

            var value = (LispVariant)resultAsArrayDo[0];
            Assert.IsTrue(value.IsSymbol);
            Assert.AreEqual("do", value.Value);

            var listValue = (IEnumerable<object>)resultAsArrayDo[1];
            var resultAsArray = listValue.ToArray();
            value = (LispVariant)resultAsArray[1];
            Assert.IsTrue(value.IsBool);
            Assert.AreEqual(true, value.Value);
            value = (LispVariant)resultAsArray[2];
            Assert.IsTrue(value.IsDouble);
            Assert.AreEqual(2.54, value.Value);
            value = (LispVariant)resultAsArray[3];
            Assert.IsTrue(value.IsString);
            Assert.AreEqual("string", value.Value);
        }
    }
}
