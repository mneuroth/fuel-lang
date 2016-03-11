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

using CsLisp;
using Microsoft.VisualStudio.TestTools.UnitTesting;

namespace LispUnitTests
{
    [TestClass]
    public class CsLispVariantTest
    {
        [TestMethod]
        public void Test_CreateVariant()
        {
            LispVariant variant = new LispVariant();
            Assert.IsNotNull(variant);

            variant = new LispVariant(3);
            Assert.IsTrue(variant.IsInt);
            Assert.AreEqual(3, variant.IntValue);

            variant = new LispVariant(3.1415);
            Assert.IsTrue(variant.IsDouble);
            Assert.AreEqual(3.1415, variant.DoubleValue);

            variant = new LispVariant("text");
            Assert.IsTrue(variant.IsString);
        }

        [TestMethod]
        public void Test_VariantCompare()
        {
            LispVariant variant1 = new LispVariant(4.3);
            LispVariant variant2 = new LispVariant(56.1);
            LispVariant variant3 = new LispVariant(42);
            LispVariant variant4 = new LispVariant("abc");
            Assert.IsTrue(variant1.CompareTo(variant2) < 0);
            Assert.IsTrue(variant2.CompareTo(variant1) > 0);
            Assert.IsTrue(variant1.CompareTo(1.23) > 0);
            Assert.IsTrue(variant1.CompareTo(-5) > 0);
            Assert.IsTrue(variant3.CompareTo(42) == 0);
            Assert.IsTrue(variant4.CompareTo("abc") == 0);
            Assert.IsTrue(variant4.CompareTo("xyz") < 0);
        }

        [TestMethod]
        public void Test_VariantConvert()
        {
            LispVariant variant1 = new LispVariant(4.3);
            LispVariant variant2 = new LispVariant(56.1);
            LispVariant variant3 = new LispVariant(42);
            LispVariant variant4 = new LispVariant("4.5");
            LispVariant variant5 = new LispVariant(true);
            LispVariant variant6 = new LispVariant(LispType.Int, (object)0);
            Assert.AreEqual(true, variant1.ToBool());
            Assert.AreEqual(true, variant3.ToBool());
            Assert.AreEqual(false, variant6.ToBool());
            Assert.AreEqual(4.5, variant4.ToDouble());
            Assert.AreEqual(1.0, variant5.ToDouble());
            Assert.AreEqual(56, variant2.ToInt());
            Assert.AreEqual(true, variant2.ToBool());
        }

        [TestMethod]
        public void Test_VariantOperations()
        {
            LispVariant variant1 = new LispVariant(4.3);
            LispVariant variant2 = new LispVariant(56.1);
            LispVariant variant3 = new LispVariant(42);
            LispVariant variant4 = new LispVariant(45);
            Assert.AreEqual(1890, (variant3 * variant4).ToInt());
            Assert.AreEqual(60.4, (variant1 + variant2).ToDouble());
        }

        [TestMethod]
        public void Test_VariantEqualOp()
        {
            LispVariant variant1 = new LispVariant(4.3);
            LispVariant variant2 = new LispVariant(56.1);
            Assert.IsFalse(LispVariant.EqualOp(variant1, variant2));
            Assert.IsTrue(LispVariant.EqualOp(variant1, variant1));
        }

        [TestMethod]
        [ExpectedException(typeof(LispException))]
        public void Test_VariantCastError()
        {
            LispVariant variant = new LispVariant(4.3);
            Assert.IsNotNull(variant);
            int value = variant.IntValue;
            Assert.AreEqual(4, value);      // will not be evaluated because of expected exception !
        }
    }
}
