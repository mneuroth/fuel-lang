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
            IEnumerable<object> result = LispParser.Parse("()");
            Assert.IsNotNull(result);
            Assert.AreEqual(0, result.Count());
        }

        [TestMethod]
        public void Test_SingleParser1()
        {
            IEnumerable<object> result = LispParser.Parse("(print 1 2.54 \"string\")");
            Assert.IsNotNull(result);
            Assert.AreEqual(4, result.Count());

            var resultAsArray = result.ToArray();
            LispVariant value = (LispVariant) resultAsArray[0];
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
            IEnumerable<object> result = LispParser.Parse("(do (print #t 2.54 \"string\"))");
            Assert.IsNotNull(result);
            Assert.AreEqual(2, result.Count());

            var resultAsArrayDo = result.ToArray();

            LispVariant value = (LispVariant)resultAsArrayDo[0];
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
