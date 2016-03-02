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
        [ExpectedException(typeof(LispException))]
        public void Test_VariantCastError()
        {
            LispVariant variant = new LispVariant(4.3);
            Assert.IsNotNull(variant);
            int value = variant.IntValue;
        }
    }
}
