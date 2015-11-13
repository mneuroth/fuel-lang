using System.IO;
using Microsoft.VisualStudio.TestTools.UnitTesting;
using CsLisp;

namespace LispUnitTests
{
    [TestClass]
    public class CsLispCompilerTests
    {
        private const string CompilerOutputFileName = "unittestoutput.exe";
        
        [TestMethod]
        public void Test_Compiler()
        {
            var code = "(print \"Hello world!\")";
            var result = LispCompiler.CompileToCsCode(code);
            LispCompiler.CompileCsCodeToAssembly(result.StringValue, CompilerOutputFileName);
            Assert.IsTrue(result.StringValue.Length > 0);
            //Assert.IsTrue(File.Exists(CompilerOutputFileName));
        }
    }
}
