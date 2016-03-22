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

using System;
using System.IO;
using System.Text;
using CsLisp;
using Microsoft.VisualStudio.TestTools.UnitTesting;

namespace LispUnitTests
{
    // see: http://stackoverflow.com/questions/4884043/how-to-write-to-console-out-during-execution-of-an-mstest-test
    internal class ConsoleRedirector : IDisposable
    {
        private readonly StringWriter _consoleOutput;

        private readonly StringReader _consoleInput;

        private readonly TextWriter _originalConsoleOutput;

        private readonly TextReader _originalConsoleInput;

        public ConsoleRedirector(string input = "")
        {
            StringBuilder stringBuilder = new StringBuilder("");
            _consoleOutput = new StringWriter(stringBuilder);           
            _consoleInput = new StringReader(input);
            _originalConsoleOutput = Console.Out;
            _originalConsoleInput = Console.In;
            Console.SetOut(_consoleOutput);
            Console.SetIn(_consoleInput);
        }
        public void Dispose()
        {
            Console.SetOut(_originalConsoleOutput);
            Console.SetIn(_originalConsoleInput);
            Console.WriteLine(ToString());
            _consoleOutput.Dispose();
            _consoleInput.Dispose();
        }
        public override string ToString()
        {
            _consoleOutput.Flush();
            return _consoleOutput.ToString();
        }
    }

    [TestClass]
    public class CsLispMainTests
    {
        /// <summary>
        /// Gets or sets the test context which provides
        /// information about and functionality for the current test run.
        /// </summary>
        public TestContext TestContext { get; set; }

        [TestMethod]
        public void Test_Debugger()
        {
            LispDebugger debugger = new LispDebugger();
            Assert.IsNotNull(debugger);
        }

        [TestMethod]
        public void Test_Main()
        {
            using (ConsoleRedirector cr = new ConsoleRedirector())
            {
                var args = new string[0];
                Fuel.Main(args);
                string s = cr.ToString().Trim();
                Assert.IsTrue(s.StartsWith(Lisp.Name));
            }
        }

        [TestMethod]
        [DeploymentItem(@"..\..\..\Scripts\simple.fuel")]
        public void Test_MainFile()
        {
            using (ConsoleRedirector cr = new ConsoleRedirector())
            {
                var args = new[] { "simple.fuel" };
                Fuel.Main(args);
                string s = cr.ToString().Trim();
                Assert.IsTrue(s.StartsWith("hello world !"));
            }
        }

        [TestMethod]
        [DeploymentItem(@"..\..\..\Scripts\error.fuel")]
        public void Test_MainFileError()
        {
            using (ConsoleRedirector cr = new ConsoleRedirector())
            {
                var args = new[] { "error.fuel" };
                Fuel.Main(args);
                string s = cr.ToString().Trim();
                Assert.IsTrue(s.Contains("Error executing script"));
                Assert.IsTrue(s.Contains("printx"));
                Assert.IsTrue(s.Contains("not found"));
            }
        }

        [TestMethod]
        [DeploymentItem(@"..\..\..\Scripts\error.fuel")]
        public void Test_MainFileErrorDetailed()
        {
            using (ConsoleRedirector cr = new ConsoleRedirector())
            {
                var args = new[] { "-x", "error.fuel" };
                Fuel.Main(args);
                string s = cr.ToString().Trim();
                Assert.IsTrue(s.Contains("Error executing script"));
                Assert.IsTrue(s.Contains("printx"));
                Assert.IsTrue(s.Contains("not found"));
                Assert.IsTrue(s.Contains("Callstack"));
                Assert.IsTrue(s.Contains("Exception in"));
            }
        }

        [TestMethod]
        public void Test_MainHelp()
        {
            using (ConsoleRedirector cr = new ConsoleRedirector())
            {
                var args = new[] { "-h" };
                Fuel.Main(args);
                string s = cr.ToString().Trim();
                Assert.IsTrue(s.StartsWith(Lisp.Name));
            }
        }

        [TestMethod]
        public void Test_MainVersion()
        {
            using (ConsoleRedirector cr = new ConsoleRedirector())
            {
                var args = new[] { "-v" };
                Fuel.Main(args);
                string s = cr.ToString().Trim();
                Assert.IsTrue(s.StartsWith(Lisp.ProgramName + " " + Lisp.Version + " from " + Lisp.Date));
            }
        }

        [TestMethod]
        public void Test_MainExecute()
        {
            using (ConsoleRedirector cr = new ConsoleRedirector())
            {
                var args = new[] { "-e", "(print (+ 1 2))" };
                Fuel.Main(args);
                string s = cr.ToString().Trim();
                Assert.IsTrue(s == "3");
            }
        }

        [TestMethod]
        public void Test_MainInteractiveImport()
        {
            using (ConsoleRedirector cr = new ConsoleRedirector("(import fuellib)\nmacros\nmodules\nfuncs\n"))
            {
                var args = new[] { "-i" };
                Fuel.Main(args);
                string s = cr.ToString().Trim();
                Assert.IsTrue(s.Contains(@".\Library\fuellib.fuel"));
                Assert.IsTrue(s.Contains(@"Dict-Remove --> function <unknown>                       : Function  : module=.\Library\fuellib.fuel"));
            }
        }

        [TestMethod]
        [DeploymentItem(@"..\..\..\TestData\simple.fuel")]
        [Ignore]    // does not work under .NET 3.5 ! why ?
        public void Test_Compile()
        {
            using (/*ConsoleRedirector cr =*/ new ConsoleRedirector())
            {
                var args = new[] { "-c", "simple.fuel" };
                Fuel.Main(args);
                Assert.IsTrue(File.Exists("simple.fuel.exe"));
            }
        }

        [TestMethod]
        [DeploymentItem(@"..\..\..\TestData\simple.fuel")]
        public void Test_Profile()
        {
            using (ConsoleRedirector cr = new ConsoleRedirector())
            {
                var args = new[] { "-m", "simple.fuel" };
                Fuel.Main(args);
                string s = cr.ToString().Trim();
                Assert.IsTrue(s.Contains("Execution time ="));
            }
        }

        [TestMethod]
        [DeploymentItem(@"..\..\..\TestData\simple.fuel")]
        public void Test_Trace()
        {
            using (ConsoleRedirector cr = new ConsoleRedirector())
            {
                var args = new[] { "-t", "simple.fuel" };
                Fuel.Main(args);
                string s = cr.ToString().Trim();
                Assert.IsTrue(s.Contains("--> do"));
                Assert.IsTrue(s.Contains("--> print"));
            }
        }

        [TestMethod]
        [DeploymentItem(@"..\..\..\TestData\simple.fuel")]
        public void Test_CompileOutput()
        {
            using (ConsoleRedirector cr = new ConsoleRedirector())
            {
                var args = new[] { "-s", "simple.fuel" };
                Fuel.Main(args);
                string s = cr.ToString().Trim();
                Assert.IsTrue(s.Contains("public static void Main(string[] args)"));
            }
        }

        [TestMethod]
        [DeploymentItem(@"..\..\..\TestData\controlflow.fuel")]
        public void Test_Compile2()
        {
            using (/*ConsoleRedirector cr =*/ new ConsoleRedirector())
            {
                var args = new[] { "-c", "controlflow.fuel" };
                Fuel.Main(args);
// TODO --> does not work yet
                Assert.IsFalse(File.Exists("controlflow.fuel.exe"));
            }
        }

        [TestMethod]
        public void Test_MainInteractive()
        {
            using (var cr = new ConsoleRedirector("help\nfuncs\nbuiltins\nq\n"))
            {
                var args = new[] { "-i" };
                Fuel.Main(args);

                string result = cr.ToString();
                TestContext.WriteLine("Result=" + result);
                string s = result;
                Assert.IsTrue(s.Contains("DBG>"));
                Assert.IsTrue(s.Contains("Type \"help\" for informations."));
                Assert.IsTrue(s.Contains("help for interactive loop:")); // help
                Assert.IsTrue(s.Contains("equal --> function <unknown>                       : Function  : module=<builtin>"));
                Assert.IsTrue(s.Contains("define-macro --> function <unknown>                       : Function"));
            }
        }

        [TestMethod]
        public void Test_MainDebuggerExecute()
        {
            using (ConsoleRedirector cr = new ConsoleRedirector("r\nhelp\nb 3\nb 4 (= a 42)\nr\nr\no\ns\n\nrestart\nv\nr\nb 3\nclear 3\nlist\nstack\nglobals\nlocals\ncode\nfuncs\nq\n"))
            {
                const string script = @"(do 
                    (def a 42) 
                    (print (+ 1 2)) 
                    (print (* 3 4 5)))";

                var args = new[] { "-d", "-e", script };
                Fuel.Main(args);
                string s = cr.ToString().Trim();
                Assert.IsTrue(s.Contains("DBG>"));
                Assert.IsTrue(s.Contains("Type \"help\" for informations."));
                Assert.IsTrue(s.Contains("--> do line=1 start=1 stop=3"));
                Assert.IsTrue(s.Contains("help for interactive loop:")); // help
                Assert.IsTrue(s.Contains("#2   line=4     module=command-line              condition=(= a 42)")); // list
                Assert.IsTrue(s.Contains("-->    1 name=<main>                              lineno=3    module=command-line")); // stack
                Assert.IsTrue(s.Contains("a --> 42                                       : Int")); // locals / globals                               
                Assert.IsTrue(s.Contains("(def a 42)")); // code
                Assert.IsTrue(s.Contains("print --> function <unknown>                       : Function  : module=<builtin>")); // funcs                    
            }
        }

        [TestMethod]
        [DeploymentItem(@"..\..\..\TestData\testdebugger.fuel")]
        public void Test_DebugFile()
        {
            using (ConsoleRedirector cr = new ConsoleRedirector("b 4\nr\nl\nk\nlist\ndown\nk\nup"))
            {
                var args = new[] { "-d", "testdebugger.fuel" };
                Fuel.Main(args);
                string s = cr.ToString().Trim();
                Assert.IsTrue(s.Contains("FUEL(isp)-DBG>                    x --> 4                                        : Int "));
                Assert.IsTrue(s.Contains("       1 name=<main>                              lineno=11   module=testdebugger.fuel"));
                Assert.IsTrue(s.Contains("       2 name=g                                   lineno=8    module=testdebugger.fuel"));
                Assert.IsTrue(s.Contains("-->    3 name=f                                   lineno=4    module=testdebugger.fuel"));
                Assert.IsTrue(s.Contains("FUEL(isp)-DBG> Breakpoints:"));
                Assert.IsTrue(s.Contains("#1   line=4     module=testdebugger.fuel         condition="));
                Assert.IsTrue(s.Contains("-->    2 name=g                                   lineno=8    module=testdebugger.fuel"));
            }
        }

        [TestMethod]
        [DeploymentItem(@"..\..\..\TestData\test.fuel")]
        [DeploymentItem(@"..\..\..\TestData\testmodule.fuel")]
        public void Test_DebugModule()
        {
            using (ConsoleRedirector cr = new ConsoleRedirector("b .\\testmodule.fuel:4\nlist\nr\nk\nl"))
            {
                var args = new[] { "-d", "test.fuel", "-l=." };
                Fuel.Main(args);
                string s = cr.ToString().Trim();
                Assert.IsTrue(s.Contains("FUEL(isp)-DBG> Breakpoints:"));
                Assert.IsTrue(s.Contains("#1   line=4     module=.\\testmodule.fuel         condition="));
                Assert.IsTrue(s.Contains("       1 name=<main>                              lineno=4    module=test.fuel"));
                Assert.IsTrue(s.Contains("-->    2 name=blub                                lineno=4    module=.\\testmodule.fuel"));
                Assert.IsTrue(s.Contains("x --> 8                                        : Int"));
            }
        }

        #region parser tests

        [TestMethod]
        public void Test_MainTestParserBracketsOutOfBalance()
        {
            using (ConsoleRedirector cr = new ConsoleRedirector())
            {
                var args = new[] { "-e", "(print ( (+ 1 2))" };
                Fuel.Main(args);
                string s = cr.ToString().Trim();
                Assert.IsTrue(s.Contains("Brackets out of balance --> line=1 start=16 stop=16 module="));
            }
        }

        [TestMethod]
        public void Test_MainTestParserUnexpectedToken()
        {
            using (ConsoleRedirector cr = new ConsoleRedirector())
            {
                var args = new[] { "-e", "dummy (print (+ 1 2))" };
                Fuel.Main(args);
                string s = cr.ToString().Trim();
                Assert.IsTrue(s.Contains("Unexpected token --> line=1 start=0 stop=5"));
            }
        }

        [TestMethod]
        public void Test_MainTestFunctionNotFound()
        {
            using (ConsoleRedirector cr = new ConsoleRedirector())
            {
                var args = new[] { "-e", "(unknown-fcn (+ 1 2))" };
                Fuel.Main(args);
                string s = cr.ToString().Trim();
                Assert.IsTrue(s.Contains("Function \"unknown-fcn\" not found --> line=1 start=1 stop=12"));
            }
        }

        [TestMethod]
        public void Test_MainTestSymbolNotFound()
        {
            using (ConsoleRedirector cr = new ConsoleRedirector())
            {
                var args = new[] { "-e", "(setf a 5)" };
                Fuel.Main(args);
                string s = cr.ToString().Trim();
                Assert.IsTrue(s.Contains("Symbol a not found --> line=1 start=1 stop=5"));
            }
        }

        [TestMethod]
        public void Test_MainTestListExpectedInDo()
        {
            using (ConsoleRedirector cr = new ConsoleRedirector())
            {
                var args = new[] { "-e", "(do (print 3) 5)" };
                Fuel.Main(args);
                string s = cr.ToString().Trim();
                Assert.IsTrue(s.Contains("List expected in do --> line=1 start=13 stop=15"));
            }
        }

        [TestMethod]
        public void Test_MainTestBadArgumentCount()
        {
            using (ConsoleRedirector cr = new ConsoleRedirector())
            {
                var args = new[] { "-e", "(do (print 3) (defn x))" };
                Fuel.Main(args);
                string s = cr.ToString().Trim();
                Assert.IsTrue(s.Contains("Bad argument count in def, has 1 expected 3 --> line=1 start=15 stop=19"));
            }
        }

        [TestMethod]
        public void Test_MainTestNoFunction()
        {
            using (ConsoleRedirector cr = new ConsoleRedirector())
            {
                var args = new[] { "-e", "(do (print 3) (map 3 '(1 2 3)))" };
                Fuel.Main(args);
                string s = cr.ToString().Trim();
                Assert.IsTrue(s.Contains("No function in map --> line=1 start=15 stop=18"));
            }
        }

        [TestMethod]
        public void Test_MainTestNoList()
        {
            using (ConsoleRedirector cr = new ConsoleRedirector())
            {
                var args = new[] { "-e", "(do (print 3) (map (lambda (x) (print x)) 3))" };
                Fuel.Main(args);
                string s = cr.ToString().Trim();
                Assert.IsTrue(s.Contains("No list in map --> line=1 start=40 stop=40"));
            }
        }

        [TestMethod]
        public void Test_MainTestSymbolExpected()
        {
            using (ConsoleRedirector cr = new ConsoleRedirector())
            {
                var args = new[] { "-e", "(do (print 3) (def 4 \"test\"))" };
                Fuel.Main(args);
                string s = cr.ToString().Trim();
                Assert.IsTrue(s.Contains("Symbol expected --> line=1 start=15 stop=18"));
            }
        }

        #endregion
    }
}
