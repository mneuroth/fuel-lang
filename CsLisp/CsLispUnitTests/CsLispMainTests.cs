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
        private StringWriter _consoleOutput;

        private StringReader _consoleInput;

        private TextWriter _originalConsoleOutput;

        private TextReader _originalConsoleInput;

        private TestContext _context;

        private StringBuilder _stringBuilder;

        public ConsoleRedirector(string input = "", TestContext context = null)
        {
            _context = context;
            _stringBuilder = new StringBuilder("");
            _consoleOutput = new StringWriter(_stringBuilder);           
            _consoleInput = new StringReader(input);
            _originalConsoleOutput = Console.Out;
            _originalConsoleInput = Console.In;
            Console.SetOut(_consoleOutput);
            Console.SetIn(_consoleInput);
            if (_context != null)
            {
                _context.WriteLine("Init !!!!! {0} <--> {1} ### {2}", _originalConsoleOutput, _consoleOutput, _consoleInput);
            }
        }
        public void Dispose()
        {
            if (_context != null)
            {
                _context.WriteLine("Dispose !!!!! {0} <--> {1} ### {2}", _originalConsoleOutput, _consoleOutput, _consoleInput);
            }
            Console.SetOut(_originalConsoleOutput);
            Console.SetIn(_originalConsoleInput);
            Console.WriteLine(ToString());
            _consoleOutput.Dispose();
            _consoleInput.Dispose();
        }
        public override string ToString()
        {
            if (_context != null)
            {
                _context.WriteLine("ToString() !!!!! {0} xxx", _consoleOutput);
            }
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
        public void Test_Main()
        {
            using (ConsoleRedirector cr = new ConsoleRedirector(context: TestContext))
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
                var args = new[] { "-l", "error.fuel" };
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
                Assert.IsTrue(s.StartsWith(Lisp.Version));
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
        [DeploymentItem(@"..\..\..\TestData\simple.fuel")]
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
            using (ConsoleRedirector cr = new ConsoleRedirector("help\nq\n", context: TestContext))
            {
                var args = new[] { "-i" };
                Fuel.Main(args);
                string s = cr.ToString().Trim();
                // TODO: strange behaviour: in debug ok in run mode --> error ? string is empty ?
                if (s.Length > 0)
                {
                    Assert.IsTrue(s.Contains("DBG>"));
                    Assert.IsTrue(s.Contains("Type \"help\" for informations."));
                    Assert.IsTrue(s.Contains("help for interactive loop:")); // help
                }
                else
                {
                    Assert.IsTrue(false);
                }
            }
        }

//        [TestMethod]
//        [Ignore]
//        public void Test_MainDebuggerExecuteOrg()
//        {
//            using (ConsoleRedirector cr = new ConsoleRedirector("r\nhelp\nb 3\nb 4 (= a 42)\nr\nr\no\ns\n\nrestart\nv\nr\nb 3\nclear 3\nlist\nstack\nglobals\nlocals\ncode\nfuncs\nq\n"))
//            {
//                const string script = @"(do 
//(def a 42) 
//(print (+ 1 2)) 
//(print (* 3 4 5)))";

//                TestContext.WriteLine("Hello world org !!!!!!!!!!!!!!");

//                var args = new[] { "-d", /*"-e",*/ script };
//                Fuel.Main(args);
//                string s = cr.ToString().Trim();
//                TestContext.WriteLine("Result=" + s);
//// TODO: strange behaviour: in debug ok in run mode --> error ? string is empty ?
//                // but only if compile tests are enabled !!!???
//                if (s.Length > 0)
//                {
//                    Assert.IsTrue(s.Contains("DBG>"));
//                    Assert.IsTrue(s.Contains("Type \"help\" for informations."));
//                    Assert.IsTrue(s.Contains("--> do line=1 pos=1 line=1"));
//                    Assert.IsTrue(s.Contains("help for interactive loop:")); // help
//                    Assert.IsTrue(s.Contains("#2   line=4     module=command-line              condition=(= a 42)")); // list
//                    Assert.IsTrue(s.Contains("-->    1 <main> lineno=3 module=command-line")); // stack
//                    Assert.IsTrue(s.Contains("a --> 42                                       : Int")); // locals / globals                               
//                    Assert.IsTrue(s.Contains("(def a 42)")); // code
//                    Assert.IsTrue(s.Contains("print --> function <unknown>                       : Function  : module=<builtin>")); // funcs                    
//                }
//                else
//                {
//                    Assert.IsTrue(false);
//                }
//            }
//        }

        [TestMethod]
        public void Test_MainDebuggerExecute()
        {
            //using (ConsoleRedirector cr = new ConsoleRedirector("r\nhelp\nb 3\nb 4 (= a 42)\nr\nr\no\ns\n\nrestart\nv\nr\nb 3\nclear 3\nlist\nstack\nglobals\nlocals\ncode\nfuncs\nq\n"))
            {
                const string script = @"(do 
(def a 42) 
(print (+ 1 2)) 
(print (* 3 4 5)))";

                TestContext.WriteLine("Hello world !!!!!!!!!!!!!!");

                var input = new StringReader("r\nhelp\nb 3\nb 4 (= a 42)\nr\nr\no\ns\n\nrestart\nv\nr\nb 3\nclear 3\nlist\nstack\nglobals\nlocals\ncode\nfuncs\nq\n");
                var output = new StringWriter();

                var args = new[] { "-d", "-e", script };
                Fuel.MainExtended(args, output, input);
                output.Flush();
                StringBuilder result = output.GetStringBuilder();
                TestContext.WriteLine("Result="+result.ToString());
                string s = result.ToString(); // output.ToString().Trim();
// TODO: strange behaviour: in debug ok in run mode --> error ? string is empty ?
                // but only if compile tests are enabled !!!???
                if (s.Length > 0)
                {
                    Assert.IsTrue(s.Contains("DBG>"));
                    Assert.IsTrue(s.Contains("Type \"help\" for informations."));
                    Assert.IsTrue(s.Contains("--> do line=1 start=1 stop=3"));
                    Assert.IsTrue(s.Contains("help for interactive loop:")); // help
                    Assert.IsTrue(s.Contains("#2   line=4     module=command-line              condition=(= a 42)")); // list
                    Assert.IsTrue(s.Contains("-->    1 <main> lineno=3 module=command-line")); // stack
                    Assert.IsTrue(s.Contains("a --> 42                                       : Int")); // locals / globals                               
                    Assert.IsTrue(s.Contains("(def a 42)")); // code
                    Assert.IsTrue(s.Contains("print --> function <unknown>                       : Function  : module=<builtin>")); // funcs                    
                }
                else
                {
                    Assert.IsTrue(false);
                }

                input.Close();
                output.Close();
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
                Assert.IsTrue(s.Contains("Bad argument count in def --> line=1 start=15 stop=19"));
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
