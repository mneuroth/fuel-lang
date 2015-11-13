using System;
using System.IO;
using Microsoft.VisualStudio.TestTools.UnitTesting;
using CsLisp;

namespace LispUnitTests
{
    // see: http://stackoverflow.com/questions/4884043/how-to-write-to-console-out-during-execution-of-an-mstest-test
    internal class ConsoleRedirector : IDisposable
    {
        private StringWriter _consoleOutput = new StringWriter();

        private StringReader _ConsoleInput = null;

        private TextWriter _originalConsoleOutput;

        private TextReader _originalConsoleInput;

        public ConsoleRedirector(string input = "")
        {
            _ConsoleInput = new StringReader(input);
            _originalConsoleOutput = Console.Out;
            _originalConsoleInput = Console.In;
            Console.SetOut(_consoleOutput);
            Console.SetIn(_ConsoleInput);
        }
        public void Dispose()
        {
            Console.SetOut(_originalConsoleOutput);
            Console.SetIn(_originalConsoleInput);
            Console.Write(ToString());
            _consoleOutput.Dispose();
            _ConsoleInput.Dispose();
        }
        public override string ToString()
        {
            return _consoleOutput.ToString();
        }
    }

    [TestClass]
    public class CsLispMain
    {
        [TestMethod]
        public void Test_Main()
        {
            using (ConsoleRedirector cr = new ConsoleRedirector())
            {
                var args = new string[0];
                Fuel.Main(args);
                string s = cr.ToString().Trim();
                Assert.AreEqual(true, s.StartsWith(Lisp.Name));
            }
        }

        [TestMethod]
        public void Test_MainHelp()
        {
            using (ConsoleRedirector cr = new ConsoleRedirector())
            {
                var args = new[] {"-h"};
                Fuel.Main(args);
                string s = cr.ToString().Trim();
                Assert.AreEqual(true, s.StartsWith(Lisp.Name));
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
                Assert.AreEqual(true, s.StartsWith(Lisp.Version));
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
                Assert.AreEqual(true, s == "3");
            }
        }

        [TestMethod]
        public void Test_MainDebuggerExecute()
        {
            using (ConsoleRedirector cr = new ConsoleRedirector("r\nhelp\nb 3\nb 4 (= a 42)\nr\nr\no\ns\n\nrestart\nv\nr\nb 3\nclear 3\nlist\nstack\nglobals\nlocals\ncode\nfuncs\nq\n"))
            {
                var args = new[] { "-d", "(do\n (def a 42)\n(print (+ 1 2))\n(print (* 3 4 5)))" };
                Fuel.Main(args);
                string s = cr.ToString().Trim();
                Assert.AreEqual(true, s.Contains("DBG>"));
                Assert.AreEqual(true, s.Contains("Type \"help\" for informations."));
                Assert.AreEqual(true, s.Contains("--> do pos=1 line=1"));
                Assert.AreEqual(true, s.Contains("help for interactive loop:"));        // help
                Assert.AreEqual(true, s.Contains("line 4     condition: (= a 42)"));    // list
                Assert.AreEqual(true, s.Contains("-->    1 <main>"));                   // stack
                Assert.AreEqual(true, s.Contains("a --> 42                                            : Int"));     // locals / globals                               
                Assert.AreEqual(true, s.Contains("(def a 42)"));                        // code
                Assert.AreEqual(true, s.Contains("print --> function <unknown>                            : Function"));    // funcs
            }
        }
    }
}
