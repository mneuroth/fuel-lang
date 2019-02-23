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
                LispMainHelper.MainExtended(args, Console.Out, Console.In);
                string s = cr.ToString().Trim();
                Assert.IsTrue(s.StartsWith(Lisp.Name));
            }
        }

        [TestMethod]
        public void Test_MainFileWithWrongCommandLineOption()
        {
            using (ConsoleRedirector cr = new ConsoleRedirector())
            {
                var args = new[] { "--blub" };
                LispMainHelper.MainExtended(args, Console.Out, Console.In);
                string s = cr.ToString().Trim();
                Assert.IsTrue(s.StartsWith("Error: unknown option(s) --blub"));
            }
        }

        [TestMethod]
        public void Test_MainFileWithMoreThanOneLibraryOption()
        {
            using (ConsoleRedirector cr = new ConsoleRedirector())
            {
                var args = new[] { "-l=.", "-l=some/path" };
                LispMainHelper.MainExtended(args, Console.Out, Console.In);
                string s = cr.ToString().Trim();
                Assert.IsTrue(s.StartsWith("Error: only one library path is supported"));
            }
        }

        [TestMethod]
        [DeploymentItem(@"..\..\..\Scripts\simple.fuel")]
        public void Test_MainFile()
        {
            using (ConsoleRedirector cr = new ConsoleRedirector())
            {
                var args = new[] { "simple.fuel" };
                LispMainHelper.MainExtended(args, Console.Out, Console.In);
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
                LispMainHelper.MainExtended(args, Console.Out, Console.In);
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
                LispMainHelper.MainExtended(args, Console.Out, Console.In);
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
                LispMainHelper.MainExtended(args, Console.Out, Console.In);
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
                LispMainHelper.MainExtended(args, Console.Out, Console.In);
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
                LispMainHelper.MainExtended(args, Console.Out, Console.In);
                string s = cr.ToString().Trim();
                Assert.IsTrue(s == "3");
            }
        }

        [TestMethod]
        public void Test_MainExecuteAutoBlockDecorate()
        {
            using (ConsoleRedirector cr = new ConsoleRedirector())
            {
                var args = new[] { "-e", "(println \"hello world\") (println \"done.\")" };
                LispMainHelper.MainExtended(args, Console.Out, Console.In);
                string s = cr.ToString().Trim();
                Assert.IsTrue(s.Contains("hello world"));
                Assert.IsTrue(s.Contains("done"));
            }
        }

        [TestMethod]
        public void Test_MainInteractiveImport()
        {
            using (ConsoleRedirector cr = new ConsoleRedirector("(import fuellib)\nmacros\nmodules\nfuncs\n"))
            {
                var args = new[] { "-i" };
                LispMainHelper.MainExtended(args, Console.Out, Console.In);
                string s = cr.ToString().Trim();
                Assert.IsTrue(s.Contains(@".\Library\fuellib.fuel"));
                Assert.IsTrue(s.Contains(@"Dict-Remove --> function (Dict-Remove obj p0)            : Function  : module=.\Library\fuellib.fuel"));
            }
        }

        [TestMethod]
        public void Test_MainInteractiveDoc()
        {
            using (ConsoleRedirector cr = new ConsoleRedirector("doc\ndoc if"))
            {
                var args = new[] { "-i" };
                LispMainHelper.MainExtended(args, Console.Out, Console.In);
                string s = cr.ToString().Trim();
                Assert.IsTrue(s.Contains(@"doc --> (doc functionname ...)"));
                Assert.IsTrue(s.Contains(@"Returns and shows the documentation of all builtin functions or for the given function name(s)."));
                Assert.IsTrue(s.Contains(@"-------------------------------"));
            }
        }

        [TestMethod]
        public void Test_MainInteractiveSearchDoc()
        {
            using (ConsoleRedirector cr = new ConsoleRedirector("searchdoc arg"))
            {
                var args = new[] { "-i" };
                LispMainHelper.MainExtended(args, Console.Out, Console.In);
                string s = cr.ToString().Trim();
                Assert.IsTrue(s.Contains(@"Syntax: (argscount)"));
                Assert.IsTrue(s.Contains(@"Syntax: (args)"));
                Assert.IsTrue(s.Contains(@"Syntax: (arg number)"));
            }
        }

        [TestMethod]
        [DeploymentItem(@"..\..\..\TestData\multiprintln.fuel")]
        public void Test_MultiPrintLn()
        {
            using (ConsoleRedirector cr = new ConsoleRedirector())
            {
                var args = new[] { "multiprintln.fuel" };
                LispMainHelper.MainExtended(args, Console.Out, Console.In);
                string s = cr.ToString().Trim();
                Assert.IsTrue(s.Contains("hello\nworld\ndone."));
            }
        }

        [TestMethod]
        [DeploymentItem(@"..\..\..\Library\fuellib.fuel", "Library")]
        [DeploymentItem(@"..\..\..\TestData\writereadfile.fuel")]
        public void Test_WriteAndReadFile()
        {
            using (ConsoleRedirector cr = new ConsoleRedirector())
            {
                var args = new[] { "writereadfile.fuel" };
                LispMainHelper.MainExtended(args, Console.Out, Console.In);
                string s = cr.ToString().Trim();
                Assert.IsTrue(s.Contains("exists file =  #t"));
                Assert.IsTrue(s.Contains("test non existing file =  #f"));
                Assert.IsTrue(s.Contains("is equal =  #t"));
            }
        }

        [TestMethod]
        [DeploymentItem(@"..\..\..\Library\fuellib.fuel", "Library")]
        [DeploymentItem(@"..\..\..\TestData\teststdlib.fuel")]
        public void Test_StdLibObjects()
        {
            using (ConsoleRedirector cr = new ConsoleRedirector())
            {
                var args = new[] {"teststdlib.fuel"};
                LispMainHelper.MainExtended(args, Console.Out, Console.In);
                string s = cr.ToString().Trim();
                Assert.IsTrue(s.Contains("DictCount= 2"));
                Assert.IsTrue(s.Contains("NewDictCount= 0"));
                Assert.IsTrue(s.Contains("DirListType= List"));
                Assert.IsTrue(s.Contains(@"File= .\FuelCompiler.dll"));
                Assert.IsTrue(s.Contains(@"File= .\FuelDebugger.dll"));
                Assert.IsTrue(s.Contains(@"File= .\FuelInterpreter.dll"));
                //Assert.IsTrue(s.Contains(@"File= .\fuel.exe"));
                Assert.IsTrue(s.Contains(@"File= .\teststdlib.fuel"));
                Assert.IsTrue(s.Contains("ListCount= 4"));
                Assert.IsTrue(s.Contains("item= {  }"));
                Assert.IsTrue(s.Contains("newitem= 12"));
                Assert.IsTrue(s.Contains("NewListCount= 0"));
                Assert.IsTrue(s.Contains("ArrayCount= 5"));
                Assert.IsTrue(s.Contains("ArrayItem1= 1"));
                Assert.IsTrue(s.Contains("ArrayItem2= blub"));
                Assert.IsTrue(s.Contains("ArrayItem3= #t"));
                Assert.IsTrue(s.Contains("ArrayItem4= 42"));
                Assert.IsTrue(s.Contains("ArrayItem5= 123"));
            }
        }

        [TestMethod]
        [DeploymentItem(@"..\..\..\TestData\simple.fuel")]
        public void Test_Compile()
        {
            using (/*ConsoleRedirector cr =*/ new ConsoleRedirector())
            {
                var args = new[] { "-c", "simple.fuel" };
                LispMainHelper.MainExtended(args, Console.Out, Console.In);
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
                LispMainHelper.MainExtended(args, Console.Out, Console.In);
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
                LispMainHelper.MainExtended(args, Console.Out, Console.In);
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
                LispMainHelper.MainExtended(args, Console.Out, Console.In);
                string s = cr.ToString().Trim();
                Assert.IsTrue(s.Contains("public static void Main(string[] args)"));
            }
        }

        [TestMethod]
        [DeploymentItem(@"..\..\..\TestData\controlflow.fuel")]
        public void Test_Compile2()
        {
            // test offline with:
            // fuel -s controlflow.fuel >controlflow.cs
            // C:\Windows\Microsoft.NET\Framework\v3.5\csc controlflow.cs /reference:cslispinterpreter.dll
            using (/*ConsoleRedirector cr =*/ new ConsoleRedirector())
            {
                var args = new[] { "-c", "controlflow.fuel" };
                LispMainHelper.MainExtended(args, Console.Out, Console.In);
                Assert.IsTrue(File.Exists("controlflow.fuel.exe"));
            }
        }

        [TestMethod]
        public void Test_MainInteractive()
        {
            using (var cr = new ConsoleRedirector("help\nfuncs\nbuiltins\nq\n"))
            {
                var args = new[] { "-i" };
                LispMainHelper.MainExtended(args, Console.Out, Console.In);

                string result = cr.ToString();
                TestContext.WriteLine("Result=" + result);
                string s = result;
                Assert.IsTrue(s.Contains("DBG>"));
                Assert.IsTrue(s.Contains("Type \"help\" for informations."));
                Assert.IsTrue(s.Contains("help for interactive loop:")); // help
                Assert.IsTrue(s.Contains("equal --> function (equal expr1 expr2)             : Function"));
                Assert.IsTrue(s.Contains("define-macro --> function (define-macro name (arguments) statement) : Function  : module=<builtin>"));
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
                LispMainHelper.MainExtended(args, Console.Out, Console.In);
                string s = cr.ToString().Trim();
                Assert.IsTrue(s.Contains("DBG>"));
                Assert.IsTrue(s.Contains("Type \"help\" for informations."));
                Assert.IsTrue(s.Contains("--> do line=1 start=1 stop=3"));
                Assert.IsTrue(s.Contains("help for interactive loop:")); // help
                Assert.IsTrue(s.Contains("#2   line=4     module=command-line              condition=(= a 42)")); // list
                Assert.IsTrue(s.Contains("-->    1 name=<main>                              lineno=3    module=command-line")); // stack
                Assert.IsTrue(s.Contains("a --> 42                                       : Int")); // locals / globals                               
                Assert.IsTrue(s.Contains("(def a 42)")); // code
                Assert.IsTrue(s.Contains("print --> function (print expr1 expr2 ...)         : Function  : module=<builtin>")); // funcs                    
            }
        }

        [TestMethod]
        [DeploymentItem(@"..\..\..\TestData\testdebugger.fuel")]
        public void Test_DebugFile()
        {
            using (ConsoleRedirector cr = new ConsoleRedirector("b 4\nr\nl\nk\nlist\ndown\nk\nup\ncode\ndown\ncode\nclear\ny\nlist\nver\nabout"))
            {
                var args = new[] { "-d", "testdebugger.fuel" };
                LispMainHelper.MainExtended(args, Console.Out, Console.In);
                string s = cr.ToString().Trim();
                Assert.IsTrue(s.Contains("FUEL(isp)-DBG>                    x --> 4                                        : Int "));
                Assert.IsTrue(s.Contains("       1 name=<main>                              lineno=11   module=testdebugger.fuel"));
                Assert.IsTrue(s.Contains("       2 name=g                                   lineno=8    module=testdebugger.fuel"));
                Assert.IsTrue(s.Contains("-->    3 name=f                                   lineno=4    module=testdebugger.fuel"));
                Assert.IsTrue(s.Contains("FUEL(isp)-DBG> Breakpoints:"));
                Assert.IsTrue(s.Contains("#1   line=4     module=testdebugger.fuel         condition="));
                Assert.IsTrue(s.Contains("-->    2 name=g                                   lineno=8    module=testdebugger.fuel"));
                Assert.IsTrue(s.Contains("  4 B  --> 	   (+ x 1)"));
                Assert.IsTrue(s.Contains("  8    --> 	   (* x x (f x))"));
                Assert.IsTrue(s.Contains("  4 B      	   (+ x 1)"));
                Assert.IsTrue(s.Contains("FUEL(isp)-DBG> Really delete all breakpoints? (y/n)"));
                Assert.IsTrue(s.Contains($"FUEL(isp) {Lisp.Version} (for .NET/C#) from {Lisp.Date}, (C) by Michael Neuroth"));
                Assert.IsTrue(s.Contains("FUEL(isp) is a fast usable embeddable lisp interpreter"));
            }
        }

        [TestMethod]
        [DeploymentItem(@"..\..\..\TestData\simple.fuel")]
        public void Test_DebugSetBreakpoints()
        {
            using (ConsoleRedirector cr = new ConsoleRedirector("b \"module name\":4 (== a 4)\nlist"))
            {
                var args = new[] { "-d", "simple.fuel" };
                LispMainHelper.MainExtended(args, Console.Out, Console.In);
                string s = cr.ToString().Trim();
                Assert.IsTrue(s.Contains("#1   line=4     module=module name               condition=(== a 4)"));
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
                LispMainHelper.MainExtended(args, Console.Out, Console.In);
                string s = cr.ToString().Trim();
                Assert.IsTrue(s.Contains("FUEL(isp)-DBG> Breakpoints:"));
                Assert.IsTrue(s.Contains("#1   line=4     module=.\\testmodule.fuel         condition="));
                Assert.IsTrue(s.Contains("       1 name=<main>                              lineno=4    module=test.fuel"));
                Assert.IsTrue(s.Contains("-->    2 name=blub                                lineno=4    module=.\\testmodule.fuel"));
                Assert.IsTrue(s.Contains("x --> 8                                        : Int"));
            }
        }

        [TestMethod]
        public void Test_Documentation()
        {
            using (ConsoleRedirector cr = new ConsoleRedirector())
            {
                var args = new[] { "--doc" };
                LispMainHelper.MainExtended(args, Console.Out, Console.In);
                string s = cr.ToString().Trim();
                Assert.IsTrue(s.Contains("lambda"));
                Assert.IsTrue(s.Contains("Syntax: (lambda (arguments) block)"));
            }
        }
        
        #region parser tests

        [TestMethod]
        public void Test_MainTestParserBracketsOutOfBalance()
        {
            using (ConsoleRedirector cr = new ConsoleRedirector())
            {
                var args = new[] { "-e", "(print ( (+ 1 2))" };
                LispMainHelper.MainExtended(args, Console.Out, Console.In);
                string s = cr.ToString().Trim();
                Assert.IsTrue(s.Contains("Brackets out of balance --> line=2 start=17 stop=18 module="));
            }
        }

        [TestMethod]
        public void Test_MainTestParserUnexpectedToken()
        {
            using (ConsoleRedirector cr = new ConsoleRedirector())
            {
                var args = new[] { "-e", "dummy (print (+ 1 2))" };
                LispMainHelper.MainExtended(args, Console.Out, Console.In);
                string s = cr.ToString().Trim();
                Assert.IsTrue(s.Contains("List expected in do --> line=1 start=0 stop=5 module="));
            }
        }

        [TestMethod]
        public void Test_MainTestFunctionNotFound()
        {
            using (ConsoleRedirector cr = new ConsoleRedirector())
            {
                var args = new[] { "-e", "(unknown-fcn (+ 1 2))" };
                LispMainHelper.MainExtended(args, Console.Out, Console.In);
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
                LispMainHelper.MainExtended(args, Console.Out, Console.In);
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
                LispMainHelper.MainExtended(args, Console.Out, Console.In);
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
                LispMainHelper.MainExtended(args, Console.Out, Console.In);
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
                LispMainHelper.MainExtended(args, Console.Out, Console.In);
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
                LispMainHelper.MainExtended(args, Console.Out, Console.In);
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
                LispMainHelper.MainExtended(args, Console.Out, Console.In);
                string s = cr.ToString().Trim();
                Assert.IsTrue(s.Contains("Symbol expected --> line=1 start=15 stop=18"));
            }
        }

        #endregion
    }
}
