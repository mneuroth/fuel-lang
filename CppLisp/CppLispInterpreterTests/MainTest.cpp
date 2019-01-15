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

#include "stdafx.h"

#include "CppUnitTest.h"

#include "../CppLispInterpreter/fuel.h"

using namespace Microsoft::VisualStudio::CppUnitTestFramework;

using namespace CppLisp;

namespace QtLispUnitTests
{
	TEST_CLASS(UntTestMain)
	{
	public:

		TEST_METHOD(Test_Debugger)
		{
			std::shared_ptr<LispDebugger> debugger = std::make_shared<LispDebugger>();
			Assert::IsNotNull(debugger.get());
		}

		TEST_METHOD(Test_Main)
		{
			//using (ConsoleRedirector cr = new ConsoleRedirector())
			{
				std::shared_ptr<TextWriter> output = std::make_shared<TextWriter>();
				std::shared_ptr<TextReader> input = std::make_shared<TextReader>();
				output->EnableToString(true);
				Fuel::MainExtended(std::vector<string>(), output, input);

				string s = output->GetContent().Trim();
				Assert::IsTrue(s.StartsWith(Lisp::Name));
			}
		}

		TEST_METHOD(Test_MainFile)
		{
			//using (ConsoleRedirector cr = new ConsoleRedirector())
			{
				std::shared_ptr<TextWriter> output = std::make_shared<TextWriter>();
				std::shared_ptr<TextReader> input = std::make_shared<TextReader>();
				output->EnableToString(true);
				std::vector<string> args;
				args.push_back("scripts\\simple.fuel");
				Fuel::MainExtended(args, output, input);

				string s = output->GetContent();
				Assert::IsTrue(s.StartsWith("hello world !"));
			}
		}

		TEST_METHOD(Test_MainFileError)
		{
			//using (ConsoleRedirector cr = new ConsoleRedirector())
			{
				std::shared_ptr<TextWriter> output = std::make_shared<TextWriter>();
				std::shared_ptr<TextReader> input = std::make_shared<TextReader>();
				output->EnableToString(true);
				std::vector<string> args;
				args.push_back("scripts\\error.fuel");
				Fuel::MainExtended(args, output, input);

				string s = output->GetContent();
				Assert::IsTrue(s.Contains("Error executing script"));
				Assert::IsTrue(s.Contains("printx"));
				Assert::IsTrue(s.Contains("not found"));
			}
		}

		TEST_METHOD(Test_MainFileErrorDetailed)
		{
			//using (ConsoleRedirector cr = new ConsoleRedirector())
			{
				std::shared_ptr<TextWriter> output = std::make_shared<TextWriter>();
				std::shared_ptr<TextReader> input = std::make_shared<TextReader>();
				output->EnableToString(true);
				std::vector<string> args;
				args.push_back("-x");
				args.push_back("scripts\\error.fuel");
				Fuel::MainExtended(args, output, input);

				string s = output->GetContent();
				Assert::IsTrue(s.Contains("Error executing script"));
				Assert::IsTrue(s.Contains("printx"));
				Assert::IsTrue(s.Contains("not found"));
				Assert::IsTrue(s.Contains("Callstack"));
				Assert::IsTrue(s.Contains("Exception in"));
			}
		}

		TEST_METHOD(Test_MainHelp)
		{
			//using (ConsoleRedirector cr = new ConsoleRedirector())
			{
				std::shared_ptr<TextWriter> output = std::make_shared<TextWriter>();
				std::shared_ptr<TextReader> input = std::make_shared<TextReader>();
				output->EnableToString(true);
				std::vector<string> args;
				args.push_back("-h");
				Fuel::MainExtended(args, output, input);

				string s = output->GetContent().Trim();
				Assert::IsTrue(s.StartsWith(Lisp::Name));
			}
		}

		TEST_METHOD(Test_MainVersion)
		{
			//using (ConsoleRedirector cr = new ConsoleRedirector())
			{
				std::shared_ptr<TextWriter> output = std::make_shared<TextWriter>();
				std::shared_ptr<TextReader> input = std::make_shared<TextReader>();
				output->EnableToString(true);
				std::vector<string> args;
				args.push_back("-v");
				Fuel::MainExtended(args, output, input);

				string s = output->GetContent().Trim();
				Assert::IsTrue(s.StartsWith(Lisp::ProgramName + " " + Lisp::Version + " from " + Lisp::Date));
			}
		}

		TEST_METHOD(Test_MainExecute)
		{
			//using (ConsoleRedirector cr = new ConsoleRedirector())
			{
				std::shared_ptr<TextWriter> output = std::make_shared<TextWriter>();
				std::shared_ptr<TextReader> input = std::make_shared<TextReader>();
				output->EnableToString(true);
				std::vector<string> args;
				args.push_back("-e");
				args.push_back("(print (+ 1 2))");
				Fuel::MainExtended(args, output, input);
				
				string s = output->GetContent();
				Assert::IsTrue(s == "3");
			}
		}

		TEST_METHOD(Test_MainExecuteAutoBlockDecorate)
		{
			//using (ConsoleRedirector cr = new ConsoleRedirector())
			{
				std::shared_ptr<TextWriter> output = std::make_shared<TextWriter>();
				std::shared_ptr<TextReader> input = std::make_shared<TextReader>();
				output->EnableToString(true);
				std::vector<string> args;
				args.push_back("-e");
				args.push_back("(println \"hello world\") (println \"done.\")");
				Fuel::MainExtended(args, output, input);

				string s = output->GetContent();
				Assert::IsTrue(s.Contains("hello world"));
				Assert::IsTrue(s.Contains("done"));
			}
		}

		TEST_METHOD(Test_MainInteractiveImport)
		{
			//using (ConsoleRedirector cr = new ConsoleRedirector("(import fuellib)\nmacros\nmodules\nfuncs\n"))
			{
				string useForInput = "(import fuellib)\nmacros\nmodules\nfuncs\n";
				std::shared_ptr<TextWriter> output = std::make_shared<TextWriter>();
				std::shared_ptr<TextReader> input = std::make_shared<TextReader>();
				output->EnableToString(true);
				input->SetContent(useForInput);
				input->EnableFromString(true);
				std::vector<string> args;
				args.push_back("-i");
				Fuel::MainExtended(args, output, input);

				string s = output->GetContent();
				Assert::IsTrue(s.Contains(".\\Library\\fuellib.fuel"));
				//Assert::IsTrue(s.Contains("Dict-Remove--> function(Dict - Remove obj p0) : Function: module = .\\Library\\fuellib.fuel"));
				Assert::IsTrue(s.Contains("foreach --> function (foreach container fcn)         : Function  : module=.\\Library\\fuellib.fuel"));
			}
		}

		TEST_METHOD(Test_MainInteractiveDoc)
		{
			//using (ConsoleRedirector cr = new ConsoleRedirector("doc\ndoc if"))
			{
				std::shared_ptr<TextWriter> output = std::make_shared<TextWriter>();
				std::shared_ptr<TextReader> input = std::make_shared<TextReader>();
				output->EnableToString(true);
				string useForInput = "doc\ndoc if";
				input->SetContent(useForInput);
				input->EnableFromString(true);
				std::vector<string> args;
				args.push_back("-i");
				Fuel::MainExtended(args, output, input);

				string s = output->GetContent();
				Assert::IsTrue(s.Contains("doc --> (doc functionname ...)"));
				Assert::IsTrue(s.Contains("Returns and shows the documentation of all builtin functions or for the given function name(s)."));
				Assert::IsTrue(s.Contains("-------------------------------"));
			}
		}

		TEST_METHOD(Test_MainInteractiveSearchDoc)
		{
			//using (ConsoleRedirector cr = new ConsoleRedirector("searchdoc arg"))
			{
				std::shared_ptr<TextWriter> output = std::make_shared<TextWriter>();
				std::shared_ptr<TextReader> input = std::make_shared<TextReader>();
				output->EnableToString(true);
				string useForInput = "searchdoc arg";
				input->SetContent(useForInput);
				input->EnableFromString(true);
				std::vector<string> args;
				args.push_back("-i");
				Fuel::MainExtended(args, output, input);

				string s = output->GetContent();
				Assert::IsTrue(s.Contains("Syntax: (argscount)"));
				Assert::IsTrue(s.Contains("Syntax: (args)"));
				Assert::IsTrue(s.Contains("Syntax: (arg number)"));
			}
		}

		TEST_METHOD(Test_MultiPrintLn)
		{
			//using (ConsoleRedirector cr = new ConsoleRedirector())
			{
				std::shared_ptr<TextWriter> output = std::make_shared<TextWriter>();
				std::shared_ptr<TextReader> input = std::make_shared<TextReader>();
				output->EnableToString(true);
				std::vector<string> args;
				args.push_back("TestData\\multiprintln.fuel");
				Fuel::MainExtended(args, output, input);

				string s = output->GetContent();
				Assert::IsTrue(s.Contains("hello\nworld\ndone."));
			}
		}

		TEST_METHOD(Test_WriteAndReadFile)
		{
			//using (ConsoleRedirector cr = new ConsoleRedirector())
			{
				std::shared_ptr<TextWriter> output = std::make_shared<TextWriter>();
				std::shared_ptr<TextReader> input = std::make_shared<TextReader>();
				output->EnableToString(true);
				std::vector<string> args;
				args.push_back("TestData\\writereadfile.fuel");
				Fuel::MainExtended(args, output, input);

				string s = output->GetContent();
				Assert::IsTrue(s.Contains("exists file =  #t"));
				Assert::IsTrue(s.Contains("test non existing file =  #f"));
				Assert::IsTrue(s.Contains("is equal =  #t"));
			}
		}

		TEST_METHOD(Test_Profile)
		{
			//using (ConsoleRedirector cr = new ConsoleRedirector())
			{
				std::shared_ptr<TextWriter> output = std::make_shared<TextWriter>();
				std::shared_ptr<TextReader> input = std::make_shared<TextReader>();
				output->EnableToString(true);
				std::vector<string> args;
				args.push_back("-m");
				args.push_back("TestData\\simple.fuel");
				Fuel::MainExtended(args, output, input);

				string s = output->GetContent();
				Assert::IsTrue(s.Contains("Execution time ="));
			}
		}

		TEST_METHOD(Test_Trace)
		{
			//using (ConsoleRedirector cr = new ConsoleRedirector())
			{
				std::shared_ptr<TextWriter> output = std::make_shared<TextWriter>();
				std::shared_ptr<TextReader> input = std::make_shared<TextReader>();
				output->EnableToString(true);
				std::vector<string> args;
				args.push_back("-t");
				args.push_back("TestData\\simple.fuel");
				Fuel::MainExtended(args, output, input);

				string s = output->GetContent();
				Assert::IsTrue(s.Contains("--> do"));
				Assert::IsTrue(s.Contains("--> print"));
			}
		}

		TEST_METHOD(Test_MainInteractive)
		{
			//using (var cr = new ConsoleRedirector("help\nfuncs\nbuiltins\nq\n"))
			{
				std::shared_ptr<TextWriter> output = std::make_shared<TextWriter>();
				std::shared_ptr<TextReader> input = std::make_shared<TextReader>();
				output->EnableToString(true);
				string useForInput = "help\nfuncs\nbuiltins\nq\n";
				input->SetContent(useForInput);
				input->EnableFromString(true);
				std::vector<string> args;
				args.push_back("-i");
				Fuel::MainExtended(args, output, input);

				string s = output->GetContent();
				//TestContext.WriteLine("Result=" + result);
				Assert::IsTrue(s.Contains("DBG>"));
				Assert::IsTrue(s.Contains("Type \"help\" for informations."));
				Assert::IsTrue(s.Contains("help for interactive loop:")); // help
				Assert::IsTrue(s.Contains("equal --> function (equal expr1 expr2)             : Function"));
				Assert::IsTrue(s.Contains("define-macro --> function (define-macro name (arguments) statement) : Function  : module=<builtin>"));
			}
		}
	
		TEST_METHOD(Test_MainDebuggerExecute)
		{
			//using (ConsoleRedirector cr = new ConsoleRedirector("r\nhelp\nb 3\nb 4 (= a 42)\nr\nr\no\ns\nrestart\nv\nr\nb 3\nclear 3\nlist\nstack\nglobals\nlocals\ncode\nfuncs\nq\n"))
			{
				const string script = "(do\n\
	(def a 42)\n\
	(print (+ 1 2))\n\
	(print (* 3 4 5)))\n";

				std::shared_ptr<TextWriter> output = std::make_shared<TextWriter>();
				std::shared_ptr<TextReader> input = std::make_shared<TextReader>();
				output->EnableToString(true);
				string useForInput = "r\nhelp\nb 3\nb 4 (= a 42)\nr\nr\no\ns\nrestart\nv\nr\nb 3\nclear 3\nlist\nstack\nglobals\nlocals\ncode\nfuncs\nq\n";
				input->SetContent(useForInput);
				input->EnableFromString(true);
				std::vector<string> args;
				args.push_back("-d");
				args.push_back("-e");
				args.push_back(script);
				Fuel::MainExtended(args, output, input);

				string s = output->GetContent();
				Assert::IsTrue(s.Contains("DBG>"));
				Assert::IsTrue(s.Contains("Type \"help\" for informations."));
				Assert::IsTrue(s.Contains("--> do line=1 start=1 stop=3"));
				Assert::IsTrue(s.Contains("help for interactive loop:")); // help
				Assert::IsTrue(s.Contains("#2   line=4     module=command-line              condition=(= a 42)")); // list
				Assert::IsTrue(s.Contains("-->    1 name=<main>                              lineno=3    module=command-line")); // stack
				Assert::IsTrue(s.Contains("a --> 42                                       : Int")); // locals / globals                               
				Assert::IsTrue(s.Contains("(def a 42)")); // code
				Assert::IsTrue(s.Contains("print --> function (println expr1 expr2 ...)       : Function  : module=<builtin>")); // funcs                    
			}
		}

		TEST_METHOD(Test_DebugFile)
		{
			//using (ConsoleRedirector cr = new ConsoleRedirector("b 4\nr\nl\nk\nlist\ndown\nk\nup\ncode\ndown\ncode\nclear\ny\nlist\nver\nabout"))
			{
				std::shared_ptr<TextWriter> output = std::make_shared<TextWriter>();
				std::shared_ptr<TextReader> input = std::make_shared<TextReader>();
				output->EnableToString(true);
				string useForInput = "b 4\nr\nl\nk\nlist\ndown\nk\nup\ncode\ndown\ncode\nclear\ny\nlist\nver\nabout";
				input->SetContent(useForInput);
				input->EnableFromString(true);
				std::vector<string> args;
				args.push_back("-d");
				args.push_back("TestData\\testdebugger.fuel");
				Fuel::MainExtended(args, output, input);

				string s = output->GetContent();
				Assert::IsTrue(s.Contains("FUEL(isp)-DBG>                    x --> 4                                        : Int "));
				Assert::IsTrue(s.Contains("       1 name=<main>                              lineno=11   module=TestData\\testdebugger.fuel"));
				Assert::IsTrue(s.Contains("       2 name=g                                   lineno=8    module=TestData\\testdebugger.fuel"));
				Assert::IsTrue(s.Contains("-->    3 name=f                                   lineno=4    module=TestData\\testdebugger.fuel"));
				Assert::IsTrue(s.Contains("FUEL(isp)-DBG> Breakpoints:"));
				Assert::IsTrue(s.Contains("#1   line=4     module=TestData\\testdebugger.fuel condition="));
				Assert::IsTrue(s.Contains("-->    2 name=g                                   lineno=8    module=TestData\\testdebugger.fuel"));
				Assert::IsTrue(s.Contains("  4 B  --> 	   (+ x 1)"));
				Assert::IsTrue(s.Contains("  8    --> 	   (* x x (f x))"));
				Assert::IsTrue(s.Contains("  4 B      	   (+ x 1)"));
				Assert::IsTrue(s.Contains("FUEL(isp)-DBG> Really delete all breakpoints? (y/n)"));
				Assert::IsTrue(s.Contains("FUEL(isp) v0.99.3 (for C++) from 12.1.2019, (C) by Michael Neuroth"));
				Assert::IsTrue(s.Contains("FUEL(isp) is a fast usable embeddable lisp interpreter"));
			}
		}

		TEST_METHOD(Test_DebugSetBreakpoints)
		{
			//using (ConsoleRedirector cr = new ConsoleRedirector("b \"module name\":4 (== a 4)\nlist"))
			{
				std::shared_ptr<TextWriter> output = std::make_shared<TextWriter>();
				std::shared_ptr<TextReader> input = std::make_shared<TextReader>();
				output->EnableToString(true);
				string useForInput = "b \"module name\":4 (== a 4)\nlist";
				input->SetContent(useForInput);
				input->EnableFromString(true);
				std::vector<string> args;
				args.push_back("-d");
				args.push_back("TestData\\simple.fuel");
				Fuel::MainExtended(args, output, input);

				string s = output->GetContent();
				Assert::IsTrue(s.Contains("#1   line=4     module=module name               condition=(== a 4)"));
			}
		}

		TEST_METHOD(Test_DebugModule)
		{
			//using (ConsoleRedirector cr = new ConsoleRedirector("b .\\testmodule.fuel:4\nlist\nr\nk\nl"))
			{
				std::shared_ptr<TextWriter> output = std::make_shared<TextWriter>();
				std::shared_ptr<TextReader> input = std::make_shared<TextReader>();
				output->EnableToString(true);
				string useForInput = "b .\\Library\\testmodule.fuel:4\nlist\nr\nk\nl";
				input->SetContent(useForInput);
				input->EnableFromString(true);
				std::vector<string> args;
				args.push_back("-d");
				args.push_back("TestData\\test.fuel");
				args.push_back("-l=.");
				Fuel::MainExtended(args, output, input);

				string s = output->GetContent();
				Assert::IsTrue(s.Contains("FUEL(isp)-DBG> Breakpoints:"));
				Assert::IsTrue(s.Contains("#1   line=4     module=.\\Library\\testmodule.fuel condition="));
				Assert::IsTrue(s.Contains("       1 name=<main>                              lineno=4    module=TestData\\test.fuel"));
				Assert::IsTrue(s.Contains("-->    2 name=blub                                lineno=4    module=.\\Library\\testmodule.fuel"));
				Assert::IsTrue(s.Contains("x --> 8                                        : Int"));
			}
		}

		TEST_METHOD(Test_Documentation)
		{
			//using (ConsoleRedirector cr = new ConsoleRedirector())
			{
				std::shared_ptr<TextWriter> output = std::make_shared<TextWriter>();
				std::shared_ptr<TextReader> input = std::make_shared<TextReader>();
				output->EnableToString(true);
				std::vector<string> args;
				args.push_back("--doc");
				Fuel::MainExtended(args, output, input);

				string s = output->GetContent();
				Assert::IsTrue(s.Contains("lambda"));
				Assert::IsTrue(s.Contains("Syntax: (lambda (arguments) block)"));
			}
		}

		TEST_METHOD(Test_MainTestParserBracketsOutOfBalance)
		{
			//using (ConsoleRedirector cr = new ConsoleRedirector())
			{
				std::shared_ptr<TextWriter> output = std::make_shared<TextWriter>();
				std::shared_ptr<TextReader> input = std::make_shared<TextReader>();
				output->EnableToString(true);
				std::vector<string> args;
				args.push_back("-e");
				args.push_back("(print ( (+ 1 2))");
				Fuel::MainExtended(args, output, input);

				string s = output->GetContent();
				Assert::IsTrue(s.Contains("Brackets out of balance --> line=2 start=17 stop=18 module="));
			}
		}

		TEST_METHOD(Test_MainTestParserUnexpectedToken)
		{
			//using (ConsoleRedirector cr = new ConsoleRedirector())
			{
				std::shared_ptr<TextWriter> output = std::make_shared<TextWriter>();
				std::shared_ptr<TextReader> input = std::make_shared<TextReader>();
				output->EnableToString(true);
				std::vector<string> args;
				args.push_back("-e");
				args.push_back("dummy (print (+ 1 2))");
				Fuel::MainExtended(args, output, input);

				string s = output->GetContent();
				Assert::IsTrue(s.Contains("List expected in do --> line=1 start=0 stop=5 module="));
			}
		}

		TEST_METHOD(Test_MainTestFunctionNotFound)
		{
			//using (ConsoleRedirector cr = new ConsoleRedirector())
			{
				std::shared_ptr<TextWriter> output = std::make_shared<TextWriter>();
				std::shared_ptr<TextReader> input = std::make_shared<TextReader>();
				output->EnableToString(true);
				std::vector<string> args;
				args.push_back("-e");
				args.push_back("(unknown-fcn (+ 1 2))");
				Fuel::MainExtended(args, output, input);

				string s = output->GetContent();
				Assert::IsTrue(s.Contains("Function \"unknown-fcn\" not found --> line=1 start=1 stop=12"));
			}
		}

		TEST_METHOD(Test_MainTestSymbolNotFound)
		{
			//using (ConsoleRedirector cr = new ConsoleRedirector())
			{
				std::shared_ptr<TextWriter> output = std::make_shared<TextWriter>();
				std::shared_ptr<TextReader> input = std::make_shared<TextReader>();
				output->EnableToString(true);
				std::vector<string> args;
				args.push_back("-e");
				args.push_back("(setf a 5)");
				Fuel::MainExtended(args, output, input);

				string s = output->GetContent();
				Assert::IsTrue(s.Contains("Symbol a not found --> line=1 start=1 stop=5"));
			}
		}

		TEST_METHOD(Test_MainTestListExpectedInDo)
		{
			//using (ConsoleRedirector cr = new ConsoleRedirector())
			{
				std::shared_ptr<TextWriter> output = std::make_shared<TextWriter>();
				std::shared_ptr<TextReader> input = std::make_shared<TextReader>();
				output->EnableToString(true);
				std::vector<string> args;
				args.push_back("-e");
				args.push_back("(do (print 3) 5)");
				Fuel::MainExtended(args, output, input);

				string s = output->GetContent();
				Assert::IsTrue(s.Contains("List expected in do --> line=1 start=13 stop=15"));
			}
		}
		
		TEST_METHOD(Test_MainTestBadArgumentCount)
		{
			//using (ConsoleRedirector cr = new ConsoleRedirector())
			{
				std::shared_ptr<TextWriter> output = std::make_shared<TextWriter>();
				std::shared_ptr<TextReader> input = std::make_shared<TextReader>();
				output->EnableToString(true);
				std::vector<string> args;
				args.push_back("-e");
				args.push_back("(do (print 3) (defn x))");
				Fuel::MainExtended(args, output, input);

				string s = output->GetContent();
				Assert::IsTrue(s.Contains("Bad argument count in def, has 1 expected 3 --> line=1 start=15 stop=19"));
			}
		}

		TEST_METHOD(Test_MainTestNoFunction)
		{
			//using (ConsoleRedirector cr = new ConsoleRedirector())
			{
				std::shared_ptr<TextWriter> output = std::make_shared<TextWriter>();
				std::shared_ptr<TextReader> input = std::make_shared<TextReader>();
				output->EnableToString(true);
				std::vector<string> args;
				args.push_back("-e");
				args.push_back("(do (print 3) (map 3 '(1 2 3)))");
				Fuel::MainExtended(args, output, input);

				string s = output->GetContent();
				Assert::IsTrue(s.Contains("No function in map --> line=1 start=15 stop=18"));
			}
		}

		TEST_METHOD(Test_MainTestNoList)
		{
			//using (ConsoleRedirector cr = new ConsoleRedirector())
			{
				std::shared_ptr<TextWriter> output = std::make_shared<TextWriter>();
				std::shared_ptr<TextReader> input = std::make_shared<TextReader>();
				output->EnableToString(true);
				std::vector<string> args;
				args.push_back("-e");
				args.push_back("(do (print 3) (map (lambda (x) (print x)) 3))");
				Fuel::MainExtended(args, output, input);

				string s = output->GetContent();
				Assert::IsTrue(s.Contains("No list in map --> line=1 start=40 stop=40"));
			}
		}

		TEST_METHOD(Test_MainTestSymbolExpected)
		{
			//using (ConsoleRedirector cr = new ConsoleRedirector())
			{
				std::shared_ptr<TextWriter> output = std::make_shared<TextWriter>();
				std::shared_ptr<TextReader> input = std::make_shared<TextReader>();
				output->EnableToString(true);
				std::vector<string> args;
				args.push_back("-e");
				args.push_back("(do (print 3) (def 4 \"test\"))");
				Fuel::MainExtended(args, output, input);

				string s = output->GetContent();
				Assert::IsTrue(s.Contains("Symbol expected --> line=1 start=15 stop=18"));
			}
		}

/*
		TEST_METHOD(Test_StdLibObjects)
		{
			//using (ConsoleRedirector cr = new ConsoleRedirector())
			{
				std::shared_ptr<TextWriter> output = std::make_shared<TextWriter>();
				std::shared_ptr<TextReader> input = std::make_shared<TextReader>();
				output->EnableToString(true);
				std::vector<string> args;
				args.push_back("TestData\\teststdlib.fuel");
				Fuel::MainExtended(args, output, input);

				// TODO --> does not work for C++ yet
				string s = output->GetContent();
				Assert::IsTrue(s.Contains("DictCount= 2"));
				Assert::IsTrue(s.Contains("NewDictCount= 0"));
				Assert::IsTrue(s.Contains("DirListType= List"));
				Assert::IsTrue(s.Contains("File= .\\FuelCompiler.dll"));
				Assert::IsTrue(s.Contains("File= .\\FuelDebugger.dll"));
				Assert::IsTrue(s.Contains("File= .\\FuelInterpreter.dll"));
				Assert::IsTrue(s.Contains("File= .\\fuel.exe"));
				Assert::IsTrue(s.Contains("File= .\\teststdlib.fuel"));
				Assert::IsTrue(s.Contains("ListCount= 4"));
				Assert::IsTrue(s.Contains("item= System.Collections.Generic.Dictionary`2[System.Object,System.Object]"));
				Assert::IsTrue(s.Contains("newitem= 12"));
				Assert::IsTrue(s.Contains("NewListCount= 0"));
				Assert::IsTrue(s.Contains("ArrayCount= 5"));
				Assert::IsTrue(s.Contains("ArrayItem1= 1"));
				Assert::IsTrue(s.Contains("ArrayItem2= blub"));
				Assert::IsTrue(s.Contains("ArrayItem3= #t"));
				Assert::IsTrue(s.Contains("ArrayItem4= 42"));
				Assert::IsTrue(s.Contains("ArrayItem5= 123"));
			}
		}
*/
		// TODO / NOT IMPLEMENTED
		// Test_Compile
		// Test_Compile2
		// Test_CompileOutput
	};
}
