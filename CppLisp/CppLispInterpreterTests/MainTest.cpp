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

using namespace CsLisp;

namespace QtLispUnitTests
{
	TEST_CLASS(UntTestMain)
	{
	public:

		TEST_METHOD(Test_SingleParserEmptyCode)
		{
			LispDebugger * debugger = new LispDebugger();
			Assert::IsNotNull(debugger);
			delete debugger;
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

// TODO --> does not work for C++ yet
				string s = output->GetContent();
				Assert::IsTrue(s.Contains("exists file =  #t"));
				Assert::IsTrue(s.Contains("test non existing file =  #f"));
				Assert::IsTrue(s.Contains("is equal =  #t"));
			}
		}

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

		TEST_METHOD(Test_TestIndexOfInString)
		{
			CsLisp::string target("abc def blub 123");

			Assert::AreEqual((size_t)4, target.IndexOf("def", "nix"));
			Assert::AreEqual(CsLisp::string::npos, target.IndexOf("test", "nix"));
		}
	};
}
