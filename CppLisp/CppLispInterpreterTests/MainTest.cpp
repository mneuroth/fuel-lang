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
				TextWriter output;
				TextReader input;
				output.EnableToString(true);
				Fuel::MainExtended(std::vector<string>(), output, input);
				string s = output.GetContent().Trim();
				Assert::IsTrue(s.StartsWith(Lisp::Name));
			}
		}

		TEST_METHOD(Test_MainFileErrorDetailed)
		{
			//using (ConsoleRedirector cr = new ConsoleRedirector())
			{
				TextWriter output;
				TextReader input;
				output.EnableToString(true);
				std::vector<string> args;
				args.push_back("-x");
				args.push_back("scripts\\error.fuel");
				string s;
				Fuel::MainExtended(args, output, input, &s);

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
				TextWriter output;
				TextReader input;
				output.EnableToString(true);
				std::vector<string> args;
				args.push_back("-h");
				Fuel::MainExtended(args, output, input);

				string s = output.GetContent().Trim();
				Assert::IsTrue(s.StartsWith(Lisp::Name));
			}
		}

		TEST_METHOD(Test_MainVersion)
		{
			//using (ConsoleRedirector cr = new ConsoleRedirector())
			{
				TextWriter output;
				TextReader input;
				output.EnableToString(true);
				std::vector<string> args;
				args.push_back("-v");
				Fuel::MainExtended(args, output, input);
				string s = output.GetContent().Trim();
				Assert::IsTrue(s.StartsWith(Lisp::ProgramName + " " + Lisp::Version + " from " + Lisp::Date));
			}
		}
	};
}
