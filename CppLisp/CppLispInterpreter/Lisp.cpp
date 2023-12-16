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

#include "Lisp.h"
#include "Exception.h"

using namespace CppLisp;

const string Lisp::ProgramName = "fuel";
const string Lisp::Name = "FUEL(isp)";
const string Lisp::Version = "v0.99.6";
const string Lisp::Date = "16.12.2023";

const string Lisp::Copyright = "(C) by Michael Neuroth";
const string Lisp::Platform = "C++";
const string Lisp::Info = Lisp::Name + " is a fast usable embeddable lisp interpreter";

const string Lisp::License = "MIT-License";
const string Lisp::LicenseUrl = "http://opensource.org/licenses/MIT";

namespace CppLisp
{
    /*public*/ static string DecorateWithBlock(const string & code, size_t & offset)
	{
		const string block = "(do ";
        const string blockEnd = "\n)";
		offset = block.Length();
        return block + code + blockEnd;
	}

	string Lisp::GetCompilerInfo()
	{
		string info;
#if defined(__GNUC__) || defined(__clang__)
		info += "GCC v";
		info += std::to_string(__GNUC__);
		info += ".";
		info += std::to_string(__GNUC_MINOR__);
		info += ".";
		info += std::to_string(__GNUC_PATCHLEVEL__);
#if defined(__i386__)
		// IA-32
		info += ", x86 32Bit";
#elif defined(__x86_64__)
		// AMD64
		info += ", x86 64Bit";
#elif defined(__arm__)
		info += ", arm 32Bit";
#elif defined(__aarch64__)
		info += ", arm 64Bit";
#else
# error Unsupported architecture
#endif
#elif defined(__clang__)
		info += "clang v";
		info += std::to_string(__clang_major__);
		info += ".";
		info += std::to_string(__clang_minor__);
#if defined(__i386__)
		// IA-32
		info += ", x86 32Bit";
#elif defined(__x86_64__)
		// AMD64
		info += ", x86 64Bit";
#elif defined(__arm__)
		info += ", arm 32Bit";
#elif defined(__aarch64__)
		info += ", arm 64Bit";
#else
# error Unsupported architecture
#endif
#endif
#ifdef __EMSCRIPTEN__
		info += ", EMSCRIPTEN";
#endif
#ifdef _MSC_VER
		info += "Microsoft C++ v";
		info += std::to_string(_MSC_VER);
#if defined(_WIN64)
		// AMD64
		info += ", 64Bit";
#elif defined(_WIN32)
		// IA-32
		info += ", 32Bit";
#else
# error Unsupported architecture
#endif
#endif
		return info;
	}

	std::shared_ptr<LispVariant> Lisp::Eval(const string & lispCode, std::shared_ptr<LispScope> scope/*= null*/, const string & moduleName/*= null*/, bool tracing/*, Dictionary<string, object> nativeItems = null*/, std::shared_ptr<TextWriter> outp, std::shared_ptr<TextReader> inp, bool onlyMacroExpand)
	{
		// first create global scope, needed for macro expanding
		var currentScope = scope == null ? LispEnvironment::CreateDefaultScope() : scope;
		currentScope->ModuleName = moduleName;
		currentScope->Tracing = tracing;
		currentScope->Output = outp != null ? outp : (scope != null ? scope->Output : std::make_shared<TextWriter>());
		currentScope->Input = inp != null ? inp : (scope != null ? scope->Input : std::make_shared<TextReader>());
		RegisterNativeObjects(/*nativeItems,*/ *currentScope);
		size_t offset = 0;
		string code = /*LispUtils.*/DecorateWithBlock(lispCode, /*out*/ offset);
		var ast = LispParser::Parse(code, offset, currentScope);
#ifdef ENABLE_COMPILE_TIME_MACROS 
		var expandedAst = std::make_shared<object>(*(LispInterpreter::ExpandMacros(std::make_shared<object>(*ast), currentScope)));
#else
		var expandedAst = std::make_shared<object>(*ast);
#endif
		std::shared_ptr<LispVariant> result;
		if (onlyMacroExpand)
		{
			result = std::make_shared<LispVariant>(LispVariant(expandedAst));
		}
		else
		{
			result = LispInterpreter::EvalAst(expandedAst, currentScope);
		}
		return result;
	}

	std::shared_ptr<LispVariant> Lisp::SaveEval(const string & lispCode, const string & moduleName, bool verboseErrorOutput, bool tracing, std::shared_ptr<TextWriter> outp, std::shared_ptr<TextReader> inp, bool onlyMacroExpand)
	{
		std::shared_ptr<LispVariant> result;
		try
		{
			result = Eval(lispCode, /*scope:*/ null, /*moduleName :*/ moduleName, /*tracing :*/ tracing, outp, inp, onlyMacroExpand);
		}
		catch (LispException exc)
		{
			/*Console.WriteLine*/ //std::cout << string::Format("\nError executing script.\n\n{0} --> line={1} start={2} stop={3} module={4}", exc.Message, exc.Data[LispUtils.LineNo], exc.Data[LispUtils.StartPos], exc.Data[LispUtils.StopPos], exc.Data[LispUtils.ModuleName]) << std::endl;
			string errMsg = string::Format("\nError executing script.\n\n{0} --> line={1} start={2} stop={3} module={4}", exc.Message, exc.Data["LineNo"]->ToString(), exc.Data["StartPos"]->ToString(), exc.Data["StopPos"]->ToString(), exc.Data["ModuleName"]->ToString());
			std::cout << errMsg << std::endl;
			if (outp != null)
			{
				outp->WriteLine(errMsg);
			}
// TODO --> implement stack trace for exception
			var stackInfo = exc.Data["StackInfo"];
			//Console.WriteLine("\nCallstack:\n{0}", stackInfo != null ? stackInfo : ">not available<");                if (verboseErrorOutput)
			errMsg = string::Format("\nCallstack:\n{0}", stackInfo != null ? stackInfo->ToString() : ">not available<");
			std::cout << errMsg << std::endl;
			if (outp != null)
			{
				outp->WriteLine(errMsg);
			}
			if (verboseErrorOutput)
			{
				/*Console.WriteLine*/std::cout << ("\nNative callstack:") << std::endl;
				errMsg = string::Format("Exception in eval(): {0} \ndata={1}", "exc->ToString()", "exc.Data");
				/*Console.WriteLine*/std::cout << errMsg << std::endl;
				if (outp != null)
				{
					outp->WriteLine(errMsg);
				}
			}
			result = LispVariant::CreateErrorValue(exc.Message);
		}
		catch (LispExceptionBase exc)
		{
			result = LispVariant::CreateErrorValue(exc.Message);
		}
		return result;
	}

	void Lisp::RegisterNativeObjects(/*Dictionary<string, object> nativeItems,*/ LispScope & /*currentScope*/)
	{
		// TODO --> implement native objects
		//			if (nativeItems != null)
		//			{
		//				foreach(KeyValuePair<string, object> item in nativeItems)
		//				{
		//					currentScope[item.Key] = new LispVariant(LispType.NativeObject, item.Value);
		//				}
		//			}
	}
}
