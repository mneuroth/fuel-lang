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

#ifndef _FUEL_H
#define _FUEL_H

#include "csstring.h"
#include "Utils.h"
#include "Lisp.h"
#include "Debugger.h"

namespace CsLisp
{
	/// <summary>
	/// Fast Usable Embeddable Lisp Interpreter and Compiler (FUEL).
	/// </summary>
	/*public*/ class Fuel
	{
	public:
		/*public*/ static void Main(std::vector<string> args);
		/*public*/ static void MainExtended(std::vector<string> args, std::shared_ptr<TextWriter> output, std::shared_ptr<TextReader> input);

	private:
		//#region private methods

		/*private*/ static void Usage(std::shared_ptr<TextWriter> output);
		/*private*/ static void InteractiveLoopHeader(std::shared_ptr<TextWriter> output);
		
		/*private*/ static std::shared_ptr<ILispDebugger> TryGetDebugger();

		//#endregion
	};
}

#endif
