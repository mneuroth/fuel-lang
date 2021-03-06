﻿/*
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

#ifndef _EXCEPTION_H
#define _EXCEPTION_H

 //using System;

#include "csstring.h"
#include "csexception.h"

#include <map>
#include <memory>

namespace CppLisp
{
	class LispScope;
	class LispToken;
	class object;

	class DLLEXPORT _ExportForMscDummyMap : std::map<string, std::shared_ptr<object>>
	{
	};

	// **********************************************************************
	/// <summary>
    /// Exception for the FUEL lisp interpreter
    /// </summary>
    /*public*/ class DLLEXPORT LispException : public LispExceptionBase
    {
	public:
		//string Message;

		//std::map<string, object> Data;
		
		/// <summary>
        /// Initialize s a new instance of the <see cref="LispException"/> class.
        /// </summary>
        /// <param name="text">The text.</param>
        /// <param name="scope">The scope.</param>
		/*public*/ LispException(const string & text, LispScope * scope /*= 0*/);

        /// <summary>
        /// Initializes a new instance of the <see cref="LispException" /> class.
        /// </summary>
        /// <param name="text">The text.</param>
        /// <param name="token">The token.</param>
        /// <param name="moduleName">Name of the module.</param>
        /// <param name="stackInfo">The stack information.</param>
		/*public*/ LispException(const string & text, std::shared_ptr<LispToken> token, const string & moduleName, const string & stackInfo = "not available");

		void AddModuleNameAndStackInfos(const string & moduleName, const string & stackInfo);
		void AddTokenInfos(std::shared_ptr<LispToken> token);

		std::map<string, std::shared_ptr<object>> Data;
	};

	// **********************************************************************
	/*public*/ class LispStopDebuggerException : LispException
	{
	public:
		LispStopDebuggerException(const string & text = "")
			: /*base*/LispException(text, 0)
		{
		}
	};
}

#endif