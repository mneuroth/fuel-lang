// QtLisp.cpp : Defines the entry point for the console application.
//

// gcc -std=c++11 -c QtLisp.cpp

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

//#include "stdafx.h"

#include <iostream>
#include <memory>

#include "Token.h"
#include "Tokenizer.h"
#include "Variant.h"
#include "Scope.h"

using namespace CsLisp;

int main()
{
	string code = "(+ 1 2 3)";
	IEnumerable<LispToken> tokens = LispTokenizer::Tokenize(code);

	//for each (LispToken elem in tokens)
	//{
	//	std::cout << "Elem: " << elem.ToString() << std::endl;
	//}

	for_each(tokens.begin(), tokens.end(), [](LispToken elem) { std::cout << "Elem: " << elem.ToString() << std::endl; });

	LispVariant aVariant(LispType::_Nil);
	std::cout << "Variant: " << aVariant.ToString() << std::endl;
	LispVariant aVariant2(std::make_shared<object>(42));
	std::cout << "Variant: " << aVariant2.ToString() << std::endl;
	LispVariant aVariant3(std::make_shared<object>(43));
	std::cout << "Variant: " << aVariant3.ToString() << std::endl;

	std::shared_ptr<object> v3 = std::make_shared<object>(aVariant3);
	int result = aVariant2.CompareTo(v3);

	LispScope scope;

    return 0;
}

