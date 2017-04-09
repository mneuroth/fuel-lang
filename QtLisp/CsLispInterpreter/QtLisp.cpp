// QtLisp.cpp : Defines the entry point for the console application.
//

// gcc -std=c++11 -c QtLisp.cpp

//#include "stdafx.h"

#include <iostream>

#include "Token.h"
#include "Tokenizer.h"

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

    return 0;
}

