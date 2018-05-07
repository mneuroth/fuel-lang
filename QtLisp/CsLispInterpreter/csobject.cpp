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

#include "csobject.h"
#include "Variant.h"

namespace CsLisp
{

	object::object(const LispVariant & value)
		: m_sValue("NOT_IMPLEMENTED_YET"), m_Type(ObjectType::__LispVariant)
	{
		m_Data.pVariant = new LispVariant(value);
	}

	object::~object()
	{
		if (IsLispToken())
		{
			delete m_Data.pVariant;
		}
	}


std::shared_ptr<LispVariant> object::ToLispVariant()
{
// TODO
	if (IsLispVariant())
	{
		return std::make_shared<LispVariant>(*(m_Data.pVariant));
	}
	return std::make_shared<LispVariant>(LispVariant(LispType::_Nil));
}

LispFunctionWrapper object::ToLispFunctionWrapper()
{
// TODO
	LispFunctionWrapper ret;
	return ret;
}

IEnumerable<std::shared_ptr<object>> object::ToEnumerableOfObject() const
{
// TODO
	return IEnumerable<std::shared_ptr<object>>();
}

}

