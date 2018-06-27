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

#include "Variant.h"
#include "cstypes.h"

namespace CsLisp
{
	double LispVariant::Tolerance = 1e-8;

	const string LispVariant::CanNotConvertTo = "can not convert {0} to {1}";
	const string LispVariant::NoOperatorForTypes = "no {0} operator for types {1} and {2}";

	int LispVariant::CompareTo(std::shared_ptr<object> other)
	{
		if (other->IsLispVariant())
		{
			var otherVariant = other->ToLispVariant();
			if (IsNumber() && otherVariant->IsNumber())
			{
				if (IsDouble() || otherVariant->IsDouble())
				{
					return CompareToType<double>(ToDouble(), otherVariant->ToDouble());
				}
				return CompareToType<int>(ToInt(), otherVariant->ToInt());
			}
			// all other types will be compared like a string
			return String::CompareOrdinal(StringValue(), otherVariant->StringValue()/*, StringComparison.Ordinal*/);
		}
		return CompareTo(std::make_shared<object>(LispVariant(other)));
	}

	int LispVariant::CompareTo(std::shared_ptr<LispVariant> other)
	{
		return CompareTo(other->NativeObjectValue());
	}

	LispFunctionWrapper LispVariant::FunctionValue() const
	{
		//get
		//{
		if (Type != LispType::_Function)
		{
			throw CreateInvalidCastException("function", "not found");
		}
		return Value->ToLispFunctionWrapper();
		//}
	}

	std::shared_ptr<IEnumerable<std::shared_ptr<object>>> LispVariant::ListValue() const
	{
		//get
		//{
			// Nil is an empty list () !
		if (Type == LispType::_Nil)
		{
			return std::make_shared<IEnumerable<std::shared_ptr<object>>>();
		}
		if (IsNativeObject() && NativeObjectValue()->IsIEnumerableOfObject())
		{
			return std::make_shared<IEnumerable<std::shared_ptr<object>>>(NativeObjectValue()->ToEnumerableOfObject());
		}
		if (Type != LispType::_List)
		{
			throw CreateInvalidCastException("list");
		}
		return std::make_shared<IEnumerable<std::shared_ptr<object>>>(Value->ToEnumerableOfObject()); // ((IEnumerable)Value).Cast<object>();
		//}
	}

	double LispVariant::DoubleValue() const
	{
		//get
		//{
		if (!IsDouble())
		{
			throw CreateInvalidCastException("double");
		}
			return (double)(*Value);
		//}
	}

	int LispVariant::IntValue() const
	{
		//get
		//{
		if (!IsInt())
		{
			throw CreateInvalidCastException("int");
		}
		return (int)(*Value);
		//}
	}

	bool LispVariant::BoolValue() const
	{
		//get
		//{
		if (!IsBool())
		{
			throw CreateInvalidCastException("bool");
		}
		return (bool)(*Value);
		//}
	}

	std::shared_ptr<object> LispVariant::NativeObjectValue() const
	{
		//get
		//{
		if (IsNativeObject() && Type != LispType::_Nil)
		{
			throw CreateInvalidCastException("native object");
		}
		return Value;
		//}
	}

	string LispVariant::NativeObjectStringRepresentation() const
	{
		//get
		//{
		string result = string::Empty;

		std::shared_ptr<object> native = NativeObjectValue();
		if (native->IsIEnumerableOfObject())
		{
			IEnumerable<std::shared_ptr<object>> container = native->ToEnumerableOfObject();
			for (var element = container.begin(); element != container.end(); element++)	//foreach(var element in container)
			{
				if (result.Length() > 0)
				{
					result += " ";
				}
				result += *element != null ? (*element)->ToString() : LispToken::NilConst;
			}
			result = "(" + result + ")";
		}
		else
		{
			result = native->ToString();
		}

		return result;
		//}
	}

	/*private*/ string LispVariant::ExpandContainerToString(std::shared_ptr<object> maybeContainer)
	{
		string ret = string::Empty;

		if (maybeContainer->IsIEnumerableOfObject() || maybeContainer->IsList())
		{
			var container = maybeContainer->ToEnumerableOfObject();
			for (var item = container.begin(); item != container.end(); item++) // foreach(var item in container)
			{
				if (ret.Length() > 0)
				{
					ret += " ";
				}
				ret += ExpandContainerToString(*item);
			}
			ret = "(" + ret + ")";
		}
		else
		{
			ret += ExpandItemForContainer(maybeContainer);
		}

		return ret;
	}

	bool LispVariant::operator==(const LispVariant & r) const
	{
		return *Value == *(r.Value);
	}

	bool LispVariant::operator!=(const LispVariant & other) const
	{
		return !(*this == other);
	}

}
