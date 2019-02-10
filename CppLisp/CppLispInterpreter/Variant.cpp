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

#include <math.h>

namespace CppLisp
{
	double LispVariant::Tolerance = 1e-8;

	const string LispVariant::CanNotConvertTo = "can not convert {0} to {1}";
	const string LispVariant::NoOperatorForTypes = "no {0} operator for types {1} and {2}";

	LispVariant::LispVariant(std::shared_ptr<object> val)
		: LispVariant(val, LispUnQuoteModus::_None)
	{
	}

	LispVariant::LispVariant(LispType type, std::shared_ptr<object> value, LispUnQuoteModus unQuoted)
	{
		Token = null;
		Type = type;
		Value = value;
		IsUnQuoted = unQuoted;
	}

	LispVariant::LispVariant(std::function<void(std::shared_ptr<object>)> action)
		: LispVariant(LispType::_LValue, std::make_shared<object>(action), LispUnQuoteModus::_None)
	{
	}

	LispVariant::LispVariant(std::shared_ptr<object> val, LispUnQuoteModus unQuoted /*= LispUnQuoteModus::_None*/)
		: LispVariant(TypeOf(val), val, unQuoted)
	{
		//std::shared_ptr<LispVariant> value = std::dynamic_pointer_cast<LispVariant>(val); //val as LispVariant;
		//if (value /*!= null*/)
		if (val->IsLispVariant())
		{
			const LispVariant & value = val->ToLispVariantRef();
			Token = value.Token;
			Type = value.Type;
			Value = value.Value;
			IsUnQuoted = value.IsUnQuoted;
		}
		else
		{
			Token = null;
			Type = ConvertObjectTypeToVariantType(val->GetType());
			Value = val;
			IsUnQuoted = unQuoted;
		}
	}

	LispVariant::LispVariant(const LispVariant & other)
	{
		Token = other.Token;
		Type = other.Type;
		Value = other.Value;
		IsUnQuoted = other.IsUnQuoted;
	}

	LispVariant::LispVariant(std::shared_ptr<LispToken> token, LispUnQuoteModus unQuoted)
		: LispVariant(TypeOf(token->Value), token->Value, unQuoted)
	{
		Token = token;
		if (token->Type == LispTokenType::Nil)
		{
			Type = LispType::_Nil;
		}
		if (token->Type == LispTokenType::Symbol)
		{
			Type = LispType::_Symbol;
		}
	}

	std::shared_ptr<LispVariant> LispVariant::CreateErrorValue(const string & errorMessage)
	{
		std::shared_ptr<object> msg = std::make_shared<object>(object(errorMessage));
		return std::make_shared<LispVariant>(LispVariant(LispType::_Error, msg));
	}

	string LispVariant::TypeString() const
	{
		//get
		//{
		switch (Type)
		{
		case _Undefined:
			return "Undefined";
		case _Nil:
			return "Nil";
		case _Bool:
			return "Bool";
		case _Int:
			return "Int";
		case _Double:
			return "Double";
		case _String:
			return "String";
		case _List:
			return "List";
		case _Function:
			return "Function";
		case _Symbol:
			return "Symbol";
		case _NativeObject:
			return /*Type+*/string("NativeObject<") + Value->GetTypeName() + string(">");
			//_Array = 10,
		case _Error:
			return "Error";
		default:
			return "unknown_type";
		}
		//}
	}

	bool LispVariant::ToBool() const
	{
		if (IsBool())
		{
			return BoolValue();
		}
		if (IsInt())
		{
			return IntValue() != 0;
		}
		if (IsDouble())
		{
			return /*Math.A*/fabs(DoubleValue()) > Tolerance;
		}
		throw CreateInvalidCastException("bool", CppLisp::string::Format(CanNotConvertTo, TypeString(), "bool"));
	}

	int LispVariant::ToInt() const
	{
		if (IsBool())
		{
			return BoolValue() ? 1 : 0;
		}
		if (IsInt())
		{
			return IntValue();
		}
		if (IsDouble())
		{
			return (int)DoubleValue();
		}
		if (IsString())
		{
			return /*Convert.ToInt32*/atoi(StringValue().c_str());
		}
		throw CreateInvalidCastException("int", CppLisp::string::Format(CanNotConvertTo, TypeString(), "int"));
	}

	double LispVariant::ToDouble() const
	{
		if (IsBool())
		{
			return BoolValue() ? 1.0 : 0.0;
		}
		if (IsInt())
		{
			return IntValue();
		}
		if (IsDouble())
		{
			return DoubleValue();
		}
		if (IsString())
		{
			return atof(StringValue().c_str()); //Convert.ToDouble(StringValue(), CultureInfo.InvariantCulture);
		}
		throw CreateInvalidCastException("double", CppLisp::string::Format(CanNotConvertTo, TypeString(), "double"));
	}

	string LispVariant::StringValue() const
	{
		//get
		//{
		if (Value != null)
		{
			return Value->ToString();
		}
		return "null";	// string::Empty;
						//}
	}

	/// <summary>
	/// Comverts this value into a string representation used by the compiler module.
	/// </summary>
	/// <returns>The string representation</returns>
	string LispVariant::ToStringCompiler() const
	{
		if (IsBool())
		{
			return BoolValue() ? "true" : "false";
		}
		if (IsDouble())
		{
			return std::to_string(DoubleValue()); //.ToString("F", CultureInfo.InvariantCulture);
		}
		if (IsString())
		{
			return "\"" + StringValue() + "\"";
		}
		return ToString();
	}

	/// <summary>
	/// Comverts this value into a string representation used by the debugger module.
	/// </summary>
	/// <returns>The string representation</returns>
	string LispVariant::ToStringDebugger() const
	{
		if (IsString())
		{
			return "\"" + StringValue() + "\"";
		}
		return ToString();
	}

	string LispVariant::ToString() const
	{
		if (IsString())
		{
			return StringValue();
		}
		if (IsInt())
		{
			return std::to_string(IntValue()); //.ToString(CultureInfo.InvariantCulture);
		}
		if (IsDouble())
		{
			return std::to_string(DoubleValue()); //.ToString(CultureInfo.InvariantCulture);
		}
		if (IsBool())
		{
			return BoolValue() ? "#t" : "#f";
		}
		if (IsNil())
		{
			return LispToken::NilConst;
		}
		if (IsList())
		{
			return ExpandContainerToString(std::make_shared<object>(ListValueRef()));
		}
		if (IsFunction())
		{
			return "function " + /*(FunctionValue().Signature != null ?*/ FunctionValue().Signature /*: "<unknown>")*/;
		}
		if (IsSymbol())
		{
			return Value->ToString();
		}
		if (IsNativeObject())
		{
			return NativeObjectStringRepresentation();
		}
		if (IsUndefined())
		{
			return "<undefined>";
		}
		if (IsError())
		{
			return "Error: " + Value->ToString();
		}
		return "?";
	}

	string LispVariant::ExpandItemForContainer(std::shared_ptr<object> item)
	{
		if (item->IsLispVariant())
		{
			const LispVariant & variant = item->ToLispVariantRef();
			if (variant.IsString())
			{
				return variant.ToStringDebugger();
			}
		}
		return item->ToString();
	}

	bool LispVariant::Equals(std::shared_ptr<object>  other) const
	{
		if (other->IsLispVariant())
		{
			const LispVariant & otherVariant = other->ToLispVariantRef();
			return EqualOp(*this, otherVariant);
		}
		return false;
	}

	bool LispVariant::SymbolCompare(std::shared_ptr<object> other) const
	{
		if (other->IsLispVariant())
		{
			const LispVariant & otherVariant = other->ToLispVariantRef();
			return Value->Equals(*(otherVariant.Value));
		}
		return false;
	}

	int LispVariant::CompareTo(std::shared_ptr<object> other)
	{
		if (other->IsLispVariant())
		{
			const LispVariant & otherVariant = other->ToLispVariantRef();
			if (IsNumber() && otherVariant.IsNumber())
			{
				if (IsDouble() || otherVariant.IsDouble())
				{
					return CompareToType<double>(ToDouble(), otherVariant.ToDouble());
				}
				return CompareToType<int>(ToInt(), otherVariant.ToInt());
			}
			// all other types will be compared like a string
			return string::CompareOrdinal(StringValue(), otherVariant.StringValue()/*, StringComparison.Ordinal*/);
		}
		return CompareTo(std::make_shared<object>(LispVariant(other)));
	}

	int LispVariant::CompareTo(std::shared_ptr<LispVariant> other)
	{
		return CompareTo(other->NativeObjectValue());
	}

	void LispVariant::Add(std::shared_ptr<object> value)
	{
		if (Type != LispType::_List)
		{
			throw CreateInvalidCastException("list");
		}
		var list = Value->ToList();
		list->Add(value);
	}

	LispVariant LispVariant::operator+(const LispVariant & r)
	{
		if (IsString() || r.IsString())
		{
			return /*new*/ LispVariant(std::make_shared<object>(StringValue() + r.StringValue()));
		}
		if (IsDouble() || r.IsDouble())
		{
			return /*new*/ LispVariant(std::make_shared<object>(ToDouble() + r.ToDouble()));
		}
		if (IsInt() || r.IsInt())
		{
			return /*new*/ LispVariant(std::make_shared<object>(ToInt() + r.ToInt()));
		}
		if (IsList() && r.IsList())
		{
			IEnumerable<std::shared_ptr<object>> newList;
			newList.AddRange(ListValueRef());
			newList.AddRange(r.ListValueRef());
			return /*new*/ LispVariant(std::make_shared<object>(newList));
		}
		throw CreateInvalidOperationException("+", *this, r);
	}

	LispVariant LispVariant::operator -(const LispVariant & r)
	{
		if (IsDouble() || r.IsDouble())
		{
			return /*new*/ LispVariant(std::make_shared<object>(ToDouble() - r.ToDouble()));
		}
		if (IsInt() || r.IsInt())
		{
			return /*new*/ LispVariant(std::make_shared<object>(ToInt() - r.ToInt()));
		}
		throw CreateInvalidOperationException("-", *this, r);
	}

	LispVariant LispVariant::operator *(const LispVariant & r)
	{
		if (IsDouble() || r.IsDouble())
		{
			return /*new*/ LispVariant(std::make_shared<object>(ToDouble() * r.ToDouble()));
		}
		if (IsInt() || r.IsInt())
		{
			return /*new*/ LispVariant(std::make_shared<object>(ToInt() * r.ToInt()));
		}
		throw CreateInvalidOperationException("*", *this, r);
	}

	LispVariant LispVariant::operator /(const LispVariant & r)
	{
		if (IsDouble() || r.IsDouble())
		{
			return /*new*/ LispVariant(std::make_shared<object>(ToDouble() / r.ToDouble()));
		}
		if (IsInt() || r.IsInt())
		{
			return /*new*/ LispVariant(std::make_shared<object>(ToInt() / r.ToInt()));
		}
		throw CreateInvalidOperationException("/", *this, r);
	}

	LispVariant LispVariant::operator %(const LispVariant & r)
	{
		if (IsDouble() || r.IsDouble())
		{
            return /*new*/ LispVariant(std::make_shared<object>(fmod(ToDouble(),r.ToDouble())));
		}
		if (IsInt() || r.IsInt())
		{
			return /*new*/ LispVariant(std::make_shared<object>(ToInt() % r.ToInt()));
		}
		throw CreateInvalidOperationException("%", *this, r);
	}

	LispVariant LispVariant::operator <(const LispVariant & r) const
	{
		if (IsDouble() || r.IsDouble())
		{
			return /*new*/ LispVariant(std::make_shared<object>(ToDouble() < r.ToDouble()));
		}
		if (IsInt() || r.IsInt())
		{
			return /*new*/ LispVariant(std::make_shared<object>(ToInt() < r.ToInt()));
		}
		if (IsString() || r.IsString())
		{
			return /*new*/ LispVariant(std::make_shared<object>(string::CompareOrdinal(ToString(), r.ToString()) < 0));
		}
		throw CreateInvalidOperationException("< or >", *this, r);
	}

	LispVariant LispVariant::operator <=(const LispVariant & r) const
	{
		if (IsDouble() || r.IsDouble())
		{
			return /*new*/ LispVariant(std::make_shared<object>(ToDouble() <= r.ToDouble()));
		}
		if (IsInt() || r.IsInt())
		{
			return /*new*/ LispVariant(std::make_shared<object>(ToInt() <= r.ToInt()));
		}
		if (IsString() || r.IsString())
		{
			return /*new*/ LispVariant(std::make_shared<object>(string::CompareOrdinal(ToString(), r.ToString()) <= 0));
		}
		throw CreateInvalidOperationException("<= or >=", *this, r);
	}

	bool LispVariant::EqualOp(const LispVariant & l, const LispVariant & r)
	{
		if (l.IsNativeObject() && r.IsNativeObject())
		{
			return l.NativeObjectValue() == r.NativeObjectValue();
		}
		if (l.IsSymbol() || r.IsSymbol())
		{
			return l.IsSymbol() && r.IsSymbol() && (l.ToString() == r.ToString());
		}
		if (l.IsBool() && r.IsBool())
		{
			return l.BoolValue() == r.BoolValue();
		}
		if (l.IsNil() || r.IsNil())
		{
			return l.IsNil() && r.IsNil();
		}
		if (l.IsList() && r.IsList())
		{
			return l.ListValueRef().SequenceEqual(r.ListValueRef());
		}
		if (l.IsUndefined() || r.IsUndefined())
		{
			return l.IsUndefined() && r.IsUndefined();
		}
		if (l.IsString() || r.IsString())
		{
			return l.ToString() == r.ToString();
		}
		if (l.IsDouble() || r.IsDouble())
		{
			return /*Math.A*/fabs(l.ToDouble() - r.ToDouble()) < Tolerance;
		}
		if (l.IsInt() || r.IsInt())
		{
			return l.ToInt() == r.ToInt();
		}
		throw CreateInvalidOperationException("==", l, r);
	}

	LispType LispVariant::TypeOf(std::shared_ptr<object> obj)
	{
		if (obj->IsInt())
		{
			return LispType::_Int;
		}
		if (obj->IsDouble())
		{
			return LispType::_Double;
		}
		if (obj->IsBool())
		{
			return LispType::_Bool;
		}
		if (obj->IsString())
		{
			return LispType::_String;
		}
		if (obj->IsIEnumerableOfObject() || obj->IsList())
		{
			return LispType::_List;
		}
		/*
		if (obj is IEnumerable)        // needed for .NET 3.5
		{
		return LispType::_List;
		}
		*/
		if (obj->IsLispFunctionWrapper())
		{
			return LispType::_Function;
		}
		if (obj->IsLispVariant())
		{
			return obj->ToLispVariantRef().Type;
		}
		if (obj->IsLispToken())
		{
			return LispType::_Symbol;
		}

		return LispType::_Undefined;
	}

    LispException LispVariant::CreateInvalidCastException(const string & name, const string & msg) const
	{
		var exception = /*new*/ LispException(string::Format("Invalid cast for {2}, value={1} {0}", msg, StringValue(), name));
		exception.AddTokenInfos(Token);
		return exception;
	}

	LispException LispVariant::CreateInvalidOperationException(const string & operation, const LispVariant & l, const LispVariant & r)
	{
		var exception = /*new*/ LispException(string::Format(NoOperatorForTypes, operation, l.Type, r.Type));
		exception.AddTokenInfos(l.Token);
		return exception;
	}

	const LispFunctionWrapper & LispVariant::FunctionValue() const
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
			return std::make_shared<IEnumerable<std::shared_ptr<object>>>(NativeObjectValue()->ToEnumerableOfObjectRef());
		}
		if (Type != LispType::_List)
		{
			throw CreateInvalidCastException("list");
		}
		return std::make_shared<IEnumerable<std::shared_ptr<object>>>(Value->ToEnumerableOfObjectRef()); // ((IEnumerable)Value).Cast<object>();
		//}
	}

	IEnumerable<std::shared_ptr<object>> g_Empty;

	const IEnumerable<std::shared_ptr<object>> & LispVariant::ListValueRef() const
	{
		//get
		//{
			// Nil is an empty list () !
		if (Type == LispType::_Nil)
		{
			return g_Empty;
		}
		if (IsNativeObject() && NativeObjectValue()->IsIEnumerableOfObject())
		{
			return NativeObjectValue()->ToEnumerableOfObjectRef();
		}
		if (Type != LispType::_List)
		{
			throw CreateInvalidCastException("list");
		}
		return Value->ToEnumerableOfObjectRef(); // ((IEnumerable)Value).Cast<object>();
		//}
	}

	IEnumerable<std::shared_ptr<object>> & LispVariant::ListValueNotConstRef()
	{
		//get
		//{
			// Nil is an empty list () !
		if (Type == LispType::_Nil)
		{
			return g_Empty;
		}
		//if (IsNativeObject() && NativeObjectValue()->IsIEnumerableOfObject())
		//{
		//	return NativeObjectValue()->ToEnumerableOfObjectRef();
		//}
		if (Type != LispType::_List)
		{
			throw CreateInvalidCastException("list");
		}
		return Value->ToEnumerableOfObjectNotConstRef(); // ((IEnumerable)Value).Cast<object>();
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
		if (IsNativeObject() && Type != LispType::_NativeObject)
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
			const IEnumerable<std::shared_ptr<object>> & container = native->ToEnumerableOfObjectRef();
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
			var container = maybeContainer->ToEnumerableOfObjectRef();
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
