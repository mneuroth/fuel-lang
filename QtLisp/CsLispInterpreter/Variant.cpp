
#include "Variant.h"
#include "cstypes.h"

namespace CsLisp
{
	double LispVariant::Tolerance = 1e-8;

	const string LispVariant::CanNotConvertTo = "can not convert {0} to {1}";
	const string LispVariant::NoOperatorForTypes = "no {0} operator for types {1} and {2}";

	int LispVariant::CompareTo(std::shared_ptr<object> other)
	{
		if (other->GetType() == ObjectType::__LispVariant /*is LispVariant*/)
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

	IEnumerable<std::shared_ptr<object>> LispVariant::ListValue() const
	{
		//get
		//{
			// Nil is an empty list () !
		if (Type == LispType::_Nil)
		{
			return IEnumerable<std::shared_ptr<object>>(); // std::make_shared<object>(new List<std::shared_ptr<object>>());
		}
		if (Type == LispType::_NativeObject && NativeObjectValue is IEnumerable<object>)
		{
			return (IEnumerable<object>)NativeObjectValue;
		}
		if (Type != LispType::_List)
		{
			throw CreateInvalidCastException("list");
		}
		return ((IEnumerable)Value).Cast<object>();
		//}
	}

	double LispVariant::DoubleValue() const
	{
		//get
		//{
		if (Type != LispType::_Double)
		{
			throw CreateInvalidCastException("double");
		}
		return (double)Value;
		//}
	}

	int LispVariant::IntValue() const
	{
		//get
		//{
		if (Type != LispType.Int)
		{
			throw CreateInvalidCastException("int");
		}
		return (int)Value;
		//}
	}

	bool LispVariant::BoolValue() const
	{
		//get
		//{
		if (Type != LispType.Bool)
		{
			throw CreateInvalidCastException("bool");
		}
		return (bool)Value;
		//}
	}

	std::shared_ptr<object> LispVariant::NativeObjectValue() const
	{
		//get
		//{
		if (Type != LispType.NativeObject && Type != LispType.Nil)
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
		string result = string.Empty;

		object native = NativeObjectValue;
		if (native is IEnumerable<object>)
		{
			var container = (IEnumerable<object>)native;
			foreach(var element in container)
			{
				if (result.Length > 0)
				{
					result += " ";
				}
				result += element != null ? element.ToString() : LispToken.Nil;
			}
			result = "(" + result + ")";
		}
		else
		{
			result = native.ToString();
		}

		return result;
		//}
	}

	/*private*/ static string LispVariant::ExpandContainerToString(std::shared_ptr<object> maybeContainer)
	{
		string ret = string::Empty;

		if (maybeContainer is IEnumerable<object>)
		{
			var container = (IEnumerable<object>)maybeContainer;
			foreach(var item in container)
			{
				if (ret.Length > 0)
				{
					ret += " ";
				}
				ret += ExpandContainerToString(item);
			}
			ret = "(" + ret + ")";
		}
		else
		{
			ret += ExpandItemForContainer(maybeContainer);
		}

		return ret;
	}

}
