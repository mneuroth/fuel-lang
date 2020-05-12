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
#include "Exception.h"
#include "Variant.h"
#include "Scope.h"
#include "Token.h"

namespace CppLisp
{
	object::object(const LispVariant & value)
		: m_Type(ObjectType::__LispVariant)
	{
		m_Data.pVariant = new LispVariant(value);
	}

	object::object(const IEnumerable<std::shared_ptr<object>> & value)
		: m_Type(ObjectType::__List)
	{
		m_Data.pList = new IEnumerable<std::shared_ptr<object>>(value);
	}

	object::object(const LispFunctionWrapper & value)
		: m_Type(ObjectType::__LispFunctionWrapper)
	{
		m_Data.pFunctionWrapper = new LispFunctionWrapper(value);
	}

	object::object(const LispMacroRuntimeEvaluate & value)
		: m_Type(ObjectType::__LispMacroRuntimeEvaluate)
	{
		m_Data.pMacro = new LispMacroRuntimeEvaluate(value);
	}

	object::object(const LispMacroCompileTimeExpand & value)
		: m_Type(ObjectType::__LispMacroCompileTimeExpand)
	{
		m_Data.pCompileMacro = new LispMacroCompileTimeExpand(value);
	}

	object::object(const LispScope & value)
		: m_Type(ObjectType::__LispScope)
	{
		m_Data.pScope = new LispScope(value);
	}

	object::object(std::function<void(std::shared_ptr<object>)> action)
		: m_Type(ObjectType::__LValue)
	{
		m_Data.pAction = new std::function<void(std::shared_ptr<object>)>(action);
	}

	object::object(const object & other)
	{
		m_Type = other.m_Type;

		if (other.IsLispVariant())
		{
			m_Data.pVariant = new LispVariant(*(other.ToLispVariant()));
		}
		else if (other.IsLispScope())
		{
			m_Data.pScope = new LispScope(*(other.GetLispScopeRef()));
		}
		else if (other.IsLispFunctionWrapper())
		{
			m_Data.pFunctionWrapper = new LispFunctionWrapper(other.ToLispFunctionWrapper());
		}
		else if (other.IsLispToken())
		{
			m_Data.pToken = new LispToken(*(other.ToLispToken()));
		}
		else if (other.IsList())
		{
			m_Data.pList = new IEnumerable<std::shared_ptr<object>>(*(other.ToList()));
		}
		else if (IsLispMacroRuntimeEvaluate())
		{
			m_Data.pMacro = new LispMacroRuntimeEvaluate(*(other.ToLispMacroRuntimeEvaluate()));
		}
		else if (IsLispMacroCompileTimeExpand())
		{
			m_Data.pCompileMacro = new LispMacroCompileTimeExpand(*(other.ToLispMacroCompileTimeExpand()));
		}
		else if (other.IsLValue())
		{
			m_Data.pAction = new std::function<void(std::shared_ptr<object>)>(*(other.m_Data.pAction));
		}
		else if (other.IsString())
		{
			m_Data.pString = new string(other.ToString());
		}
		else if (other.IsDictionary())
		{
			m_Data.pDictionary = new Dictionary<LispVariant, std::shared_ptr<object>>(other.ToDictionary());
		}
		else
		{
			m_Data = other.m_Data;
		}
	}

	object::object(const Dictionary<LispVariant, std::shared_ptr<object>> & value)
		: m_Type(ObjectType::__Dictionary)
	{
		m_Data.pDictionary = new Dictionary<LispVariant, std::shared_ptr<object>>(value);
	}

	object::~object()
	{
		CleanUpMemory();
	}

	bool object::operator==(const object & other) const
	{
		if (GetType() == other.GetType())
		{
			switch (m_Type)
			{
				case __Undefined:
					return true;
				case __Nil:
					return true;
				case __Bool:
					return (bool)(*this)==(bool)other;
				case __Int:
					return (int)(*this) == (int)other;
				case __Double:
					return (double)(*this) == (double)other;
				case __String:
					return ToString() == other.ToString();
				case __List:
					return ToListRef() == other.ToListRef();
				case __Function:
					return false;
				case __Symbol:
					return ToString() == other.ToString();
				case __NativeObject:
					return false;
					//__Array = 10,
				case __LispVariant:
					return ToLispVariantRef() == other.ToLispVariantRef();
				case __LispFunctionWrapper:
					return false;
				case __LispToken:
					return ToString() == other.ToString();
				case __IEnumerableOfObject:
					return ToListRef() == other.ToListRef();
				case __VoidPtr:
					return true;
				case __LispScope:
					return *(GetLispScopeRef()) == *(other.GetLispScopeRef());
				case __Error:
					return false;
				default:
					return false;
			}
		}
		return false;
	}

	bool object::operator!=(const object & other) const
	{
		return !(*this == other);
	}

	void object::CleanUpMemory()
	{
		if (IsLispVariant())
		{
			delete m_Data.pVariant;
		}
		else if (IsLispScope())
		{
			delete m_Data.pScope;
		}
		else if (IsLispFunctionWrapper())
		{
			delete m_Data.pFunctionWrapper;
		}
		else if (IsLispToken())
		{
			delete m_Data.pToken;
		}
		else if (IsList())
		{
			delete m_Data.pList;
		}
		else if (IsLispMacroRuntimeEvaluate())
		{
			delete m_Data.pMacro;
		}
		else if (IsLispMacroCompileTimeExpand())
		{
			delete m_Data.pCompileMacro;
		}
		else if (IsLValue())
		{
			delete m_Data.pAction;
		}
		else if (IsString())
		{
			delete m_Data.pString;
		}
		else if (IsDictionary())
		{
			delete m_Data.pDictionary;
		}
	}

	size_t object::GetHash(std::shared_ptr<LispScope> scope) const
	{
		switch (m_Type)
		{
			case __Undefined:
				return std::hash<std::string>{}("###undefined###");
			case __Nil:
				return std::hash<std::string>{}("###nil###");
			case __Bool:
				return std::hash<bool>{}(m_Data.b);
			case __Int:
				return std::hash<int>{}(m_Data.i);
			case __Double:
				return std::hash<double>{}(m_Data.d);
			case __String:
				return std::hash<std::string>{}(*(m_Data.pString));
			case __List:
				throw LispException("No hash available for Lisp type.", scope.get());
			case __Function:
				throw LispException("No hash available for Function type.", scope.get());
			case __Symbol:
				throw LispException("No hash available for Symbol type.", scope.get());
			case __NativeObject:
				throw LispException("No hash available for NativeObject type.", scope.get());
			case __LispVariant:
				throw LispException("No hash available for LispVariant type.", scope.get());
			case __LispFunctionWrapper:
				throw LispException("No hash available for FunctionWrapper type.", scope.get());
			case __LispToken:
				throw LispException("No hash available for LispToken type.", scope.get());
			case __IEnumerableOfObject:
				throw LispException("No hash available for IEnuerableOfObject type.", scope.get());
			case __VoidPtr:
				throw LispException("No hash available for VoidPtr type.", scope.get());
			case __LispScope:
				throw LispException("No hash available for LispScope type.", scope.get());
			case __Error:
				throw LispException("No hash available for Error type.", scope.get());
			default:
				return std::hash<std::string>{}("###undefined###");
		}
//		return std::hash<std::string>{}(*(m_Data.pString));
	}

	std::string object::GetTypeName() const 
	{
		switch (m_Type)
		{
			case __Undefined:
				return "Undefined";
			case __Nil:
				return "Nil";
			case __Bool:
				return "Bool";
			case __Int:
				return "Int";
			case __Double:
				return "Double";
			case __String:
				return "String";
			case __List:
				return "List";
			case __Function:
				return "Function";
			case __Symbol:
				return "Symbol";
			case __NativeObject:
				return "NativeObject";
			//__Array = 10,
			case __LispVariant:
				return "LispVariant";
			case __LispFunctionWrapper:
				return "LispFunctionWrapper";
			case __LispToken:
				return "LispToken";
			case __IEnumerableOfObject:
				return "IEnumerableOfObject";
			case __VoidPtr:
				return "VoidPtr";
			case __LispScope:
				return "LispScope";
			case __Error:
				return "Error";
			default:
				return "<unknown>";
		}
	}

	string object::ToString() const
	{
		switch (m_Type)
		{
			case __Int:
				return std::to_string(m_Data.i);
			case __Double:
				return std::to_string(m_Data.d);
			case __String:
				return *(m_Data.pString);
// TODO --> implement ToString() for more data types
			case __Bool:
				return m_Data.b ? "true" : "false";
			case __Undefined:
				return "Undefined";
			case __Nil:
				return "Nil";
			case __List:
				{
					string result;
					if (IsList())
					{
						for (var elem : *(m_Data.pList))
						{
							if (result.size() > 0)
							{
								result += " ";
							}
							result += elem->ToString();
						}
					}
					return "(" + result + ")";
			}
			case __Function:
				return "Function";
			case __Symbol:
				return "Symbol";
			case __NativeObject:
				return "NativeObject";
				//__Array = 10,
			case __Dictionary:
				return "Dictionary";
			case __LispVariant:
				return m_Data.pVariant->ToString();
			case __LispFunctionWrapper:
				return "LispFunctionWrapper";
			case __LispToken:
				return "LispToken";
			case __IEnumerableOfObject:
				return "IEnumerableOfObject";
			case __VoidPtr:
				return "VoidPtr";
			case __LispScope:
				return "LispScope";
			case __Error:
				return "Error";
			default:
				return "<unknown>";
		}
	}

	Dictionary<LispVariant, std::shared_ptr<object>> & object::ToDictionary()
	{
		return *(m_Data.pDictionary);
	}

	const Dictionary<LispVariant, std::shared_ptr<object>> & object::ToDictionary() const
	{
		return *(m_Data.pDictionary);
	}

	std::shared_ptr<LispToken> object::ToLispToken() const
	{
		if (IsLispToken())
		{
			// return a copy 
			return std::make_shared<LispToken>(*(m_Data.pToken));
		}
		return null;
	}

	std::shared_ptr<LispVariant> object::ToLispVariant() const
	{
		if (IsLispVariant())
		{
			// return a copy 
			return std::make_shared<LispVariant>(*(m_Data.pVariant));
		}
		return std::make_shared<LispVariant>(LispVariant(LispType::_Nil));
	}

	LispVariant g_Nil(LispType::_Nil);

	const LispVariant & object::ToLispVariantRef() const
	{
		if (IsLispVariant())
		{
			// return a reference
			return *(m_Data.pVariant);
		}
		return g_Nil;
	}

	LispVariant & object::ToLispVariantNotConstRef()
	{
		if (IsLispVariant())
		{
			// return a reference
			return *(m_Data.pVariant);
		}
		return g_Nil;
	}

	LispScope * object::GetLispScopeRef() const
	{
		if (IsLispScope())
		{
			return m_Data.pScope;
		}
		return null;
	}

	IEnumerable<std::shared_ptr<object>> g_EmptyList;

	const IEnumerable<std::shared_ptr<object>> & object::ToListRef() const
	{
		if (IsList())
		{
			// return a reference
			return *(m_Data.pList);
		}
		return g_EmptyList;
	}

	std::shared_ptr<IEnumerable<std::shared_ptr<object>>> object::ToList() const
	{
		if (IsList())
		{
			// return a copy 
			return std::make_shared<IEnumerable<std::shared_ptr<object>>>(*(m_Data.pList));
		}
		return null;
	}

	std::shared_ptr<LispMacroRuntimeEvaluate> object::ToLispMacroRuntimeEvaluate() const
	{
		if (IsLispMacroRuntimeEvaluate())
		{
// TODO: ist das wirklich korrekt eine Kopie zu liefern ?
			// return a copy (not fast but it works)
			return std::make_shared<LispMacroRuntimeEvaluate>(*(m_Data.pMacro));
		}
		return null;
	}

	std::shared_ptr<LispMacroCompileTimeExpand> object::ToLispMacroCompileTimeExpand() const
	{
		if (IsLispMacroCompileTimeExpand())
		{
// TODO: ist das wirklich korrekt eine Kopie zu liefern ?
			// return a copy (not fast but it works)
			return std::make_shared<LispMacroCompileTimeExpand>(*(m_Data.pCompileMacro));
		}
		return null;
	}

	std::function<void(std::shared_ptr<object>)> object::ToSetterAction() const
	{
		if (IsLValue())
		{
			return *(m_Data.pAction);
		}
		return null;
	}

	const LispFunctionWrapper & object::ToLispFunctionWrapper() const
	{
		if (IsLispFunctionWrapper())
		{
			return *(m_Data.pFunctionWrapper);
		}
		throw LispExceptionBase("Invalid cast to lisp function wrapper.");
	}

	const IEnumerable<std::shared_ptr<object>> & object::ToEnumerableOfObjectRef() const
	{
		return *(m_Data.pList); // IEnumerable<std::shared_ptr<object>>(*(m_Data.pList));
	}

	IEnumerable<std::shared_ptr<object>> & object::ToEnumerableOfObjectNotConstRef()
	{
		return *(m_Data.pList); // IEnumerable<std::shared_ptr<object>>(*(m_Data.pList));
	}

	bool object::Equals(const object & other) const
	{
		if (GetType() != other.GetType())
		{
			return false;
		}

		switch (m_Type)
		{
			case __Undefined:
				return true;		// ???
			case __Nil:
				return true;
			case __Bool:
				return (bool)(*this) == (bool)other;
			case __Int:
				return (int)(*this) == (int)other;
			case __Double:
				return (double)(*this) == (double)other;
			case __String:
				return ToString() == other.ToString();
			case __List:
				return ToListRef() == other.ToListRef();
			case __Function:
// TODO --> not implemented yet ! also not in C#
				return false;		// ???
			case __Symbol:
				return ToString() == other.ToString();
			case __NativeObject:
// TODO --> implement for native object...
				return false;
			//__Array = 10,
			case __LispVariant:
				return ToLispVariant() == other.ToLispVariant();
			case __LispFunctionWrapper:
// TODO --> std::function comparison is not supported in C++ 
                return false; //&(ToLispFunctionWrapper().Function) == &(other.ToLispFunctionWrapper().Function);
			case __LispToken:
				return ToString() == other.ToString();
			case __IEnumerableOfObject:
				return ToEnumerableOfObjectRef() == other.ToEnumerableOfObjectRef();
			case __VoidPtr:
				return true;
			case __LispScope:
				return *(GetLispScopeRef()) == *(other.GetLispScopeRef());
			case __Error:
				return true;		// ???
			default:
				return false;
		}
	}
}
