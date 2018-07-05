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
#include "Scope.h"
#include "Token.h"

#include <iostream>

namespace CsLisp
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
		else if (other.IsString())
		{
			m_Data.pString = new string(other.ToString());
		}
		else
		{
			m_Data = other.m_Data;
		}
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
					return ToList() == other.ToList();
				case __Function:
					return false;
				case __Symbol:
					return ToString() == other.ToString();
				case __NativeObject:
					return false;
					//__Array = 10,
				case __LispVariant:
					return *(ToLispVariant()) == *(other.ToLispVariant());
				case __LispFunctionWrapper:
					return false;
				case __LispToken:
					return ToString() == other.ToString();
				case __IEnumerableOfObject:
					return *(ToList()) == *(other.ToList());
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
		else if (IsString())
		{
			delete m_Data.pString;
		}
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
			case __Undefined:
				return "Undefined";
			case __Nil:
				return "Nil";
			case __Bool:
				return m_Data.b ? "true" : "false";
			case __Int:
				return std::to_string(m_Data.i);
			case __Double:
				return std::to_string(m_Data.d);
			case __String:
				return *(m_Data.pString);
// TODO --> ToString fuer weitere Datentypen realisieren
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

	LispScope * object::GetLispScopeRef() const
	{
		if (IsLispScope())
		{
			return m_Data.pScope;
		}
		return null;
	}

//	std::shared_ptr<LispScope> object::ToLispScope() const
//	{
//		if (IsLispScope())
//		{
//// TODO: ist das wirklich korrekt eine Kopie zu liefern ?
//			// return a copy 
//			return std::make_shared<LispScope>(*(m_Data.pScope));
//		}
//		return null;
//	}

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
			// return a copy 
			return std::make_shared<LispMacroRuntimeEvaluate>(*(m_Data.pMacro));
		}
		return null;
	}


	std::shared_ptr<LispMacroCompileTimeExpand> object::ToLispMacroCompileTimeExpand() const
	{
		if (IsLispMacroCompileTimeExpand())
		{
// TODO: ist das wirklich korrekt eine Kopie zu liefern ?
			// return a copy 
			return std::make_shared<LispMacroCompileTimeExpand>(*(m_Data.pCompileMacro));
		}
		return null;
	}

	LispFunctionWrapper object::ToLispFunctionWrapper() const
	{
		if (IsLispFunctionWrapper())
		{
			return *(m_Data.pFunctionWrapper);
		}
		throw LispException("Invalid cast to lisp function wrapper.");
	}

	IEnumerable<std::shared_ptr<object>> object::ToEnumerableOfObject() const
	{
		return IEnumerable<std::shared_ptr<object>>(*(m_Data.pList));
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
				return ToList() == other.ToList();
			case __Function:
// TODO --> not implemented yet !
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
// TODO --> check
                return false; //&(ToLispFunctionWrapper().Function) == &(other.ToLispFunctionWrapper().Function);
			case __LispToken:
				return ToString() == other.ToString();
			case __IEnumerableOfObject:
				return ToEnumerableOfObject() == other.ToEnumerableOfObject();
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

	void TextWriter::Write(const string & txt)
	{
		if (m_bToString)
		{
			m_sText += txt;
		}
		std::cout << txt;
	}

	void TextWriter::WriteLine()
	{
		if (m_bToString)
		{
			m_sText += "\n";
		}
		std::cout << std::endl;
	}

	void TextWriter::WriteLine(const string & txt)
	{
		if (m_bToString)
		{
			m_sText += txt + "\n";
		}
		std::cout << txt << std::endl;
	}

	void TextWriter::WriteLine(const string & txt, const string & txt1)
	{
		string temp = string::Format(txt, txt1);
		if (m_bToString)
		{
			m_sText += temp + "\n";
		}
		std::cout << temp << std::endl;
	}

	void TextWriter::WriteLine(const string & txt, const string & txt1, const string & txt2)
	{
		string temp = string::Format(txt, txt1, txt2);
		if (m_bToString)
		{
			m_sText += temp + "\n";
		}
		std::cout << temp << std::endl;
	}

	void TextWriter::WriteLine(const string & txt, const string & txt1, const string & txt2, const string & txt3)
	{
		string temp = string::Format(txt, txt1, txt2, txt3);
		if (m_bToString)
		{
			m_sText += temp + "\n";
		}
		std::cout << temp << std::endl;
	}

	void TextWriter::WriteLine(const string & txt, const string & txt1, const string & txt2, const string & txt3, const string & txt4)
	{
		string temp = string::Format(txt, txt1, txt2, txt3, txt4);
		if (m_bToString)
		{
			m_sText += temp + "\n";
		}
		std::cout << temp << std::endl;
	}

	string TextReader::ReadLine()
	{
		string input;
		std::getline(std::cin, input);
		return input;
	}
}

