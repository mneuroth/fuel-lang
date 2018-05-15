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

#ifndef _CSOBJECT_H
#define _CSOBJECT_H

#include <string>
#include <list>
#include <memory>

#include "cstypes.h"

namespace CsLisp
{
	class LispVariant;
	struct LispFunctionWrapper;

    /// <summary>
    /// Lisp data types.
    /// </summary>
    /*public*/ enum LispType
    {
        _Undefined = 0,
        _Nil = 1,
        _Bool = 2,
        _Int = 3,
        _Double = 4,
        _String = 5,
        _List = 6,
        _Function = 7,
        _Symbol = 8,
        _NativeObject = 9,
        //_Array = 10,
        _Error = 999
    };
    
	enum ObjectType
	{
		__Undefined = 0,
		__Nil = 1,
		__Bool = 2,
		__Int = 3,
		__Double = 4,
		__String = 5,
		__List = 6,
		__Function = 7,
		__Symbol = 8,
		__NativeObject = 9,
		//__Array = 10,
		__LispVariant = 11,
		__LispFunctionWrapper = 12,
		__LispToken = 13,
		__IEnumerableOfObject = 14,
		__VoidPtr = 15,
		__LispScope = 16,
        __Error = 999
    };

	// variant object
	class object
	{
	private:
// TODO --> ersetzte string durch void * fuer diverse typen !!!
		std::string m_sValue;
		union DataType
		{
			bool   b;
			int    i;
			double d;
			LispVariant * pVariant;
		} m_Data;;
        ObjectType m_Type;

	public:
		explicit object()
			: m_sValue("?"), m_Type(ObjectType::__Undefined)
		{
		}
        
		explicit object(const object & other)
			: m_sValue(other.m_sValue), m_Type(other.m_Type)
		{
		}

		explicit object(void * ptr)
			: m_sValue("NULL"), m_Type(ObjectType::__VoidPtr)
		{
		}

		explicit object(const std::string & text)
			: m_sValue(text), m_Type(ObjectType::__String)
        {
        }

		explicit object(const char * text)
			: m_sValue(text), m_Type(ObjectType::__String)
		{
		}

		explicit object(bool value)
			: m_sValue(value ? "true" : "false"), m_Type(ObjectType::__Bool)
        {
			m_Data.b = value;
        }
        
		explicit object(int value)
			: m_sValue(std::to_string(value)), m_Type(ObjectType::__Int)
        {
			m_Data.i = value;
		}

		explicit object(double value)
			: m_sValue(std::to_string(value)), m_Type(ObjectType::__Double)
        {
			m_Data.d = value;
		}

		explicit object(const IEnumerable<std::shared_ptr<object>> & value)
			: m_sValue("NOT_IMPLEMENTED_YET_LIST"), m_Type(ObjectType::__List)
		{
		}

		explicit object(const LispVariant & value);

		~object();

		object& operator=(const std::string & other)
		{
			m_sValue = other;
			return *this;
		}

		object& operator=(int other)
		{
			m_sValue = std::to_string(other);
			return *this;
		}

		object& operator=(double other)
		{
			m_sValue = std::to_string(other);
			return *this;
		}

		operator bool()
		{
			return m_sValue == "true";
		}

		operator int()
		{
			return stoi(m_sValue);
		}

		operator double()
		{
			return stod(m_sValue);
		}

		bool IsBool() const
		{
			return m_Type == ObjectType::__Bool;
		}

		bool IsInt() const
		{
			return m_Type == ObjectType::__Int;
		}

		bool IsDouble() const
		{
			return m_Type == ObjectType::__Double;
		}

		bool IsString() const
		{
			return m_Type == ObjectType::__String;
		}

		bool IsLispVariant() const
		{
			return m_Type == ObjectType::__LispVariant;
		}

		bool IsLispScope() const
		{
			return m_Type == ObjectType::__LispScope;
		}

		bool IsLispToken() const
		{
			return m_Type == ObjectType::__LispToken;
		}

		bool IsLispFunctionWrapper() const
		{
			return m_Type == ObjectType::__LispFunctionWrapper;
		}

		bool IsIEnumerableOfObject() const
		{
			return m_Type == ObjectType::__IEnumerableOfObject;
		}

		std::shared_ptr<LispVariant> ToLispVariant();

		std::shared_ptr<LispScope> ToLispScope();

		LispFunctionWrapper ToLispFunctionWrapper();

		IEnumerable<std::shared_ptr<object>> ToEnumerableOfObject() const;

		std::shared_ptr<IEnumerable<std::shared_ptr<object>>> ToList();

		string ToString() const
		{
			return m_sValue;
		}
        
        std::string GetTypeName()
        {
            return "unknown";       // TODO
        }
        
        ObjectType GetType() const
        {
            return m_Type;
        }

		bool Equals(const object & other)
		{
// TODO --> compare realisieren
			return false;
		}
	};
}

#endif