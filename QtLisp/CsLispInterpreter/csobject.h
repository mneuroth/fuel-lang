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
	class LispScope;
	struct LispFunctionWrapper;
	class LispMacroRuntimeEvaluate;

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
		__LispMacroRuntimeEvaluate = 100,
        __Error = 999
    };

	// variant object
	class object
	{
	private:
		ObjectType m_Type;

		union DataType
		{
			bool   b;
			int    i;
			double d;
			std::string * pString;
			LispVariant * pVariant;
			LispScope * pScope;
			LispToken * pToken;
			IEnumerable<std::shared_ptr<object>> * pList;
			LispFunctionWrapper * pFunctionWrapper;
			LispMacroRuntimeEvaluate * pMacro;
		} m_Data;

		void CleanUpMemory();

		// disable assignment operator
		object & operator=(const object & other);

	public:
		explicit object()
			: m_Type(ObjectType::__Undefined)
		{
		}
        
		explicit object(const object & other);

//		explicit object(void * ptr)
//			: m_Type(ObjectType::__VoidPtr)
//		{
//// TODO --> void * pointer behandeln ...
//		}

		explicit object(const std::string & text)
			: m_Type(ObjectType::__String)
        {
			m_Data.pString = new std::string(text);
        }

		explicit object(const char * text)
			: m_Type(ObjectType::__String)
		{
			m_Data.pString = new std::string(text);
		}

		explicit object(bool value)
			: m_Type(ObjectType::__Bool)
        {
			m_Data.b = value;
        }
        
		explicit object(int value)
			: m_Type(ObjectType::__Int)
        {
			m_Data.i = value;
		}

		explicit object(double value)
			: m_Type(ObjectType::__Double)
        {
			m_Data.d = value;
		}

		explicit object(const IEnumerable<std::shared_ptr<object>> & value);

		explicit object(const LispFunctionWrapper & value);

		explicit object(const LispVariant & value);

		explicit object(const LispMacroRuntimeEvaluate & value);

		explicit object(const LispScope & value);

		~object();

		bool operator==(const object & other) const;
		bool operator!=(const object & other) const;

/*
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
*/
		operator bool() const
		{
			return m_Data.b;
		}

		operator int() const
		{
			return m_Data.i;
		}

		operator double() const
		{
			return m_Data.d;
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

		bool IsList() const
		{
			return m_Type == ObjectType::__List;
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

		bool IsLispMacroRuntimeEvaluate() const
		{
			return m_Type == ObjectType::__LispMacroRuntimeEvaluate;
		}

		std::shared_ptr<LispVariant> ToLispVariant() const;

		//std::shared_ptr<LispScope> ToLispScope() const;

        LispScope * GetLispScopeRef() const;

		LispFunctionWrapper ToLispFunctionWrapper() const;

		std::shared_ptr<LispToken> ToLispToken() const;

		IEnumerable<std::shared_ptr<object>> ToEnumerableOfObject() const;

		std::shared_ptr<IEnumerable<std::shared_ptr<object>>> ToList() const;

		std::shared_ptr<LispMacroRuntimeEvaluate> ToLispMacroRuntimeEvaluate() const;

		string ToString() const;
        
		std::string GetTypeName() const;
        
        ObjectType GetType() const
        {
            return m_Type;
        }

		bool Equals(const object & other) const;
	};
}

#endif
