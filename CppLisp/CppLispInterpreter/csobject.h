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

#include "cstypes.h"

namespace CppLisp
{
	class LispVariant;
	class LispScope;
	struct LispFunctionWrapper;
	class LispMacroRuntimeEvaluate;
	class LispMacroCompileTimeExpand;

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
		_LValue = 11,
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
		__LValue = 17,
		__Dictionary = 18,
		__LispMacroRuntimeEvaluate = 100,
		__LispMacroCompileTimeExpand = 101,
        __Error = 999
    };

	// variant object
	class DLLEXPORT object
	{
	private:
		ObjectType m_Type;

		union DataType
		{
			bool   b;
			int    i;
			double d;
// TODO --> maybe use shared_ptr, so we can return the correct reference and no copy (i. e. for pCompileMacro) --> search for return copy
			std::string * pString;
			LispVariant * pVariant;
			LispScope * pScope;
			LispToken * pToken;
			IEnumerable<std::shared_ptr<object>> * pList;
			LispFunctionWrapper * pFunctionWrapper;
			LispMacroRuntimeEvaluate * pMacro;
			LispMacroCompileTimeExpand * pCompileMacro;
			std::function<void(std::shared_ptr<object>)> * pAction;
			Dictionary<LispVariant, std::shared_ptr<object>> * pDictionary;
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

		explicit object(const LispMacroCompileTimeExpand & value);

		explicit object(const LispScope & value);

		explicit object(std::function<void(std::shared_ptr<object>)> action);

		explicit object(const Dictionary<LispVariant, std::shared_ptr<object>> & value);

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
		inline operator bool() const
		{
			return m_Data.b;
		}

		inline operator int() const
		{
			return m_Data.i;
		}

		inline operator double() const
		{
			return m_Data.d;
		}

		inline bool IsBool() const
		{
			return m_Type == ObjectType::__Bool;
		}

		inline bool IsInt() const
		{
			return m_Type == ObjectType::__Int;
		}

		inline bool IsDouble() const
		{
			return m_Type == ObjectType::__Double;
		}

		inline bool IsString() const
		{
			return m_Type == ObjectType::__String;
		}

		inline bool IsDictionary() const
		{
			return m_Type == ObjectType::__Dictionary;
		}

		inline bool IsLispVariant() const
		{
			return m_Type == ObjectType::__LispVariant;
		}

		inline bool IsList() const
		{
			return m_Type == ObjectType::__List;
		}

		inline bool IsLispScope() const
		{
			return m_Type == ObjectType::__LispScope;
		}

		inline bool IsLispToken() const
		{
			return m_Type == ObjectType::__LispToken;
		}

		inline bool IsLispFunctionWrapper() const
		{
			return m_Type == ObjectType::__LispFunctionWrapper;
		}

		inline bool IsIEnumerableOfObject() const
		{
			return m_Type == ObjectType::__IEnumerableOfObject;
		}

		inline bool IsLispMacroRuntimeEvaluate() const
		{
			return m_Type == ObjectType::__LispMacroRuntimeEvaluate;
		}

		inline bool IsLispMacroCompileTimeExpand() const
		{
			return m_Type == ObjectType::__LispMacroCompileTimeExpand;
		}

		inline bool IsLValue() const
		{
			return m_Type == ObjectType::__LValue;
		}

		inline ObjectType GetType() const
		{
			return m_Type;
		}

		std::string GetTypeName() const;

		size_t GetHash(std::shared_ptr<LispScope> scope) const;

		bool Equals(const object & other) const;

		// get references to data
        LispScope * GetLispScopeRef() const;
		const LispFunctionWrapper & ToLispFunctionWrapper() const;

		// get a copy of the data
		const IEnumerable<std::shared_ptr<object>> & ToEnumerableOfObjectRef() const;
		IEnumerable<std::shared_ptr<object>> & ToEnumerableOfObjectNotConstRef();
		const LispVariant & ToLispVariantRef() const;
		LispVariant & ToLispVariantNotConstRef();
		std::shared_ptr<LispVariant> ToLispVariant() const;
		const IEnumerable<std::shared_ptr<object>> & ToListRef() const;
		std::shared_ptr<IEnumerable<std::shared_ptr<object>>> ToList() const;
		std::shared_ptr<LispToken> ToLispToken() const;
		std::shared_ptr<LispMacroRuntimeEvaluate> ToLispMacroRuntimeEvaluate() const;
		std::shared_ptr<LispMacroCompileTimeExpand> ToLispMacroCompileTimeExpand() const;
		std::function<void(std::shared_ptr<object>)> ToSetterAction() const;
		string ToString() const;
		Dictionary<LispVariant, std::shared_ptr<object>> & ToDictionary();
		const Dictionary<LispVariant, std::shared_ptr<object>> & ToDictionary() const;
	};
}

#endif
