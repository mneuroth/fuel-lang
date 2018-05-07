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
        __Error = 999
    };

	// variant object
	class object
	{
	private:
// TODO --> ersetzte string durch void * fuer diverse typen !!!
		std::string m_sValue;
        ObjectType m_Type;

	public:
		object()
			: m_sValue("?"), m_Type(ObjectType::__Undefined)
		{
		}
        
		object(const object & other)
			: m_sValue(other.m_sValue), m_Type(other.m_Type)
		{
		}

		object(void * ptr)
			: m_sValue("NULL"), m_Type(ObjectType::__VoidPtr)
		{
		}

		object(const std::string & text)
			: m_sValue(text), m_Type(ObjectType::__String)
        {
        }

        object(bool value)
			: m_sValue(value ? "true" : "false"), m_Type(ObjectType::__Bool)
        {
        }
        
        object(int value)
			: m_sValue(std::to_string(value)), m_Type(ObjectType::__Int)
        {
        }

        object(double value)
			: m_sValue(std::to_string(value)), m_Type(ObjectType::__Double)
        {
        }

		object(const List<std::shared_ptr<object>> & value)
			: m_sValue("NOT_IMPLEMENTED_YET"), m_Type(ObjectType::__List)
		{
		}

		~object()
		{
		}

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

		LispFunctionWrapper ToLispFunctionWrapper();

		/*std::list<std::shared_ptr<object>> &*/std::shared_ptr<List<std::shared_ptr<object>>> ToList();

		std::string ToString() const
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